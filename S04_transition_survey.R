# Survey of the parameter space
library( CAB )
library( foreach )
library( doParallel )
library( data.table )

##### Get the data #####
load( "behavioural_profile.RData" )
source( "cost_function.R" )
source( "COR.do.R" )
source( "COR_fx.R" )

subject = "S04"
subject_data = behavioural_profile[ Subject == subject ]

##### Define the fitting variables #####
fitting_vars = subject_data[ , name ]
rft_levels = unique( subject_data$VI[ subject_data$VI > 0 ] )
rft_conditions = rep( rft_levels, times = c(10,20,20) )

# Get minimum IRT and initial reserve values
minimum_irt = subject_data[ endsWith( name, "irt" ), min(unlist(statistic)) ]
initial_reserve = subject_data[ name == "30_irt", {
    x = mean( unlist(statistic) )
    1-exp( -1/(x-minimum_irt) )
    } ]


##### Simulation parameters #####
monitor_matrix_names = c( "accept", "DOR_max", "DOR_scale", "depletion_constant", "overall_cost", fitting_vars )

# make a matrix to store results of the simulation
result_matrix = data.table( iteration = rep( NaN, 5000 ) )
result_matrix[ , eval(monitor_matrix_names) := NaN ]

##### Make the COR model #####
COR_params = {list(
    DOR_max = 0.00001, DOR_scale = 100, depletion_constant = 0.000001, reserve_value = NaN,
    inter_rft_interval = 30, shift = 1, time = 0, session_duration = 1800,
    min_irt = minimum_irt, initial_reserve = initial_reserve
)}

############################
cl = makeCluster( 8, outfile = "survey_log.txt" )
registerDoParallel( cl )

results = foreach( n = 1:8, .packages = c("CAB", "data.table") ) %dopar%{
    t1 = Sys.time()
    result_matrix[ , DOR_max := runif(.N, 0, 0.01) ]
    result_matrix[ , DOR_scale := runif(.N, 0, 5) ]
    result_matrix[ , depletion_constant := runif(.N, 0, 1e-4) ]
    result_matrix[ , iteration := 1:.N ]
    
    cmp_COR.do = compiler::cmpfun( COR.do )

    for ( i in 1:nrow( result_matrix ) ){
        if ( i %% 10 == 0 ) print( paste0( "Cluster ", n, " iteration ", i ) )
        org = list2env( COR_params )
        list2env( result_matrix[ i, 3:5], envir = org )
        sim_event_record = simulate_COR( org, rft_conditions, COR_fx, cmp_COR.do )
        compute_cost( sim_event_record, subject_data, result_matrix )

        if ( i %% 500 == 0 ) save( result_matrix, file = paste0( "COR_survey4 ", n, ".RData" ) )
    }
    time_diff = Sys.time() - t1
    print( paste0( "Cluster ", n, " ", time_diff ) )
    result_matrix
}

results = rbindlist( results )
save( results, file = "survey_results4.RData" )
stopCluster(cl)
