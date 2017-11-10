#Library
library( CAB )
library( foreach )
library( doParallel )
library( data.table )
library( tmvtnorm )

##### Get data #####
# Get ABC_params
load( "ABC_params.RData" )
# behavioural_profile
load( "behavioural_profile.RData" )
# Load COR.do and simulate_COR
source( "COR.do.R" )
rm( simulate_COR )
# Load compute_cost, cost_function, summmary_statistic
source( "cost_function.R" )
rm( compute_cost )
# Load COR_params containing min_irt and initial_reserve
load( "COR_params.RData" )
# Load COR_fx, list of functions
source( "COR_fx.R" )
# Load do.mcmc
source( "ABC_mcmc.R" )

rft_conditions = rep( c(30,60,240), c(10,20,20) )
subjects = behavioural_profile[ , unique(Subject) ]

start_time = format( Sys.time(), "%H_%M_%d_%b_%y" )
cl = makeCluster( 8, outfile = paste0( start_time, ".txt" ) )
registerDoParallel( cl )

results = foreach ( subject = subjects, .packages = c("data.table", "CAB", "tmvtnorm") ) %dopar% {
    
    subject_data = behavioural_profile[ Subject == subject ]
    COR_params_ = COR_params[ Subject == subject ]
    phases = 1:6
    mcmc_history = data.table( results = rep(list(1),max(phases)), ABC_params = list(1) )
    
    COR.do = compiler::cmpfun( COR.do, options = list( optimize = 3 ) )
    carryover = NaN
    
    for ( phase in phases ){
        if ( phase < max(phases) ) iterations = 1000
        else iterations = 25000
        
        t1 = Sys.time()
        
        mcmc_run = do.mcmc( ABC_params, iterations, subject_data, COR_params_, rft_conditions, COR.do,
            phase, carryover )
        set( mcmc_history, i = phase, j = c("results","ABC_params"), mcmc_run )
        
        accept_rate = mcmc_run$results[[1]][ , sum(accept)/.N ]
        posterior_sample = mcmc_run$results_matrix[[1]][ accept == 1 ]
        if ( nrow( posterior_sample ) > 3 ){
            new_proposal_cov = posterior_sample[ , cov( .SD[,2:4] ) ]
            ABC_params$proposal_cov = new_proposal_cov
        }
        
        if ( accept_rate > 0.2 ){
            new_tolerance = ABC_params$threshold * 0.95
            ABC_params$threshold = new_tolerance
        }
        if ( accept_rate <= 0.1 ){
            new_tolerance = ABC_params$threshold * 1.05
            ABC_params$threshold = new_tolerance
        }
        carryover = mcmc_run$results[[1]][ which.min(overall_cost) ]

        save( mcmc_history, file = paste0( subject, ".RData" ) )
        time_diff = difftime( Sys.time(), t1, units = "secs" )
        print( paste0( "Cluster ", subject, " Phase ", phase, " " , time_diff ) )
    }
    mcmc_history
}

results = rbindlist( results, idcol = "subject" )
save( results, file = "fit_results.RData" )
stopCluster( cl )
