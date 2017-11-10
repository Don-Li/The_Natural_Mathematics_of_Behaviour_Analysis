#Library
library( CAB )
library( foreach )
library( doParallel )
library( data.table )

setwd( "fit_results" )
load( "fit_results.RData" )

results[ , phase := 1:6, by = "subject" ]
results = results[ phase == 6 ]
thin_mcmc = results[ , {
    results[[1]][ (1:.N)%%5 == 0, list( DOR_max, DOR_scale, depletion_constant ) ]
}, by = "subject" ]

set.seed(1)
posterior_samples = thin_mcmc[ , {
    take = sample( 1:.N, 500, T )
    .SD[ take ]
    }, by = "subject" ]

setwd( ".." )

# Load COR.do and simulate_COR
source( "COR.do.R" )
# behavioural_profile
load( "behavioural_profile.RData" )
# Load COR_params containing min_irt and initial_reserve
load( "COR_params.RData" )
# Load COR_fx, list of functions
source( "COR_fx.R" )

rft_conditions = rep( c(30,60,240), c(10,20,20) )
subjects = behavioural_profile[ , unique(Subject) ]
rm( behavioural_profile )
posterior_samples[ ,Subject := subjects[subject] ]

start_time = format( Sys.time(), "%H_%M_%d_%b_%y" )

cl = makeCluster( 8, outfile = paste0( start_time, "predictive.txt" ) )
registerDoParallel( cl )

results = foreach( subject_ = subjects, .packages = c("data.table", "CAB" ) ) %dopar%{
    
    COR_params_ = COR_params[ Subject == subject_ ]
    COR.do = compiler::cmpfun( COR.do, options = list( optimize = 3 ) )
    posterior_samples_ = posterior_samples[ Subject == subject_ ]
    
    results = lapply( 1:nrow(posterior_samples_), function(x){
        print( paste0( "cluster ", subject_, " run ", x ) )
        org = list2env( COR_params_ )
        list2env( posterior_samples_[ x, 2:4 ], envir = org )
        x1 = rbindlist( lapply( 1:length(rft_conditions), function(x){
            COR.do( org, rft_conditions, session = x, COR_fx )
        } ) )
        x1
    } )
    
    save( results, file = paste0( "posterior_events_", subject_, ".RData" ) )
    1
}

stopCluster(cl)
