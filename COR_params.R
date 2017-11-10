# Make parameters
library( data.table )
load( "behavioural_profile.RData" )

# Get minimum IRT and initial reserve values
COR_params = behavioural_profile[ endsWith( name, "irt" ), {
    irts = unlist( statistic )
    min_irt = min( irts )
    mean_irt = mean( irts )
    initial_reserve = 1-exp( -1/( mean_irt - min_irt ) )
    list( min_irt = min_irt, initial_reserve = initial_reserve )
    } , by = "Subject" ]

all_COR_params = {list(
    DOR_max = 0.00001, DOR_scale = 100, depletion_constant = 0.000001, reserve_value = NaN,
    inter_rft_interval = 30, shift = 1, time = 0, session_duration = 1800
)}
COR_params[ , eval( names( all_COR_params ) ) := all_COR_params ]

save( COR_params, file = "COR_params.RData" )
