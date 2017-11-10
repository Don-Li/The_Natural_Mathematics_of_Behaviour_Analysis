do.mcmc = function( ABC_params, iterations, subject_data, COR_params, rft_conditions, COR.do, phase, carryover ){
    
    tolerance = ABC_params$threshold
    prior_params = ABC_params$prior_params
    proposal_cov = ABC_params$proposal_cov

    param_names = names( prior_params )
    var_names = subject_data[, name ]
    
    results_matrix = data.table( iteration = 1:iterations )
    results_matrix[ , eval(c(param_names,var_names,"accept", "overall_cost")) := NaN ]
    
    lb = rep(0,3)
    
    proposed_params = prior_params
    got_seed = FALSE
    test = 0
    
    if ( phase == 1 ){
        while ( !got_seed ){
            test = test + 1
            if ( test > 100 ){
                test = 0; tolerance = tolerance * 1.05
            }
            
            proposed_params[] = rexp( 3, 1/prior_params )
            new_params = as.list( proposed_params )
            sim_event_record = simulate_COR2( COR_params, new_params, rft_conditions, COR_fx, COR.do )
            cost_vect = compute_cost2( sim_event_record, subject_data )
            if ( cost_vect$overall_cost <= tolerance ){
                set( results_matrix, 1L, j = param_names, new_params )
                set( results_matrix, 1L, j = names(cost_vect), cost_vect )
                set( results_matrix, i = 1L, j = "accept", 1 )
                got_seed = T
            }
    
        }
        current_params = proposed_params
    } else{
        set( results_matrix, 1L, j = names( carryover ), carryover )
        current_params = as.matrix( carryover[ , 2:4 ] )[1,]
    }
    
    
    for ( i in 2:iterations ){
        proposed_params[] = rtmvnorm( 1, mean = current_params, sigma = proposal_cov, lower = lb )
        accept_ratio = do.accept_ratio( proposed_params, current_params, proposal_cov, prior_params, lb )
        
        if ( runif(1) < accept_ratio ){
            new_params = as.list( proposed_params )
            sim_event_record = simulate_COR2( COR_params, new_params, rft_conditions, COR_fx, COR.do )
            cost_vect = compute_cost2( sim_event_record, subject_data )
            
            if ( cost_vect$overall_cost <= tolerance ){
                set( results_matrix, i = i, j = param_names, new_params )
                set( results_matrix, i = i, j = names(cost_vect), cost_vect )
                set( results_matrix, i = i, j = "accept", 1 )
                accept = T
                current_params = proposed_params
            } else accept = F
        } else accept = F


        if ( !accept ){
            previous_results = results_matrix[ i-1 ]
            set( results_matrix, i = i, j = names(previous_results), previous_results )
            set( results_matrix, i = i, j = "accept", 0 )
        }
        
    }
    results_matrix[ , iteration := 1:.N ]
    ABC_params$threshold = tolerance
    list( results_matrix = list(results_matrix), ABC_params = list(ABC_params) )
}

do.accept_ratio = function( proposed_params, current_params, proposal_cov, prior_params, lb ){
    backwards_kernel = dtmvnorm( proposed_params, mean = current_params, sigma = proposal_cov, log = T,
        lower = lb )
    forwards_kernel = dtmvnorm( current_params, mean = proposed_params, sigma = proposal_cov, log = T, 
        lower = lb )
    proposal_prior = sum( dexp( proposed_params, 1/prior_params, log = T ) )
    current_prior = sum( dexp( current_params, 1/prior_params, log = T ) )
        
    exp( (proposal_prior+backwards_kernel) - (current_prior + forwards_kernel) )
}
