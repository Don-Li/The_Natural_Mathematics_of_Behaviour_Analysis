cost_function = function( name, prediction, statistic ){
    
    distributions = !endsWith( name, "resp_rate" )
    x = lapply( 1:nrow(subject_data), function(x){
        if ( distributions[x] ){
            return( ks( statistic[[x]], prediction[[x]] ) )
        } else{
            return( sum( ( prediction[[x]] - statistic[[x]] )^2 ) )
        }
    } )
    names(x) = name
    x
}


# Summary statistics

summary_statistics = function( event_record ){
    
    response_rate = event_record[ ,{
        sum( event == "resp_time" ) / 1800
    }, by = "Session" ][ , list( statistic = list(V1), name = "resp_rate", VI = 0 ) ]
    
    distributions = event_record[,{
        prp = simple_ixyi( event, time, "rft_time", "resp_time", "start" )["ixyi"]
        irt = simple_ixyi( event, time, "resp_time", "resp_time", c("rft_time","start") )["ixyi"]
        iri = simple_ixyi( event, time, "rft_time", "rft_time", "start" )["ixyi"]
        list( statistic = c(prp, irt, iri), name = paste0( VI, "_", c("prp", "irt", "iri") ) )
    }, by = "VI" ]
    rbindlist( list( distributions, response_rate ), use.names = T )
}

compute_cost = function( sim_event_record, subject_data, result_matrix, i ){
    summaries = summary_statistics( sim_event_record )
    subject_data[ summaries, prediction := i.statistic, on = "name" ]
    cost = subject_data[ , cost_function(name, prediction, statistic) ]
    overall_cost = exp( sum(log(cost))/ncol(cost) )
    
    set( result_matrix, i = i, j = names(cost), value = cost )
    set( result_matrix, i = i, j = "overall_cost", value = overall_cost )
}

compute_cost2 = function( sim_event_record, subject_data ){
    summaries = summary_statistics( sim_event_record )
    subject_data[ summaries, prediction := i.statistic, on = "name" ]
    cost = subject_data[ , cost_function(name, prediction, statistic) ]
    overall_cost = exp( sum(log(cost))/ncol(cost) )
    set( cost, i = 1L, j = "overall_cost", overall_cost )
}
