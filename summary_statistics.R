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

