library (CABdata )

data( "variable_interval_1_2017" )
working_data = copy( variable_interval_1_2017 )

#Remove the first 10 sessions for VI30
working_data = working_data[ !(VI == 30 & VI_session %in% 1:10) ][ !Subject %in% c("222", "224") ]

#Clean responses during food
working_data[ , cleaned_prp_event_record := lapply( event_record, function(x) clean_short_IxyI( x, "rft_time", "resp_time",  2 ) ) ]
#Clean short IRTs
working_data[ ,cleaned_irt_event_record := lapply( event_record, function(x) clean_short_IxyI( x, "resp_time", gap = 0.15 ) ) ]

#Add total session numbers
working_data[ , Session := 1:.N, by = c("Subject") ]

#Collapse event records
event_records = working_data[ ,{
    event_records = lapply( 1:.N, function(x){
        er = as.list( cleaned_irt_event_record[[x]]@events )
        as.data.table( c( er, Session[x] ) )
    } )
    event_records = rbindlist( event_records )
    }, by = c("Subject", "VI" )]
names( event_records )[3:5] = c("time", "event", "Session")

# Molar statistics
response_rate = event_records[ , {
    resp_rate = sum( event == "resp_time" ) / 1800
    list( resp_rate = resp_rate, VI = VI[[1]] )
}, by = c("Session", "Subject") ][ , {
    list(statistic = list(resp_rate), name = "resp_rate", VI = 0 )
    }, by = c("Subject") ]

distributions = event_records[ ,{
    prp = simple_ixyi( event, time, "rft_time", "resp_time", "start" )["ixyi"]
    irt = simple_ixyi( event, time, "resp_time", "resp_time", c("rft_time","start") )["ixyi"]
    iri = simple_ixyi( event, time, "rft_time", "rft_time", "start" )["ixyi"]
    list( statistic = c(prp, irt, iri),
        name = paste0( VI, "_", c("prp", "irt", "iri") ) )
}, by = c("Subject", "VI") ]


#Save the data
behavioural_profile = rbindlist( list(distributions, response_rate ), use.names = T )
save( behavioural_profile, file = "behavioural_profile.RData" )