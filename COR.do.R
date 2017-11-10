COR.do = function( org, rft_conditions, session, COR_fx ){

    VI_iri = rft_conditions[ session ]
    event_record = data.table( time = rep( NaN, 50000 ), event = "", Session = session, VI = VI_iri )
    er_headers = c("time", "event")
    er = 1L
    set( event_record, i = er, j = er_headers, value = list(0,"start") )
    er = er + 1L
    
    if ( session == 1 ){
        org$reserve_value = org$initial_reserve
    }
    org$time = 0

    rft_time = COR_fx$rft_schedule( org, VI_iri )
    session_duration = org$session_duration
    iri_resp = 0
    iri_resp_times = rep( NaN, 1000 )

    repeat{
        response_time = COR_fx$emission( org )

        if ( is.nan( response_time ) | response_time > session_duration ) break

        iri_resp = iri_resp + 1
        iri_resp_times[ iri_resp ] = response_time
        
        er = er + 1L
        set( event_record, er, er_headers, list(response_time, "resp_time") )
        org$time = response_time

        COR_fx$depletion(org)

        if ( response_time >= rft_time ){
            
            er = er+1L
            set( event_record, er, er_headers, list(org$time, "rft_time") )
            
            COR_fx$DOR( org, iri_resp_times )
            # 2 for rft delivery, 1 for PRP
            org$time = org$time + 3
            
            iri_resp_times[] = NaN
            iri_resp = 0

            rft_time = COR_fx$rft_schedule( org, VI_iri )
        }
    }

    event_record = event_record[ !is.nan(time) ]
}

simulate_COR = function( org, rft_conditions, COR_fx, COR.do ){
    rbindlist( lapply( 1:length(rft_conditions), function(x){
        COR.do( org, rft_conditions, session = x, COR_fx )
    } ) )
}

simulate_COR2 = function( COR_params, new_params, rft_conditions, COR_fx, COR.do ){
    org = list2env( COR_params )
    list2env( new_params, envir = org )
    rbindlist( lapply( 1:length(rft_conditions), function(x){
        COR.do( org, rft_conditions, session = x, COR_fx )
    } ) )
}
