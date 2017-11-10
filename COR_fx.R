# COR functions

DOR = function( org, iri_resp_times ){
    iri_resp_times = iri_resp_times[ !is.nan(iri_resp_times) ]
    iri_resp_times = max(iri_resp_times)-iri_resp_times
    reserve_value = sum( org$DOR_max * exp( -org$DOR_scale * iri_resp_times ) )+ org$reserve_value
    org$reserve_value = min( 1, reserve_value )
}

depletion = function( org ){
    reserve_value = org$reserve_value - org$depletion_constant
    org$reserve_value = max( 0, reserve_value )
}

rft_schedule = function( org, VI_iri ){
    rexp( 1,  1/VI_iri ) + org$time + org$shift
}

emission = function( org ){
    prob_to_rate = -log( 1 - org$reserve_value )
    rexp( 1, prob_to_rate ) + org$min_irt + org$time
}

COR_fx = list( DOR = DOR, depletion = depletion, rft_schedule = rft_schedule, emission = emission )
