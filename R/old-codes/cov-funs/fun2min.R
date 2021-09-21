source('R/cov-funs/covfun.R')
source('R/cov-funs/llfun.R')

fun2min_1 = function(covparms){
  
  ll = llfun_1(covparms, locs, windspeed)
  return(-ll)
    
}

fun2min_2 = function(covparms){
  
  ll = llfun_2(covparms, locs, windspeed)
  return(-ll)
  
}

fun2min_intermediate = function(covparms){
  
  ll = llfun_intermediate(covparms, locs, windspeed)
  return(-ll)
  
}

fun2min_3 = function(covparms){
  
  ll = llfun_3(covparms, locs, windspeed)
  return(-ll)
  
}

