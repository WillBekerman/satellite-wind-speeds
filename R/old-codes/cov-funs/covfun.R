covfun_1 = function(covparms, locs){
  # covparms: any parameters defining covariance (length = 3)
  # locs: any information specific to observations that is relevant to the convariance
  # (nx4 matrix) [latitudes, longitudes, times]
  # response y: nx1 vector of windspeeds
  
  # returns an nxn matrix of covariances
  
  ## Assign local variables
  sigma_sq = covparms[1]
  alpha_1 = covparms[2]
  alpha_2 = covparms[3]
  
  lats = locs[,1]
  longs = locs[,2]
  longs = longs - 180 # get into range (-180, 180)
  
  lats = lats*pi/180 # convert to radians
  longs = longs*pi/180
  
  x = 6371*cos(lats)*cos(longs)
  y = 6371*cos(lats)*sin(longs)
  z = 6371*sin(lats)
  
  times = locs[,3]
  
  ## Compute covariances
  n = nrow(locs) # number of observations
  covmat = matrix(NA, n, n)
  
  for (obs1 in 1:n){
    
    x1 = x[obs1]
    y1 = y[obs1]
    z1 = z[obs1]
    t1 = times[obs1]
    
    for (obs2 in (obs1:n)){
      
      x2 = x[obs2]
      y2 = y[obs2]
      z2 = z[obs2]
      t2 = times[obs2]
      
      dist = sqrt( (x2-x1)^2 + (y2-y1)^2 + (z2-z1)^2 )
      k0 = sigma_sq*exp( -1*( sqrt( (dist^2 / alpha_1^2) + ((t1-t2)^2 / alpha_2^2) ) ) )
      
      covmat[obs1,obs2] = k0
    
      }
  }
    
  covmat[lower.tri(covmat)] = t(covmat)[lower.tri(covmat)]  
  
  return(covmat)
  
}


############## %%%%%%%%%%%%%%%%%% ###############


covfun_2 = function(covparms, locs){
  # covparms: any parameters defining covariance (length = 12)
  # locs: any information specific to observations that is relevant to the convariance
  # (nx4 matrix) [latitudes, longitudes, times, satellite numbers]
  # response y: nx1 vector of windspeeds
  
  # returns an nxn matrix of covariances
  
  ## Assign local variables
  sigma_sq = covparms[1]
  alpha_1 = covparms[2]
  alpha_2 = covparms[3]
  
  sig_sq_0 = covparms[4]
  sig_sq_1 = covparms[5]
  sig_sq_2 = covparms[6]
  sig_sq_3 = covparms[7]
  sig_sq_4 = covparms[8]
  sig_sq_5 = covparms[9]
  sig_sq_6 = covparms[10]
  sig_sq_7 = covparms[11]
  sig_sq_8 = covparms[12]
  
  sig_j = c(sig_sq_0, sig_sq_1, sig_sq_2, sig_sq_3, sig_sq_4, sig_sq_5, sig_sq_6, 
            sig_sq_7, sig_sq_8)
  
  lats = locs[,1]
  longs = locs[,2]
  longs = longs - 180 # get into range (-180, 180)
  
  lats = lats*pi/180 # convert to radians
  longs = longs*pi/180
  
  x = 6371*cos(lats)*cos(longs)
  y = 6371*cos(lats)*sin(longs)
  z = 6371*sin(lats)
  
  times = locs[,3]
  sat_nums = locs[,4]
  
  ## Compute covariances
  n = nrow(locs) # number of observations
  covmat = matrix(NA, n, n)
  
  for (obs1 in 1:n){
    
    x1 = x[obs1]
    y1 = y[obs1]
    z1 = z[obs1]
    t1 = times[obs1]
    sat_num1 = sat_nums[obs1]
    
    for (obs2 in (obs1:n)){
      
      x2 = x[obs2]
      y2 = y[obs2]
      z2 = z[obs2]
      t2 = times[obs2]
      sat_num2 = sat_nums[obs2]
      
      dist = sqrt( (x2-x1)^2 + (y2-y1)^2 + (z2-z1)^2 )
      k0 = sigma_sq*exp( -1*( sqrt( (dist^2 / alpha_1^2) + ((t1-t2)^2 / alpha_2^2) ) ) )
      
      if (obs1 == obs2) {
        j = sat_nums[obs1]
        k0 = k0 + sig_j[(j+1)] #get( paste0('sig_sq_', paste(j)) )
      }
      
      covmat[obs1,obs2] = k0
      
    }
  }
  
  covmat[lower.tri(covmat)] = t(covmat)[lower.tri(covmat)]  
  
  return(covmat)
  
}


############## %%%%%%%%%%%%%%%%%% ###############


covfun_intermediate = function(covparms, locs){
  # covparms: any parameters defining covariance (length = 13)
  # locs: any information specific to observations that is relevant to the convariance
  # (nx4 matrix) [latitudes, longitudes, times, satellite numbers]
  # response y: nx1 vector of windspeeds
  
  # returns an nxn matrix of covariances
  
  ## Assign local variables
  sigma_sq = covparms[1]
  alpha_1 = covparms[2]
  alpha_2 = covparms[3]
  
  sig_obs = covparms[4]
  tau_sq = covparms[5]
  
  lats = locs[,1]
  longs = locs[,2]
  longs = longs - 180 # get into range (-180, 180)
  
  lats = lats*pi/180 # convert to radians
  longs = longs*pi/180
  
  x = 6371*cos(lats)*cos(longs)
  y = 6371*cos(lats)*sin(longs)
  z = 6371*sin(lats)
  
  times = locs[,3]
  sat_nums = locs[,4]
  
  ## Compute covariances
  n = nrow(locs) # number of observations
  covmat = matrix(NA, n, n)
  
  for (obs1 in 1:n){
    
    x1 = x[obs1]
    y1 = y[obs1]
    z1 = z[obs1]
    t1 = times[obs1]
    sat_num1 = sat_nums[obs1]
    
    for (obs2 in (obs1:n)){
      
      x2 = x[obs2]
      y2 = y[obs2]
      z2 = z[obs2]
      t2 = times[obs2]
      sat_num2 = sat_nums[obs2]
      
      dist = sqrt( (x2-x1)^2 + (y2-y1)^2 + (z2-z1)^2 )
      k0 = sigma_sq*exp( -1*( sqrt( (dist^2 / alpha_1^2) + ((t1-t2)^2 / alpha_2^2) ) ) )
      
      j1 = sat_nums[obs1]
      j2 = sat_nums[obs2]
      
      if(j1 == j2){
        k0 = k0 + tau_sq
        if (obs1 == obs2) {
          k0 = k0 + sig_obs
        }
      }
      
      covmat[obs1,obs2] = k0
      
    }
  }
  
  covmat[lower.tri(covmat)] = t(covmat)[lower.tri(covmat)]  
  
  return(covmat)
  
}


############## %%%%%%%%%%%%%%%%%% ###############


covfun_3 = function(covparms, locs){
  # covparms: any parameters defining covariance (length = 13)
  # locs: any information specific to observations that is relevant to the convariance
  # (nx4 matrix) [latitudes, longitudes, times, satellite numbers]
  # response y: nx1 vector of windspeeds
  
  # returns an nxn matrix of covariances
  
  ## Assign local variables
  sigma_sq = covparms[1]
  alpha_1 = covparms[2]
  alpha_2 = covparms[3]
  
  sig_sq_0 = covparms[4]
  sig_sq_1 = covparms[5]
  sig_sq_2 = covparms[6]
  sig_sq_3 = covparms[7]
  sig_sq_4 = covparms[8]
  sig_sq_5 = covparms[9]
  sig_sq_6 = covparms[10]
  sig_sq_7 = covparms[11]
  sig_sq_8 = covparms[12]
  
  sig_j = c(sig_sq_0, sig_sq_1, sig_sq_2, sig_sq_3, sig_sq_4, sig_sq_5, sig_sq_6, 
            sig_sq_7, sig_sq_8)
  
  tau_sq = covparms[13]
  
  lats = locs[,1]
  longs = locs[,2]
  longs = longs - 180 # get into range (-180, 180)
  
  lats = lats*pi/180 # convert to radians
  longs = longs*pi/180
  
  x = 6371*cos(lats)*cos(longs)
  y = 6371*cos(lats)*sin(longs)
  z = 6371*sin(lats)
  
  times = locs[,3]
  sat_nums = locs[,4]
  
  ## Compute covariances
  n = nrow(locs) # number of observations
  covmat = matrix(NA, n, n)
  
  for (obs1 in 1:n){
    
    x1 = x[obs1]
    y1 = y[obs1]
    z1 = z[obs1]
    t1 = times[obs1]
    sat_num1 = sat_nums[obs1]
    
    for (obs2 in (obs1:n)){
      
      x2 = x[obs2]
      y2 = y[obs2]
      z2 = z[obs2]
      t2 = times[obs2]
      sat_num2 = sat_nums[obs2]
      
      dist = sqrt( (x2-x1)^2 + (y2-y1)^2 + (z2-z1)^2 )
      k0 = sigma_sq*exp( -1*( sqrt( (dist^2 / alpha_1^2) + ((t1-t2)^2 / alpha_2^2) ) ) )
      
      j1 = sat_nums[obs1]
      j2 = sat_nums[obs2]
      
      if(j1 == j2){
        k0 = k0 + tau_sq
        if (obs1 == obs2) {
          k0 = k0 + sig_j[(j1+1)] #get( paste0('sig_sq_', paste(j)) )
        }
      }
      
      covmat[obs1,obs2] = k0
      
    }
  }
  
  covmat[lower.tri(covmat)] = t(covmat)[lower.tri(covmat)]  
  
  return(covmat)
  
}

