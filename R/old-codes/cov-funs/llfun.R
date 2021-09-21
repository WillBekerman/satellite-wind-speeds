llfun_1 = function(covparms, locs, windspeed){
  
  cov_mat = covfun_1(covparms, locs)
  y = windspeed
  n = length(y)
  
  x = rep(1, n)
  b = mean(y)
  
  chol_mat = t(chol(cov_mat))
  log_det = 2*sum( log( diag(chol_mat) ) )
  
  z = forwardsolve(chol_mat, y-b*x) # L^-1 * y-bx
  quad_form = sum( z^2 )
  
  ll = -(n/2)*log(2*pi) - (1/2)*(log_det) - (1/2)*(quad_form)
  return(ll)
  
}


############## %%%%%%%%%%%%%%%%%% ###############


llfun_2 = function(covparms, locs, windspeed){
  
  cov_mat = covfun_2(covparms, locs)
  y = windspeed
  n = length(y)
  
  mu_vec = rep(0, length( unique(locs[,4]) ))
  
  for (mu in 1:length(mu_vec)){
    mu_vec[mu] = mean(y[which(locs[,4]==(mu-1))])
  }
  
  xmat = matrix(0, nrow = n, ncol = length(mu_vec))
  for (col in 1:ncol(xmat)){
    sat_obs = which(locs[,4]==(col-1))
    xmat[sat_obs,col] = 1
  }
  
  x = xmat
  b = mu_vec
  
  chol_mat = t(chol(cov_mat))
  log_det = 2*sum( log( diag(chol_mat) ) )
  
  z = forwardsolve(chol_mat, y-x%*%b) # L^-1 * y-xb
  quad_form = sum( z^2 )
  
  ll = -(n/2)*log(2*pi) - (1/2)*(log_det) - (1/2)*(quad_form)
  return(ll)
  
}


############## %%%%%%%%%%%%%%%%%% ###############


llfun_intermediate = function(covparms, locs, windspeed){
  
  cov_mat = covfun_intermediate(covparms, locs)
  y = windspeed
  n = length(y)
  
  x = rep(1, n)
  b = mean(y)
  
  chol_mat = t(chol(cov_mat))
  log_det = 2*sum( log( diag(chol_mat) ) )
  
  z = forwardsolve(chol_mat, y-b*x) # L^-1 * y-bx
  quad_form = sum( z^2 )
  
  ll = -(n/2)*log(2*pi) - (1/2)*(log_det) - (1/2)*(quad_form)
  return(ll)
  
}


############## %%%%%%%%%%%%%%%%%% ###############


llfun_3 = function(covparms, locs, windspeed){
  
  cov_mat = covfun_3(covparms, locs)
  y = windspeed
  n = length(y)
  
  x = rep(1, n)
  b = mean(y)
  
  chol_mat = t(chol(cov_mat))
  log_det = 2*sum( log( diag(chol_mat) ) )
  
  z = forwardsolve(chol_mat, y-b*x) # L^-1 * y-bx
  quad_form = sum( z^2 )
  
  ll = -(n/2)*log(2*pi) - (1/2)*(log_det) - (1/2)*(quad_form)
  return(ll)
  
}

