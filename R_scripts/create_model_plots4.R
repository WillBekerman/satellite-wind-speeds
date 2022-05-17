
# load libraries
# Assume we are running from the R_scripts directory
# source functions
source("../R/load_data.R")

# load in fit models
save_dir <- "../figures"
models_dir <- "../model_fits"
model_files <- list.files( models_dir, pattern = "\\.RData$", full.names = TRUE )
model_files <- model_files[ grep("fit4", model_files) ]
nperiods <- length(model_files)
nsat <- 8

nmeans <- 16
bias_est <- array( NA, c(nmeans, nmeans, nperiods) )
bias_var <- array( NA, c(nmeans, nmeans, nperiods) )

# read the models into a list
# list index of list is time period
for(j in 1:nperiods){

    model <- loadRData( model_files[j] )

    # create a matrix with pairwise biases and standard errors
    bhat <- model$betahat
    bcov <- model$betacov

    bhat <- c(0,bhat)
    names(bhat)[1] <- "sat_ant1P"

    bcov <- rbind( rep(0, ncol(bcov)), bcov )
    bcov <- cbind( rep(0, nrow(bcov)), bcov )
    rownames(bcov)[1] <- "sat_ant1P"
    colnames(bcov)[1] <- "sat_ant1P"
    
    sub_terms <- names(bhat)[ grep("sat_ant", names(bhat) ) ]
    
    for(j1 in 1:nmeans){ for(j2 in 1:nmeans){
        s1 <- sub_terms[j1]
        s2 <- sub_terms[j2]
        bias_est[j1,j2,j] <- bhat[ s1 ] - bhat[ s2 ]
        bias_var[j1,j2,j] <- bcov[s1,s1] + bcov[s2,s2] - 2*bcov[s1,s2] 
    }}
            
}







cc <- paste0( rep(1:8,each=2), rep(c("P","S"),8) )
par(mfrow=c(1,3), mar = c(4,4,4,8) )
fields::image.plot(1:16, 1:16, mat_bias, axes = FALSE, ann = FALSE)
mtext("Bias", line = 1 )
axis(1, at = 1:16, labels = cc )
axis(2, at = 1:16, labels = cc )
fields::image.plot(1:16, 1:16, mat_se, axes = FALSE, ann = FALSE)
mtext("SE Bias", line = 1 )
axis(1, at = 1:16, labels = cc )
axis(1, at = 1:16, labels = cc )
axis(2, at = 1:16, labels = cc )
fields::image.plot(1:16, 1:16, abs(mat_bias/mat_se), axes = FALSE, ann = FALSE)
mtext("abs t-stat", line = 1 )
axis(1, at = 1:16, labels = cc )
axis(1, at = 1:16, labels = cc )
axis(2, at = 1:16, labels = cc )


    









