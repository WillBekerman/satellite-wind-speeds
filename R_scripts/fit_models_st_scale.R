
# extends fit_models.R to include a refit with estimated st_scale

## in form of c("2020-MM-DD, "2020-MM-DD")
## example
## args <- c("2020-01-01","2020-01-07")
args <- commandArgs(trailingOnly=TRUE) 

# process date parameters
# script assumes we have files for each date in the range
date_1 <- as.Date(args[1])
date_2 <- as.Date(args[2])
covfun <- "matern_spacetime"
dates <- seq(date_1, to = date_2, by = 'day')

# source functions
source("../R/load_data.R")

## OPEN APPROPRIATE CYGNSS FILES AND CREATE A CYGNSS DATA MATRIX

# list only the files with the .RData extension
# Assume we are running from the R_scripts directory
cygnss_dir <- "../data/processed_daily_cygnss"
filelist <- list.files(path = cygnss_dir, pattern = "\\.RData$")

# find each day's file, and append to the data matrix
cygnss_dat <- matrix(NA, nrow = 0, ncol = 8)
for(j in 1:length(dates)){
    file_ind <- grep( dates[j], filelist )
    temp_data <- loadRData( file.path( cygnss_dir, filelist[ file_ind ] ) )
    cygnss_dat <- rbind( cygnss_dat, temp_data )
}
# assign variable names
colnames(cygnss_dat) <-
    c("sat", "lat", "lon", "sc_lat", "sc_lon", "wind_speed", "wind_speed_uncertainty", "time", "antenna")

# remove missing values
cygnss_dat <- na.omit(cygnss_dat)
cygnss_dat <- cygnss_dat[cygnss_dat$antenna == 2 | cygnss_dat$antenna == 3,]

# standardize satellite naming
cygnss_dat[,'sat'][cygnss_dat[,'sat'] == 247] <- 1
cygnss_dat[,'sat'][cygnss_dat[,'sat'] == 249] <- 2
cygnss_dat[,'sat'][cygnss_dat[,'sat'] == 43] <- 3
cygnss_dat[,'sat'][cygnss_dat[,'sat'] == 44] <- 4
cygnss_dat[,'sat'][cygnss_dat[,'sat'] == 47] <- 5
cygnss_dat[,'sat'][cygnss_dat[,'sat'] == 54] <- 6
cygnss_dat[,'sat'][cygnss_dat[,'sat'] == 55] <- 7
cygnss_dat[,'sat'][cygnss_dat[,'sat'] == 73] <- 8


## OPEN APPROPRIATE JASON FILES AND CREATE A JASON DATA MATRIX

# list only the files with the .RData extension
# Assume we are running from the R_scripts directory
jason_dir <- "../data/processed_daily_jason"
filelist <- list.files(path = jason_dir, pattern = "\\.RData$")

# find each day's file, and append to the data matrix
jason_dat <- matrix(NA, nrow = 0, ncol = 8)
for(j in 1:length(dates)){
    print(j)
    file_ind <- grep( dates[j], filelist )
    temp_data <- loadRData( file.path( jason_dir, filelist[ file_ind ] ) )
    jason_dat <- rbind( jason_dat, temp_data )
}
# assign variable names
colnames(jason_dat) <-
    c("lat", "lon", "surface_type", "alt", "bathymetry", "wind_speed", "wind_speed_mle3", "time")

# remove missing values
jason_dat <- na.omit(jason_dat)
rm(temp_data); gc()


# since cygnss does not record measurements over
# land and Jason does (but are told to be inaccurate), we only keep Jason observations
# over open oceans/semi-enclosed seas
# surface_type: 0 = open oceans or semi-enclosed seas; 1 = enclosed seas or lakes; 2 = continental ice; 3 = land
jason_dat <- jason_dat[ jason_dat[,'surface_type'] == 0 , ]

numsat <- 20000
set.seed(100)

models <- list()

for( cyg_sat in 1:8 ){
  
    cat('\nCYGNSS Satellite Number:', cyg_sat)

    # pull out data from satellite cyg_sat
    dat_this_cyg <- cygnss_dat[ cygnss_dat[,'sat'] == cyg_sat , ]
    
    # handle case of zero cyg_sat observations
    if (nrow(dat_this_cyg) == 0){
      models[[cyg_sat]] <- NA
      next
    }

    # sample numsat observations from cygnss
    obs <- sort( sample(1:nrow(dat_this_cyg), numsat) )

    # define the cygnss dataset
    dat_cyg <- as.data.frame( dat_this_cyg[ obs, c("lat","lon","time","wind_speed","antenna") ] )
    # dat_cyg$sat_num <- 1
    dat_cyg$starboard <- ifelse(dat_cyg$antenna == 2, 1, 0)
    dat_cyg$port <- ifelse(dat_cyg$antenna == 3, 1, 0)
    dat_cyg <- subset(dat_cyg, select = -antenna)

    # sample numsat observations from jason
    obs <- sort( sample(1:nrow(jason_dat), numsat) )

    # define the jason dataset
    dat_jas <- as.data.frame( jason_dat[ obs, c("lat","lon","time","wind_speed") ] )
    # dat_jas$sat_num <- 0
    dat_jas$starboard <- 0
    dat_jas$port <- 0

    # combine cygnss and jason datasets
    dat <- rbind( dat_cyg, dat_jas )
    
    # standardize time as number of hours since earliest measurement in dat
    dat$time <- dat$time - min(dat$time)
    dat$time <- dat$time/3600

    # create x,y,z coordinates
    dat$x <- 6371*cos( dat$lat*pi/180 )*cos( (dat$lon-180)*pi/180 )
    dat$y <- 6371*cos( dat$lat*pi/180 )*sin( (dat$lon-180)*pi/180 )
    dat$z <- 6371*sin( dat$lat*pi/180 )

    # create variables for design matrix
    dat$intercept <- 1
    dat$radians <- dat$lat*pi/180
    dat$radians_sq <- dat$radians^2
    dat$radians_cub <- dat$radians^3
    
    # create y, locs, X for input to fit_model
    y <- dat$wind_speed
    locs <- as.matrix( dat[ , c("x","y","z","time") ] )
    # X <- as.matrix( dat[, c("intercept","time","sat_num","radians","radians_sq","radians_cub")] )
    X <- as.matrix( dat[, c("intercept","time","starboard","port","radians","radians_sq","radians_cub")] )
    
    # fit the model, then refit with estimated st_scale
    model <- GpGp::fit_model(y, locs, X, covfun_name = covfun, st_scale = c(2000,3) )
    model <- GpGp::fit_model(
        y, locs, X, 
        covfun_name = covfun, 
        start_parms = model$covparms,
        st_scale = model$covparms[2:3], m_seq = 30 
    )

    # save it
    models[[cyg_sat]] <- model
}

## takes ~ 2.5 hrs w/ 20000 obs/sat

save_dir <- "../model_fits"
filename <- paste0('fit_st_scale-', dates[1], '-to-', dates[length(dates)], '.RData')
save(models, file = file.path(save_dir, filename) )


