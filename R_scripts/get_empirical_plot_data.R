# load libraries
library(fields)

# Assume we are running from the R_scripts directory
# source functions
source("../R/load_data.R")

# date_1 <- as.Date('2019-09-28')
# date_2 <- as.Date('2020-09-25')
# dates <- seq(date_1, to = date_2, by = 'day')
# 
# # filter out "bad" dates, i.e. dates with no Jason data
# `%nin%` <- Negate('%in%')
# dates <- dates[dates %nin% seq(as.Date("2020-02-01"), as.Date("2020-02-14"), by = 'day')]
# dates <- dates[dates %nin% seq(as.Date("2020-06-13"), as.Date("2020-06-19"), by = 'day')]

## in form of c("2020-MM-DD, "2020-MM-DD", "antenna")
## example
## args <- c("2020-01-01","2020-01-07", "starboard")
args <- commandArgs(trailingOnly=TRUE)

# process date parameters
# script assumes we have files for each date in the range
date_1 <- as.Date(args[1])
date_2 <- as.Date(args[2])
dates <- seq(date_1, to = date_2, by = 'day')

# process antenna parameter
antenna <- 'starboard'
antenna_num <- ifelse(antenna == 'starboard', 2, 3) # corresponds to cygnss dataset lables

# initialize lists that we intend to save
num_cygnss <- 8
dist_l <- list()
jws_l <- list()
cws_l <- list()
for(j in 1:num_cygnss){
    dist_l[[j]] <- numeric()
    jws_l[[j]] <- numeric()
    cws_l[[j]] <- numeric()
}

# directory for cygnss data, cygnss RData files
cygnss_dir <- "../data/processed_daily_cygnss"
filelist_cygnss <- list.files(path = cygnss_dir, pattern = "\\.RData$")

# directory for jason data, jason RData files
jason_dir <- "../data/processed_daily_jason"
filelist_jason <- list.files(path = jason_dir, pattern = "\\.RData$")

# these are the cygnss and jason variable names we want to use
cygnss_vars <- c(
    "sat", "lat", "lon", "sc_lat", "sc_lon",
    "wind_speed", "wind_speed_uncertainty", "time", "antenna"
)
  
jason_vars <- c(
    "lat", "lon", "surface_type", "alt", "bathymetry",
    "wind_speed", "wind_speed_mle3", "time"
)

# loop over the dates
for (j in 1:length(dates)){

  cat('\nDate: ', as.character(dates[j]))

  # load in the cygnss data file for dates[j]
  file_ind <- grep( dates[j], filelist_cygnss )
  cygnss_dat <- loadRData( file.path( cygnss_dir, filelist_cygnss[ file_ind ] ) )
  colnames(cygnss_dat) <- cygnss_vars
    
  # get rid of missing values
  cygnss_dat <- na.omit(cygnss_dat)
  cygnss_dat <- cygnss_dat[cygnss_dat$antenna == antenna_num,]

  # relabel the satellite numbers
  satnum_dict <- c("247"=1, "249"=2, "43"=3, "44"=4, "47"=5, "54"=6, "55"=7, "73"=8)
  for(s in 1:num_cygnss){
      ii <- cygnss_dat[,'sat'] == as.numeric( names(satnum_dict)[s] )
      cygnss_dat[ii,'sat'] <- satnum_dict[s]
  }
  
  # load in the jason data
  file_ind <- grep( dates[j], filelist_jason )
  jason_dat <- loadRData( file.path( jason_dir, filelist_jason[ file_ind ] ) )
  colnames(jason_dat) <- jason_vars

  # get rid of missing values and land values
  jason_dat <- na.omit(jason_dat)
  jason_dat <- jason_dat[ jason_dat[,'surface_type'] == 0 , ]

  # reset time relative to minimum time across cygnss and jason records (set to zero)
  min_time <- min(cygnss_dat[,'time'], jason_dat[,'time'])
  cygnss_dat[,'time'] <- cygnss_dat[,'time'] - min_time
  jason_dat[,'time'] <- jason_dat[,'time'] - min_time


  ### Distances
  num_hours = 24
  num_sec = 3600*num_hours
  by = 60*60*2 # two hours
  tolerance = 60*60*1 # one hour
  
  for (time in (seq(0, num_sec, by = by))) {
    
    cat(paste0(round(time/num_sec,4)*100,"%\n"))
    
    # go to next time in seq if cygnss_dat or jason_dat does not have measurement at time
    # within tolerance
    if ( all( abs(time - jason_dat$time)  > tolerance ) ||
         all( abs(time - cygnss_dat$time) > tolerance )
    ){
      next
    }
    
    for (satnum in 1:num_cygnss){
      
      # extract data from cygnss_dat corresponding to satnum
      cygnss_satnum <- cygnss_dat[which(cygnss_dat$sat==satnum),]
      
      # initialize lists that we intend to save
      jws_vec = cws_vec = dist_vec = numeric()
      
      if ( any( abs(time - cygnss_satnum$time) < tolerance ) ) {
        i <- which(abs(time - cygnss_satnum$time) %in%
                   abs(time - cygnss_satnum$time[which(abs(time - cygnss_satnum$time) < tolerance )]) )
        ij <- which(abs(time - jason_dat$time) %in% abs(time - jason_dat$time[which(abs(time - jason_dat$time) < tolerance )]) )
        
        x1 <- cbind(jason_dat$lon[ij], jason_dat$lat[ij])
        x2 <- cbind(cygnss_satnum$lon[i], cygnss_satnum$lat[i])
        
        jws <- jason_dat$wind_speed[ij]
        cws <- cygnss_satnum$wind_speed[i]
        
        dist_mat <- matrix(NA, nrow=length(jws), ncol=length(cws))
        dist_mat <- rdist.earth( x1, x2, miles = FALSE )
        
        min_ix <- which(dist_mat==min(dist_mat), arr.ind = T)
        jws_vec <- c(jws_vec, jws[min_ix[1]])
        cws_vec <- c(cws_vec, cws[min_ix[2]])
        dist_vec <- c(dist_vec, min(dist_mat))
      }
      
      jws_l[[satnum]] <- c(jws_l[[satnum]], jws_vec)
      cws_l[[satnum]] <- c(cws_l[[satnum]], cws_vec)
      dist_l[[satnum]] <- c(dist_l[[satnum]], dist_vec)
      
    }
  }
}

save_dir <- "../data/empirical_plot_data"
#save(dist_l, jws_l, cws_l, file = file.path(save_dir, "empirical_plot_data_lists.RData"))

filename <- paste0('empirical_plot_data_lists-', dates[1], '-to-', dates[length(dates)], '-', antenna, '.RData')
save(dist_l, jws_l, cws_l, file = file.path(save_dir, filename))

rm(list = ls())
gc()
