# load libraries
library(ggplot2)
library(fields)

# Assume we are running from the R_scripts directory
# source functions
source("../R/load_data.R")

date_1 <- as.Date('2019-09-28')
date_2 <- as.Date('2019-10-04')
dates <- seq(date_1, to = date_2, by = 'day')

`%nin%` <- Negate('%in%')
dates <- dates[dates %nin% c(seq(as.Date("2020-02-01"), as.Date("2020-02-14"), by = 'day'))]
dates <- dates[dates %nin% c(seq(as.Date("2020-06-13"), as.Date("2020-06-19"), by = 'day'))]

dist_l = jws_l = cws_l = list(numeric(), numeric(), numeric(), numeric(), numeric(), numeric(), numeric(), numeric())

#library(profvis)
#profvis({
for (j in 1:length(dates)){
  cat('\nDate: ', as.character(dates[j]))
  
  cygnss_dir <- "../data/processed_daily_cygnss"
  filelist <- list.files(path = cygnss_dir, pattern = "\\.RData$")
  cygnss_dat <- matrix(NA, nrow = 0, ncol = 8)
  file_ind <- grep( dates[j], filelist )
  temp_data <- loadRData( file.path( cygnss_dir, filelist[ file_ind ] ) )
  cygnss_dat <- rbind( cygnss_dat, temp_data )
  colnames(cygnss_dat) <- c("sat", "lat", "lon", "sc_lat", "sc_lon", "wind_speed", "wind_speed_uncertainty", "time")
  cygnss_dat[,'time'] <- cygnss_dat[,'time'] - min(cygnss_dat[,'time'])
  cygnss_dat <- na.omit(cygnss_dat)
  cygnss_dat[,'sat'][cygnss_dat[,'sat'] == 247] <- 1
  cygnss_dat[,'sat'][cygnss_dat[,'sat'] == 249] <- 2
  cygnss_dat[,'sat'][cygnss_dat[,'sat'] == 43] <- 3
  cygnss_dat[,'sat'][cygnss_dat[,'sat'] == 44] <- 4
  cygnss_dat[,'sat'][cygnss_dat[,'sat'] == 47] <- 5
  cygnss_dat[,'sat'][cygnss_dat[,'sat'] == 54] <- 6
  cygnss_dat[,'sat'][cygnss_dat[,'sat'] == 55] <- 7
  cygnss_dat[,'sat'][cygnss_dat[,'sat'] == 73] <- 8
  
  jason_dir <- "../data/processed_daily_jason"
  filelist <- list.files(path = jason_dir, pattern = "\\.RData$")
  jason_dat <- matrix(NA, nrow = 0, ncol = 8)
  file_ind <- grep( dates[j], filelist )
  temp_data <- loadRData( file.path( jason_dir, filelist[ file_ind ] ) )
  jason_dat <- rbind( jason_dat, temp_data )
  colnames(jason_dat) <- c("lat", "lon", "surface_type", "alt", "bathymetry", "wind_speed", "wind_speed_mle3", "time")
  jason_dat[,'time'] <- jason_dat[,'time'] - min(jason_dat[,'time'])
  jason_dat <- na.omit(jason_dat)
  rm(temp_data); gc()
  jason_dat <- jason_dat[ jason_dat[,'surface_type'] == 0 , ]
  
  
  ### Distances
  num_hours = 24
  num_sec = 3600*num_hours
  by = 1*60 # one minute
  tolerance = 0.5*60 # thirty seconds
  
  for (time in (seq(0, num_sec, by = by))) {

    cat(paste0(round(time/num_sec,4)*100,"%\n"))
      
    # go to next time in seq if cygnss_dat or jason_dat does not have measurement at time
    # within tolerance
    if ( !any( abs(time - jason_dat[,ncol(jason_dat)]) < tolerance ) || !any( abs(time - cygnss_dat[,ncol(cygnss_dat)]) < tolerance ) ) {
      next
    }
    
    for (satnum in 1:8){
    
      cygnss_satnum <- cygnss_dat[which(cygnss_dat$sat==satnum),]
      jws_vec = cws_vec = dist_vec = numeric()
      
      if (any( abs(time - cygnss_satnum$time) < tolerance ) ) {
        i <- which(abs(time - cygnss_satnum$time) %in% abs(time - cygnss_satnum$time[which(abs(time - cygnss_satnum$time) < tolerance )]) )
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
#})

t1 = 100
t2 = 200
t3 = 500

par(mfrow = c(1,2))

for (sat in 1:8){
  
  jason_speed = jws_l[[sat]][which(dist_l[[sat]]<t3 & dist_l[[sat]]>t2)]
  cygnss_speed = cws_l[[sat]][which(dist_l[[sat]]<t3 & dist_l[[sat]]>t2)]
  sat_col = rep(1, length(cws_l[[sat]][which(dist_l[[sat]]<t3 & dist_l[[sat]]>t2)]))
  plot(jason_speed, cygnss_speed, xlab = 'Jason WS (m/s)', ylab = 'CYGNSS WS (m/s)',
       main = paste0('CYGNSS Satellite ', sat, ' vs. Jason'), xlim = c(0,20), ylim = c(0,20), col = 2, pch = 19, cex = .95) # circle
  
  jason_speed = jws_l[[sat]][which(dist_l[[sat]]<t2 & dist_l[[sat]]>t1)]
  cygnss_speed = cws_l[[sat]][which(dist_l[[sat]]<t2 & dist_l[[sat]]>t1)]
  sat_col = rep(1, length(cws_l[[sat]][which(dist_l[[sat]]<t2 & dist_l[[sat]]>t1)]))
  points(jason_speed, cygnss_speed, col = 3, pch = 19, cex = .95) # square
  
  jason_speed = jws_l[[sat]][which(dist_l[[sat]]<t1)]
  cygnss_speed = cws_l[[sat]][which(dist_l[[sat]]<t1)]
  sat_col = rep(1, length(cws_l[[sat]][which(dist_l[[sat]]<t1)]))
  points(jason_speed, cygnss_speed, col = 4, pch = 19, cex = .95) # triangle
  
  legend('bottomright', legend= c( paste0('< ',t3,' km'), paste0('< ',t2,' km'), paste0('< ',t1,' km') ), fill=2:4, cex=0.8)
  abline(0,1)
  
}

#save_dir <- "../figures"
#pdf(file.path( save_dir, 'three_plot.pdf' ), width = 12, height = 4)
#grid.arrange(bias_plot, nugget_plot, sat_variance_plot, nrow = 1 )
#dev.off()

rm(list = ls())
gc()
