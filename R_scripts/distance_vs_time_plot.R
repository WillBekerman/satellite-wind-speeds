# load libraries
library(fields)
library(maps)
library(ggplot2)

# Assume we are running from the R_scripts directory
# source functions
source("../R/load_data.R")

## Generate a plot comparing wind speed measurements recorded by CYGNSS and Jason-3
## satellites during some randomly chosen week
dates_txt = read.table('../start_end_dates.txt')
set.seed(100)
random_week = sample(1:nrow(dates_txt), 1)
dates_use = dates_txt[random_week,]

# Get CYGNSS data
cygnss_dir <- "../data/processed_daily_cygnss"
filelist <- list.files(path = cygnss_dir, pattern = "\\.RData$")
cygnss_dat <- matrix(NA, nrow = 0, ncol = 9)
date_1 <- as.Date(as.character(dates_use[[1]]))
date_2 <- as.Date(as.character(dates_use[[2]]))
dates <- seq(date_1, to = date_2, by = 'day')
#date_chosen <- sample(dates, 1)
date_chosen <- '2019-09-29'
file_ind <- grep( date_chosen, filelist )
temp_data <- loadRData( file.path( cygnss_dir, filelist[ file_ind ] ) )
cygnss_dat <- rbind( cygnss_dat, temp_data )
colnames(cygnss_dat) <-
  c("sat", "lat", "lon", "sc_lat", "sc_lon", "wind_speed", "wind_speed_uncertainty", "time", "antenna")
cygnss_dat <- na.omit(cygnss_dat)
# standardize satellite naming
cygnss_dat[,'sat'][cygnss_dat[,'sat'] == 247] <- 1
cygnss_dat[,'sat'][cygnss_dat[,'sat'] == 249] <- 2
cygnss_dat[,'sat'][cygnss_dat[,'sat'] == 43] <- 3
cygnss_dat[,'sat'][cygnss_dat[,'sat'] == 44] <- 4
cygnss_dat[,'sat'][cygnss_dat[,'sat'] == 47] <- 5
cygnss_dat[,'sat'][cygnss_dat[,'sat'] == 54] <- 6
cygnss_dat[,'sat'][cygnss_dat[,'sat'] == 55] <- 7
cygnss_dat[,'sat'][cygnss_dat[,'sat'] == 73] <- 8
# get rid of unnecessary columns
cygnss_dat <- as.data.frame( cygnss_dat[ , c("sat","lat","lon","wind_speed","time") ] )

# Get Jason-3 data
jason_dir <- "../data/processed_daily_jason"
filelist <- list.files(path = jason_dir, pattern = "\\.RData$")
jason_dat <- matrix(NA, nrow = 0, ncol = 8)
file_ind <- grep( date_chosen, filelist )
temp_data <- loadRData( file.path( jason_dir, filelist[ file_ind ] ) )
jason_dat <- rbind( jason_dat, temp_data )

colnames(jason_dat) <-
  c("lat", "lon", "surface_type", "alt", "bathymetry", "wind_speed", "wind_speed_mle3", "time")
jason_dat <- na.omit(jason_dat)
jason_dat <- jason_dat[ jason_dat[,'surface_type'] == 0 , ]
# get rid of unnecessary columns
jason_dat <- as.data.frame( jason_dat[ , c("lat","lon","wind_speed","time") ] )

rm(temp_data); gc()

# standardize times in hours
cygnss_dat$time <- cygnss_dat$time - min(c( cygnss_dat$time,jason_dat$time ))
#cygnss_dat$time <- cygnss_dat$time/3600 # hours
jason_dat$time <- jason_dat$time - min(c( cygnss_dat$time,jason_dat$time ))
#jason_dat$time <- jason_dat$time/3600 # hours


########################## CYGNSS VS CYGNSS CODES #################################

# initialize lists that we intend to save
num_cygnss <- 8
dist_l <- list()
time_l <- list()
for(j in 1:choose(num_cygnss,2)){ # j for each unique pairwise comparison b/w cygnss satellites
  dist_l[[j]] <- numeric()
  time_l[[j]] <- numeric()
}


### Distances
num_hours = 24
num_sec = 3600*num_hours
#by = 60*60*2 # two hours
#tolerance = 60*60*1 # one hour
by = 60
tolerance = 10

# Nested loop structure
# for (satnum_base in 1:(num_cygnss-1)){
#   for (satnum_compare in (satnum_base+1):num_cygnss){
#     cat('comparison',c(satnum_base, satnum_compare),'\n')
#   }
# }

comparisons <- list()
i = 1
for (satnum_base in 1:(num_cygnss-1)){
  for (satnum_compare in (satnum_base+1):num_cygnss){
    comparisons[[i]] <- c(satnum_base, satnum_compare)
    i = i+1
  }
}

for (time in (seq(0, num_sec, by = by))) {
  
  cat(paste0(round(time/num_sec,4)*100,"%\n"))
  
  # go to next time in seq if cygnss_dat does not have measurement at time
  # within tolerance
  if ( all( abs(time - cygnss_dat$time) > tolerance ) ){
    next
  }
  
  for (satnum_base in 1:(num_cygnss-1)){
    
    # extract data from cygnss_dat corresponding to satnum_base
    cygnss_satnum_base <- cygnss_dat[which(cygnss_dat$sat==satnum_base),]
    
    for (satnum_compare in (satnum_base+1):num_cygnss){
      
      # extract data from cygnss_dat corresponding to satnum_compare
      cygnss_satnum_compare <- cygnss_dat[which(cygnss_dat$sat==satnum_compare),]
      
      # initialize lists that we intend to save
      time_vec = dist_vec = numeric()
      
      if ( any( abs(time - cygnss_satnum_base$time) < tolerance ) && any( abs(time - cygnss_satnum_compare$time) < tolerance )) {
        i <- which(abs(time - cygnss_satnum_base$time) %in%
                     abs(time - cygnss_satnum_base$time[which(abs(time - cygnss_satnum_base$time) < tolerance )]) )
        ij <- which(abs(time - cygnss_satnum_compare$time) %in% abs(time - cygnss_satnum_compare$time[which(abs(time - cygnss_satnum_compare$time) < tolerance )]) )
        
        x1 <- cbind(cygnss_satnum_compare$lon[ij], cygnss_satnum_compare$lat[ij])
        x2 <- cbind(cygnss_satnum_base$lon[i], cygnss_satnum_base$lat[i])
        
        dist_mat <- matrix(NA, nrow=length(ij), ncol=length(i))
        dist_mat <- rdist.earth( x1, x2, miles = FALSE )
        
        dist_vec <- c(dist_vec, min(dist_mat))
        time_vec <- c(time_vec, time)
      }
      
      # find which element in dist_l and time_l we should append dist_vec and time_vec to
      list_ix = which( lapply(rapply(lapply(comparisons, match, c(satnum_base,satnum_compare) ), na.omit, how = "replace"), length) == 2 )
      
      dist_l[[list_ix]] <- c(dist_l[[list_ix]], dist_vec)
      time_l[[list_ix]] <- c(time_l[[list_ix]], time_vec)
      
    }
    
  }
}


########################## CYGNSS VS JASON CODES #################################

# initialize lists that we intend to save
num_cygnss <- 8
dist_l_jas <- list()
time_l_jas <- list()
for(j in 1:num_cygnss){
  dist_l_jas[[j]] <- numeric()
  time_l_jas[[j]] <- numeric()
}


### Distances
num_hours = 24
num_sec = 3600*num_hours
#by = 60*60*2 # two hours
#tolerance = 60*60*1 # one hour
by = 60
tolerance = 10

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
    time_vec = dist_vec = numeric()
    
    if ( any( abs(time - cygnss_satnum$time) < tolerance ) ) {
      i <- which(abs(time - cygnss_satnum$time) %in%
                   abs(time - cygnss_satnum$time[which(abs(time - cygnss_satnum$time) < tolerance )]) )
      ij <- which(abs(time - jason_dat$time) %in% abs(time - jason_dat$time[which(abs(time - jason_dat$time) < tolerance )]) )
      
      x1 <- cbind(jason_dat$lon[ij], jason_dat$lat[ij])
      x2 <- cbind(cygnss_satnum$lon[i], cygnss_satnum$lat[i])
      
      #jws <- jason_dat$wind_speed[ij]
      #cws <- cygnss_satnum$wind_speed[i]
      #j_time <- jason_dat$time[ij]
      #c_time <- cygnss_satnum$time[i]
      
      dist_mat <- matrix(NA, nrow=length(ij), ncol=length(i))
      dist_mat <- rdist.earth( x1, x2, miles = FALSE )
      
      #min_ix <- which(dist_mat==min(dist_mat), arr.ind = T)
      #jws_vec <- c(jws_vec, jws[min_ix[1]])
      #cws_vec <- c(cws_vec, cws[min_ix[2]])
      dist_vec <- c(dist_vec, min(dist_mat))
      time_vec <- c(time_vec, time)
    }
    
    #jws_l[[satnum]] <- c(jws_l[[satnum]], jws_vec)
    #cws_l[[satnum]] <- c(cws_l[[satnum]], cws_vec)
    dist_l_jas[[satnum]] <- c(dist_l_jas[[satnum]], dist_vec)
    time_l_jas[[satnum]] <- c(time_l_jas[[satnum]], time_vec)
    
  }
}


########################## PLOTTING CODES #################################
# Create and save plot
save_dir <- "../figures"
pdf(file.path( save_dir, 'updated_cygnss_vs_jason_dist_plt.pdf' ), width = 14, height = 10)

# Plot
par(mfrow=c(4,1), mar = c(4, 4, 3, 2) + 0.1) #save w/ 1000 height for 4x1

numplots = 1
plot(0:20000, cex = 0, main = 'Distance vs Time', xlab = '', ylab = 'Distance (1000 km)', type='n', yaxt = 'n', xaxt = 'n')
ytick<-seq(0, 20000, by=2000)
axis(side=2, at=ytick, labels = seq(0, 20, by=2))
xtick<-seq(0, 6*3600*numplots, by=1800)
axis(side=1, at=xtick, labels = seq(0, 6*60*numplots, by=30))

times = seq(0, num_sec, by = num_sec/4)
ix = 1
for (time in times) {
  if (ix == 5) break
  if (time == 6*3600*numplots){ # 
    if (ix == 4) {
      plot(c(6*3600*numplots, 6*3600*(numplots+1)), c(0, 20000), cex = 0, xlab = 'Time (min)', ylab = 'Distance (1000 km)', type='n', yaxt = 'n', xaxt = 'n')
      ytick<-seq(0, 20000, by=2000)
      axis(side=2, at=ytick, labels = seq(0, 20, by=2))
      xtick<-seq(6*3600*numplots, (6*3600*(numplots+1)), by=1800) #xtick<-seq(0, 6*3600*numplots, by=1800)
      axis(side=1, at=xtick, labels = seq(6*60*numplots, 6*60*(numplots+1), by=30))
      numplots = numplots+1
    }
    else{
      plot(c(6*3600*numplots, 6*3600*(numplots+1)), c(0, 20000), cex = 0, xlab = '', ylab = 'Distance (1000 km)', type='n', yaxt = 'n', xaxt = 'n')
      ytick<-seq(0, 20000, by=2000)
      axis(side=2, at=ytick, labels = seq(0, 20, by=2))
      xtick<-seq(6*3600*numplots, (6*3600*(numplots+1)), by=1800) #xtick<-seq(0, 6*3600*numplots, by=1800)
      axis(side=1, at=xtick, labels = seq(6*60*numplots, 6*60*(numplots+1), by=30))
      numplots = numplots+1
    }
  }
  
  for (cyg_sat in 1:choose(num_cygnss,2)){
    ix_cygsat = which(time_l[[cyg_sat]] > times[ix] & time_l[[cyg_sat]] < times[ix+1])
    lines(time_l[[cyg_sat]][ix_cygsat], dist_l[[cyg_sat]][ix_cygsat], col='lightgrey')
  }
  
  ix_1 = which(time_l_jas[[1]] > times[ix] & time_l_jas[[1]] < times[ix+1])
  lines(time_l_jas[[1]][ix_1], dist_l_jas[[1]][ix_1], col = 'red')
  
  ix_2 = which(time_l_jas[[2]] > times[ix] & time_l_jas[[2]] < times[ix+1])
  lines(time_l_jas[[2]][ix_2], dist_l_jas[[2]][ix_2], col = 'orange')
  
  ix_3 = which(time_l_jas[[3]] > times[ix] & time_l_jas[[3]] < times[ix+1])
  lines(time_l_jas[[3]][ix_3], dist_l_jas[[3]][ix_3], col = 'yellow')
  
  ix_4 = which(time_l_jas[[4]] > times[ix] & time_l_jas[[4]] < times[ix+1])
  lines(time_l_jas[[4]][ix_4], dist_l_jas[[4]][ix_4], col = 'green')
  
  ix_5 = which(time_l_jas[[5]] > times[ix] & time_l_jas[[5]] < times[ix+1])
  lines(time_l_jas[[5]][ix_5], dist_l_jas[[5]][ix_5], col = 'aquamarine')
  
  ix_6 = which(time_l_jas[[6]] > times[ix] & time_l_jas[[6]] < times[ix+1])
  lines(time_l_jas[[6]][ix_6], dist_l_jas[[6]][ix_6], col = 'blue')
  
  ix_7 = which(time_l_jas[[7]] > times[ix] & time_l_jas[[7]] < times[ix+1])
  lines(time_l_jas[[7]][ix_7], dist_l_jas[[7]][ix_7], col = 'pink')
  
  ix_8 = which(time_l_jas[[8]] > times[ix] & time_l_jas[[8]] < times[ix+1])
  lines(time_l_jas[[8]][ix_8], dist_l_jas[[8]][ix_8], col = 'purple')
  
  ix = ix + 1
  
}

dev.off()

