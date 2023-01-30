# load libraries
library(fields)
library(maps)
library(viridis)

# Assume we are running from the R_scripts directory
# source functions
source("../R/load_data.R")

## Generate a plot comparing wind speed measurements recorded by CYGNSS and Jason-3
## satellites during some randomly chosen week
dates_txt = read.table('../start_end_dates.txt')
random_week = 10
date_1 <- as.Date( dates_txt[random_week,1] )
date_2 <- as.Date( dates_txt[random_week,2] )

# Get CYGNSS data
cygnss_dir <- "../data/processed_daily_cygnss"
filelist <- list.files(path = cygnss_dir, pattern = "\\.RData$")
cygnss_dat <- matrix(NA, nrow = 0, ncol = 9)
dates <- seq(date_1, to = date_2, by = 'day')
for(j in 1:length(dates)){
  file_ind <- grep( dates[j], filelist )
  temp_data <- loadRData( file.path( cygnss_dir, filelist[ file_ind ] ) )
  cygnss_dat <- rbind( cygnss_dat, temp_data )
}
colnames(cygnss_dat) <-
  c("sat", "lat", "lon", "sc_lat", "sc_lon", "wind_speed", "wind_speed_uncertainty", "time", "antenna")
cygnss_dat <- na.omit(cygnss_dat)

# Get Jason-3 data
jason_dir <- "../data/processed_daily_jason"
filelist <- list.files(path = jason_dir, pattern = "\\.RData$")
jason_dat <- matrix(NA, nrow = 0, ncol = 8)
for(j in 1:length(dates)){
  file_ind <- grep( dates[j], filelist )
  temp_data <- loadRData( file.path( jason_dir, filelist[ file_ind ] ) )
  jason_dat <- rbind( jason_dat, temp_data )
}
colnames(jason_dat) <-
  c("lat", "lon", "surface_type", "alt", "bathymetry", "wind_speed", "wind_speed_mle3", "time")
jason_dat <- na.omit(jason_dat)
jason_dat <- jason_dat[ jason_dat[,'surface_type'] == 0 , ]

rm(temp_data); gc()

# Create and save plot
save_dir <- "../figures"
pdf(file.path( save_dir, 'updated_cygnss_vs_jason_plt.pdf' ), width = 14, height = 10)

par(mfrow=c(2,1), mar = c(1.5,4,4,2)+0.1)
quilt.plot(cygnss_dat$lon, cygnss_dat$lat, cygnss_dat$wind_speed, main = 'CYGNSS Wind Speeds', zlim = c(0, 20), xaxt='n', yaxt='n', cex.main=1.75)
map("world2", add=T)

reduced_jason_dat = jason_dat[which(jason_dat$lat > -38),]
reduced_jason_dat = reduced_jason_dat[which(reduced_jason_dat$lat < 38),]

quilt.plot(reduced_jason_dat$lon, reduced_jason_dat$lat, reduced_jason_dat$wind_speed, main = 'Jason-3 Wind Speeds', zlim = c(0, 20), xaxt='n', yaxt='n', cex.main=1.75)
map("world2", add=T)
dev.off()


# plot with 2 cygnss, one jason
save_dir <- "../figures"
pdf(file.path( save_dir, 'cygnss1_cygnss4_jason_world.pdf' ), width = 8, height = 8)

# function for plotting map
map_plot <- function( x, y, z, title ){
    quilt.plot(x, y, z, nx = 90, ny = 18,
        main = title, zlim = c(0, 20), axes = FALSE,
        cex.main=1.75, col = viridis(64), legend.cex = 2)
    mtext("m/s", side=4, line = 6 )
    axis(1, at = seq(0,360,by=60))
    axis(2, at = seq(-30,30,by=15))
    box()
}

    
par(mfrow=c(3,1), mar = c(3,3,2,2))

# plot cygnss 1 (247)
ii <- cygnss_dat$sat == 247
print(sum(ii))
map_plot( cygnss_dat$lon[ii], cygnss_dat$lat[ii], cygnss_dat$wind_speed[ii], "CYGNSS 1")
map("world2", add=T)

# plot cygnss 4 (44) 
ii <- cygnss_dat$sat == 44
print(sum(ii))
map_plot( cygnss_dat$lon[ii], cygnss_dat$lat[ii], cygnss_dat$wind_speed[ii], "CYGNSS 4")
map("world2", add=T)

# jason
ii <- jason_dat$lat > -38 & jason_dat$lat < 38
print(sum(ii))
map_plot(jason_dat$lon[ii], jason_dat$lat[ii], jason_dat$wind_speed[ii], "Jason-3" )
map("world2", add=T)

dev.off()

