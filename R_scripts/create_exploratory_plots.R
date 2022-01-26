# load libraries
library(fields)
library(maps)

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

