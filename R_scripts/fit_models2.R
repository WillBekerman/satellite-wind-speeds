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
    c("sat","lat","lon","sc_lat","sc_lon","wind_speed","wind_speed_uncertainty","time","antenna")

# remove missing values
cygnss_dat <- na.omit(cygnss_dat)
cygnss_dat <- cygnss_dat[cygnss_dat$antenna == 2 | cygnss_dat$antenna == 3,]

# standardize satellite naming
cygnss_dat[,'sat'][cygnss_dat[,'sat'] == 247] <- "1"
cygnss_dat[,'sat'][cygnss_dat[,'sat'] == 249] <- "2"
cygnss_dat[,'sat'][cygnss_dat[,'sat'] == 43] <- "3"
cygnss_dat[,'sat'][cygnss_dat[,'sat'] == 44] <- "4"
cygnss_dat[,'sat'][cygnss_dat[,'sat'] == 47] <- "5"
cygnss_dat[,'sat'][cygnss_dat[,'sat'] == 54] <- "6"
cygnss_dat[,'sat'][cygnss_dat[,'sat'] == 55] <- "7"
cygnss_dat[,'sat'][cygnss_dat[,'sat'] == 73] <- "8"

# split up by satellite, empty out the cygnss_dat data frame
cygnss_split <- split( cygnss_dat, cygnss_dat[[ "sat" ]] )
cygnss_dat <- cygnss_dat[ integer(0), ]

# loop over satellites, subsample, reassign to cygnss_dat
numsat <- 20000
set.seed(100)
for(j in 1:8){
    r <- round( seq( 1, nrow(cygnss_split[[j]]), length.out = numsat ) )
    cygnss_dat <- rbind( cygnss_dat, cygnss_split[[j]][r,] )
}
    
# name the antenna variable
cygnss_dat$ant <- NA
cygnss_dat$ant[ cygnss_dat$antenna == 2 ] <- "starboard"
cygnss_dat$ant[ cygnss_dat$antenna == 3 ] <- "port"

# standardize time as number of hours since earliest measurement in dat
cygnss_dat$time <- ( cygnss_dat$time - min(cygnss_dat$time) )/3600

# create x,y,z coordinates
cygnss_dat$x <- 6371*cos( cygnss_dat$lat*pi/180 )*cos( (cygnss_dat$lon-180)*pi/180 )
cygnss_dat$y <- 6371*cos( cygnss_dat$lat*pi/180 )*sin( (cygnss_dat$lon-180)*pi/180 )
cygnss_dat$z <- 6371*sin( cygnss_dat$lat*pi/180 )

# create variables for design matrix
cygnss_dat$radians <- cygnss_dat$lat*pi/180

# create y, locs, X for input to fit_model
#ii <- cygnss_dat$lat > -30
y <- cygnss_dat$wind_speed
locs <- as.matrix( cygnss_dat[ , c("x","y","z","time") ] )
X <- model.matrix( ~ radians + I(radians^2) + I(radians^3) + time + ant*sat, data = cygnss_dat )
#y <- cygnss_dat$wind_speed[ii]
#locs <- as.matrix( cygnss_dat[ , c("x","y","z","time") ] )[ii,]
#X <- model.matrix( ~ radians + I(radians^2) + I(radians^3) + time + ant*sat, data = cygnss_dat )[ii,]

# fit the model
model <- GpGp::fit_model(y, locs, X,
    covfun_name = "exponential_spacetime",
    start_parms = c(6.5292, 6135.0707, 18.1037, 1e-04),
    m_seq = c(30),
    silent = TRUE
)
summary(model)

save_dir <- "../model_fits"
filename <- paste0('fit2-', dates[1], '-to-', dates[length(dates)], '.RData')
save(model, file = file.path(save_dir, filename) )

