# this is similar to fit_models2, except we include the jason
# data as the reference level in the model

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

# Read in the CYGNSS Data
dir <- "../data/processed_daily_cygnss"
filelist <- list.files(path = dir, pattern = "\\.RData$")
cnames1 <- c("spacecraft_id","lat","lon","wind_speed","sample_time","antenna")
cnames2 <- c(          "sat","lat","lon","wind_speed",       "time","antenna")

# find each day's file, subset and append to the data matrix
dat <- as.data.frame( matrix(NA, nrow = 0, ncol = length(cnames1)) )
for(j in 1:length(dates)){
    file_ind <- grep( dates[j], filelist )
    temp_data <- loadRData( file.path( dir, filelist[ file_ind ] ) )
    dat <- rbind( dat, temp_data[, cnames1, drop=FALSE] )
}
dat_cyg <- dat
colnames(dat_cyg) <- cnames2

# Read in the jason Data
using_jason <- TRUE
dir <- "../data/processed_daily_jason"
filelist <- list.files(path = dir, pattern = "\\.RData$")
cnames1 <- c("lat","lon","wind_speed_alt","time")
cnames2 <- c("lat","lon",    "wind_speed","time")

# find each day's file, subset and append to the data matrix
dat <- as.data.frame( matrix(NA, nrow = 0, ncol = length(cnames1)) )
for(j in 1:length(dates)){
    file_ind <- grep( dates[j], filelist )
    temp_data <- loadRData( file.path( dir, filelist[ file_ind ] ) )
    dat <- rbind( dat, temp_data[, cnames1, drop=FALSE] )
}
dat_jas <- dat
colnames(dat_jas) <- cnames2
dat_jas$sat <- 0
dat_jas$antenna <- 3

# reorder to match cygnss data
dat_jas <- dat_jas[ colnames(dat_cyg) ]

# combine the two datasets
dat <- rbind( dat_cyg, dat_jas )

# remove missing values
dat <- na.omit(dat)
dat <- dat[dat$antenna == 2 | dat$antenna == 3,]

# standardize satellite naming
dat[,'sat'][dat[,'sat'] == 0] <- "0"
dat[,'sat'][dat[,'sat'] == 247] <- "1"
dat[,'sat'][dat[,'sat'] == 249] <- "2"
dat[,'sat'][dat[,'sat'] == 43] <- "3"
dat[,'sat'][dat[,'sat'] == 44] <- "4"
dat[,'sat'][dat[,'sat'] == 47] <- "5"
dat[,'sat'][dat[,'sat'] == 54] <- "6"
dat[,'sat'][dat[,'sat'] == 55] <- "7"
dat[,'sat'][dat[,'sat'] == 73] <- "8"

# split up by satellite, empty out the dat data frame
dat_split <- split( dat, dat[[ "sat" ]] )
dat <- dat[ integer(0), ]

# loop over satellites, subsample, reassign to dat
numsat <- 20000
set.seed(100)
for(j in 1:length(dat_split)){
    r <- round( seq( 1, nrow(dat_split[[j]]), length.out = numsat ) )
    dat <- rbind( dat, dat_split[[j]][r,] )
}

# name the antenna variable
dat$ant <- NA
dat$ant[ dat$antenna == 2 ] <- "starboard"
dat$ant[ dat$antenna == 3 ] <- "port"

# create an interaction variable between sat and antenna
dat$sat_ant <- paste(dat$sat, dat$ant, sep="_")

# standardize time as number of hours since earliest measurement in dat
dat$time <- ( dat$time - min(dat$time) )/3600

# create x,y,z coordinates
dat$x <- 6371*cos( dat$lat*pi/180 )*cos( (dat$lon-180)*pi/180 )
dat$y <- 6371*cos( dat$lat*pi/180 )*sin( (dat$lon-180)*pi/180 )
dat$z <- 6371*sin( dat$lat*pi/180 )

# create variables for design matrix
dat$radians <- dat$lat*pi/180

# create y, locs, X for input to fit_model
y <- dat$wind_speed
locs <- as.matrix( dat[ , c("x","y","z","time") ] )
X <- model.matrix( ~ radians + I(radians^2) + I(radians^3) + time + sat_ant, data = dat )

# fit the model
model <- GpGp::fit_model(y, locs, X,
    covfun_name = "exponential_spacetime",
    start_parms = c(6.5292, 6135.0707, 18.1037, 1e-04),
    m_seq = c(30),
    silent = TRUE
)
summary(model)

save_dir <- "../model_fits"
filename <- paste0('fit3-', dates[1], '-to-', dates[length(dates)], '.RData')
save(model, file = file.path(save_dir, filename) )

