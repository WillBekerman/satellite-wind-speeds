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
dat_jas$antenna <- 0

# reorder to match cygnss data
dat_jas <- dat_jas[ colnames(dat_cyg) ]

# combine the two datasets
dat <- rbind( dat_cyg, dat_jas )

# assign Starboard and Port identifiers to an antenna variable
dat$ant <- "0" 
dat$ant[ dat$antenna == 2 ] <- "S"
dat$ant[ dat$antenna == 3 ] <- "P"

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

# standardize time as number of hours since earliest measurement in dat
dat$time <- ( dat$time - min(dat$time) )/3600

# save the data
save(dat, file = paste0("../data/for_fitting/data_",date_1,"_",date_2,".RData") )
