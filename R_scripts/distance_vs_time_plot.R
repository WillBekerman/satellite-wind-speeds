# load libraries
library(fields)

# Assume we are running from the R_scripts directory
# source functions
source("../R/load_data.R")

# Get CYGNSS data
cygnss_dir <- "../data/processed_daily_cygnss"
filelist <- list.files(path = cygnss_dir, pattern = "\\.RData$")
cygnss_dat <- matrix(NA, nrow = 0, ncol = 9)
date_chosen <- '2019-09-29'
file_ind <- grep( date_chosen, filelist )
cygnss_dat <- loadRData( file.path( cygnss_dir, filelist[ file_ind ] ) )
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
cygnss_dat <- as.data.frame( cygnss_dat[ , c("sat","lat","lon","wind_speed","time","antenna") ] )

# Get Jason-3 data
jason_dir <- "../data/processed_daily_jason"
filelist <- list.files(path = jason_dir, pattern = "\\.RData$")
jason_dat <- matrix(NA, nrow = 0, ncol = 8)
file_ind <- grep( date_chosen, filelist )
jason_dat <- loadRData( file.path( jason_dir, filelist[ file_ind ] ) )

colnames(jason_dat) <-
  c("lat", "lon", "surface_type", "alt", "bathymetry", "wind_speed", "wind_speed_mle3", "time")
jason_dat <- na.omit(jason_dat)
jason_dat <- jason_dat[ jason_dat[,'surface_type'] == 0 , ]
# get rid of unnecessary columns
jason_dat <- as.data.frame( jason_dat[ , c("lat","lon","wind_speed","time") ] )

# standardize times in hours
start_time <- min(c( cygnss_dat$time,jason_dat$time ))
cygnss_dat$time <- cygnss_dat$time - start_time
jason_dat$time <- jason_dat$time - start_time


########################## CYGNSS VS CYGNSS CODES #################################

all_times <- 60*(0:1440)
num_times <- length(all_times)
num_cygnss <- 8

cygnss_locs <- list()
for(j in 1:num_cygnss){

    # grab the data for this satellite, starboard antenna
    ii <- cygnss_dat$sat == j# & cygnss_dat$antenna == 2
    cygnss_locs[[j]] <- cygnss_dat[ii,]

}

# plot the cygnss distances
x1 <- 100
y1 <- 22000
x2 <- 180
x3 <- 130
x4 <- 130
cols <- c("darkred","darkorange","gold2","darkgreen","aquamarine2","blue","magenta","purple")

pdf("../figures/distance_vs_time.pdf", width = 14, height = 5.25 )
plot(0, type="n", main = 'Distance vs Time', xlab = 'Time (minutes)', ylab = 'Distance (km)', xlim = c(0,max(all_times/60)), ylim = c(0,23000) )
text(x1,y1,"CYGNSS vs. CYGNSS", col="gray")
text(x1 + x2,y1, "Jason-3 vs.")
for(j in 1:num_cygnss){
    text( x1 + x2 + x3 + x4*(j-1), y1, paste0("CYGNSS ",j), col = cols[j] )
}
 
dist_list = list()
comparison_ix = 1
for(j1 in 1:(num_cygnss-1)){
    for(j2 in (j1+1):num_cygnss){

        dat1 <- cygnss_locs[[j1]] 
        dat2 <- cygnss_locs[[j2]]
        distvec <- rep(NA, num_times)
        for(k in 1:num_times){

            i1 <- which( abs( dat1$time - all_times[k] ) < 10 )
            i2 <- which( abs( dat2$time - all_times[k] ) < 10 )
            distmat <- fields::rdist.earth(
                dat1[i1,c("lon","lat"),drop=FALSE],
                dat2[i2,c("lon","lat"),drop=FALSE],
                miles = FALSE
            )
            if( length(distmat) > 0 ){
                distvec[k] <- min(distmat)
            }

        }
        ii <- !is.na(distvec)
        if (j1 == 1 && j2 == 4){lines( all_times[ii]/60, distvec[ii], col = "black" )} # plot (1,4) in black
        else {lines( all_times[ii]/60, distvec[ii], col = "lightgray" )}
        
        dist_list[[comparison_ix]] <- distvec
        comparison_ix = comparison_ix+1
        
    }
}

# plot the cygnss-jason distances
for(j in 1:num_cygnss){
    distvec <- rep(NA, num_times)
    for(k in 1:num_times){

        i1 <- which( abs( cygnss_locs[[j]]$time - all_times[k] ) < 10 )
        i2 <- which( abs( jason_dat$time - all_times[k] ) < 10 )
        distmat <- fields::rdist.earth(
            cygnss_locs[[j]][i1,c("lon","lat"),drop=FALSE],
            jason_dat[i2,c("lon","lat"),drop=FALSE],
            miles = FALSE
        )
        if( length(distmat) > 0 ){
            distvec[k] <- min(distmat)
        }

    }
    ii <- !is.na(distvec)
    lines( all_times[ii]/60, distvec[ii], col = cols[j], lwd = 1.5 )
}
dev.off()


dist_list <- lapply(dist_list, function(x) x[!is.na(x)]) # remove NAs from list of distance vectors
sort( as.numeric(lapply(dist_list, mean)),index.return=T )
## ON THIS DATE, WE HAVE THAT:
## minimum average distances: {(5,8), (1,3), (1,8)}
## maximum average distances: {(4,6), (6,8), (5,6)}

## Note that (1,4) mean distance >5000km on this date

