# load libraries
library(ggplot2)
library(fields)

# Assume we are running from the R_scripts directory
# source functions
source("../R/load_data.R")

distance = function(x1,x2){
  return (rdist.earth.vec(x1,x2,miles=F))
}

date_1 <- as.Date('2019-09-28')
date_2 <- as.Date('2019-09-28')
dates <- seq(date_1, to = date_2, by = 'day')

`%nin%` <- Negate('%in%')
dates <- dates[dates %nin% c(seq(as.Date("2020-02-01"), as.Date("2020-02-14"), by = 'day'))]
dates <- dates[dates %nin% c(seq(as.Date("2020-06-13"), as.Date("2020-06-19"), by = 'day'))]

dist_1 = dist_2 = dist_3 = dist_4 = dist_5 = dist_6 = dist_7 = dist_8 = numeric()
jws_1 = cws_1 = jws_2 = cws_2 = jws_3 = cws_3 = jws_4 = cws_4 = jws_5 = cws_5 = jws_6 = cws_6 = jws_7 = cws_7 = jws_8 = cws_8 = numeric()

library(profvis)
profvis({
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
  cygnss_1 <- cygnss_dat[which(cygnss_dat[,'sat']==1),]
  cygnss_2 <- cygnss_dat[which(cygnss_dat[,'sat']==2),]
  cygnss_3 <- cygnss_dat[which(cygnss_dat[,'sat']==3),]
  cygnss_4 <- cygnss_dat[which(cygnss_dat[,'sat']==4),]
  cygnss_5 <- cygnss_dat[which(cygnss_dat[,'sat']==5),]
  cygnss_6 <- cygnss_dat[which(cygnss_dat[,'sat']==6),]
  cygnss_7 <- cygnss_dat[which(cygnss_dat[,'sat']==7),]
  cygnss_8 <- cygnss_dat[which(cygnss_dat[,'sat']==8),]
  
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

    print(time/num_sec)
      
    # go to next time in seq if cygnss_dat or jason_dat does not have measurement at time
    # within tolerance
    if (!any( abs(time - jason_dat[,ncol(jason_dat)]) < tolerance ) || !any( abs(time - cygnss_dat[,ncol(cygnss_dat)]) < tolerance ) ) {
      next
    }
    
    
    if (any( abs(time - cygnss_1[,ncol(cygnss_1)]) < tolerance ) ) {
      i = which(abs(time - cygnss_1[,ncol(cygnss_1)]) %in% abs(time - cygnss_1[,ncol(cygnss_1)][which(abs(time - cygnss_1[,ncol(cygnss_1)]) < tolerance )]) )
      ij = which(abs(time - jason_dat[,ncol(jason_dat)]) %in% abs(time - jason_dat[,ncol(jason_dat)][which(abs(time - jason_dat[,ncol(jason_dat)]) < tolerance )]) )
      
      x1 = cbind(jason_dat[ij,2], jason_dat[ij,1]) # first col long, second col lat
      x2 = cbind(cygnss_1[i,3], cygnss_1[i,2]) # first col long, second col lat
      
      jws = jason_dat[ij,6]
      cws = cygnss_1[i,6]
      
      dist_mat = matrix(NA, nrow=length(jws), ncol=length(cws))
      
      # this can be replaced by
      dist_mat <- rdist.earth( x1, x2, miles = FALSE )
      #for (jws_candidate_num in 1:length(jws)){
      #  for (cws_candidate_num in 1:length(cws)){
      #    dist_mat[jws_candidate_num, cws_candidate_num] = distance(t(as.matrix(x1[jws_candidate_num,])),t(as.matrix(x2[cws_candidate_num,])))
      #  }
      #}
      
      # don't call the which function twice here. Call once, save the result, and then assign.
      jws_1 = c(jws_1, jws[which(dist_mat==min(dist_mat), arr.ind = T)[1]])
      cws_1 = c(cws_1, cws[which(dist_mat==min(dist_mat), arr.ind = T)[2]])
      dist_1 = c(dist_1, min(dist_mat))
    }
    
   if(FALSE){ 
      # you shouldn't rewrite this code several times. Figure out how to put
      # the data into arrays or lists, so that you can iterate over the arrays.
      # at the very least, write a function to perform these operations,
      # and then call the function 8 times.
    if (any( abs(time - cygnss_2[,ncol(cygnss_2)]) < tolerance ) ) {
      i = which(abs(time - cygnss_2[,ncol(cygnss_2)]) %in% abs(time - cygnss_2[,ncol(cygnss_2)][which(abs(time - cygnss_2[,ncol(cygnss_2)]) < tolerance )]) )
      ij = which(abs(time - jason_dat[,ncol(jason_dat)]) %in% abs(time - jason_dat[,ncol(jason_dat)][which(abs(time - jason_dat[,ncol(jason_dat)]) < tolerance )]) )
      
      x1 = cbind(jason_dat[ij,2], jason_dat[ij,1]) # first col long, second col lat
      x2 = cbind(cygnss_2[i,3], cygnss_2[i,2]) # first col long, second col lat
      
      jws = jason_dat[ij,6]
      cws = cygnss_2[i,6]
      
      dist_mat = matrix(NA, nrow=length(jws), ncol=length(cws))
      
      for (jws_candidate_num in 1:length(jws)){
        for (cws_candidate_num in 1:length(cws)){
          dist_mat[jws_candidate_num, cws_candidate_num] = distance(t(as.matrix(x1[jws_candidate_num,])),t(as.matrix(x2[cws_candidate_num,])))
        }
      }
      
      jws_2 = c(jws_2, jws[which(dist_mat==min(dist_mat), arr.ind = T)[1]])
      cws_2 = c(cws_2, cws[which(dist_mat==min(dist_mat), arr.ind = T)[2]])
      dist_2 = c(dist_2, min(dist_mat))
    }
    
    if (any( abs(time - cygnss_3[,ncol(cygnss_3)]) < tolerance ) ) {
      i = which(abs(time - cygnss_3[,ncol(cygnss_3)]) %in% abs(time - cygnss_3[,ncol(cygnss_3)][which(abs(time - cygnss_3[,ncol(cygnss_3)]) < tolerance )]) )
      ij = which(abs(time - jason_dat[,ncol(jason_dat)]) %in% abs(time - jason_dat[,ncol(jason_dat)][which(abs(time - jason_dat[,ncol(jason_dat)]) < tolerance )]) )
      
      x1 = cbind(jason_dat[ij,2], jason_dat[ij,1]) # first col long, second col lat
      x2 = cbind(cygnss_3[i,3], cygnss_3[i,2]) # first col long, second col lat
      
      jws = jason_dat[ij,6]
      cws = cygnss_3[i,6]
      
      dist_mat = matrix(NA, nrow=length(jws), ncol=length(cws))
      
      for (jws_candidate_num in 1:length(jws)){
        for (cws_candidate_num in 1:length(cws)){
          dist_mat[jws_candidate_num, cws_candidate_num] = distance(t(as.matrix(x1[jws_candidate_num,])),t(as.matrix(x2[cws_candidate_num,])))
        }
      }
      
      jws_3 = c(jws_3, jws[which(dist_mat==min(dist_mat), arr.ind = T)[1]])
      cws_3 = c(cws_3, cws[which(dist_mat==min(dist_mat), arr.ind = T)[2]])
      dist_3 = c(dist_3, min(dist_mat))
    }
    
    if (any( abs(time - cygnss_4[,ncol(cygnss_4)]) < tolerance ) ) {
      i = which(abs(time - cygnss_4[,ncol(cygnss_4)]) %in% abs(time - cygnss_4[,ncol(cygnss_4)][which(abs(time - cygnss_4[,ncol(cygnss_4)]) < tolerance )]) )
      ij = which(abs(time - jason_dat[,ncol(jason_dat)]) %in% abs(time - jason_dat[,ncol(jason_dat)][which(abs(time - jason_dat[,ncol(jason_dat)]) < tolerance )]) )
      
      x1 = cbind(jason_dat[ij,2], jason_dat[ij,1]) # first col long, second col lat
      x2 = cbind(cygnss_4[i,3], cygnss_4[i,2]) # first col long, second col lat
      
      jws = jason_dat[ij,6]
      cws = cygnss_4[i,6]
      
      dist_mat = matrix(NA, nrow=length(jws), ncol=length(cws))
      
      for (jws_candidate_num in 1:length(jws)){
        for (cws_candidate_num in 1:length(cws)){
          dist_mat[jws_candidate_num, cws_candidate_num] = distance(t(as.matrix(x1[jws_candidate_num,])),t(as.matrix(x2[cws_candidate_num,])))
        }
      }
      
      jws_4 = c(jws_4, jws[which(dist_mat==min(dist_mat), arr.ind = T)[1]])
      cws_4 = c(cws_4, cws[which(dist_mat==min(dist_mat), arr.ind = T)[2]])
      dist_4 = c(dist_4, min(dist_mat))
    }
    
    
    if (any( abs(time - cygnss_5[,ncol(cygnss_5)]) < tolerance ) ) {
      i = which(abs(time - cygnss_5[,ncol(cygnss_5)]) %in% abs(time - cygnss_5[,ncol(cygnss_5)][which(abs(time - cygnss_5[,ncol(cygnss_5)]) < tolerance )]) )
      ij = which(abs(time - jason_dat[,ncol(jason_dat)]) %in% abs(time - jason_dat[,ncol(jason_dat)][which(abs(time - jason_dat[,ncol(jason_dat)]) < tolerance )]) )
      
      x1 = cbind(jason_dat[ij,2], jason_dat[ij,1]) # first col long, second col lat
      x2 = cbind(cygnss_5[i,3], cygnss_5[i,2]) # first col long, second col lat
      
      jws = jason_dat[ij,6]
      cws = cygnss_5[i,6]
      
      dist_mat = matrix(NA, nrow=length(jws), ncol=length(cws))
      
      for (jws_candidate_num in 1:length(jws)){
        for (cws_candidate_num in 1:length(cws)){
          dist_mat[jws_candidate_num, cws_candidate_num] = distance(t(as.matrix(x1[jws_candidate_num,])),t(as.matrix(x2[cws_candidate_num,])))
        }
      }
      
      jws_5 = c(jws_5, jws[which(dist_mat==min(dist_mat), arr.ind = T)[1]])
      cws_5 = c(cws_5, cws[which(dist_mat==min(dist_mat), arr.ind = T)[2]])
      dist_5 = c(dist_5, min(dist_mat))
    }
    
    if (any( abs(time - cygnss_6[,ncol(cygnss_6)]) < tolerance ) ) {
      i = which(abs(time - cygnss_6[,ncol(cygnss_6)]) %in% abs(time - cygnss_6[,ncol(cygnss_6)][which(abs(time - cygnss_6[,ncol(cygnss_6)]) < tolerance )]) )
      ij = which(abs(time - jason_dat[,ncol(jason_dat)]) %in% abs(time - jason_dat[,ncol(jason_dat)][which(abs(time - jason_dat[,ncol(jason_dat)]) < tolerance )]) )
      
      x1 = cbind(jason_dat[ij,2], jason_dat[ij,1]) # first col long, second col lat
      x2 = cbind(cygnss_6[i,3], cygnss_6[i,2]) # first col long, second col lat
      
      jws = jason_dat[ij,6]
      cws = cygnss_6[i,6]
      
      dist_mat = matrix(NA, nrow=length(jws), ncol=length(cws))
      
      for (jws_candidate_num in 1:length(jws)){
        for (cws_candidate_num in 1:length(cws)){
          dist_mat[jws_candidate_num, cws_candidate_num] = distance(t(as.matrix(x1[jws_candidate_num,])),t(as.matrix(x2[cws_candidate_num,])))
        }
      }
      
      jws_6 = c(jws_6, jws[which(dist_mat==min(dist_mat), arr.ind = T)[1]])
      cws_6 = c(cws_6, cws[which(dist_mat==min(dist_mat), arr.ind = T)[2]])
      dist_6 = c(dist_6, min(dist_mat))
    }
    
    if (any( abs(time - cygnss_7[,ncol(cygnss_7)]) < tolerance ) ) {
      i = which(abs(time - cygnss_7[,ncol(cygnss_7)]) %in% abs(time - cygnss_7[,ncol(cygnss_7)][which(abs(time - cygnss_7[,ncol(cygnss_7)]) < tolerance )]) )
      ij = which(abs(time - jason_dat[,ncol(jason_dat)]) %in% abs(time - jason_dat[,ncol(jason_dat)][which(abs(time - jason_dat[,ncol(jason_dat)]) < tolerance )]) )
      
      x1 = cbind(jason_dat[ij,2], jason_dat[ij,1]) # first col long, second col lat
      x2 = cbind(cygnss_7[i,3], cygnss_7[i,2]) # first col long, second col lat
      
      jws = jason_dat[ij,6]
      cws = cygnss_7[i,6]
      
      dist_mat = matrix(NA, nrow=length(jws), ncol=length(cws))
      
      for (jws_candidate_num in 1:length(jws)){
        for (cws_candidate_num in 1:length(cws)){
          dist_mat[jws_candidate_num, cws_candidate_num] = distance(t(as.matrix(x1[jws_candidate_num,])),t(as.matrix(x2[cws_candidate_num,])))
        }
      }
      
      jws_7 = c(jws_7, jws[which(dist_mat==min(dist_mat), arr.ind = T)[1]])
      cws_7 = c(cws_7, cws[which(dist_mat==min(dist_mat), arr.ind = T)[2]])
      dist_7 = c(dist_7, min(dist_mat))
    }
    
    if (any( abs(time - cygnss_8[,ncol(cygnss_8)]) < tolerance ) ) {
      i = which(abs(time - cygnss_8[,ncol(cygnss_8)]) %in% abs(time - cygnss_8[,ncol(cygnss_8)][which(abs(time - cygnss_8[,ncol(cygnss_8)]) < tolerance )]) )
      ij = which(abs(time - jason_dat[,ncol(jason_dat)]) %in% abs(time - jason_dat[,ncol(jason_dat)][which(abs(time - jason_dat[,ncol(jason_dat)]) < tolerance )]) )
      
      x1 = cbind(jason_dat[ij,2], jason_dat[ij,1]) # first col long, second col lat
      x2 = cbind(cygnss_8[i,3], cygnss_8[i,2]) # first col long, second col lat
      
      jws = jason_dat[ij,6]
      cws = cygnss_8[i,6]
      
      dist_mat = matrix(NA, nrow=length(jws), ncol=length(cws))
      
      for (jws_candidate_num in 1:length(jws)){
        for (cws_candidate_num in 1:length(cws)){
          dist_mat[jws_candidate_num, cws_candidate_num] = distance(t(as.matrix(x1[jws_candidate_num,])),t(as.matrix(x2[cws_candidate_num,])))
        }
      }
      
      jws_8 = c(jws_8, jws[which(dist_mat==min(dist_mat), arr.ind = T)[1]])
      cws_8 = c(cws_8, cws[which(dist_mat==min(dist_mat), arr.ind = T)[2]])
      dist_8 = c(dist_8, min(dist_mat))
    }

                   }
    
  }
  
}
})

i <- 1
dist_l = jws_l = cws_l = list()
while(i <= 8) {
  dist_l[[i]] <- get(paste0('dist_',i))
  jws_l[[i]] <- get(paste0('jws_',i))
  cws_l[[i]] <- get(paste0('cws_',i))
  i <- i + 1
}

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
