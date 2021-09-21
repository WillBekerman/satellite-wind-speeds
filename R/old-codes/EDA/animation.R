source('R/data-processing/data_processing.R')

library(animation)
library(fields)
library(maps)

num_hours = 24*7 # use whole week
num_sec = 3600*num_hours
by = 60 # 1 hour

tolerance = 30 # show if obs w/in 30 sec

ani.record(reset = TRUE)  # clear history before recording

ix = 1
done_1 = done_2 = done_3 = done_4 = done_5 = done_6 = done_7 = done_8 = done_j = F
for (time in (seq(0, num_sec, by = by))) {
  datetime = seq(ISOdate(2020,9,20,0,0,00,tz="UTC"),to=ISOdate(2020,9,26,23,59,59,tz="UTC"),by='mins')[ix]
  if (ix > 2){
    if (any( abs(time - jason_dat[,ncol(jason_dat)]) < tolerance ) ) {
      i = which(abs(time - jason_dat[,ncol(jason_dat)]) == min( abs(time - jason_dat[,ncol(jason_dat)][which(abs(time - jason_dat[,ncol(jason_dat)]) < tolerance )]) ))
      lat = jason_dat[i[1],1]
      long = jason_dat[i[1],2]
    } else if (done_j){
      lat = jason_dat[ijlast[1],1]
      long = jason_dat[ijlast[1],2]
    }

    ll = paste0("; Jason at (", format(round(lat, 2), nsmall = 2), sep = ", ", format(round(long,2), nsmall = 2) , ")")
    plot(-1000,-1000, pch = 16, main = paste0(datetime, ll), xlab = 'Longitude', ylab = 'Latitude', col = cygnss_dat[1,1], 
         xlim = c( min(jason_dat[,2]), max(jason_dat[,2]) ), ylim = c( min(jason_dat[,1]), max(jason_dat[,1])))     #xlim = c( min(cygnss_dat[,3]), max(cygnss_dat[,3]) ), ylim = c( min(cygnss_dat[,2]), max(cygnss_dat[,2])))     
  } else {
    plot(-1000,-1000, pch = 16, main = datetime, xlab = 'Longitude', ylab = 'Latitude', col = cygnss_dat[1,1], 
         xlim = c( min(jason_dat[,2]), max(jason_dat[,2]) ), ylim = c( min(jason_dat[,1]), max(jason_dat[,1])))     #xlim = c( min(cygnss_dat[,3]), max(cygnss_dat[,3]) ), ylim = c( min(cygnss_dat[,2]), max(cygnss_dat[,2])))     }
  }
  map("world2", add=T)
  
  # cygnss sats
  if (any( abs(time - cygnss_1[,ncol(cygnss_1)]) < tolerance ) ) {
    i = which(abs(time - cygnss_1[,ncol(cygnss_1)]) == min( abs(time - cygnss_1[,ncol(cygnss_1)][which(abs(time - cygnss_1[,ncol(cygnss_1)]) < tolerance )]) ))
    done_1 = T
    i1last = i
    points(cygnss_1[i[1],3+2], cygnss_1[i[1],2+2], pch = 16, col = 'red', cex = 2)
  } else if (done_1){
    points(cygnss_1[i1last[1],3+2], cygnss_1[i1last[1],2+2], pch = 16, col = 'red', cex = 2)
  }
  if (any( abs(time - cygnss_2[,ncol(cygnss_2)]) < tolerance ) ) {
    i = which(abs(time - cygnss_2[,ncol(cygnss_2)]) == min( abs(time - cygnss_2[,ncol(cygnss_2)][which(abs(time - cygnss_2[,ncol(cygnss_2)]) < tolerance )]) ))
    done_2 = T
    i2last = i
    points(cygnss_2[i[1],3+2], cygnss_2[i[1],2+2], pch = 16, col = 'orange', cex = 2)
  } else if (done_2){
    points(cygnss_2[i2last[1],3+2], cygnss_2[i2last[1],2+2], pch = 16, col = 'orange', cex = 2)
  }
  if (any( abs(time - cygnss_3[,ncol(cygnss_3)]) < tolerance ) ) {
    i = which(abs(time - cygnss_3[,ncol(cygnss_3)]) == min( abs(time - cygnss_3[,ncol(cygnss_3)][which(abs(time - cygnss_3[,ncol(cygnss_3)]) < tolerance )]) ))
    done_3 = T
    i3last = i
    points(cygnss_3[i[1],3+2], cygnss_3[i[1],2+2], pch = 16, col = 'yellow', cex = 2)
  } else if (done_3){
    points(cygnss_3[i3last[1],3+2], cygnss_3[i3last[1],2+2], pch = 16, col = 'yellow', cex = 2)
  }
  if (any( abs(time - cygnss_4[,ncol(cygnss_4)]) < tolerance ) ) {
    i = which(abs(time - cygnss_4[,ncol(cygnss_4)]) == min( abs(time - cygnss_4[,ncol(cygnss_4)][which(abs(time - cygnss_4[,ncol(cygnss_4)]) < tolerance )]) ))
    done_4 = T
    i4last = i
    points(cygnss_4[i[1],3+2], cygnss_4[i[1],2+2], pch = 16, col = 'green', cex = 2)
  } else if (done_4){
    points(cygnss_4[i4last[1],3+2], cygnss_4[i4last[1],2+2], pch = 16, col = 'green', cex = 2)
  }
  if (any( abs(time - cygnss_5[,ncol(cygnss_5)]) < tolerance ) ) {
    i = which(abs(time - cygnss_5[,ncol(cygnss_5)]) == min( abs(time - cygnss_5[,ncol(cygnss_5)][which(abs(time - cygnss_5[,ncol(cygnss_5)]) < tolerance )]) ))
    done_5 = T
    i5last = i
    points(cygnss_5[i[1],3+2], cygnss_5[i[1],2+2], pch = 16, col = 'aquamarine', cex = 2)
  } else if (done_5){
    points(cygnss_5[i5last[1],3+2], cygnss_5[i5last[1],2+2], pch = 16, col = 'aquamarine', cex = 2)
  }
  if (any( abs(time - cygnss_6[,ncol(cygnss_6)]) < tolerance ) ) {
    i = which(abs(time - cygnss_6[,ncol(cygnss_6)]) == min( abs(time - cygnss_6[,ncol(cygnss_6)][which(abs(time - cygnss_6[,ncol(cygnss_6)]) < tolerance )]) ))
    done_6 = T
    i6last = i
    points(cygnss_6[i[1],3+2], cygnss_6[i[1],2+2], pch = 16, col = 'blue', cex = 2)
  } else if (done_6){
    points(cygnss_6[i6last[1],3+2], cygnss_6[i6last[1],2+2], pch = 16, col = 'blue', cex = 2)
  }
  if (any( abs(time - cygnss_7[,ncol(cygnss_7)]) < tolerance ) ) {
    i = which(abs(time - cygnss_7[,ncol(cygnss_7)]) == min( abs(time - cygnss_7[,ncol(cygnss_7)][which(abs(time - cygnss_7[,ncol(cygnss_7)]) < tolerance )]) ))
    done_7 = T
    i7last = i
    points(cygnss_7[i[1],3+2], cygnss_7[i[1],2+2], pch = 16, col = 'pink', cex = 2)
  } else if (done_7){
    points(cygnss_7[i7last[1],3+2], cygnss_7[i7last[1],2+2], pch = 16, col = 'pink', cex = 2)
  }
  if (any( abs(time - cygnss_8[,ncol(cygnss_8)]) < tolerance ) ) {
    i = which(abs(time - cygnss_8[,ncol(cygnss_8)]) == min( abs(time - cygnss_8[,ncol(cygnss_8)][which(abs(time - cygnss_8[,ncol(cygnss_8)]) < tolerance )]) ))
    done_8 = T
    i8last = i
    points(cygnss_8[i[1],3+2], cygnss_8[i[1],2+2], pch = 16, col = 'purple', cex = 2)
  } else if (done_8) {
    points(cygnss_8[i8last[1],3+2], cygnss_8[i8last[1],2+2], pch = 16, col = 'purple', cex = 2)
  }
  
  
  # jason sat
  if (any( abs(time - jason_dat[,ncol(jason_dat)]) < tolerance ) ) {
    i = which(abs(time - jason_dat[,ncol(jason_dat)]) == min( abs(time - jason_dat[,ncol(jason_dat)][which(abs(time - jason_dat[,ncol(jason_dat)]) < tolerance )]) ))
    done_j = T
    ijlast = i
    points(jason_dat[i[1],2], jason_dat[i[1],1], pch = 18, col = 1, cex = 2)
  } else if (done_j){
    points(jason_dat[ijlast[1],2], jason_dat[ijlast[1],1], pch = 18, col = 1, cex = 2)
  }
  
  # add legend
  if (ix>2) {legend("bottomright", legend = 'Jason', pch = 18, col = 1, cex = 1.2, pt.cex = 1.85)}
  ix = ix+1
  ani.record()  # record the current frame
}

## now we can replay it, with an appropriate pause between
## frames
oopts = ani.options(interval = 0.2)
ani.replay()

## save video
saveVideo(ani.replay(), video.name = "animation_full_cygsclatsclon.mp4") # if want fast, use interval = 0.05





