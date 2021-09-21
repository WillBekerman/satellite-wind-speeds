source('R/data-processing/data_processing.R')

# Import libraries
library(fields)
library(maps)



# CYGNSS vs JASON WS plots 9/20-9/26 (same latitudes shown)
pdf('cygnss_vs_jason_plt.pdf', width = 10.5, height = 7)
par(mfrow=c(2,1), mar = c(0.75, 4, 2.5, 2) + 0.1)
quilt.plot(cygnss_dat[,3], cygnss_dat[,2], cygnss_dat[,6], main = 'CYGNSS (09/20/2020 - 09/26/2020)', zlim = c(0, 20), yaxt = 'n', xaxt = 'n')
map("world2", add=T, fill = T, col = 'white')

reduced_jason_dat = jason_dat[which(jason_dat[,1] > -38),]
reduced_jason_dat = reduced_jason_dat[which(reduced_jason_dat[,1] < 38),]

quilt.plot(reduced_jason_dat[,2][which(reduced_jason_dat[,3] == 0)], reduced_jason_dat[,1][which(reduced_jason_dat[,3] == 0)], reduced_jason_dat[,6][which(reduced_jason_dat[,3] == 0)], main = 'Jason-3 (09/20/2020 - 09/26/2020)', zlim = c(0, 20), yaxt = 'n', xaxt = 'n')
map("world2", add=T, fill = T, col = 'white')
dev.off()


# CYGNSS satellite orbits shown on globe (first 30k obs)
spacecrafts = c('CYGNSS 1', 'CYGNSS 2', 'CYGNSS 3', 'CYGNSS 4', 'CYGNSS 5', 'CYGNSS 6',
                'CYGNSS 7', 'CYGNSS 8')
plot(cygnss_dat[1:30000,3], cygnss_dat[1:30000,2], main = 'CYGNSS Satellite Orbits', xlab = 'Longitude', ylab = 'Latitude', col = cygnss_dat[1:30000,1])
legend("bottomright", spacecrafts[unique(cygnss_dat[1:30000,1])],col=unique(cygnss_dat[1:30000,1]), pch=16, cex=0.65, pt.cex = 1)
map("world2", add=T)



# Distance of each CYGNSS sat vs. JASON for one day
pdf('cygnss_vs_jason_dist_plt.pdf', width = 8, height = 14)

par(mfrow=c(4,1)) #save w/ 1000 height for 4x1

num_sec = 86400

par(mar = c(1.5, 4, 4.0, 2) + 0.1)
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
  if (time == 6*3600*numplots && ix != 4){ 
    par(mar = c(1.5, 4, 1.5, 2) + 0.1)
    plot(c(6*3600*numplots, 6*3600*(numplots+1)), c(0, 20000), cex = 0, xlab = '', ylab = 'Distance (1000 km)', type='n', yaxt = 'n', xaxt = 'n')
    ytick<-seq(0, 20000, by=2000)
    axis(side=2, at=ytick, labels = seq(0, 20, by=2))
    xtick<-seq(6*3600*numplots, (6*3600*(numplots+1)), by=1800)
    axis(side=1, at=xtick, labels = seq(6*60*numplots, 6*60*(numplots+1), by=30))
    numplots = numplots+1
  }
  else if (time == 6*3600*numplots && ix == 4){ 
    par(mar = c(4.0, 4, 1.5, 2) + 0.1)
    plot(c(6*3600*numplots, 6*3600*(numplots+1)), c(0, 20000), cex = 0, xlab = 'Time (min)', ylab = 'Distance (1000 km)', type='n', yaxt = 'n', xaxt = 'n')
    ytick<-seq(0, 20000, by=2000)
    axis(side=2, at=ytick, labels = seq(0, 20, by=2))
    xtick<-seq(6*3600*numplots, (6*3600*(numplots+1)), by=1800)
    axis(side=1, at=xtick, labels = seq(6*60*numplots, 6*60*(numplots+1), by=30))
    numplots = numplots+1
  }
  
  ix_1 = which(time_1 > times[ix] & time_1 < times[ix+1])
  lines(time_1[ix_1], dist_1[ix_1], col = 'red')
  
  ix_2 = which(time_2 > times[ix] & time_2 < times[ix+1])
  lines(time_2[ix_2], dist_2[ix_2], col = 'orange')
  
  ix_3 = which(time_3 > times[ix] & time_3 < times[ix+1])
  lines(time_3[ix_3], dist_3[ix_3], col = 'yellow')
  
  ix_4 = which(time_4 > times[ix] & time_4 < times[ix+1])
  lines(time_4[ix_4], dist_4[ix_4], col = 'green')
  
  ix_5 = which(time_5 > times[ix] & time_5 < times[ix+1])
  lines(time_5[ix_5], dist_5[ix_5], col = 'aquamarine')
  
  ix_6 = which(time_6 > times[ix] & time_6 < times[ix+1])
  lines(time_6[ix_6], dist_6[ix_6], col = 'blue')
  
  ix_7 = which(time_7 > times[ix] & time_7 < times[ix+1])
  lines(time_7[ix_7], dist_7[ix_7], col = 'pink')
  
  ix_8 = which(time_8 > times[ix] & time_8 < times[ix+1])
  lines(time_8[ix_8], dist_8[ix_8], col = 'purple')
  
  ix = ix + 1
  
}
dev.off()



par(mfrow=c(4,1)) #save w/ 1000 height for 4x1

numplots = 1
plot(c(0, 6*3600*numplots), c(0,10000), cex = 0, main = 'Distance vs Time', xlab = 'Time (min)', ylab = 'Distance (1000 km)', type='n', yaxt = 'n', xaxt = 'n')
ytick<-seq(0, 10000, by=2000)
axis(side=2, at=ytick, labels = seq(0, 10, by=2))
xtick<-seq(0, 6*3600*numplots, by=1800)
axis(side=1, at=xtick, labels = seq(0, 6*60*numplots, by=30))

times = seq(0, num_sec, by = num_sec/4)
ix = 1
for (time in times) {
  if (ix == 5) break
  if (time == 6*3600*numplots){ # 
    plot(c(6*3600*numplots, 6*3600*(numplots+1)), c(0, 10000), cex = 0, main = 'Distance vs Time', xlab = 'Time (min)', ylab = 'Distance (1000 km)', type='n', yaxt = 'n', xaxt = 'n')
    ytick<-seq(0, 10000, by=2000)
    axis(side=2, at=ytick, labels = seq(0, 10, by=2))
    xtick<-seq(6*3600*numplots, (6*3600*(numplots+1)), by=1800) #xtick<-seq(0, 6*3600*numplots, by=1800)
    axis(side=1, at=xtick, labels = seq(6*60*numplots, 6*60*(numplots+1), by=30))
    numplots = numplots+1
  }
  
  ix_1 = which(time_1 > times[ix] & time_1 < times[ix+1])
  lines(time_1[ix_1], dist_1[ix_1], col = 'red')
  
  ix_2 = which(time_2 > times[ix] & time_2 < times[ix+1])
  lines(time_2[ix_2], dist_2[ix_2], col = 'orange')
  
  ix_3 = which(time_3 > times[ix] & time_3 < times[ix+1])
  lines(time_3[ix_3], dist_3[ix_3], col = 'yellow')
  
  ix_4 = which(time_4 > times[ix] & time_4 < times[ix+1])
  lines(time_4[ix_4], dist_4[ix_4], col = 'green')
  
  ix_5 = which(time_5 > times[ix] & time_5 < times[ix+1])
  lines(time_5[ix_5], dist_5[ix_5], col = 'aquamarine')
  
  ix_6 = which(time_6 > times[ix] & time_6 < times[ix+1])
  lines(time_6[ix_6], dist_6[ix_6], col = 'blue')
  
  ix_7 = which(time_7 > times[ix] & time_7 < times[ix+1])
  lines(time_7[ix_7], dist_7[ix_7], col = 'pink')
  
  ix_8 = which(time_8 > times[ix] & time_8 < times[ix+1])
  lines(time_8[ix_8], dist_8[ix_8], col = 'purple')
  
  ix = ix + 1
}



# CYGNSS vs JASON WS plot for obs w/in 500km, 200km, 100km
pdf('cyg_vs_jason_difs_bydist_plt.pdf', width = 6, height = 6)
jason_speeds = c(jws_1[which(dist_1<500 & dist_1>200)], jws_2[which(dist_2<500 & dist_2>200)], jws_3[which(dist_3<500 & dist_3>200)], jws_4[which(dist_4<500 & dist_4>200)], jws_5[which(dist_5<500 & dist_5>200)], jws_6[which(dist_6<500 & dist_6>200)], jws_7[which(dist_7<500 & dist_7>200)], jws_8[which(dist_8<500 & dist_8>200)])
cygnss_speeds = c(cws_1[which(dist_1<500 & dist_1>200)], cws_2[which(dist_2<500 & dist_2>200)], cws_3[which(dist_3<500 & dist_3>200)], cws_4[which(dist_4<500 & dist_4>200)], cws_5[which(dist_5<500 & dist_5>200)], cws_6[which(dist_6<500 & dist_6>200)], cws_7[which(dist_7<500 & dist_7>200)], cws_8[which(dist_8<500 & dist_8>200)])

sat_cols = c(rep(1, length(cws_1[which(dist_1<500 & dist_1>200)])), rep(2, length(cws_2[which(dist_2<500 & dist_2>200)])), rep(3, length(cws_3[which(dist_3<500 & dist_3>200)])), rep(4, length(cws_4[which(dist_4<500 & dist_4>200)])),
             rep(5, length(cws_5[which(dist_5<500 & dist_5>200)])), rep(6, length(cws_6[which(dist_6<500 & dist_6>200)])), rep(7, length(cws_7[which(dist_7<500 & dist_7>200)])), rep(8, length(cws_8[which(dist_8<500 & dist_8>200)])))

plot(jason_speeds, cygnss_speeds, xlab = 'Jason-3 Wind Speed (m/s)', ylab = 'CYGNSS Wind Speed (m/s)',
     main = 'CYGNSS vs. Jason-3 Wind Speed Measurements', xlim = c(-0.25,9.58), ylim = c(-0.25,9.58), col = sat_cols, pch = 16, cex = 1.2) # circle

jason_speeds = c(jws_1[which(dist_1<200 & dist_1>100)], jws_2[which(dist_2<200 & dist_2>100)], jws_3[which(dist_3<200 & dist_3>100)], jws_4[which(dist_4<200 & dist_4>100)], jws_5[which(dist_5<200 & dist_5>100)], jws_6[which(dist_6<200 & dist_6>100)], jws_7[which(dist_7<200 & dist_7>100)], jws_8[which(dist_8<200 & dist_8>100)])
cygnss_speeds = c(cws_1[which(dist_1<200 & dist_1>100)], cws_2[which(dist_2<200 & dist_2>100)], cws_3[which(dist_3<200 & dist_3>100)], cws_4[which(dist_4<200 & dist_4>100)], cws_5[which(dist_5<200 & dist_5>100)], cws_6[which(dist_6<200 & dist_6>100)], cws_7[which(dist_7<200 & dist_7>100)], cws_8[which(dist_8<200 & dist_8>100)])

sat_cols = c(rep(1, length(cws_1[which(dist_1<200 & dist_1>100)])), rep(2, length(cws_2[which(dist_2<200 & dist_2>100)])), rep(3, length(cws_3[which(dist_3<200 & dist_3>100)])), rep(4, length(cws_4[which(dist_4<200 & dist_4>100)])),
             rep(5, length(cws_5[which(dist_5<200 & dist_5>100)])), rep(6, length(cws_6[which(dist_6<200 & dist_6>100)])), rep(7, length(cws_7[which(dist_7<200 & dist_7>100)])), rep(8, length(cws_8[which(dist_8<200 & dist_8>100)])))

points(jason_speeds, cygnss_speeds, col = sat_cols, pch = 15, cex = 1.2) # square

jason_speeds = c(jws_1[which(dist_1<100)], jws_2[which(dist_2<100)], jws_3[which(dist_3<100)], jws_4[which(dist_4<100)], jws_5[which(dist_5<100)], jws_6[which(dist_6<100)], jws_7[which(dist_7<100)], jws_8[which(dist_8<100)])
cygnss_speeds = c(cws_1[which(dist_1<100)], cws_2[which(dist_2<100)], cws_3[which(dist_3<100)], cws_4[which(dist_4<100)], cws_5[which(dist_5<100)], cws_6[which(dist_6<100)], cws_7[which(dist_7<100)], cws_8[which(dist_8<100)])

sat_cols = c(rep(1, length(cws_1[which(dist_1<100)])), rep(2, length(cws_2[which(dist_2<100)])), rep(3, length(cws_3[which(dist_3<100)])), rep(4, length(cws_4[which(dist_4<100)])),
             rep(5, length(cws_5[which(dist_5<100)])), rep(6, length(cws_6[which(dist_6<100)])), rep(7, length(cws_7[which(dist_7<100)])), rep(8, length(cws_8[which(dist_8<100)])))

points(jason_speeds, cygnss_speeds, col = sat_cols, pch = 17, cex = 1.2) # triangle
lines(-1:11, -1:11, type='l')
legend("bottomright", legend = c('< 500 km', '< 200 km', '< 100 km'), pch = c(16,15,17), col = 1, cex = 1.05, pt.cex = 1.2)
dev.off()



# Difference in WS for CYGNSS vs JASON vs distance b/w sats

jason_speeds = c(jws_1, jws_2, jws_3, jws_4, jws_5, jws_6, jws_7, jws_8)
cygnss_speeds = c(cws_1, cws_2, cws_3, cws_4, cws_5, cws_6, cws_7, cws_8)

distances = c(dist_1, dist_2, dist_3, dist_4, dist_5, dist_6, dist_7, dist_8)

sat_cols = c(rep(1, length(cws_1)), rep(2, length(cws_2)), rep(3, length(cws_3)), rep(4, length(cws_4)),
             rep(5, length(cws_5)), rep(6, length(cws_6)), rep(7, length(cws_7)), rep(8, length(cws_8)))

plot(distances, cygnss_speeds - jason_speeds, xlab = 'Distance b/w Satellites (km)', ylab = 'CYGNSS WS - Jason WS (m/s)',
     col = sat_cols, main = 'Difference in Wind Speed vs. Distance', cex = .5, xlim = c(0,1000), ylim = c(-10,10))

