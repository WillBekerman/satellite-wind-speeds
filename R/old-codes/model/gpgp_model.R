source('R/data-processing/data_processing.R')

library(fields)
library(GpGp)
library(maps)
library(ggplot2)

numsat = 20000
set.seed(100)

# Sample data (20000 obs/sat)
locs = matrix(NA, nrow = numsat*9, ncol = 4)
windspeed = rep(0, numsat*9)

for (cyg_sat in 1:8){
  dat = cygnss_dat[which(cygnss_dat[,1] == cyg_sat),]
  obs = sample(1:nrow(dat), numsat)
  
  locs[(numsat*(cyg_sat-1)+1):(numsat*cyg_sat),1] = (dat[,2])[obs]
  locs[(numsat*(cyg_sat-1)+1):(numsat*cyg_sat),2] = (dat[,3])[obs]
  locs[(numsat*(cyg_sat-1)+1):(numsat*cyg_sat),3] = (dat[,8])[obs]
  locs[(numsat*(cyg_sat-1)+1):(numsat*cyg_sat),4] = (dat[,1])[obs]
  
  windspeed[(numsat*(cyg_sat-1)+1):(numsat*cyg_sat)] = dat[,6][obs]
}

obs = sample(1:nrow(jason_dat), numsat)
locs[((numsat*8+1):(numsat*9)),1] = (jason_dat[,1])[obs]
locs[((numsat*8+1):(numsat*9)),2] = (jason_dat[,2])[obs]
locs[((numsat*8+1):(numsat*9)),3] = (jason_dat[,8])[obs]
locs[((numsat*8+1):(numsat*9)),4] = rep(0,numsat)

windspeed[((numsat*8+1):(numsat*9))] = jason_dat[,6][obs]


lats = locs[,1]
lats_one = lats
lats_sq = lats^2
lats_cub = lats^3
longs = locs[,2]
longs = longs - 180 # get into range (-180, 180)

lats = lats*pi/180 # convert to radians
longs = longs*pi/180

x = 6371*cos(lats)*cos(longs)
y = 6371*cos(lats)*sin(longs)
z = 6371*sin(lats)

locs_new = locs[,3:4]
xyz = cbind(x,y,z)
locs_new = cbind( xyz, locs_new)
locs = locs_new


c0 = numeric(8)
c1 = numeric(8)
c2 = numeric(8)
c3 = numeric(8)
c4 = numeric(8)
c5 = numeric(8)
c6 = numeric(8)
c7 = numeric(8)
c8 = numeric(8)
l = list()

# Fit models for each sat
for (sat in 1:8){ # sat 9 is Jason
  
  cat('\nCYGNSS Satellite Number:', sat)
  
  ### CYGNSS
    ## x,y,z,time,sat in locs
    ## intercept, time, sat, lat, lat^2, lat^3 in design mat
  locs_mod0 = locs[(numsat*(sat-1)+1):(numsat*sat), 1:4]
  locs_mod0 = cbind( locs_mod0, rep(0, numsat) )
  X0 = cbind( rep(1, numsat), locs_mod0 ) # linear covariates -- look into later
  
  lat1cyg = lats_one[(numsat*(sat-1)+1):(numsat*sat)]
  lat2cyg = lats_sq[(numsat*(sat-1)+1):(numsat*sat)]
  lat3cyg = lats_cub[(numsat*(sat-1)+1):(numsat*sat)]
  
  ### Jason
    ## x,y,z,time,sat in locs
    ## intercept, time, sat, lat, lat^2, lat^3 in design mat
  locs_mod1 = locs[((numsat*8+1):(numsat*9)), 1:4]
  locs_mod1 = cbind( locs_mod1, rep(1, numsat) )
  X1 = cbind( rep(1, numsat), locs_mod1 ) # linear covariates
  
  lat1jas = lats_one[((numsat*8+1):(numsat*9))]
  lat2jas = lats_sq[((numsat*8+1):(numsat*9))]
  lat3jas = lats_cub[((numsat*8+1):(numsat*9))]
  
  lat1 = c(lat1cyg, lat1jas)
  lat2 = c(lat2cyg, lat2jas)
  lat3 = c(lat3cyg, lat3jas)
  
  speeds = c(windspeed[(numsat*(sat-1)+1):(numsat*sat)], windspeed[((numsat*8+1):(numsat*9))])
  locs_mod = rbind(locs_mod0, locs_mod1)
  X = rbind(X0, X1)
  
  X = X[,-c(2:4)] # remove xyz as covariates
  
  X = cbind(X, lat1, lat2, lat3) # add lat, lat^2, lat^3 as covariates
  
  mod_spacetime_cat = fit_model(y = speeds, locs = locs_mod, X = X, covfun_name = 'matern_spacetime_categorical_local')
  parms = mod_spacetime_cat$covparms
  
  c0[sat] = parms[0+1]
  c1[sat] = parms[1+1]
  c2[sat] = parms[2+1]
  c3[sat] = parms[3+1]
  c4[sat] = parms[4+1]
  c5[sat] = parms[5+1]
  c6[sat] = parms[6+1]
  c7[sat] = parms[7+1]
  c8[sat] = parms[8+1]
  
  l[sat] = list(mod_spacetime_cat)
}

## takes ~ 2.5 hrs w/ 20000 obs/sat

ggplot(data.frame(2*(c0*c8+c4)), aes(x=1:8, y=2*(c0*c8+c4))) + geom_point(size = 3, shape = 18, color = 'red') + labs(title="Variance b/w CYGNSS and Jason Observations (20000 obs/satellite)", x="CYGNSS Satellite Number", y = "Variance")
ggplot(data.frame(c0*c8), aes(x=1:8, y=c0*c8)) + geom_point(size = 3, shape = 18, color = 'red') + labs(title="c0*c8 (20000 obs/satellite)", x="CYGNSS Satellite Number", y = "c0*c8")
ggplot(data.frame(c4), aes(x=1:8, y=c4)) + geom_point(size = 3, shape = 18, color = 'red') + labs(title="c4 (20000 obs/satellite)", x="CYGNSS Satellite Number", y = "c4")



## Predictions Maps (these are all using the most recently fit model -- Cygnss 8)

                  #%#%#%#%#%#%#%#%#% CYGNSS Prediction #%#%#%#%#%#%#%#%#%
# Set up prediction locations and design matrix
griddf = expand.grid(lngcoords = seq(from = 0, to = 360, by = 1),
                     latcoords = seq(from = -90, to = 90, by = 1))
#latcoords = seq(from = -66, to = 66, by = 1))
locs_pred = as.matrix(griddf)

lats = locs_pred[,2]
lats_one = lats
lats_sq = lats^2
lats_cub = lats^3
longs = locs_pred[,1]
longs = longs - 180 # get into range (-180, 180)

lats = lats*pi/180 # convert to radians
longs = longs*pi/180

x = 6371*cos(lats)*cos(longs)
y = 6371*cos(lats)*sin(longs)
z = 6371*sin(lats)

locs_pred = cbind(x,y,z)

#locs_pred = cbind(locs_pred, rep(100000, nrow(locs_pred))) # time is 100000
locs_pred = cbind(locs_pred, rep(median(locs_mod[,4]), nrow(locs_pred))) # time is median
locs_pred = cbind(locs_pred, rep(0, nrow(locs_pred))) # cygnss

X_pred = cbind( rep(1,nrow(locs_pred)), locs_pred)
X_pred = X_pred[,-c(2:4)] # no xyz as covariates

X_pred = cbind(X_pred, lats_one, lats_sq, lats_cub)

set.seed(100)
pred_spacetime_cygnss = predictions(fit = mod_spacetime_cat, locs_pred = locs_pred, X_pred = X_pred, m = 30)

image.plot(sort(unique(as.matrix(griddf)[,1])), sort(unique(as.matrix(griddf)[,2])), array(pred_spacetime_cygnss, c(length(unique(as.matrix(griddf)[,1])),length(unique(as.matrix(griddf)[,2])))), main='CYGNSS Wind Speed Prediction', ylab = 'Latitude', xlab = 'Longitude', zlim = c(1.68,16.2), legend.args=list(text='Wind speed (m/s)', side=4,line=2.5, cex=0.8), yaxt = 'n') # note that min value across three plots is 1.689, max is 16.118   
axis(side = 2, 
     at = seq(-90,90,30), 
     labels = seq(-90,90,30),tck=-.05)
map("world2", add=T, fill=T, col = 'white')


                      #%#%#%#%#%#%#%#%#% Jason Prediction #%#%#%#%#%#%#%#%#%
# Set up prediction locations and design matrix
griddf = expand.grid(lngcoords = seq(from = 0, to = 360, by = 1),
                     latcoords = seq(from = -90, to = 90, by = 1))
#latcoords = seq(from = -66, to = 66, by = 1))
locs_pred = as.matrix(griddf)

lats = locs_pred[,2]
lats_one = lats
lats_sq = lats^2
lats_cub = lats^3
longs = locs_pred[,1]
longs = longs - 180 # get into range (-180, 180)

lats = lats*pi/180 # convert to radians
longs = longs*pi/180

x = 6371*cos(lats)*cos(longs)
y = 6371*cos(lats)*sin(longs)
z = 6371*sin(lats)

locs_pred = cbind(x,y,z)

#locs_pred = cbind(locs_pred, rep(100000, nrow(locs_pred))) # time is 100000
locs_pred = cbind(locs_pred, rep(median(locs_mod[,4]), nrow(locs_pred))) # time is median
locs_pred = cbind(locs_pred, rep(1, nrow(locs_pred))) # jason

X_pred = cbind( rep(1,nrow(locs_pred)), locs_pred)
X_pred = X_pred[,-c(2:4)] # no xyz as covariates

X_pred = cbind(X_pred, lats_one, lats_sq, lats_cub)

set.seed(100)
#pred_spacetime_jason = predictions(fit = l[[7]], locs_pred = locs_pred, X_pred = X_pred, m = 30)
pred_spacetime_jason = predictions(fit = mod_spacetime_cat, locs_pred = locs_pred, X_pred = X_pred, m = 30)

image.plot(sort(unique(as.matrix(griddf)[,1])), sort(unique(as.matrix(griddf)[,2])), array(pred_spacetime_jason, c(length(unique(as.matrix(griddf)[,1])),length(unique(as.matrix(griddf)[,2])))), main='Jason Wind Speed Prediction', ylab = 'Latitude', xlab = 'Longitude', zlim = c(1.68,16.2), legend.args=list(text='Wind speed (m/s)', side=4,line=2.5, cex=0.8), yaxt = 'n') # note that min value across three plots is 1.689, max is 16.118   
axis(side = 2, 
     at = seq(-90,90,30), 
     labels = seq(-90,90,30),tck=-.05)
map("world2", add=T, fill=T, col = 'white')


                    #%#%#%#%#%#%#%#%#% "True WS" Prediction #%#%#%#%#%#%#%#%#%
# Set up prediction locations and design matrix
griddf = expand.grid(lngcoords = seq(from = 0, to = 360, by = 1),
                     latcoords = seq(from = -90, to = 90, by = 1))
#latcoords = seq(from = -66, to = 66, by = 1))
locs_pred = as.matrix(griddf)

lats = locs_pred[,2]
lats_one = lats
lats_sq = lats^2
lats_cub = lats^3
longs = locs_pred[,1]
longs = longs - 180 # get into range (-180, 180)

lats = lats*pi/180 # convert to radians
longs = longs*pi/180

x = 6371*cos(lats)*cos(longs)
y = 6371*cos(lats)*sin(longs)
z = 6371*sin(lats)

locs_pred = cbind(x,y,z)

#locs_pred = cbind(locs_pred, rep(100000, nrow(locs_pred))) # time is 100000
locs_pred = cbind(locs_pred, rep(median(locs_mod[,4]), nrow(locs_pred))) # time is median
locs_pred = cbind(locs_pred, rep(2, nrow(locs_pred))) # true prediction

X_pred = cbind( rep(1,nrow(locs_pred)), locs_pred)
X_pred = X_pred[,-c(2:4)] # no xyz as covariates

X_pred = cbind(X_pred, lats_one, lats_sq, lats_cub)

set.seed(100)
pred_spacetime_true = predictions(fit = mod_spacetime_cat, locs_pred = locs_pred, X_pred = X_pred, m = 30)

image.plot(sort(unique(as.matrix(griddf)[,1])), sort(unique(as.matrix(griddf)[,2])), array(pred_spacetime_true, c(length(unique(as.matrix(griddf)[,1])),length(unique(as.matrix(griddf)[,2])))), main='True Wind Speed Prediction', ylab = 'Latitude', xlab = 'Longitude', zlim = c(1.68,16.2), legend.args=list(text='Wind speed (m/s)', side=4,line=2.5, cex=0.8), yaxt = 'n') # note that min value across three plots is 1.689, max is 16.118   
axis(side = 2, 
     at = seq(-90,90,30), 
     labels = seq(-90,90,30),tck=-.05)
map("world2", add=T, fill=T, col = 'white')


