# Import libraries
library(purrr)
library(R.utils)
library(rgdal) # package for geospatial analysis
library(ggplot2)
library(ncdf4) # package for netcdf manipulation

# Open CYGNSS data files
cygnss_092020_raw <- nc_open('cyg.ddmi.s20200920-000000-e20200920-235959.l2.wind-mss.a30.d31.nc')
cygnss_092120_raw <- nc_open('cyg.ddmi.s20200921-000000-e20200921-235959.l2.wind-mss.a30.d31.nc')
cygnss_092220_raw <- nc_open('cyg.ddmi.s20200922-000000-e20200922-235959.l2.wind-mss.a30.d31.nc')
cygnss_092320_raw <- nc_open('cyg.ddmi.s20200923-000000-e20200923-235959.l2.wind-mss.a30.d31.nc')
cygnss_092420_raw <- nc_open('cyg.ddmi.s20200924-000000-e20200924-235959.l2.wind-mss.a30.d31.nc')
cygnss_092520_raw <- nc_open('cyg.ddmi.s20200925-000000-e20200925-235959.l2.wind-mss.a30.d31.nc')
cygnss_092620_raw <- nc_open('cyg.ddmi.s20200926-000000-e20200926-235959.l2.wind-mss.a30.d31.nc')

# print(<name_of_dataset>) to see variables

# important variables: spacecraft_id, lat, lon, sc_lat, sc_lon, wind_speed, 
# wind_speed_uncertainty, sample_time

dats = c("cygnss_092020", "cygnss_092120", "cygnss_092220", "cygnss_092320", "cygnss_092420", "cygnss_092520", "cygnss_092620")
vars = c("spacecraft_id", "lat", "lon", "sc_lat", "sc_lon", "wind_speed", "wind_speed_uncertainty", "sample_time")

fillvalues = list()
for (att in vars){
  fillvalues[[att]] = ncatt_get(cygnss_092020_raw, att, "_FillValue")
}

for (dat in dats){
  dataset = get(paste(dat, "_raw", sep=""))
  varlist = list()
  for (var in vars) {
    varlist[[var]] = ncvar_get(dataset, var)
    varlist[[var]][varlist[[var]] == fillvalues[[var]]$value] <- NA
  }
  assign(paste(dat, "_df", sep=""), t(data.frame(matrix(unlist(varlist), nrow=length(varlist), byrow=TRUE),stringsAsFactors=FALSE)))
}

# change sample_times to seconds since since 2020-09-20 00:00:00.0000 
cygnss_092020_df[,length(vars)] = cygnss_092020_df[,length(vars)] + 0.5
cygnss_092120_df[,length(vars)] = cygnss_092120_df[,length(vars)] + 0.5 + 1*86400
cygnss_092220_df[,length(vars)] = cygnss_092220_df[,length(vars)] + 0.5 + 2*86400
cygnss_092320_df[,length(vars)] = cygnss_092320_df[,length(vars)] + 0.5 + 3*86400
cygnss_092420_df[,length(vars)] = cygnss_092420_df[,length(vars)] + 0.5 + 4*86400
cygnss_092520_df[,length(vars)] = cygnss_092520_df[,length(vars)] + 0.5 + 5*86400
cygnss_092620_df[,length(vars)] = cygnss_092620_df[,length(vars)] + 0.5 + 6*86400

# assign variable names to df column names
colnames(cygnss_092020_df) <- vars
colnames(cygnss_092120_df) <- vars
colnames(cygnss_092220_df) <- vars
colnames(cygnss_092320_df) <- vars
colnames(cygnss_092420_df) <- vars
colnames(cygnss_092520_df) <- vars
colnames(cygnss_092620_df) <- vars

# omit rows w/ NAs
cygnss_092020_df = na.omit(cygnss_092020_df)
cygnss_092120_df = na.omit(cygnss_092120_df)
cygnss_092220_df = na.omit(cygnss_092220_df)
cygnss_092320_df = na.omit(cygnss_092320_df)
cygnss_092420_df = na.omit(cygnss_092420_df)
cygnss_092520_df = na.omit(cygnss_092520_df)
cygnss_092620_df = na.omit(cygnss_092620_df)

cygnss_dat = do.call("rbind", list(cygnss_092020_df, cygnss_092120_df, cygnss_092220_df, cygnss_092320_df, 
                                   cygnss_092420_df, cygnss_092520_df, cygnss_092620_df))


# standardize satellite naming
cygnss_dat[,1][cygnss_dat[,1] == 247] = 1
cygnss_dat[,1][cygnss_dat[,1] == 249] = 2
cygnss_dat[,1][cygnss_dat[,1] == 43] = 3
cygnss_dat[,1][cygnss_dat[,1] == 44] = 4
cygnss_dat[,1][cygnss_dat[,1] == 47] = 5
cygnss_dat[,1][cygnss_dat[,1] == 54] = 6
cygnss_dat[,1][cygnss_dat[,1] == 55] = 7
cygnss_dat[,1][cygnss_dat[,1] == 73] = 8      


# Open JASON data files
jason_092020_raw <- nc_open('jason_092020.nc')
jason_092120_raw <- nc_open('jason_092120.nc')
jason_092220_raw <- nc_open('jason_092220.nc')
jason_092320_raw <- nc_open('jason_092320.nc')
jason_092420_raw <- nc_open('jason_092420.nc')
jason_092520_raw <- nc_open('jason_092520.nc')
jason_092620_raw <- nc_open('jason_092620.nc')

# print(<name_of_dataset>) to see variables

# important variables: spacecraft_id, lat, lon, sc_lat, sc_lon, wind_speed, 
# wind_speed_uncertainty, sample_time

dats = c("jason_092020", "jason_092120", "jason_092220", "jason_092320", "jason_092420", "jason_092520", "jason_092620")
vars = c("lat", "lon", "surface_type", "alt", "bathymetry", "wind_speed_alt", "wind_speed_alt_mle3", "time")

fillvalues = list()
for (att in vars){
  fillvalues[[att]] = ncatt_get(jason_092020_raw, att, "_FillValue")
}

for (dat in dats){
  dataset = get(paste(dat, "_raw", sep=""))
  varlist = list()
  for (var in vars) {
    varlist[[var]] = ncvar_get(dataset, var)
    varlist[[var]][varlist[[var]] == fillvalues[[var]]$value] <- NA
  }
  assign(paste(dat, "_df", sep=""), t(data.frame(matrix(unlist(varlist), nrow=length(varlist), byrow=TRUE),stringsAsFactors=FALSE)))
}

# change sample_times to seconds since since 2020-09-20 00:00:00.0000 
jason_092020_df[,length(vars)] = jason_092020_df[,length(vars)] - (56*60 + 1)
jason_092120_df[,length(vars)] = jason_092120_df[,length(vars)] + (21*60 + 47) + 1*86400
jason_092220_df[,length(vars)] = jason_092220_df[,length(vars)] + (43*60 + 21.9) + 2*86400
jason_092320_df[,length(vars)] = jason_092320_df[,length(vars)] + (8*60 + 43.8) + 3*86400
jason_092420_df[,length(vars)] = jason_092420_df[,length(vars)] + (30*60 + 18.6) + 4*86400
jason_092520_df[,length(vars)] = jason_092520_df[,length(vars)] + (51*60 + 53.5) + 5*86400
jason_092620_df[,length(vars)] = jason_092620_df[,length(vars)] + (17*60 + 15.6) + 6*86400

# assign variable names to df column names
colnames(jason_092020_df) <- vars
colnames(jason_092120_df) <- vars
colnames(jason_092220_df) <- vars
colnames(jason_092320_df) <- vars
colnames(jason_092420_df) <- vars
colnames(jason_092520_df) <- vars
colnames(jason_092620_df) <- vars

# omit rows w/ NAs
jason_092020_df = na.omit(jason_092020_df)
jason_092120_df = na.omit(jason_092120_df)
jason_092220_df = na.omit(jason_092220_df)
jason_092320_df = na.omit(jason_092320_df)
jason_092420_df = na.omit(jason_092420_df)
jason_092520_df = na.omit(jason_092520_df)
jason_092620_df = na.omit(jason_092620_df)

jason_dat = do.call("rbind", list(jason_092020_df, jason_092120_df, jason_092220_df, jason_092320_df, 
                                  jason_092420_df, jason_092520_df, jason_092620_df))

### Drop Jason observations not in 9/20 - 9/26
jason_dat = jason_dat[-which(jason_dat[,ncol(jason_dat)] < 0),]
jason_dat = jason_dat[-which(jason_dat[,ncol(jason_dat)] > 86400*7),]

#since cygnss does not record measurements over
#land and Jason does (but are told to be inaccurate), we only keep Jason observations
#over open oceans/semi-enclosed seas
#surface_type: 0 = open oceans or semi-enclosed seas; 1 = enclosed seas or lakes; 2 = continental ice; 3 = land
jason_dat = jason_dat[which(jason_dat[,3] == 0),]


#### Sanity check: similar times for CYGNSS and Jason satellites
summary(jason_dat[,ncol(jason_dat)])
summary(cygnss_dat[,ncol(cygnss_dat)])
















time_1 = dist_1 = time_2 = dist_2 = time_3 = dist_3 = time_4 = dist_4 = time_5 = dist_5 = time_6 = dist_6 = time_7 = dist_7 = time_8 = dist_8 = numeric()

for (time in (seq(0, num_sec, by = by))) {
  
  # if (time > 6*3600*numplots){ # 
  #   plot(0:20000, cex = 0, main = 'Distance vs Time', xlab = 'Time (min)', ylab = 'Distance (1000 km)', type='n', yaxt = 'n', xaxt = 'n')
  #   ytick<-seq(0, 20000, by=2000)
  #   axis(side=2, at=ytick, labels = seq(0, 20, by=2))
  #   xtick<-seq(0, 6*3600, by=1800)
  #   axis(side=1, at=xtick, labels = seq(6*60*numplots, 6*60*(numplots+1), by=30))
  #   numplots = numplots+1
  # }
  
  # go to next time in seq if cygnss_dat or jason_dat does not have measurement at time
  # within tolerance
  if (!any( abs(time - jason_dat[,ncol(jason_dat)]) < tolerance ) || !any( abs(time - cygnss_dat[,ncol(cygnss_dat)]) < tolerance ) ) {
    next
  }
  
  
  if (any( abs(time - cygnss_1[,ncol(cygnss_1)]) < tolerance ) ) {
    i = which(abs(time - cygnss_1[,ncol(cygnss_1)]) == min( abs(time - cygnss_1[,ncol(cygnss_1)][which(abs(time - cygnss_1[,ncol(cygnss_1)]) < tolerance )]) ))
    #points(cygnss_1[i[1],3], cygnss_1[i[1],2], pch = 16, col = 'red', cex = 2)
    
    ij = which(abs(time - jason_dat[,ncol(jason_dat)]) == min( abs(time - jason_dat[,ncol(jason_dat)][which(abs(time - jason_dat[,ncol(jason_dat)]) < tolerance )]) ))
    x1 = cbind(jason_dat[ij[1],2], jason_dat[ij[1],1]) # first col long, second col lat
    x2 = cbind(cygnss_1[i[1],3+2], cygnss_1[i[1],2+2]) # first col long, second col lat
    #points(time, distance(x1,x2), pch = 16, col = 'red')
    time_1 = c(time_1, time)
    dist_1 = c(dist_1, distance(x1,x2))
  }
  
  
  if (any( abs(time - cygnss_2[,ncol(cygnss_2)]) < tolerance ) ) {
    i = which(abs(time - cygnss_2[,ncol(cygnss_2)]) == min( abs(time - cygnss_2[,ncol(cygnss_2)][which(abs(time - cygnss_2[,ncol(cygnss_2)]) < tolerance )]) ))
    #points(cygnss_2[i[1],3], cygnss_2[i[1],2], pch = 16, col = 'orange', cex = 2)
    ij = which(abs(time - jason_dat[,ncol(jason_dat)]) == min( abs(time - jason_dat[,ncol(jason_dat)][which(abs(time - jason_dat[,ncol(jason_dat)]) < tolerance )]) ))
    x1 = cbind(jason_dat[ij[1],2], jason_dat[ij[1],1]) # first col long, second col lat
    x2 = cbind(cygnss_2[i[1],3+2], cygnss_2[i[1],2+2]) # first col long, second col lat
    #points(time, distance(x1,x2), pch = 16, col = 'orange')
    time_2 = c(time_2, time)
    dist_2 = c(dist_2, distance(x1,x2))
  }
  
  if (any( abs(time - cygnss_3[,ncol(cygnss_3)]) < tolerance ) ) {
    i = which(abs(time - cygnss_3[,ncol(cygnss_3)]) == min( abs(time - cygnss_3[,ncol(cygnss_3)][which(abs(time - cygnss_3[,ncol(cygnss_3)]) < tolerance )]) ))
    #points(cygnss_3[i[1],3], cygnss_3[i[1],2], pch = 16, col = 'yellow', cex = 2)
    ij = which(abs(time - jason_dat[,ncol(jason_dat)]) == min( abs(time - jason_dat[,ncol(jason_dat)][which(abs(time - jason_dat[,ncol(jason_dat)]) < tolerance )]) ))
    x1 = cbind(jason_dat[ij[1],2], jason_dat[ij[1],1]) # first col long, second col lat
    x2 = cbind(cygnss_3[i[1],3+2], cygnss_3[i[1],2+2]) # first col long, second col lat
    #points(time, distance(x1,x2), pch = 16, col = 'yellow')
    time_3 = c(time_3, time)
    dist_3 = c(dist_3, distance(x1,x2))
  }
  
  if (any( abs(time - cygnss_4[,ncol(cygnss_4)]) < tolerance ) ) {
    i = which(abs(time - cygnss_4[,ncol(cygnss_4)]) == min( abs(time - cygnss_4[,ncol(cygnss_4)][which(abs(time - cygnss_4[,ncol(cygnss_4)]) < tolerance )]) ))
    #points(cygnss_4[i[1],3], cygnss_4[i[1],2], pch = 16, col = 'green', cex = 2)
    ij = which(abs(time - jason_dat[,ncol(jason_dat)]) == min( abs(time - jason_dat[,ncol(jason_dat)][which(abs(time - jason_dat[,ncol(jason_dat)]) < tolerance )]) ))
    x1 = cbind(jason_dat[ij[1],2], jason_dat[ij[1],1]) # first col long, second col lat
    x2 = cbind(cygnss_4[i[1],3+2], cygnss_4[i[1],2+2]) # first col long, second col lat
    #points(time, distance(x1,x2), pch = 16, col = 'green')
    time_4 = c(time_4, time)
    dist_4 = c(dist_4, distance(x1,x2))
  }
  
  
  if (any( abs(time - cygnss_5[,ncol(cygnss_5)]) < tolerance ) ) {
    i = which(abs(time - cygnss_5[,ncol(cygnss_5)]) == min( abs(time - cygnss_5[,ncol(cygnss_5)][which(abs(time - cygnss_5[,ncol(cygnss_5)]) < tolerance )]) ))
    #points(cygnss_5[i[1],3], cygnss_5[i[1],2], pch = 16, col = 'aquamarine', cex = 2)
    ij = which(abs(time - jason_dat[,ncol(jason_dat)]) == min( abs(time - jason_dat[,ncol(jason_dat)][which(abs(time - jason_dat[,ncol(jason_dat)]) < tolerance )]) ))
    x1 = cbind(jason_dat[ij[1],2], jason_dat[ij[1],1]) # first col long, second col lat
    x2 = cbind(cygnss_5[i[1],3+2], cygnss_5[i[1],2+2]) # first col long, second col lat
    #points(time, distance(x1,x2), pch = 16, col = 'aquamarine')
    time_5 = c(time_5, time)
    dist_5 = c(dist_5, distance(x1,x2))
  }
  
  if (any( abs(time - cygnss_6[,ncol(cygnss_6)]) < tolerance ) ) {
    i = which(abs(time - cygnss_6[,ncol(cygnss_6)]) == min( abs(time - cygnss_6[,ncol(cygnss_6)][which(abs(time - cygnss_6[,ncol(cygnss_6)]) < tolerance )]) ))
    #points(cygnss_6[i[1],3], cygnss_6[i[1],2], pch = 16, col = 'blue', cex = 2)
    ij = which(abs(time - jason_dat[,ncol(jason_dat)]) == min( abs(time - jason_dat[,ncol(jason_dat)][which(abs(time - jason_dat[,ncol(jason_dat)]) < tolerance )]) ))
    x1 = cbind(jason_dat[ij[1],2], jason_dat[ij[1],1]) # first col long, second col lat
    x2 = cbind(cygnss_6[i[1],3+2], cygnss_6[i[1],2+2]) # first col long, second col lat
    #points(time, distance(x1,x2), pch = 16, col = 'blue')
    time_6 = c(time_6, time)
    dist_6 = c(dist_6, distance(x1,x2))
  }
  
  if (any( abs(time - cygnss_7[,ncol(cygnss_7)]) < tolerance ) ) {
    i = which(abs(time - cygnss_7[,ncol(cygnss_7)]) == min( abs(time - cygnss_7[,ncol(cygnss_7)][which(abs(time - cygnss_7[,ncol(cygnss_7)]) < tolerance )]) ))
    #points(cygnss_7[i[1],3], cygnss_7[i[1],2], pch = 16, col = 'pink', cex = 2)
    ij = which(abs(time - jason_dat[,ncol(jason_dat)]) == min( abs(time - jason_dat[,ncol(jason_dat)][which(abs(time - jason_dat[,ncol(jason_dat)]) < tolerance )]) ))
    x1 = cbind(jason_dat[ij[1],2], jason_dat[ij[1],1]) # first col long, second col lat
    x2 = cbind(cygnss_7[i[1],3+2], cygnss_7[i[1],2+2]) # first col long, second col lat
    #points(time, distance(x1,x2), pch = 16, col = 'pink')
    time_7 = c(time_7, time)
    dist_7 = c(dist_7, distance(x1,x2))
  }
  
  if (any( abs(time - cygnss_8[,ncol(cygnss_8)]) < tolerance ) ) {
    i = which(abs(time - cygnss_8[,ncol(cygnss_8)]) == min( abs(time - cygnss_8[,ncol(cygnss_8)][which(abs(time - cygnss_8[,ncol(cygnss_8)]) < tolerance )]) ))
    #points(cygnss_8[i[1],3], cygnss_8[i[1],2], pch = 16, col = 'purple', cex = 2)
    ij = which(abs(time - jason_dat[,ncol(jason_dat)]) == min( abs(time - jason_dat[,ncol(jason_dat)][which(abs(time - jason_dat[,ncol(jason_dat)]) < tolerance )]) ))
    x1 = cbind(jason_dat[ij[1],2], jason_dat[ij[1],1]) # first col long, second col lat
    x2 = cbind(cygnss_8[i[1],3+2], cygnss_8[i[1],2+2]) # first col long, second col lat
    #points(time, distance(x1,x2), pch = 16, col = 'purple')
    time_8 = c(time_8, time)
    dist_8 = c(dist_8, distance(x1,x2))
  }
  
}


