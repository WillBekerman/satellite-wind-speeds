library("ncdf4") # package for netcdf manipulation

# assume we are in R_scripts directory. list only the files with the .nc extension
nc_filepath <- "../data/downloaded_jason"
processed_filepath <- "../data/processed_notdaily_jason"
filelist <- list.files(nc_filepath, pattern = "\\.nc$")

# standardize times as seconds since midnight on 2019-01-01
nday = as.numeric(difftime(as.Date('2019-01-01'), as.Date('2000-01-01'), units='days'))
time_offset = -86400*nday

# loop over the files
for(j in 1:length(filelist)){
  
    # process jth .nc file
    ncin_old <- nc_open( file.path( nc_filepath, filelist[j] ) )
    
    # define variables that we want to extract
    vars = c(
        "lat", "lon", "surface_type", "alt", "bathymetry",
        "wind_speed_alt", "wind_speed_alt_mle3", "time"
    )

    # loop over the variables, put in varlist, replace fillvalue with NA
    varlist <- list()
    for (att in vars){
      fillvalue <- ncatt_get(ncin_old, att, "_FillValue")
      varlist[[att]] <- ncvar_get(ncin_old, att)
      varlist[[att]][ varlist[[att]] == fillvalue$value ] <- NA
    }

    # coerce to data frame, get rid of missing rows, apply time offset
    df <- as.data.frame( varlist )
    df <- na.omit(df)
    df$time <- df$time + time_offset
    
    # close .nc file
    nc_close(ncin_old)
    
    # save it
    filename <- sub( "\\.nc$", "\\.RData", filelist[j] )
    save(df, file = file.path(processed_filepath, filename )) # save .RData file

}


