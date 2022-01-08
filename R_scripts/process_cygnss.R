library("ncdf4") # package for netcdf manipulation

# assume we are in R_scripts directory. list only the files with the .nc extension
nc_filepath <- "../data/downloaded_cygnss"
processed_filepath <- "../data/processed_daily_cygnss"
filelist <- list.files(nc_filepath, pattern = "\\.nc$")

# extract the dates of the files. assumes these are in positions 11-18 of the filename
dates_text <- substr( filelist, 11, 18 )
dates <- sort( as.Date( dates_text, format = "%Y%m%d" ) )

# loop over the files
for(j in 1:length(filelist)){
  
    # standardize times as seconds since midnight on 2020-01-01
    sec_offset <- as.numeric(difftime(dates[j], as.Date('2020-01-01'), units='secs'))
    time_offset <- sec_offset + 0.5
    
    # process jth .nc file
    ncin_old <- nc_open( file.path( nc_filepath, filelist[j] ) )
    
    # define variables that we want to extract
    vars <- c(
        "spacecraft_id", "lat", "lon", "sc_lat", "sc_lon", "wind_speed",
        "wind_speed_uncertainty", "sample_time", "antenna"
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
    df$sample_time <- df$sample_time + time_offset
    
    # close .nc file
    nc_close(ncin_old)
    
    # save it
    filename <- paste0('cygnss-', dates[j], '.RData')
    save(df, file = file.path(processed_filepath, filename )) # save .RData file

}


