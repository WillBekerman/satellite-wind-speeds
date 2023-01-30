library("ncdf4") # package for netcdf manipulation

# assume we are in R_scripts directory. list only the files with the .nc extension
nc_filepath <- "../data/downloaded_cygnss"
filelist <- list.files(nc_filepath, pattern = "\\.nc$")

# extract the dates of the files. assumes these are in positions 11-18 of the filename
dates_text <- substr( filelist, 11, 18 )
dates <- sort( as.Date( dates_text, format = "%Y%m%d" ) )

# loop over the files
for(j in 1:length(filelist)){
  
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
  
  # coerce to data frame, get rid of missing rows
  df <- as.data.frame( varlist )
  nrows_unfiltered <- nrow(df)
  df <- na.omit(df)
  nrows_filtered <- nrow(df)

  # close .nc file
  nc_close(ncin_old)
  
  # print results
  cat("Date:", as.character(dates[j]) )
  cat('\n')
  cat("Unfiltered row count:", nrows_unfiltered)
  cat('\n')
  cat("Filtered row count:", nrows_filtered)
  cat('\n')
  cat("Proportion of rows kept after filtering:", nrows_filtered/nrows_unfiltered)
  cat('\n')
  cat('\n')
  
}


