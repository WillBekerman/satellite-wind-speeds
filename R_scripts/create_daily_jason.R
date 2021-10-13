# working directory assumed to be "R_scripts"

# source functions
source("../R/load_data.R")

# list only the files with the .RData extension
filepath <- "../data/processed_notdaily_jason"
filelist <- list.files(filepath, pattern = "\\.RData$")

# find the date range of these files
start_dates <- as.Date( substr( filelist, 21, 28 ), format = "%Y%m%d" )
end_dates <- as.Date( substr( filelist, 37, 44 ), format = "%Y%m%d" )
dates <- seq.Date( min(start_dates), max(end_dates), by = 1 )

# path for saving daily files
savepath <- file.path("../data/processed_daily_jason")

# loop over the dates
for (j in 1:length(dates)){
  
    # find the files with data for this date
    files_today <- filelist[ grep( format(dates[j], "%Y%m%d" ), filelist ) ]

    # load each of them in, put them in a temporary data frame
    temp_df <- loadRData( file.path( filepath, files_today[1] ) )

    if( length(files_today) > 1 ){

        for(k in 2:length(files_today)){

            temp_df <- rbind( temp_df, loadRData( file.path( filepath, files_today[k] ) ) )

        }

    }

    # subset to the time period corresponding to this date, remove missing values
    nday <- as.numeric(difftime(dates[j], as.Date('2019-01-01'), units='days'))
    time_range <- 24*60*60*c(nday,nday+1)
    df <- subset( temp_df, temp_df$time >= time_range[1] & temp_df$time < time_range[2] )
    df <- na.omit(df)

    # save
    filename <- paste0("jason-",dates[j],".RData")
    save(df, file = file.path(savepath, filename))
  
}
  

