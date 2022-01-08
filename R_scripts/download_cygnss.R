
setwd("../data/downloaded_cygnss")

#args <- c("2020","1","2")
args <- commandArgs(trailingOnly=TRUE)
year <- args[1]
cycle_start <- as.numeric( args[2] )
cycle_end <- as.numeric( args[3] )

date_start <- as.Date(paste(year,"01","01",sep="-")) + cycle_start - 1

vars <- c(
    "spacecraft_id", "lat", "lon", "sc_lat", "sc_lon", "wind_speed",
    "wind_speed_uncertainty", "sample_time", "antenna"
)
varlist <- paste(vars,collapse=",")


url_stem <- "https://podaac-opendap.jpl.nasa.gov/opendap/allData/cygnss/L2/v3.0/"

for(j in 1:length(cycle_start:cycle_end)){
    
    j3 <- sprintf("%03d",cycle_start+j-1)
    cat(paste0("Cycle: ",j3,"\n"))

    ss <- paste0("s",format(date_start+j-1,"%Y%m%d"),"-000000-")
    ee <- paste0("e",format(date_start+j-1,"%Y%m%d"),"-235959")
    fname <- paste0("cyg.ddmi.",ss,ee,".l2.wind-mss.a30.d31.nc.nc")

    com <- paste0(
        "wget --http-user=wjb239 --http-password=5NGvYoziBgij2TMp9f5 -r ",
        "-nc -np -nd -A *.nc ",
        url_stem,year,"/",j3,"/",fname,"?",varlist
    )
    com <- paste0(
        "wget --http-user=wjb239 --http-password=5NGvYoziBgij2TMp9f5 ",
        url_stem,year,"/",j3,"/",fname,"?",varlist
    )
    print(com)
    system( com )
    file.copy( paste0(fname,"?",varlist), substr(fname,1,nchar(fname)-3) )
    file.remove( paste0(fname,"?",varlist) )

}
