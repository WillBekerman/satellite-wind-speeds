
# Assume we are running from the R_scripts directory
# source functions
source("../R/load_data.R")

# define cutoffs (in km)
t1 = 25

# load in paired data
save_dir <- "../figures"
data_dir <- "../data/empirical_plot_data/"
data_files <- list.files( data_dir, pattern = "\\.RData$", full.names = TRUE )
nperiods <- length(data_files)/2

# read the paired data into a list of lists for each antenna
# first index of each list is time period, second index is cygnss satellite number
jason_port=list()
jason_starboard=list()
cygnss_port=list()
cygnss_starboard=list()
for(j in 1:8){
  jason_port[[j]] <- numeric()
  jason_starboard[[j]] <- numeric()
  cygnss_port[[j]] <- numeric()
  cygnss_starboard[[j]] <- numeric()
}

for(j in 1:length(data_files)){
  load(data_files[j])
  if (grepl("starboard",data_files[j])){ # starboard antenna
    for (sat in 1:8){
      jason_speed = jws_l[[sat]][which(dist_l[[sat]]<t1)]
      cygnss_speed = cws_l[[sat]][which(dist_l[[sat]]<t1)]
      jason_starboard[[sat]] <- c(jason_starboard[[sat]], jason_speed )
      cygnss_starboard[[sat]] <- c(cygnss_starboard[[sat]], cygnss_speed )
    }
  }
  else{ # port antenna
    for (sat in 1:8){
      jason_speed = jws_l[[sat]][which(dist_l[[sat]]<t1)]
      cygnss_speed = cws_l[[sat]][which(dist_l[[sat]]<t1)]
      jason_port[[sat]] <- c(jason_port[[sat]], jason_speed )
      cygnss_port[[sat]] <- c(cygnss_port[[sat]], cygnss_speed )
    }
  }
}

pdf("pair_average_difference.pdf", width=7, height=4.2)
par(mfrow=c(2,4),oma=c(1,2,0,0),mar=c(3,2,2,1), cex.axis=0.75)
for(j in 1:8){
    x1 <- jason_starboard[[j]]
    x2 <- cygnss_starboard[[j]]
    plot( 0, type="n", xlim = c(0,15), ylim = c(-15,15),
        xlab = "Pair Average (m/s)",
        ylab = "Pair Difference (m/s)"
    )
    abline(0,0,col="magenta")
    points(0.5*(x1+x2), x2 -x1,
        xlim = c(0,15),
        ylim = c(-15,15),
        pch = 16,
        cex = 0.2
    )
    mtext(paste0("CYGNSS ",j," Port vs Jason-3"),line=0.5,cex=0.7)
    if( j %in% c(1,5) ){ mtext("Pair Difference (m/s)", side=2, line = 2.5, cex = 0.7) }
    if( j > 4 ){ mtext("Pair Average (m/s)", side=1, line = 2.5, cex = 0.7) }
}
dev.off()
