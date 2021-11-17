# load libraries
library(ggplot2)
library(fields)

# Assume we are running from the R_scripts directory

# Load in empirical plot data
empirical_plot_data_dir <- "../data/empirical_plot_data"
load( file.path(empirical_plot_data_dir, "empirical_plot_data_lists.RData") )

# Cutoffs (in km)
t1 = 100
t2 = 200
t3 = 500

par(mfrow = c(1,2)) # set plotting parameters
save_dir <- "../figures"

for (sat in 1:8){
  
  pdf(file.path( save_dir, paste0("empirical-plt-cyg",sat,".pdf") ), width = 4, height = 4)
  
  jason_speed = jws_l[[sat]][which(dist_l[[sat]]<t3 & dist_l[[sat]]>t2)]
  cygnss_speed = cws_l[[sat]][which(dist_l[[sat]]<t3 & dist_l[[sat]]>t2)]
  sat_col = rep(1, length(cws_l[[sat]][which(dist_l[[sat]]<t3 & dist_l[[sat]]>t2)]))
  plot(jason_speed, cygnss_speed, xlab = 'Jason WS (m/s)', ylab = 'CYGNSS WS (m/s)',
       main = paste0('CYGNSS Satellite ', sat, ' vs. Jason'), xlim = c(0,20), ylim = c(0,20), col = 2, pch = 19, cex = .95) # circle
  
  jason_speed = jws_l[[sat]][which(dist_l[[sat]]<t2 & dist_l[[sat]]>t1)]
  cygnss_speed = cws_l[[sat]][which(dist_l[[sat]]<t2 & dist_l[[sat]]>t1)]
  sat_col = rep(1, length(cws_l[[sat]][which(dist_l[[sat]]<t2 & dist_l[[sat]]>t1)]))
  points(jason_speed, cygnss_speed, col = 3, pch = 19, cex = .95) # square
  
  jason_speed = jws_l[[sat]][which(dist_l[[sat]]<t1)]
  cygnss_speed = cws_l[[sat]][which(dist_l[[sat]]<t1)]
  sat_col = rep(1, length(cws_l[[sat]][which(dist_l[[sat]]<t1)]))
  points(jason_speed, cygnss_speed, col = 4, pch = 19, cex = .95) # triangle
  
  legend('bottomright', legend= c( paste0('< ',t3,' km'), paste0('< ',t2,' km'), paste0('< ',t1,' km') ), fill=2:4, cex=0.8)
  abline(0,1)
  
  dev.off()
  
}

rm(list = ls())
gc()
