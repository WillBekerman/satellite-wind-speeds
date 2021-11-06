# load libraries
library(ggplot2)
library(gridExtra)

# Assume we are running from the R_scripts directory
# source functions
source("../R/load_data.R")

# load in fit models
save_dir <- "../figures"
models_dir <- "../model_fits"
model_files <- list.files( models_dir, pattern = "\\.RData$", full.names = TRUE )
nperiods <- length(model_files)

# read the models into a list of lists
# first index of list is time period, second index is cygnss satellite number
models <- list()
for(j in 1:length(model_files)){
    models[[j]] <- loadRData( model_files[j] )
}

# put the parameter estimates in named data frames, for compatability with ggplot
parameter_frame <- as.data.frame( matrix(NA, 8*nperiods, 1 + 1 + 6 + 6 + 9) )
beta_names <- paste0( "b", 0:5 )
sebeta_names <- paste0( "se", beta_names )
covparm_names <- paste0( "c", 0:8 ) 
colnames(parameter_frame) <- c("period","sat",beta_names, sebeta_names, covparm_names)

for(p in 1:nperiods){
    for(sat in 1:8){
        r <- (p-1)*8 + sat
        parameter_frame[r,"period"] <- p
        parameter_frame[r,"sat"] <- sat
        if ( all(is.na(models[[p]][[sat]])) ) { next } # handle case of zero cyg_sat observations
        parameter_frame[r,beta_names] <- models[[p]][[sat]]$betahat
        parameter_frame[r,sebeta_names] <- models[[p]][[sat]]$sebeta
        parameter_frame[r,covparm_names] <- models[[p]][[sat]]$covparms
    }
}

# handle case of zero cyg_sat observations
parameter_frame <- na.omit(parameter_frame)

# CYGNSS satellite bias estimates plot
bias_plot <-
    ggplot( parameter_frame, aes(x=sat, y = b2 ) ) + 
    geom_point(size = 3, shape = 18, color = 'red') +
    labs(title="Satellite Bias Estimates", x="CYGNSS Satellite Number", y = "Wind Speed (m/s)")

pdf(file.path( save_dir, 'cygnss_bias_plot.pdf' ), width = 8, height = 5)
bias_plot
dev.off()

# Covariance parameter estimates plot
pp <- list()
for(j in 1:length(covparm_names)){
    pp[[j]] <-
        ggplot(parameter_frame, aes_string(x="sat", y=covparm_names[j])) +
        geom_point(size = 2.5, shape = 18, color = 'red') +
        labs(title=paste0("c", j-1, " Estimates"), x="CYGNSS Satellite Number", y = covparm_names[j])
}

pdf(file.path( save_dir, 'covparms_plot.pdf' ), width = 10, height = 10)
grid.arrange(pp[[1]],pp[[2]],pp[[3]],pp[[4]],pp[[5]],pp[[6]],pp[[7]],pp[[8]],pp[[9]], nrow=3)
dev.off()

# Three important plots together -- cygnss satellite bias estimates and variance components
nugget_plot <-
    ggplot( parameter_frame, aes(x=sat, y = c0*c8 ) ) + 
    geom_point(size = 3, shape = 18, color = 'red') +
    labs(title="Nugget Estimates", x="CYGNSS Satellite Number", y = "Wind Speed (m/s)")
    
sat_variance_plot <-
    ggplot( parameter_frame, aes(x=sat, y = c4 ) ) + 
    geom_point(size = 3, shape = 18, color = 'red') +
    labs(title="Satellite Variance Estimates", x="CYGNSS Satellite Number", y = "Wind Speed (m/s)")
    
pdf(file.path( save_dir, 'three_plot.pdf' ), width = 12, height = 4)
grid.arrange(bias_plot, nugget_plot, sat_variance_plot, nrow = 1 )
dev.off()

# To Will:
# I'm not sure how to modify the histogram plot for multiple periods, partly because I'm
# not good at ggplot, but also because we would get 8 different histograms for each period.

# Estimated distributions of WS differences between CYGNSS and Jason-3 plot
p1 <- ggplot(data = data.frame(x = c(-10, 10)), aes(x=x)) +
  stat_function(fun = dnorm, args = list(mean = b2[1], sd = sqrt(2*(c0[1]*c8[1]+c4[1]))), aes(colour = "CYGNSS 1")) + ylab("") +
  stat_function(fun = dnorm, args = list(mean = b2[2], sd = sqrt(2*(c0[2]*c8[2]+c4[2]))), aes(colour = "CYGNSS 2")) + ylab("") +
  stat_function(fun = dnorm, args = list(mean = b2[3], sd = sqrt(2*(c0[3]*c8[3]+c4[3]))), aes(colour = "CYGNSS 3")) + ylab("") +
  stat_function(fun = dnorm, args = list(mean = b2[4], sd = sqrt(2*(c0[4]*c8[4]+c4[4]))), aes(colour = "CYGNSS 4")) + ylab("") +
  stat_function(fun = dnorm, args = list(mean = b2[5], sd = sqrt(2*(c0[5]*c8[5]+c4[5]))), aes(colour = "CYGNSS 5")) + ylab("") +
  stat_function(fun = dnorm, args = list(mean = b2[6], sd = sqrt(2*(c0[6]*c8[6]+c4[6]))), aes(colour = "CYGNSS 6")) + ylab("") +
  stat_function(fun = dnorm, args = list(mean = b2[7], sd = sqrt(2*(c0[7]*c8[7]+c4[7]))), aes(colour = "CYGNSS 7")) + ylab("") +
  stat_function(fun = dnorm, args = list(mean = b2[8], sd = sqrt(2*(c0[8]*c8[8]+c4[8]))), aes(colour = "CYGNSS 8")) + ylab("")+
  scale_colour_manual("CYGNSS Satellite", values = c("red", "orange", "yellow", "green", "blue", "purple", "brown", "black")) +
  scale_y_continuous(name = "Density")+scale_x_continuous(name = "Difference in Wind Speed (m/s)")+ ggtitle("Estimated Distributions of Difference in Wind Speed Measurements \nbetween CYGNSS and Jason-3 Satellites at Same Spacetime")
pdf(file.path( save_dir, 'distributions_plot.pdf' ), width = 8, height = 4)
grid.arrange(p1)
dev.off()

rm(list = ls())
gc()

