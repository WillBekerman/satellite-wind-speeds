# load libraries
setwd('./GitHub/satellite-wind-speeds/R_scripts')
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
parameter_frame <- as.data.frame( matrix(NA, 8*nperiods, 1 + 1 + 7 + 7 + 5) )
beta_names <- paste0( "b", 0:6 )
sebeta_names <- paste0( "se", beta_names )
covparm_names <- paste0( "c", 0:4 ) 
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


########## REGULAR PLOT #########

##### REGULAR PLOT ####  
parameter_frame_starboard <- parameter_frame[,c('sat','b2')]
names(parameter_frame_starboard) <- c("sat", "b")
parameter_frame_starboard$sat <- parameter_frame_starboard$sat-0.1

parameter_frame_port <- parameter_frame[,c('sat','b3')]
names(parameter_frame_port) <- c("sat", "b")
parameter_frame_port$sat <- parameter_frame_port$sat+0.1

parameter_frame_bias <- rbind(parameter_frame_starboard, parameter_frame_port)
parameter_frame_bias$groupnew <- rep((1:length(parameter_frame$sat)),2)

bias_plot = 
  ggplot(parameter_frame_bias) +
  geom_point(aes(x=sat, y=b), size = 2, shape = 18, color = 'red') + 
  geom_line(aes(x=sat, y=b, group=groupnew), size=0.25) +
  labs(title="CYGNSS Satellite Bias Estimates", x="CYGNSS Satellite Number", y = "Wind Speed (m/s)")+
  geom_hline(yintercept=0, linetype="dashed", color = "black", size = 1.25)

pdf(file.path( save_dir, 'cygnss_bias_plot_allweeks.pdf' ), width = 8, height = 5)
bias_plot
dev.off()

########## 99% CIs ##########

##### 99% CIs ####
library(Hmisc)
library(dplyr)
parameter_frame_sat_factor = parameter_frame
parameter_frame_sat_factor$sat = as.factor(parameter_frame_sat_factor$sat)
parameter_frame_sat_factor_bias_starboard <- parameter_frame_sat_factor %>%
  group_by(sat) %>% summarise( n=n(), mean=mean(b2), sd=sd(b2) ) %>%
  mutate( se=sd/sqrt(n)) %>% mutate( ic=se * qt((1-0.01)/2 + 0.5, n-1))
parameter_frame_sat_factor_bias_port <- parameter_frame_sat_factor %>%
  group_by(sat) %>% summarise( n=n(), mean=mean(b3), sd=sd(b3) ) %>%
  mutate( se=sd/sqrt(n)) %>% mutate( ic=se * qt((1-0.01)/2 + 0.5, n-1))

parameter_frame_sat_factor_bias_starboard$sat <- ( (1:8)-0.1 )
parameter_frame_sat_factor_bias_port$sat <- ( (1:8)+0.1 )

parameter_frame_sat_factor_bias <- rbind(parameter_frame_sat_factor_bias_starboard, parameter_frame_sat_factor_bias_port)
parameter_frame_sat_factor_bias$group <- c(rep(1,8),rep(2,8))
parameter_frame_sat_factor_bias$groupnew <- rep(seq(1,8),2)

ci_plot = 
  ggplot(parameter_frame_sat_factor_bias) +
  geom_errorbar(aes(x=sat, ymin=mean-ic, ymax=mean+ic), width=0.4, colour="black") +
  geom_point(aes(x=sat, y=mean), size = 3, shape = 18, color = 'red') + 
  geom_line(aes(x=sat, y=mean, group=groupnew)) +
  labs(title="CYGNSS Satellite Bias Estimates (99% CI)", x="CYGNSS Satellite Number", y = "Wind Speed (m/s)")+
  geom_hline(yintercept=0, linetype="dashed", color = "black", size = 1.25)

pdf(file.path( save_dir, 'cygnss_bias_plot_allweeks_ci.pdf' ), width = 8, height = 5)
ci_plot
dev.off()

