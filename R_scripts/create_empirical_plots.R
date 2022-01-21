# load libraries
library(ggplot2)
library(gridExtra)

# Assume we are running from the R_scripts directory
# source functions
source("../R/load_data.R")

# define cutoffs (in km)
t1 = 25

# load in fit models
save_dir <- "../figures"
models_dir <- "../data/empirical_plot_data/"
model_files <- list.files( models_dir, pattern = "\\.RData$", full.names = TRUE )
nperiods <- length(model_files)/2

# read the models into a list of lists for each antenna
# first index of each list is time period, second index is cygnss satellite number
difs_port=list()
difs_starboard=list()
for(j in 1:8){
  difs_port[[j]] <- numeric()
  difs_starboard[[j]] <- numeric()
}

for(j in 1:length(model_files)){
  if (j %% 2 == 0){ # starboard antenna
    for (sat in 1:8){
      load(model_files[j])
      jason_speed = jws_l[[sat]][which(dist_l[[sat]]<t1)]
      cygnss_speed = cws_l[[sat]][which(dist_l[[sat]]<t1)]
      difs_starboard[[sat]] = c(difs_starboard[[sat]],mean(cygnss_speed-jason_speed))
    }
  }
  else{ # port antenna
    for (sat in 1:8){
      load(model_files[j])
      jason_speed = jws_l[[sat]][which(dist_l[[sat]]<t1)]
      cygnss_speed = cws_l[[sat]][which(dist_l[[sat]]<t1)]
      difs_port[[sat]] = c(difs_port[[sat]],mean(cygnss_speed-jason_speed))
    }
  }
}

# put the parameter estimates in named data frames, for compatability with ggplot
parameter_frame <- as.data.frame( matrix(NA, 8*nperiods, 1 + 1 + 2) )
beta_names <- c("b2","b3")
colnames(parameter_frame) <- c("period","sat",beta_names)

for(p in 1:nperiods){
  for(sat in 1:8){
    r <- (p-1)*8 + sat
    parameter_frame[r,"period"] <- p
    parameter_frame[r,"sat"] <- sat
    if ( all(is.na(difs_starboard[[sat]][p])) || all(is.na(difs_port[[sat]][p])) ) { next } # handle case of zero cyg_sat observations
    parameter_frame[r,"b2"] <- difs_starboard[[sat]][p]
    parameter_frame[r,"b3"] <- difs_port[[sat]][p]
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

pdf(file.path( save_dir, 'cygnss_bias_plot_allweeks_empirical.pdf' ), width = 8, height = 5)
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

pdf(file.path( save_dir, 'cygnss_bias_plot_allweeks_ci_empirical.pdf' ), width = 8, height = 5)
ci_plot
dev.off()

########## Empirical vs. Model-Based Bias Estimates ##########

##### Empirical vs. Model-Based Bias Estimates ####
emp_bias = parameter_frame_sat_factor_bias$mean
mod_bias = c(-0.83350768, -0.61337754, -0.56678786,  0.16565146, -0.59138114, -0.63610807, -0.12863345,
             0.07835286, -0.94083090, -0.68383480, -0.68908047,  0.04801258, -0.83953602, -0.73557632,
             -0.30802416, -0.08667706)
biases = data.frame(emp=emp_bias, mod=mod_bias, sat=as.factor( rep(1:8,2) ), antenna=as.factor( c(rep('Starboard',8),rep('Port',8)) ))

library(ggrepel)
empvmod = 
  ggplot(biases, aes(x=mod_bias, y=emp_bias, color=sat, group = antenna)) + 
  geom_point(aes(shape=antenna, color=sat), size = 3.5)+
  geom_abline(slope = 1, intercept = 0, linetype='dashed', color="black", size = 1)+
  scale_color_brewer(palette="Dark2") + 
  labs(title="CYGNSS Satellite Bias Estimates: Empirical vs. Model-Based", x="Model-Based Average Estimates", y = "Empirical Average Estimates")+
  scale_x_continuous(limits = c(-1.1,1.1))+scale_y_continuous(limits = c(-1.1, 1.1))+ 
  labs(color='CYGNSS\nSatellite',shape='CYGNSS\nAntenna')+
  theme(legend.position='bottom',legend.box='vertical')

pdf(file.path( save_dir, 'empvmod.pdf' ), width = 8, height = 6.5)
empvmod
dev.off()

