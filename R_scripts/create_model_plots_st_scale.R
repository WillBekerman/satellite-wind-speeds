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
model_files <- model_files[ grep("st_scale", model_files) ]
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
parameter_frame_starboard$sat <- parameter_frame_starboard$sat-0.2

parameter_frame_port <- parameter_frame[,c('sat','b3')]
names(parameter_frame_port) <- c("sat", "b")
parameter_frame_port$sat <- parameter_frame_port$sat+0.2

parameter_frame_bias <- rbind(parameter_frame_starboard, parameter_frame_port)
parameter_frame_bias$groupnew <- rep((1:length(parameter_frame$sat)),2)

bias_plot = 
  ggplot(parameter_frame_bias) +
  geom_point(aes(x=sat, y=b), size = 2, shape = 1, color = 'red') + 
  geom_line(aes(x=sat, y=b, group=groupnew), size=0.25) +
  labs(title="Model-Based CYGNSS Satellite Bias Estimates", x="CYGNSS Satellite Number", y = "Wind Speed (m/s)")+
  geom_hline(yintercept=0, linetype="dashed", color = "black", size = 1.25) +
  scale_x_continuous(breaks = c(0.8,1,1.2,1.8,2,2.2,2.8,3,3.2,3.8,4,4.2,4.8,5,5.2,5.8,6,6.2,6.8,7,7.2,7.8,8,8.2), 
                     labels = c("S","1","P","S","2","P","S","3","P","S","4","P","S","5","P","S","6","P","S","7","P","S","8","P")) +
  theme(axis.text.x = element_text(color = rep(c('red','grey30','red'),8), size = rep(c(9,11,9),8)),
        axis.ticks.x = element_line(color = rep(c('red','grey30','red'),8), size = rep(c(0.2,0.5,0.2),8)),
        panel.grid.minor = element_blank(), panel.grid.major.x = element_line(color = rep(c(NA,'white',NA),8)))

pdf(file.path( save_dir, 'cygnss_bias_plot_allweeks_st_scale.pdf' ), width = 8, height = 5)
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

parameter_frame_sat_factor_bias_starboard$sat <- ( (1:8)-0.2 )
parameter_frame_sat_factor_bias_port$sat <- ( (1:8)+0.2 )

parameter_frame_sat_factor_bias <- rbind(parameter_frame_sat_factor_bias_starboard, parameter_frame_sat_factor_bias_port)
parameter_frame_sat_factor_bias$group <- c(rep(1,8),rep(2,8))
parameter_frame_sat_factor_bias$groupnew <- rep(seq(1,8),2)

ci_plot = 
  ggplot(parameter_frame_sat_factor_bias) +
  geom_errorbar(aes(x=sat, ymin=mean-ic, ymax=mean+ic), width=0.4, colour="black") +
  geom_point(aes(x=sat, y=mean), size = 3, shape = 1, color = 'red') + 
  geom_line(aes(x=sat, y=mean, group=groupnew)) +
  labs(title="Model-Based CYGNSS Satellite Bias Estimates (99% CI)", x="CYGNSS Satellite Number", y = "Wind Speed (m/s)")+
  geom_hline(yintercept=0, linetype="dashed", color = "black", size = 1.25) +
  scale_x_continuous(breaks = c(0.8,1,1.2,1.8,2,2.2,2.8,3,3.2,3.8,4,4.2,4.8,5,5.2,5.8,6,6.2,6.8,7,7.2,7.8,8,8.2), 
                     labels = c("S","1","P","S","2","P","S","3","P","S","4","P","S","5","P","S","6","P","S","7","P","S","8","P")) +
  theme(axis.text.x = element_text(color = rep(c('red','grey30','red'),8), size = rep(c(9,11,9),8)),
        axis.ticks.x = element_line(color = rep(c('red','grey30','red'),8), size = rep(c(0.2,0.5,0.2),8)),
        panel.grid.minor = element_blank(), panel.grid.major.x = element_line(color = rep(c(NA,'white',NA),8)))

pdf(file.path( save_dir, 'cygnss_bias_plot_allweeks_ci.pdf' ), width = 8, height = 5)
ci_plot
dev.off()

########## SIGMA PLOT #########

##### SIGMA PLOT ####  
parameter_frame_sigma <- parameter_frame[,c('sat','c0','c4')]
names(parameter_frame_sigma) <- c("sat", "c0", "c4")
parameter_frame_sigma$sigma <- sqrt(2)*sqrt( parameter_frame_sigma$c0*parameter_frame_sigma$c4 )

parameter_frame_sigma_new = parameter_frame_sigma[order(parameter_frame_sigma$sigma),]
for (sat_num in 1:8){
  old = parameter_frame_sigma_new$sat[which(parameter_frame_sigma_new$sat == sat_num)]
  parameter_frame_sigma_new$sat[which(parameter_frame_sigma_new$sat == sat_num)] = old + seq(-0.25,0.25,length.out = length(old))
}

sigma_plot = 
  ggplot(parameter_frame_sigma_new) +
  geom_point(aes(x=sat, y=sigma), size = 2, shape = 1, color = 'red') + 
  labs(title=expression(paste(sqrt(2),sigma," Estimates")), x="CYGNSS Satellite Number", y = "Wind Speed (m/s)")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 8))+
  theme(axis.text.x = element_text(size = rep(11,8)))

cairo_pdf(file.path( save_dir, 'cygnss_sigma_plot_allweeks.pdf' ), width = 6, height = 3.75)
sigma_plot
dev.off()

########## a3-a2 PLOT #########

##### a3-a2 PLOT ####  
parameter_frame_a3a2 <- parameter_frame[,c('sat','b2','b3')]
names(parameter_frame_a3a2) <- c("sat", "a2", "a3")
parameter_frame_a3a2$dif <- parameter_frame_a3a2$a3 - parameter_frame_a3a2$a2

parameter_frame_a3a2_new = parameter_frame_a3a2[order(parameter_frame_a3a2$dif),]
for (sat_num in 1:8){
  old = parameter_frame_a3a2_new$sat[which(parameter_frame_a3a2_new$sat == sat_num)]
  parameter_frame_a3a2_new$sat[which(parameter_frame_a3a2_new$sat == sat_num)] = old + seq(-0.25,0.25,length.out = length(old))
}

a3a2_dif_plot = 
  ggplot(parameter_frame_a3a2_new) +
  geom_point(aes(x=sat, y=dif), size = 2, shape = 1, color = 'red') + 
  labs(title="a3-a2 Estimates", x="CYGNSS Satellite Number", y = "Wind Speed (m/s)")+
  geom_hline(yintercept=0, linetype="dashed", color = "black", size = 1.25)+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 8))+
  theme(axis.text.x = element_text(size = rep(11,8)))

pdf(file.path( save_dir, 'cygnss_a3a2_dif_plot_allweeks.pdf' ), width = 6, height = 3.75)
a3a2_dif_plot
dev.off()

########## BIAS AS FUNCTION OF MEAN PLOT #########

##### BIAS AS FUNCTION OF MEAN PLOT ####  
parameter_frame_a3a2 <- parameter_frame[,c('sat','b2','b3')]
names(parameter_frame_a3a2) <- c("sat", "a2", "a3")
parameter_frame_a3a2$dif <- parameter_frame_a3a2$a3 - parameter_frame_a3a2$a2

parameter_frame_a3a2_new = parameter_frame_a3a2[order(parameter_frame_a3a2$dif),]
for (sat_num in 1:8){
  old = parameter_frame_a3a2_new$sat[which(parameter_frame_a3a2_new$sat == sat_num)]
  parameter_frame_a3a2_new$sat[which(parameter_frame_a3a2_new$sat == sat_num)] = old + seq(-0.25,0.25,length.out = length(old))
}

a3a2_dif_plot = 
  ggplot(parameter_frame_a3a2_new) +
  geom_point(aes(x=sat, y=dif), size = 2, shape = 1, color = 'red') + 
  labs(title="a3-a2 Estimates", x="CYGNSS Satellite Number", y = "Wind Speed (m/s)")+
  geom_hline(yintercept=0, linetype="dashed", color = "black", size = 1.25)+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 8))+
  theme(axis.text.x = element_text(size = rep(11,8)))

pdf(file.path( save_dir, 'cygnss_a3a2_dif_plot_allweeks.pdf' ), width = 6, height = 3.75)
a3a2_dif_plot
dev.off()


#par(mfrow=c(1,1))
#plot(parameter_frame$b0, parameter_frame$b2, col = 'red')
#points(parameter_frame$b0, parameter_frame$b3, col = 'blue')

#par(mfrow=c(2,1))
plot(parameter_frame$b0, parameter_frame$b2, col = parameter_frame$sat)
#for (satnum in 1:8){ lines(parameter_frame$b0[parameter_frame$sat==satnum], parameter_frame$b2[parameter_frame$sat==satnum], col = satnum) }
for (satnum in 1:8){ abline(lm(parameter_frame$b2[parameter_frame$sat==satnum]~parameter_frame$b0[parameter_frame$sat==satnum]), col = satnum) }

#par(mfrow=c(2,1))
plot(parameter_frame$b0, parameter_frame$b3, col = parameter_frame$sat)
#for (satnum in 1:8){ lines(parameter_frame$b0[parameter_frame$sat==satnum], parameter_frame$b2[parameter_frame$sat==satnum], col = satnum) }
for (satnum in 1:8){ abline(lm(parameter_frame$b3[parameter_frame$sat==satnum]~parameter_frame$b0[parameter_frame$sat==satnum]), col = satnum) }
