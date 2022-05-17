
## in form of c("YYYY-MM-DD, "YYYY-MM-DD")
## example
## args <- c("2020-01-01","2020-01-07")
args <- commandArgs(trailingOnly=TRUE) 

# process date parameters
# script assumes we have files for each date in the range
date_1 <- as.Date(args[1])
date_2 <- as.Date(args[2])
fname <- paste0("../data/for_fitting/data_",date_1,"_",date_2,".RData")
load(fname)

# subset to satellites 1-8 (CYGNSS, not Jason)
dat <- dat[ dat$sat %in% 1:8, ]

# create variables for design matrix
dat$lat_rad <- dat$lat*pi/180

# create a identifier to individual satellite-antenna combination
dat$sat_ant <- paste0( dat$sat, dat$ant )

# create y, locs, X for input to fit_model
y <- dat$wind_speed
locs <- as.matrix( dat[ , c("lon","lat","time") ] )
X <- model.matrix( ~ lat_rad + I(lat_rad^2) + I(lat_rad^3) + time + sat_ant, data = dat )

# fit the model
model <- GpGp::fit_model(
    y, locs, X,
    covfun_name = "exponential_spheretime",
    start_parms = c(6.5292, 0.5, 18.1037, 1e-04),
    m_seq = c(10,30),
    st_scale = c(0.5,5),
    silent = TRUE
)

# fit the model
model <- GpGp::fit_model(
    y, locs, X,
    covfun_name = "exponential_spheretime",
    start_parms = model$covparms,
    m_seq = c(30),
    st_scale = model$covparms[2:3],
    silent = TRUE
)

# print the summary
summary(model)

# retain colnames of X in names of beta
names( model$betahat ) <- colnames(X)
rownames( model$betacov ) <- colnames(X)
colnames( model$betacov ) <- colnames(X)

# strip model of large objects
model$y <- NULL
model$locs <- NULL
model$X <- NULL

# save the model
save(model, file = paste0("../model_fits/fit4_",args[1],"_",args[2],".RData") )

