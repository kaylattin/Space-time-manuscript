############################################################################
############################################################################
###                                                                      ###
###             R CODE: RUNNING DATASETS THROUGH RSTAN MODEL             ###
###                                                                      ###
############################################################################
############################################################################

# Load in libraries
library(tidyverse)
library(cmdstanr)
library(shinystan)
library(rstan)
library(bayesplot)
library(rethinking)

setwd("~/Space-time-manuscript") # Set working directory
rm(list = ls()) # Clean environment
gc() # Clean garbage

d <- read.csv("~/Space-time-manuscript/FINAL_REVISED_DATASET_TO_200km.csv") # Load in dataset

##----------------------------------------------------------------
##                        DATA PREPARATION                       -
##----------------------------------------------------------------

##-------------------------
##  Initial data plotting  
##-------------------------

# Species richness
hist(d$Richness)
min(d$Richness)
max(d$Richness)
mean(d$Richness)
median(d$Richness)

# Total abundance
hist(d$TA)
min(d$TA)
max(d$TA)
mean(d$TA)
median(d$TA)

##----------------------------
##  Preparing data for input  
##----------------------------

## Create space and time indicator variables
d$space <- d$space.time
d$space[which(d$space == 1)] <- 0
d$space[which(d$space == 2)] <- 1

d$time <- d$space.time
d$time[which(d$time == 1)] <- 1
d$time[which(d$time == 2)] <- 0

d$ObsN <- as.integer(as.factor(d$ObsN)) # Create integer categorical for unique observer
d$Region <- as.integer(as.factor(d$ref)) # Create integer categorical for each space-time comparison

x.seq = seq( from=min(d$Forest.cover), to = max(d$Forest.cover), length.out = 100 ) # Set up fake data for simulations
xrep = seq(from = 0, to = 1, length.out = nrow(d))

##----------------------------
##  Create data input array
##----------------------------

d_slim <- list(
  ncounts = nrow(d),
  nreg = length(unique(d$Region)),
  nobs = length(unique(d$ObsN)),
  nst = 2,
  ndata = 100,
  
  ta = d$TA_ALL, # turn on if running abundance model
  #richness = d$Richness_ALL, # turn on if running richness model
  spacetime = d$space.time,
  space = d$space,
  time = d$time,
  reg = d$Region,

  pforest = d$Forest.cover,
  obs = as.integer(as.factor(d$ObsN)),
  nfirstobs = 2,
  firstobs = d$FirstObs,
  xseq = (x.seq-mean(d$Forest.cover))/sd(d$Forest.cover),
  xrep = xrep
)


##---------------------------------------------------------------
##                          RUN MODEL                           -
##---------------------------------------------------------------

##----------------------------
##  Compile model in cmdstan  
##----------------------------

file <- file.path("~/Space-time-manuscript/Main models/AbundanceRegressionPoisson_FINAL.stan") # turn on if running abundance model
#file <- file.path("~/Space-time-manuscript/Main models/RichnessRegressionPoisson_FINAL.stan") # turn on if running richness model
mod <- cmdstan_model(file, pedantic = TRUE)
check_cmdstan_toolchain(fix = TRUE)


# Run the model -------------------------------------------------
fit <- mod$sample(
  data = d_slim,
  chains = 4,
  iter_warmup = 1000,
  iter_sampling = 2000,
  parallel_chains = 2,
  show_messages = TRUE,
  adapt_delta = 0.99,
  max_treedepth = 18,
  step_size = 0.01,
  output_dir = "~/Space-time-manuscript/cmdstan_output_files/"
)

# Create a stanfit S4 object 
stanfit <- rstan::read_stan_csv(fit$output_files())
  save(stanfit, file =  "Output_FINAL_REVISED_TO_200km.RData")

##----------------------------
##  Posterior checks  
##----------------------------

# Get observed y values
y <- d$Richness_ALL

# Load up in shinystan for convergence diagnostics & posterior predictive / assumptions
shinyfit <- as.shinystan(stanfit)
launch_shinystan(shinyfit)

# Posterior predictive check using bayesplot()
y_rep <- as.data.frame(stanfit, pars = "y_rep")
ppc_dens_overlay(y = y, yrep = y_rep)

sims <- as.matrix(stanfit)
head(sims)
mcmc_areas(sims, prob = 0.9)

fits <- stanfit %>% as_tibble() %>% rename(a_space = 'a[1,1]') %>% rename(b_space = 'b[1,1]') %>% select(a_space, b_space)
head(fits)
