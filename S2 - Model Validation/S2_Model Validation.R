## load up stuff
library(tidyverse)
library(cmdstanr)
library(shinystan)
library(rstan)
library(bayesplot)
library(rethinking)
library(pdftools)
library(rstantools)
library(posterior)

n <- 30

# Set intercept and slope values (time = space)
a <- 1
b_space <- 1
b_time <- 1

# Generate data
d <- vector("list")
a1 <- vector("list")
b1 <- vector("list")
a2 <- vector("list")
b2 <- vector("list")

### Do linear regressions independently for each region to get an idea of what the relationships are
## The graphs plotted visually show predicted space and time slopes that are very similar to each other
## Yet the space-time slope correlation across the 30 comparisons is weak
## From experience, the space-time correlation from this exercise aligns closely with the Rstan model below
## Is it reasonable to expect a 'perfect' 1:1 relationship between space and time slopes in order for space-time substitution to be valid?
## Is that too strict?
par(mfrow=c(4,2),
    omi=c(0.3, 0.3, 0.3, 0.3))

for(i in 1:30){

  fcover1 <- runif(n=n, min=0, max=1)
  elev <- runif(n=n, min=135, max=212)
  space.time <- rep(2, length.out=n)
  x_space <- data.frame(cbind(fcover1, elev, space.time))
  
  fcover2 <- runif(n=n, min=0, max=1)
  elev <- runif(n=n, min=135, max=212)
  space.time <- rep(1, length.out=n)
  x_time <- data.frame(cbind(fcover2, elev, space.time))
  
  mu_space <- exp(a + b_space * x_space$fcover)
  mu_time <- exp(a + b_time * x_time$fcover)
  
  #mu <- exp(a + b_space * x$x1 * x$s1 + b_time *x$x1 * x*t1)
  
  RF_space <- rpois(n=n, lambda=mu_space)
  RF_time <- rpois(n=n, lambda=mu_time)
  
  RF <- append(RF_space, RF_time)
  
  fcover <- rbind(x_space, x_time)
  fcover$ref <- i
  
  d[[i]] <- data.frame(cbind(RF, fcover))
  
  m1 <- glm(RF_space ~ fcover1, family = poisson())
  m2 <- glm(RF_time ~ fcover2, family = poisson())
  
  a1[[i]] <- exp(m1$coefficients[[1]])
  b1[[i]] <- exp(m1$coefficients[[2]])
  
  a2[[i]] <- exp(m2$coefficients[[1]])
  b2[[i]] <- exp(m2$coefficients[[2]])
  
  plot(x = d[[i]]$fcover, y = d[[i]]$RF, col = d[[i]]$space.time, pch = 21, xlab = "forest cover", ylab = "richness")
  abline(a = a1[[i]], b = b1[[i]], col = "black")
  abline(a = a2[[i]], b = b2[[i]], col = "red")
  
  }
par(mfrow=c(1,1))

b1 <- log(do.call("rbind", b1))
b2 <- log(do.call("rbind", b2))
d <- do.call("rbind", d)

plot(b2, b1)
abline(a = 0, b = 1, lty = 2)

# Highlight all of the above and keep re-running - overall, we never see a 
# perfect 1:1 relationship, despite the comparison plots showing slopes that are 
# VERY similar to each other

# true slopes = 1, and yet there is lots of variation around that in the predicted
# slopes. Slopes can range from 0.2 - 2. AND, lots of variation between instances of random data generation
# Ofc, this isn't with MY rstan model, but just a simple glm


### TRYING IN RSTAN MODEL
# Some data prep
d$Region <- as.integer(as.factor(d$ref)) # Create integer categorical for 'region' or space-time comparison

d$space <- d$space.time
d$space[which(d$space == 1)] <- 0
d$space[which(d$space == 2)] <- 1

d$time <- d$space.time
d$time[which(d$time == 2)] <- 0
d$time[which(d$time == 1)] <- 1

# Establish input dataset
d_slim <- list(
  ncounts = nrow(d),
  nreg = length(unique(d$Region)),
  nst = 2,
  
  richness = d$RF,
  spacetime = d$space.time,
  space = d$space,
  time = d$time,
  reg = d$Region,

  pforest = d$fcover
)

# Compile the model in cmdstan  ~~~~~~~~~~~~~~~~~~~
# file <- file.path ("AbundanceRegression.stan") # turn on if running abundance model
file <- file.path("~/space-for-time/Rstan models/RichnessRegressionPoisson_Simple.stan") # turn on if running richness model
mod <- cmdstan_model(file, pedantic = TRUE)
check_cmdstan_toolchain(fix = TRUE)


# Run the model ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
  output_dir = "~/space-time/cmdstan_output_files/"
)

# create a stanfit S4 object 
stanfit <- rstan::read_stan_csv(fit$output_files())
save(stanfit, file =  "Output_ModelValidation_Equal.RData")


load("Output_ModelValidation_Equal.RData")

# Extract parameters for ggplot
b_space <- summary(stanfit, pars = "b_space") # spatial slopes
b_time <- summary(stanfit, pars = "b_time") # temporal slopes
a <- summary(stanfit, pars = "a") # intercept

# Extract the slopes into a response and predictor variable (space = x, time = y)
x <- b_space$summary[,1]
y <- b_time$summary[,1]

bs <- rstan::extract(stanfit, "b_space") # extract spatial slopes
bt <- rstan::extract(stanfit, "b_time") # extract temporal slopes

b_space <- bs$b_space # assign to a referenceable object
b_time <- bt$b_time

### R PLOTTING (based on Statistical Rethinking by Richard McElreath) ### 
# Set up fake data
pred_data = seq(from = (min(x) - 0.05), to = (max(x) + 0.05), length.out = 200 )

# Initialize
niterations = 8000
N = 200
intercept <- vector("list")
slope <- vector("list")
pred_lines <- vector("list")
mu <- matrix(nrow=8000, ncol=200)

# The code below cycles through every MCMC iteration (total of 8000) and calculates correlation of b_space (across 33 regions) and b_time (across 33 regions)
# Then, it predicts plottable relationship using the intercept & slope for each iteration
for (i in 1:niterations){
  
  mlm = lm(b_time[i,] ~ b_space[i,]) # linear regression between time and space
  
  intercept[i] = mlm$coefficients[[1]] # extract intercept
  slope[i] = mlm$coefficients[[2]] # extract slope between space-time
  
  mu[i,] <- sapply(pred_data,  function(x) mean( unlist(intercept[i]) + unlist(slope[i]) * x ) ) # derive plottable values for mean relationship between time and space
  
}

mu.mean <- apply( mu, 2, mean ) # mean of distribution of y for each value of x
ci.mean <- apply( mu, 2, PI, prob = 0.95 ) # calculate confidence interval

# Plot in R
plot(x, y)
lines( pred_data, mu.mean, col = "#2c7bb6", lwd = 1.5)
abline(a = 0, b = 1 ,lty = 2)
shade( ci.mean, pred_data)


## Slope differences
b_dif_rg <- as.matrix(stanfit, pars = "b_dif_rg")

col <- rep("#253494", 6)
#253494 - richness
#31a354 - abund

### main model - by region
color_scheme_set(col)
bayesplot_theme_set(theme_classic())
p1 <- mcmc_intervals(b_dif_rg)

p1 <- p1 + vline_0(size = 0.25, color = "darkgray", linetype = 2) + yaxis_text(FALSE) +
  labs(
    x = "Slope difference", 
    y = "Comparison",
    size = 4
  ) +
  theme(plot.margin = unit(c(1,1,1,1),"cm"),
        plot.title = element_text(size = 20, face = "bold"),
        axis.title = element_text(size = 14), axis.title.x = element_text(vjust= -2 ), axis.title.y = element_text(vjust = 5), 
        axis.text = element_text(size = 12)) + labs(title = "")
p1

### Regressions per comparison
d$Region <- as.integer(as.factor(d$ref))
draws <- rstan::extract(stanfit, pars = c("a", "b_space", "b_time", "retrans_noise"))
y_space <- as.data.frame(draws$y_space)

pdf("~/space-for-time/Revised Submission_Ecological Applications/Plots_ModelValidation_Equal.pdf", height = 10)

par(mfrow=c(4,2),
    omi=c(0.3, 0.3, 0.3, 0.3))

for(i in 1:30){
  d_filter <- d %>% filter(Region == i)
  d_filter$Colour= "#2c7bb6"
  d_filter$Shape = 16
  # Set new column values to appropriate colours
  d_filter$Colour[d_filter$space.time == 2] = "#ED432D"
  d_filter$Shape[d_filter$space.time == 2] = 17
  
  d_space <- d_filter %>% filter(space.time == 2)
  d_time <- d_filter %>% filter(space.time == 1)
  
  
  x.seq <- seq( from=min(d$fcover), to = max(d$fcover), length.out = 30 )
  
  x.scaled <- (x.seq-mean(d$fcover))/sd(d$fcover)
  mu.space <- sapply(x.scaled,  function(x) mean(exp(draws$a[, i, 2] + draws$b_space[, i] * x + 
                                                       draws$retrans_noise) ))
  mu.time <- sapply(x.scaled,  function(x) mean(exp(draws$a[, i, 1] + draws$b_time[, i] * x + 
                                                      draws$retrans_noise)) )
  
  ci.space <- sapply(x.scaled,  function(x) PI(exp(draws$a[, i, 2] + draws$b_space[, i] * x + 
                                                     draws$retrans_noise)) )
  ci.time <- sapply(x.scaled,  function(x) PI(exp(draws$a[, i, 1] + draws$b_time[, i] * x + 
                                                    draws$retrans_noise )) )
  
  plot(d_filter$fcover, d_filter$RF, 
       col = alpha(d_filter$Colour, 0.6), 
       pch = d_filter$Shape,
       yaxt = "n", cex.lab = 1.25, 
       cex.main = 1.5, main = paste("Comparison", i), xlab = "Percent forest cover", ylab = "Species richness")
  
  
  axis(2, cex.axis = 1.25)
  
  shade( ci.space, x.scaled, col = alpha("#ED432D", 0.05))
  shade( ci.time, x.scaled, col = alpha("#2c7bb6", 0.05))
  
  lines( x.scaled, mu.space, col = "#ED432D", lwd = 1.5)
  lines( x.scaled, mu.time, col = "#2c7bb6", lwd = 1.5 )
  
}
dev.off()
pdf_convert("~/space-for-time/Revised Submission_Ecological Applications/Plots_ModelValidation_Equal.pdf",  pages = 1:4)

