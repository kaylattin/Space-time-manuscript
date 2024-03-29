# Load in libraries
library(tidyverse)
library(ggpubr)
library(rstan)
library(ggrepel)
library(gridExtra)
library(rethinking)
library(shinystan)


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ## RICHNESS ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
load("Output_FINAL_REVISED_RF_200km.RData")

# Extract parameters for ggplot
b_space <- summary(stanfit, pars = "b_space") # spatial slopes
b_time <- summary(stanfit, pars = "b_time") # temporal slopes
avg_space <- summary(stanfit, pars = "avg_b_space")
avg_time <- summary(stanfit, pars = "avg_b_time")

# Extract the slopes into a response and predictor variable (space = x, time = y)
x <- b_space$summary[,1]
y <- b_time$summary[,1]
avS1 <- avg_space$summary[,1]
avT1 <- avg_time$summary[,1]

#### GGPLOT ####
r <- data.frame(x, y)

## GGplot object
p1 <- ggplot(r, mapping = aes(x, y)) +
  geom_point(aes(x, y, color = "Forest species"),
             size = 3,
             alpha = 0.8,
  ) +
  scale_colour_manual(values="#DC267F") +
  scale_y_continuous(limits = c(-3.5, 3.5), breaks = seq(from = -3.5, to = 3.5, by = 0.5)) +
  scale_x_continuous(limits = c(-2, 2), breaks = seq(-2, 2, by = 0.5)) +
  geom_hline(yintercept=0) +
  geom_vline(xintercept = 0) +
  labs(
    x = "Spatial slope", 
    y = "Temporal slope",
    size = 4
  ) +
  theme_classic()

p1 <- p1 + geom_point(aes(x=avS1, y=avT1), pch = 10, size = 7, stroke = 1, colour="black", alpha = 0.7) +
  geom_segment(aes(x=avS1, y=avT1, xend = -Inf, yend = avT1), linetype = "dashed") +
  geom_segment(aes(x=avS1, y=avT1, xend = avS1, yend = -Inf), linetype = "dashed")
p1


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ## RICHNESS - OPEN HABITATS BIRDS ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
load("Output_FINAL_REVISED_RO_200km.RData")

# Extract parameters for ggplot
b_space <- summary(stanfit, pars = "b_space") # spatial slopes
b_time <- summary(stanfit, pars = "b_time") # temporal slopes
avg_space <- summary(stanfit, pars = "avg_b_space")
avg_time <- summary(stanfit, pars = "avg_b_time")

# Extract the slopes into a response and predictor variable (space = x, time = y)
x <- b_space$summary[,1]
y <- b_time$summary[,1]
avS2 <- avg_space$summary[,1]
avT2 <- avg_time$summary[,1]

#### GGPLOT ####
r <- data.frame(x, y)

## GGplot object
p2 <- p1 +   geom_point(data = r,
                        aes(x, y, color = "Open-habitat species"),
                        size = 3,
                        pch = 21,
                        fill = "white",
                        stroke = 2,
                        alpha = 0.8,
) +
  scale_colour_manual(values=c("#DC267F", "#f792b0")) +
  theme_classic()

p2 <- p2 + geom_point(aes(x=avS1, y=avT1), pch = 10, size = 7, stroke = 1, colour="black", alpha = 0.7) +
  geom_segment(aes(x=avS1, y=avT1, xend = -Inf, yend = avT1), linetype = "dashed") +
  geom_segment(aes(x=avS1, y=avT1, xend = avS1, yend = -Inf), linetype = "dashed") + 
  geom_point(aes(x=avS2, y=avT2), pch = 10, size = 7, stroke = 1, colour="black", alpha = 0.7) +
  geom_segment(aes(x=avS2, y=avT2, xend = -Inf, yend = avT2), linetype = "dashed") +
  geom_segment(aes(x=avS2, y=avT2, xend = avS2, yend = -Inf), linetype = "dashed") + 
  theme(plot.margin = unit(c(1,1,1,1),"cm"),
        plot.title = element_text(size = 24, face = "bold"),
        legend.text = element_text(size=18),
        axis.title = element_text(size = 20), axis.title.x = element_text(vjust= -2 ), axis.title.y = element_text(vjust = 5), 
        axis.text = element_text(size = 18)) + labs(title = "Species richness") + theme(legend.position = "bottom")
p2

# Save Richness plot
ggsave(filename = "~/Space-time-manuscript/ALL_SpaceTimeSlopes_RICHNESS_200km.png", device = "png", plot = p2,
       width = 30, height = 30, units = "cm")



## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ## ABUNDANCE - FOREST ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
load("Output_FINAL_REVISED_TF_200km.RData")

# Extract parameters for ggplot
b_space <- summary(stanfit, pars = "b_space") # spatial slopes
b_time <- summary(stanfit, pars = "b_time") # temporal slopes
avg_space <- summary(stanfit, pars = "avg_b_space")
avg_time <- summary(stanfit, pars = "avg_b_time")

# Extract the slopes into a response and predictor variable (space = x, time = y)
x <- b_space$summary[,1]
y <- b_time$summary[,1]
avS1 <- avg_space$summary[,1]
avT1 <- avg_time$summary[,1]

#### GGPLOT ####
r <- data.frame(x, y)

## GGplot object
p1 <- ggplot(r, mapping = aes(x, y)) +
  geom_point(aes(x, y, color = "Forest species"),
             size = 3,
             alpha = 0.8,
  ) +
  scale_colour_manual(values="#FE6100") +
  scale_y_continuous(limits = c(-3.5, 3.5), breaks = seq(from = -3.5, to = 3.5, by = 0.5)) +
  scale_x_continuous(limits = c(-2.5, 2.5), breaks = seq(-2.5, 2.5, by = 0.5)) +
  geom_hline(yintercept=0) +
  geom_vline(xintercept = 0) +
  labs(
    x = "Spatial slope", 
    y = "Temporal slope",
    size = 4
  ) +
  theme_classic()

p1 <- p1 + geom_point(aes(x=avS1, y=avT1), pch = 10, size = 7, stroke = 1, colour="black", alpha = 0.7) +
  geom_segment(aes(x=avS1, y=avT1, xend = -Inf, yend = avT1), linetype = "dashed") +
  geom_segment(aes(x=avS1, y=avT1, xend = avS1, yend = -Inf), linetype = "dashed")
p1


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ## ABUNDANCE - OPEN-HABITAT ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
load("Output_FINAL_REVISED_TO_200km.RData")

# Extract parameters for ggplot
b_space <- summary(stanfit, pars = "b_space") # spatial slopes
b_time <- summary(stanfit, pars = "b_time") # temporal slopes
avg_space <- summary(stanfit, pars = "avg_b_space")
avg_time <- summary(stanfit, pars = "avg_b_time")

# Extract the slopes into a response and predictor variable (space = x, time = y)
x <- b_space$summary[,1]
y <- b_time$summary[,1]
avS2 <- avg_space$summary[,1]
avT2 <- avg_time$summary[,1]

#### GGPLOT ####
r <- data.frame(x, y)

## GGplot object
p2 <- p1 +   geom_point(data = r,
                        aes(x, y, color = "Open-habitat species"),
                        size = 3,
                        pch = 21,
                        fill = "white",
                        stroke = 2,
                        alpha = 0.8,
) +
  scale_colour_manual(values=c("#FE6100", "#f7b456")) +
  theme_classic()

p2 <- p2 + geom_point(aes(x=avS1, y=avT1), pch = 10, size = 7, stroke = 1, colour="black", alpha = 0.7) +
  geom_segment(aes(x=avS1, y=avT1, xend = -Inf, yend = avT1), linetype = "dashed") +
  geom_segment(aes(x=avS1, y=avT1, xend = avS1, yend = -Inf), linetype = "dashed") + 
  geom_point(aes(x=avS2, y=avT2), pch = 10, size = 7, stroke = 1, colour="black", alpha = 0.7) +
  geom_segment(aes(x=avS2, y=avT2, xend = -Inf, yend = avT2), linetype = "dashed") +
  geom_segment(aes(x=avS2, y=avT2, xend = avS2, yend = -Inf), linetype = "dashed") +  
  theme(plot.margin = unit(c(1,1,1,1),"cm"),
        plot.title = element_text(size = 24, face = "bold"),
        legend.text = element_text(size=18),
        axis.title = element_text(size = 20), axis.title.x = element_text(vjust= -2 ), axis.title.y = element_text(vjust = 5), 
        axis.text = element_text(size = 18)) + labs(title = "Total abundance") + theme(legend.position = "bottom")
p2

# Save abundance plot
  ggsave(filename = "~/Space-time-manuscript/ALL_SpaceTimeSlopes_ABUNDANCE_200km.png", device = "png", plot = p2,
       width = 30, height = 30, units = "cm")                       
