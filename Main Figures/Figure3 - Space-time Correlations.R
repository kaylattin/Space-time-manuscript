############################################################################
############################################################################
###                                                                      ###
###            R CODE: PLOTTING SPACE-TIME SLOPE CORRELATIONS            ###
###                                                                      ###
############################################################################
############################################################################

# Load in libraries
library(tidyverse)
library(ggpubr)
library(rstan)
library(ggrepel)
library(gridExtra)
library(rethinking)
library(shinystan)

##################################################################
##                       SPECIES RICHNESS                       ##
##################################################################

##----------------
##  Forest Birds  
##----------------

load("Output_FINAL_REVISED_RF.RData") # Load in dataset

# Extract model parameters for ggplot
b_space <- summary(stanfit, pars = "b_space") # spatial slopes
b_time <- summary(stanfit, pars = "b_time") # temporal slopes
avg_space <- summary(stanfit, pars = "avg_b_space")
avg_time <- summary(stanfit, pars = "avg_b_time")

# Extract the slopes into a response and predictor variable (space = x, time = y)
x1 <- b_space$summary[,1]
y1 <- b_time$summary[,1]
avS1 <- avg_space$summary[,1]
avT1 <- avg_time$summary[,1]

### GGPLOT FIGURE ------------------------------------------------

r1 <- data.frame(x1, y1) # array of x and y values

## ggplot object
p1 <- ggplot(r1, mapping = aes(x1, y1)) +
  geom_point(aes(x1, y1, color = "Forest species"),
             size = 2,
             pch = 16,
             alpha = 0.8,
  ) +
  scale_colour_manual(values="#253494") +
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

p1

##----------------------
##  Open-Habitat Birds  
##----------------------

load("Output_FINAL_REVISED_RO.RData") # Load in dataset

# Extract model parameters for ggplot
b_space <- summary(stanfit, pars = "b_space") # spatial slopes
b_time <- summary(stanfit, pars = "b_time") # temporal slopes
avg_space <- summary(stanfit, pars = "avg_b_space")
avg_time <- summary(stanfit, pars = "avg_b_time")

# Extract the slopes into a response and predictor variable (space = x, time = y)
x2 <- b_space$summary[,1]
y2 <- b_time$summary[,1]
avS2 <- avg_space$summary[,1]
avT2 <- avg_time$summary[,1]

### GGPLOT FIGURE ------------------------------------------------

r2 <- data.frame(x2, y2)

## ggplot object
p2 <- p1 +   geom_point(data = r2,
                        aes(x2, y2, color = "Open-habitat species"),
                        size = 2,
                        fill = "white",
                        pch = 21,
                        stroke = 1,
                        alpha = 0.8
) +
  scale_colour_manual(name = "", values=c("#253494", "#41b6c4"),
                      guide = guide_legend(override.aes = list(shape = c(16 ,21),
                                                               fill = c(NA, "white"),
                                                               stroke = c(NA, 2)))) +
  theme_classic()
p2

p2 <- p2 + geom_point(aes(x=avS1, y=avT1), pch = 10, size = 4, stroke = 1, colour="black", alpha = 0.7) +
  geom_segment(aes(x=avS1, y=avT1, xend = -Inf, yend = avT1), linetype = "dashed") +
  geom_segment(aes(x=avS1, y=avT1, xend = avS1, yend = -Inf), linetype = "dashed") + 
  geom_point(aes(x=avS2, y=avT2), pch = 10, size = 4, stroke = 1, colour="black", alpha = 0.7) +
  geom_segment(aes(x=avS2, y=avT2, xend = -Inf, yend = avT2), linetype = "dashed") +
  geom_segment(aes(x=avS2, y=avT2, xend = avS2, yend = -Inf), linetype = "dashed") + 
  theme(plot.margin = unit(c(0,0,0,0),"cm"),
        plot.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size=9),
        axis.title = element_text(size = 10), axis.title.x = element_text(vjust= -2 ), axis.title.y = element_text(vjust = 5), 
        axis.text = element_text(size = 9)) + labs(title = "Species richness") +
  theme(legend.position = "bottom")
p2


##----------------------------------------------------------------------------

#################################################################
##                       TOTAL ABUNDANCE                       ##
#################################################################

##----------------
##  Forest Birds  
##----------------

load("Output_FINAL_REVISED_TF.RData")

# Extract parameters for ggplot
b_space <- summary(stanfit, pars = "b_space") # spatial slopes
b_time <- summary(stanfit, pars = "b_time") # temporal slopes
avg_space <- summary(stanfit, pars = "avg_b_space")
avg_time <- summary(stanfit, pars = "avg_b_time")

# Extract the slopes into a response and predictor variable (space = x, time = y)
x3 <- b_space$summary[,1]
y3 <- b_time$summary[,1]
avS3 <- avg_space$summary[,1]
avT3 <- avg_time$summary[,1]

### GGPLOT FIGURE ------------------------------------------------

r3 <- data.frame(x3, y3)

## ggplot object
p3 <- ggplot(r3, mapping = aes(x3, y3)) +
  geom_point(aes(x3, y3, color = "Forest species"),
             size = 2,
             alpha = 0.8,
  ) +
  scale_colour_manual(values="#31a354") +
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
p3 



##----------------------
##  Open-Habitat Birds  
##----------------------

load("Output_FINAL_REVISED_TO.RData")

# Extract parameters for ggplot
b_space <- summary(stanfit, pars = "b_space") # spatial slopes
b_time <- summary(stanfit, pars = "b_time") # temporal slopes
avg_space <- summary(stanfit, pars = "avg_b_space")
avg_time <- summary(stanfit, pars = "avg_b_time")

# Extract the slopes into a response and predictor variable (space = x, time = y)
x4 <- b_space$summary[,1]
y4 <- b_time$summary[,1]
avS4 <- avg_space$summary[,1]
avT4 <- avg_time$summary[,1]

#### GGPLOT ####
r4 <- data.frame(x4, y4)

## GGplot object
p4 <- p3 +   geom_point(data = r4,
                        aes(x4, y4, color = "Open-habitat species"),
                        size = 2,
                        pch = 21,
                        fill = "white",
                        stroke = 1,
                        alpha = 0.8,
) +
  scale_colour_manual(name = "", values=c("#31a354", "#addd8e"),
                      guide = guide_legend(override.aes = list(shape = c(16 ,21),
                                                               fill = c(NA, "white"),
                                                               stroke = c(NA, 2)))) +
  theme_classic()
p4

p4 <- p4 + geom_point(aes(x=avS3, y=avT3), pch = 10, size = 4, stroke = 1, colour="black", alpha = 0.7) +
  geom_segment(aes(x=avS3, y=avT3, xend = -Inf, yend = avT3), linetype = "dashed") +
  geom_segment(aes(x=avS3, y=avT3, xend = avS3, yend = -Inf), linetype = "dashed") + 
  geom_point(aes(x=avS4, y=avT4), pch = 10, size = 4, stroke = 1, colour="black", alpha = 0.7) +
  geom_segment(aes(x=avS4, y=avT4, xend = -Inf, yend = avT4), linetype = "dashed") +
  geom_segment(aes(x=avS4, y=avT4, xend = avS4, yend = -Inf), linetype = "dashed") +  
  theme(plot.margin = unit(c(0,0,0,0),"cm"),
        plot.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size=9),
        axis.title = element_text(size = 10), axis.title.x = element_text(vjust= -2 ), axis.title.y = element_text(vjust = 5), 
        axis.text = element_text(size = 9)) + labs(title = "Total abundance") +
  theme(legend.position = "bottom")
p4

p2 <- p2 + labs(tag = "A") + theme(plot.tag = element_text(face = "bold", size = 28)) + theme(plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))
p4 <- p4 + labs(tag = "B") + theme(plot.tag = element_text(face = "bold", size = 28)) + theme(plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))
p2
p4

plot <- ggarrange(p2, p4, ncol = 2, nrow = 1)
plot

# Save Total Abundance Plot
ggsave(filename = "FIGURE 3_FINAL.tiff", device = "tiff", plot = plot,
       width = 5200, height = 3000, units = "px", dpi = 600, compression = "lzw")  

### END OF CODE ---------------------------------------------------------------


