###########################################################################
###########################################################################
###                                                                     ###
###       R CODE: SIMULATED DATA AND SPACE-TIME SLOPE DIFFERENCES       ###
###                                                                     ###
###########################################################################
###########################################################################

library(bayesplot)
library(RColorBrewer)
library(ggplot2)
library(ggpubr)

# Set up Space-time comparison labels
region <- vector()

for( i in 1:25 ) {
  
  region[i] <- paste("Space-time Comparison", i, sep = " ")
  
}

region <- rev(region)

# ##----------------------------------------------------------------
# ##                    Simulated Y as X Changes                   -
# ##----------------------------------------------------------------
# 
# xrep = seq(from = 0, to = 1, length.out = nrow(d))
# 
# load("Output_FINAL_REVISED_RF.RData")
# 
# yrep <- summary(stanfit, pars = "sim_space")
# y <- yrep$summary[,1]
# l <- rep("S_RF" , 898)
# y_space_RF <- data.frame(xrep, y, l)
# 
# yrep <- summary(stanfit, pars = "sim_time")
# y <- yrep$summary[,1]
# l <- rep("T_RF" , 898)
# y_time_RF <- data.frame(xrep, y, l)
# 
# load("Output_FINAL_REVISED_RO.RData")
# yrep <- summary(stanfit, pars = "sim_space")
# y <- yrep$summary[,1]
# l <- rep("S_RO" , 897)
# y_space_RO <- data.frame(xrep, y, l)
# 
# yrep <- summary(stanfit, pars = "sim_time")
# y <- yrep$summary[,1]
# l <- rep("T_RO" , 897)
# y_time_RO <- data.frame(xrep, y, l)
# 
# load("Output_FINAL_REVISED_TF.RData")
# yrep <- summary(stanfit, pars = "sim_space")
# y <- yrep$summary[,1]
# l <- rep("S_TF" , 898)
# y_space_TF <- data.frame(xrep, y, l)
# 
# yrep <- summary(stanfit, pars = "sim_time")
# y <- yrep$summary[,1]
# l <- rep("T_TF" , 898)
# y_time_TF <- data.frame(xrep, y, l)
# 
# load("Output_FINAL_REVISED_TO.RData")
# yrep <- summary(stanfit, pars = "sim_space")
# y <- yrep$summary[,1]
# l <- rep("S_TO" , 897)
# y_space_TO <- data.frame(xrep, y, l)
# 
# yrep <- summary(stanfit, pars = "sim_time")
# y <- yrep$summary[,1]
# l <- rep("T_TO" , 897)
# y_time_TO <- data.frame(xrep, y, l)
# 
# # Create space simulated dataframes for forest and open birds
# sp_fr <- rbind(y_space_RF, y_space_TF)
# sp_op <- rbind(y_space_RO, y_space_TO)
# 
# p <- ggplot(sp_fr, mapping = aes(xrep, y)) +
#   geom_line(data = sp_fr,
#             aes(xrep, y, color = l),
#             size = 2,
#             linetype = "dashed"
#             ) + scale_colour_manual(values=c("#253494", "#31a354")) +
#   theme_classic()
#   
# p
# 
#   ## GGplot object
#   p2 <- p +   geom_line(data = sp_op,
#                           aes(xrep, y, color = l),
#                           size = 2,
#                           linetype = "dashed"
#   ) +
#   scale_colour_manual(values=c("#253494", "#41b6c4", "#31a354", "#addd8e")) +
#   theme_classic()
#   
#   p2
# 
#   
# ti_fr <- rbind(y_time_RF, y_time_TF)
# ti_op <- rbind(y_time_RO, y_time_TO)
# 
# p <- ggplot(ti_fr, mapping = aes(xrep, y)) +
#   geom_line(data = sp_fr,
#             aes(xrep, y, color = l),
#             size = 2
#   ) + scale_colour_manual(values=c("#253494", "#31a354")) +
#   theme_classic()
# 
# p
# 
# ## GGplot object
# p2 <- p +   geom_line(data = ti_op,
#                       aes(xrep, y, color = l),
#                       size = 2
# ) +
#   scale_colour_manual(values=c("#253494", "#41b6c4", "#31a354", "#addd8e")) +
#   theme_classic()
# 
# p2
  
##----------------------------------------------------------------

##---------------------------------------------------------------
##            Space-Time Slope Differnces : Summaries           -
##---------------------------------------------------------------

load("Output_FINAL_REVISED_RF.RData")
diff_RF <- as.matrix(stanfit, pars = "b_dif")
avg_RF <- mean(diff_RF)
PI_RF <- PI(diff_RF)
slopes_RF <- c(mean(as.matrix(stanfit, pars = "avg_b_time")), mean(as.matrix(stanfit, pars = "avg_b_space")))
slopes_PI_RF <- c(PI(as.matrix(stanfit, pars = "avg_b_time")), PI(as.matrix(stanfit, pars = "avg_b_space")))

load("Output_FINAL_REVISED_RO.RData")
diff_RO <- as.matrix(stanfit, pars = "b_dif")
avg_RO <- mean(diff_RO)
PI_RO <- PI(diff_RO)
slopes_RO <- c(mean(as.matrix(stanfit, pars = "avg_b_time")), mean(as.matrix(stanfit, pars = "avg_b_space")))
slopes_PI_RO <- c(PI(as.matrix(stanfit, pars = "avg_b_time")), PI(as.matrix(stanfit, pars = "avg_b_space")))

load("Output_FINAL_REVISED_TF.RData")
diff_TF <- as.matrix(stanfit, pars = "b_dif")
avg_TF <- mean(diff_TF)
PI_TF <- PI(diff_TF)
slopes_TF <- c(mean(as.matrix(stanfit, pars = "avg_b_time")), mean(as.matrix(stanfit, pars = "avg_b_space")))
slopes_PI_TF <- c(PI(as.matrix(stanfit, pars = "avg_b_time")), PI(as.matrix(stanfit, pars = "avg_b_space")))
  
load("Output_FINAL_REVISED_TO.RData")
diff_TO <- as.matrix(stanfit, pars = "b_dif")
avg_TO <- mean(diff_TO)
PI_TO <- PI(diff_TO)
slopes_TO <- c(mean(as.matrix(stanfit, pars = "avg_b_time")), mean(as.matrix(stanfit, pars = "avg_b_space")))
slopes_PI_TO <- c(PI(as.matrix(stanfit, pars = "avg_b_time")), PI(as.matrix(stanfit, pars = "avg_b_space")))

overall_diff <- data.frame(avg_RF, avg_RO, avg_TF, avg_TO)
PI_diff <- data.frame(PI_RF, PI_RO, PI_TF, PI_TO)
slopes <- rbind(slopes_RF, slopes_RO, slopes_TF, slopes_TO)
slopes_PI <- rbind(slopes_PI_RF, slopes_PI_RO, slopes_PI_TF, slopes_PI_TO)

combined <- rbind(mcmc_intervals_data(diff_RF), mcmc_intervals_data(diff_RO),
                  mcmc_intervals_data(diff_TF), mcmc_intervals_data(diff_TO))
combined$model <- rep(c("Forest Species Richness", "Open-Habitat Species Richness", "Forest Bird Total Abundance", "Open-Habitat Bird Abundance"), each = ncol(diff_RF))
combined <- transform(combined, model=factor(model, levels=unique(model)))

theme_set(bayesplot::theme_default())
pos <- position_nudge(y = ifelse(combined$model == "Open-Habitat Species Richness", 0, 
                                 ifelse(combined$model == "Forest Bird Total Abundance", -0.1,
                                        ifelse(combined$model == "Open-Habitat Bird Abundance", -0.2, 0.1))))
p <- ggplot(combined, aes(x = m, y = parameter, color = model)) + theme_bw() +
  geom_point(position = pos, size = 6) +
  vline_0(size = 0.25, color = "darkgray", linetype = 2) +
  geom_linerange(aes(xmin = ll, xmax = hh), position = pos, size = 2) + 
  yaxis_text(FALSE) +
  theme(plot.margin = unit(c(1,1,1,1),"cm"),
        plot.title = element_text(size = 24, face = "bold"),
        legend.text = element_text(size=18, family = "Arial"),
        axis.title = element_text(size = 20), axis.title.x = element_text(vjust= -2, family ="Arial" ), axis.title.y = element_text(vjust = 5, family = "Arial"), 
        axis.text = element_text(size = 18, family = "Arial")) + labs(title = "", x = "Slope difference", y = "Model")

p <- p + scale_color_manual(values = c("#253494", "#41b6c4", "#31a354", "#addd8e")) 
p
ggsave(filename = "~/Space-time-manuscript/FINAL_Avg_SlopeDifferences.png", device = "png", plot = p,
       width = 30, height = 20, units = "cm") 

##----------------------------------------------------------------

##---------------------------------------------------------------
##              Space-Time Slope Differnces : Plots             -
##---------------------------------------------------------------

load("Output_FINAL_REVISED_RF.RData")
b_dif_rg <- as.matrix(stanfit, pars = "b_dif_rg")

col <- rep("#253494", 6)
#253494 - richness
#31a354 - abund

### main model - by region
color_scheme_set(col)
  bayesplot_theme_set(theme_bw())
  
comparisons <- seq(1:30)

p1 <- mcmc_intervals(b_dif_rg)

p1 <- p1 + vline_0(size = 0.25, color = "darkgray", linetype = 2) +
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


load("Output_FINAL_REVISED_RO.RData")

b_dif_rg <- as.matrix(stanfit, pars = "b_dif_rg")


col <- rep("#67C5D0", 6)
# #67C5D0 - richness
# #addd8e - abund

color_scheme_set(col)
bayesplot_theme_set(theme_bw())
p2 <- mcmc_intervals(b_dif_rg)

p2 <- p2 + vline_0(size = 0.25, color = "darkgray", linetype = 2) +
  labs(
    x = "Slope difference", 
    y = "Comparison",
    size = 4
  ) +
  theme(plot.margin = unit(c(1,1,1,1),"cm"),
        plot.title = element_text(size = 20, face = "bold"),
        axis.title = element_text(size = 14), axis.title.x = element_text(vjust= -2 ), axis.title.y = element_text(vjust = 5), 
        axis.text = element_text(size = 12)) + labs(title = "")

p2

load("Output_FINAL_REVISED_TF.RData")
b_dif_rg <- as.matrix(stanfit, pars = "b_dif_rg")


col <- rep("#31a354", 6)

### main model - by region
color_scheme_set(col)
bayesplot_theme_set(theme_bw())
p3 <- mcmc_intervals(b_dif_rg)

p3 <- p3 + vline_0(size = 0.25, color = "darkgray", linetype = 2) +
  labs(
    x = "Slope difference", 
    y = "Comparison",
    size = 4
  ) +
  theme(plot.margin = unit(c(1,1,1,1),"cm"),
        plot.title = element_text(size = 20, face = "bold"),
        axis.title = element_text(size = 14), axis.title.x = element_text(vjust= -2 ), axis.title.y = element_text(vjust = 5), 
        axis.text = element_text(size = 12)) + labs(title = "")

p3

load("Output_FINAL_REVISED_TO.RData")

b_dif_rg <- as.matrix(stanfit, pars = "b_dif_rg")

col <- rep("#addd8e", 6)

### main model - by region
color_scheme_set(col)
bayesplot_theme_set(theme_bw())
p4 <- mcmc_intervals(b_dif_rg)

p4 <- p4 + vline_0(size = 0.25, color = "darkgray", linetype = 2) +
  labs(
    x = "Slope difference", 
    y = "Comparison",
    size = 4
  ) +
  theme(plot.margin = unit(c(1,1,1,1),"cm"),
        plot.title = element_text(size = 20, face = "bold"),
        axis.title = element_text(size = 14), axis.title.x = element_text(vjust= -2 ), axis.title.y = element_text(vjust = 5), 
        axis.text = element_text(size = 12)) + labs(title = "")

p4


all <- ggarrange(p3, p1, p4, p2,
          ncol = 2, nrow = 2) 

all

ggsave(filename = "FINAL_SlopeDifferences.png", device = "png", plot = all,
       width = 30, height = 30, units = "cm")

### END OF CODE ---------------------------------------------------------------


