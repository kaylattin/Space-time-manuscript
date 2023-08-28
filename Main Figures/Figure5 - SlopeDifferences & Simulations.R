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
library(rethinking)

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
p <- ggplot(combined, aes(x = m, y = parameter, color = model, shape = model, fill = model)) +
    geom_linerange(aes(xmin = ll, xmax = hh), position = pos, size = 1) + 
  geom_point(position = pos, size = 3, stroke = 1) +
  vline_0(linewidth = 0.25, color = "darkgray", linetype = 2) +
  yaxis_text(FALSE) +
  labs(title = "", x = "Slope difference", y = "Model") + 
  theme(plot.margin = unit(c(0.2,0.2,0.2,0.2),"cm"),
        plot.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size=9, family = "Arial"),
        axis.title = element_text(size = 10), axis.title.x = element_text(vjust= -2, family = "Arial"), axis.title.y = element_text(vjust = 5, family = "Arial"), 
        axis.text = element_text(size = 9, family = "Arial"),
        panel.background = element_rect(fill = "white")) + 
  scale_y_discrete(labels = "") +
  scale_fill_manual(values=c("#253494", "white", "#31a354","white"),
                    guide = "none") +
  scale_shape_manual(values=c(16, 21, 16, 21),
                     guide = "none") +
  scale_color_manual(name = "", values = c("#253494", "#41b6c4", "#31a354", "#addd8e"),
                     guide = guide_legend(nrow = 4, override.aes = list(shape = c(16,21,16,21),
                                                              fill = c(NA, "white", NA, "white"),
                                                              stroke = c(NA, 2, NA, 2)))) + theme_bw()

p <- p + labs(tag = "B") + theme(plot.tag = element_text(face = "bold", size = 18), legend.position = "bottom")
p

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
  
comparisons <- seq(1:25)
fit1 <- summary(stanfit, pars = "b_dif_rg")
p1 <- mcmc_intervals(b_dif_rg, point_size = 0, inner_size = 1)

p1 <- p1 + vline_0(size = 0.25, color = "darkgray", linetype = 2) +
  labs(
    x = "Slope difference", 
    y = "Comparison",
    size = 4
  ) +
  scale_y_discrete(labels = rep(1:25)) +
  theme(plot.margin = unit(c(0.2,0.2,0.4,0.4),"cm"),
        plot.title = element_text(size = 10, face = "bold"),
        axis.title = element_text(size = 9), axis.title.x = element_text(vjust= -2 ), axis.title.y = element_text(vjust = 5), 
        axis.text = element_text(size = 6)) + labs(title = "") +
  geom_point(aes(x = fit1$summary[,1], y = (rep(1:25))),
                 fill = "#253494", stroke = 1, pch = 21, size = 1.5, colour = "#253494")
p1

load("Output_FINAL_REVISED_RO.RData")

b_dif_rg <- as.matrix(stanfit, pars = "b_dif_rg")

col <- rep("#67C5D0", 6)
# #67C5D0 - richness
# #addd8e - abund

color_scheme_set(col)
bayesplot_theme_set(theme_bw())

fit2 <- summary(stanfit, pars = "b_dif_rg")
p2 <- mcmc_intervals(b_dif_rg, point_size = 0, inner_size = 1)
p2 <- p2 + vline_0(size = 0.25, color = "darkgray", linetype = 2) +
  labs(
    x = "Slope difference", 
    y = "Comparison",
    size = 4
  ) +
  scale_y_discrete(labels = rep(1:25)) +
  theme(plot.margin = unit(c(0.2,0.2,0.4,0.4),"cm"),
        plot.title = element_text(size = 10, face = "bold"),
        axis.title = element_text(size = 9), axis.title.x = element_text(vjust= -2 ), axis.title.y = element_text(vjust = 5), 
        axis.text = element_text(size = 6)) + labs(title = "") +
  geom_point(aes(x = fit2$summary[,1], y = (rep(1:25))),
             fill = "white", stroke = 1, pch = 21, size = 1.5, colour = "#67C5D0")

p2

load("Output_FINAL_REVISED_TF.RData")
b_dif_rg <- as.matrix(stanfit, pars = "b_dif_rg")


col <- rep("#31a354", 6)

### main model - by region
color_scheme_set(col)
bayesplot_theme_set(theme_bw())
fit3 <- summary(stanfit, pars = "b_dif_rg")

p3 <- mcmc_intervals(b_dif_rg, point_size = 0, inner_size = 1)
p3 <- p3 + vline_0(size = 0.25, color = "darkgray", linetype = 2) +
  labs(
    x = "Slope difference", 
    y = "Comparison",
    size = 4
  ) +
  scale_y_discrete(labels = rep(1:25)) +
  theme(plot.margin = unit(c(0.2,0.2,0.4,0.4),"cm"),
        plot.title = element_text(size = 10, face = "bold"),
        axis.title = element_text(size = 9), axis.title.x = element_text(vjust= -2 ), axis.title.y = element_text(vjust = 5), 
        axis.text = element_text(size = 6)) + labs(title = "") +
  geom_point(aes(x = fit3$summary[,1], y = (rep(1:25))),
             fill = "#31a354", stroke = 1, pch = 21, size = 1.5, colour = "#31a354")

p3

load("Output_FINAL_REVISED_TO.RData")

b_dif_rg <- as.matrix(stanfit, pars = "b_dif_rg")

col <- rep("#addd8e", 6)

### main model - by region
color_scheme_set(col)
bayesplot_theme_set(theme_bw())
fit4 <- summary(stanfit, pars = "b_dif_rg")
p4 <- mcmc_intervals(b_dif_rg, point_size = 0, inner_size = 1)

p4 <- p4 + vline_0(size = 0.25, color = "darkgray", linetype = 2) +
  labs(
    x = "Slope difference", 
    y = "Comparison",
    size = 4
  ) +
  scale_y_discrete(labels = rep(1:25)) +
  theme(plot.margin = unit(c(0.2,0.2,0.4,0.4),"cm"),
        plot.title = element_text(size = 10, face = "bold"),
        axis.title = element_text(size = 9), axis.title.x = element_text(vjust= -2 ), axis.title.y = element_text(vjust = 5), 
        axis.text = element_text(size = 6)) + labs(title = "") +
  geom_point(aes(x = fit4$summary[,1], y = (rep(1:25))),
             fill = "white", stroke = 1, pch = 21, size = 1.5, colour = "#addd8e")

p4

all4 <- ggarrange(p1, p3, p2, p4,
          ncol = 2, nrow = 2) 

all4 <- all4 + labs(tag = "A") + theme(plot.tag = element_text(face = "bold", size = 18, vjust = 1.2, hjust = -0.5), plot.margin = unit(c(0.1,0,0,0),"cm"))
all4
all <- ggarrange(all4, p, ncol = 2, nrow = 1, widths = c(1.5,1))
all

ggsave(filename = "FIGURE 5_FINAL.tiff", device = "tiff", plot = all,
       width = 5600, height = 3000, units = "px", dpi = 600, bg = "white")

### END OF CODE ---------------------------------------------------------------


