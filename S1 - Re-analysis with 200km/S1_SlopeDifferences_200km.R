library(bayesplot)
library(RColorBrewer)
library(ggplot2)
library(ggpubr)


region <- vector()

for( i in 1:13 ) {
  
  region[i] <- paste("Region", i, sep = " ")
  
}
region <- rev(region)

### SIMULATED Y AS X CHANGES
# xrep = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)
# 
# load("Output_RF_200km_ALL.RData")
# yrep <- as.matrix(stanfit, pars = "y_rep")
# 
# yrep <- summary(stanfit, pars = "sim_space")
# y_space_RF <- yrep$summary[,1]
# yrep <- summary(stanfit, pars = "sim_time")
# y_time_RF <- yrep$summary[,1]
# 
# load("Output_RO_200km_ALL.RData")
# yrep <- summary(stanfit, pars = "sim_space")
# y_space_RO <- yrep$summary[,1]
# yrep <- summary(stanfit, pars = "sim_time")
# y_time_RO <- yrep$summary[,1]
# 
# load("Output_TF_200km_ALL.RData")
# yrep <- summary(stanfit, pars = "sim_space")
# y_space_TF <- yrep$summary[,1]
# yrep <- summary(stanfit, pars = "sim_time")
# y_time_TF <- yrep$summary[,1]
# 
# load("Output_TO_200km_ALL.RData")
# yrep <- summary(stanfit, pars = "sim_space")
# y_space_TO <- yrep$summary[,1]
# yrep <- summary(stanfit, pars = "sim_time")
# y_time_TO <- yrep$summary[,1]

## SLOPE SUMMARIES --------------------------------------
load("Output_FINAL_REVISED_RF_200km.RData")
diff_RF <- as.matrix(stanfit, pars = "b_dif")
avg_RF <- mean(diff_RF)
PI_RF <- PI(diff_RF)
slopes_RF <- c(mean(as.matrix(stanfit, pars = "avg_b_time")), mean(as.matrix(stanfit, pars = "avg_b_space")))
slopes_PI_RF <- c(PI(as.matrix(stanfit, pars = "avg_b_time")), PI(as.matrix(stanfit, pars = "avg_b_space")))

load("Output_FINAL_REVISED_RO_200km.RData")
diff_RO <- as.matrix(stanfit, pars = "b_dif")
avg_RO <- mean(diff_RO)
PI_RO <- PI(diff_RO)
slopes_RO <- c(mean(as.matrix(stanfit, pars = "avg_b_time")), mean(as.matrix(stanfit, pars = "avg_b_space")))
slopes_PI_RO <- c(PI(as.matrix(stanfit, pars = "avg_b_time")), PI(as.matrix(stanfit, pars = "avg_b_space")))

load("Output_FINAL_REVISED_TF_200km.RData")
diff_TF <- as.matrix(stanfit, pars = "b_dif")
avg_TF <- mean(diff_TF)
PI_TF <- PI(diff_TF)
slopes_TF <- c(mean(as.matrix(stanfit, pars = "avg_b_time")), mean(as.matrix(stanfit, pars = "avg_b_space")))
slopes_PI_TF <- c(PI(as.matrix(stanfit, pars = "avg_b_time")), PI(as.matrix(stanfit, pars = "avg_b_space")))

load("Output_FINAL_REVISED_TO_200km.RData")
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

p <- p + scale_color_manual(values = c("#DC267F", "#f792b0", "#FE6100", "#f7b456"))
p

ggsave(filename = "~/Space-time-manuscript/FINAL_Avg_SlopeDifferences_200km.png", device = "png", plot = p,
       width = 30, height = 20, units = "cm") 

#### PLOTS --------------------------------------------------

load("Output_FINAL_REVISED_RF_200km.RData")
b_dif_rg <- as.matrix(stanfit, pars = "b_dif_rg")

col <- rep("#DC267F", 6)

comparisons <- seq(1:13)

### main model - by region
color_scheme_set(col)
bayesplot_theme_set(theme_bw())
p1 <- mcmc_intervals(b_dif_rg)

p1 <- p1 + vline_0(size = 0.25, color = "darkgray", linetype = 2) + yaxis_text(comparisons) +
    labs(
    x = "Slope difference", 
    y = "Comparison",
    size = 4
  ) +
  theme(plot.margin = unit(c(1,1,1,1),"cm"), 
        plot.title = element_text(size = 20, face = "bold"),
        axis.title = element_text(size = 14), axis.title.x = element_text(vjust= -2 ), axis.title.y = element_text(vjust = 5), 
        axis.text = element_text(size = 12)) + labs(title = "Forest Species Richness")
p1


load("Output_FINAL_REVISED_TF_200km.RData")

b_dif_rg <- as.matrix(stanfit, pars = "b_dif_rg")


col <- rep("#FE6100", 6)

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
        axis.text = element_text(size = 12)) + labs(title = "Forest Bird Total Abundance")



load("Output_FINAL_REVISED_RO_200km.RData")
b_dif_rg <- as.matrix(stanfit, pars = "b_dif_rg")


col <- rep("#f792b0", 6)

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
        axis.text = element_text(size = 12)) + labs(title = "Open-Habitat Species Richness")



load("Output_FINAL_REVISED_TO_200km.RData")

b_dif_rg <- as.matrix(stanfit, pars = "b_dif_rg")

col <- rep("#f7b456", 6)


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
        plot.title = element_text(size = 18, face = "bold"),
        axis.title = element_text(size = 14), axis.title.x = element_text(vjust= -2 ), axis.title.y = element_text(vjust = 5), 
        axis.text = element_text(size = 12)) + labs(title = "Open-Habitat Bird Total Abundance")
p4

all <- ggarrange(p1, p2, p3, p4,
                 ncol = 2, nrow = 2) 

all
ggsave(filename = "FINAl_SlopeDifferences_200km.png", device = "png", plot = all,
       width = 30, height = 30, units = "cm")
