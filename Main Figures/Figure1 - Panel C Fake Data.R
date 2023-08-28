library(MASS)
library(ggplot2)
library(ggpubr)

samples = 19
r = 0.83

data = mvrnorm(n=samples, mu=c(0, 0), Sigma=matrix(c(1, r, r, 1), nrow=2), empirical=TRUE)
X = data[, 1]  # standard normal (mu=0, sd=1)
Y = data[, 2]  # standard normal (mu=0, sd=1)
d <- data.frame(X, Y)

p1 <- ggplot(d, mapping = aes(X, Y)) +
  geom_point(size = 2,
             pch = 16,
             alpha = 0.8,
             colour = "#2c7bb6"
  ) + 
  geom_smooth(method = 'lm',
              colour = "#2c7bb6",
              fill = "#2c7bb6",
              alpha = 0.05) + theme_classic() +
  theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())
p1

samples = 15
r = 0.83

data = mvrnorm(n=samples, mu=c(0, 0), Sigma=matrix(c(1, r, r, 1), nrow=2), empirical=TRUE)
X = data[, 1]  # standard normal (mu=0, sd=1)
Y = data[, 2]  # standard normal (mu=0, sd=1)
d2 <- data.frame(X, Y)

p2 <- ggplot(d2, mapping = aes(X, Y)) +
  geom_point(size = 2,
             pch = 16,
             alpha = 0.8,
             colour = "#ED432D"
  ) + 
  geom_smooth(method = 'lm',
              colour = "#ED432D",
              fill = "#ED432D",
              alpha = 0.05) + theme_classic() +
  theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())

p2

plot <- ggarrange(p1, p2, ncol = 2, nrow = 1)
plot

# Save Total Abundance Plot
ggsave(filename = "FIGURE 1-3.tiff", device = "tiff", plot = plot,
       width = 5200, height = 2500, units = "px", dpi = 600, compression = "lzw")  
