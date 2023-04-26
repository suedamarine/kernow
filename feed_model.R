library(tidyverse)

# import data
feed <- read.csv("data/feed.csv") %>%
  mutate(mass = (mass_1 + mass_2) / 2,
         prop_feed = (rate_1 + rate_2) / 2)

feed %>% ggplot(aes(mass, prop_feed)) +
  geom_point()

feed.twinlog <- lm(log(prop_feed) ~ log(mass), data = feed)
summary(feed.twinlog)

plot(log(prop_feed) ~ log(mass), data = feed, col = "grey", pch = 20, cex = 1.5, main = "feed_wt")
abline(feed.twinlog, col = "darkorange", lwd = 2)

plot(prop_feed ~ mass, data = feed, col = "grey", pch = 20, cex = 1.5, main = "Feed to weight")
curve(exp(feed.twinlog$coefficients[1]) * x ^ feed.twinlog$coefficients[2], from = 0, to = 25, add = TRUE, col = "darkorange", lwd = 2)

feed.plot <- feed %>%
  ggplot(aes(mass, prop_feed)) +
  geom_point() +
  stat_function(fun = function(x) exp(feed.twinlog$coefficients[1]) * x ^ feed.twinlog$coefficients[2],
                color = "orange",
                xlim = c(0.1, 25),
                alpha = 0.4) +
  labs(x = "Shrimp bodyweight (g)",
       y = "Feed as a proportion of bodyweight (%)") +
  theme_minimal()

pdf("plots/feed_plot.pdf",
    height = 3)
feed.plot
dev.off()

