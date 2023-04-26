# import libraries
library(tidyverse)

# import data
growout.model <- read.csv("data/shrimp_growth.csv") %>%
  mutate(wt.mid = (initial + final) / 2,
         geometric.mid = sqrt(initial * final),
         g.gain.day = (final - initial) / days)

# define parameters
bw.1 <- 0.5
bw.2 <- 25


# run a couple of models to see how relationship looks between arithmetic mean mass and geometric mean 
# arithmetic mean of bodyweight between weighings
shrimp.fit.twinlog <- lm(log(g.gain.day) ~ log(wt.mid),
                         data = growout.model)
summary(shrimp.fit.twinlog)

# plot this relationship
plot(log(g.gain.day) ~ log(wt.mid), data = growout.model,
     col = "grey",
     pch = 20,
     cex = 1.5,
     main = "g per day by weight")
# add the trendline
abline(shrimp.fit.twinlog,
       col = "darkorange",
       lwd = 2)
# check the residual plot
plot(fitted(shrimp.fit.twinlog),
     resid(shrimp.fit.twinlog),
     col = "grey",
     pch = 20,
     xlab = "fitted",
     ylab = "residuals",
     main = "fitted vs residuals")
abline(h = 0,
       col = "darkorange",
       lwd = 2)
# check a Q-Q plot
qqnorm(resid(shrimp.fit.twinlog),
       main = "normal Q-Q plot",
       col = "darkgrey")
qqline(resid(shrimp.fit.twinlog),
       col = "dodgerblue",
       lwd = 2)
# plot the growth curve to see how the model matches data points taken from the literature
plot(g.gain.day ~ wt.mid,
     data = growout.model,
     col = "grey",
     pch = 20,
     cex = 1.5,
     main = "g per day gained")
curve(exp(shrimp.fit.twinlog$coefficients[1]) * x ^ shrimp.fit.twinlog$coefficients[2],
      from = 0,
      to = 30,
      add = TRUE,
      col = "darkorange",
      lwd = 2)

# calculate coefficients for growth curve bw_1 = [bw_0^c_1 + c_2 * days]^c_4
c_1 <- 1 - shrimp.fit.twinlog$coefficients[2]
c_2 <- c_1 * exp(shrimp.fit.twinlog$coefficients[1])
c_4 <- 1 / c_1

# perform the log-log relationship for geometric weight
shrimp.geometric.twinlog <- lm(log(g.gain.day) ~ log(geometric.mid),
                               data = growout.model)
summary(shrimp.geometric.twinlog)

plot(log(g.gain.day) ~ log(geometric.mid),
     data = growout.model,
     col = "grey",
     pch = 20,
     cex = 1.5,
     main = "g per day per weight")
abline(shrimp.geometric.twinlog,
       col = "darkorange",
       lwd = 2)

# plot the growth curve to see how the model matches data points taken from the literature
plot(g.gain.day ~ geometric.mid,
     data = growout.model,
     col = "grey",
     pch = 20,
     cex = 1.5,
     main = "g per day gained")
curve(exp(shrimp.geometric.twinlog$coefficients[1]) * x ^ shrimp.geometric.twinlog$coefficients[2],
      from = 0,
      to = 30,
      add = TRUE,
      col = "darkorange",
      lwd = 2)

# calculate coefficients for growth curve bw_1 = [bw_0^c_1 + c_2 * days]^c_4
geometric_c_1 <- 1 - shrimp.geometric.twinlog$coefficients[2]
geometric_c_2 <- geometric_c_1 * exp(shrimp.geometric.twinlog$coefficients[1])
geometric_c_4 <- 1 / geometric_c_1

# plot relationships between weight and grams gained per day
p_shrimp_gain <- growout.model %>%
  ggplot(aes(wt.mid, g.gain.day)) +
  geom_point() +
  stat_function(fun = function(x) exp(shrimp.fit.twinlog$coefficients[1]) * x^shrimp.fit.twinlog$coefficients[2], color = "dodgerblue") +
  stat_function(fun = function(x) exp(shrimp.geometric.twinlog$coefficients[1]) * x^shrimp.geometric.twinlog$coefficients[2], color = "black") +
  xlab("Mass (g)") +
  ylab("Daily mass gain (g/shrimp/day)") +
  theme_minimal()

# print plot to file
pdf("plots/p_shrimp_gain.pdf", height = 4)
print(p_shrimp_gain)
dev.off()

bodyweight.growout <- tibble(day = seq(0,180,1)) %>%
  mutate(arithmetic_bw = (bw_1^c_1 + c_2 * day)^c_4,
         geometric_bw = (bw_1^geometric_c_1 + geometric_c_2 * day)^geometric_c_4) %>%
  pivot_longer(c(arithmetic_bw, geometric_bw), names_to = "model", values_to = "bw") %>%
  filter(model == "arithmetic_bw")

# write csv
write.csv(bodyweight.growout, "tabs/bodyweight_growout.csv")

growout_plot <- bodyweight.growout %>%
  filter(model == "arithmetic_bw") %>%
  ggplot(aes(day, bw)) +
  geom_line() +
  xlim(0, 150) +
  xlab("Days") +
  ylab("Mass (g)") +
  theme_minimal()

# print plot to file
pdf("plots/growout_plot.pdf", height = 3)
print(growout_plot)
dev.off()

# perform similar calculations for nursery stages
# import data
nursery.model <- read.csv("data/pl_growth.csv") %>%
  mutate(wt.mid = (initial + final) / 2,
         geometric.mid = sqrt(initial * final),
         g.gain.day = (final - initial) / days)

# define parameters
bw_0 <- 0.02

# run a couple of models to see how relationship looks between arithmetic mean mass and geometric mean 
# arithmetic mean of bodyweight between weighings
nursery.fit.twinlog <- lm(log(g.gain.day) ~ log(wt.mid),
                         data = nursery.model)
summary(nursery.fit.twinlog)

# plot this relationship
plot(log(g.gain.day) ~ log(wt.mid), data = nursery.model,
     col = "grey",
     pch = 20,
     cex = 1.5,
     main = "g per day by weight")
# add the trendline
abline(nursery.fit.twinlog,
       col = "darkorange",
       lwd = 2)
# check the residual plot
plot(fitted(nursery.fit.twinlog),
     resid(nursery.fit.twinlog),
     col = "grey",
     pch = 20,
     xlab = "fitted",
     ylab = "residuals",
     main = "fitted vs residuals")
abline(h = 0,
       col = "darkorange",
       lwd = 2)
# check a Q-Q plot
qqnorm(resid(nursery.fit.twinlog),
       main = "normal Q-Q plot",
       col = "darkgrey")
qqline(resid(nursery.fit.twinlog),
       col = "dodgerblue",
       lwd = 2)
# plot the growth curve to see how the model matches data points taken from the literature
plot(g.gain.day ~ wt.mid,
     data = nursery.model,
     col = "grey",
     pch = 20,
     cex = 1.5,
     main = "g per day gained")
curve(exp(nursery.fit.twinlog$coefficients[1]) * x ^ nursery.fit.twinlog$coefficients[2],
      from = 0,
      to = 30,
      add = TRUE,
      col = "darkorange",
      lwd = 2)


# calculate coefficients for growth curve bw_1 = [bw_0^c_1 + c_2 * days]^c_4
c_1_nursery <- 1 - nursery.fit.twinlog$coefficients[2]
c_2_nursery <- c_1_nursery * exp(nursery.fit.twinlog$coefficients[1])
c_4_nursery <- 1 / c_1_nursery

# perform the log-log relationship for geometric weight
nursery.geometric.twinlog <- lm(log(g.gain.day) ~ log(geometric.mid),
                               data = nursery.model)
summary(nursery.geometric.twinlog)

plot(log(g.gain.day) ~ log(geometric.mid),
     data = nursery.model,
     col = "grey",
     pch = 20,
     cex = 1.5,
     main = "g per day per weight")
abline(nursery.geometric.twinlog,
       col = "darkorange",
       lwd = 2)

# plot the growth curve to see how the model matches data points taken from the literature
plot(g.gain.day ~ geometric.mid,
     data = nursery.model,
     col = "grey",
     pch = 20,
     cex = 1.5,
     main = "g per day gained")
curve(exp(nursery.geometric.twinlog$coefficients[1]) * x ^ nursery.geometric.twinlog$coefficients[2],
      from = 0,
      to = 30,
      add = TRUE,
      col = "darkorange",
      lwd = 2)

# calculate coefficients for growth curve bw_1 = [bw_0^c_1 + c_2 * days]^c_4
geometric_c_1_nursery <- 1 - nursery.geometric.twinlog$coefficients[2]
geometric_c_2_nursery <- geometric_c_1_nursery * exp(nursery.geometric.twinlog$coefficients[1])
geometric_c_4_nursery <- 1 / geometric_c_1_nursery

# plot relationships between weight and grams gained per day
p_nursery_gain <- nursery.model %>%
  ggplot(aes(wt.mid, g.gain.day)) +
  geom_point() +
  stat_function(fun = function(x) exp(nursery.fit.twinlog$coefficients[1]) * x^nursery.fit.twinlog$coefficients[2], color = "dodgerblue") +
  stat_function(fun = function(x) exp(nursery.geometric.twinlog$coefficients[1]) * x^nursery.geometric.twinlog$coefficients[2], color = "black") +
  xlab("Mass (g)") +
  ylab("Daily mass gain (g/shrimp/day)") +
  theme_minimal()

# print plot to file
pdf("plots/p_nursery_gain.pdf", height = 4)
print(p_nursery_gain)
dev.off()

bodyweight.nursery <- tibble(day = seq(0,56,1)) %>%
  mutate(arithmetic_bw = (bw_0^c_1_nursery + c_2_nursery * day)^c_4_nursery) 

# write csv
write.csv(bodyweight.nursery, "tabs/bodyweight_nursery.csv")

nursery_plot <- bodyweight.nursery %>%
  ggplot(aes(day, arithmetic_bw)) +
  geom_line() +
  xlim(0, 50) +
  xlab("Days") +
  ylab("Mass (g)") +
  theme_minimal()

# print plot to file
pdf("plots/nursery_plot.pdf", height = 3)
print(nursery_plot)
dev.off()

# find when key bodyweights are attained
shrimp.0.5g <- ceiling(which(abs(bodyweight.nursery$arithmetic_bw - bw.1) == min(abs(bodyweight.nursery$arithmetic_bw - bw.1))))
shrimp.25g <- ceiling(which(abs(bodyweight.growout$bw - bw.2) == min(abs(bodyweight.growout$bw - bw.2)))) + shrimp.0.5g



