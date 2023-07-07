# load libraries
library(tidyverse)

# component specifics
media.area <- 750 # 750 m2/m3
biomedia.volume.ratio <- 2
biomedia.aeration.ratio <- 3

# system rates
nitrification.rate <- 0.4 # 0.4g TAN/m2/day
skimmer.ratio <- 0.3


# system efficiency 
passive.nitrification <- 0.05 # 5% passive nitrification rate
passive.denitrification <- 0.05 # 5% passive denitrification rate

# get max and average tan loading for each system (overlapping batches where appropriate)
tan.system.loading <- system.parameters %>%
  group_by(system, facility.day) %>%
  summarise(tan.sum = sum(total.tan)) %>%
  summarise(tan.max = max(tan.sum),
            tan.avg = mean(tan.sum)) %>%
  arrange(factor(system, levels = c("nursery_1", "growout_1", "growout_2"))) %>%
  pivot_longer(c(tan.max:tan.avg), names_to = "tan.metric", values_to = "TAN", values_drop_na = TRUE) %>%
  mutate(nitrate.exchange = TAN * 1e6 * (1 - passive.denitrification) / desired.nitrate / 1000, # exchange volume m3 to maintain nitrate levels 
         available.tan = TAN * (1 - passive.nitrification) - (desired.tan / 1e6) * nitrate.exchange * 1000, # available tan after passive denitrification and water exchange
         biofilter.exchange = abs(available.tan  * 1e6 / (tan.c2 - tan.c1) / metabolic.period / 1000),
         biomedia.volume = available.tan / (nitrification.rate / 1000) / metabolic.period * 24 / media.area, # biomedia volume required m3
         biofilter.volume = biomedia.volume * biomedia.volume.ratio,
         biofilter.aeration = biomedia.aeration.ratio * biomedia.volume)




