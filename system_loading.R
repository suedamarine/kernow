# load libraries
library(tidyverse)


# build system loading onto production plan
system.loading <- prod.plan %>%
  mutate(total.number = case_when(system == "nursery_1" ~ nursery.1.initial * exp(1)^(log(nursery.survival) / nursery.1.period * batch_day),
                                  system == "growout_1" ~ growout.1.initial * exp(1)^(log(growout.1.survival) / growout.1.period * (batch_day - (nursery.1.period + 1))),
                                  system == "growout_2" ~ growout.2.initial * exp(1)^(log(growout.2.survival) / growout.2.period * (batch_day - (nursery.1.period + 1) - growout.1.period))),
         bw = case_when(system == "nursery_1" ~ (bw_0^c_1_nursery + c_2_nursery * batch_day)^c_4_nursery,
                        TRUE ~ (bw_1^c_1 + c_2 * (batch_day - nursery.1.period + 1))^c_4),
         total.bw.kg = bw / 1000 * total.number,
         feed.rate = case_when(bw < 0.1 ~ 0.35,
                               TRUE ~ exp(feed.twinlog$coefficients[1]) * bw ^ feed.twinlog$coefficients[2] / 100),
         total.feed = feed.rate * total.bw.kg,
         protein.proportion = case_when(bw < 0.25 ~ 0.5,
                                        bw >= 0.25 & bw < 1.0 ~ 0.45,
                                        bw >= 1.0 & bw < 3 ~ 0.4,
                                        bw >= 3 ~ 0.35),
         total.protein = total.feed * protein.proportion)

# get summary info on max biomass per stanza
stanza.biomass.max <- system.loading %>%
  group_by(system) %>%
  summarize(max.biomass = max(total.bw.kg)) %>%
  arrange(match(system, c("nursery_1", "growout_1", "growout_2")))

# get a diameter from an area (circle)
circle.d <- function(a){
  r <- sqrt(a/pi)
  print(r * 2)
}

nursery.area <- 450
growout.1.area <- 4000
growout.2.area <- 6000


## save this for later 
#density = case_when(system == "nursery_1" ~ total.bw.kg / nursery.area,
                    #system == "growout_1" ~ total.bw.kg / growout.1.area,
                    #system == "growout_2" ~ total.bw.kg / growout.2.area),




