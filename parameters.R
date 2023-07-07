# load libraries
library(tidyverse)

# for marine shrimp in zero-exchange system, Ptan = F * PC * 0.144
# for starvation, TAN = 0.067 mg N-NH3 / h / g
# starvation oxygen concentration ~ 1 mg oxy/g/h
# organic carbon requirements: TAN * 15.17 is carb requirement, less feed carb content* 0.1089 / 0.4

# loading parameters
system.parameters <- system.loading %>%
  mutate(total.tan = total.feed * protein.proportion * 0.144,
         clearwater.tss = total.feed * 0.25,
         biofloc.tss = total.feed * 0.25 + total.tan * 8.07,
         clearwater.od = total.feed * 0.25 + total.tan * 4.57,
         biofloc.od = total.feed * 0.25 + total.tan * 4.71,
         clearwater.alk = total.tan * 7.05,
         biofloc.alk = total.tan * 3.57,
         biofloc.carbon = total.feed * protein.proportion * 0.144 * 15.17 - (total.feed * 0.1089 / 0.4),
         clearwater.co2 = clearwater.od * 1.375,
         biofloc.co2 = biofloc.od * 1.375)

clearwater.max.loading.summary <- system.parameters %>%
  filter(facility.day > 157 & facility.day < 365) %>%
  group_by(system, facility.day) %>%
  summarise(individuals.sum = sum(total.number),
            feed.sum = sum(total.feed),
            protein.sum = sum(total.protein),
            tan.sum = sum(total.tan),
            tss.sum = sum(clearwater.tss),
            od.sum = sum(clearwater.od),
            alk.sum = sum(clearwater.alk),
            co2.sum = sum(clearwater.co2)) %>%
  summarise(individuals.max = max(individuals.sum),
            feed.max = max(feed.sum),
            protein.max = max(protein.sum),
            tan.max = max(tan.sum),
            tss.max = max(tss.sum),
            od.max = max(od.sum),
            alk.max = max(alk.sum),
            co2.max = max(co2.sum)) %>%
  arrange(match(system, c("nursery_1", "growout_1", "growout_2")))
  
biofloc.max.loading.summary <- system.parameters %>%
  filter(facility.day > 157 & facility.day < 365) %>%
  group_by(system, facility.day) %>%
  summarise(individuals.sum = sum(total.number),
            feed.sum = sum(total.feed),
            protein.sum = sum(total.protein),
            tan.sum = sum(total.tan),
            tss.sum = sum(biofloc.tss),
            od.sum = sum(biofloc.od),
            alk.sum = sum(biofloc.alk),
            co2.sum = sum(biofloc.co2)) %>%
  summarise(individuals.max = max(individuals.sum),
            feed.max = max(feed.sum),
            protein.max = max(protein.sum),
            tan.max = max(tan.sum),
            tss.max = max(tss.sum),
            od.max = max(od.sum),
            alk.max = max(alk.sum),
            co2.max = max(co2.sum)) %>%
  arrange(match(system, c("nursery_1", "growout_1", "growout_2")))

clearwater.avg.loading.summary <- system.parameters %>%
  filter(facility.day > 157 & facility.day < 365) %>%
  group_by(system, facility.day) %>%
  summarise(individuals.sum = sum(total.number),
            feed.sum = sum(total.feed),
            protein.sum = sum(total.protein),
            tan.sum = sum(total.tan),
            tss.sum = sum(clearwater.tss),
            od.sum = sum(clearwater.od),
            alk.sum = sum(clearwater.alk),
            co2.sum = sum(clearwater.co2)) %>%
  summarise(individuals.avg = mean(individuals.sum),
            feed.avg = mean(feed.sum),
            protein.avg = mean(protein.sum),
            tan.avg = mean(tan.sum),
            tss.avg = mean(tss.sum),
            od.avg = mean(od.sum),
            alk.avg = mean(alk.sum),
            co2.avg = mean(co2.sum)) %>%
  arrange(match(system, c("nursery_1", "growout_1", "growout_2")))

biofloc.avg.loading.summary <- system.parameters %>%
  filter(facility.day > 157 & facility.day < 365) %>%
  group_by(system, facility.day) %>%
  summarise(individuals.sum = sum(total.number),
            feed.sum = sum(total.feed),
            protein.sum = sum(total.protein),
            tan.sum = sum(total.tan),
            tss.sum = sum(biofloc.tss),
            od.sum = sum(biofloc.od),
            alk.sum = sum(biofloc.alk),
            co2.sum = sum(biofloc.co2)) %>%
  summarise(individuals.avg = mean(individuals.sum),
            feed.avg = mean(feed.sum),
            protein.avg = mean(protein.sum),
            tan.avg = mean(tan.sum),
            tss.avg = mean(tss.sum),
            od.avg = mean(od.sum),
            alk.avg = mean(alk.sum),
            co2.avg = mean(co2.sum)) %>%
  arrange(match(system, c("nursery_1", "growout_1", "growout_2")))

