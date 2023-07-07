# import libraries
library(tidyverse)
library(RColorBrewer)

batch.frequency <- 14 # days between batches
batch.system.periods <- 4 # overall number of system periods
production.cycle <- 175  # period whose factor should include frequency, system period and exceeds minimum growout period
nursery.1.period <- 32
growout.1.period <- 63
growout.2.period <- 63

# phase survival (Mark indicates 70%, 90%, 90%) - take conservative view of c(60, 80, 80)
nursery.survival <- 0.6
growout.1.survival <- 0.8
growout.2.survival <- 0.8

target.mass <- 25 # mass in g
nursery.1.max <- bodyweight.nursery %>%
  filter(day == nursery.1.period) %>%
  select(arithmetic_bw)
growout.1.max <- bodyweight.growout %>%
  filter(model == "arithmetic_bw" & day == growout.1.period) %>%
  select(bw)
growout.2.max <- bodyweight.growout %>%
  filter(model == "arithmetic_bw" & day == growout.1.period + growout.2.period) %>%
  select(bw)

annual.target <- 1000000 # kilos
annual.batches <- 365 / batch.frequency
batch.target <- as.integer(annual.target / annual.batches)

# set production period to two growth periods to observe system loading
prod_plan <- data.frame(facility.day = seq(0, 4 * production.cycle - 1)) %>%
  mutate(batch_1 = case_when(facility.day < production.cycle ~ facility.day))

for (lag_size in c(0:24)) {
  
  new_col_name <- paste0("lag.batch.", lag_size)
  
  prod_plan <- prod_plan %>%
    mutate(!!sym(new_col_name) := lag(batch_1, n = lag_size * batch.frequency, default = NA))
}

# remove column called batch_1
prod.plan <- prod_plan %>% select(-batch_1) %>%
  pivot_longer(c(lag.batch.0:lag.batch.24), names_to = "batch", values_to = "batch_day", values_drop_na = TRUE) %>%
  mutate(system = case_when(batch_day %in% c(0:nursery.1.period) ~ "nursery_1",
                            batch_day %in% c(nursery.1.period + 1: growout.1.period) ~ "growout_1",
                            batch_day %in% c(nursery.1.period + growout.1.period + 1 : growout.2.period) ~ "growout_2")) %>%
  drop_na()

# custom color
system.cols <- c(nursery_1 = "#bdc9e1", growout_1 = "#74a9cf", growout_2 = "#0570b0")

systems_plot <- prod.plan %>%
  filter(batch_day <= production.cycle,
         batch %in% c("lag.batch.0", "lag.batch.1", "lag.batch.2", "lag.batch.3", "lag.batch.4", "lag.batch.5", "lag.batch.6", "lag.batch.7", "lag.batch.8", "lag.batch.9", "lag.batch.10", "lag.batch.11")) %>%
  ggplot(aes(facility.day, batch, color = system)) +
  geom_line(size = 3) +
  scale_y_discrete(limits = c("lag.batch.0", "lag.batch.1", "lag.batch.2", "lag.batch.3", "lag.batch.4", "lag.batch.5", "lag.batch.6", "lag.batch.7", "lag.batch.8", "lag.batch.9", "lag.batch.10", "lag.batch.11"),
                   labels = c("Batch 1", "Batch 2", "Batch 3", "Batch 4", "Batch 5", "Batch 6", "Batch 7", "Batch 8", "Batch 9", "Batch 10", "Batch 11", "Batch 12")) +
  scale_color_manual("", breaks = c("nursery_1", "growout_1", "growout_2"),
                     values = c(system.cols),
                     labels = c("Nursery 1", "Growout 1", "Growout 2")) +
  labs(x = "Day", y = "") + 
  theme_minimal()

# numbers required per batch
final.batch.number <- batch.target / target.mass * 1000
growout.2.initial <- final.batch.number / growout.1.survival
growout.1.initial <- growout.2.initial / growout.2.survival
nursery.1.initial <- growout.1.initial / nursery.survival


