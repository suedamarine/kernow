# load libraries
library(tidyverse)

# define water quality parameters
clearwater.tss <- 20 # max 20 mg/l in tanks
desired.o2 <- 5.0 # min 5 mg/l in tanks
desired.tan <- 1 # max 1 mg/l TAN in tanks
desired.nitrate <- 150 # max 50 mg/l nitrate in tanks
desired.co2 <- 15 # max 15 mg/l in tanks
desired.t <- 28 # system operating temp in C
site.altitude <- 50 # site altitude in metres
cone.bar <- 0.7
feeding.period <- 20
metabolic.period <- feeding.period + 4



# component specifics
# system efficiency 
tan.c1 <- desired.tan
tan.cbest <- 0
tan.te <- 1
tan.c2 <- tan.c1 + tan.te * (tan.cbest - tan.c1)


