dat <- read.csv("femaleMiceWeights.csv")
library(dplyr)

controls <- filter(dat, Diet=="chow") %>% 
  select(Bodyweight) %>% unlist