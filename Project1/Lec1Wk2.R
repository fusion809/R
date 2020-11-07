dat <- read.csv("femaleMiceWeights.csv")
library(dplyr)

controls <- filter(dat, Diet=="chow") %>% 
  select(Bodyweight) %>% unlist

treatment <- filter(dat, Diet=="hf") %>% 
  select(Bodyweight) %>% unlist

obs <- mean(treatment)-mean(controls)

population <- read.csv("femaleControlsPopulation.csv")
population <- unlist(population)

obs <- mean(treatment)-mean(controls)

n <- 100000
nulls <- vector("numeric", n)
for (i in 1:n) {
  controlR <- sample(population, 12)
  treatmentR <- sample(population, 12)
  nulls[i] <- mean(treatmentR)-mean(controlR)
}

mean(abs(nulls) > obs)


