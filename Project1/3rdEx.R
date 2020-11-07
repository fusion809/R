dat <- read.csv("msleep_ggplot2.csv")
library(dplyr)

filteredDat = filter(dat, order=="Primates")
nrow(filteredDat)

totalSleep <- select(filteredDat, sleep_total) %>% unlist
meanSleep <- mean(totalSleep)

meanSleep2 <- summarize(filteredDat)