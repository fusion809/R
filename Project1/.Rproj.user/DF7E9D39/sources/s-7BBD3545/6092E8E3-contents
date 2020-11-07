library(downloader) 
library(dplyr)
library(rafalib)
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleMiceWeights.csv"
filename <- "femaleMiceWeights.csv"
if(!file.exists("femaleMiceWeights.csv")) download(url,destfile=filename)
dat <- read.csv(filename)

X <- filter(dat, Diet=="chow") %>% select(Bodyweight) %>% unlist
Y <- filter(dat, Diet=="hf") %>% select(Bodyweight) %>% unlist

Xbar = mean(X)
Ybar = mean(Y)
Xsd = sd(X)
Ysd = sd(Y)
SE = sqrt((Xsd^2)/length(X) + (Ysd^2)/length(Y))
print(SE)