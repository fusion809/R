library(downloader) 
library(dplyr)
library(rafalib)
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/mice_pheno.csv"
filename <- basename(url)
download(url, destfile=filename)
dat <- read.csv(filename)

dat <- na.omit( dat )

x <- filter(dat, Diet=="chow") %>% filter(Sex=="M") %>%
  select(Bodyweight) %>% unlist
xbar <- mean(x)
xsd <- popsd(x)

y <- filter(dat, Diet=="hf") %>% filter(Sex=="M") %>%
  select(Bodyweight) %>% unlist
ybar <- mean(y)
ysd <- popsd(y)


sd = sqrt((ysd^2)/length(y) + (xsd^2)/length(x))
tstat = (ybar-xbar)/sd

print(2*(1-pnorm(tstat)))