library(downloader)
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/babies.txt"
filename <- basename(url)
download(url, destfile=filename)
babies <- read.table("babies.txt", header=TRUE)

bwt.nonsmoke <- filter(babies, smoke==0) %>% select(bwt) %>% unlist 
bwt.smoke <- filter(babies, smoke==1) %>% select(bwt) %>% unlist

library(rafalib)
diff <- mean(bwt.nonsmoke)-mean(bwt.smoke)
nssd <- popsd(bwt.nonsmoke)
ssd <- popsd(bwt.smoke)

set.seed(1)
N = 25
nsN <- sample(bwt.nonsmoke, N)
sN <- sample(bwt.smoke, N)
diffN <- mean(nsN) - mean(sN)
SEN <- sqrt((sd(nsN)^2)/N + (sd(sN)^2)/N)
tStat <- diffN/SEN
Q <- qt(1-0.01/2, df=2*N-2)
Q*SEN