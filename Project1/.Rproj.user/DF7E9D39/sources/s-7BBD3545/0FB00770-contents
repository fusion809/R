url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/babies.txt"
filename <- basename(url)
download(url, destfile=filename)
babies <- read.table("babies.txt", header=TRUE)

bwt.nonsmoke <- filter(babies, smoke==0) %>% select(bwt) %>% unlist 
bwt.smoke <- filter(babies, smoke==1) %>% select(bwt) %>% unlist

library(rafalib)
popDiff <- mean(bwt.nonsmoke)-mean(bwt.smoke)
nsSd <- popsd(bwt.nonsmoke)
sSd <- popsd(bwt.smoke)

N <- 90
B <- 10000
alpha <- 0.01
set.seed(1)
rejOrAc <- function(N, bwt.nonsmoke, bwt.smoke, alpha) {
  t.test(sample(bwt.nonsmoke, N), sample(bwt.smoke, N))$p.value < alpha
}

avg <- replicate(B, rejOrAc(N, bwt.nonsmoke, bwt.smoke, alpha))
print(mean(avg))