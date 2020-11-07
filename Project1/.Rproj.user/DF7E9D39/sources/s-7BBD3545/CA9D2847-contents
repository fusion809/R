p <- 1/6
q <- 5/6
n <- 10000
set.seed(1)
test_with_seed <- function(p, q, n) {
  (mean(sample(1:6, n, replace=TRUE)==6)
    -p)/sqrt(p*q/n)
}
avgs <- replicate(n, test_with_seed(p, q, n))

