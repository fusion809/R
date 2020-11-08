library(abind)
# Problem parameters
g <- 9.81
l <- 1.0
theta0 <- 0
thetaDot0 <- 0

# Calculate period using Chebyshev-Gauss quadrature
N <- 1000
n <- 1:N
x <- cos(pi*(2*n-1)/(2*N))
a <- -pi
b <- 0
u <- (b-a)/2 * x + (a+b)/2
val <- (b-a)*pi/(2*N) * sum(sqrt(1-x^2)/sqrt(-2*(g/l)*sin(u)))

# Time values
t0 <- 0
tf <- 8*val
dt <- 3.4e-3
t <- c(t0)

# Initialize solution vectors
theta <- c(theta0)
thetaDot <- c(thetaDot0)
depVars <- c(theta0, thetaDot0)
depVars <- t(depVars)
i <- 1

# Solution parameters
epsilon <- 1e-11

# Problem function
simpPen <- function(g, l, t, depVars, dt) {
  theta <- depVars[1]
  thetaDot <- depVars[2]
  thetaDDot <- (-g/l)*cos(theta)
  diffAr <- c(dt*thetaDot, dt*thetaDDot)
  return(diffAr)
}

# Loop over time until theta and theta dot for final t is found
while (i < 10) {
  dt <- min(dt, tf-t[i])

  # RKF45 approximators for next theta and theta dot values
  K1 <- simpPen(g, l, t[i], depVars[i,], dt)
  # k1 <- K1[1]
  # l1 <- K1[2]
  
  K2 <- simpPen(g, l, t[i] + dt/4, depVars[i,] +  K1/4, dt)
  # k2 <- K2[1]
  # l2 <- K2[2]
  
  K3 <- simpPen(g, l, t[i] + 3*dt/8, depVars[i,] +  3*K1/32 + 9*K2/32, dt)
  # k3 <- K3[1]
  # l3 <- K3[2]

  K4 <- simpPen(g, l, t[i] + 12*dt/13, depVars[i,] +  1932*K1/2197 - 7200*K2/2197 +
                 7296*K3/2197, dt)
  # k4 <- K4[1]
  # l4 <- K4[2]
  
  K5 <- simpPen(g, l, t[i] + dt, depVars[i,] +  439*K1/216 - 8*K2 + 3680*K3/513
                - 845*K4/4104, dt)
  # k5 <- K5[1]
  # l5 <- K5[2]
  
  K6 <- simpPen(g, l, t[i]+dt/2, depVars - 8*K1/27 + 2*K2 - 3544*K3/2565 + 
                  1859*K4/4104 - 11*K5/40, dt)
  # k6 <- K6[1]
  # l6 <- K6[2]
  
  # 4th order approximations to next dependent variable values
  depVars1 <- depVars[i,] +  25*K1/216 + 1408*K3/2565 + 2197*K4/4104 - K5/5

  # 5th order approximations to next dependent variable values
  depVars2 <- depVars[i,] +  16*K1/135 + 6656*K3/12825 + 28561*K4/56430 - 9*K5/50 + 
    2*K6/55;

  # Error approximations
  R <- abs(depVars1-depVars2)/dt
  R <- max(R)
  
  # Step size correction
  s <- (epsilon/(2*R))^(0.25)
  
  # If error estimate is less than or equal to epsilon, move on, otherwise
  # adjust step size and re-perform calculation
  if (R <= epsilon) {
    t <- append(t, t[i]+dt)
    depVars <- rbind(depVars, depVars1)
    i <- i+1
  }
  dt <- dt * s
}

# Generate plot
# par(mfrow=c(1,3))
# plot(t, theta)
# plot(t, thetaDot)
# plot(theta, thetaDot)