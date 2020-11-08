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
dt <- 1e-1
t <- c(t0)

# Initialize solution vectors
theta <- c(theta0)
thetaDot <- c(thetaDot0)
i <- 1

# Solution parameters
epsilon <- 1e-11

# Problem function
simpPen <- function(g, l, t, theta, thetaDot, dt) {
  thetaDDot <- (-g/l)*cos(theta)
  diffAr <- c(dt*thetaDot, dt*thetaDDot)
  return(diffAr)
}

# Loop over time until theta and theta dot for final t is found
while (t[i] < tf) {
  dt <- min(dt, tf-t[i])

  # RK45 approximators for next dependent variable values
  K1 <- simpPen(g, l, t[i], theta[i], thetaDot[i], dt)
  k1 <- K1[1]
  l1 <- K1[2]
  
  K2 <- simpPen(g, l, t[i] + dt/4, theta[i] + k1/4, thetaDot[i] + l1/4, dt)
  k2 <- K2[1]
  l2 <- K2[2]
  
  K3 <- simpPen(g, l, t[i] + 3*dt/8, theta[i] + 3*k1/32 + 9*k2/32, 
                thetaDot[i] + 3*l1/32 + 9*l2/32, dt)
  k3 <- K3[1]
  l3 <- K3[2]

  K4 <- simpPen(g, l, t[i] + 12*dt/13, theta[i] + 1932*k1/2197 - 7200*k2/2197 +
                 7296*k3/2197, thetaDot[i] + 1932*l1/2197 - 7200*l2/2197 +
                 7296*l3/2197, dt)
  k4 <- K4[1]
  l4 <- K4[2]
  
  K5 <- simpPen(g, l, t[i] + dt, theta[i] + 439*k1/216 - 8*k2 + 3680*k3/513
                - 845*k4/4104, thetaDot[i] + 439*l1/216-8*l2 + 3680*l3/513
               - 845*l4/4104, dt)
  k5 <- K5[1]
  l5 <- K5[2]
  
  K6 <- simpPen(g, l, t[i]+dt/2, theta[i] - 8*k1/27 + 2*k2 - 3544*k3/2565 + 
                  1859*k4/4104 - 11*k5/40, thetaDot[i] - 8*l1/27 + 2*l2 - 
                 3544*l3/2565 + 1859*l4/4104 - 11*l5/40, dt)
  k6 <- K6[1]
  l6 <- K6[2]

  # 4th order approximations for the next theta and theta dot values  
  theta1 <- theta[i] + 25*k1/216 + 1408*k3/2565 + 2197*k4/4104 - k5/5
  thetaDot1 <- thetaDot[i] + 25*l1/216 + 1408*l3/2565 + 2197*l4/4104 - l5/5;

  # 5th order approximations for the next theta and theta dot values
  theta2 <- theta[i] + 16*k1/135 + 6656*k3/12825 + 28561*k4/56430 - 9*k5/50 + 
    2*k6/55;
  thetaDot2 <- thetaDot[i] + 16*l1/135 + 6656*l3/12825 + 28561*l4/56430 - 
    9*l5/50+2*l6/55;
  
  # Error estimate
  RTheta <- abs(theta1-theta2)/dt
  RThetaDot <- abs(thetaDot1-thetaDot2)/dt
  R <- max(RTheta, RThetaDot)

  # Step size correction
  s <- (epsilon/(2*R))^(0.25)

  # If error is less than the specified error tolerance, move on
  if (R <= epsilon) {
    t <- append(t, t[i]+dt)
    theta <- append(theta, theta1)
    thetaDot <- append(thetaDot, thetaDot1)
    i <- i+1
  }
  dt <- dt * s
}

# Plot solution
mypar(1,3)
plot(t, theta)
plot(t, thetaDot)
plot(theta, thetaDot)