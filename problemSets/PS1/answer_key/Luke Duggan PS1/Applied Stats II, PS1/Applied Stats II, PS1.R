### APPLIED STATS: PS1
### LUKE DUGGAN

#####################################################
### QUESTION ONE

# Firstly we create the data.

set.seed(123)
data <- rcauchy(1000, location=0, scale=1)
data <- sort(data)

# We want to use the Kolmogorov-Smirnov test to test the null hypothesis that
# our sample comes from a normal distribution (of course, we know that this
# hypothesis is in fact false).

# We create the empirical CDF and the test statistic.

empirical_cdf <- ecdf(data)

D <- max(abs(empirical_cdf(data) - pnorm(data)))
D

# As we see, the test statistic takes a value of roughly 0.135. This is roughly
# equal to the value provided by R's built in Kolmogorov-Smirnov function, 
# although not exactly.I'm not sure why this is.

ks.test(data, pnorm)$statistic

# Next, we write the Kolmogorov-Smirnov CDF. x is the argument to the function;
# n is the summand. 

KS_CDF <- function(x, n) {
  
  sum <- 0
  
  for (k in (1:n)) {
    sum <- sum + (sqrt(2*pi)/(x))*exp(-(pi^2)*((2*k-1)^2)*(8*(x)^2)^-1)
  }
  
  return(sum)
}

# Unfortunately, this function doesn't seem to return the right input for either
# of the values of the test statistic we've seen.

KS_CDF(0.1347281,100)
KS_CDF(0.1357281, 100)
ks.test(data, pnorm)

# I really don't know why the output is wrong: having checked small values of n
# with a scientific calculator, I don't think the function is written incorrectly.

# Finally, on the question of approximation. The above function requires the user
# to supply the summand n as an argument. One could instead use a convergence 
# condition, e.g., if one wanted an approximation to the 3rd decimal place,
# one would instruct the function to halt upon reaching a summand n such that
# KS_CDF(x, n) and KS_CDF(x, n+1) agree in their third or fourth decimal place,
# since (because a series of positive terms has increasing partial sums) all
# subsequent values of n will agree in this way too.

#########################################################
### QUESTION TWO

# First we generate the data.

set.seed(123)

data <- data.frame(x = runif(200, 1, 10))
data$y <- 2.75*data$x + rnorm(200, 0, 1.5)

head(data)

# Next, we estimate a linear model by OLS. Note: in the true data-generating process,
# there is no intercept. The question doesn't say whether or not to include an intercept
# in the model: I chose to do so (the estimate turns out not to be statistically significantly different from 0).

OLS <- lm(data$y ~ data$x, data = data)

summary(OLS)
OLS$coefficients

# We now estimate the same linear model: however, instead of using OLS, we use
# maximum likelihood, and the "BFGS" variant of the Newton-Raphson numerical method
# to approximate our estimates.

# Firstly, we have to create the function to be optimized: namely, the log-likelihood function for a normal distribution.

linear.lik <- function(theta, y, X) {
  n = nrow(X)
  k = ncol(X)
  
  beta = theta[1:k]
  sigma2 = (theta[k+1])^2
  
  e = y - X%*%beta
  
  logli <- -(0.5)*n*log(2*pi) - (0.5)*n*log(sigma2) - (t(e)%*%e)/(2*sigma2)
  
  return(-logli)
}

# We arbitrarily choose the initialization values to be (1,1,1).

linear.MLE <- optim(fn = linear.lik, par = c(1,1,1), hessian = TRUE, y = data$y, 
                    X = cbind(1, data$x), method = "BFGS")

linear.MLE$par

# The estimated coefficients are almost exactly identical to OLS. For some reason,
# the estimated population variance is negative, which makes no sense: 
# however, its absolute value is pretty close to the true parameter 1.5.
