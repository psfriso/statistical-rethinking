library('rethinking')
data("Howell1")
d = Howell1

hist(d$height)
plot( d$height, d$age)

d2 = d[ d$age >= 18.0 , ]
plot( d2$height, d2$age)
str(d2)
# The Model
curve( dnorm(x,178,20) , from = 100, to = 250) # mean ~ Normal( 178, 20)
curve( dunif(x, 0 , 50) , from = -10 , to = 60) # sd  ~ Uniform( 0 , 50)

sample_mu = rnorm(1e4, 178, 20)
sample_sigma = runif( 1e4, 0, 50)
prior_h = rnorm( 1e4, sample_mu, sample_sigma)
dens(prior_h)

mu.list = seq( from = 140 , to = 160, length.out = 200)
sigma.list = seq( from = 4, to = 9, length.out = 200 )
post = expand.grid( mu = mu.list, sigma = sigma.list)

post$LL = sapply( 1:nrow(post),
                  function(i) sum( dnorm(
                   d2$height, 
                   mean = post$mu[i],
                   sd = post$sigma[i],
                   log = TRUE
                  )))

post$prod = post$LL + dnorm( post$mu, 178, 20, log = TRUE) +
                      dunif( post$sigma, 0, 50, log = TRUE)

post$prob = exp( post$prod - max(post$prod) )

# 3D plotting
image_xyz( post$mu, post$sigma, post$prob)

sample.rows = sample( 1:nrow(post), size = 1e4, replace = TRUE, prob= post$prob )
sample.mu = post$mu[ sample.rows ]
sample.sigma = post$sigma[ sample.rows ]
plot( sample.mu, sample.sigma, cex=0.5 , pch=16 , col=col.alpha(rangi2, 0.1) )

dens(sample.mu)
dens(sample.sigma)
post[28339,]

HPDI( sample.mu )
HPDI( sample.sigma )

# Fitting a model with map (quadratic approximation)
# We ll leave aside the grid approximation. We will use map to find the values of sigma and mu
# that maximize the posterior probability

flist = alist(
  height ~ dnorm( mu, sigma),
  mu ~ dnorm( 178, 20),
  sigma ~ dunif( 0, 50)
)

# Lets fit the model to the data in d2

m4.1 = map( flist, data=d2 )
precis( m4.1)

# covariance matrix of the parameters in the model (sigma, mu)
vcov( m4.1 )
# Variance and correlation
diag( vcov( m4.1) )
cov2cor( vcov(m4.1) )

# Adding a predictor

plot(d2$height ~ d2$weight)

# All steps to build a bayesian regression model

data("Howell1")
d = Howell1
d2 = d[ d$age >= 18 ,]

# fit the model
m4.3 = map(
  alist(
    height ~ dnorm( mu, sigma) ,
    mu <- a + b * weight ,
    a ~ dnorm( 156, 100 ),
    b ~ dnorm( 0, 10 ),
    sigma ~ dunif( 0, 50)
  ),
  data = d2
)

precis(m4.3)
precis(m4.3, corr = TRUE )
# Basic plot of the model
plot( height ~ weight, data = d2 , col = 'blue')
abline( a = coef(m4.3)["a"] , b = coef(m4.3)["b"] )
# Extract samples from the fitted model
post = extract.samples( m4.3 )
post[1:5,]

# Let's plot 20 lines to see the uncertainty in the model
post = extract.samples( m4.3, n = 20)
plot( height ~ weight, data = d2 ,
      col = rangi2, xlim = range(d2$weight) , ylim = range(d2$height),
      xlab = 'weight', ylab = 'height')
mtext(concat("N = ", length(d2$height)))

# Plot the lines
for (i in 1:20){
  abline( a = post$a[i] , b = post$b[i], col = col.alpha("black", 0.3))
}
# Sampling the posterior at weights = 50
post = extract.samples( m4.3 )
mu_at_50 = post$a + post$b *50
mu_at_50[1:5]
dens( mu_at_50, col = rangi2, lwd =2, xlab = "mu |weight = 50")
HPDI( mu_at_50, prob = 0.9)

# Repeat the same but for every value of the weight
weight.seq = seq( from = 25 , to = 70, by = 1)
mu = link( m4.3 , data = data.frame( weight = weight.seq) )

# use type = n to hide raw data
plot( height ~ weight , d2, type = 'n')

for ( i in 1:100 ){
  points( weight.seq , mu[i,] , pch = 16, col = col.alpha( rangi2, 0.2) )
}

# summarize the distribution of mu
mu.mean = apply( mu, 2, mean) # 2 stands for columns
mu.HPDI = apply( mu, 2, HPDI, prob = 0.9)

# Data, model and uncertainty of the model
plot( height ~ weight, data = d2, col = col.alpha(rangi2, 0.5) )
lines( weight.seq, mu.mean)
shade( mu.HPDI, weight.seq)

# Exercises
# 4M1
trials = 1e4
mu.priors    = rnorm(trials, mean = 0, sd = 10)
sigma.priors = runif( trials, min = 0, max = 10)
simulated.height.from.priors = rnorm( n = trials, mean = mu.priors, sd = sigma.priors)
dens(simulated.height.from.priors)

#4M2: translate the previous model to a map formula

map.model = alist(
  height ~ dnorm( mu, sigma), 
  mu ~ dnorm(0 ,10),
  sigm ~ dunif( 0, 10)
)

#4H1
data("Howell1")
d = Howell1
d2 = d[ d$age >= 18 ,]

d2$scaled.w =  (d2$weight - mean(d2$weight) )/ sd(d2$weight)

# fit the model
model = map(
  alist(
    height ~ dnorm( mu, sigma) ,
    mu <- a + b * scaled.w ,
    a ~ dnorm( 156, 100 ),
    b ~ dnorm( 0, 10 ),
    sigma ~ dunif( 0, 50)
  ),
  data = d2
)

# simulate predicted heights from the above model
ind.weights = c(46.95, 43.72, 64.78, 32.59, 54.63)
ind.weights.centred = (ind.weights - mean(d2$weight) ) / sd(d2$weight)
predicted.heights <- sim(model, data = list(scaled.w = ind.weights.centred) )

# summarize the results
predicted.height.means = apply( predicted.heights, MARGIN = 2, FUN = mean)
predicted.height.intervals = apply( predicted.heights, MARGIN = 2, FUN = HPDI, prob = 0.89 )

cbind(ind.weights, predicted.height.means, 
      predicted.height.intervals[1,],predicted.height.intervals[2,] )

#4H2
d4 = d[ d$age < 18 ,]
dim(d4)

d4$weight.scaled = ( d4$weight - mean(d4$weight) )/sd(d4$weight)

model = map(
  alist(
    height ~ dnorm( mu, sigma ),
    mu <-  a + b * weight.scaled,
    a ~ dnorm( 100, 50  ),
    b ~ dnorm( 0, 10 ),
    sigma ~ dunif(0,50)
  ), 
  data = d4
)

precis( model )

( 10 * coef(model)['b'] ) /  sd(d4$weight) # how much taller for 10 units of weight extra

# b
library(MASS)
trials <- 1e5

# too much going on. re-fit the model without scaling for simplicity.
model = map(
  alist(
    height ~ dnorm( mu, sigma ),
    mu <-  a + b * weight,
    a ~ dnorm( 100, 50  ),
    b ~ dnorm( 0, 10 ),
    sigma ~ dunif(0,50)
  ), 
  data = d4
)

precis(model)
range(d4$weight)
weight.seq = seq( from = 1, to = 45, by = 1)

# simulate mu then compute mean and hpdi
posterior.samples <- data.frame( mvrnorm(n = trials, mu = coef(model), Sigma = vcov(model)) )
mu.link <- function(weight) posterior.samples$a + posterior.samples$b * weight
mu <- sapply(X = weight.seq, FUN = mu.link)
mu.mean <- apply(X = mu, MARGIN = 2, FUN = mean)
mu.hpdi <- apply(X = mu, MARGIN = 2, FUN = HPDI, prob = .89)

# simulate heights then compute hpdi
height.link <- function(weight) rnorm(n = nrow(posterior.samples), mean = mu.link(weight), sd = posterior.samples$sigma)
height.samples <- sapply(X = weight.seq, FUN = height.link)
height.hpdi <- apply(X = height.samples, MARGIN = 2, FUN = HPDI, prob = .89)

# plot results
plot(height ~ weight, data = d4, col = col.alpha(rangi2, .5))
lines(x = weight.seq, y = mu.mean)
shade(object = mu.hpdi, lim = weight.seq)
shade(object = height.hpdi, lim = weight.seq)

#4H3 -using log(weight) instead of weight to in the height model

log.model = map(
  alist(
    height ~ dnorm( mu, sigma ),
    mu <-  a + b * log(weight),
    a ~ dnorm( 178, 100  ),
    b ~ dnorm( 0, 100 ),
    sigma ~ dunif(0,50)
  ), 
  data = d4
)

precis(log.model)

# build the posterior fitted log model
posterior.samples <- data.frame( mvrnorm(n = trials, 
                                         mu = coef(log.model), 
                                         Sigma = vcov(log.model)) )
# function to simulate height MEANS from log model
mu.link <- function(weight) posterior.samples$a + posterior.samples$b * weight
# simulated heights model
mu <- sapply(X = log(weight.seq), FUN = mu.link) # aqui
# mean and intervals for the simulated heights MEANS!!
mu.mean <- apply(X = mu, MARGIN = 2, FUN = mean)
mu.hpdi <- apply(X = mu, MARGIN = 2, FUN = HPDI, prob = .89)

# simulated heights
height.link <- function(weight) rnorm(n = nrow(posterior.samples), 
                                mean = mu.link(weight), sd = posterior.samples$sigma)
height.samples <- sapply(X = log(weight.seq), FUN = height.link)
height.hpdi <- apply(X = height.samples, MARGIN = 2, FUN = HPDI, prob = .89)

# plot results
plot(height ~ weight, data = d4, col = col.alpha(rangi2, .5))
lines(x = weight.seq, y = mu.mean)
shade(object = mu.hpdi, lim = weight.seq )
shade(object = height.hpdi, lim = weight.seq )
