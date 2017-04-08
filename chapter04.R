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

alist