library('rethinking')
# Sampling from a grid-approximate posterior
p_grid = seq( from = 0, to = 1, length.out = 1000 )
prior = rep(1, 1000)
likelihood = dbinom(6, size = 9, prob = p_grid)
posterior = likelihood * prior
posterior = posterior / sum( posterior )
plot(posterior, type = 'l')
# extract samples from the posterior distribution
samples = sample( p_grid, prob = posterior , size = 1e4, replace = TRUE)
#plot(samples)
dens(samples)
# which is the probability of water proportion in tosses < 0.5?
sum( posterior[ p_grid < 0.5] )
abline( v = 0.5)
sum( samples < 0.5 ) / length(samples)
quantile( samples, 0.8)
quantile( samples, c(0.1, 0.9) )
HPDI( samples , p = 0.6)
aux = HPDI( samples , p = 0.6)
lmin =aux[[1]]
lmax = aux[[2]]
# in this area lays the 60% interval with maximum post probability
dens(samples)
abline( v = lmin)
abline( v = lmax)
#
# Exercises
# 3E1
p_grid = seq( from = 0, to = 1, length.out = 1000)
prior( 1, 1000)
likelihood = dbinom( 6, size = 9, prob = p_grid)
plot(likelihood, type = 'l')
posterior = likelihood * prior
posterior = posterior / sum(posterior)
plot(posterior, type = 'l')
# sample the posterior distribution
set.seed(100)
samples = sample( p_grid, prob = posterior, size = 1e4, replace = TRUE)
dens(samples)
# how much posterior distribution lies below p = 0.2?
sum( posterior[ p_grid < 0.2] ) # 0.0008560
sum( samples < 0.2 )/ length(samples)
# posterior probability above 0.8
sum( posterior[ p_grid > 0.8] ) # 0.12
sum( samples > 0.8 )/ length(samples)
# posterior probability between 0.2 and 0.8
1 - sum( posterior[ p_grid < 0.2] ) - sum( posterior[ p_grid > 0.8] ) 
sum(samples > 0.2 & samples < 0.8) / length(samples)
# which value of p leaves below 20% of posterior probability
quantile(samples, 0.2)
# checking
sum( samples < 0.519519 )/ length(samples)
# which value of p leaves above 20% of the posterior probability
quantile(samples, 0.8)
sum( samples < 0.7567568 )/ length(samples)
# which values of p contain the narrowest interaval equal to 66% of the posterior prob
HPDI( samples, prob = 0.66)
dens(samples)
abline( v=HPDI( samples, prob = 0.66) )
# which values of p contatin the 66% of posterior probability 
# (assuming equal posterior probabity both below and above the interval)
PI( samples, prob = 0.66)
#
# Medium
# 3M1
p_grid = seq( from = 0, to = 1, length.out = 2000 )
prior = rep(1, 2000)
likelihood = dbinom( 8 , size = 15 , prob=p_grid)
posterior = likelihood * prior
posterior = posterior/sum(posterior)
plot(posterior~p_prid, type = 'l')
# 3M2 
samples = sample( p_grid, size = 1e4, prob = posterior, replace = TRUE)
HPDI( samples = samples, p = 0.9)
plot(posterior~p_prid, type = 'l')
abline( v = HPDI( samples = samples, p = 0.9) )
# 3M3
# Posterior predictive check for this model and data. Simulate the distribution of samples, 
# averaging over the posterior uncertainty
predictive.distribution = rbinom( n = 1e4, size = 15, prob = samples)
# What is the probability of observing 8 waters in 15 tosses?
mean( predictive.distribution == 8 )
# 3M4 
# This would be how is to observe a 6 Waters to 9 tosess ratio under the current model/data 
pexp = 6/9
n = 15 * pexp
mean( predictive.distribution == n )
# Using the posterior distribution constructed (3M3): Probability of observing 6 waters
# in 9 tosses?
# or
predictive.distribution = rbinom( n = 1e4, size = 9, prob = samples)
mean( predictive.distribution == 6 )

# 3M5
# new prior
w = 8
tosses = 15
p_grid = seq( from = 0, to = 1, length.out = 1000 )
prior = ifelse( p_grid < 0.5, 0, 1)
likelihood = dbinom( w , size = tosses , prob=p_grid)
posterior = likelihood * prior
posterior = posterior/sum(posterior)
plot(posterior~p_grid, type = 'l')
#
# 3M5 - 3M2 
samples = sample( p_grid, size = 1e4, prob = posterior, replace = TRUE)
HPDI( samples = samples, p = 0.9)
# 0.5005005 0.7097097 # Before 0.3326663 0.7203602 
# 3M5 - 3M3
predictive.distribution = rbinom( n = 1e4, size = 15, prob = samples)
mean( predictive.distribution == 8 )
#
# Hard
# 3H1
data(homeworkch3)
cor(birth1 , birth2)
# total number of boys
sum( c(birth1,birth2) )
# Using grid approximation compute the posterior distribution of a birth being a boy
n.boys = sum( c(birth1,birth2) )
n.births = length(birth1) + length(birth2) 
p_grid = seq(from = 0, to =1, length.out = 1000)
prior = rep(1, length(p_grid))
likelihood = dbinom( n.boys, size = n.births , prob = p_grid)
posterior = prior * likelihood
posterior = posterior / sum(posterior)
plot(posterior ~ p_grid, type = 'l')
# Which value (of probability - p_grid) maximizes the posterior distribution?
p_grid[ which.max(posterior) ]
# Estimate higher posterior densities
samples = sample(p_grid , size = 1e4, prob = posterior , replace = TRUE)
str(samples)
HPDI(samples =samples , prob = c(0.5, 0.89, 0.97) ) 
# 3H3
predictive.distribution = rbinom( 1e4, size =n.births , prob = samples)
str( predictive.distribution )
dens(predictive.distribution)
abline(v=n.boys, col = 'red')

## 3H4
predictive.distribution = rbinom( 1e4, size =100, prob = samples)
dens(predictive.distribution)
abline(v=sum(birth1), col = 'red')

## 3H5
# so far we assumed that births are independent.
# we will check this assumtpion
# 
boys.after.girl = birth2[ birth1 == 0 ]
predictive.distribution = rbinom(n = 1e4, size = length(boys.after.girl), prob = samples)
dens(predictive.distribution)
abline( v = sum(boys.after.girl) )
