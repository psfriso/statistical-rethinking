
# Likelihood of six W in nine tosses
dbinom(6, size = 9, prob = 0.5)
# 0.1640625. Probability of 6 W (true) in 9 tosess with p=0.5

# posterior distribution of
# 1. Define the grid. Decide how many points to use in estimating the posterior
p_grid = seq( from = 0, to = 1, length.out = 20)
# 2. Compute the value of the prior at each parameter value of the grid
prior = rep(1,20)
# 3. Compute the likelihood at each parameter value
likelihood = dbinom(6, size = 9, prob = p_grid)
# 4. Compute the unstandarized posterior at each parameter value
unstd.posterior = likelihood * prior
# 5. Standarize the posterior.
posterior = unstd.posterior / sum(unstd.posterior)
# plotting the posterior
plot( p_grid, posterior , type = 'b' , xlab = 'probability of water',
      ylab = 'posterior probability')
mtext('20 points')
# Another prior ( expecting values > 0.5)
prior = ifelse(p_grid < 0.5, 0, 1)
# prior maximizing p = 0.5
prior = exp( -5 * abs(p_grid - 0.5) )
plot(prior)
#
# Code to compute the quadratic approximation of the posterior
library(rethinking)
# To use map you provide a formula a list of data and a list of start values for the
# parameter
globe.qa = map(
  alist(
    w ~ dbinom(9,p), # binomial likelihood
    p ~ dunif(0,1) # uniform prior
  ), 
  data = list( w = 6 ) )

precis(globe.qa)

# Exercises
# 2M1
# 1) W W W
p_grid = seq( from = 0, to = 1, length.out = 20)
prior = rep(1,20)
likelihood = dbinom( 3 , size = 3 , prob = p_grid)
unstd.posterior = likelihood * prior
posterior = unstd.posterior / sum(unstd.posterior)
plot( p_grid, posterior , type = 'b' , xlab = 'probability of water',
      ylab = 'posterior probability')
mtext('20 points')
# 2) W W W L
p_grid = seq( from = 0, to = 1, length.out = 20)
prior = rep(1,20)
likelihood = dbinom( 3 , size = 4 , prob = p_grid)
unstd.posterior = likelihood * prior
posterior = unstd.posterior / sum(unstd.posterior)
plot( p_grid, posterior , type = 'b' , xlab = 'probability of water',
      ylab = 'posterior probability')
mtext('20 points')
# 3) L W W L W W W
p_grid = seq( from = 0, to = 1, length.out = 20)
prior = rep(1,20)
likelihood = dbinom( 5 , size = 7 , prob = p_grid)
unstd.posterior = likelihood * prior
posterior = unstd.posterior / sum(unstd.posterior)
plot( p_grid, posterior , type = 'b' , xlab = 'probability of water',
      ylab = 'posterior probability')
mtext('20 points')

# 2M2 Now with a different prior
prior = ifelse( p_grid < 0.5 , 0, 3)
likelihood = dbinom( 5 , size = 7 , prob = p_grid)
unstd.posterior = likelihood * prior
posterior = unstd.posterior / sum(unstd.posterior)
plot( p_grid, posterior , type = 'b' , xlab = 'probability of water',
      ylab = 'posterior probability')
mtext('20 points')

# 2M3
prior = c(0.5, 0.5)
likelihood = c( 0.3, 1)
unstandardized.posterior <- prior * likelihood
posterior <- unstandardized.posterior / sum(unstandardized.posterior)
posterior[1]

#2M4
prior =  rep(1,3)
card.1.likelihood = 1
card.2.likelihood = 0.5
card.3.likelihood = 0
likelihood = c(card.1.likelihood, card.2.likelihood , card.3.likelihood)
unstandardized.posterior <- prior * likelihood
posterior <- unstandardized.posterior / sum(unstandardized.posterior)

# the probability the other size is black is equal to the probability that we've drawn card 1
posterior[1]

#2M5
prior =  rep(1,4)
card.1.likelihood = 1
card.2.likelihood = 0.5
card.3.likelihood = 0
card.4.likelihood = 1
likelihood = c(card.1.likelihood, card.2.likelihood , card.3.likelihood, card.4.likelihood)
unstandardized.posterior <- prior * likelihood
posterior <- unstandardized.posterior / sum(unstandardized.posterior)
posterior
# the probability the other size is black is equal to the probability that we've drawn card 1
# and card 4
posterior[1] + posterior[4]

#2M6
prior =  c(1/3,2/3 ,1)
card.1.likelihood = 1
card.2.likelihood = 0.5
card.3.likelihood = 0
likelihood = c(card.1.likelihood, card.2.likelihood , card.3.likelihood)
unstandardized.posterior <- prior * likelihood
posterior <- unstandardized.posterior / sum(unstandardized.posterior)
# again, the probability the other size is black is equal to the probability that we've drawn card 1
posterior[1]


#2M7
prior =  rep(1,3)
# 1 B/B
# 2 B/W
# 3 W/W
card.1.likelihood = 6
card.2.likelihood = 2
card.3.likelihood = 0

likelihood = c(card.1.likelihood,
               card.2.likelihood,
               card.3.likelihood)

unstandardized.posterior <- prior * likelihood
posterior <- unstandardized.posterior / sum(unstandardized.posterior)

posterior[1]

#2H1
# Both species of pandas equally likely
prior = rep(1,2)
spA.likelihood = 0.1 # twins 10%, we saw twins
spB.likelihood = 0.2 # twins 20%, we saw twins
likelihood = c( spA.likelihood, spB.likelihood  )
unstandardized.posterior <- prior * likelihood
posterior <- unstandardized.posterior / sum(unstandardized.posterior)
posterior[1] * 0.1 + posterior[2] * 0.2  

#2H2
# same posterior than before
posterior[1]

#2H3
# same posterior than before
prior = posterior
spA.likelihood = 1 - 0.1 # twins 10%, we saw twins
spB.likelihood = 1 - 0.2 # twins 20%, we saw twins
likelihood = c( spA.likelihood, spB.likelihood  )
unstandardized.posterior <- prior * likelihood
posterior <- unstandardized.posterior / sum(unstandardized.posterior)
posterior[1]

#2H4
prior = rep(1,2)
spA.likelihood.test = 0.80
spB.likelihood.test = 1 - 0.65 # positive for A
likelihood.test = c( spA.likelihood.test, spB.likelihood.test  )
unstandardized.posterior.test <- prior * likelihood.test
posterior.test <- unstandardized.posterior.test / sum(unstandardized.posterior.test)
posterior.test[1]

# using birth data
# first birth
spA.likelihood = 0.1 # twins 10%, we saw twins
spB.likelihood = 0.2 # twins 20%, we saw twins
likelihood = c( spA.likelihood, spB.likelihood  )
unstandardized.posterior <- prior * likelihood
posterior <- unstandardized.posterior / sum(unstandardized.posterior)
# second birth
prior = posterior
spA.likelihood = 1 - 0.1 # twins 10%, we saw twins
spB.likelihood = 1 - 0.2 # twins 20%, we saw twins
likelihood = c( spA.likelihood, spB.likelihood  )
unstandardized.posterior <- prior * likelihood
posterior <- unstandardized.posterior / sum(unstandardized.posterior)
# now run the test
prior = posterior # this changed
spA.likelihood.test = 0.80
spB.likelihood.test = 1 - 0.65 # positive for A
likelihood.test = c( spA.likelihood.test, spB.likelihood.test  )
unstandardized.posterior.test <- prior * likelihood.test
posterior.test <- unstandardized.posterior.test / sum(unstandardized.posterior.test)
posterior.test[1]
