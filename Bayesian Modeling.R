#Bayesian modeling with RJAGS

prior_A <- rbeta(n = 10000, shape1 = 45, shape2 = 55)

prior_sim <- data.frame(prior_A)

ggplot(prior_sim, aes(x = prior_A)) +
  geom_density()

#compare
prior_B <- rbeta(n = 10000, shape1 = 1, shape2 = 1)

prior_C <- rbeta(n = 10000, shape1 = 100, shape2 = 100)

prior_sim <- data.frame(samples = c(prior_A, prior_B, prior_C),
                        priors = rep(c("A", "B", "C"), each = 10000))

ggplot(prior_sim, aes(x = samples, fill = priors)) +
  geom_density(alpha = 0.5)

#NOTES
#Prior B reflects 'vague' prior information about p - it gives equal prior weight to all values of p between 0 and 1. Prior C reflects more prior certainty about p - it has less spread and is centered around a mean that's greater than that for Prior A.


