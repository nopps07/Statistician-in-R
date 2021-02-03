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


# Define a vector of 1000 p values    
p_grid <- seq(from = 0, to = 1, length.out = 1000)

# Simulate 1 poll result for each p in p_grid   
poll_result <- rbinom(1000, 10, p_grid)

# Create likelihood_sim data frame
likelihood_sim <- data.frame(p_grid, poll_result)    

# Density plots of p_grid grouped by poll_result
ggplot(likelihood_sim, aes(x = p_grid, y = poll_result, group = poll_result)) + 
  geom_density_ridges()

# Density plots of p_grid grouped by poll_result
ggplot(likelihood_sim, aes(x = p_grid, y = poll_result, group = poll_result, fill = poll_result == 6)) + 
  geom_density_ridges()



# DEFINE the model
vote_model <- "model{
    # Likelihood model for X
    X ~ dbin(p, n)
    
    # Prior model for p
    p ~ dbeta(a ,b)
}"

# COMPILE the model    
vote_jags <- jags.model(textConnection(vote_model), 
                        data = list(a = 45, b = 55, X = 6, n = 10),
                        inits = list(.RNG.name = "base::Wichmann-Hill", .RNG.seed = 100))

# SIMULATE the posterior
vote_sim <- coda.samples(model = vote_jags, variable.names = c("p"), n.iter = 10000)

# PLOT the posterior
plot(vote_sim, trace = FALSE)

# COMPILE the model    
vote_jags1 <- jags.model(textConnection(vote_model), 
                        data = list(a = 1, b = 1, X = 6, n = 10),
                        inits = list(.RNG.name = "base::Wichmann-Hill", .RNG.seed = 100))

vote_jags2 <- jags.model(textConnection(vote_model),
                         data = list(a = 1, b = 1, X = 220, n = 400),
                         inits = list(.RNG.name = "base::Wichmann-Hill", .RNG.seed = 100))
                        
vote_jags3 <- jags.model(textConnection(vote_model),
                         data = list(a = 45, b = 55, X = 220, n = 400),
                         inits = list(.RNG.name = "base::Wichmann-Hill", .RNG.seed = 100))

# SIMULATE the posterior
vote_sim <- coda.samples(model = vote_jags, variable.names = c("p"), n.iter = 10000)

# PLOT the posterior
plot(vote_sim, trace = FALSE, xlim = c(0,1), ylim = c(0,18))