#Bayesian modeling with RJAGS

######################
##########CH1#########
######################

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



# Take 10000 samples from the m prior
prior_m <- rnorm(10000, 50, 25)

# Take 10000 samples from the s prior    
prior_s <- runif(10000, 0, 200)

# Store samples in a data frame
samples <- data.frame(prior_m, prior_s)

# Density plots of the prior_m & prior_s samples    
ggplot(samples, aes(x = prior_m)) + 
  geom_density()
ggplot(samples, aes(x = prior_s)) + 
  geom_density()



# Check out the first 6 rows of sleep_study
head(sleep_study)

# Define diff_3
sleep_study <- sleep_study %>% 
  mutate(diff_3 = day_3 - day_0)

# Histogram of diff_3    
ggplot(sleep_study, aes(x = diff_3)) + 
  geom_histogram(binwidth = 20, color = "white")

# Mean and standard deviation of diff_3
sleep_study %>% 
  summarize(mean(diff_3), sd(diff_3))

# DEFINE the model    
sleep_model <- "model{
    # Likelihood model for Y[i]
    for(i in 1:length(Y)) {
        Y[i] ~ dnorm(m, s^(-2))
    }

    # Prior models for m and s
    m ~ dnorm(50, 25^(-2))
    s ~ dunif(0, 200)
}"

# COMPILE the model
sleep_jags <- jags.model(
  textConnection(sleep_model),
  data = list(Y = sleep_study$diff_3),
  inits = list(.RNG.name = "base::Wichmann-Hill", .RNG.seed = 1989)
)

# SIMULATE the posterior    
sleep_sim <- coda.samples(model = sleep_jags, variable.names = c("m", "s"), n.iter = 10000)

# PLOT the posterior    
plot(sleep_sim, trace = FALSE)




# Check out the head of sleep_sim
head(sleep_sim)

# Store the chains in a data frame
sleep_chains <- data.frame(sleep_sim[[1]], iter = 1:10000)

# Check out the head of sleep_chains
head(sleep_chains)

# Use plot() to construct trace plots of the m and s chains
plot(sleep_sim, density = FALSE)

# Use ggplot() to construct a trace plot of the m chain
ggplot(sleep_chains, aes(x = iter, y = m)) + 
  geom_line()

# Trace plot the first 100 iterations of the m chain
ggplot(sleep_chains[1:100,], aes(x = iter, y = m)) + 
  geom_line()

# Use plot() to construct density plots of the m and s chains
plot(sleep_sim, trace = FALSE)

# Use ggplot() to construct a density plot of the m chain
ggplot(sleep_chains, aes(x = m)) + 
  geom_density()


# COMPILE the model
sleep_jags_multi <- jags.model(textConnection(sleep_model), data = list(Y = sleep_study$diff_3), n.chains = 4)   

# SIMULATE the posterior    
sleep_sim_multi <- coda.samples(model = sleep_jags_multi, variable.names = c("m", "s"), n.iter = 1000)

# Check out the head of sleep_sim_multi
head(sleep_sim_multi)

# Construct trace plots of the m and s chains
plot(sleep_sim_multi, density = FALSE)

# SIMULATE the posterior    
sleep_sim_1 <- coda.samples(model = sleep_jags, variable.names = c("m", "s"), n.iter = 1000)

# Summarize the m and s chains of sleep_sim_1
summary(sleep_sim_1)

# RE-SIMULATE the posterior    
sleep_sim_2 <- coda.samples(model = sleep_jags, variable.names = c("m", "s"), n.iter = 10000)

# Summarize the m and s chains of sleep_sim_2
summary(sleep_sim_2)

# COMPILE the model
sleep_jags <- jags.model(textConnection(sleep_model),
                         data = list(Y = sleep_study$diff_3),
                         init = list(.RNG.name = "base::Wichmann-Hill", .RNG.seed = 1989)) 

# SIMULATE the posterior    
sleep_sim <- coda.samples(model = sleep_jags, variable.names = c("m", "s"), n.iter = 10000)

# Summarize the m and s chains of sleep_sim
summary(sleep_sim)


######################
##########CH3#########
######################

# Take 10000 samples from the a, b, & s priors
a <- rnorm(10000, 0, 200)
b <- rnorm(10000, 1, 0.5)
s <- runif(10000, 0, 20)

# Store samples in a data frame
samples <- data.frame(set = 1:10000, a, b, s)

# Construct density plots of the prior samples    
ggplot(samples, aes(x = a)) + 
  geom_density()
ggplot(samples, aes(x = b)) + 
  geom_density()
ggplot(samples, aes(x = s)) + 
  geom_density()

# Replicate the first 12 parameter sets 50 times each
prior_scenarios_rep <- bind_rows(replicate(n = 50, expr = samples[1:12, ], simplify = FALSE)) 

# Simulate 50 height & weight data points for each parameter set
prior_simulation <- prior_scenarios_rep %>% 
  mutate(height = rnorm(n = 600, mean = 170, sd = 10)) %>% 
  mutate(weight = rnorm(n = 600, mean = a + b * height, sd = s))

# Plot the simulated data & regression model for each parameter set
ggplot(prior_simulation, aes(x = height, y = weight)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE, size = 0.75) + 
  facet_wrap(~ set)

# Construct a scatterplot of wgt vs hgt
ggplot(bdims, aes(x = hgt, y = wgt)) + 
  geom_point()

# Add a model smooth
ggplot(bdims, aes(x = hgt, y = wgt)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE)


weight_model <- "model{
    # Define model for data Y[i]
    for(i in 1:length(Y)) {
      Y[i] ~ dnorm(m[i], s^(-2))
      m[i] <- a + b * X[i]
    }

    # Define the a, b, s priors
    a ~ dnorm(0, 200^(-2))
    b ~ dnorm(1, 0.5^(-2))
    s ~ dunif(0, 20)
}"

# COMPILE the model
weight_jags <- jags.model(
  textConnection(weight_model),
  data = list(Y = bdims$wgt, X = bdims$hgt),
  inits = list(.RNG.name = "base::Wichmann-Hill", .RNG.seed = 1989)
)


# Summarize the posterior Markov chains
summary(weight_sim_big)

# Calculate the estimated posterior mean of b
mean(weight_chains$b)

# Plot the posterior mean regression model
ggplot(bdims, aes(x = hgt, y = wgt)) + 
  geom_point() + 
  geom_abline(intercept = mean(weight_chains$a), slope = mean(weight_chains$b), color = "red")

# Visualize the range of 20 posterior regression models
ggplot(bdims, aes(x = hgt, y = wgt)) + 
  geom_point() +
  geom_abline(intercept = weight_chains$a[1:20], slope = weight_chains$b[1:20], color = "gray", size = 0.25)


# Summarize the posterior Markov chains
summary(weight_sim_big)

# Calculate the 95% posterior credible interval for b
ci_95 <- quantile(weight_chains$b, probs = c(0.025, 0.975))
ci_95

# Calculate the 90% posterior credible interval for b
ci_90 <- quantile(weight_chains$b, probs = c(0.05, 0.95))
ci_90

# Mark the 90% credible interval 
ggplot(weight_chains, aes(x = b)) + 
  geom_density() + 
  geom_vline(xintercept = ci_90, color = "red")

# Mark 1.1 on a posterior density plot for b
ggplot(weight_chains, aes(x = b)) + 
  geom_density() + 
  geom_vline(xintercept = 1.1, color = "red")

# Summarize the number of b chain values that exceed 1.1
table(weight_chains$b > 1.1)

# Calculate the proportion of b chain values that exceed 1.1
mean(weight_chains$b > 1.1)



# Simulate 1 prediction under the first parameter set
rnorm(n = 1, mean = weight_chains$m_180[1], sd = weight_chains$s[1])

# Simulate 1 prediction under the second parameter set
rnorm(n = 1, mean = weight_chains$m_180[2], sd = weight_chains$s[2])

# Simulate & store 1 prediction under each parameter set
weight_chains <- weight_chains  %>% 
  mutate(Y_180 = rnorm(n = 100000, mean = m_180, sd = s))

# Print the first 6 parameter sets & predictions
head(weight_chains)

# Construct a posterior credible interval for the prediction
ci_180 <- quantile(weight_chains$Y_180, probs = c(0.025, 0.975))
ci_180

# Construct a density plot of the posterior predictions
ggplot(weight_chains, aes(x = Y_180)) + 
  geom_density() + 
  geom_vline(xintercept = ci_180, color = "red")

# Visualize the credible interval on a scatterplot of the data
ggplot(bdims, aes(x = hgt, y = wgt)) + 
  geom_point() + 
  geom_abline(intercept = mean(weight_chains$a), slope = mean(weight_chains$b), color = "red") + 
  geom_segment(x = 180, xend = 180, y = ci_180[1], yend = ci_180[2], color = "red")


######################
##########CH4#########
######################
install.packages("crosstalk")
library(mosaic)

# Confirm that weekday is a factor variable
class(RailTrail$weekday)

summary(RailTrail)

# Construct a density plot of volume by weekday
ggplot(RailTrail, aes(x = volume, fill = weekday)) + 
  geom_density(alpha = 0.5)

rail_model_1 <- "model{
  # Likelihood model for Y[i]
  for(i in 1:length(Y)) {
    Y[i] ~ dnorm(m[i], s^(-2))
    m[i] <- a + b[X[i]]
  }
  
  # Prior models for a, b, s
  a ~ dnorm(400, 100^(-2))
  b[1] <- 0
  b[2] ~ dnorm(0, 200^(-2))
  s ~ dunif(0, 200)
}"

# COMPILE the model
rail_jags_1 <- jags.model(
  textConnection(rail_model_1),
  data = list(Y = RailTrail$volume, X = RailTrail$weekday),
  inits = list(.RNG.name = "base::Wichmann-Hill", .RNG.seed = 10)
)

head(RailTrail$volume)

# SIMULATE the posterior    
rail_sim_1 <- coda.samples(model = rail_jags_1, variable.names = c("a", "b", "s"), n.iter = 10000)

# Store the chains in a data frame
rail_chains_1 <- data.frame(rail_sim_1[[1]])

# PLOT the posterior    
plot(rail_sim_1)


# Construct a chain of values for the typical weekday volume
rail_chains_1 <- rail_chains_1 %>% 
  mutate(weekday_mean = a + b.2.)

# Construct a density plot of the weekday chain
ggplot(rail_chains_1, aes(x = weekday_mean)) + 
  geom_density()

# 95% credible interval for typical weekday volume
quantile(rail_chains_1$weekday_mean, c(0.025, 0.975))


# Construct a plot of volume by hightemp & weekday
ggplot(RailTrail, aes(y = volume, x = hightemp, color = weekday)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE)


# DEFINE the model    
rail_model_2 <- "model{
    # Likelihood model for Y[i]
    for(i in 1:length(Y)) {
      Y[i] ~ dnorm(m[i], s^(-2))
	  m[i] <- a + b[X[i]] + c * Z[i]
    }

    # Prior models for a, b, c, s
    a ~ dnorm(0, 200^(-2))
    b[1] <- 0
    b[2] ~ dnorm(0, 200^(-2))
    c ~ dnorm(0, 20^(-2))
    s ~ dunif(0, 200)
}"

# COMPILE the model
rail_jags_2 <- jags.model(textConnection(rail_model_2), 
                          data = list(Y = RailTrail$volume, X = RailTrail$weekday, Z = RailTrail$hightemp), 
                          inits = list(.RNG.name = "base::Wichmann-Hill", .RNG.seed = 10))

# SIMULATE the posterior    
rail_sim_2 <- coda.samples(model = rail_jags_2, variable.names = c("a", "b", "c", "s"), n.iter = 10000)

# Store the chains in a data frame
rail_chains_2 <- data.frame(rail_sim_2[[1]])

# PLOT the posterior    
plot(rail_sim_2)


# Plot the posterior mean regression models
ggplot(RailTrail, aes(y = volume, x = hightemp, color = weekday)) + 
  geom_point() + 
  geom_abline(intercept = mean(rail_chains_2$a), slope = mean(rail_chains_2$c), color = "red") + 
  geom_abline(intercept = mean(rail_chains_2$a) + mean(rail_chains_2$b.2.), slope = mean(rail_chains_2$c), color = "turquoise3")




# DEFINE the model    
poisson_model <- "model{
    for(i in 1:length(Y)) {
        Y[i] ~ dpois(l[i])
        log(l[i]) = a + b[X[i]] + c * Z[i]
    }

    a ~ dnorm(0, 200^(-2))
    b[1] = 0
    b[2] ~ dnorm(0, 2^(-2))
    c ~ dnorm(0, 2^(-2))
    }
    "

# COMPILE the model
poisson_jags <- jags.model(
  textConnection(poisson_model),
  data = list(Y = RailTrail$volume, X = RailTrail$weekday, Z = RailTrail$hightemp),
  inits = list(.RNG.name = "base::Wichmann-Hill", .RNG.seed = 10)
)

# SIMULATE the posterior    
poisson_sim <- coda.samples(model = poisson_jags, variable.names = c("a", "b", "c"), n.iter = 10000)

# Store the chains in a data frame
poisson_chains <- data.frame(poisson_sim[[1]])

# PLOT the posterior    
plot(poisson_sim)


# Plot the posterior mean regression models
ggplot(RailTrail, aes(x = hightemp, y = volume, color = weekday)) + 
  geom_point() + 
  stat_function(fun = function(x){exp(mean(poisson_chains$a) + mean(poisson_chains$c) * x)}, color = "red") + 
  stat_function(fun = function(x){exp(mean(poisson_chains$a) + mean(poisson_chains$b.2.) + mean(poisson_chains$c) * x)}, color = "turquoise3")


# Calculate the typical volume on 80 degree weekends & 80 degree weekdays
poisson_chains <- poisson_chains %>% 
  mutate(l_weekend = exp(a + c * 80)) %>% 
  mutate(l_weekday = exp(a + b.2. + c * 80))

# Construct a 95% CI for typical volume on 80 degree weekend
quantile(poisson_chains$l_weekend, c(0.025, 0.975))

# Construct a 95% CI for typical volume on 80 degree weekday
quantile(poisson_chains$l_weekday, c(0.025, 0.975))

# Simulate weekday predictions under each parameter set
poisson_chains <- poisson_chains %>% 
  mutate(Y_weekday = rpois(n = 10000, lambda = l_weekday))

# Construct a density plot of the posterior weekday predictions
ggplot(poisson_chains, aes(x = Y_weekday)) + 
  geom_density()

# Posterior probability that weekday volume is less 400
mean(poisson_chains$Y_weekday < 400)

