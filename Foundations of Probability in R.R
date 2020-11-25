#Ch1 Binomial distribution

# Generate 10 separate random flips with probability .3
rbinom(10, 1, .3)

# Generate 100 occurrences of flipping 10 coins, each with 30% probability
rbinom(100, 10, 0.3)


# Calculate the probability that 2 are heads using dbinom
dbinom(2, 10, .3)

# Confirm your answer with a simulation using rbinom
mean(rbinom(10000, 10, .3) == 2)

# Calculate the probability that at least five coins are heads
1 - pbinom(4, 10, .3)

# Confirm your answer with a simulation of 10,000 trials
mean(rbinom(10000, 10, .3) >= 5)
# Here is how you computed the answer in the last problem
mean(rbinom(10000, 10, .3) >= 5)

# Try now with 100, 1000, 10,000, and 100,000 trials
mean(rbinom(100, 10, .3) >= 5)
mean(rbinom(1000, 10, .3) >= 5)
mean(rbinom(10000, 10, .3) >= 5)
mean(rbinom(100000, 10, .3) >= 5)


# Calculate the expected value using the exact formula
25 * .3

# Confirm with a simulation using rbinom
mean(rbinom(10000, 25, .3))


# Calculate the variance using the exact formula
25 * .3 * (1 - .3)

# Confirm with a simulation using rbinom
var(rbinom(10000, 25, .3))


#Ch2
#Probability of Event A and Event B

# Simulate 100,000 flips of a coin with a 40% chance of heads
A <- rbinom(100000, 1, .4)

# Simulate 100,000 flips of a coin with a 20% chance of heads
B <- rbinom(100000, 1, .2)

# Estimate the probability both A and B are heads
mean(A & B)

# Simulate 100,000 flips of a coin with a 40% chance of heads
A <- rbinom(100000, 1, .4)

# Simulate 100,000 flips of a coin with a 20% chance of heads
B <- rbinom(100000, 1, .2)

# Estimate the probability both A and B are heads
mean(A & B)


# Use rbinom to simulate 100,000 draws from each of X and Y
X <- rbinom(100000, 10, .6)
Y <- rbinom(100000, 10, .7)

# Estimate the probability either X or Y is <= to 4
mean(X <= 4 | Y <= 4)

# Use pbinom to calculate the probabilities separately
prob_X_less <- pbinom(4, 10, .6)
prob_Y_less <- pbinom(4, 10, .7)

# Combine these to calculate the exact probability either <= 4
prob_X_less + prob_Y_less - prob_X_less * prob_Y_less


# Simulate 100,000 draws of a binomial with size 20 and p = .1
X <- rbinom(100000, 20, .1)

# Estimate the expected value of X
mean(X)

# Estimate the expected value of 5 * X
mean(5 * X)

# X is simulated from 100,000 draws of a binomial with size 20 and p = .1
X <- rbinom(100000, 20, .1)

# Estimate the variance of X
var(X)

# Estimate the variance of 5 * X
var(5 * X)


# Simulate 100,000 draws of X (size 20, p = .3) and Y (size 40, p = .1)
X <- rbinom(100000, 20, .3) 
Y <- rbinom(100000, 40, .1)

# Estimate the expected value of X + Y
mean(X + Y)


# Simulation from last exercise of 100,000 draws from X and Y
X <- rbinom(100000, 20, .3) 
Y <- rbinom(100000, 40, .1)

# Find the variance of X + Y
var(X + Y)

# Find the variance of 3 * X + Y
var(3 * X + Y)



##Ch3
#Bayesian Statistics
# Simulate 50000 cases of flipping 20 coins from fair and from biased
fair <- rbinom(50000, 20, .5)
biased <- rbinom(50000, 20, .75)

# How many fair cases, and how many biased, led to exactly 11 heads?
fair_11 <- sum(fair == 11)
biased_11 <- sum(biased == 11)

# Find the fraction of fair coins that are 11 out of all coins that were 11
fair_11 / (fair_11 + biased_11)

# Simulate 50000 cases of flipping 20 coins from fair and from biased
fair <- rbinom(50000,20,.5)
biased <- rbinom(50000,20,.75)

# How many fair cases, and how many biased, led to exactly 16 heads?
fair_16 <- sum(fair == 16)
biased_16 <- sum(biased == 16)

# Find the fraction of fair coins that are 16 out of all coins that were 16
fair_16 / (fair_16 + biased_16)

# Simulate 8000 cases of flipping a fair coin, and 2000 of a biased coin
fair_flips <- rbinom(8000, 20, 0.5)
biased_flips <- rbinom(2000, 20, 0.75)

# Find the number of cases from each coin that resulted in 14/20
fair_14 <- sum(fair_flips == 14)
biased_14 <- sum(biased_flips == 14)

# Use these to estimate the posterior probability
fair_14 / (fair_14 + biased_14)



# Use dbinom to calculate the probability of 11/20 heads with fair or biased coin
probability_fair <- dbinom(11, 20, 0.5)
probability_biased <- dbinom(11, 20, 0.75)

# Calculate the posterior probability that the coin is fair
probability_fair / (probability_biased + probability_fair)


# Find the probability that a coin resulting in 14/20 is fair
dbinom(14, 20, .5) / (dbinom(14, 20, .5) + dbinom(14, 20, .75))

# Find the probability that a coin resulting in 18/20 is fair
dbinom(18, 20, .5) / (dbinom(18, 20, .5) + dbinom(18, 20, .75))


# Use dbinom to find the probability of 16/20 from a fair or biased coin
probability_16_fair <- dbinom(16, 20, 0.5)
probability_16_biased <- dbinom(16, 20, 0.75)

# Use Bayes' theorem to find the posterior probability that the coin is fair
probability_16_fair * 0.99 / (probability_16_fair * 0.99 + probability_16_biased * 0.01)




##Ch4
#Normal distribution
# Draw a random sample of 100,000 from the Binomial(1000, .2) distribution
binom_sample <- rbinom(100000, 1000, .2)

# Draw a random sample of 100,000 from the normal approximation
normal_sample <- rnorm(100000, 200, sqrt(160))

# Compare the two distributions with the compare_histograms function
compare_histograms(binom_sample, normal_sample)


# Draw a random sample of 100,000 from the Binomial(10, .2) distribution
binom_sample <- rbinom(100000, 10, .2)

# Draw a random sample of 100,000 from the normal approximation
normal_sample <- rnorm(100000, 2, sqrt(1.6))

# Compare the two distributions with the compare_histograms function
compare_histograms(binom_sample, normal_sample)


#The poisson distibution
# Draw a random sample of 100,000 from the Binomial(1000, .002) distribution
binom_sample <- rbinom(100000, 1000, .002)

# Draw a random sample of 100,000 from the Poisson approximation
poisson_sample <- rpois(100000, 2)

# Compare the two distributions with the compare_histograms function
compare_histograms(binom_sample, poisson_sample)

# Simulate 100,000 draws from Poisson(2)
poisson_sample <- rpois(100000, 2)

# Find the percentage of simulated values that are 0
mean(poisson_sample == 0)

# Use dpois to find the exact probability that a draw is 0
dpois(0, 2)

# Simulate 100,000 draws from Poisson(1)
X <- rpois(100000, 1)

# Simulate 100,000 draws from Poisson(2)
Y <- rpois(100000, 2)

# Add X and Y together to create Z
Z <- X + Y

# Use compare_histograms to compare Z to the Poisson(3)
compare_histograms(Z, rpois(100000, 3))


#The geometric distribution
# Simulate 100 instances of flipping a 20% coin
flips <- rbinom(100, 1, .2)

# Use which to find the first case of 1 ("heads")
which(flips == 1)[1]


# Existing code for finding the first instance of heads
which(rbinom(100, 1, 0.2) == 1)[1]

# Replicate this 100,000 times using replicate()
replications <- replicate(100000, which(rbinom(100, 1, 0.2) == 1)[1])

# Histogram the replications with qplot
qplot(replications)


# Replications from the last exercise
replications <- replicate(100000, which(rbinom(100, 1, .2) == 1)[1])

# Generate 100,000 draws from the corresponding geometric distribution
geom_sample <- rgeom(100000, .2)

# Compare the two distributions with compare_histograms
compare_histograms(replications, geom_sample)


# Find the probability the machine breaks on 5th day or earlier
pgeom(4, .1)

# Find the probability the machine is still working on 20th day
1 - pgeom(19, .1)



# Calculate the probability of machine working on day 1-30
still_working <- 1 - pgeom(0:29, .1)

# Plot the probability for days 1 to 30
qplot(1:30, still_working)

