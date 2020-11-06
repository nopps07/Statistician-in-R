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