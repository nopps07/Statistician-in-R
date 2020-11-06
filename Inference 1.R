install.packages("infer")
install.packages("NHANES")
library(NHANES)
library(ggplot2)
library(plotly)
library(dplyr)
library(broom)
library(readr)
library(infer)

colnames(NHANES)

# Create bar plot for Home Ownership by Gender
ggplot(NHANES, aes(x = Gender, fill = HomeOwn)) + 
  # Set the position to fill
  geom_bar(position = "fill") +
  ylab("Relative frequencies")

# Density plot of SleepHrsNight colored by SleepTrouble
ggplot(NHANES, aes(x = SleepHrsNight, color = SleepTrouble)) + 
  # Adjust by 2
  geom_density(adjust = 2) + 
  # Facet by HealthGen
  facet_wrap(~ HealthGen)

#Note
#If this were your dataset, it would be important to stop here and consider the cause of the missingness. For the course, we will now proceed without worrying about the missing observations.


homes <- NHANES %>%
  # Select Gender and HomeOwn
  select(Gender, HomeOwn) %>%
  # Filter for HomeOwn equal to "Own" or "Rent"
  filter(HomeOwn %in% c("Own", "Rent"))

diff_orig <- homes %>%   
  # Group by gender
  group_by(Gender) %>%
  # Summarize proportion of homeowners
  summarise(prop_own = mean(HomeOwn %in% "Own")) %>%
  # Summarize difference in proportion of homeowners
  summarise(obs_diff_prop = diff(prop_own)) # male - female

# See the result
diff_orig


# Perform 10 permutations
homeown_perm <- homes %>%
  specify(HomeOwn ~ Gender, success = "Own") %>%
  hypothesize(null = "independence") %>% 
  generate(reps = 10, type = "permute") 
# Print results to console
homeown_perm


# Perform 100 permutations
homeown_perm <- homes %>%
  specify(HomeOwn ~ Gender, success = "Own") %>%
  hypothesize(null = "independence") %>% 
  generate(reps = 100, type = "permute") %>% 
  calculate(stat = "diff in props", order = c("male", "female"))

# Dotplot of 100 permuted differences in proportions
ggplot(homeown_perm, aes(x = stat)) + 
  geom_dotplot(binwidth = 0.001)

# Perform 1000 permutations
homeown_perm <- homes %>%
  # Specify HomeOwn vs. Gender, with `"Own" as success
  specify(HomeOwn ~ Gender, success = "Own") %>%
  # Use a null hypothesis of independence
  hypothesize(null = "independence") %>% 
  # Generate 1000 repetitions (by permutation)
  generate(reps = 1000, type = "permute") %>% 
  # Calculate the difference in proportions (male then female)
  calculate(stat = "diff in props", order = c("male" ,"female"))

# Density plot of 1000 permuted differences in proportions
ggplot(homeown_perm, aes(x = stat)) + 
  geom_density()

# Plot permuted differences, diff_perm
ggplot(homeown_perm, aes(x = diff_perm)) + 
  # Add a density layer
  geom_density() +
  # Add a vline layer with intercept diff_orig
  geom_vline(aes(xintercept = diff_orig), color = "red")

# Compare permuted differences to observed difference
homeown_perm %>%
  summarize(n_perm_le_obs = sum(diff_perm <= diff_orig))
#This only represents 21.2% of the null statistics, so you can conclude that the observed difference is consistent with the permuted distribution.

#
#
#Ch2
disc <- data.frame(
  promote = c(rep("promoted", 35), rep("not_promoted", 13)),
  sex = c(rep("male", 21), rep("female", 14),
          rep("male", 3), rep("female", 10)))

# Create a contingency table summarizing the data
disc %>%
  # Count the rows by sex, promote
  count(sex, promote)

# Find proportion of each sex who were promoted
disc %>%
  # Group by sex
  group_by(sex) %>%
  # Calculate proportion promoted summary stat
  summarise(promoted_prop = mean(promote == "promoted"))

# Replicate the entire data frame, permuting the promote variable
disc_perm <- disc %>%
  specify(promote ~ sex, success = "promoted") %>%
  hypothesize(null = "independence") %>%
  generate(reps = 5, type = "permute")

disc_perm %>%
  # Group by replicate
  group_by(replicate) %>%
  # Count per group
  count(sex, promote)

disc_perm %>%
  # Calculate difference in proportion, male then female
  calculate(stat = "diff in props", order = c("male", "female"))
#note: Each replicate had slightly different counts of promotion and sex, which led to slightly different statistics being calculated for each replicate.



# Calculate the observed difference in promotion rate
diff_orig <- disc %>%
  # Group by sex
  group_by(sex) %>%
  # Summarize to calculate fraction promoted
  summarise(prop_prom = mean(promote == "promoted")) %>%
  # Summarize to calculate difference
  summarise(stat = diff(prop_prom)) %>% 
  pull()

# See the result
diff_orig

# Create data frame of permuted differences in promotion rates
disc_perm <- disc %>%
  # Specify promote vs. sex
  specify(promote ~ sex, success = "promoted") %>%
  # Set null hypothesis as independence
  hypothesize(null = "independence") %>%
  # Generate 1000 permutations
  generate(reps = 1000, type = "permute") %>%
  # Calculate difference in proportions
  calculate(stat = "diff in props", order = c("male", "female"))



# From previous steps
diff_orig <- disc %>%
  group_by(sex) %>%
  summarize(prop_prom = mean(promote == "promoted")) %>%
  summarize(stat = diff(prop_prom)) %>% 
  pull()

disc_perm <- disc %>%
  specify(promote ~ sex, success = "promoted") %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "diff in props", order = c("male", "female"))

# Using permutation data, plot stat
ggplot(disc_perm, aes(x = stat)) + 
  # Add a histogram layer
  geom_histogram(binwidth = 0.01) +
  # Add a vertical line at diff_orig
  geom_vline(aes(xintercept = diff_orig), color = "red")

#note: There are very few permuted differences which are as extreme as the observed difference, but there cannot be a causative conclusion because the study was observational.

disc_perm %>% 
  summarize(
    # Find the 0.9 quantile of diff_perm's stat
    q.90 = quantile(stat, p = 0.9),
    # ... and the 0.95 quantile
    q.95 = quantile(stat, p = 0.95),
    # ... and the 0.99 quantile
    q.99 = quantile(stat, p = 0.99)
  )

# Use disc_perm
disc_perm %>% 
  # ... to calculate summary stats
  summarise(
    # Find the 0.01 quantile of stat
    q.01 = quantile(stat, p = 0.01),
    # ... and 0.05
    q.05 = quantile(stat, p = 0.05),
    # ... and 0.1 
    q.10 = quantile(stat, p = 0.1)
  )


#a larger sample will lead to a less variable statistic.

#The observed difference is consistent with differences you would see by chance if the sample size was small. The observed difference would virtually never be observed by chance if the sample size was big.



calc_upper_quantiles <- function(dataset) {
  dataset %>% 
    summarize(
      q.90 = quantile(stat, p = 0.90),
      q.95 = quantile(stat, p = 0.95),
      q.99 = quantile(stat, p = 0.99)
    )
}

# Recall the quantiles associated with the original dataset
calc_upper_quantiles(disc_perm)

# Calculate the quantiles associated with the small dataset
calc_upper_quantiles(disc_perm_small)

# Calculate the quantiles associated with the big dataset
calc_upper_quantiles(disc_perm_big)

?get_p_value

# Visualize and calculate the p-value for the original dataset
disc_perm %>%
  visualise(obs_stat = diff_orig, direction = "greater")

disc_perm %>%
  get_p_value(obs_stat = diff_orig, direction = "greater")

# Visualize and calculate the p-value for the small dataset
disc_perm_small %>%
  visualise(obs_stat = diff_orig_small, direction = "greater")

disc_perm_small %>%
  get_p_value(obs_stat = diff_orig_small, direction = "greater")

# Visualize and calculate the p-value for the big dataset
disc_perm_big %>%
  visualise(obs_stat = diff_orig_big, direction = "greater")

disc_perm_big %>%
  get_p_value(obs_stat = diff_orig_big, direction = "greater")


# Recall the original data
disc %>% 
  count(sex, promote)

# Tabulate the new data
disc_new %>% 
  count(sex, promote)

# Recall the distribution of the original permuted differences
ggplot(disc_perm, aes(x = stat)) + 
  geom_histogram() +
  geom_vline(aes(xintercept = diff_orig), color = "red")

# Plot the distribution of the new permuted differences
ggplot(disc_perm_new, aes(x = stat)) + 
  geom_histogram() +
  geom_vline(aes(xintercept = diff_orig_new), color = "red")

# Recall the p-value from the original data
disc_perm %>%
  summarize(p_value = mean(diff_orig <= stat))

# Find the p-value from the new data
disc_perm_new %>%
  summarize(p_value = mean(diff_orig_new <= stat))

# Calculate the two-sided p-value
disc_perm %>%
  summarize(p_value = 2 * mean(diff_orig <= stat))


#CH3

# Tabulate the data
opportunity %>%
  count(decision, group)

# Find the proportion who bought the DVD in each group
opportunity %>%
  group_by(group) %>%
  summarize(buy_prop = mean(decision == "buyDVD"))

# Plot group, filled by decision
ggplot(opportunity, aes(x = group, fill = decision)) + 
  # Add a bar layer, with position "fill"
  geom_bar(position = "fill")

#The treatment seems to have worked


# From previous steps
diff_obs <- opportunity %>%
  group_by(group) %>%
  summarize(prop_buy = mean(decision == "buyDVD")) %>%
  summarize(stat = diff(prop_buy)) %>% 
  pull()
opp_perm <- opportunity %>%
  specify(decision ~ group, success = "buyDVD") %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "diff in props", order = c("treatment", "control"))

# Using the permuation data, plot stat
ggplot(opp_perm, aes(x = stat)) + 
  # Add a histogram layer with binwidth 0.005
  geom_histogram(binwidth = 0.005) +
  # Add a vline layer with intercept diff_obs
  geom_vline(aes(xintercept = diff_obs), color = "red")

# Visualize the statistic 
opp_perm %>%
  visualize(obs_stat = diff_orig, direction = "less")

# Calculate the p-value using `get_p_value`
opp_perm %>%
  get_p_value(obs_stat = diff_orig, direction = "less")

# Calculate the p-value using `summarize`
opp_perm %>%
  summarize(p_value = mean(stat <= diff_orig))



#Ch4
#Bootstrap provides an approximation of the standard error

# From previous steps
ex1_props <- all_polls %>% 
  group_by(poll) %>% 
  summarize(stat = mean(vote == "yes"))
ex2_props <- all_polls %>%
  filter(poll == 1) %>%
  select(vote) %>%
  specify(response = vote, success = "yes") %>%
  generate(reps = 1000, type = "bootstrap") %>% 
  calculate(stat = "prop")

# Calculate variability of p-hat
ex1_props %>% 
  summarize(variability = sd(stat))

# Calculate variability of p-hat*
ex2_props %>% 
  summarize(variability = sd(stat))

# Combine data from both experiments
both_ex_props <- bind_rows(ex1_props, ex2_props, .id = "experiment")

# Using both_ex_props, plot stat colored by experiment
ggplot(both_ex_props, aes(stat, color = experiment)) + 
  # Add a density layer with bandwidth 0.1
  geom_density(bw = 0.1)

#Resamples with replacement are an excellent model for the process of taking the original sample from the population. Remember, in research problems, you don't have an ability to take more than one original sample, but you can take as many resamples as you like.



# Proportion of yes votes by poll
props <- all_polls %>% 
  group_by(poll) %>% 
  summarize(prop_yes = mean(vote == "yes"))

# The true population proportion of yes votes
true_prop_yes <- 0.6

# Proportion of polls within 2SE
props %>%
  # Add column: is prop_yes in 2SE of 0.6
  mutate(is_in_conf_int = abs(prop_yes - true_prop_yes) < 2 * sd(prop_yes)) %>%
  # Calculate  proportion in conf int
  summarize(prop_in_conf_int = mean(is_in_conf_int))
#it looks like 96.6% are within 2 standard errors of the true population parameter.


# From previous exercises
one_poll <- all_polls %>%
  filter(poll == 1) %>%
  select(vote)

one_poll_boot <- one_poll %>%
  specify(response = vote, success = "yes") %>%
  generate(reps = 1000, type = "bootstrap") %>% 
  calculate(stat = "prop")

p_hat <- one_poll %>%
  # Calculate proportion of yes votes
  summarize(stat = mean(vote == "yes")) %>%
  pull()

# Create an interval of plausible values
one_poll_boot %>%
  summarize(
    # Lower bound is p_hat minus 2 std errs
    lower = p_hat - 2 * sd(stat),
    # Upper bound is p_hat plus 2 std errs
    upper = p_hat + 2 * sd(stat)
  )

# From previous step
percentile_ci <- one_poll_boot %>% 
  get_confidence_interval(level = 0.95)

one_poll_boot %>% 
  # Visualize in-between the endpoints given by percentile_ci
  visualize(endpoints = percentile_ci, direction = "between")

calc_t_conf_int <- function(resampled_dataset) {
  resampled_dataset %>%
    summarize(
      lower = p_hat - 2 * sd(stat),
      upper = p_hat + 2 * sd(stat)
    )
}

# Find the bootstrap t-confidence interval for 30 resamples
calc_t_conf_int(one_poll_boot)

# ... and for 300 resamples
calc_t_conf_int(one_poll_boot_300)

# ... and for 3 resamples
calc_t_conf_int(one_poll_boot_3)


calc_p_hat <- function(dataset) {
  dataset %>%
    summarize(stat = mean(vote == "yes")) %>%
    pull()
}
calc_t_conf_int <- function(resampled_dataset, p_hat) {
  resampled_dataset %>%
    summarize(
      lower = p_hat - 2 * sd(stat),
      upper = p_hat + 2 * sd(stat)
    )
}

# Find proportion of yes votes from original population
p_hat <- calc_p_hat(one_poll)

# Review the value
p_hat  

# Calculate bootstrap t-confidence interval (original 0.6 param)
calc_t_conf_int(one_poll_boot, p_hat)

# Find proportion of yes votes from new population
p_hat_0.8 <- calc_p_hat(one_poll_0.8)

# Review the value
p_hat_0.8  

# Calculate the bootstrap t-confidence interval (new 0.8 param)
calc_t_conf_int(one_poll_boot_0.8, p_hat_0.8)


# Calculate a 95% bootstrap percentile interval
one_poll_boot %>% 
  get_confidence_interval(level = 0.95) 

# Calculate a 99% bootstrap percentile interval
one_poll_boot %>% 
  get_confidence_interval(level = 0.99) 

# Calculate a 90% bootstrap percentile interval
one_poll_boot %>% 
  get_confidence_interval(level = 0.90) 

# Plot ci_endpoints vs. ci_percent to compare the intervals
ggplot(conf_int_data, aes(ci_percent, ci_endpoints)) +
  # Add a line layer
  geom_line()

