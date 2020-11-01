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

