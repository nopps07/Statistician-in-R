##Inference for Categorical Data
library(dplyr)
library(ggplot2)
library(infer)

#Ch1

##Note: Given GSS dataset does not match the GSS used in the course.
##consci should have been binary.

# From previous steps
gss2016 <- gss %>%
  filter(year == 2016)

head(gss2016)

ggplot(gss2016, aes(x = consci)) +
  geom_bar()
# Compute proportion of high conf
p_hat <- gss2016 %>%
  summarize(prop_high = mean(consci == "High")) %>%
  pull()


# Create single bootstrap data set
boot1 <- gss2016 %>%
  # Specify the response
  specify(response = consci, success = "High") %>%
  # Generate one bootstrap replicate
  generate(reps = 1, type = "bootstrap")
# Compute proportion with high conf
boot1 %>%
  summarize(prop_high = mean(consci == "High")) %>%
  pull()


# From previous steps
boot_dist <- gss2016 %>%
  specify(response = consci, success = "High") %>%
  generate(reps = 500, type = "bootstrap") %>%
  calculate(stat = "prop")
ggplot(boot_dist, aes(x = stat)) +
  geom_density()
SE <- boot_dist %>%
  summarize(se = sd(stat)) %>%
  pull()
# Create CI
c(p_hat - 2 * SE, p_hat + 2 * SE)


# From previous steps
boot_dist_small <- gss2016_small %>%
  specify(response = consci, success = "High") %>%
  generate(reps = 500, type = "bootstrap") %>%
  calculate(stat = "prop")
SE_small_n <- boot_dist_small %>%
  summarize(se = sd(stat)) %>%
  pull()
boot_dist_smaller <- gss2016_smaller %>%
  specify(response = consci, success = "High") %>%
  generate(reps = 500, type = "bootstrap") %>%
  calculate(stat = "prop")

# Compute and save estimate of second SE
SE_smaller_n <- boot_dist_smaller %>%
  summarise(se = sd(stat)) %>%
  pull()

# Compare the results for each dataset size
message("gss2016_small has ", nrow(gss2016_small), " rows and standard error ", SE_small_n)
message("gss2016_smaller has ", nrow(gss2016_smaller), " rows and standard error ", SE_smaller_n)

# From previous steps
ggplot(gss2016, aes(x = meta_region)) +
  geom_bar()
boot_dist <- gss2016 %>%
  specify(response = meta_region, success = "pacific") %>%
  generate(reps = 500, type = "bootstrap") %>%
  calculate(stat = "prop")

# Calculate std error
SE_low_p <- boot_dist %>%
  summarise(se = sd(stat)) %>%
  pull()

# Compare SEs
c(SE_low_p, SE)

# From previous step
n <- nrow(gss2016)
p_hat <- gss2016 %>%
  summarize(prop_pacific = mean(meta_region == "pacific")) %>%
  pull()

# Check conditions
n * p_hat >= 10
n * (1 - p_hat) >= 10

# Calculate SE
SE_approx <- sqrt(p_hat * (1 - p_hat) / n)

# Form 95% CI
c(p_hat - 2 * SE_approx, p_hat + 2 * SE_approx)



##Ch2


##Ch3
# From previous step
gss_party <- gss2016 %>%
  filter(party != "Oth")

# Visualize distribution take 2 
gss_party %>%
  ggplot(aes(x = party, fill = natspac)) +
  # Add bar layer of counts
  geom_bar()

# Create table of natspac and party
Obs <- gss_party %>%
  # Select columns of interest
  select(natspac, party) %>%
  # Create table
  table()

# Convert table back to tidy df
Obs %>%
  # Tidy the table
  tidy() %>%
  # Expand out the counts
  uncount(n)