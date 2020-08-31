#A/B Testing in R

#Conversion rate

#Test1
#Question. Will changing the homepage photo result in more "Adopt today' clicks?
#Hyphothesis: seeing a cat in a hat will make people more likely to want to adopt.
#Dependent variable: Clicked "Adopt Today!" button or not.
#Independent variable: Homepage photo

#Important check: what does 'more' mean? Conversion rate - today? last year? last week?

#Chapter 1
# Load tidyverse
library(tidyverse)
library(dplyr)

# Read in data
click_data <- read_csv("click_data.csv")
click_data

# Find oldest and most recent date
min(click_data$visit_date)
max(click_data$visit_date)

# Conversion rate
click_sum <- click_data %>%
  group_by(month(visit_date)) %>%
  summarise(conversion_rate = mean(clicked_adopt_today))

ggplot(click_sum, aes(x = month(visit_date), y = conversion_rate)) +
  geom_point() +
  geom_line()

# Calculate the mean conversion rate by day of the week
click_data %>%
  group_by(wday(visit_date)) %>%
  summarize(conversion_rate = mean(clicked_adopt_today))

# Calculate the mean conversion rate by week of the year
click_data %>%
  group_by(week(visit_date)) %>%
  summarize(conversion_rate = mean(clicked_adopt_today))
# Overall, By month, By day of the week, By week of the year

# Compute conversion rate by week of the year
click_data_sum <- click_data %>%
  group_by(week(visit_date)) %>%
  summarize(conversion_rate = mean(clicked_adopt_today))
# Build plot
ggplot(click_data_sum, aes(x = `week(visit_date)`,
                           y = conversion_rate)) +
  geom_point() +
  geom_line() +
  scale_y_continuous(limits = c(0, 1),
                     labels = percent)
   

# Power Analysis! (safeguard)
# 1. Statistical test - statistical test you plan to run
# 2. Baseline value - value for the current control condition
# 3. Desired value - expected value for the test condition
# 4. Proportion of the date - from the test condition (ideally 0.5)
# 5. Significance threshold / alpha - level where effect significant (generally 0.05)
# 6. Power / 1 - Beta - probablity correctly rejecting null hypothesis (generally 0.8)

install.packages("powerMediation")
library(powerMediation)
total_sample_size <- SSizeLogisticBin(p1 = 0.2,
                                      p2 = 0.3,
                                      B = 0.5,
                                      alpha = 0.05,
                                      power = 0.8)
help("SSizeLogisticBin")
#p1 (X = 0, the control condition) / p2 (X = 1, the test condition)

# Compute and look at sample size for experiment in August
total_sample_size <- SSizeLogisticBin(p1 = 0.54,
                                      p2 = 0.64,
                                      B = 0.5,
                                      alpha = 0.05,
                                      power = 0.8)
#We now know if we ran the experiment in August
#we would need at least 758 data points or 379 per group.

# Compute and look at sample size for experiment in August with a 5 percentage point increase
total_sample_size <- SSizeLogisticBin(p1 = 0.54,
                                      p2 = 0.59,
                                      B = 0.5,
                                      alpha = 0.05,
                                      power = 0.8)
#3058 (about 1543 per group), which is much more than 10%

