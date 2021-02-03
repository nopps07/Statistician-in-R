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
?SSizeLogisticBin
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



###Chapter 2
## Analyzing results
library(broom)

experiment_data <- read_csv("experiment_data.csv")

experiment_data_sum <- experiment_data %>%
  group_by(visit_date, condition) %>%
  summarize(conversion_rate = mean(clicked_adopt_today))

ggplot(experiment_data_sum,
       aes(x = visit_date,
           y = conversion_rate,
           color = condition,
           group = condition)) +
  geom_point() +
  geom_line()

glm(clicked_adopt_today ~ #dependent
      condition,#independent
    family = "binomial",
    data = experiment_data
) %>%
  tidy()
#To define whether the test succeeds 


# Group and summarize data
experiment_data_clean_sum <- experiment_data_clean %>%
  group_by(condition, visit_date) %>%
  summarize(conversion_rate = mean(clicked_adopt_today))
# Make plot of conversion rates over time
ggplot(experiment_data_clean_sum,
       aes(x = visit_date,
           y = conversion_rate,
           color = condition,
           group = condition)) +
  geom_point() +
  geom_line()

?glm
#family:
#a description of the error distribution and link function to be used in the model.

# View summary of results
experiment_data_clean %>%
  group_by(condition) %>%
  summarize(conversion_rate = mean(clicked_adopt_today))

# Run logistic regression
experiment_results <- glm(clicked_adopt_today ~ condition,
                          family = "binomial",
                          data = experiment_data_clean) %>%
  tidy()


## Designing follow-up experiments
#  Tips for designing a new experiment
#1. Build several small follow-up experiments
#2. Avoid 'confounding variables'
#3. Test small changes
#Be specific! must be able to pick what variable is different

# Run logistic regression power analysis
total_sample_size <- SSizeLogisticBin(p1 = 0.39,
                                      p2 = 0.59,
                                      B = 0.5,
                                      alpha = 0.05,
                                      power = 0.8)
#Kitten...! the number of samples you need if you wanna see efficient results

# Another case (kitten)
# Read in data for follow-up experiment
followup_experiment_data <- read_csv("followup_experiment_data.csv")
# View conversion rates by condition
followup_experiment_data %>%
  group_by(condition) %>%
  summarise(conversion_rate = mean(clicked_adopt_today))
# Run logistic regression
followup_experiment_results <- glm(clicked_adopt_today ~ condition,
                                   family = "binomial",
                                   data = followup_experiment_data) %>%
  tidy()
# P.value > 0.05 -> kittens aren't actually that desirable?
# or because we went in with bad assumptions?
# we need to take into account the meaning of 'control' in any tests!


#New column
eight_month_checkin_data_sum <- eight_month_checkin_data %>%
  mutate(month_text = month(visit_date, label = TRUE)) %>% #making new column
  group_by(month_text, condition) %>%
  summarise(conversion_rate = mean(clicked_adopt_today))

#plot
ggplot(eight_month_checkin_data_sum, aes(x = month_text, y = conversion_rate,
                                         color = condition, group = condition)) +
  geom_point() +
  geom_line()

#Plot Styling 1
ggplot(eight_month_checkin_data_sum, aes(x = month_text, y = conversion_rate,
                                         color = condition, group = condition)) +
  geom_point() +
  geom_line() +
  scale_y_continuous(limits = c(0, 1),
                     labels = percent) +
  labs(x = "Month",
       y = "Conversion Rate")

#Plot Styling 2
ggplot(eight_month_checkin_data_sum, aes(x = month_text, y = conversion_rate,
                                         color = condition, group = condition)) +
  geom_point(size = 4) + #dots get larger
  geom_line(lwd = 1) + #lines get thicker
  scale_y_continuous(limits = c(0, 1),
                     labels = percent) +
  labs(x = "Month",
       y = "Conversion Rate")

#Follow-up experiment assumptions
eight_month_checkin_data_sum <- eight_month_checkin_data %>%
  mutate(month_text = month(visit_date, label = TRUE)) %>%
  group_by(month_text, condition) %>%
  summarise(conversion_rate = mean(clicked_adopt_today))

eight_month_checkin_data_diff <- eight_month_checkin_data_sum %>%
  spread(condition, conversion_rate) %>%
  mutate(condition_diff = cat_hat - no_hat)

mean(eight_month_checkin_data_diff$condition_diff)
sd(eight_month_checkin_data_diff$condition_diff)

# Compute difference over time
no_hat_data_diff <- no_hat_data_sum %>%
  spread(year, conversion_rate) %>%
  mutate(year_diff = `2018` - `2017`)
no_hat_data_diff

# Compute summary statistics
mean(no_hat_data_diff$year_diff, na.rm = TRUE)
sd(no_hat_data_diff$year_diff, na.rm = TRUE)

# Run power analysis for logistic regression
total_sample_size <- SSizeLogisticBin(p1 = 0.49,
                                      p2 = 0.64,
                                      B = 0.5,
                                      alpha = 0.05,
                                      power = 0.8)
#must understand p1 and p2

# View summary of data
followup_experiment_data_sep %>%
  group_by(condition) %>%
  summarise(conversion_rate = mean(clicked_adopt_today))

# Run logistic regression
followup_experiment_sep_results <- glm(clicked_adopt_today ~ condition,
                                       family = "binomial",
                                       data = followup_experiment_data_sep) %>%
  tidy()


## Chapter 3
#A/B testing 

# Compute summary by month
viz_website_2017 %>%
  group_by(month(visit_date)) %>%
  summarise(article_conversion_rate = mean(clicked_article))

# Compute 'like' click summary by month
viz_website_2017_like_sum <- viz_website_2017 %>%
  mutate(month = month(visit_date, label = TRUE)) %>%
  group_by(month) %>%
  summarize(like_conversion_rate = mean(clicked_like))

# Plot 'like' click summary by month
ggplot(viz_website_2017_like_sum,
       aes(x = month, y = like_conversion_rate, group = 1)) +
  geom_point() +
  geom_line() +
  scale_y_continuous(limits = c(0, 1), labels = percent)

# Plot comparison of 'like'ing and 'sharing'ing an article
ggplot(viz_website_2017_like_share_sum,
       aes(x = month, y = conversion_rate, color = action, group = action)) +
  geom_point() +
  geom_line() +
  scale_y_continuous(limits = c(0, 1), labels = percent)


#Types of A/B Testing
#A/B - compare a control and a test condition ("Tips" vs "Tools.")
#A/A - compare two groups of control conditions ("Tips (Group 1)" to "Tips (Group 2)")
#A/B/N - compare a control condition to any number of different test conditions (e.g. "Tips" vs "Tools" vs "strategies")

# Compute conversion rates for A/A experiment
viz_website_2018_01_sum <- viz_website_2018_01 %>%
  group_by(condition) %>%
  summarize(like_conversion_rate = mean(clicked_like))

viz_website_2018_01_sum

# Plot conversion rates for two conditions
ggplot(viz_website_2018_01_sum,
       aes(x = condition, y = like_conversion_rate)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(limits = c(0, 1), labels = percent)

# Run logistic regression
aa_experiment_results <- glm(clicked_like ~ condition, #condition is independent
                             family = "binomial",
                             data = viz_website_2018_01) %>%
  tidy()
aa_experiment_results

# Confounding variabless
# it is an element of the environment that could affect your ability to find out the truth of an A/B experiment.
# External factor? white colour goes up since Snow White is on cinema

# Compute 'like' conversion rate by week and condition
viz_website_2018_02 %>%
  mutate(week = week(visit_date)) %>%
  group_by(week, condition) %>%
  summarize(like_conversion_rate = mean(clicked_like))

# Compute 'like' conversion rate by if article published and condition
viz_website_2018_02 %>%
  group_by(article_published, condition) %>%
  summarize(like_conversion_rate = mean(clicked_like))


# Plot 'like' conversion rates by date for experiment
ggplot(viz_website_2018_02_sum,
       aes(x = visit_date,
           y = like_conversion_rate,
           color = condition,
           linetype = article_published,
           group = interaction(condition, article_published))) +
  geom_point() +
  geom_line() +
  geom_vline(xintercept = as.numeric(as.Date("2018-02-15"))) +
  scale_y_continuous(limits = c(0, 0.3), labels = percent)
# Question
# Find the date when the article was first published. Now add that date as the value for xintercept in geom_vline(). (Note: it should be of the format "YYYY-MM-DD".)

# Side effects
# Examples:
# load times, information 'above the fold'
# Confounding variables VS Side effects
# When choosing your pictures you introduced a confounding variable of cat age,
# when adding the pictures you had a side effect of load times.

# Compute 'like' conversion rate and mean pageload time by day
viz_website_2018_03_sum <- viz_website_2018_03 %>%
  group_by(visit_date, condition) %>%
  summarize(mean_pageload_time = mean(pageload_time),
            like_conversion_rate = mean(clicked_like))
head(viz_website_2018_03_sum)

# Plot effect of 'like' conversion rate by pageload time
ggplot(viz_website_2018_03_sum,
       aes(x = mean_pageload_time, y = like_conversion_rate, color = condition)) +
  geom_point()

# Plot 'like' conversion rate by day
ggplot(viz_website_2018_03_sum,
       aes(x = visit_date,
           y = like_conversion_rate,
           color = condition,
           linetype = pageload_delay_added,
           group = interaction(condition, pageload_delay_added))) +
  geom_point() +
  geom_line() +
  geom_vline(xintercept = as.numeric(as.Date("2018-03-15"))) +
  scale_y_continuous(limits = c(0, 0.3), labels = percent)


## Chapter 4

# power analyses 

# Load package to run power analysis
library(powerMediation)

# Run power analysis for logistic regression
total_sample_size <- SSizeLogisticBin(p1 = 0.17,
                                      p2 = 0.27,
                                      B = 0.5,
                                      alpha = 0.05,
                                      power = 0.8)
total_sample_size

install.packages("pwr")
library(pwr)

?pwr.t.test
#type refers to number and type of samples
#alternative to hypothesis

# Run power analysis for t-test
sample_size <- pwr.t.test(d = 0.3,
                          sig.level = 0.05,
                          power = 0.8)
sample_size


# Statistical Tests
# Logistic regression: a binary (categorical) dependent variable (cliked or didn't)
# T-test (linear regression) - a continuous dependent variable (e.g., time spent on website)

# Run logistic regression
ab_experiment_results <- glm(clicked_like ~ condition,
                             family = "binomial",
                             data = viz_website_2018_04) %>%
  tidy()

#Intepretation?
#Not in the direction we wanted.
#looks like 'Tools' actually had lower 'like' click rates than 'Tips'

# Run t-test
ab_experiment_results <- t.test(time_spent_homepage_sec ~ condition,
                                data = viz_website_2018_04)
ab_experiment_results


# Stopping rules and Sequential Analysis
# Stopping rules:
# Procedures that allow interim analyses in clinical trials at predefined times,
# while preserving the type I error at some pre-specifed level.
# Sequential Analysis:
# A procedure in which a statistical test of significance is conducted repeatedly
# over time as the data are collected.
# After each observation, the cumulative data are analysed and one of the following three decisions taken:
# 1. stop the data collection, reject N0 and claim statistical significance
# 2. stop the data collection, do not reject N0 and state that the results are not statistically significant
# 3. continue the data collection, since as yet the cumulated data are inadequate to draw a conclusion.

# Why stopping rules are usefl
# Prevent p-hacking
# Accounts for unsure effect size.
# Allows for better allocation of resources

# 분석과정에서 통계적으로 유의미한 결과값을 얻을 수 있는
# 데이터의 개수를 매번 정확하게 수집하기 힘들기 때문에 쓰는거네.

install.packages("gsDesign")
library(gsDesign)

# P-value cutoff???
# Run sequential analysis
seq_analysis_3looks <- gsDesign(k = 3,
                                test.type = 1,
                                alpha = 0.05,
                                beta = 0.2,
                                sfu = "Pocock")

# Fill in max number of points and compute points per group and find stopping points
max_n <- 3000
max_n_per_group <- max_n / 2
stopping_points <- max_n_per_group * seq_analysis_3looks$timing
stopping_points


# Multivariate Testing
# Sometimes the goal of an experiment is to see how two different changes might affect each other.
# Watch this video again in order to understand the result of multivariate analyses in R
# *Baseline values - use factor() to avoid confusion

multivar_results <- check %>%
  mutate(one = factor(one,
                      levels = c("Gunho", "Lee"))) %>%
  mutate(two = factor(two,
                      levels = c("Korean", "Asia"))) %<%
  lm(dependent ~ one * two,
     data = .) %>%
  tidy()

str(viz_website_2018_05)
# Compute summary values for four conditions
viz_website_2018_05_sum <- viz_website_2018_05 %>%
  group_by(word_one, word_two) %>%
  summarize(mean_time_spent_homepage_sec = mean(time_spent_homepage_sec))

head(viz_website_2018_05_sum)
# Plot summary values for four conditions
ggplot(viz_website_2018_05_sum,
       aes(x = word_one,
           y = mean_time_spent_homepage_sec,
           fill = word_two)) +
  geom_bar(stat = "identity", position = "dodge")

# Compute summary values for four conditions
viz_website_2018_05_sum <- viz_website_2018_05 %>%
  group_by(word_one, word_two) %>%
  summarize(like_conversion_rate = mean(clicked_like))

# Plot summary values for four conditions
ggplot(viz_website_2018_05_sum,
       aes(x = word_one,
           y = like_conversion_rate,
           fill = word_two)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(limits = c(0, 1), labels = percent)

# Organize variables and run logistic regression
viz_website_2018_05_like_results <- viz_website_2018_05 %>%
  mutate(word_one = factor(word_one,
                           levels = c("tips", "tools"))) %>%
  mutate(word_two = factor(word_two,
                           levels = c("better", "amazing"))) %>%
  glm(clicked_like ~ word_one * word_two,
      family = "binomial",
      data = .) %>%
  tidy()
viz_website_2018_05_like_results
# How to interpret them
