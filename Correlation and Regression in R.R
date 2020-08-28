#Corrleation and Regression in R

library(ggplot2)

ggplot(data = mpg, aes(x = cyl, y = cty)) +
  geom_point() 


# Boxplot of weight vs. weeks
ggplot(data = ncbirths, 
       aes(x = cut(weeks, breaks = 5), y = weight)) + 
  geom_boxplot()
#cut!

#Refer Openintro package to earn more datasets
library(openintro)

#Get rid of outliers!
m <- mammals

ggplot(mammals, aes(x = BodyWt, y = BrainWt)) +
  geom_point()

MLB <- mlbBat10
ggplot(mlbBat10, aes(x = OBP, y = SLG)) +
  geom_point()

ggplot(data = bdims, aes(x = hgt, y = wgt, color = factor(sex))) +
  geom_point()



#Do this later for Data manipulation
library(naniar)
?naniar
n_miss(mammals)

ggplot(smoking, aes(x = age, y = amtWeekdays)) + geom_point()

#log scale
ggplot(data = mammals, aes(x = BodyWt, y = BrainWt)) +
  geom_point() +
  scale_x_log10() + 
  scale_y_log10()
#equivalent to coord_trans(x = , y= )


library(dplyr)
#Outlier
#Add transparency by Alpha so that overplotting can be seen
#and jitter
#identify
mlbBat10 %>%
  filter(SB > 60 | HR > 50) %>%
  select(name, team, position, SB, HR)


# Filter for AB greater than or equal to 200
ab_gt_200 <- mlbBat10 %>%
  filter(AB >= 200) 

# Scatterplot of SLG vs. OBP
ggplot(ab_gt_200, aes(x = OBP, y = SLG)) +
  geom_point()

# Identify the outlying player
ab_gt_200 %>%
  filter(OBP < 0.200)

#Chapter 2
#Coefficient correlation captures the strength of the linear relation only

#Compute correlation
ncbirths %>%
  summarize(N = n(), r = cor(weight, mage))


#Compute correlation for all non-missing pairs
ncbirths %>%
  summarize(N = n(), r = cor(weight, weeks, use = "pairwise.complete.obs"))


#The Anscombe Dataset: Correlation
# Compute properties of Anscombe
Anscombe %>%
  group_by(set) %>%
  summarize(
    N = n(), 
    mean_of_x = mean(x), 
    std_dev_of_x = sd(x), 
    mean_of_y = mean(y), 
    std_dev_of_y = sd(y), 
    correlation_between_x_and_y = cor(x, y)
  )
# Run this and look at the plot
ggplot(data = mlbBat10, aes(x = OBP, y = SLG)) +
  geom_point()
# Correlation for all baseball players
mlbBat10 %>%
  summarize(N = n(), r = cor(OBP, SLG))
# Run this and look at the plot
mlbBat10 %>% 
  filter(AB > 200) %>%
  ggplot(aes(x = OBP, y = SLG)) + 
  geom_point()
# Correlation for all players with at least 200 ABs
mlbBat10 %>%
  filter(AB >= 200) %>%
  summarize(N = n(), r = cor(OBP, SLG))
# Run this and look at the plot
ggplot(data = bdims, aes(x = hgt, y = wgt, color = factor(sex))) +
  geom_point() 
# Correlation of body dimensions
bdims %>%
  group_by(sex) %>%
  summarize(N = n(), r = cor(hgt, wgt))
# Run this and look at the plot
ggplot(data = mammals, aes(x = BodyWt, y = BrainWt)) +
  geom_point() + scale_x_log10() + scale_y_log10()
# Correlation among mammals, with and without log
mammals %>%
  summarize(N = n(), 
            r = cor(BodyWt, BrainWt), 
            r_log = cor(log(BodyWt), log(BrainWt)))



#Multiple regression model is that a regression such that more than one explanatory variable can be taken into account
#Bivariate variables
#Correlation does not imply causation!!!

#Spurious correlations
# Create faceted scatterplot
ggplot(data = noise, aes(x = x, y = y)) +
  geom_point() + 
  facet_wrap(~ z)
# Compute correlations for each dataset
noise_summary <- noise %>%
  group_by(z) %>%
  summarize(N = n(), spurious_cor = cor(x, y))
# Isolate sets with correlations above 0.2 in absolute strength
noise_summary %>%
  filter(abs(spurious_cor) > 0.2)
#Q. Is this essential step in statistical analysis?


#Chapter 3: Visualising linear models
#The least squared regression line
# Scatterplot with regression line
ggplot(data = bdims, aes(x = hgt, y = wgt)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE)
#se = standard error
# Estimate optimal value of my_slope
add_line(my_slope = 1)

#the code of the function add_line
#bdims_summary <- bdims %>%
#  summarize(N = n(), r = cor(hgt, wgt),
#            mean_hgt = mean(hgt), mean_wgt = mean(wgt),
#            sd_hgt = sd(hgt), sd_wgt = sd(wgt)) %>%
#  mutate(true_slope = r * sd_wgt / sd_hgt, 
#         true_intercept = mean_wgt - true_slope * mean_hgt)
#p <- ggplot(data = bdims, aes(x = hgt, y = wgt)) + 
#  geom_point() + 
#  geom_point(data = bdims_summary, 
#             aes(x = mean_hgt, y = mean_wgt), 
#             color = "red", size = 3)
#
#my_data <- bdims_summary %>%
#  mutate(my_slope = my_slope, 
#         my_intercept = mean_wgt - my_slope * mean_hgt)
#p + geom_abline(data = my_data, 
#                aes(intercept = my_intercept, slope = my_slope), color = "dodgerblue")
#}

#Check list
#1. the basic formula of a regression model
#2. Fitted Values
#3. Residuals (1 - 2)
#4. Key concepts below:
#Y-hat = expected value corresponding to Y
#Beta-hats = estimates of true, unknown hats
#Residuals (e's) are estimates of true, unknown epsilons
#"Error" is rather like 'Noise'

p = 64.594 - 0.591 * h
h = 92.4

# Print bdims_summary
bdims_summary
# Add slope and intercept
bdims_summary %>%
  mutate(slope = r * sd_wgt / sd_hgt, 
         intercept = mean_wgt - slope * mean_hgt)
#check
#how to get intercept

#Regression to the mean (check)
# Height of children vs. height of father
ggplot(data = Galton_men, aes(x = father, y = height)) +
  geom_point() + 
  geom_abline(slope = 1, intercept = 0) + 
  geom_smooth(method = "lm", se = FALSE)
# Height of children vs. height of mother
ggplot(data = Galton_women, aes(x = mother, y = height)) +
  geom_point() + 
  geom_abline(slope = 1, intercept = 0) + 
  geom_smooth(method = "lm", se = FALSE)
#Conclusion:
#Because of regression to the mean, an outstanding basketball player is likely
#to have sons that are good at basketball, but not as good as him.



#Chapter 4
#Interpretation of Regression