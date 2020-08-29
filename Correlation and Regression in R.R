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

#What you should avoid
#1.Implying that the regression model establishes a cause-and-effect relationship
#2.Switching the rold of the response and explanatory variables
#3.Confusing percentage change with percentage point change.
# Linear model for weight as a function of height
lm(wgt ~ hgt, data = bdims)
# Linear model for SLG as a function of OBP
lm(SLG ~ OBP, data = mlbBat10)
# Log-linear model for body weight as a function of brain weight
lm(log(BodyWt) ~ log(BrainWt), data = mammals)

#fitted.values
#it returns a vector containing the Y hat values for each data point.
#each fitted value generates a residual.
#Understand the below
mod <- lm(wgt ~ hgt, data = bdims)
# Show the coefficients
coef(mod)
# Show the full output
summary(mod)
# Mean of weights equal to mean of fitted values?
mean(bdims$wgt) == mean(fitted.values(mod))
# Mean of the residuals
mean(residuals(mod))

#Tidying your linear model
# Load broom
library(broom)
# Create bdims_tidy
bdims_tidy <- augment(mod)
# Glimpse the resulting data frame
glimpse(bdims_tidy)
# Check
# Practice to grasp the info of 'augment'

# Visualise new observations
isrs <- broom::augment(mod, newdata = new_data)
ggplot(data = textbooks, aes(x = amazNew, y = unlaNew)) +
  geom_point() + geom_smooth(method = "lm") +
  geom_point(data = isrs, aes(y = .fitted), size = 3, color = "red")
# Print ben
ben
# Predict the weight of ben
predict(mod, newdata = ben)
# Note: mod라는 regression model 이 있고, observation 'Ben' 에 대응하는 예측값을 predict로 추출이 가능하다
# Add the line to the scatterplot
ggplot(data = bdims, aes(x = hgt, y = wgt)) + 
  geom_point() + 
  geom_abline(data = coefs, 
              aes(intercept = `(Intercept)`, slope = hgt),  
              color = "dodgerblue")


#Chapter 5
#Assessing Model Fit
#Found the line that minimises the sum of the squared residuals.
#Function
library(broom)
mod_possum <- lm(totalL ~ tailL, data = possum)
mod_possum %>%
  augment() %>%
  summarise(SSE = sum(.resid^2),
            SSE_also = (n() - 1) * var(.resid))
#the root mean squared error, RMSE

#Note:
#The residual standard error reported that the regression model
#for poverty rate of U.S. countries in terms of high school
#graduation rate is 4.67. What does it mean?
#Answer:
#The typical difference between the observed poverty rate
#and the poverty rate predicted by the model is about
#4.67 percentage points!!. (not %)
#Check:
#understand 'to make this estimate unbiased'*

# Compute the mean of the residuals
mean(residuals(mod))
# Compute RMSE
sqrt(sum(residuals(mod)^2) / df.residual(mod))

#Null(average) model
#For all observations... Y-hat is equal to Y-average

#Coefficient of determination

# Manually
# Compute R-squared
bdims_tidy %>%
  summarize(var_y = var(wgt), var_e = var(.resid)) %>%
  mutate(R_squared = 1 - (var_e / var_y))
#lm(A ~ B)
#R^2 = 0.515 meaning that 51.4% of the variability in A
#is explained by B

# Compute SSE for null model
mod_null %>%
  summarize(SSE = sum(.resid^2))
# Compute SSE for regression model
mod_hgt %>%
  summarize(SSE = sum(.resid^2))


#Unusual points: leverage, Influence
#leverage computations:
mod <- lm(HR ~ SB, data = regulars)
mod %>%
  augment() %<%
  arrange(desc(.hat)) %>%
  select(HR, SB, .fitted, .resid, .hat) %>%
  head()
#OBservations of high leverage, by virtue of their
#extreme values of the explanatory variable,
#may or may not have a considerable effect on the slope of the model

#But
#'Infuential' is the opposite

#Influence via Cock's distance
mod <- lm(HR ~ SB, data = regulars_plus)
mod %>%
  augment() %>%
  arrange(desc(.cooked)) %>%
  select(HB, SB, .fitted, .resid, .hat, .cooked) %>%
  head()
#Higher .cooksd the more influence it has.



#Dealing with Outliers
#Note:
#Anytime you are thinking about removing outliers,
#Q1. you should ask yourself what the justification is
#'Bc it improves my results' is not a good justification.
#Q2. how does the scope of inference change?
#For instance, if you have no scientific reason to exclude specific variables... be skeptical!!
#A desire to have a hiher R^2 is not a good enough reason!

# Create nontrivial_players
nontrivial_players <- mlbBat10 %>%
  filter(AB >= 10 & OBP < 0.500)
# Fit model to new data
mod_cleaner <- lm(SLG ~ OBP, data = nontrivial_players)
# View model summary
summary(mod_cleaner)
summary(mod)
# Visualize new model
ggplot(nontrivial_players, aes(x = OBP, y = SLG)) +
  geom_point() +
  geom_smooth(method = "lm")

# Rank high leverage points
mod %>%
  augment() %>%
  arrange(desc(.hat), .cooksd) %>%
  head()

