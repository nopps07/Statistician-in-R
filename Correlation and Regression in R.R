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