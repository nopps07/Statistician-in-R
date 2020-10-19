library(ggplot2)

#Two paralel lines
#Two categorical variables. Different intercepts

mod <- lm(hwy ~ displ + factor(year), data = mpg)
mod

str(mpg)

library(broom)
augmod <- augment(mod)
glimpse(augmod)
str(augmod)
#Applying augment() to our model will return a data frame with the fitted values attached,


data_space <- ggplot(augmod, aes(x = hwy, y = displ, color = factor.year.)) + geom_point()
data_space +
  geom_line(aes(y = .fitted))



# Augment the model
augmented_mod <- augment(mod)
glimpse(augmented_mod)

# scatterplot, with color
data_space <- ggplot(augmented_mod, aes(x = wheels, y = totalPr, color = cond)) + 
  geom_point()

# single call to geom_line()
data_space + 
  geom_line(aes(y = .fitted))



# Avoiding misunderstandings
# 1. There is only one slope
# 2. Which is the reference level?
# 3. What are the units?
# 4. After controlling for...



#Fitted Values
#returns a vector
predict(mod)
#returns a data.frame
augment(mod)

#Predictions
new_obs <- data.frame(displ = 1.8, year = 2008)
predict(mod, newdata = new_obs)
augment(mod, newdata = new_obs)


?mutate

# R^2 and adjusted R^2
summary(mod)

# add random noise
mario_kart_noisy <- mario_kart %>%
  mutate(noise = rnorm(nrow(mario_kart)))

# compute new model
mod2 <- lm(totalPr ~ wheels + cond + noise, data = mario_kart_noisy)

# new R^2 and adjusted R^2
summary(mod2)

#con: noise decreases the R^2 values


#Understanding interactions
ggplot(data = mpg, aes(x = displ, y = hwy, color = factor(year))) +
  geom_point() +
  geom_smooth(method = "lm", se = 0)

#add interaction term manually
lm(hwy ~ displ + factor(year) + displ:factor(year), data = mpg)

#lm(y ~ x + z + x:z, data = mydata)
#The use of : means that the interaction between x and z will be a third term in the model

# interaction plot
ggplot(mario_kart, aes(y = totalPr, x = duration, color = cond)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE)


# Simpson's Paradox
# When the paradox occurs, the group membership is an important confounder that must be controlled in order for an appropriate model

slr <- ggplot(mario_kart, aes(y = totalPr, x = duration)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE)

# model with one slope
lm(totalPr ~ duration, data = mario_kart)

# plot with two slopes
slr + aes(color = cond)



