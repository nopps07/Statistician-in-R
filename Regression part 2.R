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
