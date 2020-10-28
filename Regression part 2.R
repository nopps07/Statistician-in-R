library(ggplot2)
library(plotly)
library(dplyr)
library(broom)


#Two paralel lines
#Two categorical variables. Different intercepts

mod <- lm(hwy ~ displ + factor(year), data = mpg)
mod

str(mpg)


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



## Adding a numerical explanatory variable
# Tiling the plan
grid <- babies %>%
  data_grid(
    gestation = seq_range(gestation, by = 1),
    age = seq_range(age, by = 1)
  )
mod <- lm(bwt ~ gestation + age, data = babies)
bwt_hats <- augment(mod, newdata = grid)

data_space +
  geom_tile(data = bwt_hats, aes(fill = .fitted, alpha = 0.5)) +
  scale_fill_continuous("bwt", limits = range(babies$bwt))

#3D visualization
plot_ly(data = babies, z = ~bwt, x = ~gestation, y = ~age, opacity = 0.6) %>%
  add_markers(text = ~case, marker = list(size = 2)) %>%
  add_surface(x = ~x, y = ~y, z = ~plane, showscale = FALSE,
              cmax = 1, surfacecolor = color1, colorscale = col1)

# add predictions to grid
price_hats <- augment(mod, newdata = grid)

# tile the plane
data_space + 
  geom_tile(data = price_hats, aes(fill = .fitted), alpha = 0.5)


# draw the 3D scatterplot
p <- plot_ly(data = mario_kart, z = ~totalPr, x = ~duration, y = ~startPr, opacity = 0.6) %>%
  add_markers() 

# draw the plane
p %>%
  add_surface(x = ~x, y = ~y, z = ~plane, showscale = FALSE)


#Mtcars
m <- plot_ly(data = mtcars, z = ~mpg, x = ~cyl, y = ~wt, opacity = 0.6) %>%
  add_markers()

m %>%
  add_surface(x = ~x, y = ~y, z = ~plane, showscale = FALSE)


## Conditional interpretation of coefficients

#Important notes:
#In multiple regression model, be sure to always include a phrase to the effect of "holding x constant'


#Geometry
#1 num + 1 cat: parallel lines
# 2 num: a plane
#2 num + 1 cat: parallel planes

#sample 3d model
plot_ly(data = babies, z = ~bwt, x = ~gestation, y = ~age, opacitiy = 0.6) %>%
  add_markers(color = ~factor(smoke), text = ~case, marker = list(size = 2)) %>%
  add_surface(x = ~x, y = ~y, z = ~plane0, showscale =FALSE,
              cmin = 0, cmax = 1, surfacecolor = color1, colorscale = col1) %>%
  add_surface(x = ~x, y = ~y, z = ~plane1, showscale =FALSE,
              cmin = 0, cmax = 1, surfacecolor = color2, colorscale = col1)


#the dot operator 
#meaning 'all other variables in the data'

#Be careful with the exception
#. - case : "all variables except for the one named case"

