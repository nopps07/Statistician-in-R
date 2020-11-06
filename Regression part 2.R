library(ggplot2)
library(plotly)
library(dplyr)
library(broom)
library(readr)

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



# Ch4 : what is logistic regression?
ggplot(data = heartTr, aes(x = age, y = survived)) +
  geom_jitter(width = 0, height = 0.05, alpha = 0.5)

#making a binary variable
heartTr <- heartTr %>%
  mutate(is_alive = ifelse(survived == "alive", 1, 0))

data_space <- ggplot(data = heartTr, aes(x= age, y = is_alive)) +
  geom_jitter(width = 0, height = 0.05, alpha = 0.5)

data_space +
  geom_smooth(method = "lm", se = FALSE)

#Generalised linear models
glm(is_alive ~ age, data = heartTr, family = binomial)

# filter
MedGPA_middle <- MedGPA %>%
  filter(GPA >= 3.375, GPA <= 3.770)

# scatterplot with jitter
data_space <- ggplot(data = MedGPA_middle, aes(y = Acceptance, x = GPA)) + 
  geom_jitter(width = 0, height = 0.05, alpha = 0.5)

# linear regression line
data_space + 
  geom_smooth(method = "lm", se = FALSE)

# fit model
glm(Acceptance ~ GPA, data = MedGPA, family = binomial)

#Using bins

# scatterplot with jitter
data_space <- ggplot(data = MedGPA, aes(y = Acceptance, x = GPA)) + 
  geom_jitter(width = 0, height = 0.05, alpha = 0.5)

# add logistic curve
data_space +
  geom_smooth(method = "glm", se = FALSE, method.args = list(family = "binomial"))

#geom_smooth(method = "glm", method.args = binomial, se = FALSE)


# binned points and line
data_space <- ggplot(data = MedGPA_binned, aes(x = mean_GPA, y = acceptance_rate)) + 
  geom_point() + geom_line()

# augmented model
MedGPA_plus <- mod %>%
  augment(type.predict = "response")

# logistic model on probability scale
data_space +
  geom_line(data = MedGPA_plus, aes(x = GPA, y = .fitted), color = "red")


#Probability scale
heartTr_plus <- mod %>%
  augment(type.predict = "response") %>%
  mutate(y_hat = .fitted)
#Probabiliy scale plot
ggplot(date , aes(x = x1, y = y1)) +
  geom_point() + geom_line() +
  scale_y_continuous("Y name", limits = c(0, 1))

#Odds scale
heartTr_plus <- heartTr_plus %>%
  mutate(odds_hat = y_hat / (1 - y_hat))
#Odds scale plot
ggplot(data, aes(x = x1, y = y1)) +
  geom_point() + geom_line() +
  scale_y_continuous("Y name")

#log-odds scale
heartTr_plus <- heartTr_plus %>%
  mutate(log_odds_hat = log(odds_hat))

#Comparison
#Probability scale
#scale: intuitive, easy to interpret
#function: non-liner, hard to interpret

#odds scale
#scale: harder to interpret
#function: exponential, harder to interpret

#log-odds scale
#scale: impossible to interpret
#function: linear, easy to interpret

#odds ratios
exp(coef(mod))
#0.94
#each additional year of age is associated with a 6% decrease in the odds of survival


# compute odds for bins
MedGPA_binned <- MedGPA_binned %>%
  mutate(odds = acceptance_rate / (1 - acceptance_rate))

# plot binned odds
data_space <- ggplot(data = MedGPA_binned, aes(x = mean_GPA, y = odds)) + 
  geom_point() + geom_line()

# compute odds for observations
MedGPA_plus <- MedGPA_plus %>%
  mutate(odds_hat = .fitted / (1 - .fitted))

# logistic model on odds scale
data_space +
  geom_line(data = MedGPA_plus, aes(x = GPA, y = odds_hat), color = "red")


# compute log odds for bins
MedGPA_binned <- MedGPA_binned %>%
  mutate(log_odds = log(acceptance_rate / (1 - acceptance_rate)))

# plot binned log odds
data_space <- ggplot(data = MedGPA_binned, aes(x = mean_GPA, y = log_odds)) + 
  geom_point() + geom_line()

# compute log odds for observations
MedGPA_plus <- MedGPA_plus %>%
  mutate(log_odds_hat = log(.fitted / (1 - .fitted)))

# logistic model on log odds scale
data_space +
  geom_line(data = MedGPA_plus, aes(x = GPA, y = log_odds_hat), color = "red")



#learning from a model
mod <- glm(is_alive ~ age + transplant,
           data = heartTr, family = binomial)

ex(coef(mod))

#making probabilstic predictions
augment(mod, type.predict = "response")

#Out-of-sample predictions
cheney <- data.frame(age = 71, transparent = "treatment")
cheney
augment(mod, nexdata = cheney, type.predict = "response")

#Making binary predictions
mod_plus <- augment(mod, type.predict = "response") %>%
  mutate(alive_hat = round(.fitted))

mod_plus %>%
  select(is_alive, age, transplant, .fitted, alive_hat)

#Confusion matrix
mod_plus %>%
  select(is_alive, alive_hat) %%
  table()


# create new data frame
new_data <- data.frame(GPA = 3.51)

# make predictions
augment(mod, newdata = new_data, type.predict = "response")

nyc <- read_csv("https://assets.datacamp.com/production/repositories/845/datasets/639a7a3f9020edb51bcbc4bfdb7b71cbd8b9a70e/nyc.csv")
str(nyc)
pairs(nyc)

# Price by Food plot
ggplot(nyc, aes(x = Food, y = Price)) +
  geom_point()

# Price by Food model
lm(Price ~ Food, data = nyc)

nyc %>%
  group_by(East) %>%
  summarize(mean_price = mean(Price))

lm(Price ~ Food + East, data = nyc)

# fit model
lm(Price ~ Food + Service, nyc)

# draw 3D scatterplot
p <- plot_ly(data = nyc, z = ~Price, x = ~Food, y = ~Service, opacity = 0.6) %>%
  add_markers() 

# draw a plane
p %>%
  add_surface(x = ~x, y = ~y, z = ~plane, showscale = FALSE) 


# Collinearity
nyc %>%
  mutate(Price_cents = Price / 100) %>%
  summarize(cor_collinear = cor(Price, Price_cents))
#Multicollinearity?


# draw 3D scatterplot
p <- plot_ly(data = nyc, z = ~Price, x = ~Food, y = ~Service, opacity = 0.6) %>%
  add_markers(color = ~factor(East)) 

# draw two planes
p %>%
  add_surface(x = ~x, y = ~y, z = ~plane0, showscale = FALSE) %>%
  add_surface(x = ~x, y = ~y, z = ~plane1, showscale = FALSE)



