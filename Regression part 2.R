library(ggplot2)

mod <- lm(hwy ~ displ + factor(year), data = mpg)
mod

ggplot(mpg, aes(x = year, y = hwy)) +
  geom_line()

