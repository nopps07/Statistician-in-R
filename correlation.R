install.packages("visdat")

library(ggplot2)
library(broom)
library(dplyr)
library(naniar)
library(visdat)


body <- read.csv("https://assets.datacamp.com/production/repositories/1793/datasets/ee832ef6c2fa7036704c53e90dc1e710a3b50dbc/nhanes_bodymeasures.csv")

glimpse(body)

str(body)


miss_summary(body)
vis_dat(body)
vis_miss(body)
gg_miss_var(body)
