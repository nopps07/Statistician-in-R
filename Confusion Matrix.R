getwd()
install.packages("caret")
install.packages("e1071")

compas = read.csv("compas.csv")

library(ggplot2)
library(caret)
library(e1071)

str(compas)


data

prediction <- factor(compas$is_recidivist)
reference <- factor(compas$prediction)
table(prediction, reference)


ex1 <- confusionMatrix(prediction, reference, positive = "1")
ex1

x <- c(1, 2, 3, 4)

x <- c("red", "blue", "yellow", "orange", "green", "purple")
y <- x[2,]
y