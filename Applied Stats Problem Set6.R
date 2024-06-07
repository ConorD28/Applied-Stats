#Problem Set 6
#1.
sparrows <- read.csv(file.choose(), stringsAsFactors = T)
head(sparrows)
summary(sparrows)
library(ggplot2)
round(cor(sparrows[,-c(1,2)]),2)
ggplot(sparrows, aes(FL, HL)) + geom_point(size = 4)
ggplot(sparrows, aes(FL, HL, color = Status)) + geom_point(size = 4)+ facet_wrap(~AG)

#2.
summary(glm(Status ~ AG + TL + AE + WT + BH + HL + FL + TT + SK + KL, sparrows, family = "binomial"))
summary(glm(Status ~ TL + AE + WT + BH + HL + FL + TT + SK + KL, sparrows, family = "binomial"))
summary(glm(Status ~ TL + AE + WT + BH + HL + TT + SK + KL, sparrows, family = "binomial"))
summary(glm(Status ~ TL + AE + WT + BH + HL + SK + KL, sparrows, family = "binomial"))
summary(glm(Status ~ TL + WT + BH + HL + SK + KL, sparrows, family = "binomial"))
summary(glm(Status ~ TL + WT + BH + HL + KL, sparrows, family = "binomial"))
summary(glm(Status ~ TL + WT + HL + KL, sparrows, family = "binomial"))

#3.
install.packages("glmnet")
library(glmnet)
x <- model.matrix(Status ~ ., sparrows)[,-1]
y <- sparrows$Status
grid <- 10^seq(10, -2, length = 100)
cv.out <- cv.glmnet(x, y, alpha = 1, lambda = grid, family = "binomial")
plot(cv.out)
bestlam <- cv.out$lambda.min
out <- glmnet(x, y, alpha = 1, lambda = grid, family = "binomial")
lasso.coef <- predict(out, type = "coefficients", s = bestlam)
lasso.coef

#4. and 5.
summary(glm(Status ~ TL + WT + HL + KL, sparrows, family = "binomial"))
summary(glm(Status ~ TL + AE + WT + BH + HL + TT + SK + KL, sparrows, family = "binomial"))
summary(glm(Status ~ AG + TL + AE + WT + BH + HL + FL + TT + SK + KL, sparrows, family = "binomial"))
