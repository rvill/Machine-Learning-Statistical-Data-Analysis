library(rpart)
library("e1071")

data(kyphosis)

train = kyphosis[1:60,]
test = kyphosis[61:81,]

fit <- svm(Kyphosis ~ Age + Number + Start, data=train, kernel = "linear", cost = 1)
fit
summary(fit)

predict = predict(fit, test)
