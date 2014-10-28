banks.data =read.csv(file=file.choose(),header=TRUE)
banks.data
plot(banks.data)
ToExpAssets = banks.data$TotExp.Assets
TotLnsLsesAssets = banks.data$TotLns.Lses.Assets
FinCond = banks.data$Financial.Condition
plot(ToExpAssets, FinCond)
plot(TotLnsLsesAssets, FinCond)

#run logistic regression on dataset, predictor variables = X
glm.fit = glm(banks.data$Financial.Condition~ TotExp.Assets + TotLns.Lses.Assets, data=banks.data, family=binomial(link="logit")) 
summary(glm.fit)
coef(glm.fit) #coefficients
names(glm.fit)
plot(glm.fit)

#predict
glm.probs = predict(glm.fit, type="response")
glm.probs
plot(glm.probs)

#test
newdata = data.frame(TotExp.Assets =0.11, TotLns.Lses.Assets=0.6)
predict(glm.fit,newdata, type="response")
plot(newdata, col="red")

#c.



Class <- predict(glm.fit,type="response")>.8
table(banks.data$Financial.Condition, Class)
10/(10+0) #100% have been correctly predicted
Class2 <- predict(newdata,type="response")>.5

#odds ratio, ref: http://www.ats.ucla.edu/stat/r/dae/logit.htm
exp(coef(glm.fit))

#draw curve
plot(FinCond ~ ToExpAssets, data = banks.data)
lines(banks.data$TotExp.Assets,glm.fit$fitted, type="l", col="red")
?lines

ggplot

#plot(FinCond ~ ToExpAssets, data = banks.data)
#lines(banks.data$TotExp.Assets, glm.fit$fitted.values, type="1" col="red")
#?lines
#?curve

TotExpAssets = banks.data$TotExp.Assets

#test
install.packages("shiny")
library(shiny)
runExample("01_hello")


#c.
Class <- predict()
2.17^2
oddsColumn <- 
  
#Q2. 
install.packages("tree")
library(ISLR)
library(tree)
attach(Carseats)
View(Carseats)
ebay.traindata=read.csv(file=file.choose(),header=TRUE)
ebay.testdata =read.csv(file=file.choose(),header=TRUE)
View(ebay.traindata)

ebay.data = read.csv(file=file.choose(),header=TRUE)
View(ebay.data)
tree_model = tree(ebay.traindata$Competitive~ebay.traindata$ClosePrice + ebay.traindata$OpenPrice + ebay.traindata$sellerRating + ebay.traindata$Duration,ebay.traindata)
plot(tree_model)
text(tree_model,  cex =0.75)
?tree
summary(tree_model)
#check how the model is doing by using the test data
tree_pred = predict(tree_model,ebay.testdata,type="class")

set.seed(101)
alpha <- 0.6 #percentage of training set
inTrain <- sample(1:nrow(ebay.data), alpha*nrow(ebay.data))
train.set <- ebay.data[inTrain,]
test.set <- ebay.data[-inTrain,]
View(inTrain)

?sample

tree.model2 <- tree(Competitive ~ train.set$ClosePrice + train.set$OpenPrice + train.set$sellerRating + train.set$Duration, data = train.set )
tree.model2
plot(tree.model2)
text(tree.model2, cex=0.75)

tree(formula = Competitive ~ train.set$ClosePrice + train.set$OpenPrice + train.set$sellerRating + train.set$Duration, data = train.set ) 

my.prediction <- predict(tree.model2, test.set)
mean(my.prediction != testing_High)
head(my.prediction)

testing_High = train.set$Competitive

#prune the tree

#cross validation to check where to stop pruning
set.seed(3)
cv_tree = cv.tree(tree.model2)
names(cv_tree)
plot(cv_tree$size, cv_tree$dev, type="b")

#split data into testing and training
#set.seed(2)
#train = sample(1:nrow(ebay.data),nrow(ebay.data)/2)
#train =-train
#training_data = ebay.data[train,]
#testing_data = ebay.data[test, ]
#testing_data
#prune the tree
  pruned_model = prune.tree(tree_model, best = 10)
plot(pruned_model)
text(pruned_model, pretty =0)

#check how it's doing again
tree_pred = predict (pruned_model, test.set)
mean(tree_pred != test.set)





############################try again
library(MASS)
library(tree)

attach(Boston)
View(Boston)
ebay.data = read.csv(file=file.choose(),header=TRUE)
set.seed(1)
set.seed(1234)
View(ebay.data)
train =sample(1:nrow(ebay.data), nrow(ebay.data)/6) #nrow(ebay.data)/2)
test = -train
training_data = ebay.data[train,]
testing_data = ebay.data[test,]
#testing_medv = medv[test]
testing_comp = ebay.data$Competitive[test]

tree_model3 = tree(Competitive ~., training_data)
tree_model3
plot(tree_model3)
text(tree_model3) # ,cex = 0.75)

##check how model is doing before pruning, using the testing dataset
tree_pred= predict(tree_model3, testing_data)
mean((tree_pred - testing_comp)^2) #0.113 mean squared error, 11.34%

#let's see if we can get better than 11.34%, let's prune

#1st do cross validation for pruning the tree

cv_tree = cv.tree(tree_model3)
plot(cv_tree$size, cv_tree$dev, type = "b", xlab ="tree size", ylab= "mse")
which.min(cv_tree$dev) #at index 1
cv_tree$size[1] #of size 13 at index 1 is the minimum size

##prune the tree to size 13
pruned_model = prune.tree(tree_model3, best = 13)

plot(pruned_model)
text(pruned_model, cex=0.75)
  


#####Oct. 3, 2014\
##creating dummy variables
ebay.dummydata = read.csv(file=file.choose(),header=TRUE)
View(ebay.dummydata)

#currency dummy
ebay.dummydata$currency.f = factor(ebay.dummydata$currency, labels=c("US", "GBP", "EUR"))
ebay.dummydata$currency.f
#testing mean of currency and open price
tapply(ebay.dummydata$OpenPrice,ebay.dummydata$currency.f, mean)

#category dummy
ebay.dummydata$category.f= factor(ebay.dummydata$Category, labels=c("Music/Movies/Games","Books","Home/Garden","Clothing/Accessories","Antique/Art/Craft", "Collectibles", "Automotive","SportingGoods","Toys/Hobbies", "Computer","EverythingElse","Electronics","Jewelry","Pottery/Glass","Business/Industrial","Coins/Stamps", "Health/Beauty","Photography"))
ebay.dummydata$category.f<-as.factor(ebay.dummydata$Category)
table(ebay.dummydata$category.f)
tapply(ebay.dummydata$OpenPrice,ebay.dummydata$category.f, mean) 




#end day dummy
ebay.dummydata$endDay.f = factor(ebay.dummydata$endDay, labels=c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"))

#duration dummy
ebay.dummydata$duration.f <-as.factor(ebay.dummydata$Duration)
table(ebay.dummydata$duration.f)


library(rpart)
#checking iris as test
data(iris)
View(iris)
set.seed(1234)
ind <- sample(2,nrow(iris),replace=TRUE,prob=c(0.7,0.3))
View(ind)
trainDataIris <- iris[ind==1,]
testDataIris <- iris[ind==2,]
View(trainDataIris)
View(testDataIris)

#on ebay data
set.seed(1234)
ind1 <- sample(2,nrow(ebay.dummydata), replace = TRUE, prob=c(0.6,0.4))
ebay.train <-ebay.dummydata[ind1==1,]
ebay.test <-ebay.dummydata[ind1==2,]

#train a decision tree
library(rpart)
#give strange results, doesn't split with each category named
myFormula <- ebay.dummydata$Competitive ~ ebay.dummydata$currency.f + ebay.dummydata$category.f + ebay.dummydata$endDay.f + ebay.dummydata$duration.f

#add other columsn in formula
myFormula2 <- ebay.dummydata$Competitive ~ ebay.dummydata$currency.f + ebay.dummydata$category.f + ebay.dummydata$endDay.f + ebay.dummydata$duration.f + ebay.dummydata$sellerRating + ebay.dummydata$ClosePrice + ebay.dummydata$OpenPrice  
summary(myFormula2)


ebay_rpart <- rpart(myFormula, data=ebay.train, control=rpart.control(minsplit=50,  maxdepth=7)) #minlength=7,
ebay_rpart <- rpart(myFormula2, data=ebay.train, control=rpart.control(minsplit=50,  minlength=7))
print(ebay_rpart)
plot(ebay_rpart)
text(ebay_rpart, use.n=T)

print(ebay_rpart$cptable)
print(ebay_rpart)

#question d
myFormulaForQuestionD <-  ebay.dummydata$sellerRating~ ebay.dummydata$Competitive + ebay.dummydata$ClosePrice + ebay.dummydata$OpenPrice  + ebay.dummydata$currency
ebay_rpartD <- rpart(myFormulaForQuestionD, data=ebay.train, control=rpart.control(minsplit=50,  minlength=7))
plot(ebay_rpartD)
print(ebay_rpartD)
text(ebay_rpartD, use.n=T)

#select tree with min prediction error
opt <-which.min(ebay_rpart$cptable[,"xerror"])
cp <- ebay_rpart$cptable[opt,"CP"]
ebay_prune <-prune(ebay_rpart,cp=cp)
print(ebay_prune)

plot(ebay_prune)
text(ebay_prune,use.n=T)

#possible plot
plot(ebay.dummydata$OpenPrice, ebay.dummydata$ClosePrice,pch=21, bg=c("red","green3","blue")[unclass(ebay.dummydata$Competitive)])

plot(ebay.dummydata$OpenPrice, ebay.dummydata$ClosePrice,pch=21, bg=c("red","green3","blue")[unclass(ebay.dummydata$Competitive)])


DEXebay_pred <-predict(ebay_prune,newdata=ebay.test)
xlim <- range(ebay.dummydata$Competitive)
plot(DEXebay_pred ~ ebay.dummydata$Competitive, data=ebay.test,xlab="Observed", ylab="Predicted", ylim=xlim,xlim=xlim)
abline(a=0,=1)


plot(ebay.dummydata$OpenPrice ~ ebay.dummydata$sellerRating, data=ebay.test,xlab="Observed", ylab="Predicted", ylim=xlim,xlim=xlim)
ir.tr <- tree(ebay.dummydata$Competitive ~ ebay.dummydata$sellerRating,ebay.dummydata)
ir.tr2 <- tree(ebay.dummydata$ClosePrice ~ ebay.dummydata$OpenPrice,ebay.dummydata)
ir.tr3 <- tree(ebay.dummydata$Category ~ ebay.dummydata$endDay)

ir.tr4 <- tree(ebay.dummydata$Category ~ ebay.dummydata$OpenPrice)


partition.tree(ir.tr, data=ebay.test)
partition.tree(ir.tr2, data=ebay.test)
partition.tree(ir.tr3, data=ebay.test)


#how does the pruned tree perform on the test data set?
tree.pred = predict(ir.tr3, data=ebay.test, type="class")
table(tree.pred, ebay.dummydata$Competitive)
summary(ir.tr3)

tree.pred = predict(ir.tr4, data=ebay.test, type="class")
summary(ir.tr4)

