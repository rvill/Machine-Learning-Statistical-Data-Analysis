
#2.8

# Read in the training data
X <- as.matrix(read.table(gzfile("/home/rvill/Desktop/MIT_Spring2014/15.077-StatData/pset_4/zip.train.gz")))
X
?as
?matrix
y2or3 <- which(X[, 1] == 2 | X[, 1] == 3)
X.train <- X[y2or3, -1]
y.train <- X[y2or3, 1] == 3

# Read in the test data
X <- as.matrix(read.table(gzfile("/home/rvill/Desktop/MIT_Spring2014/15.077-StatData/pset_4/zip.test.gz")))
y2or3 <- which(X[, 1] == 2 | X[, 1] == 3)
X.test <- X[y2or3, -1]
y.test <- X[y2or3, 1] == 3

drawDigit <- function(x) {
  for (i in 1:16) {
    for (j in 1:16) {
      color <- gray(1 - (1 + x[(i - 1) * 16 + j])/2)
      grid.rect(j, 17 - i, 1, 1, default.units = "native",
                gp = gpar(col = color, fill = color))
    }
  }
}


library(grid)
grid.newpage()
pushViewport(viewport(xscale = c(0, 6), yscale = c(0, 6)))
for (k in 1:25) {
  pushViewport(viewport(x = (k - 1)%%5 + 1, y = 5 - floor((k - 1)/5), width = 1, 
                        height = 1, xscale = c(0, 17), yscale = c(0, 17),
                        default.units = "native"))
  drawDigit(X.train[k, ])
  popViewport(1)
}
popViewport(1)

?lm

# Classification by linear regression
L <- lm(y.train ~ X.train)
yhat <- (cbind(1, X.test) %*% L$coef) >= 0.5
L.error <- mean(yhat != y.test)


# Classification by k-nearest neighbors
library(class)
k <- c(1, 3, 5, 7, 15)
k.error <- rep(NA, length(k))
for (i in 1:length(k)) {
  yhat <- knn(X.train, X.test, y.train, k[i])
  k.error[i] <- mean(yhat != y.test)
}

# Compare results
error <- matrix(c(L.error, k.error), ncol = 1)
colnames(error) <- c("Error Rate")
rownames(error) <- c("Linear Regression", paste("k-NN with k =", k))
error


##                   Error Rate
## Linear Regression    0.04121
## k-NN with k = 1      0.02473
## k-NN with k = 3      0.03022
## k-NN with k = 5      0.03022
## k-NN with k = 7      0.03297
## k-NN with k = 15     0.03846



plot(c(1, 15), c(0, 1.1 * max(error)), type = "n", main = "classifier comparison", 
     ylab = "Error Rate", xlab = "k")
abline(h = L.error, col = 2, lty = 3)
points(k, k.error, col = 4)
lines(k, k.error, col = 4, lty = 2)

#3.17
install.packages("ElemStatLearn")
library(help=ElemStatLearn)
library(ElemStatLearn)
data(spam)
?spam
View(spam)
#test
nms <- read.table("http://www.biostat.jhsph.edu/~hcorrada/PracticalML/Data/spam_names.txt", 
                  stringsAsFactors=FALSE)
View(nms)

spam
###################

#ref1: file:///home/rvill/Downloads/ESL-Solutions.pdf
#ref2: http://www.unt.edu/rss/class/Jon/Benchmarks/CrossValidation1_JDS_May2011.pdf
#ref3: http://faculty.chicagobooth.edu/matt.taddy/teaching/02Regression.pdf

spam.data =read.csv(file=file.choose(),header=TRUE)
View(spam.data)
spam.train=read.table(file=file.choose(), header=TRUE)
View(spam.train)


#Least Squares Regression
mod.ls <- lm(X1 ~. -1, spam.data)
summary(mod.ls)
coef(mod.ls)
names(mod.ls)
qr(mod.ls)
install.packages('DAAG') #contains 3 functions: k-fold cross validation, cv.lm for simple linear regression, and CVlm, for multiple linear regression, CVbinary for logistic regression models
library(DAAG)

val.daag <- CVlm(df=spam.data, m= 10, form.lm = formula(X1 ~. -1, spam.data))
?spam

#Ridge Regression
mod.ridge <- lm.ridge(X1 ~ ., spam.data)
install.packages("ridge")
library(ridge)
mod.ridge <- linearRidge(X1 ~., spam.data)

summary(mod.ridge)
names(mod.ridge)
mod.ridge$xm
plot(lm.ridge(X1 ~ ., spam.data,
              lambda = seq(0,0.1,0.001)))

print(mod.ridge, digits = max(3, getOption("digits") - 3),
     signif.stars = getOption("show.signif.stars"), all.coef = FALSE)

  
  
#Lasso
#ref: http://www.utstat.utoronto.ca/reid/sta414/Table33R.txt
library(lars)
mod.lars = lars(as.matrix(spam.data[,1:8]),spam.data$X1)
summary(mod.lars)
names(mod.lars)
plot(mod.lars, plottype="coefficients")

#mse
mse = function(x,y) { mean((x-y)^2)}
msese = function(x,y) { sqrt(var((x-y)^2)) }



##################
################
#ref: https://github.com/ajtulloch/Elements-of-Statistical-Learning/tree/master/ElemStatLearnCode
install.packages("ProjectTemplate")
install.packages("lars")
install.packages("MASS")
install.packages("pls")

library("ProjectTemplate")
create.project('3.17')
setwd('/home.3.17')
load.project()

library("lars") # For least-angle and lasso
library("MASS") # For ridge
library("pls") # For PLS and PCR

spam.train <- read.table()

mod.ls <- lm(X1 ~ . - 1, spam.train)
mod.ridge <- lm.ridge(X1 ~ ., spam.train)
mod.pcr <- pcr(formula=X1 ~ ., data=spam.train, validation="CV")
mod.plsr <- plsr(formula=X1 ~ ., data=spam.train, validation="CV")
mod.lars <- lars(as.matrix(spam.train[,1:ncol(spam.train) - 1]), 
                 spam.train[,ncol(spam.train)], 
                 type="lar")
mod.lasso <- lars(as.matrix(spam.train[,1:ncol(spam.train) - 1]), 
                  spam.train[,ncol(spam.train)], 
                  type="lasso")

mods.coeffs <- data.frame(ls=mod.ls$coef,
                          ridge=mod.ridge$coef,
                          lasso=mod.lasso$beta[10,],
                          pcr=mod.pcr$coef[,,10],
                          plsr=mod.plsr$coef[,,10]
)

install.packages("reshape")
library("ggplot2")
library("reshape")

mods.coeffs$xs = row.names(mods.coeffs)
plot.data <- melt(mods.coeffs, id="xs")

ggplot(data=plot.data, 
       aes(x=factor(xs), 
           y=value, 
           group=variable, 
           colour=variable)) + 
  geom_line() + 
  geom_point() +
  xlab("Factor") + 
  ylab("Regression Coefficient") +
  opts(title = "Estimated coefficients for regression methods on spam data",
       axis.ticks = theme_blank(), 
       axis.text.x = theme_blank()) +
  scale_colour_hue(name="Regression Method",
                   labels=c("OLS",
                            "Ridge",
                            "Lasso",
                            "PCR",
                            "PLS")
  )

ggsave(file.path('graphs', 'exercise_3_17.pdf'))
ggsave(file.path('graphs', 'exercise_3_17.png'))


#######
#3.29

#ref: http://www.stat.sc.edu/~hitchcock/ridgeRexample704.txt
#ref2: http://rtutorialseries.blogspot.com/2009/12/r-tutorial-series-multiple-linear.html
#ref3: http://dl.dropboxusercontent.com/u/10246536/Web/RTutorialSeries/example_multipleRegression.txt
#load data 
datavar <- read.csv("/home/rvill/Desktop/MIT_Spring2014/15.077-StatData/pset_4/dataset_multipleRegression.csv")
attach(data)

datavar

select(lm.ridge(formula = ROLL ~ UNEM + HGRAD, data = datavar))
datavar.ridge.reg <- lm.ridge(ROLL~  UNEM + HGRAD, lambda = .019)

#show results
datavar.ridge.reg 

#replicate X* = X
datavar2 <- read.csv("/home/rvill/Desktop/MIT_Spring2014/15.077-StatData/pset_4/dataset_copyvars-3.29.csv")
attach(datavar2)
datavar2
select(lm.ridge(formula = ROLL ~ UNEM + UNEM2 + HGRAD + HGRAD2, data = datavar2))
datavar2.ridge.reg <- lm.ridge(ROLL~   + UNEM2 + HGRAD + HGRAD2, lambda = .019)

#show results
datavar2.ridge.reg



########
#compare with to original least-squares fit

X.matrix <- cbind(rep(1,length=length(ROLL)), UNEM, HGRAD)

?rep #replicate elements of vectors and lists

fitted.vals <- X.matrix %*% c(43.840113, 2.117493, -0.959731)
#etting the sse for the ridge-regression fit
sse.ridge <- sum (datavar-fitted.vals^2)
sse.ridge

#the original least-squares fit
datavar.reg <- lm(ROLL ~ UNEM + HGRAD)
datavar.reg
#getting the sse for the original least-squares fit:
sum(resid(datavar.reg)^2)
??MASS




