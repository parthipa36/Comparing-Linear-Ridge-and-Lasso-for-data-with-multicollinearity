setwd("C:\\Users\\PARTHI vs BHARATHI\\Downloads\\PRAXIS\\A Term 2\\MI\\Paper")

library(glmnet)

df = read.csv("winequality-red.csv")

View(df)


dim(df)
colnames(df)


## Correlation among the attributes:
View(cor(df))

# install.packages("corrplot")
library(corrplot)

corrplot(cor(df), type = "lower", order = "hclust", 
         tl.col = "black", tl.srt = 45)




# data splitting test 50% and train 50%: 
set.seed(1)

rand = sample(1:nrow(df),as.integer(dim(df)[1] * 0.5))
train = df[rand, ]
test = df[-rand, ]

colnames(train)


## Linear model fitting: 

m1 <- lm(quality ~ ., train)
summary(m1)
summary(m1)$r.square

#TRAIN AND TEST ACCURACY
sum(m1$residuals^2)
pred = predict(m1, newdata=test)
sum((pred-test$quality)^2)

## Checking for multicollinearity:
library(car)

vif(m1)

View(vif(m1))


## generating a grid of lambda values:

lambda.grid = 10^seq(5, -2, length=100)

x <- model.matrix(quality ~ 0 + . , data=train)


## Ridge model applying for different lambda values:  

ridge.mod = glmnet(x, train$quality, alpha=0, lambda=lambda.grid)

plot(ridge.mod, xvar='lambda')



## LASSO model applying for different lambda values:

lasso.mod = glmnet(x, train$quality, alpha=1, lambda=lambda.grid)

plot(lasso.mod, xvar='lambda')



set.seed(1)

ridge.cv.out = cv.glmnet(x, train$quality, alpha=0)

set.seed(1)

lasso.cv.out = cv.glmnet(x, train$quality, alpha=1) 


plot(ridge.cv.out,col="blue") 
plot(lasso.cv.out,col="blue") 



## Best lambda for Ridge
ridge.bestlam = ridge.cv.out$lambda.min
ridge.bestlam

## Best lambda for LASSO
lasso.bestlam = lasso.cv.out$lambda.min
lasso.bestlam



## coefficients for Ridge and LASSO models using their best lambdas:

ridge.mod.best = glmnet(x, train$quality, alpha=0, lambda=ridge.bestlam)
coef(ridge.mod.best)

lasso.mod.best = glmnet(x, train$quality, alpha=1, lambda=lasso.bestlam)
coef(lasso.mod.best)



# RSS on the train set and then move to the test set.

x.test <- model.matrix(quality ~ 0 + . , data=test)

# Errors in Linear model
y.lm.train = predict(m1, newdata = data.frame(x))
sum((y.lm.train - train$quality)^2)
y.lm.test = predict(m1, newdata = data.frame(x.test))
sum((y.lm.test - test$quality)^2)

# Errors in Ridge model
y.ridge.best.train = predict(ridge.mod.best, newx=x)
sum((y.ridge.best.train - train$quality)^2)
y.ridge.best.test = predict(ridge.mod.best, newx=x.test)
sum((y.ridge.best.test - test$quality)^2)

# Errors in LASSO model
y.lasso.best.train = predict(lasso.mod.best, newx=x)
sum((y.lasso.best.train - train$quality)^2)
y.lasso.best.test = predict(lasso.mod.best, newx=x.test)
sum((y.lasso.best.test - test$quality)^2)













