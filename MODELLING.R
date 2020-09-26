##Split data
set.seed(1)
train_indices <- sample(nrow(data_model), round(nrow(data_model)*0.8))
train_data <- data_model[train_indices,]
test_data <- data_model[-train_indices,]

##RIDGE
library(glmnet)

x.train=model.matrix(suicideRate~.,train_data)[,-1]
x.test=model.matrix(suicideRate~.,test_data)[,-1]

# create grid for lambda, fit model using all lambdas
grid=10^seq(-1,-5,length=100) # lambda ranges from 0.1 to 0.00001 
ridge.mod=glmnet(x.train,train_data$suicideRate,alpha=0,lambda=grid) 

# optimize lambda using cross-validation
set.seed(1)
cv.ridge=cv.glmnet(x.train,train_data$suicideRate,alpha=0,lambda=grid)
plot(cv.ridge)
bestlam.r=cv.ridge$lambda.min
mse.r=min(cv.ridge$cvm)
bestlam.r
mse.r

# get coefficents for best model
ridge.coef=predict(ridge.mod,type="coefficients",s=bestlam.r)
ridge.coef

# plotfitted values, compare with actual
fit.ridge=predict(ridge.mod,s=bestlam.r,x.test)
plot(fit.ridge,test_data$suicideRate,pch=19,col="blue")

#mse, R2
mse.ridge <- mean((test_data$suicideRate - fit.ridge)^2)
R2.ridge=cor(fit.ridge,test_data$suicideRate)^2


##LASSO
#fit model using all lambdas
lasso.mod=glmnet(x.train,train_data$suicideRate,alpha=1,lambda=grid)  

# optimize lambda using cross-validation
set.seed(1)
cv.lasso=cv.glmnet(x.train,train_data$suicideRate,alpha=1,lambda=grid)
plot(cv.lasso)
bestlam.l=cv.lasso$lambda.min
mse.l=min(cv.lasso$cvm)
bestlam.l
mse.l

# get coefficents for best model
lasso.coef=predict(lasso.mod,type="coefficients",s=bestlam.l)
lasso.coef

# plotfitted values, compare with actual
fit.lasso=predict(lasso.mod,s=bestlam.r,x.test)
plot(fit.lasso,test_data$suicideRate,pch=19,col="blue", xlab="Predicted values", ylab="Actual values")
points(fit.ridge,test_data$suicideRate,col="red",pch=19)
legend("topleft",legend=c("L1 Regularization",
                          "L2 Regularization"),col=c("blue","red"),pch=c(19,19))

#mse, R2
mse.lasso <- mean((test_data$suicideRate - fit.lasso)^2)
R2.lasso=cor(fit.lasso,test_data$suicideRate)^2