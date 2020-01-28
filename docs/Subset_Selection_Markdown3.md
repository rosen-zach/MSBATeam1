Subset Selection
================
Omar Hanley, Zachary Rosen, Todd Schaffer, and Yannan Zhu
January 29, 2020

# Lab 1: Subset Selection Methods
</br>

<div style="margin-bottom:100px;">
## Best Subset Selection
Exhaustively, all possible *p* predictors are fit with a least squares regression line with the goal of finding a "best" fit
</div>

<div style="margin-bottom:100px;">
### Preprocessing
As with any machine learning problem, we must first load packages, examine and preprocess the data
```{r setup, warning=FALSE, results="hide"}
rm(list=ls())
library(ISLR)
library(leaps)
```
```{r, comment= NA, collapse=TRUE}
head(Hitters)

dim(Hitters)

sum(is.na(Hitters$Salary))
```

We have to remove NA values because they will not work with the regsubsets() function
```{r, comment= NA, warning=FALSE, collapse=TRUE}
Hitters=na.omit(Hitters)
dim(Hitters)

sum(is.na(Hitters))
```

Finally, we have removed NA values and reduced the number of observations from 322 to 263
</div>

<div style="margin-bottom:100px;">
### Fitting A Model
We assign the model to the variable regfit.full and call the regsubsets() function to collectively exhaust all possible fits
```{r, comment= NA, warning=FALSE, collapse=TRUE}
regfit.full = regsubsets(Salary~.,data= Hitters)

summary(regfit.full)
```

The default amount of predictors to be included in the regsubsets() function is 8. Hence the "1 subsets of each size up to 8" statement in the summary. Additionally, the asterisks indicate which features were chosen to best predict the response variable
<br><br><br><br>

Now, use the parameter nvmax=19 in the regsubsets() function to include all possible features
```{r, comment= NA, warning=FALSE, collapse=TRUE}
regfit.full=regsubsets(Salary~.,data=Hitters,nvmax=19)
reg.summary=summary(regfit.full)

names(reg.summary)
```
</div>

<div style="margin-bottom:100px;">
### Plotting the Data
Plot the number of variables against the error predictions  

* This allows you to visually notice where adding features/information becomes invaluable
```{r, comment= NA, warning=FALSE, collapse=TRUE, fig.height=7}
par(mfrow=c(2,2))
plot(reg.summary$rss,xlab="Number of Variables",ylab="RSS",type="l")
points(which.min(reg.summary$rss),reg.summary$rss[which.min(reg.summary$rss)],
       col="red",cex=2,pch=20)
plot(reg.summary$adjr2,xlab="Number of Variables",ylab="Adjusted RSq",type="l")
points(which.max(reg.summary$adjr2),reg.summary$adjr2[which.max(reg.summary$adjr2)],
       col="red",cex=2,pch=20)
plot(reg.summary$cp,xlab="Number of Variables",ylab="Cp",type='l')
points(which.min(reg.summary$cp),reg.summary$cp[which.min(reg.summary$cp)],
       col="red",cex=2,pch=20)
plot(reg.summary$bic,xlab="Number of Variables",ylab="BIC",type='l')
points(which.min(reg.summary$bic),reg.summary$bic[which.min(reg.summary$bic)],
       col="red",cex=2,pch=20)
mtext("Where does adding features become ineffective?", 
      side=3,line=-2,outer=TRUE, font=2, cex=1.25)
which.min(reg.summary$rss)

which.max(reg.summary$adjr2)

which.min(reg.summary$cp)

which.min(reg.summary$bic)
```

For example, although the minimum/maximums weren't exactly 6, when the number of variables reached approximately 6, the value of adding more became drastically less valuable across all plots
<br><br><br><br>

Next, we will plot the errors against their respective variables  

* Black means the variable is included in the model associated with that level of error
```{r, comment= NA, warning=FALSE, collapse=TRUE}
### ask professor about which model to use and why
plot(regfit.full,scale="r2", main="R-Squared")
mtext("Features", side=1, line=-2, outer=TRUE)
plot(regfit.full,scale="adjr2", main="Adjusted R-Squared")
mtext("Features", side=1, line=-2, outer=TRUE)
plot(regfit.full,scale="Cp", main="Cp")
mtext("Features", side=1, line=-2, outer=TRUE)
plot(regfit.full,scale="bic", main="BIC")
mtext("Features", side=1, line=-2, outer=TRUE)
```

While this is informative, the information in each of these plots can be very confusing to interpret

* Our recommendation would be to exclude this from any formal presentation
* Alternatively, using the summary() and names() functions to determine which variables are included at certain a point is easier
```{r, comment= NA, warning=FALSE, collapse=TRUE}
regfit.summary <- summary(regfit.full)
names(regfit.summary)
```
</div>

<div style="margin-bottom:100px;">
## Stepwise Selection

Regarding code, the only difference between stepwise and best subset is stating a method parameter within the regsubsets() function  

* For example, method="forward" means forward stepwise selection will be used as the method for fitting this model
</div>

<div style="margin-bottom:100px;">
### Forward and Backward Stepwise Selection
```{r, comment= NA, warning=FALSE, collapse=TRUE}
regfit.fwd=regsubsets(Salary~.,data=Hitters,nvmax=19,method="forward")
regfit.bwd=regsubsets(Salary~.,data=Hitters,nvmax=19,method="backward")

coef(regfit.full,7)

coef(regfit.fwd,7)

coef(regfit.bwd,7)
```

The limitations to forward and backward stepwise selection are, they do not collectively exhaust all possibilities 

* Additionally, you can achieve different results with each model choosing the same amount of features
* On the other hand, forward and backward stepwise selection are significantly less computationally complex  
</div>

<div style="margin-bottom:100px;">
## Choosing Among Models
Now that we've gone through indirectly calculating test errors, we use two methods to calculate direct test errors
</div>

<div style="margin-bottom:100px;">
### Hold-Out Method

Directly calculating a test error requires the data being split into a test and train set

* We are "holding out" a test set of data in order to validate our model
```{r, comment= NA, warning=FALSE, collapse=TRUE}
set.seed(1)
train = sample(c(TRUE,FALSE), nrow(Hitters),rep=TRUE)
test=(!train)
```


```{r, comment= NA, warning=FALSE, collapse=TRUE}
regfit.best = regsubsets(Salary~.,data=Hitters[train,],nvmax=19)
test.mat = model.matrix(Salary~.,data=Hitters[test,])
val.errors=rep(NA,19)
for(i in 1:19){
  coefi=coef(regfit.best,id=i)
  pred=test.mat[,names(coefi)]%*%coefi
  val.errors[i]=mean((Hitters$Salary[test]-pred)^2)
}
```

* The coef() function displays all the model coefficients associated with the model passed as an argument
* This is useful because there is no method in R to form a prediction for regsubsets()
</br></br></br>

After saving our validate errors in the variable val.errors, we plot the RMSE of the models associated with each number of variables
```{r, comment= NA, warning=FALSE, collapse=TRUE}
plot(sqrt(val.errors), ylab="Root MSE", type="b", xlab="Number of Features", main="Best Subset Error")
which.min(val.errors)
```

We then define a function to predict our outcomes for reproducibility
```{r, comment= NA, warning=FALSE, collapse=TRUE}
coef(regfit.best,10)

predict.regsubsets=function(object,newdata,id,...){
  form=as.formula(object$call[[2]])
  mat=model.matrix(form,newdata)
  coefi=coef(object,id=id)
  xvars=names(coefi)
  mat[,xvars]%*%coefi
}
```
</div>

<div style="margin-bottom:100px;">
## Cross-Validation Method

The data is now split into *k* number of groups and our model is run *k*-1 times on *k*-1 groups, holding a different group each time to use as the test set

* For our example, we use k=10, meaning the data is evently split into 10 groups
* One group may contain more samples if the observations are not evenly divisible by *k*
```{r, comment= NA, warning=FALSE, collapse=TRUE}
regfit.best=regsubsets(Salary~.,data=Hitters,nvmax=19)
coef(regfit.best,10)
k=10
set.seed(1)
folds=sample(1:k,nrow(Hitters),replace=TRUE)
cv.errors=matrix(NA,k,19, dimnames=list(NULL, paste(1:19)))
for(j in 1:k){
  best.fit=regsubsets(Salary~.,data=Hitters[folds!=j,],nvmax=19)
  for(i in 1:19){
    pred=predict(best.fit,Hitters[folds==j,],id=i)
    cv.errors[j,i]=mean( (Hitters$Salary[folds==j]-pred)^2)
  }
}
mean.cv.errors=apply(cv.errors,2,mean)
mean.cv.errors
par(mfrow=c(1,1))
plot(mean.cv.errors,type='b', main="Cross-Validated Error")
reg.best=regsubsets(Salary~.,data=Hitters, nvmax=19)
coef(reg.best,11)

```

One disadvantage to using Cross-Validation is its computational complexity 

* The training algorithm must run *k* times, meaning *k* times as much computation
</div>

# Applied Exercise: Exercise 6.8
</br>

<div style="margin-bottom:100px;">
In this exercise, we will generate simulated data and then use this data to perform best subset selection
</div>

<div style="margin-bottom:100px;">
a)  Use the rnorm() function to generate a predictor X of length n=100, as well as a noise vector \(\epsilon\) of length n=100
```{r comment= NA, warning=FALSE, collapse=TRUE}
set.seed(1)
X = rnorm(100)
eps = rnorm(100)
```
</div>

<div style="margin-bottom:100px;">
b) Generate a response vector Y of length n=100 according to the model
Y = β0 + β1X + β2X2 + β3X3 + \(\epsilon\), where β0, β1, β2, andβ3 are constants of your choice
* We are selecting β0=3, β1=2, β2=−3 and β3=0.3
```{r comment= NA, warning=FALSE, collapse=TRUE}
beta0 = 3
beta1 = 2
beta2 = -3
beta3 = 0.3
Y = beta0 + beta1 * X + beta2 * X^2 + beta3 * X^3 + eps
```
</div>

<div style="margin-bottom:100px;">
c) Use regsubsets to select best model having polynomial of X of degree 10. What is the best model obtained according to Cp, BIC, and adjusted R-Squared?
```{r comment= NA, warning=FALSE, collapse=TRUE}
data.full = data.frame(y=Y, x=X)
mod.full = regsubsets(y ~ poly(x, 10, raw=T), data=data.full, nvmax=10)
mod.summary = summary(mod.full)

which.min(mod.summary$cp)
which.min(mod.summary$bic)
which.max(mod.summary$adjr2)
```
   
* Show some plots to provide evidence for your answer, and report the coeﬃcients of the best model obtained
```{r comment= NA, warning=FALSE, collapse=TRUE, fig.height=10}
par(mfrow=c(3,1))
plot(mod.summary$cp, xlab="Subset Size", ylab="Cp", type="l")
points(3, mod.summary$cp[3], col="red", lwd=7)

plot(mod.summary$bic, xlab="Subset Size", ylab="BIC", type="l")
points(3, mod.summary$bic[3], col="red", lwd=7)

plot(mod.summary$adjr2, xlab="Subset Size", ylab="Adjusted R2", type="l")
points(3, mod.summary$adjr2[3], col="red", lwd=7)
mtext("Best Subset Selection",
      side=3,line=-2,outer=TRUE, font=2, cex=1.25)
coefficients(mod.full, id=3)
```

We find that with Cp, BIC and Adjusted R-Squared critera, 3 is picked as the best subset size across all errors.
All statistics pick X7 over X3. The remaining coefficients are quite close to β s
</div>

<div style="margin-bottom:100px;">
d) Repeat (c), using forward stepwise selection and backwards stepwise selection 
```{r comment= NA, warning=FALSE, collapse=TRUE}
mod.fwd = regsubsets(y ~ poly(x, 10, raw=T), 
                     data=data.full, nvmax=10, method="forward")
mod.bwd = regsubsets(y ~ poly(x, 10, raw=T), 
                     data=data.full, nvmax=10, method="backward")
fwd.summary = summary(mod.fwd)
bwd.summary = summary(mod.bwd)
which.min(fwd.summary$cp)

which.min(bwd.summary$cp)

which.min(fwd.summary$bic)

which.min(bwd.summary$bic)

which.max(fwd.summary$adjr2)

which.max(bwd.summary$adjr2)
```

Plot the statistics
```{r comment= NA, warning=FALSE, collapse=TRUE, fig.height=10}
par(mfrow=c(3, 1))
plot(fwd.summary$cp, xlab="Subset Size", ylab="Cp", type="l")
points(3, fwd.summary$cp[3], col="red", lwd=7)
plot(fwd.summary$bic, xlab="Subset Size", ylab="BIC", type="l")
points(3, fwd.summary$bic[3], col="red", lwd=7)
plot(fwd.summary$adjr2, xlab="Subset Size", ylab="Adjusted R2", type="l")
points(3, fwd.summary$adjr2[3], col="red", lwd=7)
mtext("Forward Stepwise Selection", 
      side=3,line=-2,outer=TRUE, font=2, cex=1.25)
```
```{r comment= NA, warning=FALSE, collapse=TRUE, fig.height=10}
par(mfrow=c(3, 1))
plot(bwd.summary$cp, xlab="Subset Size", ylab="Cp", type="l")
points(3, bwd.summary$cp[3], col="red", lwd=7)
plot(bwd.summary$bic, xlab="Subset Size", ylab="BIC", type="l")
points(3, bwd.summary$bic[3], col="red", lwd=7)
plot(bwd.summary$adjr2, xlab="Subset Size", ylab="Adjusted R2", type="l")
points(4, bwd.summary$adjr2[4], col="red", lwd=7)
mtext("Backward Stepwise Selection", 
      side=3,line=-2,outer=TRUE, font=2, cex=1.25)
```

We see that all statistics pick 3 variable models except backward stepwise with adjusted R2. Here are the coefficients:
```{r comment= NA, warning=FALSE, collapse=TRUE}
coefficients(mod.fwd, id=3)

coefficients(mod.bwd, id=3)

coefficients(mod.fwd, id=4)
```

Here, forward stepwise picks X7 over X3. Backward stepwise with 3 variables picks X9 while backward stepwise with 4 variables picks X4 and X7. All other coefficients are close to β s
</div>
