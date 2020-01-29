Subset Selection
================
Omar Hanley, Zachary Rosen, Todd Schaffer, and Yannan Zhu
January 29, 2020

# Lab 1: Subset Selection Methods

<div style="margin-bottom:100px;">

## Best Subset Selection

Exhaustively, all possible *p* predictors are fit with a least squares
regression line with the goal of finding a “best” fit

</div>

<div style="margin-bottom:100px;">

### Preprocessing

As with any machine learning problem, we must first load packages,
examine and preprocess the data

``` r
rm(list=ls())
dim(Hitters)
[1] 322  20

sum(is.na(Hitters$Salary))
[1] 59
```

We have to remove NA values because they will not work with the
regsubsets() function

``` r
Hitters=na.omit(Hitters)
dim(Hitters)
[1] 263  20

sum(is.na(Hitters))
[1] 0
```

Finally, we have removed NA values and reduced the number of
observations from 322 to 263

</div>

<div style="margin-bottom:100px;">

### Fitting A Model

We assign the model to the variable regfit.full and call the
regsubsets() function to collectively exhaust all possible fits

``` r
regfit.full = regsubsets(Salary~.,data =  Hitters)

summary(regfit.full)
Subset selection object
Call: regsubsets.formula(Salary ~ ., data = Hitters)
19 Variables  (and intercept)
           Forced in Forced out
AtBat          FALSE      FALSE
Hits           FALSE      FALSE
HmRun          FALSE      FALSE
Runs           FALSE      FALSE
RBI            FALSE      FALSE
Walks          FALSE      FALSE
Years          FALSE      FALSE
CAtBat         FALSE      FALSE
CHits          FALSE      FALSE
CHmRun         FALSE      FALSE
CRuns          FALSE      FALSE
CRBI           FALSE      FALSE
CWalks         FALSE      FALSE
LeagueN        FALSE      FALSE
DivisionW      FALSE      FALSE
PutOuts        FALSE      FALSE
Assists        FALSE      FALSE
Errors         FALSE      FALSE
NewLeagueN     FALSE      FALSE
1 subsets of each size up to 8
Selection Algorithm: exhaustive
         AtBat Hits HmRun Runs RBI Walks Years CAtBat CHits CHmRun CRuns
1  ( 1 ) " "   " "  " "   " "  " " " "   " "   " "    " "   " "    " "  
2  ( 1 ) " "   "*"  " "   " "  " " " "   " "   " "    " "   " "    " "  
3  ( 1 ) " "   "*"  " "   " "  " " " "   " "   " "    " "   " "    " "  
4  ( 1 ) " "   "*"  " "   " "  " " " "   " "   " "    " "   " "    " "  
5  ( 1 ) "*"   "*"  " "   " "  " " " "   " "   " "    " "   " "    " "  
6  ( 1 ) "*"   "*"  " "   " "  " " "*"   " "   " "    " "   " "    " "  
7  ( 1 ) " "   "*"  " "   " "  " " "*"   " "   "*"    "*"   "*"    " "  
8  ( 1 ) "*"   "*"  " "   " "  " " "*"   " "   " "    " "   "*"    "*"  
         CRBI CWalks LeagueN DivisionW PutOuts Assists Errors NewLeagueN
1  ( 1 ) "*"  " "    " "     " "       " "     " "     " "    " "       
2  ( 1 ) "*"  " "    " "     " "       " "     " "     " "    " "       
3  ( 1 ) "*"  " "    " "     " "       "*"     " "     " "    " "       
4  ( 1 ) "*"  " "    " "     "*"       "*"     " "     " "    " "       
5  ( 1 ) "*"  " "    " "     "*"       "*"     " "     " "    " "       
6  ( 1 ) "*"  " "    " "     "*"       "*"     " "     " "    " "       
7  ( 1 ) " "  " "    " "     "*"       "*"     " "     " "    " "       
8  ( 1 ) " "  "*"    " "     "*"       "*"     " "     " "    " "       
```

The default amount of predictors to be included in the regsubsets()
function is 8. Hence the “1 subsets of each size up to 8” statement in
the summary. Additionally, the asterisks indicate which features were
chosen to best predict the response variable <br><br><br><br>

Now, use the parameter nvmax=19 in the regsubsets() function to include
all possible features

``` r
regfit.full=regsubsets(Salary~.,data=Hitters,nvmax=19)
reg.summary=summary(regfit.full)

names(reg.summary)
[1] "which"  "rsq"    "rss"    "adjr2"  "cp"     "bic"    "outmat" "obj"   
```

</div>

<div style="margin-bottom:100px;">

### Plotting the Data

Plot the number of variables against the error terms

  - This allows you to visually notice where adding features/information
    becomes invaluable

<!-- end list -->

``` r
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
```

![](Subset_Selection_Markdown3_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
which.min(reg.summary$rss)
[1] 19

which.max(reg.summary$adjr2)
[1] 11

which.min(reg.summary$cp)
[1] 10

which.min(reg.summary$bic)
[1] 6
```

For example, although the minimum/maximums weren’t exactly 6, when the
number of variables reached approximately 6, the value of adding more
became drastically less valuable across all plots <br><br><br><br>

Next, we will plot the error terms against their respective variables

  - Black means the variable is included in the model associated with
    that level of error

<!-- end list -->

``` r
### ask professor about which model to use and why
plot(regfit.full,scale="r2", main="R-Squared")
mtext("Features", side=1, line=-2, outer=TRUE)
```

![](Subset_Selection_Markdown3_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
plot(regfit.full,scale="adjr2", main="Adjusted R-Squared")
mtext("Features", side=1, line=-2, outer=TRUE)
```

![](Subset_Selection_Markdown3_files/figure-gfm/unnamed-chunk-6-2.png)<!-- -->

``` r
plot(regfit.full,scale="Cp", main="Cp")
mtext("Features", side=1, line=-2, outer=TRUE)
```

![](Subset_Selection_Markdown3_files/figure-gfm/unnamed-chunk-6-3.png)<!-- -->

``` r
plot(regfit.full,scale="bic", main="BIC")
mtext("Features", side=1, line=-2, outer=TRUE)
```

![](Subset_Selection_Markdown3_files/figure-gfm/unnamed-chunk-6-4.png)<!-- -->

While this is informative, the information in each of these plots can be
very confusing to interpret

  - Our recommendation would be to exclude this from any formal
    presentation
  - Alternatively, using the summary() and names() functions to
    determine which variables are included at certain a point is easier

<!-- end list -->

``` r
regfit.summary <- summary(regfit.full)
names(regfit.summary)
[1] "which"  "rsq"    "rss"    "adjr2"  "cp"     "bic"    "outmat" "obj"   
```

</div>

<div style="margin-bottom:100px;">

## Stepwise Selection

Regarding code, the only difference between stepwise and best subset is
stating a method parameter within the regsubsets() function

  - For example, method=“forward” means forward stepwise selection will
    be used as the method for fitting this model

</div>

<div style="margin-bottom:100px;">

### Forward and Backward Stepwise Selection

``` r
regfit.fwd=regsubsets(Salary~.,data=Hitters,nvmax=19,method="forward")
regfit.bwd=regsubsets(Salary~.,data=Hitters,nvmax=19,method="backward")
# PUT GRAPHS HERE TO SHOW WHY 7

coef(regfit.full,7)
 (Intercept)         Hits        Walks       CAtBat        CHits 
  79.4509472    1.2833513    3.2274264   -0.3752350    1.4957073 
      CHmRun    DivisionW      PutOuts 
   1.4420538 -129.9866432    0.2366813 

coef(regfit.fwd,7)
 (Intercept)        AtBat         Hits        Walks         CRBI 
 109.7873062   -1.9588851    7.4498772    4.9131401    0.8537622 
      CWalks    DivisionW      PutOuts 
  -0.3053070 -127.1223928    0.2533404 

coef(regfit.bwd,7)
 (Intercept)        AtBat         Hits        Walks        CRuns 
 105.6487488   -1.9762838    6.7574914    6.0558691    1.1293095 
      CWalks    DivisionW      PutOuts 
  -0.7163346 -116.1692169    0.3028847 
```

The limitations to forward and backward stepwise selection are, they do
not collectively exhaust all possibilities

  - On the other hand, forward and backward stepwise selection are
    significantly less computationally complex  

</div>

<div style="margin-bottom:100px;">

### Choosing Among Models

Now that we’ve gone through indirectly calculating test errors, we begin
Cross-Validate to get a direct test error

  - Directly calculating a test error requires the data being split into
    a test and train set

<!-- end list -->

``` r
set.seed(1)
train = sample(c(TRUE,FALSE), nrow(Hitters),rep=TRUE)
test=(!train)
```

``` r
regfit.best = regsubsets(Salary~.,data=Hitters[train,],nvmax=19)
test.mat = model.matrix(Salary~.,data=Hitters[test,])
val.errors=rep(NA,19)
for(i in 1:19){
  coefi=coef(regfit.best,id=i)
  pred=test.mat[,names(coefi)]%*%coefi
  val.errors[i]=mean((Hitters$Salary[test]-pred)^2)
}
```

After saving our validate errors in the variable val.errors, we plot the
RMSE of the models associated with each number of
variables

``` r
plot(sqrt(val.errors), ylab="Root MSE", type="b", xlab="Number of Features")
```

![](Subset_Selection_Markdown3_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

``` r
which.min(val.errors)
[1] 7
```

The coef() function displays all the model coefficients associated with
the model passed as an argument

  - This is useful because there is no method in R to form a prediction
    for regsubsets()
  - We will then define a function to predict our outcomes for
    reproducibility

<!-- end list -->

``` r
coef(regfit.best,10)
 (Intercept)        AtBat         Hits        HmRun        Walks 
  71.8074075   -1.5038124    5.9130470  -11.5241809    8.4349759 
      CAtBat        CRuns         CRBI       CWalks    DivisionW 
  -0.1654850    1.7064330    0.7903694   -0.9107515 -109.5616997 
     PutOuts 
   0.2426078 

predict.regsubsets=function(object,newdata,id,...){
  form=as.formula(object$call[[2]])
  mat=model.matrix(form,newdata)
  coefi=coef(object,id=id)
  xvars=names(coefi)
  mat[,xvars]%*%coefi
}
```

``` r
regfit.best=regsubsets(Salary~.,data=Hitters,nvmax=19)
coef(regfit.best,10)
 (Intercept)        AtBat         Hits        Walks       CAtBat 
 162.5354420   -2.1686501    6.9180175    5.7732246   -0.1300798 
       CRuns         CRBI       CWalks    DivisionW      PutOuts 
   1.4082490    0.7743122   -0.8308264 -112.3800575    0.2973726 
     Assists 
   0.2831680 
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
       1        2        3        4        5        6        7        8 
149821.1 130922.0 139127.0 131028.8 131050.2 119538.6 124286.1 113580.0 
       9       10       11       12       13       14       15       16 
115556.5 112216.7 113251.2 115755.9 117820.8 119481.2 120121.6 120074.3 
      17       18       19 
120084.8 120085.8 120403.5 
par(mfrow=c(1,1))
plot(mean.cv.errors,type='b')
```

![](Subset_Selection_Markdown3_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

``` r
reg.best=regsubsets(Salary~.,data=Hitters, nvmax=19)
coef(reg.best,11)
 (Intercept)        AtBat         Hits        Walks       CAtBat 
 135.7512195   -2.1277482    6.9236994    5.6202755   -0.1389914 
       CRuns         CRBI       CWalks      LeagueN    DivisionW 
   1.4553310    0.7852528   -0.8228559   43.1116152 -111.1460252 
     PutOuts      Assists 
   0.2894087    0.2688277 
```

</div>

# Applied Exercise: Exercise 6.8

In this exercise, we will generate simulated data, and will then use
this data to perform best subset selection. a) Use the rnorm() function
to generate a predictor X of length n = 100, as well as a noise vector
\(\epsilon\) of length n = 100.

``` r
set.seed(1)
X = rnorm(100)
eps = rnorm(100)
```

2)  Generate a response vector Y of length n = 100 according to the
    model Y = β0 + β1X + β2X2 + β3X3 + \(\epsilon\), where β0, β1, β2,
    andβ3 are constants of your choice.

<!-- end list -->

  - We are selecting β0=3, β1=2, β2=−3 and β3=0.3.

<!-- end list -->

``` r
beta0 = 3
beta1 = 2
beta2 = -3
beta3 = 0.3
Y = beta0 + beta1 * X + beta2 * X^2 + beta3 * X^3 + eps
```

3)    - Use regsubsets to select best model having polynomial of X of
        degree 10
      - What is the best model obtained according to Cp, BIC, and
        adjustedR2?

<!-- end list -->

``` r
data.full = data.frame(y = Y, x = X)
mod.full = regsubsets(y ~ poly(x, 10, raw = T), data = data.full, nvmax = 10)
mod.summary = summary(mod.full)

which.min(mod.summary$cp)
[1] 3
which.min(mod.summary$bic)
[1] 3
which.max(mod.summary$adjr2)
[1] 3
```

  - Show some plots to provide evidence for your answer, and report the
    coeﬃcients of the best model
obtained.

<!-- end list -->

``` r
plot(mod.summary$cp, xlab = "Subset Size", ylab = "Cp", pch = 20, type = "l")
points(3, mod.summary$cp[3], pch = 4, col = "red", lwd = 7)
```

![](Subset_Selection_Markdown3_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

``` r

plot(mod.summary$bic, xlab = "Subset Size", ylab = "BIC", pch = 20, type = "l")
points(3, mod.summary$bic[3], pch = 4, col = "red", lwd = 7)
```

![](Subset_Selection_Markdown3_files/figure-gfm/unnamed-chunk-17-2.png)<!-- -->

``` r

plot(mod.summary$adjr2, xlab = "Subset Size", ylab = "Adjusted R2", pch = 20, 
    type = "l")
points(3, mod.summary$adjr2[3], pch = 4, col = "red", lwd = 7)
```

![](Subset_Selection_Markdown3_files/figure-gfm/unnamed-chunk-17-3.png)<!-- -->

``` r

coefficients(mod.full, id = 3)
          (Intercept) poly(x, 10, raw = T)1 poly(x, 10, raw = T)2 
           3.07627412            2.35623596           -3.16514887 
poly(x, 10, raw = T)7 
           0.01046843 
```

We find that with Cp, BIC and Adjusted R2 criteria, 3, 3, and 3 variable
models are respectively picked. All statistics pick X7 over X3. The
remaining coefficients are quite close to β s.

4)  Repeat (c), using forward stepwise selection and also using
    backwards stepwise
selection.

<!-- end list -->

``` r
mod.fwd = regsubsets(y ~ poly(x, 10, raw = T), data = data.full, nvmax = 10, 
    method = "forward")
mod.bwd = regsubsets(y ~ poly(x, 10, raw = T), data = data.full, nvmax = 10, 
    method = "backward")
fwd.summary = summary(mod.fwd)
bwd.summary = summary(mod.bwd)
which.min(fwd.summary$cp)
[1] 3

which.min(bwd.summary$cp)
[1] 3

which.min(fwd.summary$bic)
[1] 3

which.min(bwd.summary$bic)
[1] 3

which.max(fwd.summary$adjr2)
[1] 3

which.max(bwd.summary$adjr2)
[1] 3
```

Plot the statistics

``` r
par(mfrow = c(3, 2),cex.axis=1.8, cex.lab=1.8)
plot(fwd.summary$cp, xlab = "Subset Size", ylab = "Forward Cp", pch = 20, type = "l")
points(3, fwd.summary$cp[3], pch = 4, col = "red", lwd = 7)
plot(bwd.summary$cp, xlab = "Subset Size", ylab = "Backward Cp", pch = 20, type = "l")
points(3, bwd.summary$cp[3], pch = 4, col = "red", lwd = 7)
plot(fwd.summary$bic, xlab = "Subset Size", ylab = "Forward BIC", pch = 20, 
    type = "l")
points(3, fwd.summary$bic[3], pch = 4, col = "red", lwd = 7)
plot(bwd.summary$bic, xlab = "Subset Size", ylab = "Backward BIC", pch = 20, 
    type = "l")
points(3, bwd.summary$bic[3], pch = 4, col = "red", lwd = 7)
plot(fwd.summary$adjr2, xlab = "Subset Size", ylab = "Forward Adjusted R2", 
    pch = 20, type = "l")
points(3, fwd.summary$adjr2[3], pch = 4, col = "red", lwd = 7)
plot(bwd.summary$adjr2, xlab = "Subset Size", ylab = "Backward Adjusted R2", 
    pch = 20, type = "l")
points(4, bwd.summary$adjr2[4], pch = 4, col = "red", lwd = 7)
```

![](Subset_Selection_Markdown3_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->
We see that all statistics pick 3 variable models except backward
stepwise with adjusted R2. Here are the coefficients

``` r
coefficients(mod.fwd, id = 3)
          (Intercept) poly(x, 10, raw = T)1 poly(x, 10, raw = T)2 
           3.07627412            2.35623596           -3.16514887 
poly(x, 10, raw = T)7 
           0.01046843 

coefficients(mod.bwd, id = 3)
          (Intercept) poly(x, 10, raw = T)1 poly(x, 10, raw = T)2 
          3.078881355           2.419817953          -3.177235617 
poly(x, 10, raw = T)9 
          0.001870457 

coefficients(mod.fwd, id = 4)
          (Intercept) poly(x, 10, raw = T)1 poly(x, 10, raw = T)2 
          3.112358625           2.369858879          -3.275726574 
poly(x, 10, raw = T)4 poly(x, 10, raw = T)7 
          0.027673638           0.009997134 
```

Here forward stepwise picks X7 over X3. Backward stepwise with 3
variables picks X9 while backward stepwise with 4 variables picks X4 and
X7. All other coefficients are close to β s.
