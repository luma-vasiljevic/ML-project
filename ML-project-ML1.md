Predicting exercise execution accuracy from activity monitors
========================================================

First we load the data and remove aggregate variables from the dataset since they are mostly NANs for both training and test data.

Next, we remove correlated columns to reduce the number of variables, and create a set of predictors comprising of 39 variables for both test & training set.


We create a cross-validation set from the training set:

We create two random forest models, one form the entire training set, and one from the training set excluding the cross-validation subset. Random forest algorithm in R calculates an out of bag error, which is equivalent to an out of sample error calculated from the cross-validation subset. But, to confirm we run the prediction on the cross-validation subset



```r
#Data exploration
library(stringr)
library(caret)
```

```
## Warning: package 'caret' was built under R version 3.1.1
```

```
## Loading required package: lattice
## Loading required package: ggplot2
```

```r
set.seed(32323)
data=read.csv('pml-training.csv')

clean <- function(daata)
{
cols=colnames(data)
events=unique(data[,c(2,5)])
#remove stat variables
keep=!str_detect(cols,"kurtosis")
data=data[,keep]
cols=colnames(data)
keep=!str_detect(cols,"skewness")
data=data[,keep]
cols=colnames(data)
keep=!str_detect(cols,"max")
data=data[,keep]
cols=colnames(data)
keep=!str_detect(cols,"min")
data=data[,keep]
cols=colnames(data)
keep=!str_detect(cols,"amplitude")
data=data[,keep]
cols=colnames(data)
keep=!str_detect(cols,"var")
data=data[,keep]
cols=colnames(data)
keep=!str_detect(cols,"avg")
data=data[,keep]
cols=colnames(data)
keep=!str_detect(cols,"stddev")
data=data[,keep]
cols=colnames(data)
data
}
#PCAs=prcomp(data[,8:59])
#processed=PCAs$x
pos_monitors_train=clean(data)[,8:59]
row_label_train=data[,c(2,5,6,7)]
outcome_train=data$classe
data=read.csv('pml-testing.csv')
pos_monitors_test=clean(data)[,8:59]
row_label_test=data[,c(2,5,6,7)]
M=abs(cor(pos_monitors_train))
diag(M)=0
M[upper.tri(M)]=0
cor=which(M>0.8,arr.ind=T)
idx2=union(cor[,1],cor[,2])
idx1=setdiff(c(1:52),idx2)
idx3=unique(cor[,2])
idx=union(unique(cor[,2]),idx1)
pred_train=pos_monitors_train[,setdiff(idx,c(4,8,9,33))]
pred_test=pos_monitors_test[,setdiff(idx,c(4,8,9,33))]

#PCAs=prcomp(pred)
#processed=PCAs$x
library(randomForest)
```

```
## Warning: package 'randomForest' was built under R version 3.1.1
```

```
## randomForest 4.6-10
## Type rfNews() to see new features/changes/bug fixes.
```

```r
#create cross validation set
train1=createDataPartition(y=outcome_train,p=.8)

model=randomForest(pred_train[train1$Resample1,],outcome_train[train1$Resample1])
model1=randomForest(pred_train,outcome_train)
cv_pred=pred_train[-train1$Resample1,]
cv_outcome=outcome_train[-train1$Resample1]

validation=predict(model,cv_pred)
OutOfSampleError=(1-sum(validation==cv_outcome)/length(cv_outcome))*100
```

Characteristics of the model on the full training set:

```r
model1
```

```
## 
## Call:
##  randomForest(x = pred_train, y = outcome_train) 
##                Type of random forest: classification
##                      Number of trees: 500
## No. of variables tried at each split: 6
## 
##         OOB estimate of  error rate: 0.43%
## Confusion matrix:
##      A    B    C    D    E class.error
## A 5577    2    0    0    1   0.0005376
## B   12 3781    4    0    0   0.0042139
## C    0   18 3400    4    0   0.0064290
## D    0    0   32 3182    2   0.0105721
## E    0    0    3    7 3597   0.0027724
```

Characteristics of the model on the full training set:

```r
model
```

```
## 
## Call:
##  randomForest(x = pred_train[train1$Resample1, ], y = outcome_train[train1$Resample1]) 
##                Type of random forest: classification
##                      Number of trees: 500
## No. of variables tried at each split: 6
## 
##         OOB estimate of  error rate: 0.58%
## Confusion matrix:
##      A    B    C    D    E class.error
## A 4460    3    0    0    1   0.0008961
## B   11 3018    9    0    0   0.0065833
## C    0   19 2717    2    0   0.0076698
## D    0    0   34 2536    3   0.0143801
## E    0    1    2    6 2877   0.0031185
```

The out of sample error from the quiz set (cross-validation subset) is: 0.4843%
Note that the out of bag error from model1 and out of sample error from the validation subset are very similar.

The model had 100% accuracy on the test set.
