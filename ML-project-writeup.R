#Data exploration
library(stringr)
library(caret)
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
#create cross validation set
train1=createDataPartition(y=outcome_train,p=.8)

model=randomForest(pred_train[train1$Resample1,],outcome_train[train1$Resample1])
model1=randomForest(pred_train,outcome_train)
cv_pred=pred_train[-train1$Resample1,]
cv_outcome=outcome_train[-train1$Resample1]

validation=predict(model,cv_pred)
OutOfSampleError=1-sum(validation==cv_outcome)/length(cv_outcome)