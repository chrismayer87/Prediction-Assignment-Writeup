# Prediction Assignment Writeup
## Question:

Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, your goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways. More information is available from the website here: http://groupware.les.inf.puc-rio.br/har (see the section on the Weight Lifting Exercise Dataset).
Data

The training data for this project are available here:

https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv

The test data are available here:

https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv

## Submission Goal

The goal of your project is to predict the manner in which they did the exercise. This is the "classe" variable in the training set. You may use any of the other variables to predict with. You should create a report describing how you built your model, how you used cross validation, what you think the expected out of sample error is, and why you made the choices you did. You will also use your prediction model to predict 20 different test cases.

## Load libraries and packages needed


```r
library(AppliedPredictiveModeling)
library(RGtk2)
library(rattle)
library(rpart.plot)
library(pgmm)
library(ElemStatLearn)
library(randomForest)
library(gbm)
library(caret)
library(lubridate)
library(forecast)
library(sqldf)
```

## Pull study data and clean

After downloading to computer, the files were read into R Studio. Additionally, I reviewed the data and came up with some cleaning steps for missing data lines and later variables that were not pertinent.


```r
training = read.csv("C:/Users/cam7de/Desktop/Coursera/03252017/pml-training.csv",na.strings=c("NA","#DIV/0!",""))
test = read.csv("C:/Users/cam7de/Desktop/Coursera/03252017/pml-testing.csv",na.strings=c("NA","#DIV/0!",""))
```

I wanted to evaluate the outcome variable quickly to see what the distribution was like. We can see it fairly well distributed so that can help the goal of modeling on this dataset.


```r
sqldf('select classe, count(*) from training group by 1')
```

```
## Loading required package: tcltk
```

```
##   classe count(*)
## 1      A     5580
## 2      B     3797
## 3      C     3422
## 4      D     3216
## 5      E     3607
```

Visual inspection finds that the first 7 variables don't seem useful as well as the variables 60 and over.


```r
training_1 <- names(training[,colSums(is.na(training)) == 0])[8:59]
test_1 <- names(test[,colSums(is.na(test)) == 0])[8:59]

training <- training[,c(training_1,"classe")]
test <- test[,c(test_1,"problem_id")]
```

Splitting the data up for internal training and test data sets. Separate final test data will not be used until the end.


```r
set.seed(13)
inTrain <- createDataPartition(training$classe, p=0.65, list=FALSE)
dt_training <- training[inTrain,]
dt_test <- training[-inTrain,]

dim(dt_training); 
```

```
## [1] 12757    53
```

```r
dim(dt_test);
```

```
## [1] 6865   53
```

## Random forest approach


```r
#build model on the training
modfit1=randomForest(classe~., data=dt_training, method='class')
#checking prediction on the test
pred1 = predict(modfit1,dt_test,type='class') 
conf1=confusionMatrix(pred1,dt_test$classe)
conf1$overall[1]
```

```
## Accuracy 
## 0.994756
```

This is a very high finding and in-line with expectations of such. Significant computing power was necessary. This was tested against a recursive partioning approach and found that random forest was much more accurate.

## Cross-check


```r
modfit2 <- train(classe~., method = "rpart", data=dt_training)
#checking prediction on the test
pred2 = predict(modfit2,dt_test)
conf2=confusionMatrix(pred2,dt_test$classe)
conf2$overall[1]
```

```
##  Accuracy 
## 0.4908958
```

Random forest approach was much higher. Now, using the prediction on the final test set. WIth out of sample error, expect findings less than 99% in the testing.


```r
pred_final = predict(modfit1,test,type='class') 
nofiles = length(pred_final)
for (i in 1:nofiles){
  filename =  paste0("problem_id",i,".txt")
  write.table(pred_final[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
}
#pred_final
```


