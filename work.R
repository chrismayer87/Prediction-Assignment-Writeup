
# after inspection, need code for some cleaning
training = read.csv("C:/Users/cam7de/Desktop/Coursera/03252017/pml-training.csv",na.strings=c("NA","#DIV/0!",""))
test = read.csv("C:/Users/cam7de/Desktop/Coursera/03252017/pml-testing.csv",na.strings=c("NA","#DIV/0!",""))

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

# evaluate outcome variable
sqldf('select classe, count(*) from training group by 1')

# it looks like first 7 variables arent useful
# visual inspection looks like 60 and after arent helpful

training_1 <- names(training[,colSums(is.na(training)) == 0])[8:59]
test_1 <- names(test[,colSums(is.na(test)) == 0])[8:59]

training <- training[,c(training_1,"classe")]
test <- test[,c(test_1,"problem_id")]

set.seed(13)
inTrain <- createDataPartition(training$classe, p=0.65, list=FALSE)
dt_training <- training[inTrain,]
dt_test <- training[-inTrain,]

dim(dt_training); 
dim(dt_test);

#random forest

#build model on the training
modfit1=randomForest(classe~., data=dt_training, method='class')
#checking prediction on the test
pred1 = predict(modfit1,dt_test,type='class') 
conf1=confusionMatrix(pred1,dt_test$classe)
conf1$overall[1]

#Tree-Based Models
#Recursive partitioning is a fundamental tool in data mining. It helps us explore the stucture of a set of data, while developing easy to visualize decision rules for predicting a categorical (classification tree) or continuous (regression tree) outcome. This section briefly describes CART modeling, conditional inference trees, and random forests.
modfit2 <- train(classe~., method = "rpart", data=dt_training)
#checking prediction on the test
pred2 = predict(modfit2,dt_test)
conf2=confusionMatrix(pred2,dt_test$classe)
conf2$overall[1]

#shows strength of the random forest approach
#using prediction on the final test data set
pred_final = predict(modfit1,test,type='class') 

#setwd("C:/Users/cam7de/Desktop/Coursera/03252017")
#pml_write_files = function(x){
#  n = length(x)
#  for(i in 1:n){
#    filename = paste0("problem_id_",i,".txt")
#    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
#  }
#}
#pml_write_files(pred1)
nofiles = length(pred_final)
for (i in 1:nofiles){
  filename =  paste0("problem_id",i,".txt")
  write.table(pred_final[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
}
pred_final

#due to out of sample error, expect much less


