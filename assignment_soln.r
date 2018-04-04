library(caret)
library(klaR)
data=read.csv("Customer_Churn_Data.csv")
data<-data[complete.cases(data),-c(1,2,3,5)]

#partitioning dataset
trainIndex <- createDataPartition(data$churn, p=0.7, list=FALSE)
train <- data[ trainIndex,]
test <- data[-trainIndex,]

set.seed(54321)


#naive bayes model
naivebayes <- NaiveBayes(churn~., data=train)
# train a decision tree model
ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
decisiontree <- train(churn~., data =train, method = "rpart",parms = list(split = "information"),trControl=ctrl,tuneLength = 10)
#svm model
svm <- train(churn ~., method = "svmRadial", data = train)
#random forest
abcd$total_day_minutes<-as.factor(abcd$total_day_minutes)
abcd$total_day_calls<-as.factor(abcd$total_day_calls)
abcd$total_day_charge<-as.factor(abcd$total_day_charge)
abcd$total_eve_minutes<-as.factor(abcd$total_eve_minutes)
abcd$total_eve_calls<-as.factor(abcd$total_eve_calls)
abcd$total_eve_charge<-as.factor(abcd$total_eve_charge)
abcd$churn<-as.factor(abcd$churn)
mod.rf <- train(abcd$churn ~ total_day_calls+total_day_charge, method = "rf", data = train,metric=metric, trControl=control,na.action = na.fail)


#training the model
xtest <-test[,1:17]
ytest <-test[,18]
predictionbayes <- predict(naivebayes,xtest)
predictiondt<-predict(decisiontree,xtest)
predictionsvm <- predict(svm, xtest)
predictionrf <- predict(mod.rf, xtest)

#confusion matrix
confusionMatrix(predictionbayes$class, ytest)
confusionMatrix(predictiondt, ytest)
confusionMatrix(predictionsvm, ytest)
confusionMatrix(predictionrf, ytest)


#predicting
datan=read.csv("datacopy.csv")
ntrainIndex <- createDataPartition(data$churn, p=0.7, list=FALSE)
ntrain <- datan[ ntrainIndex,]
ntest <- datan[-ntrainIndex,]
nnaivebayes <- NaiveBayes(churn~., data=ntrain)
nxtest <-test[,1:17]
nytest <-test[,18]
npredictionbayes <- predict(nnaivebayes,nxtest)
confusionMatrix(npredictionbayes$class, nytest)

