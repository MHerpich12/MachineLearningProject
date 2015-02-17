library(caret)
library(randomForest)

##Input training data
training <- read.csv('pml-training.csv', na.strings=c("","NA"))

##Prune training data
NAvec <- vector(mode="integer",length=length(names(training)))
for (i in 1:length(names(training))) {
  NAvec[i] <- sum(is.na(training[,i]))/length(training[,i])
}
colhead <- vector(mode="integer")
for (i in 1:length(NAvec)){
  if(NAvec[i]<0.5) {
    colhead <- c(colhead,i)
  }
}
training <- training[,colhead]
training <- training[,-c(1:7)]

##Implement PCA
set.seed(1000)
preProc <- preProcess(training[,-length(names(training))],method="pca")
trainProc <- predict(preProc,training[,-length(names(training))])
finaltrain <- cbind(trainProc,training$classe)
names(finaltrain)[length(finaltrain)] <- "classe"

##Break training data into training and validation
set.seed(1000)
inTrain <- createDataPartition(y=finaltrain$classe,p=0.7,list=FALSE)
ftrain1 <- finaltrain[inTrain,]
fvalid1 <- finaltrain[-inTrain,]

##Train random forest model on data and validate
set.seed(1000)
##Data <- sample(1:length(ftrain1$classe),7500)
##ftrain1sub <- ftrain1[Data,]
fitControl <- trainControl(method="repeatedcv",number=3,repeats=3)
Model <- train(classe~.,data=ftrain1,method="rf",prox=TRUE,trControl=fitControl)
print(confusionMatrix(predict(Model,fvalid1[,-length(names(fvalid1))],fvalid1$classe)))

##Input testing data
testing <- read.csv('pml-testing.csv', na.strings=c("","NA"))

##Prune testing data
set.seed(1000)
testing <- testing[,colhead]
testing <- testing[,-c(1:7)]
testing <- testing[,-c(53)]
testProc <- predict(preProc,testing)

##Apply algorithm to testing data
Outcome <- as.vector(predict(Model,testProc))




