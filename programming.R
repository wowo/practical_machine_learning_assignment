library(caret)
train <- read.csv(file = 'pml-training.csv');
finalTest <- read.csv(file = 'pml-testing.csv');

partition <- createDataPartition(train$classe, p=.75, list=F)
trainPC <- train[partition,]
testPC <- train[-partition,]

drops <- c('X', 'user_name', 'raw_timestamp_part_1', 'raw_timestamp_part_2', 'cvtd_timestamp', 'new_window', 'num_window')
trainPC <- trainPC[, !(names(trainPC) %in% drops)]
colsToDrop <- nearZeroVar(trainPC)
trainPC <- trainPC[, -colsToDrop]
trainPC$classe <- as.factor(trainPC$classe)
trainPC[is.na(trainPC)] <- 0

testPC <- testPC[, !(names(testPC) %in% drops)]
testPC <- testPC[, -colsToDrop]
testPC$classe <- as.factor(testPC$classe)
testPC[is.na(testPC)] <- 0

finalTest <- finalTest[, !(names(finalTest) %in% drops)]
finalTest <- finalTest[, -colsToDrop]
finalTest[is.na(finalTest)] <- 0

modFit <- randomForest(classe ~ ., data=trainPC)
variablesImportance <- varImp(modFit, scale=TRUE)
pred <- predict(modFit, trainPC)
table(pred, trainPC$classe)


pred <- predict(modFit, testPC)
table(pred, testPC$classe)

finalTest$prediction <- predict(modFit, finalTest)

pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

# pml_write_files(finalTest$prediction)
  