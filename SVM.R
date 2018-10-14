library(caret)
library(kernlab)
library(ROCR)

data(segmentationData)
#segmentationData - data frame of cells
unique(segmentationData$Class)
#The outcome class is contained in a factor variable called Class with levels 
#"PS" for poorly segmented and "WS" for well segmented.
names(segmentationData)
head(segmentationData)


#Data Exploration

#Number of rows and columns
dim(segmentationData)

#Distribution of Target Variable
table(segmentationData$Class)

table(segmentationData$Class) / length(segmentationData$Class)

#Data Exploration
#Number of rows and columns
dim(segmentationData)


#Distribution of Target Variable
table(segmentationData$Class)

table(segmentationData$Class) / length(segmentationData$Class)

Index <- createDataPartition(segmentationData$Class,p=.7,list=FALSE)
?createDataPartition
#A series of test/training partitions are created using createDataPartition
#p 	the percentage of data that goes to training
#list -logical - should the results be in a list (TRUE) or a matrix 

svm.train <- segmentationData[Index,]
svm.validate  <- segmentationData[-Index,]
trainX <-svm.train[,4:61] 

#build SVM model in R

#Setup for cross validation
set.seed(123)
ctrl <- trainControl(method="cv",
                     number = 2,
                     summaryFunction=twoClassSummary,
                     classProbs=TRUE)

#classProbs-should class probabilities be computed for classification models 
#(along with predicted values) in each resample?

# Grid search to fine tune SVM
grid <- expand.grid(sigma = c(.01, .015, 0.2),
                    C = c(0.75, 0.9, 1, 1.1, 1.25)
)


#Train SVM
svm.tune <- train(x=trainX,
                  y= svm.train$Class,
                  method = "svmRadial",
                  metric="ROC",
                  tuneGrid = grid,
                  trControl=ctrl)



svm.tune

# Predict Target Label
valX <-svm.validate[,4:61]
pred <- predict(svm.tune, valX, type="prob")[2]

# Model Performance Statistics
pred_val <-prediction(pred[,1], svm.validate$Class)

# Calculating Area under Curve
perf_val <- performance(pred_val,"auc")
perf_val

# Calculating True Positive and False Positive Rate
perf_val <- performance(pred_val, "tpr", "fpr")

# Plot the ROC curve
plot(perf_val, col = "green", lwd = 1.5)
