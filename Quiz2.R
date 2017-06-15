#' ---
#' title: 'Practical Machine Learning Quiz2 The caret package
#' author: Bruno Fischer Colonimos
#' date: "`r format(Sys.Date(), '%d %B %Y')`"
#' output:
#'         html_document:
#'             theme: readable
#'---


library(caret); library(kernlab)




#' Q1
#' ===

if (!require(AppliedPredictiveModeling)) {
        install.packages("AppliedPredictiveModeling")
        library(AppliedPredictiveModeling)
}
data("AlzheimerDisease")
head(diagnosis)
str(predictors)
head(predictors)

#' code 1
adData <- data.frame(diagnosis,predictors)
trainIndex = createDataPartition(diagnosis, p = 0.50)
training = adData[trainIndex,]
testing = adData[-trainIndex,]

#' code 2
adData = data.frame(predictors)
trainIndex = createDataPartition(diagnosis,p=0.5,list=FALSE)
training = adData[trainIndex,]
testing = adData[-trainIndex,]

#' code 4 <++++++++++
adData = data.frame(diagnosis,predictors)
trainIndex = createDataPartition(diagnosis, p = 0.50,list=FALSE)
training = adData[trainIndex,]
testing = adData[-trainIndex,]



#' Verification
table(adData$diagnosis)/ sum(table(adData$diagnosis))
table(training$diagnosis)/ sum(table(training$diagnosis))
table(testing$diagnosis)/ sum(table(testing$diagnosis))



#' Q2
#' ===

library(AppliedPredictiveModeling)
data(concrete)

library(caret)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength,
                              p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]

dim(training)

colnames(training)
training$index <- 1:dim(training)[1]

g0 <- ggplot(training, aes(index,CompressiveStrength)) +
        geom_point()

idxp <- function(varname) {
        ggplot(training, aes(index,CompressiveStrength)) +
                geom_point(mapping = aes_(color= as.name(varname)))
}

idxp("Cement")

gr <- lapply(FUN=idxp, X=colnames(training))

length(gr)

gr[[1]]
gr[[2]]
gr[[3]]
gr[[4]]
gr[[5]]
gr[[6]]
gr[[7]]
gr[[8]]
gr[[9]]
gr[[10]]

#' Ans: ??


#' Q3
#' ===
#'

ggplot(data = training, aes(Superplasticizer)) +
        geom_histogram()



#' Q4
#' ===
#'

library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

colnames(training)

grep("^IL", colnames(training), value=TRUE)

trainIL <- training[ , grep("^IL", colnames(training), value=TRUE)]

preProc <- preProcess(trainIL,
                      method="pca", thresh = 0.8)

preProc$numComp

#' Ans = 7



#' Q5
#' ===
#'

#' Create a training data set consisting of only the
#' predictors with variable names beginning with IL and the
#' diagnosis
trainsmall <- data.frame(diagnosis = training[["diagnosis"]],
                         trainIL)


#'
#' ### model 2
#'

#' Build two predictive models, one using the predictors as
#' they are

modelFit1 <- train(x = trainIL, y = trainsmall$diagnosis,
                  method = "glm")

testIL <- testing[ , grep("^IL", colnames(training), value=TRUE)]
testsmall <- data.frame(diagnosis = testing[["diagnosis"]],
                        testIL)


res1 <- confusionMatrix(testing$diagnosis,predict(modelFit1,testIL))
res1$table
res$positive
rov1 <- res1$overall
str(rov)
rov1["Accuracy"]




#'
#' ### model 2
#'

#' Build a model using PCA with principal components explaining 80% of
#' the variance in the predictors. Use method="glm" in the
#' train function
preProc <- preProcess(trainIL,
                      method="pca", thresh = 0.8)

trainPC <- predict(preProc,trainIL)

# Does notwork anymore
# modelFit <- train(form=training$type ~ .,
#                   method="glm",data=trainPC)
# alternative
modelFit <- train(x = trainPC, y = trainsmall$diagnosis,
                  method = "glm")


testIL <- testing[ , grep("^IL", colnames(training), value=TRUE)]
testsmall <- data.frame(diagnosis = testing[["diagnosis"]],
                        testIL)

testPC <- predict(preProc,testIL)

res <- confusionMatrix(testing$diagnosis,predict(modelFit,testPC))
res$table
res$positive
rov <- res$overall
str(rov)
rov["Accuracy"]
