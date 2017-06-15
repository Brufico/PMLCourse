#' ---
#' title: 'The caret package lecture 2.1 -  Overview'
#' author: Bruno Fischer Colonimos
#' date: "`r format(Sys.Date(), '%d %B %Y')`"
#' output:
#'         html_document:
#'             theme: readable
#'
#'         pdf_document:
#'             number_sections: yes
#'             urlcolor: blue
#'             fontsize: 10pt
#'             geometry: top=.9in, left=1in, right=1in, bottom = 1in, footskip = 0.3in
#' ---






#' Lecture 2.1 : The caret package
#'================================
#'
#'
#' Splitting data
#' --------------

library(caret); library(kernlab); data(spam)
View(spam)
colnames(spam)
summary(spam$type)/ sum(summary(spam$type)) # proportion of spam/ nonspam in the data

inTrain <- createDataPartition(y=spam$type,
                               p=0.75, list=FALSE)
training <- spam[inTrain,]
testing <- spam[-inTrain,]
dim(training)

summary(training$type)/ sum(summary(training$type)) # proportion of spam/ nonspam in the training set
summary(testing$type)/ sum(summary(testing$type)) # proportion of spam/ nonspam in the testing set




#' SPAM Example: Fit a model
#' ------------------------

set.seed(32343)
modelFit <- train(type ~.,data=training, method="glm")
modelFit


#'
#' SPAM Example: Final model
#' --------------------------
#'
modelFit$finalModel


#' SPAM Example: Prediction
#' ------------------------
#'
predictions <- predict(modelFit,newdata=testing)
predictions



#' SPAM Example: Confusion Matrix
#' -------------------------------

confusionMatrix(predictions,testing$type)

