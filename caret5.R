#' ---
#' title: 'The caret package lecture 2.5 : Preprocessing'
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



#' Lecture 2.5 : Basic Preprocessing
#' =================================

#' Why preprocess?
#' ---------------

library(caret); library(kernlab); data(spam)
set.seed(1234)
inTrain <- createDataPartition(y=spam$type,
                               p=0.75, list=FALSE)
training <- spam[inTrain,]
testing <- spam[-inTrain,]
hist(training$capitalAve,main="",xlab="ave. capital run length")


mean(training$capitalAve)
sd(training$capitalAve)


#' Standardizing
#' ----------------

trainCapAve <- training$capitalAve
trainCapAveS <- (trainCapAve  - mean(trainCapAve))/sd(trainCapAve)
mean(trainCapAveS)
sd(trainCapAveS)


#' Standardizing - test set (with the parametersestimated from the training set)

testCapAve <- testing$capitalAve
testCapAveS <- (testCapAve  - mean(trainCapAve))/sd(trainCapAve)
# (Mean and std.dev come from the training set)
mean(testCapAveS)
sd(testCapAveS)



#' Standardizing - preProcess function
#' ------------------------------------
set.seed(1234) # inutile?
preObj <- preProcess(training[,-58],method=c("center","scale"))
trainCapAveS <- predict(preObj,training[,-58])$capitalAve
mean(trainCapAveS)
sd(trainCapAveS)



#' Standardizing - preProcess function
#' ------------------------------------

testCapAveS <- predict(preObj,testing[,-58])$capitalAve
mean(testCapAveS)
sd(testCapAveS)




#' Standardizing - preProcess argument of the train() function
#' -----------------------------------------------------------

set.seed(32343)
modelFit <- train(type ~.,data=training,
                  preProcess=c("center","scale"),method="glm")
modelFit



#' Standardizing - Box-Cox transforms
#' ----------------------------------

preObj <- preProcess(training[,-58],method=c("BoxCox"))
trainCapAveS <- predict(preObj,training[,-58])$capitalAve
par(mfrow=c(1,2)); hist(trainCapAveS); qqnorm(trainCapAveS)



#' Standardizing - Imputing data
#' -----------------------------

set.seed(13343)

# Make some values NA
training$capAve <- training$capitalAve
selectNA <- rbinom(dim(training)[1],size=1,prob=0.05)==1
training$capAve[selectNA] <- NA

# Impute and standardize
preObj <- preProcess(training[,-58],method="knnImpute")
capAve <- predict(preObj,training[,-58])$capAve

# Standardize true values
capAveTruth <- training$capitalAve
capAveTruth <- (capAveTruth-mean(capAveTruth))/sd(capAveTruth)


#' Standardizing - Imputing data
#' -----------------------------

quantile(capAve - capAveTruth)
quantile((capAve - capAveTruth)[selectNA])
quantile((capAve - capAveTruth)[!selectNA])


#' Notes and further reading
#' -------------------------

#' Training and test must be processed in the same way
#' Test transformations will likely be imperfect
#' Especially if the test/training sets collected at different times
#' Careful when transforming factor variables!
#'         preprocessing with caret : http://caret.r-forge.r-project.org/preprocess.html






