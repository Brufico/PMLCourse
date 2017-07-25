#' ---
#' title: Quiz4
#' author: BFC
#' date: 21/07/2017
#' fontsize: 12pt
#' urlcolor: blue
#' linkcolor: red
#' documentclass: article
#' classoption: a4paper
#' geometry: "left=1.5cm,right=1.5cm,top=1.5cm,bottom=2cm,footskip=1cm"
#' output:
#'   pdf_document:
#'     highlight: monochrome
#'     number_sections: yes
#'     toc: yes
#'     toc_depth: 3
#' # html_document:
#' #    number_sections: yes
#' #    theme: readable
#' #    toc: yes
#' ---
#'
# + setup
knitr::opts_chunk$set(echo = TRUE, fig.show = "hold", fig.width = 3.5, fig.asp = 1, fig.align = "center")



#'*************************************************
#'
#' General libraries
#' ==============================================
#'


library(ggplot2)
library(caret)
library(gbm)# Generalized Boosted regression Models
library(plyr)
library(dplyr)



#'
#'
#' Question 1 : Vowel Recognition (Deterding data)
#' ==============================================
#'


#' specific libraries + Data
#' -----------------
library(ElemStatLearn)
data(vowel.train)
data(vowel.test)

vowel.test$y <-  factor(vowel.test$y)
vowel.train$y <-  factor(vowel.train$y)

# str(vowel.test)
# str(vowel.train)


#'
#' Fitting 1) random forest predictor, 2) boosted predictor using gbm
#' ------------------------------------------------------------------
#'

#+ modelfit1q1, cache = TRUE

# Crossvalidation params (k-folds)
number <-  6 # number of folds
repeats <-  3 # repeats


set.seed(33833)

modrf <- train(y ~ ., data=vowel.train,
               method="rf",
               trControl = trainControl(method = "repeatedcv",
                                        number = number, repeats = repeats),
               prox=TRUE) # prox?


#+ modelfit2q1, cache = TRUE, results = "hide"

modgbm <- train(y ~ . , data = vowel.train,
                method = "gbm",
                trControl = trainControl(method = "repeatedcv",
                                         number = number, repeats = repeats)
)

#'
#' Compute and compare model accuracies
#' ------------------------------------
#'
#+ modelaccq1, cache = TRUE, dependson = "modelfit1q1"
# test set accuracy

# compute each accuracy
tset <- vowel.test

tset$predrf <- predict(modrf, newdata = vowel.test)
tset$predgbm <- predict(modgbm, newdata = vowel.test)

# confusion matrices + accuracy
cfm_rf <- confusionMatrix(tset$predrf,tset$y)
cfm_gbm <- confusionMatrix(tset$predgbm,tset$y)

# restrict to agreement cases
tsetagree <- tset[tset$predgbm == tset$predrf, ]
cfm_agree <- confusionMatrix(tsetagree$predrf, tsetagree$y)

# display only accuracies together
accget <- function(cfm)
        c(cfm$overall["Accuracy"],
          cfm$overall["AccuracyLower"],
          cfm$overall["AccuracyUpper"])

knitr::kable(t(data.frame(RF = accget(cfm_rf),
                   GBM = accget(cfm_gbm),
                   AGREE = accget(cfm_agree))))



#'
#' Conclusion
#' -----------
#'
#' Answer c: \
#' RF Accuracy = 0.6082 , GBM Accuracy = 0.5152 , Agreement Accuracy = 0.6361



#'
#' ***************************************************************************
#'


#'
#' Q2: Alzheimer
#' =================================
#'



set.seed(3433)
library(AppliedPredictiveModeling)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

#'
#' Fit diagnosis using a random forest "rf", "gbm" and "lda"
#' -------------------------------------------------------------
#'
#' Set the seed to 62433 and predict diagnosis with all the
#' other variables using a random forest ("rf"), boosted
#' trees ("gbm") and linear discriminant analysis ("lda")
#' model.


#+ modelfitparamsq2, cache = FALSE

# Crossvalidation params (k-folds)
number <-  5 # number of folds
repeats <-  3 # repeats


#+ modelfit1q2, cache = TRUE, dependson = "modelfitparamsq2"
set.seed(62433 )

modrf2 <- train(diagnosis ~ ., data = training,
               method = "rf",
               trControl = trainControl(method = "cv",
                                        number = number))


#+ modelfit2q2, cache = TRUE, results = "hide", dependson = "modelfitparamsq2"
modgbm2 <- train(diagnosis ~ . , data = training,
                method = "gbm",
                trControl = trainControl(method = "cv",
                                         number = number)
)

#+ modelfit3q2, cache = TRUE, results = "hide", dependson = "modelfitparamsq2"
modlda2 <- train(diagnosis ~ . , data = training,
                method = "lda",
                trControl = trainControl(method = "cv",
                                         number = number)
)


#'
#' Predict diagnosis using each model
#' -----------------------------------
#'

#+ predictQ2, cache = FALSE
trset <- training

trset$predrf <- predict(modrf2, newdata = training)
trset$predgbm <- predict(modgbm2, newdata = training)
trset$predlda <- predict(modlda2, newdata = training)

# Verify now these predictions
#
# pairs(~ predrf + predgbm + predlda, data = trset)

confusionMatrix(trset$predrf,trset$predgbm)$table
confusionMatrix(trset$predrf,trset$predlda)$table

confusionMatrix(trset$predrf,trset$diagnosis)$table
confusionMatrix(trset$predlda,trset$diagnosis)$table

confusionMatrix(trset$predgbm,trset$diagnosis)$table

numdif <- function(a,b){
        sum(a != b)
}

with(trset,
     c(numdif(predrf,predgbm),
     numdif(predrf,predlda),
     numdif(predgbm,predlda),
     numdif(predgbm,diagnosis)
     )
)

#'
#' Stack model together using random forest "rf"
#' -------------------------------------------------------------
#'

#+ stackfitq2, cache = TRUE, dependson = "modelfitparamsq2"
modstack2 <- train(diagnosis ~ predrf + predgbm + predlda,
                   data = trset,
                  method = "rf",
                  trControl = trainControl(method = "repeatedcv",
                                           number = number, repeats = repeats)
)

#+ stackpredq2
trset$predstack <- predict(modstack2, newdata = trset)

#'
#' Compute and compare model accuracies
#' ------------------------------------
#'
#+ modelaccq2, cache = FALSE
# test set accuracy

# compute each accuracy
tset <- testing #copy

tset$predrf <- predict(modrf2, newdata = tset)
tset$predgbm <- predict(modgbm2, newdata = tset)
tset$predlda <- predict(modlda2, newdata = tset)
tset$predstack <- predict(modstack2, newdata = tset)

# confusion matrices + accuracy
cfm_rf <- confusionMatrix(tset$predrf,tset$diagnosis)
cfm_gbm <- confusionMatrix(tset$predgbm,tset$diagnosis)
cfm_lda <- confusionMatrix(tset$predlda,tset$diagnosis)
cfm_stack <- confusionMatrix(tset$predstack,tset$diagnosis)

# display only accuracies together
accget <- function(cfm)
        c(cfm$overall["Accuracy"],
          cfm$overall["AccuracyLower"],
          cfm$overall["AccuracyUpper"])

#' Comparison of accuracies:

#+ displayacc
knitr::kable(t(data.frame(RF = accget(cfm_rf),
                          GBM = accget(cfm_gbm),
                          LDA = accget(cfm_lda),
                          STACK = accget(cfm_stack) )))


#'
#' Conclusion
#' -----------
#'
#' Answer c: \
#' Stacked Accuracy: 0.80 is better than random forests and lda and the same as boosting.
#'
#' (BUT there seems to be a MUCH bigger problem)
#'


#' ***************************************************************************


#'
#' Q3: Concrete
#' =================================
#'
#'
#' Data
#' -----
#

set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]

# modify order of cols
# head(concrete)
#concrete <- cbind(concrete[9], concrete[-9])
# tail(concrete)

#'
#' Lasso
#' -----
#'

set.seed(233)

lmodel <- train(x=training[-9], y=training[[9]],
                method = "lasso",
                trControl = trainControl(method = "cv", number = 5))

#
# lmodel <- train(CompressiveStrength ~ . , data=training,
#                 method = "lasso",
#                 trControl = trainControl(method = "cv", number = 5))

#'
#' ### see caret model
print(lmodel)

#'
#' ### see finalmodel

print(lmodel$finalModel)

#' Plot coeff paths
#'
#+ plotpaths, fig.width = 7, fig.asp =0.75
plot.enet(lmodel$finalModel)



#'
#' Conclusion
#' ----------
#'
#' Answer; Cement
#'


#' ***************************************************************************


#'
#' Q4: number of visitors to the instructors blog
#' =================================
#'
#'
#' Data
#' -----

#'
#' ### Download
#'
datedwn <- date()
datadir <- "data"
fname <- "gaData.csv"
fpath <- file.path(datadir, fname)
url <- "https://d396qusza40orc.cloudfront.net/predmachlearn/gaData.csv"

retrievefile = FALSE  # (downloaded already)

if (retrievefile) {
        download.file(url = url, destfile = fpath)
}

#' ### Read in & split data

library(lubridate) # For year() function below
dat = read.csv(fpath)
training = dat[year(dat$date) < 2012,]
testing = dat[(year(dat$date)) > 2011,]
tstrain = ts(training$visitsTumblr)

# added
tstest <- ts(testing$visitsTumblr)

#' better way to split ? Yes, Later
#'

#'
#' Analysis
#' --------
#'

#+ getfcastdata
library(forecast)

# model fitting
batmod <- bats(tstrain)
# forecasting
fcast <- forecast(batmod, levels=c(0.95) , h = 235)

#+ fcastplot, fig.width = 7, fig.asp = 0.5
plot(fcast)
lines(tstest, col="red") # problem

#+ fcastsummary, results = "hide"
sfcast <- summary(fcast)

#+ fcastelements,

# fcast model
# names(fcast)
# [1] "model"     "mean"      "level"     "x"         "series"    "upper"     "lower"     "fitted"
# [9] "method"    "residuals"

fcast$method
fcast$model
fcast$level

head(fcast$mean)
fcast$series
head(fcast$x)
head(fcast$upper)
head(fcast$lower)
head(fcast$fitted)


# objct summary(forecast)
names(sfcast)
# "Point Forecast" "Lo 80"          "Hi 80"          "Lo 95"          "Hi 95"
head(sfcast)


# using sfcast to answer th question
fmin <- sfcast$`Lo 95`
fmax <- sfcast$`Hi 95`

# % out of confidence limits
sum(tstest > fmax | tstest < fmin)/length(tstest) # 0.038

#'
#' Conclusion
#' ----------

#' . For how many of the testing points is the true value
#' within the 95% prediction interval bounds? ==> 96%

#'
#' ****************************************************************
#'
#' Redoing Q4 again (with better selection method for the training and testing sets)
#' ==============================================================
#'

dat = read.csv(fpath)
# training = dat[year(dat$date) < 2012,]
# testing = dat[(year(dat$date)) > 2011,]
tsdat = ts(dat$visitsTumblr)

tstrain <- window(tsdat,start=1,end=365)
tstest <- window(tsdat,start=366,end=(600))



# model fitting
batmod <- bats(tstrain)
# forecasting
fcast <- forecast(batmod, levels=c(0.95) , h = 235)

#+ fcastplot2, fig.width = 7, fig.asp = 0.5
plot(fcast, ylim = c(-200, 1500))
lines(tstest, col="red") # problem


#+ fcastsummary2, results = "hide"
sfcast <- summary(fcast)

#+ fcastelements2,

# fcast model
# names(fcast)
# [1] "model"     "mean"      "level"     "x"         "series"    "upper"     "lower"     "fitted"
# [9] "method"    "residuals"

fcast$method
fcast$model
fcast$level

head(fcast$mean)
fcast$series
head(fcast$x)
head(fcast$upper)
head(fcast$lower)
head(fcast$fitted)


# object summary(forecast)
names(sfcast)
# "Point Forecast" "Lo 80"          "Hi 80"          "Lo 95"          "Hi 95"
head(sfcast)


# using sfcast to answer th question
fmin <- sfcast$`Lo 95`
fmax <- sfcast$`Hi 95`

# % out of confidence limits
sum(tstest > fmax | tstest < fmin)/length(tstest) # 0.038




#'
#' Q5 Concrete again
#' ================
#'
#' Use Model : svmLinear2 (package e1071)
#'

#' Data
#' _______

set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]


head(training)


set.seed(325)
# try1
# mod <- train(CompressiveStrength ~ .,
#              data = training,
#              method="svmLinear2" )

# try2 (both work)
mod <- train(x = training[-9], y = training[[9]],
             method="svmLinear2" )


#prediction
predsvm <- predict(mod, testing)

# RMSE
postResample(pred=predsvm, obs = testing$CompressiveStrength)


# ANs = 10.62 ==> 6.93 est *FAUX*
#            ==> 6.72 OK (ne pas chercher à comprendre)


#' ********************************
#'
#' Answers Summary
#' ==========


# Q1: **************
# #' Answer c: \
#' RF Accuracy = 0.6082 , GBM Accuracy = 0.5152 , Agreement Accuracy = 0.6361
#'
#' Q2 **************
#' #' Answer c: \
#' Stacked Accuracy: 0.80 is better than random forests and lda and the same as boosting.
#'
#' Q3 ***********************
#' Cement
#'
#' Q4 *********************
#' ' #' . For how many of the testing points is the true value
#' within the 95% prediction interval bounds? ==> 96%
#'
#' Q5
#' # ANs ( calcul) = 10.62
#'
#' ==> 6.93 est *FAUX*
#' ==> 6.72 OK
#' 107.44
#' 11543.39
#

