#' ---
#' title: Quiz4
#' author: BFC
#' date: 21/07/2017
#' output:
#'   html_document:
#'      number_sections: yes
#'      theme: readable
#'      toc: yes
#' ---
#'
#' -------------
#'
#' General libraries
#' ==============================================
#'
#'
#'
#' Question 1 : Vowel Recognition (Deterding data)
#' ==============================================
#'

library(ggplot2)
library(caret)
library(gbm)# Generalized Boosted regression Models
library(plyr)
library(dplyr)


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



#' ***************************************************************************


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
#' Stacked Accuracy: 0.80 is better than
#' random forests and lda and the same as boosting.
