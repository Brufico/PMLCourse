#' ---
#' title: Quiz4 - Q2
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


library(ggplot2)
library(caret)
library(gbm)# Generalized Boosted regression Models
library(plyr)
library(dplyr)



#' ***************************************************************************


#'
#' Q2: Alzheimer
#' =================================
#'

#' Get Data
#' ----------

set.seed(3433)
library(AppliedPredictiveModeling)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)



#'
#' Some preliminary exploration
#' -----------------

# str(adData)

# spot the factors
colnames(adData)[which(sapply(adData, is.factor)==TRUE)]

# ==> diagnosis , genotype are factors

# representing diagnosis and Genotype as dummy variables

dummies <- dummyVars(~ diagnosis + Genotype, data=adData, fullRank = TRUE)
dumvars <- predict(dummies, newdata=adData)

dumvars <- as.data.frame(dumvars)

# head(dumvars)
#
# adData <- adData %>%
#         mutate(diagnosis = dumvars[1]) %>%
#         select(-Genotype)

adData <- cbind(adData,dumvars[-1])


#' Finish data preparation
#' -----------------------

inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

# personal:
ytrain <- training["diagnosis"]
xtrain <- training[-1]
ytest <- testing["diagnosis"]
xtest <- testing[-1]



#' some more eploration on the training set
#' ----------------------------------------
# windows()
# featurePlot(x = training[, 2:10],
#             y = training$diagnosis,
#             plot="pairs")
# dev.off()

# looking for hignly correlated covariates

correlmax <- function(j) {
        diagnum <- as.numeric(training[[j]])

        cory <- sapply(X = 21:40  ,  #2:ncol(training)
                       function(i) {
                               if (i == j) {
                                       0
                               } else {
                                       cor(x=diagnum,y=training[[i]] )
                                }

                       }
        )

        max(abs(cory))

}

maxc <- sapply(3:ncol(training), correlmax)

max(maxc)

maxc[which(maxc >= 0.8)]

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
# repeats <-  3 # repeats


set.seed(62433 )


#' set up parallel processing
library(parallel)
library(doParallel)
cluster <- makeCluster(detectCores() - 1) # convention to leave 1 core for OS
registerDoParallel(cluster)



#+ modelfit1q2, cache = TRUE, dependson = "modelfitparamsq2"


# tcontrol_rcv <- trainControl(method = "repeatedcv",
#                          number = number, repeats = repeats)


tcontrol_cv <- trainControl(method = "cv", number = number)



modrf2 <- train( diagnosis ~ . , data = training,
               method = "rf",
               trControl = trainControl(method = "cv", number = number))





#+ modelfit2q2, cache = TRUE, results = "hide", dependson = "modelfitparamsq2"
modgbm2 <- train(diagnosis ~ . , data = training,
                method = "gbm",
                trControl = tcontrol_cv,
                verbose = FALSE)

#+ modelfit3q2, cache = TRUE, results = "hide", dependson = "modelfitparamsq2"
modlda2 <- train(diagnosis ~ . , data = training,
                method = "lda",
                trControl = tcontrol_cv )


# de-register parallel processing
stopCluster(cluster)
registerDoSEQ()




#'
#' Predict diagnosis using each model
#' -----------------------------------
#'

#+ predictQ2, cache = FALSE
trset <- training

trset$predrf <- predict(modrf2, newdata = training[-1])
trset$predgbm <- predict(modgbm2, newdata = training[-1])
trset$predlda <- predict(modlda2, newdata = training[-1])


# Verify now these predictions
#
# pairs(~ predrf + predgbm + predlda, data = trset)

table(trset$predrf,trset$predgbm)

confusionMatrix(trset$predrf,trset$predgbm)$table
confusionMatrix(trset$predrf,trset$predlda)$table

confusionMatrix(trset$predrf,trset$diagnosis)$table
confusionMatrix(trset$predlda,trset$diagnosis)$table

confusionMatrix(trset$predgbm,trset$diagnosis)$table



#'
#' Stack model together using random forest "rf"
#' -------------------------------------------------------------
#'

#+ stackfitq2, cache = TRUE, dependson = "modelfitparamsq2"
modstack2 <- train(diagnosis ~ predrf + predgbm + predlda,
                   data = trset[-1],
                  method = "rf",
                  trControl = trainControl(method = "cv",
                                           number = number #, repeats = repeats
                                           )
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
# we get for cfm_stack: ==> Error in confusionMatrix.default(tset$predstack, tset$diagnosis) :
# The data must contain some levels that overlap the reference.

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
