#' ---
#' title: 'The caret package lecture 2.3 : Training options'
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





#' Lecture 2.3 :Training options:
#' ==============================
#' SPAM Example contd

inTrain <- createDataPartition(y=spam$type,
                               p=0.75, list=FALSE)
training <- spam[inTrain,]
testing <- spam[-inTrain,]

# Training (standard)
modelFit <- train(type ~.,data=training, method="glm")



#' Train options
#' --------------

args(train.default)

function (x, y, method = "rf", preProcess = NULL, ..., weights = NULL,
          metric = ifelse(is.factor(y), "Accuracy", "RMSE"),
          maximize = ifelse(metric == "RMSE", FALSE, TRUE),
          trControl = trainControl(), tuneGrid = NULL,
          tuneLength = 3)
        NULL

#' Metric options
#'
#' Continous outcomes:
#'      RMSE = Root mean squared error
#'      RSquared = $R^2$ from regression models
#'
#' Categorical outcomes:
#'      Accuracy = Fraction correct
#'      Kappa = A measure of concordance
#'



#' trainControl
#' ------------

args(trainControl)

function (method = "boot",
          number = ifelse(method %in% c("cv", "repeatedcv"), 10, 25),
          repeats = ifelse(method %in% c("cv", "repeatedcv"), 1, number),
          p = 0.75, initialWindow = NULL,
          horizon = 1, fixedWindow = TRUE, verboseIter = FALSE, returnData = TRUE,
          returnResamp = "final", savePredictions = FALSE, classProbs = FALSE,
          summaryFunction = defaultSummary, selectionFunction = "best",
          custom = NULL, preProcOptions = list(thresh = 0.95, ICAcomp = 3,
                                               k = 5), index = NULL, indexOut = NULL, timingSamps = 0,
          predictionBounds = rep(FALSE, 2), seeds = NA, allowParallel = TRUE)
        NULL



#' trainControl resampling
#' -----------------------

#' method
#'      boot = bootstrapping
#'      boot632 = bootstrapping with adjustment
#'      cv = cross validation
#'      repeatedcv = repeated cross validation
#'      LOOCV = leave one out cross validation
#' number
#'      For boot/cross validation
#'      Number of subsamples to take
#' repeats
#'      Number of times to repeate subsampling
#'      If big this can slow things down


#' Setting the seed
#' ----------------
#'
#' It is often useful to set an overall seed
#' You can also set a seed for each resample
#' Seeding each resample is useful for parallel fits



#' seed example
#' ------------

set.seed(1235)
modelFit2 <- train(type ~.,data=training, method="glm")
modelFit2


#' Further resources
#' -----------------

#' Caret tutorial
#' Model training and tuning






