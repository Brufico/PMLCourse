#' ---
#' title: Improving Performance of Random Forest in `caret::train()`
#' author: Leonard Greski
#' date: 21/07/2017
#' output:
#'   html_document:
#'      number_sections: yes
#'      theme: readable
#'      toc: yes
#' ---
#'
#'
#' ****************
#'
#'
#' the Sonar example
#' ==================


#' Data
#' ----


library(mlbench)
data(Sonar)
library(caret)
set.seed(95014)

# create training & testing data sets

inTraining <- createDataPartition(Sonar$Class, p = .75, list=FALSE)
training <- Sonar[inTraining,]
testing <- Sonar[-inTraining,]

# set up training run for x / y syntax because model format performs poorly
x <- training[,-61]
y <- training[,61]


#' The Process: A Parallel Implementation of Random Forest
#' -------------------------------------------------------

#' Once a person works through the varied sources of
#' documentation on the machine learning models and
#' supporting R packages, the process for executing a random
#' forest model (or any other model) in caret::train() is
#' relatively straightforward, and includes the following
#' steps.
#'
#' 1. Configure parallel processing
#' 2. Configure trainControl object
#' 3. Develop training model
#' 4. De-register parallel processing cluster
#'
#'
#' Configure parallel processing
#' -------------------------------------------------------

library(parallel)
library(doParallel)
cluster <- makeCluster(detectCores() - 1) # convention to leave 1 core for OS
registerDoParallel(cluster)

#'
#' Configure trainControl object
#' -----------------------------
#'
#' Step 2: Configure trainControl object

#' The most critical arguments for the trainControl function
#' are the resampling metdhod `method`, the number that
#' specifies the quantity of folds for k-fold
#' cross-validation, and allowParallel which tells caret to
#' use the cluster that we've registered in the previous
#' step.

fitControl <- trainControl(method = "cv",
                           number = 10,
                           allowParallel = TRUE)


#' Develop training model
#' ----------------------

#' Next, we use caret::train() to train the model, using the
#' trainControl() object that we just created.

fit <- train(x,y, method="rf",data=Sonar, #why do we need data=?
             trControl = fitControl)

#' De-register parallel processing cluster
#' ---------------------------------------

#' After processing the data, we explicitly shut down the
#' cluster by calling the `stopCluster()` and `registerDoSEQ()`
#' functions. `registerDoSEQ()`` function is required to force
#' R to return to single threaded processing.

stopCluster(cluster)
registerDoSEQ()


# At this point we have a trained model in the fit object,
# and can take a number of steps to evaluate the suitability
# of this model, including accuracy and a confusion matrix
# that is based on comparing the modeled data to the held
# out folds.

fit
fit$resample
confusionMatrix.train(fit)

#' If desired, at this point one can make a prediction on
#' the held out testing data partition. Since the primary
#' purpose of this article is to illustrate the syntax
#' required for parallel processing and to discuss its
#' impact on the course project for Practical Machine
#' Learning, we will not fit the testing data or evaluate
#' the model accuracy here.



