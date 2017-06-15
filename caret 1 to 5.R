#' ---
#' title: 'Practical Machine Learning Week 2: The caret package lectures 2.1 to 2.5'
#' author: Jeff Leek, notes by Bruno Fischer Colonimos
#' date: "`r format(Sys.Date(), '%d %B %Y')`"
#' output:
#'         html_document:
#'             toc: yes
#'             number_sections: yes
#'             theme: readable
#'
#'         pdf_document:
#'             toc: yes
#'             toc_depth: 3
#'             number_sections: yes
#'         urlcolor : blue
#'         fontsize: 11pt
#'         geometry: top=.9in, left=1in, right=1in, bottom = 1in, footskip = 0.3in
#'
#' ---






#'----------------
#'
#' Lecture 2.1 : The caret R package
#' ==================================
#'

#' Caret functionality
#' --------------------
#'
#' * Some preprocessing (cleaning)
#'     * preProcess
#' * Data splitting
#'     * createDataPartition
#'     * createResample
#'     * createTimeSlices
#' * Training/testing functions
#'     * train
#'     * predict
#' * Model comparison
#'     * confusionMatrix
#'
#'
#'
#' Machine learning algorithms in R
#' --------------------------------
#'
#' * Linear discriminant analysis
#' * Regression
#' * Naive Bayes
#' * Support vector machines
#' * Classification and regression trees
#' * Random forests
#' * Boosting
#' * etc.
#'
#' Why caret ?
#' -----------
#' Unifying framework
#'



#'
#' Spam example : Splitting data
#' -----------------------------
#'

#+ getlibs, include = FALSE
if (!require(caret)) {
        install.packages(caret)
        library(caret)
}

if (!require(kernlab)) {
        install.packages(kernlab)
        library(kernlab)
}

if (!require(e1071)) {
        install.packages("e1071")
        library(e1071)
}


#'
#' ### data
#'
#+ getdata
data(spam)
# View(spam)
# colnames(spam)

summary(spam$type)/ sum(summary(spam$type)) # proportion of spam/ nonspam in the data

#'
#' ### split data
#'

# partition the data ==> create InTrain = matrix[ n lines x 1 col] of ints
# Should we not set the seed ?????? = yes
#
inTrain <- createDataPartition(y=spam$type,
                               p=0.75, list=FALSE)
training <- spam[inTrain,] # createtraining set
testing <- spam[-inTrain,] # create testing set
dim(training)

summary(training$type)/ sum(summary(training$type)) # proportion of spam/ nonspam in the training set
summary(testing$type)/ sum(summary(testing$type)) # proportion of spam/ nonspam in the testing set




#'
#' SPAM Example: Fit a model
#' ------------------------
#'
#+ , warning = FALSE
set.seed(32343)
modelFit <- train(type ~.,data=training, method="glm") # fit a glm model. put the model in modelFit
modelFit # take a look at the model


#'
#' SPAM Example: see the Final model
#' --------------------------
#'
modelFit$finalModel


#'
#' SPAM Example: Prediction
#' ------------------------
#'
predictions <- predict(modelFit,newdata=testing) # predict( model, newdata = dataframe)
# predictions # commented out



#'
#' SPAM Example: compute the Confusion Matrix
#' ------------------------------------------

confusionMatrix(predictions,testing$type)



#'
#' Caret Further information
#' ------------------------------------------
#'
#' * Caret tutorials:
#'
#'     * [http://www.edii.uclm.es/~useR-2013/Tutorials/kuhn/user_caret_2up.pdf](http://www.edii.uclm.es/~useR-2013/Tutorials/kuhn/user_caret_2up.pdf)
#'     * [http://cran.r-project.org/web/packages/caret/vignettes/caret.pdf](http://cran.r-project.org/web/packages/caret/vignettes/caret.pdf)
#'
#' * A paper introducing the caret package
#'     * [http://www.jstatsoft.org/v28/i05/paper](http://www.jstatsoft.org/v28/i05/paper)








#'
#' Lecture 2.2 : Data Slicing
#'===========================
#'
#' used to :
#'
#' * make training and test samples
#' * split the training samples in order to do crossvalidation
#'

#' Splitting data : training set vs testing set (again)
#' ---------------------------------------------

summary(spam$type)/ sum(summary(spam$type))

inTrain <- createDataPartition(y=spam$type,
                               p=0.75, list=FALSE)
training <- spam[inTrain,]
testing <- spam[-inTrain,]
dim(training)
dim(testing)





#'
#' SPAM Example: K-fold
#' --------------------
#'

set.seed(32323)
folds <- createFolds(y=spam$type,k=10,
                     list=TRUE,
                     returnTrain=TRUE)
sapply(folds,length)

str(folds)
str(folds[[1]])
folds[[1]][1:20]

folds[[2]][1:20]


set.seed(32323)
folds <- createFolds(y=spam$type,k=10,
                     list=TRUE,returnTrain=FALSE)
sapply(folds,length)

str(folds)
str(folds[[1]])
folds[[1]][1:20]

folds[[2]][1:20]

# do no( understand: according to the lecture, K-fold are used to subset the
# training dataset. ==> ok, here the train has not been used)


#' SPAM Example: Resampling
#' -------------------------

set.seed(32323)
folds <- createResample(y=spam$type,times=10,
                        list=TRUE)
sapply(folds,length)
folds[[1]][1:10]


#' SPAM Example: Time Slices
#' -------------------------

set.seed(32323)
tme <- 1:1000
folds <- createTimeSlices(y=tme,initialWindow=20,
                          horizon=10)
names(folds)
folds$train[[1]]
folds$test[[1]]

folds$train[[2]]
folds$test[[2]]



#' Further information
#' -------------------

#' * Caret tutorials:
#'      http://www.edii.uclm.es/~useR-2013/Tutorials/kuhn/user_caret_2up.pdf
#'      http://cran.r-project.org/web/packages/caret/vignettes/caret.pdf
#' * A paper introducing the caret package
#'      http://www.jstatsoft.org/v28/i05/paper




#'
#' Lecture 2.3 : Training options:
#' ==============================
#'
#' SPAM Example contd

# # as before
# inTrain <- createDataPartition(y=spam$type,
#                                p=0.75, list=FALSE)
# training <- spam[inTrain,]
# testing <- spam[-inTrain,]

# Training (as before)
# modelFit <- train(type ~.,data=training, method="glm")



#'
#' Train options
#' --------------
#'
#+ , eval = FALSE
 args(train.default) # ??? train.default not known to R but help ok

 function (x, y, method = "rf", preProcess = NULL, ..., weights = NULL,
           metric = ifelse(is.factor(y), "Accuracy", "RMSE"),
           maximize = ifelse(metric == "RMSE", FALSE, TRUE),
           trControl = trainControl(), tuneGrid = NULL,
           tuneLength = 3)
         NULL

#'
#' Metric options
#' --------------
#'
#' * Continous outcomes:
#'     * RMSE = Root mean squared error
#'     * RSquared = $R^2$ from regression models
#'
#' * Categorical outcomes:
#'     * Accuracy = Fraction correct
#'     * Kappa = A measure of [concordance](http://en.wikipedia.org/wiki/Cohen%27s_kappa)
#'



#'
#' trainControl
#' ------------
#'

args(trainControl) # works ok




#'
#' trainControl resampling
#' -----------------------
#'
#' * method
#'     * boot = bootstrapping
#'     * boot632 = bootstrapping with adjustment
#'     * cv = cross validation
#'     * repeatedcv = repeated cross validation
#'     * LOOCV = leave one out cross validation
#' * number
#'     * For boot/cross validation
#'     * Number of subsamples to take
#' * repeats
#'     * Number of times to repeate subsampling
#'     * If big this can slow things down


#'
#' Setting the seed
#' ----------------
#'
#' It is often useful to set an overall seed
#' You can also set a seed for each resample
#' Seeding each resample is useful for parallel fits
#'


#'
#' seed example
#' ------------
#'
#+ , warning = FALSE
set.seed(1235)
modelFit2 <- train(type ~.,data=training, method="glm")
modelFit2 # this will yield exactly the same result every time


#' Further resources
#' -----------------
#'
#' * [Caret tutorial](http://www.edii.uclm.es/~useR-2013/Tutorials/kuhn/user_caret_2up.pdf)
#' * [Model training and tuning](http://caret.r-forge.r-project.org/training.html)
#'

#' ---

#'
#' Lecture 2.4 - Plotting predictors - Example: Wage data
#' ======================================================
#'

#+ libraries2, include=FALSE
if (!require(ISLR)) {
        install.packages("ISLR")
        library(ISLR)
}

if (!require(gridExtra)) {
        install.packages("gridExtra")
        library(gridExtra)
}

if (!require(Hmisc)) {
        install.packages("Hmisc")
        library(Hmisc)
}




library(ggplot2)
library(caret)

#'
#' get the data
#' ------------
data(Wage)
summary(Wage)


#'
#' Make training/test sets
#' -----------------------
#'
set.seed(1) # yes! ensures repeatability
inTrain <- createDataPartition(y=Wage$wage,
                               p=0.7, list=FALSE)
training <- Wage[inTrain,]
testing <- Wage[-inTrain,]
dim(training); dim(testing)

# str(training) # check
#

#'
#' Feature plot (caret package)
#' ----------------------------
#'

featurePlot(x=training[,c("age","education","jobclass")],
            y = training$wage,
            plot="pairs")


#' Qplot (ggplot2 package)
#' -----------------------

qplot(age,wage,data=training)



#' Qplot with color (ggplot2 package)
#' ----------------------------------

qplot(age,wage,colour=jobclass,data=training)



#' Add regression smoothers (ggplot2 package)
#' ------------------------------------------

qq <- qplot(age,wage,colour=education,data=training)
qq +  geom_smooth(method='lm',formula=y~x)


#' cut2, making factors (Hmisc package)
#' ------------------------------------

library(Hmisc)
cutWage <- cut2(training$wage,g=3)
table(cutWage)


#' Boxplots with cut2
#' -------------------

p1 <- qplot(cutWage,age, data=training,fill=cutWage,
            geom=c("boxplot"))
p1

#' Boxplots with points overlayed
#' -------------------------------

p2 <- qplot(cutWage,age, data=training,fill=cutWage,
            geom=c("boxplot","jitter"))
grid.arrange(p1,p2,ncol=2)


#' Tables
#' --------

t1 <- table(cutWage,training$jobclass)
t1
prop.table(t1,1) # table of proportions (1 = rows, 2 = per column)


#' Density plots
#' -------------

qplot(wage,colour=education,data=training,geom="density")

#' Notes and further reading
#' -------------------------

#' * Make your plots only in the training set
#'     * Don't use the test set for exploration!
#'
#' * Things you should be looking for
#'     * Imbalance in outcomes/predictors
#'     * Outliers
#'     * Groups of points not explained by a predictor
#'     * Skewed variables

#'
#' ggplot2 tutorial http://rstudio-pubs-static.s3.amazonaws.com/2176_75884214fc524dc0bc2a140573da38bb.html
#' caret visualizations http://caret.r-forge.r-project.org/visualizations.html





#'
#' Lecture 2.5 : Basic Preprocessing
#' =================================
#'

if (!require(RANN)) {
        install.packages("RANN")
        library(RANN)
}


#' Why preprocess?
#' ---------------
#'

#' Sometimes predictors will look very strange, or the distribution will be very
#' strange, and you might need to transform them in order to make them more
#' useful for prediction algorithms.
#'
#' This is particularly true when you're using
#' model based algorithms, like linear discriminate analysis, naive Bayes,
#' linear regression and things like that. We'll talk about all those methods
#' later in the class, but just keep in mind that pre-processing can be more
#' useful often when you're using model based approaches, than when you're using
#' more non parametric approaches

#'
#' ### Load data
#'
# library(caret); library(kernlab);
# data(spam)

#'
#' ### split data
#'
set.seed(1234)
inTrain <- createDataPartition(y=spam$type,
                               p=0.75, list=FALSE)
training <- spam[inTrain,]
testing <- spam[-inTrain,]

names(training)[58]


#' Histogram
hist(training$capitalAve, main="", xlab="ave. capital run length")

#' ==> Very skewed variable, so very hard to model. You may want to reprocess.

#' summaries
mean(training$capitalAve)
sd(training$capitalAve)
#' ==> very high variation for that variable

#'
#' Standardizing ( z score)
#' --------------------------
#'

trainCapAve <- training$capitalAve
trainCapAveS <- (trainCapAve  - mean(trainCapAve))/sd(trainCapAve)
mean(trainCapAveS) # ==> standardized ==> mean = 0
sd(trainCapAveS) # ==> standardized ==> sd = 1


#'
#' Standardizing - test set (with the parameters estimated **from the training set**)
#' -------------------------------------------------------------------
#'

testCapAve <- testing$capitalAve
testCapAveS <- (testCapAve  - mean(trainCapAve))/sd(trainCapAve)
# (Mean and std.dev come from the training set)
mean(testCapAveS) # ==> standardized but  mean not exactly = 0
sd(testCapAveS) # ==> standardized but sd not exactly = 1



#'
#' Standardizing - training set - preProcess function (same job automated)
#' --------------------------------------------------------
#'
# Remark : why leave out column 58 (type = spam or nonspam) ?
#
set.seed(1234) # inutile?
preObj <- preProcess(training[,-58],method=c("center","scale")) # returns a preprocessing object
trainCapAveS <- predict(preObj,training[,-58])$capitalAve # with predict + preproc. objet + training set
mean(trainCapAveS)
sd(trainCapAveS)



#'
#' Standardizing - test set -  preProcess function and preprocessing object
#' -------------------------------------------------------------
#'

testCapAveS <- predict(preObj,testing[,-58])$capitalAve # with predict + preprocessing objet + test set
mean(testCapAveS)
sd(testCapAveS)




#'
#' Standardizing - preProcess argument of the train() function
#' -----------------------------------------------------------
#'
#+ , warning = FALSE, cache = TRUE
set.seed(32343)
modelFit <- train(type ~.,data=training,
                  preProcess=c("center","scale"), method="glm")
modelFit



#'
#' Standardizing - Box-Cox transforms
#' ----------------------------------
#'
#' Box-cox ==> tries to transform in order to make the variable approx normal

preObj <- preProcess(training[,-58],method=c("BoxCox"))
trainCapAveS <- predict(preObj,training[,-58])$capitalAve
par(mfrow=c(1,2)); hist(trainCapAveS); qqnorm(trainCapAveS)
par(mfrow=c(1,1))


#'
#' Standardizing - Imputing data (missing data)
#' --------------------------------------------
#'
#' Prediction models mostly fail with missing data
#' ==> impute data using k-nearest neighbors imputation

set.seed(13343)

# Make some values NA
training$capAve <- training$capitalAve
selectNA <- rbinom(dim(training)[1],size=1,prob=0.05)==1
training$capAve[selectNA] <- NA

# Impute and standardize
preObj <- preProcess(training[,-58],method="knnImpute")
capAve <- predict(preObj,training[,-58])$capAve # necessitates the package RANN ?

# Standardize true values
capAveTruth <- training$capitalAve
capAveTruth <- (capAveTruth-mean(capAveTruth))/sd(capAveTruth)


#'
#' Standardizing - Imputing data - comparing inputed vs true value
#' ---------------------------------------------------------------
#'

#'One thing you can do is:  you can look at the comparison between the actual and
#'inputed values. And we can see how close those two values are to each other.
#'Here you can see the values are mostly very close to zero. So the imputation
#'work relatively well

quantile(capAve - capAveTruth)

#' You can also do look at just the values that were imputed. So again here I'm
#' looking at a capAve quantile of the same difference between the imputed
#' values. And the true values, they're only for the ones that were missing. And
#' here you can see again, most of the values are close to zero, but here we're
#' only looking at the ones we're missing, so clearly some of them are more
#' variable than previously.

quantile((capAve - capAveTruth)[selectNA])

#' And then you can look at the ones that were not the ones that we selected to
#' be NA. And you can see that they're even closer to each other, and so the
#' ones that got imputed are a little bit further apart. But aren't that much
#' further apart.

quantile((capAve - capAveTruth)[!selectNA])


#'
#' Notes and further reading
#' -------------------------
#'
#' * Training and test must be processed in the same way
#' * Test transformations will likely be imperfect
#'     * Especially if the test/training sets collected at different times
#' * Careful when transforming factor variables!
#' * [preprocessing with caret](http://caret.r-forge.r-project.org/preprocess.html)






