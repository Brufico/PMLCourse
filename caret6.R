#' ---
#' title: 'Practical Machine Learning Week 2 : The caret package lecture 2.6 - Covariates Creation'
#' author: Jeff Leek, notes by Bruno Fischer Colonimos
#' date: "`r format(Sys.Date(), '%d %B %Y')`"
#' output:
#'         pdf_document:
#'             toc: yes
#'             toc_depth: 3
#'             number_sections: yes
#'         urlcolor : blue
#'         fontsize: 12pt
#'         geometry: top=.5in, left=1in, right=1in, bottom = 1in, footskip = 0.3in
#'
#'         html_document:
#'             toc: yes
#'             number_sections: yes
#'             theme: readable
#' ---



#' Lecture 2.5 : Basic Preprocessing
#' =================================
#'
#' Covariates = predictors = features
#'
#' Two levels of covariate creation
#' --------------------------------

#' Covariates are sometimes called predictors and sometimes called features. They
#' are the variables that you will actually include in your model that you're
#' going to be using to combine them to predict whatever outcome that you care
#' about.
#'
#' * Level 1: From raw data to covariate : image, text.... ==> variables that describe the raw data
#' * Level 2: Transforming tidy covariates

library(kernlab);data(spam)
spam$capitalAveSq <- spam$capitalAve^2


#'
#' Level 1, Raw data -> covariates
#' --------------------------------
#'
#' * Depends heavily on application
#' * The balancing act is summarization vs. information loss

#' * Examples:
#'     * Text files: frequency of words, frequency of phrases (Google ngrams), frequency of capital letters.
#'     * Images: Edges, corners, blobs, ridges (computer vision feature detection)
#'     * Webpages: Number and type of images, position of elements, colors, videos (A/B Testing)
#'     * People: Height, weight, hair color, sex, country of origin.
#' * The more knowledge of the system you have the better the job you will do.
#' * When in doubt, err on the side of more features
#' * Can be automated, but use caution!


#'
#' Level 2, Tidy covariates -> new covariates
#' ------------------------------------------
#'
#' * More necessary for some methods (regression, svms) than for others (classification trees).
#' * Should be done only on the training set
#' * The best approach is through exploratory analysis (plotting/tables)
#' * New covariates should be added to data frames


#'
#' Load example data
#' -----------------
#'

library(ISLR); library(caret)
data(Wage)
inTrain <- createDataPartition(y=Wage$wage,
                               p=0.7, list=FALSE)
training <- Wage[inTrain,]
testing <- Wage[-inTrain,]


#' Common covariates to add, *dummy variables* : function *dummyVars()*
#' --------------------------------------------------------------------

#' Basic idea - convert factor variables to indicator variables : function *dummyVars()* of the caret package
#'
table(training$jobclass)
dummies <- dummyVars(wage ~ jobclass,data=training) # function dummyVars() ==> returns a model object
head(predict(dummies,newdata=training)) # use the object with predict


#'
#' Removing zero covariates (no variability at all): function nearZeroVar()
#' -------------------------------------------------------------------------
#'

nsv <- nearZeroVar(training,saveMetrics=TRUE) # function nearZeroVar() of the caret package
nsv # dataframe:

#' ==> eliminate Sex and Region from the features
#'



#'
#' Spline basis
#' ------------
#'

#'  Sometimes, you want to be able to fit curvy lines, and one way to do that is
#'  with a basis functions, and so you can find those, for example, in the
#'  splines package, and so one thing that you can do is create this, the bs
#'  function will create a polynomial variable. So in this case, we pass at a
#'  single variable, in this case, the training set, we take the age variable,
#'  and we say we want a third degree polynomial for this variable. So when you
#'  do that, you essentially get, you'll get a three-column matrix out. So this
#'  is now three new variables.
#'  variable 1 corresponds to age, the actual age values scaled for computational purposes (?? exact meaning ???)
#'  variable 2 corresponds to age^2
#'  variable 3 corresponds to age^3

library(splines)
bsBasis <- bs(training$age,df=3)
head(bsBasis, 10)

#' See also: ns(),poly()
#'


#'---
#'
#' Fitting curves with splines
#' ---------------------------
#'
# linear model using the splines predictors
lm1 <- lm(wage ~ bsBasis, data=training)

# plot(training$age,training$wage,pch=19,cex=0.5)
# points(training$age, predict(lm1,newdata=training), type = "p", col="red",pch=19,cex=0.5) #????

library(ggplot2)

smoothdf <- data.frame(age = training$age, wage = predict(lm1,newdata=training))

ggplot(training, aes(age, wage)) +
        geom_point( alpha = 0.5) +
        geom_line(data = smoothdf, aes(age, wage), size = 1, color = "red" )



#'
#' Splines on the test set
#' -----------------------
#'
# regenerate features for the test set, based on those of the training set.
bstest <- predict(bsBasis,age=testing$age)
head(bstest)


#' Notes and further reading
#' --------------------------

#'
#' ### Level 1 feature creation (raw data to covariates)
#'
#' * Science is key. Google "feature extraction for [data type]"
#' * Err on overcreation of features
#' * In some applications (images, voices) automated feature creation is possible/necessary
#' * http://www.cs.nyu.edu/~yann/talks/lecun-ranzato-icml2013.pdf

#'
#' ### Level 2 feature creation (covariates to new covariates)
#' * The function preProcess in caret will handle some preprocessing.
#' * Create new covariates if you think they will improve fit
#' * Use exploratory analysis on the training set for creating them
#' * Be careful about overfitting!

#' * [preprocessing with caret](http://caret.r-forge.r-project.org/preprocess.html)
#' * If you want to fit spline models, use the gam method in the caret package
#' which allows smoothing of multiple variables. More on feature creation/data
#' tidying in the Obtaining Data course from the Data Science course track.



