#' ---
#' title: 'The caret package lecture 2.2 : Data Slicing'
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




#' Lecture 2.2 : Data Slicing
#'===========================

#' Splitting data : training set vs testing set (again)
#' ---------------------------------------------

library(caret); library(kernlab); data(spam)
View(spam)

summary(spam$type)/ sum(summary(spam$type))

inTrain <- createDataPartition(y=spam$type,
                               p=0.75, list=FALSE)
training <- spam[inTrain,]
testing <- spam[-inTrain,]
dim(training)
dim(testing)





#' SPAM Example: K-fold
#' --------------------

set.seed(32323)
folds <- createFolds(y=spam$type,k=10,
                     list=TRUE,returnTrain=TRUE)
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

# do no( understand: according to the lecture, K-fold are used tu subset the
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




