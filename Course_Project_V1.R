#' ---
#' title: "How Well Do You Exercise?"
#' subtitle: "Practical Machine Learning - Course Project"
#' author:  "Bruno Fischer Colonimos"
#' abstract: |
#'         The course project consists in building a prediction model of the quality of
#'         execution of a training exercise based on several types of
#'         accelerometer, gyroscope, magnetometer sensors data.
#' date: "`r format(Sys.Date(), '%d %B %Y')`"
#' fontsize: 12pt
#' urlcolor: blue
#' linkcolor: red
#' # documentclass: article
#' # classoption: a4paper
#' # geometry: "left=1.5cm,right=1.5cm,top=1.5cm,bottom=2cm,footskip=1cm"
#' output:
#'         # pdf_document:
#'         #   highlight: monochrome
#'         #   number_sections: yes
#'         #   toc: yes
#'         #   toc_depth: 4
#'         html_document:
#'         number_sections: yes
#' theme: readable
#' toc: yes
#' toc_depth: 4
#' ---

#'
#' Preliminary code
#' ================
#'
#' libraries
#' ---------
library(caret)

#' Global options
#' --------------

retrievedata = FALSE


#' Data
#' ====

#' Reference: Velloso, E.; Bulling, A.; Gellersen, H.; Ugulino, W.; Fuks, H.
#' Qualitative Activity Recognition of Weight Lifting Exercises.
#' Proceedings of 4th International Conference in Cooperation with SIGCHI (Augmented Human '13) . Stuttgart, Germany: ACM SIGCHI, 2013.
#' [See more](http://groupware.les.inf.puc-rio.br/har#weight_lifting_exercises#ixzz4nrBaHTUn)


datadir <- "data"
traindatafile <- "pml_training.csv"
testdatafile <- "pml_testing.csv"
trainpath <- file.path(datadir, traindatafile)
testpath <- file.path(datadir, testdatafile)

urltrain <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
urltest <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

#'
#' Retrieving the data from the internet (only once: `set retrievedata == TRUE`)
#' --------------------------------------------------
if (retrievedata) {
        download.file(urltrain, trainpath)
        download.file(urltest, testpath)
}


#'
#' Read in the data
#' ----------------

training <- read.csv(trainpath)
testing <- read.csv(testpath)

# get the variable names
varnames <- colnames(training)

#'
#' Elimination of unwanted columns
#' -------------------------------

# Summary variable patterns
sumpat <- c("kurtosis_","skewness_", "max_", "min_", "amplitude_", "var_", "avg_", "stddev_")

# identify the summary columns
lmatch <- Reduce(f = function(x,y) {x | y},
                 init= FALSE,
                 x = lapply(seq_along(sumpat),
                            FUN= function(i) {grepl(sumpat[i], varnames)}))




# length(varnames[!lmatch])

# remove summary columns
training <- training[ ,!lmatch]
# remove id + chronological info
training <- training[-(1:7)]


# remove "class" column and store apart
trainclass <- training[[(ncol(training))]] # vector
training <- training[-(ncol(training))]



# do the same for the testing set:*************
varnametst <- colnames(testing)

# identify the summary columns
lmatchtst <- Reduce(f = function(x,y) {x | y},
                 init= FALSE,
                 x = lapply(seq_along(sumpat),
                            FUN= function(i) {grepl(sumpat[i], varnametst)}))


# remove summary columns
testing <- testing[!lmatchtst]
# remove id + chronological info
testing <- testing[-(1:7)]


# Check variable types:
sapply(seq_along(training) ,
       FUN = function(i) {
        varname <- varnames[i]
        variable <- training[[i]]
        list(varname, typeof(variable))})


# ============================================================================

#'  Exploration
#' =============

# Investigate the class
tc <-  table(trainclass)
tc


# principal components

prc <- prcomp(training)
names(prc)

round(prc$sdev/ sum(prc$sdev) * 100, 0)



df <- as.data.frame(prc$x[, 1:10])
colnames(df)
df <- cbind(df, data.frame(classe=trainclass))


# random sample for a subset of df
dfred <- df[sample(x = 1:nrow(df), size = 5000 ), ]

table(dfred$classe)


ggplot(data = dfred, aes(PC1, PC2, color = classe)) + geom_point(alpha = 0.5)

ggplot(data = dfred, aes(PC2, PC3, color = classe)) + geom_point(alpha = 0.5)

ggplot(data = dfred, aes(PC3, PC4, color = classe)) + geom_point(alpha = 0.5)

ggplot(data = dfred, aes(PC1, PC3, color = classe)) + geom_point(alpha = 0.5)
