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

retrievedata = FALSE # if TRUE, forces data download


#' Data
#' ====

#' Data derived from : Velloso, E.; Bulling, A.; Gellersen, H.; Ugulino, W.; Fuks, H.
#' _Qualitative Activity Recognition of Weight Lifting Exercises._
#' Proceedings of 4th International Conference in Cooperation with SIGCHI (Augmented Human '13) . Stuttgart, Germany: ACM SIGCHI, 2013.
#' [See more](http://groupware.les.inf.puc-rio.br/har#weight_lifting_exercises#ixzz4nrBaHTUn)


datadir <- "data"
traindatafile <- "pml_training.csv"
testdatafile <- "pml_testing.csv"
trainpath <- file.path(datadir, traindatafile)
testpath <- file.path(datadir, testdatafile)

#' Data location:
urltrain <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
urltest <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

#'
#' Retrieving the data from the internet url (only once: set
#' `retrievedata == TRUE` to force downloading)
#' --------------------------------------------------


if (!file.exists(trainpath) | retrievedata) {
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


trainingcomp <- training
names(trainingcomp)

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


# check for NAs

sum(sapply(seq_along(training) ,
           FUN = function(i) {
                   sum(is.na(training[[i]]))
           }
))
# no missing value

# ============================================================================

#'  Exploration
#' =============

# Investigate the class
tc <-  table(trainclass)
tc


#' principal components
#' --------------------

prc <- prcomp(training)
names(prc)

prcp <- prc$sdev / sum(prc$sdev)
sprcp <- cumsum(prcp)

ggplot(data = data.frame(PC = seq_along(prcp),
                         pcvar = prcp,
                         cumpcvar = sprcp),
       aes(PC)) +
        geom_line(aes(y = pcvar, linetype = "% variance"), color = "red" ) +
        geom_line(aes(y = cumpcvar, linetype = "cum % variance"), color = "black" )



df <- as.data.frame(prc$x[, 1:10])
colnames(df)
df <- cbind(df, data.frame(classe=trainclass))


# random sample for a subset of df
dfred <- df[sample(x = 1:nrow(df), size = 5000 ), ]

table(dfred$classe)


ggplot(data = dfred, aes(PC1, PC2, color = classe)) + geom_point(alpha = 0.5)
ggplot(data = dfred, aes(PC1, PC3, color = classe)) + geom_point(alpha = 0.5)
ggplot(data = dfred, aes(PC1, PC4, color = classe)) + geom_point(alpha = 0.5)
ggplot(data = dfred, aes(PC1, PC5, color = classe)) + geom_point(alpha = 0.5)

ggplot(data = dfred, aes(PC2, PC3, color = classe)) + geom_point(alpha = 0.5)
ggplot(data = dfred, aes(PC2, PC4, color = classe)) + geom_point(alpha = 0.5)
ggplot(data = dfred, aes(PC2, PC5, color = classe)) + geom_point(alpha = 0.5)

ggplot(data = dfred, aes(PC3, PC4, color = classe)) + geom_point(alpha = 0.5)
ggplot(data = dfred, aes(PC3, PC5, color = classe)) + geom_point(alpha = 0.5)
ggplot(data = dfred, aes(PC3, PC5, color = classe)) + geom_point(alpha = 0.5)

ggplot(data = dfred, aes(PC4, PC5, color = classe)) + geom_point(alpha = 0.5)


# alternative cuts
ggplot(dfred, aes(x = PC3))+ geom_histogram()

dfred$PC3cut <- cut(dfred$PC3, breaks = c(- 1500, -250 , -750 ,500),
                    ordered_result = TRUE)

ggplot(dfred)+ geom_bar(aes(dfred$PC3cut))

ggplot(data = dfred, aes(PC1, PC2, color = classe)) +
        geom_point(alpha = 0.5) +
        facet_wrap("PC3cut")


ggplot(data = dfred, aes(PC1, PC5, color = classe)) +
        geom_point(alpha = 0.5) +
        facet_wrap("PC3cut")



#'
#' Classification
#' ==============
#'

#' glm impossible (more than 2 classes)


#' knn
#' ----

nfolds <- 5


modknn <- train(classe ~ . , data = trainingcomp,
                method = "knn",
                trControl = trainControl(method = "cv",
                                         number = nfolds))

modknn
# k-Nearest Neighbors
#
# 19622 samples
# 52 predictor
# 5 classes: 'A', 'B', 'C', 'D', 'E'
#
# No pre-processing
# Resampling: Cross-Validated (5 fold)
# Summary of sample sizes: 15698, 15698, 15698, 15698, 15696
# Resampling results across tuning parameters:
#
#         k  Accuracy   Kappa
# 5  0.9207010  0.8996626
# 7  0.8994492  0.8727446
# 9  0.8814586  0.8499469
#
# Accuracy was used to select the optimal model using  the largest value.
# The final value used for the model was k = 5.



#' knn with pca
#' ----

nfolds <- 5


modknnpca <- train(classe ~ . , data = trainingcomp,
                method = "knn",
                preProcess= "pca",
                trControl = trainControl(method = "cv",
                                         number = nfolds))

modknnpca


# k-Nearest Neighbors
#
# 19622 samples
# 52 predictor
# 5 classes: 'A', 'B', 'C', 'D', 'E'
#
# Pre-processing: principal component signal extraction (52), centered (52), scaled (52)
# Resampling: Cross-Validated (5 fold)
# Summary of sample sizes: 15698, 15696, 15697, 15698, 15699
# Resampling results across tuning parameters:
#
#         k  Accuracy   Kappa
# 5  0.9669241  0.9581582
# 7  0.9563746  0.9447980
# 9  0.9452647  0.9307305
#
# Accuracy was used to select the optimal model using  the largest value.
# The final value used for the model was k = 5.









#'
#' Trees
#'-----

# with pce
modcart <- train(classe ~ . , data = trainingcomp,
                method = "rpart",
                preProcess= "pca",
                trControl = trainControl(method = "cv",
                                         number = nfolds))
modcart

# CART
#
# 19622 samples
# 52 predictor
# 5 classes: 'A', 'B', 'C', 'D', 'E'
#
# Pre-processing: principal component signal extraction (52), centered (52), scaled (52)
# Resampling: Cross-Validated (5 fold)
# Summary of sample sizes: 15697, 15696, 15698, 15698, 15699
# Resampling results across tuning parameters:
#
#         cp          Accuracy   Kappa
# 0.03567868  0.3709122  0.1520145
# 0.05998671  0.3548102  0.1203340
# 0.11515454  0.2843747  0.0000000
#
# Accuracy was used to select the optimal model using  the largest value.
# The final value used for the model was cp = 0.03567868.


#'
#' Random forests
#'-----


library(parallel)
library(doParallel)
cluster <- makeCluster(detectCores() - 1) # convention to leave 1 core for OS
registerDoParallel(cluster)

modrfpca <- train(classe ~ . , data = trainingcomp,
                 method = "rf",
                 preProcess= "pca",
                 trControl = trainControl(method = "cv",
                                          number = nfolds))



stopCluster(cluster)
registerDoSEQ()

modrfpca

# Random Forest
#
# 19622 samples
# 52 predictor
# 5 classes: 'A', 'B', 'C', 'D', 'E'
#
# Pre-processing: principal component signal extraction (52), centered (52), scaled (52)
# Resampling: Cross-Validated (5 fold)
# Summary of sample sizes: 15697, 15697, 15698, 15698, 15698
# Resampling results across tuning parameters:
#
#         mtry  Accuracy   Kappa
# 2    0.9809905  0.9759506
# 27    0.9728364  0.9656388
# 52    0.9729384  0.9657643
#
# Accuracy was used to select the optimal model using  the largest value.
# The final value used for the model was mtry = 2.


#'
#' qda
#' -----
#'


library(parallel)
library(doParallel)
cluster <- makeCluster(detectCores() - 1) # convention to leave 1 core for OS
registerDoParallel(cluster)

modqda <- train(classe ~ . , data = trainingcomp,
                  method = "qda",
                  #preProcess= "pca",
                  trControl = trainControl(method = "cv",
                                           number = nfolds))



stopCluster(cluster)
registerDoSEQ()

modqda
names(modqda)
# Quadratic Discriminant Analysis
#
# 19622 samples
# 52 predictor
# 5 classes: 'A', 'B', 'C', 'D', 'E'
#
# No pre-processing
# Resampling: Cross-Validated (5 fold)
# Summary of sample sizes: 15699, 15698, 15698, 15697, 15696
# Resampling results:
#
#         Accuracy   Kappa
# 0.8938953  0.8659797
#
# > modqda
# Quadratic Discriminant Analysis
#
# 19622 samples
# 52 predictor
# 5 classes: 'A', 'B', 'C', 'D', 'E'
#
# No pre-processing
# Resampling: Cross-Validated (5 fold)
# Summary of sample sizes: 15699, 15698, 15698, 15697, 15696
# Resampling results:
#
#         Accuracy   Kappa
# 0.8938953  0.8659797

modqda$resample



#'
#' qda with pca
#' -----
#'


library(parallel)
library(doParallel)
cluster <- makeCluster(detectCores() - 1) # convention to leave 1 core for OS
registerDoParallel(cluster)

modqdapca <- train(classe ~ . , data = trainingcomp,
                method = "qda",
                preProcess= "pca",
                trControl = trainControl(method = "cv",
                                         number = nfolds))



stopCluster(cluster)
registerDoSEQ()

modqdapca

# Quadratic Discriminant Analysis
#
# 19622 samples
# 52 predictor
# 5 classes: 'A', 'B', 'C', 'D', 'E'
#
# Pre-processing: principal component signal extraction (52), centered (52), scaled (52)
# Resampling: Cross-Validated (5 fold)
# Summary of sample sizes: 15697, 15697, 15698, 15697, 15699
# Resampling results:

#         Accuracy   Kappa
# 0.7416683  0.6762325



#'
#' lda (with pca)
#' --------------
#'


library(parallel)
library(doParallel)
cluster <- makeCluster(detectCores() - 1) # convention to leave 1 core for OS
registerDoParallel(cluster)

modlda <- train(classe ~ . , data = trainingcomp,
                method = "lda",
                preProcess= "pca",
                trControl = trainControl(method = "cv",
                                         number = nfolds))



stopCluster(cluster)
registerDoSEQ()

modlda

# Linear Discriminant Analysis
#
# 19622 samples
# 52 predictor
# 5 classes: 'A', 'B', 'C', 'D', 'E'
#
# Pre-processing: principal component signal extraction (52), centered (52), scaled (52)
# Resampling: Cross-Validated (5 fold)
# Summary of sample sizes: 15697, 15697, 15697, 15698, 15699
# Resampling results:
#
#         Accuracy   Kappa
# 0.5273172  0.4011699

modlda$resample




#' Stacking models
#' ---------------

#' We can stack models together: ??? each prediction is qualitative

#' * knn (with pca) == 0.9669241 == modknnpca
#' * Random forest (with PCA)  == 0.9809905 == (modrfpca)
#' * qda (with PCA) == 0.7416683 == modqdapca
#'
