#' ---
#' title: 'The caret package lecture 2.7 : Preprocessing with PCA'
#' author: Bruno Fischer Colonimos
#' date: "`r format(Sys.Date(), '%d %B %Y')`"
#' output:
#'         html_document:
#'             theme: readable
#' ---

library(dplyr)

#' Correlated predictors
#' =====================

library(caret); library(kernlab); data(spam)
inTrain <- createDataPartition(y=spam$type,
                               p=0.75, list=FALSE)
training <- spam[inTrain,]
testing <- spam[-inTrain,]

M <- abs(cor(training[,-58])) # column 58 is the outcome => left out
diag(M) <- 0 # we are not interested in the diagonal coeffs (1)

# mR <- round(M,2)
# fix(mR)

hg <- which(M > 0.80, arr.ind = TRUE)
mm <- sapply(1:dim(hg)[1],
             function(i) {
                     if(hg[i,1]>= hg[i,2] ) {
                             value <- M[hg[i,1], hg[i,2] ]
                             vars <- paste0( colnames(spam)[hg[i,1]], "_", colnames(spam)[hg[i,2]])
                             list(value, vars)
                     } else {list(NA, NA)}

             })

mm[, which(!is.na(mm[1,]))]





#' Correlated predictors
#' =====================

names(spam)[c(34,32)]
plot(spam[,34],spam[,32])

Basic PCA idea

We might not need every predictor
A weighted combination of predictors might be better
We should pick this combination to capture the "most information" possible
Benefits
Reduced number of predictors
Reduced noise (due to averaging)

We could rotate the plot

$$ X = 0.71 \times {\rm num 415} + 0.71 \times {\rm num857}$$

        $$ Y = 0.71 \times {\rm num 415} - 0.71 \times {\rm num857}$$

        X <- 0.71*training$num415 + 0.71*training$num857
Y <- 0.71*training$num415 - 0.71*training$num857
plot(X,Y)

Related problems

You have multivariate variables $X_1,\ldots,X_n$ so $X_1 = (X_{11},\ldots,X_{1m})$

        Find a new set of multivariate variables that are uncorrelated and explain as much variance as possible.
If you put all the variables together in one matrix, find the best matrix created with fewer variables (lower rank) that explains the original data.

The first goal is statistical and the second goal is data compression.
Related solutions - PCA/SVD

SVD

If $X$ is a matrix with each variable in a column and each observation in a row then the SVD is a "matrix decomposition"

$$ X = UDV^T$$

        where the columns of $U$ are orthogonal (left singular vectors), the columns of $V$ are orthogonal (right singluar vectors) and $D$ is a diagonal matrix (singular values).

PCA

The principal components are equal to the right singular values if you first scale (subtract the mean, divide by the standard deviation) the variables.
Principal components in R - prcomp

smallSpam <- spam[,c(34,32)]
prComp <- prcomp(smallSpam)
plot(prComp$x[,1],prComp$x[,2])

Principal components in R - prcomp

prComp$rotation

PCA on SPAM data

typeColor <- ((spam$type=="spam")*1 + 1)
prComp <- prcomp(log10(spam[,-58]+1))
plot(prComp$x[,1],prComp$x[,2],col=typeColor,xlab="PC1",ylab="PC2")

PCA with caret

preProc <- preProcess(log10(spam[,-58]+1),method="pca",pcaComp=2)
spamPC <- predict(preProc,log10(spam[,-58]+1))
plot(spamPC[,1],spamPC[,2],col=typeColor)

Preprocessing with PCA

preProc <- preProcess(log10(training[,-58]+1),method="pca",pcaComp=2)
trainPC <- predict(preProc,log10(training[,-58]+1))
modelFit <- train(training$type ~ .,method="glm",data=trainPC)

Preprocessing with PCA

testPC <- predict(preProc,log10(testing[,-58]+1))
confusionMatrix(testing$type,predict(modelFit,testPC))

Alternative (sets # of PCs)

             modelFit <- train(training$type ~ .,method="glm",preProcess="pca",data=training)
             confusionMatrix(testing$type,predict(modelFit,testing))

             Final thoughts on PCs

             Most useful for linear-type models
             Can make it harder to interpret predictors
             Watch out for outliers!
                     Transform first (with logs/Box Cox)
             Plot predictors to identify problems
             For more info see
             Exploratory Data Analysis
             Elements of Statistical Learning



