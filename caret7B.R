#' ---
#' title: 'Practical Machine Learning Week 2 : The caret package lecture 2.7 - Preprocessing with PCA'
#' author: Jeff Leek, notes by Bruno Fischer Colonimos
#' date: "`r format(Sys.Date(), '%d %B %Y')`"
#' output:
#'         html_document:
#'             theme: readable
#'             toc: yes
#'             number_sections: yes
#'
#'         pdf_document:
#'             toc: yes
#'             toc_depth: 3
#'             number_sections: yes
#'         urlcolor : blue
#'         fontsize: 11pt
#'         geometry: top=.9in, left=1in, right=1in, bottom = 1in, footskip = 0.3in
#' ---


#'
#' ---
#'

#' Correlated predictors
#' ======================

#+ lib, include=FALSE, echo=FALSE, warning = FALSE
library(caret)
library(kernlab)

#'
#' get + split Spam data example
#' -----------------------------
#'
#+
data(spam)

set.seed(123)
inTrain <- createDataPartition(y=spam$type,
                               p=0.75, list=FALSE)
training <- spam[inTrain,]
testing <- spam[-inTrain,]


#'
#' Correlated predictors
#' --------------------
#'
#' The first thing that I do is that I leave out just the 58th column of this
#' training set, Which in this case is the outcome. So I'm looking at all the
#' other predictor variables. And I calculate the correlation between all those
#' column and I take its absolute value.

M <- abs(cor(training[,-58]))
diag(M) <- 0

#' And then I look, which of these variables have a high correlation with each other?

which(M > 0.85,arr.ind=T)

# see the names:
names(spam)[c(34,32)]

#' So it turns out two variables have a very high correlation with each other.
#' They are the num415 and num857. So it's how, if the number 415 and 857
#' appears in the email and frequently appears together.

#' And if I plot those two columns against each other, I see exactly what I'd
#' expect. So, the frequency of four 415 and 857 is incredibly highly
#' correlated, this basically lie perfectly on a line with each other
#'
plot(spam[,34],spam[,32])


#'
#' Basic PCA idea
#' ==============
#'
#' * We might not need every predictor
#' * A weighted combination of predictors might be better
#' * We should pick this combination to capture the "most information" possible
#' * Benefits
#'     * Reduced number of predictors
#'     * Reduced noise (due to averaging)
#'

#' We could rotate the plot

#' $$ X = 0.71 \times {\rm num 415} + 0.71 \times {\rm num857}$$
#'
#' $$ Y = 0.71 \times {\rm num 415} - 0.71 \times {\rm num857}$$


X <- 0.71*training$num415 + 0.71*training$num857
Y <- 0.71*training$num415 - 0.71*training$num857
plot(X,Y)


#'
#' Related problems
#' ------------------
#'

#' You have multivariate variables $X_1,\ldots,X_n$ so $X_1 =
#' (X_{11},\ldots,X_{1m})$

#'
#' * Find a new set of multivariate variables that are uncorrelated and explain as much variance as possible.
#' * If you put all the variables together in one matrix, find the best matrix created with fewer variables
#'   (lower rank) that explains the original data.
#'
#' The first goal is statistical and the second goal is data compression.
#'
#'
#' Related solutions - PCA/SVD
#' ---------------------------

#' ### SVD
#'

#' If $X$ is a matrix with each variable in a column and each
#' observation in a row then the SVD is a "matrix
#' decomposition"
#' $$X = UDV^T$$
#'
#' where the columns of $U$ are orthogonal (left singular
#' vectors), the columns of $V$ are orthogonal (right
#' singluar vectors) and $D$ is a diagonal matrix (singular
#' values).
#'
#' ### PCA
#'
#' The principal components are equal to the right singular
#' values (vectors) if you first scale (subtract the mean, divide by
#' the standard deviation) the variables.

#'
#' Principal components in R - prcomp
#' ----------------------------------
#'

smallSpam <- spam[,c(34,32)]
prComp <- prcomp(smallSpam) # principal components object
# str(prComp)
plot(prComp$x[,1],prComp$x[,2])


#' Rotation matrix
prComp$rotation

#'
#' PCA on SPAM data
#' ================
#'

#' So here, I'm creating a variable that's just going to be the color we're
#' going to color our points by. And this statement here calculates the
#' principal components on the entire data set. You'll notice that I've applied
#' a function of the data set, the log 10 transform  and I've added one. I've
#' done this to make the data look a little bit more Gaussian. Because some of
#' the variables are normal looking, because some of the variables are skewed.
#' And you often have to do that for principal component analysis to look
#' sensible. So I can now again plot principal component one, versus principal
#' component two. Principle component one is no longer a very easy addition of
#' two variables. It might be some quite complicated combination of all the
#' variables in the data set. But it's the combination that explains the most
#' variation in the data. Principle component two is the combination that
#' explains the second most variation. And principal component three explains
#' the third most and so forth.

typeColor <- ((spam$type=="spam")*1 + 1)
prComp <- prcomp(log10(spam[,-58]+1))
# plot(prComp$x[,1],prComp$x[,2],col=typeColor,xlab="PC1",ylab="PC2")

# plot with ggplot2
spamcompdf <- cbind(spam, prComp$x[,1:2])
ggplot(spamcompdf, aes(PC1, PC2, color = type)) + geom_point(alpha = 0.5)



#'
#' PCA with caret
#' ==============
#'
#' Whole dataset
#' -------------
#'
preProc <- preProcess(log10(spam[,-58] + 1),
                      method="pca",pcaComp=2)
spamPC <- predict(preProc,log10(spam[,-58] + 1))

# plot(spamPC[,1],spamPC[,2],col=typeColor)

# plot with ggplot2
spamcompdf2 <- cbind(spam, spamPC[,1:2])
ggplot(spamcompdf2, aes(PC1, PC2, color = type)) + geom_point(alpha = 0.5)



#'
#' Preprocessing with PCA (train on training set)
#' =======================
#'

preProc <- preProcess(log10(training[,-58]+1),
                      method="pca",pcaComp=2)
trainPC <- predict(preProc,log10(training[,-58]+1))

#' Does notwork anymore (Post of Leonard Gresky) if all variables do not belong to 'data'
#'
# modelFit <- train(form=training$type ~ .,
#                    method="glm", data=trainPC)
#
#' "You shouldn't use the data set name on the LHS of the formula. The formula
#' interface should be used when the variables are in columns of the object that
#' the data argument refers to.
#'
#' If type is not in training and there are only numeric variables in trainPC,
#' then you should use the non-formula method:"
#'
# modelFit <- train(x = trainPC, y = training$type, method="glm")


#'
#' alternative
#'

#+ , warning =FALSE, cache = TRUE
modelFit <- train(x = trainPC, y = training$type,
                  method="glm")


#'
#' Preprocessing with PCA 2 (predict in test set)
#' ========================
#'

testPC <- predict(preProc,log10(testing[,-58]+1))
confusionMatrix(testing$type,predict(modelFit,testPC))



#' Alternative way (sets # of PCs ?) PCA as argument to the train function
#' -----------------------------------------------------------------------
# error (Post of Leonard Gresky again)
# modelFit <- train(training$type ~ .,method="glm",
#                   preProcess="pca",data=training)
# confusionMatrix(testing$type,predict(modelFit,testing))


# try again, modifying train function call ==> Ok

#+ , warning =FALSE, cache = TRUE
modelFit <- train(x = training[ , -58], y = training$type,
                  preProcess = "pca",
                  method="glm")
confusionMatrix(testing$type,predict(modelFit,testing))




#'
#' Final thoughts on PCs
#' ---------------------


#' * Most useful for linear-type models
#' * Can make it harder to interpret predictors
#' * Watch out for outliers!
#'     * Transform first (with logs/Box Cox)
#'     * Plot predictors to identify problems
#' * For more info see
#'     * Exploratory Data Analysis
#'     * [Elements of Statistical Learning](http://statweb.stanford.edu/~tibs/ElemStatLearn/)
#'


