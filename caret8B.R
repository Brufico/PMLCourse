#' ---
#' title: 'Practical Machine Learning Week 2 : The caret package lecture 2.8 - Predicting with regression'
#' author: Jeff Leek, notes by Bruno Fischer Colonimos
#' date: "`r format(Sys.Date(), '%d %B %Y')`"
#' output:
#'         pdf_document:
#'             toc: yes
#'             toc_depth: 3
#'             number_sections: yes
#'         urlcolor : blue
#'         fontsize: 11pt
#'         geometry: top=.9in, left=1in, right=1in, bottom = 1in, footskip = 0.3in
#'
#'         html_document:
#'             theme: readable
#'             toc: yes
#'             number_sections: yes
#' ---

#' -----------------------------------------
#'
#' Key ideas
#' =========
#'

#' * Fit a simple regression model
#' * Plug in new covariates and multiply by the coefficients
#' * Useful when the linear model is (nearly) correct
#'
#' Pros:
#'
#' * Easy to implement
#' * Easy to interpret
#'
#' Cons:
#'
#' * Often poor performance in nonlinear settings
#'
#'
#' Example: Old faithful eruptions
#' --------------------------------
#'

#+ libs, echo = False
library(caret)

#+ data,
data(faithful)
set.seed(333)
inTrain <- createDataPartition(y=faithful$waiting,
                               p=0.5, list=FALSE)
trainFaith <- faithful[inTrain,]; testFaith <- faithful[-inTrain,]
head(trainFaith)


#'
#' Eruption duration versus waiting time
#' ======================================
#'

plot(trainFaith$waiting,trainFaith$eruptions,pch=19,
     col="blue",xlab="Waiting",ylab="Duration")


#' Fit a linear model
#' $$ED_i = b_0 + b_1 WT_i + e_i$$

lm1 <- lm(eruptions ~ waiting,data=trainFaith)
summary(lm1)


#'
#' Model fit
#' =========
#'

plot(trainFaith$waiting,trainFaith$eruptions,
     pch=19,col="blue",xlab="Waiting",ylab="Duration")
lines(trainFaith$waiting,lm1$fitted,lwd=3)


#' Predict a new value
#' ===================

#' $$\hat{ED} = \hat{b}_0 + \hat{b}_1 WT$$

coef(lm1)[1] + coef(lm1)[2]*80

# or, using the lm object
newdata <- data.frame(waiting=80)
predict(lm1,newdata)


#'
#' Plot predictions - training and test
#' ====================================
#'

par(mfrow=c(1,2)) # make 2 plots

plot(trainFaith$waiting,trainFaith$eruptions,
     pch=19,col="blue", main = "Training set", xlab="Waiting",ylab="Duration")
lines(trainFaith$waiting, predict(lm1), lwd=3)

plot(testFaith$waiting,testFaith$eruptions,
     pch=19,col="blue", main = "Test set", xlab="Waiting",ylab="Duration")
lines(testFaith$waiting,predict(lm1, newdata=testFaith), lwd=3)

par(mfrow=c(1,1))


#'
#' Get training set/test set errors
#' ==================================
#'

# Calculate RMSE on training
sqrt(sum((lm1$fitted-trainFaith$eruptions)^2))

# Calculate RMSE on test
sqrt(sum((predict(lm1,newdata=testFaith)-testFaith$eruptions)^2))


#'
#' Prediction intervals
#' =====================
#'

pred1 <- predict(lm1,newdata=testFaith,interval="prediction")
ord <- order(testFaith$waiting)
plot(testFaith$waiting, testFaith$eruptions, pch=19, col="blue")
matlines(testFaith$waiting[ord], pred1[ord,], type="l", ,col=c(1,2,2),lty = c(1,1,1), lwd=3)



#'
#' Same process with caret
#' =======================
#'

modFit <- train(eruptions ~ waiting, data=trainFaith, method = "lm")
summary(modFit$finalModel)

#'
#' Notes and further reading
#' -------------------------
#'

#' Regression models with multiple covariates can be included
#' Often useful in combination with other models

#'
#' Books:
#'
#' * [Elements of statistical learning](http://www-stat.stanford.edu/~tibs/ElemStatLearn/)
#' * [Modern applied statistics with S](http://www.amazon.com/Modern-Applied-Statistics-W-N-Venables/dp/0387954570)
#' * [Introduction to statistical learning](http://www-bcf.usc.edu/~gareth/ISL/)
#'
