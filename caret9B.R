#' ---
#' title: 'Practical Machine Learning Week 2 : The caret package lecture 2.9 - Predicting with regression and multiple covariates'
#' author:  Jeff Leek, notes by Bruno Fischer Colonimos
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

#'
#'---
#'
#' Example: Wage data
#' ====================
#'

#+ libs, echo = FALSE,
library(ISLR)
library(ggplot2)
library(caret);

data(Wage)
Wage <- subset(Wage,select=-c(logwage))
summary(Wage)

#' Get training/test sets
#' ======================

set.seed(234)
inTrain <- createDataPartition(y=Wage$wage,
                               p=0.7, list=FALSE)
training <- Wage[inTrain,]; testing <- Wage[-inTrain,]
dim(training); dim(testing)


#'
#' Feature plot
#' =============
#'

featurePlot(x=training[,c("age","education","jobclass")],
            y = training$wage,
            plot="pairs")



#'
#' Plot age versus wage
#' ====================
#'


#' Base
qplot(age,wage,data=training)

#' Plot age versus wage colour by jobclass
qplot(age,wage,colour=jobclass,data=training)

#' Plot age versus wage colour by education
qplot(age,wage,colour=education,data=training)


#' Fit a linear model
#' ==================

#'
#' $$ ED_i = b_0 + b_1 age + b_2 I(Jobclass_i="Information") + \sum_{k=1}^4 \gamma_k I(education_i= level k) $$
#'

modFit<- train(wage ~ age + jobclass + education,
               method = "lm",data=training)
finMod <- modFit$finalModel
print(modFit)
summary(modFit) # the caret model ==
summary(finMod) # the lm model ==  same output

#' levels(training$education) #==>
#' Education levels: "1. < HS Grad", "2. HS Grad", "3. Some College", "4. College Grad", "5. Advanced Degree"
#'

#' Diagnostics
#' ============
#'
#' Predefined diagnostics plots (on final lm model)
#'
#+, fig.height = 6, fig.width = 6
curpar <- par("mfrow")
par(mfrow=c(3,2))
plot(finMod,1,pch=19,cex=0.5,col="#00000040") #' residuals vs fitted
plot(finMod,which=2,pch=19,cex=0.5,col="#00000040") # Normal Q-Q plot residuals
plot(finMod,which=3,pch=19,cex=0.5,col="#00000040") # Scale-Location = sqrt(residuals) vs fitted
plot(finMod,which=4,pch=19,cex=0.5,col="#00000040") # Cook's distance vs index
plot(finMod,which=5,pch=19,cex=0.5,col="#00000040") # Std residuals vs leverage (with cook's distance levels)
plot(finMod,which=6,pch=19,cex=0.5,col="#00000040") # Cook's dist vs leverage (hi/(1-hi))
par(mfrow=curpar)


# by default
curpar <- par("mfrow")
par(mfrow=c(2,2))
plot(finMod,pch=19,cex=0.5, col="#00000040")
par(mfrow=curpar)


# methods(plot)
# plot.lm unknown
# args(plot.lm)

#' residuals vs fitted via ggplot2
train2 <- cbind(training, data.frame(fitted = finMod$fitted, resid = finMod$residuals))
ggplot(data = train2, aes(fitted, resid)) +
        geom_point( color = "#00000040") +
        geom_smooth(color = "red", method = "loess")



#'
#' Color by variables not used in the model
#' ------------------------------------------
#'
# qplot(finMod$fitted,finMod$residuals,colour=race,
#       data=training)

ggplot(data = train2, aes(fitted, resid, color = race)) + geom_point()




#'
#' Plot residuals by index
#' ------------
#'
plot(finMod$residuals, pch=19)

#' No trend!!!
#'


#'
#' Predicted versus truth in test set
#' ----------------------------------
#'

pred <- predict(modFit, testing)
qplot(wage,pred,colour=year,data=testing)


#'
#' If you want to use all covariates
#' -----------------------------
#'
#+ , warning = FALSE
modFitAll<- train(wage ~ .,data=training,method="lm")
pred <- predict(modFitAll, testing)
qplot(wage,pred,data=testing)



#'
#' Notes and further reading
#' -------------------------
#'
#' * Often useful in combination with other models
#' * [Elements of statistical learning](http://www-stat.stanford.edu/~tibs/ElemStatLearn/)
#' * [Modern applied statistics with S](http://www.amazon.com/Modern-Applied-Statistics-W-N-Venables/dp/0387954570)
#' * [Introduction to statistical learning](http://www-bcf.usc.edu/~gareth/ISL/)
#'
