#' ---
#' title: 'The caret package lecture 2.4: Plotting predictors'
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




#' Lecture 2.4 - Plotting predictors - Example: Wage data
#' ======================================================

library(ISLR); library(ggplot2); library(caret); library(gridExtra);
data(Wage)
summary(Wage)


#' Get training/test sets
#' -----------------------

inTrain <- createDataPartition(y=Wage$wage,
                               p=0.7, list=FALSE)
training <- Wage[inTrain,]
testing <- Wage[-inTrain,]
dim(training); dim(testing)



#' Feature plot (caret package)
#' ----------------------------

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

qplot(wage, colour=education,data=training,geom="density")

#' Notes and further reading
#' -------------------------

#' Make your plots only in the training set Don't use the test set for
#' exploration!

#' Things you should be looking for
#' * Imbalance in outcomes/predictors
#' * Outliers
#' * Groups of points not explained by a predictor
#' * Skewed variables


#' ggplot2 tutorial http://rstudio-pubs-static.s3.amazonaws.com/2176_75884214fc524dc0bc2a140573da38bb.html
#' caret visualizations http://caret.r-forge.r-project.org/visualizations.html










