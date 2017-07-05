#' ---
#' title: "PML - Quiz3"
#' subtitle: "Practical Machine Learning Week 3 - Quiz"
#' author: "Jeff Leek, notes + Solutions attempts by Bruno Fischer Colonimos"
#' date: "`r format(Sys.Date(), '%d %B %Y')`"
#' fontsize: 12pt
#' # urlcolor: blue
#' documentclass: article
#' classoption: a4paper
#' geometry: "left=1.5cm,right=1.5cm,top=1.5cm,bottom=2cm,footskip=1cm"
#' footer:  \thetitle \ \textemdash \ \thepage
#' output:
#'   pdf_document:
#'     # modified latex template
#'     # template: test_latex_temp
#'     template: latex_temp2.latex
#'     highlight: monochrome
#'     number_sections: yes
#'     toc: yes
#'     toc_depth: 4
#'
#'   html_document:
#'     number_sections: yes
#'     theme: readable
#'     toc: yes
#'     toc_depth: 4
#' ---
#'
#'----------------
#'

#' Preliminary steps
#' =================
#'
#' ## Required packages (install before running)
#'

#+ setupchunk
fullwidth <-  6.8
halfwidth  <-  3.4
knitr::opts_chunk$set(echo = TRUE, fig.asp= 0.75, fig.width = halfwidth, fig.align="center", out.width = "0.48\\textwidth")


#' ## Libraries and auxiliary code (install before running)

#' Hidden.

#+ libs1, echo = FALSE, warning = FALSE, message = FALSE, results = "hide"

library(ggplot2)
library(caret)
library(rpart)
library(rattle)
library(pander)

# change the theme ?
themechange <- TRUE # change to theme_bw
specialpalette <- TRUE # controls the colour of the plots

if (themechange) {
        theme_set(theme_bw() +
                          theme(plot.title = element_text(hjust = 0.5),
                                plot.subtitle = element_text(hjust = 0.5)))
}


if (specialpalette) {
        # palette color-blind-friendly: The palette with black (Winston, Cookbook for R,
        # http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/)
        cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73",
                        "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

        # modified BFC
        cbfPalette <- cbbPalette
        cbfPalette[3] <- cbbPalette[7]
        cbfPalette[6] <- cbbPalette[3]
        cbfPalette[8] <- cbbPalette[7]

        # * `set_default_scale` has been removed. If you want to change the default
        # scale for an aesthetic, just create a function called
        # `scale_aesthetic_continuous` or `scale_aesthetic_discrete` that returns the
        # scale that you want.

        scale_colour_discrete <- function(...) {
                scale_colour_manual(values = cbbPalette)
        }

        scale_fill_discrete <- function(...) {
                scale_fill_manual(values = cbbPalette)
        }

        scale_colour_continuous <- function(...) {
                ggplot2::scale_colour_gradient(
                        low = "#009900", high = "#FF0000")
        }

}

#' --------------------------------------------------------------------------------

#'
#' Question 1
#' ==========
#'
library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)

#'
#' Data partitionning
#' ------------------
#'

# Not this ! ************
# inTrain <- createDataPartition(y = segmentationOriginal$Case ,
#                                p=0.7, list=FALSE)
# training <- segmentationOriginal[inTrain,]
# testing <- segmentationOriginal[-inTrain,]

# curious ***************
# sum(segmentationOriginal$Case == "Train")
# sum(segmentationOriginal$Case == "Test")
# sum(segmentationOriginal$Case != "Test")
# sum(segmentationOriginal$Case != "Train")

# splitting the data (and remove the "Case" column.. )
training <- segmentationOriginal[segmentationOriginal$Case == "Train", -2 ]
testing <- segmentationOriginal[segmentationOriginal$Case != "Train", -2 ]

# Verification
table(segmentationOriginal$Case)
dim(segmentationOriginal)
dim(training)
dim(testing)



#'
#' fit a CART model
#' ----------------
#'

#+ createtree, cache=TRUE
modFit <- train(Class ~ .,method="rpart",data=training)
print(modFit$finalModel)


#+ printtree, fig.asp = 2

fancyRpartPlot(modFit$finalModel,
               main = "Tree",
               palettes=c("Greys", "Oranges", "Reds") )



#'
#' Predict
#' -------
#'
#' ### All test set
#'

predtest <- predict(modFit,newdata = testing)




#'
#' ### New cases
#'
#' * a. TotalIntench2 = 23,000; FiberWidthCh1 = 10; PerimStatusCh1=2
#' * b. TotalIntench2 = 50,000; FiberWidthCh1 = 10;VarIntenCh4 = 100
#' * c. TotalIntench2 = 57,000; FiberWidthCh1 = 8;VarIntenCh4 = 100
#' * d. FiberWidthCh1 = 8;VarIntenCh4 = 100; PerimStatusCh1=2
#'

# newx <- data.frame(TotalIntenCh2 = c(23000, 50000, 57000, -1),
#                    FiberWidthCh1 = c(10, 10, 8, 8),
#                    PerimStatusCh1 = c(2, -1, -1, 2),
#                    VarIntenCh4 = c(-1, 100, 100, 100) )
#
# rownames(newx) <- c("a", "b", "c", "d")
#
#
# newpred <- predict(modFit,newdata=newx)
# ==> Error in eval(predvars, data, env) : object 'Cell' not found

# une dataframe complète (toutes les colonnes) semble nécessaire



# # tentative 2
#
# # a)
# newx_a <- testing[1,]
# newx_a[, "TotalIntenCh2" ] <- 23000
# newx_a[, "FiberWidthCh1" ] <- 10
# newx_a[, "PerimStatusCh1" ] <- 2
#
# newpred <- predict(modFit,newdata=newx_a)
# newpred
#
# # a == PS
#
# # _________
# # b)
# newx_a <- testing[1,]
# newx_a[, "TotalIntenCh2" ] <- 50000
# newx_a[, "FiberWidthCh1" ] <- 10
# newx_a[, "VarIntenCh4 " ] <- 100
#
# newpred <- predict(modFit,newdata=newx_a)
# newpred
#
# # a == PS
# # b == WS
#
# # ______________________
# # c)
# newx_a <- testing[1,]
# newx_a[, "TotalIntenCh2" ] <- 57000
# newx_a[, "FiberWidthCh1" ] <- 8
# newx_a[, "VarIntenCh4 " ] <- 100
#
# newpred <- predict(modFit,newdata=newx_a)
# newpred
#
# # a == PS
# # b == WS
# # c == PS
#
# # ______________________
# # d
# newx_a <- testing[1,]
# newx_a[, "TotalIntenCh2" ] <- NA
# newx_a[, "FiberWidthCh1" ] <- 8
# newx_a[, "VarIntenCh4 " ] <- 100
# newx_a[, "PerimStatusCh1" ] <- 2
#
# newpred <- predict(modFit,newdata=newx_a)
# # Error: la variable 'TotalIntenCh2' a été ajustée avec le
# # type "numeric" mais le type "logical" a été fourni
# newpred
#
# # a == PS
# # b == WS
# # c == PS
# # d == PS (douteux) ==> impossible à prédire ?
#
#Finalement ***********************


newx <- testing[1:4, ]
newx[ , ] <- 0 # unnecessary

# a
newx[1, "TotalIntenCh2" ] <- 23000
newx[1, "FiberWidthCh1" ] <- 10
newx[1, "PerimStatusCh1" ] <- 2
# b
newx[2, "TotalIntenCh2" ] <- 50000
newx[2, "FiberWidthCh1" ] <- 10
newx[2, "VarIntenCh4 " ] <- 100
# c
newx[3, "TotalIntenCh2" ] <- 57000
newx[3, "FiberWidthCh1" ] <- 8
newx[3, "VarIntenCh4 " ] <- 100
# d)
newx[4, "TotalIntenCh2" ] <- NA
newx[4, "FiberWidthCh1" ] <- 8
newx[4, "VarIntenCh4 " ] <- 100
newx[4, "PerimStatusCh1" ] <- 2


predict(modFit, newdata = newx[4,] )

# ==>  PS WS PS PS (valeur quelconque pour TotalIntenCh2, ou
# PS WS PS factor(0) si in.NA(TotalIntenCh2). donc, plutôt a



#'
#' Question 3
#' ==========
#'
#' Data
#' ----
#'

library(pgmm)
data(olive)
olive <- olive[,-1]

# str(olive )

#'
#' Fit a CART model
#' ----------------
#'

#+ createtre_eolive, cache = TRUE
modFit <- train(Area ~ .,method="rpart",data=olive)
print(modFit$finalModel)


#+ printtree_olive, fig.asp = 0.75, fig.width = fullwidth

fancyRpartPlot(modFit$finalModel,
               main = "Tree",
               palettes=c("Greys", "Oranges", "Reds") )

#'
#' Predict
#' --------
#'

newdata = as.data.frame(t(colMeans(olive)))

predict(modFit, newdata = newdata)

## 2.783282
## ==> réponse a


#'
#' Redo all, but converting Area into a factor
#' -------------------------------------------
#'

#+ createtree2
olive$Area <- as.factor(olive$Area)
modFit <- train(Area ~ .,method="rpart",data=olive)
print(modFit$finalModel)


#+ printtree2, fig.asp = 0.75, fig.width = fullwidth

fancyRpartPlot(modFit$finalModel,
               main = "Tree",
               palettes=c("Greys", "Oranges", "Reds") )

#'
#'  Predict2
#'

newdata = as.data.frame(t(colMeans(olive[-1])))
predict(modFit, newdata = newdata)



#'
#' Question 4
#' ==========
#'
#' Data
#' ----
#'

library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)

# added (to eliminate error)
str(SAheart$chd)
SAheart$chd <- as.factor(SAheart$chd)

trainSA = SAheart[train,]
testSA = SAheart[-train,]


#'
#' Model training
#' --------------
#'

# colnames(trainSA)

set.seed(13234)
a_modfit <- train(chd ~ age + alcohol + obesity + tobacco + typea + ldl,
                  method = "glm", family = "binomial",
                data = trainSA )

#'
#' Predictions + Missclassification on the test set
#' ------------------------------------------------
#'
#' ### prediction
#'


a_pred_test <- predict(a_modfit, newdata = testSA)

a_pred_train <- predict(a_modfit, newdata = trainSA)


#'
#' ### Missclassification
#'
#' function

missClass <-  function(values,prediction){
        sum(((prediction > 0.5)*1) != values)/length(values)
}


missClass2 <-  function(values,prediction){
        sum(((prediction == 1)*1) != values)/length(values)
}



# missClass(testSA$chd, a_pred)


mctest <- missClass2(testSA$chd, a_pred_test)
mctrain <- missClass2(trainSA$chd, a_pred_train)

c(mctest, mctrain)

#' Missclassification rates: Test set :`r mctest`, Training set : `r mctrain`

#
# ==> Answer c : 0.31/0.27


#'
#' Question 5
#' ==========
#'
#' Data
#' ----
#'
library(ElemStatLearn)
data(vowel.train)
data(vowel.test)
dim(vowel.train)
dim(vowel.test)

vowel.train$y <- as.factor(vowel.train$y)
vowel.test$y <- as.factor(vowel.test$y)

#'
#' Model training
#' --------------
#'
#+ randomforestfit, cache = TRUE
set.seed(33833)
modFit <- train(y~ ., data=vowel.train, method="rf", prox=TRUE) # prox?
modFit


#' Variable importance
#+ , warning=FALSE
vi <- varImp(modFit)
vi

knitr::kable(vi$importance[])

vio <- order(vi$importance$Overall,  decreasing = TRUE)
vin <- rownames(vi$importance)[vio]
vin
pander(vin)

# conclusion: almost the last: `r pander(vin)`

