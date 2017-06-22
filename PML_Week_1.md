# Practical Machine Learning Course, Week 1
Bruno Fischer Colonimos  
`r format(Sys.Date(), '%d %B %Y')`  



--------------------------------------------------------------------------------

The steps
==========

question -> input data -> features -> algorithm -> parameters -> evaluation

Example: spam
-------------




```r
table(spam$type)
```

```
## 
## nonspam    spam 
##    2788    1813
```

```r
plot (density(spam$your[spam$type == "nonspam"]), 
      col="blue",
      main="",
      xlab="Frequency of 'your'")
lines(density(spam$your[spam$type =="spam"]), col="red")
abline(v=0.5,col="black")
```

![](PML_Week_1_files/figure-html/unnamed-chunk-1-1.png)<!-- -->

Prediction by setting a cutoff level c


```r
prediction <- ifelse(spam$your > 0.5,"spam","nonspam")
ptable <- table(prediction,spam$type)/length(spam$type)
# ptable
# names(ptable)
# colnames(ptable)
# rownames(ptable)
# str(ptable)
# kable(ptable)
kable(ptable, caption = "prediction (row) vs type (column)")
```



Table: prediction (row) vs type (column)

             nonspam        spam
--------  ----------  ----------
nonspam    0.4590306   0.1017170
spam       0.1469246   0.2923278

```r
# # alternative
# fptable <-as.data.frame(ptable)
# kable(
#         reshape(fptable, idvar = c("prediction"), timevar = c("Var2"), direction = "wide")
# )
```


```r
accuracy <- ptable[1,1] + ptable[2,2]
# or
accuracy <- tr((ptable))
```

==> the accuracy of that algorithm is 0.7513584



Relative order of importance
================================

Importance: 
-----------

 $$\textbf {question > data > features > algorithms}$$ 


Collecting the right Data: Garbage in - garbage out 
---------------------------------------------------

$$question -> \textbf{input data} -> features -> algorithm -> parameters -> evaluation$$

1. May be easy (movie ratings -> new movie ratings)
2. May be harder (gene expression data -> disease)
3. Depends on what is a "good prediction".
4. Often more data > better models
5. The most important step!



Features 
---------

$$question -> input data -> \textbf{features} -> algorithm -> parameters -> evaluation$$


Properties of good features

* Lead to data compression
* Retain relevant information
* Are created based on expert application knowledge

Common mistakes

* Trying to automate feature selection
* Not paying attention to data-specific quirks
* Throwing away information unnecessarily


Algorithms matter less than you'd think
-----------------------------------------
$$question -> input data -> features -> \textbf{algorithm} -> parameters -> evaluation$$

Algorithms matter less than you'd think, and this can be a bit of a source of surprise and frustration for some people. It's very typical of many applications  that using a very sensible approach will get you a very large way to solve the problem. And then, getting the absolute best method can improve, but it often doesn't improve that much over, sort of, most good sensible methods. 


Issues to consider
-------------------
* interpretable
* simple
* accurate
* fast
* scalable

Prediction is about accuracy tradeoffs

* Interpretability versus accuracy
* Speed versus accuracy
* Simplicity versus accuracy
* Scalability versus accuracy


Interpretability matters\
Scalability matters

-----------------------------------------------------------------------------------------------------------------

In sample and out of sample errors
==================================

In sample versus out of sample
------------------------------

In Sample Error: The error rate you get on the same data set you used to build your predictor.
Sometimes called resubstitution error.

Out of Sample Error: The error rate you get on a new data set. Sometimes called generalization error.


**Key ideas**

1. Out of sample error is what you care about
2. In sample error < out of sample error
3. The reason is overfitting: Matching your algorithm to the data you have


Example
-------


```r
library(kernlab); data(spam); set.seed(333)
smallSpam <- spam[sample(dim(spam)[1],size=10),]
spamLabel <- (smallSpam$type == "spam")*1 + 1
plot(smallSpam$capitalAve, col=spamLabel)
```

![](PML_Week_1_files/figure-html/errs-1.png)<!-- -->

**Prediction rule 1**

* capitalAve > 2.7 = "spam"
* capitalAve < 2.40 = "nonspam"
* capitalAve between 2.40 and 2.45 = "spam"
* capitalAve between 2.45 and 2.7 = "nonspam"


```r
# function
rule1 <- function(x){
        prediction <- rep(NA,length(x))
        prediction[x > 2.7] <- "spam"
        prediction[x < 2.40] <- "nonspam"
        prediction[(x >= 2.40 & x <= 2.45)] <- "spam"
        prediction[(x > 2.45 & x <= 2.70)] <- "nonspam"
        return(prediction)
}
# application to the training sample
table(rule1(smallSpam$capitalAve), smallSpam$type)
```

```
##          
##           nonspam spam
##   nonspam       5    0
##   spam          0    5
```


(perfect in-sample accuracy)


**Prediction rule 2**

* capitalAve > 2.8 = "spam"
* capitalAve < 2.8 = "nonspam"


```r
# function
rule2 <- function(x, cutoff=2.8){
        prediction <- rep(NA,length(x))
        prediction[x > cutoff] <- "spam"
        prediction[x <= cutoff] <- "nonspam"
        return(prediction)
}
# application to the training sample
table(rule2(smallSpam$capitalAve), smallSpam$type)
```

```
##          
##           nonspam spam
##   nonspam       5    1
##   spam          0    4
```


**Comparison on the whole data**


```r
#  rule1
tb1 <- table(rule1(spam$capitalAve), spam$type)
kable(tb1)
```

           nonspam   spam
--------  --------  -----
nonspam       2141    588
spam           647   1225

```r
#  accuracy
tr(tb1)
```

```
## [1] 3366
```

```r
#  rule2
tb2 <- table(rule2(spam$capitalAve), spam$type)
kable(tb2)
```

           nonspam   spam
--------  --------  -----
nonspam       2224    642
spam           564   1171

```r
# accuracy
tr(tb2)
```

```
## [1] 3395
```

What's going on? **Overfitting**

* Data have two parts
    - Signal
    - Noise
* The goal of a predictor is to find signal
* You can always design a perfect in-sample predictor
* You capture both signal + noise when you do that
* Predictor won't perform as well on new samples


[http://en.wikipedia.org/wiki/Overfitting]




Prediction Study Design
========================


Principles
----------

1. Define your error rate
2. Split data into:
    - Training, Testing, Validation (optional)
3. On the training set pick features
    - Use cross-validation
4. On the training set pick prediction function
    - Use cross-validation
5. If no validation
    - Apply 1 x to test set
6. If validation
    - Apply to test set and refine
    - Apply 1x to validation


Know the benchmarks (?)
----------------------

Study design: Netflix 
----------------------

[http://www2.research.att.com/~volinski/papers/ASAStatComp.pdf]



Avoid small sample sizes
------------------------

* Suppose you are predicting a binary outcome
    - Diseased/healthy
    - Click on add/not click on add
* One classifier is flipping a coin
* Probability of perfect classification is approximately:$\left ( \frac{1}{2} \right )^{sample size}$
    - n = 1 flipping coin 50% chance of 100% accuracy
    - n = 2 flipping coin 25% chance of 100% accuracy
    - n = 10 flipping coin 0.10% chance of 100% accuracy


Rules of thumb for prediction study design
------------------------------------------

* If you have a large sample size
    - 60% training
    - 20% test
    - 20% validation *
* If you have a medium sample size
    - 60% training
    - 40% testing
* If you have a small sample size
    - Do cross validation
    - Report caveat of small sample size


Some principles to remember
---------------------------
* Set the test/validation set aside and *don't look at it*
    - In general *randomly sample* training and test
* Your data sets must reflect structure of the problem
    - If predictions evolve with time split train/test in time chunks (called backtesting in finance)
* All subsets should reflect as much diversity as possible
    - Random assignment does this
    - You can also try to balance by features - but this is tricky



Types of errors
===============

Basic terms
-------------

In general, Positive = identified and negative = rejected. Therefore:

* *True positive* = correctly identified
* *False positive* = incorrectly identified
* *True negative* = correctly rejected
* *False negative* = incorrectly rejected


Medical testing example:

* True positive = Sick people correctly diagnosed as sick
* False positive= Healthy people incorrectly identified as sick
* True negative = Healthy people correctly identified as healthy
* False negative = Sick people incorrectly identified as healthy.


http://en.wikipedia.org/wiki/Sensitivity_and_specificity


Key quantities
--------------

### definition test results

|        | disease + | disease - |
|:------:|:---------:|:---------:|
| Test + |  True +   |  False +  |
| Test - |  False -  |  True  -  |



Sensitivity ==> Pr ( positive test | disease )
Specificity ==> Pr ( negative test | no disease )
Positive Predictive Value ==> Pr (disease | positive test)
Negative Predictive Value ==> Pr ( no disease | negative test)
Accuracy ==> Pr ( correct outcome )


### Key Quantities as fractions

#### Test results

|        |disease +| disease -|
|:------:|:-------:|:--------:|
| Test + |    TP   |    FP    |
| Test - |    FN   |    TN    |


#### definitions

|         Notion            |       Equation          |       Equation (math)   | Meaning |
|:-------------------------:|:-----------------------:|:-----------------------:|:-----------------------:|
|        Sensitivity        |       TP / (TP+FN)      | $\frac {TP}{(TP+FN)}$   |p(test+ knowing disease+) |
|        Specificity        |       TN / (FP+TN)      | $\frac {TN}{(FP+TN)}$   |p(test- knowing disease-) |
| Positive Predictive Value |       TP / (TP+FP)      | $\frac {TP}{(TP+FP)}$   |p(disease+ knowing test+) |
| Negative Predictive Value |       TN /(FN+TN)       | $\frac {TN}{(FN+TN)}$   |p(disease+ knowing test+) |
|          Accuracy         | (TP+TN) / (TP+FP+FN+TN) | $\frac {(TP+TN)}{TP+FP+FN+TN}$|p(correct assessment by test) |

Screening tests
---------------

Assume that some disease has a 0.1% prevalence in the
population. Assume we have a test kit for that disease that
works with 99% sensitivity and 99% specificity. What is the
probability of a person having the disease given the test result
is positive, if we randomly select a subject from:

* the general population?
* a high risk sub-population with 10% disease prevalence?


Sens = p(test+ knowing disease+) = 0.99 = $\frac {TP}{(TP+FN)}$
Spec = p(test- knowing disease-) = 0.99 = $\frac {TN}{(FP+TN)}$
p(disease+) = p (prevalence)
p(disease+ knowing test+) = $\frac {p}{(p \cdot Sensitivity + (1-p) \cdot (1 - Specificity)}$





Error measures for continuous data
==================================

Mean squared error (MSE):

$$\frac{1}{n} \sum_{i=1}^n (Prediction_i - Truth_i)^2$$

Root mean squared error (RMSE):

$$\sqrt{\frac{1}{n} \sum_{i=1}^n(Prediction_i - Truth_i)^2}$$


Common error measures
----------------------

* Mean squared error (or root mean squared error)
    * Continuous data, sensitive to outliers

* Median absolute deviation
    * Continuous data, often more robust

* Sensitivity (recall)
    * If you want few missed positives

* Specificity
    * If you want few negatives called positives

* Accuracy
    * Weights false positives/negatives equally

* Concordance
    * One example is kappa

* Predictive value of a positive (precision)
    * When you are screeing and prevelance is low



Receiver Operating Characteristic (ROC) curves
===============================================

Why a curve?
------------

* In binary classification you are predicting one of two categories
    * Alive/dead
    * Click on ad/don't click
* But your predictions are often quantitative
    * Probability of being alive
    * Prediction on a scale from 1 to 10
* The cutoff you choose gives different results



y = p(true positive)  
x = p(false positive) 

or
(y = sensitivity ?)
(x = 1- specificity ?)


Area under the curve (AUC)
--------------------

AUC = 0.5 ==> guessing\
AUC = 1   ==> perfect classifier

in general, AUC = 0.8 == good




Crossvalidation
===============

Key idea
--------

1. Accuracy on the training set (resubstitution accuracy) is optimistic
2. A better estimate comes from an independent set (test set accuracy)
3. But we can't use the test set when building the model or it becomes part of the training set
4. So we estimate the test set accuracy with the training set.


Cross-validation
----------------

### Approach:

1. Use the training set
2. (Sub-)Split it into training/test sets
3. Build a model on the training set
4. Evaluate on the test set
5. Repeat and average the estimated errors


### Used for

1. Picking variables to include in a model
2. Picking the type of prediction function to use
3. Picking the parameters in the prediction function
4. Comparing different predictors


Ways to do it
--------------

* Random subsampling
* K-fold
* Leave-one-out



Considerations
--------------

* For time series data data must be used in "chunks"
* For k-fold cross validation
    * Larger k = less bias, more variance
    * Smaller k = more bias, less variance
* Random sampling must be done _without replacement_
* Random sampling with replacement is the _bootstrap_
    * Underestimates of the error
    * Can be corrected, but it is complicated ([0.632 Bootstrap](http://www.jstor.org/discover/10.2307/2965703?uid=2&uid=4&sid=21103054448997))
* If you cross-validate to pick predictors estimate you must estimate errors on independent data. 




What data should you use ?
==========================


Key idea
---------

* To predict X use data related to X
    * To predict player performance use data about player performance
    * To predict movie preferences use data about movie preferences
    * To predict hospitalizations use data about hospitalizations


Not a hard rule
----------------

* To predict flu outbreaks use Google searches
[http://www.google.org/flutrends/](http://www.google.org/flutrends/)


* Looser connection = harder prediction
* Data properties matter
* Unrelated data is the most common mistake

[http://www.nejm.org/doi/full/10.1056/NEJMon1211064](http://www.nejm.org/doi/full/10.1056/NEJMon1211064)

