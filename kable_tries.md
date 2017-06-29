# Kable functions
Bruno Fischer Colonimos  
`r format(Sys.Date(), '%d %B %Y')`  

----------------

Auxiliary code
==============


```r
# def getlib ==> Works in scripts, but not in Rmd
getlib <- function(libname) {
        if (!require(libname, character.only = TRUE)) {
                install.packages(libname)
                library(libname, character.only = TRUE)

        }
}
#  get libraries
getlib("knitr")
```

```
## Loading required package: knitr
```

```r
getlib("psych")
```

```
## Loading required package: psych
```

```r
getlib("reshape2")
```

```
## Loading required package: reshape2
```

```r
getlib("kernlab")
```

```
## Loading required package: kernlab
```

```
## 
## Attaching package: 'kernlab'
```

```
## The following object is masked from 'package:psych':
## 
##     alpha
```


Example data
============

Data Frame
-----------



```r
set.seed(358)

size <- 50
# one factor
fv <- factor(sample(x = c("a", "b", "c"), size = size, replace = TRUE))

# conditional factor values

cfv<- factor( sapply(X = fv, FUN = function(x) {
        if (x == "a") {y <- sample(c("x", "y"), size = 1, prob = c(0.8, 0.2))
        } else if (x == "b") {y <- sample(c("x", "y"), size = 1, prob = c(0.5, 0.5))
        } else {y <- sample(c("x", "y"), size = 1, prob = c(0.1, 0.9))
        }
        y
} ) )

# conditional numeric values
#
cvals <- sapply(X = fv, FUN = function(x) {
        if (x == "a") {y <- rnorm(n = 1, mean = 10, sd = 5)
        } else if (x == "b") {y <- rnorm(n = 1, mean = 15, sd = 5)
        } else {y <- rnorm(n = 1, mean = 20, sd = 5)
        }
        }
        )

# the dataframe
df <- data.frame(
        fv <- fv,
        cfv <- cfv,
        cvals <- cvals,
        vals = rnorm(n = size, mean = 10, sd = 5)
)
```


Example Matrices
----------------



```r
# matrix without names
m0 <- matrix(1:12, nrow = 3)
m0
```

```
##      [,1] [,2] [,3] [,4]
## [1,]    1    4    7   10
## [2,]    2    5    8   11
## [3,]    3    6    9   12
```

```r
# matrix with column names
# mcol
mcol <- m0
colnames(mcol) <- rep("", ncol(mcol))
colnames(mcol) <- paste0("Col ", 1:ncol(mcol))
mcol
```

```
##      Col 1 Col 2 Col 3 Col 4
## [1,]     1     4     7    10
## [2,]     2     5     8    11
## [3,]     3     6     9    12
```

```r
# matrix with row names
# mrow
mrow <- m0
rownames(mrow) <- rep("", nrow(mrow))
rownames(mrow) <- paste0("Row ", 1:nrow(mrow))
mrow
```

```
##       [,1] [,2] [,3] [,4]
## Row 1    1    4    7   10
## Row 2    2    5    8   11
## Row 3    3    6    9   12
```

```r
# matrix with column and rox names
# mcr
mcr <- m0
colnames(mcr) <- rep("", ncol(mcr))
colnames(mcr) <- paste0("Col ", 1:ncol(mcr))
rownames(mcr) <- rep("", nrow(mcol))
rownames(mcr) <- paste0("Row ", 1:nrow(mcol))
mcr
```

```
##       Col 1 Col 2 Col 3 Col 4
## Row 1     1     4     7    10
## Row 2     2     5     8    11
## Row 3     3     6     9    12
```


Experiments with kable + matrices
=================================



```r
kable(m0) #fails : no header
```



---  ---  ---  ---
  1    4    7   10
  2    5    8   11
  3    6    9   12
---  ---  ---  ---

```r
kable(mrow) #fails  : no header
```



------  ---  ---  ---  ---
Row 1     1    4    7   10
Row 2     2    5    8   11
Row 3     3    6    9   12
------  ---  ---  ---  ---

```r
kable(mcol) # works ok
```



 Col 1   Col 2   Col 3   Col 4
------  ------  ------  ------
     1       4       7      10
     2       5       8      11
     3       6       9      12

```r
kable(mcr) # works ok
```

         Col 1   Col 2   Col 3   Col 4
------  ------  ------  ------  ------
Row 1        1       4       7      10
Row 2        2       5       8      11
Row 3        3       6       9      12

```r
try(expr = kable(m0), silent = TRUE)
```



---  ---  ---  ---
  1    4    7   10
  2    5    8   11
  3    6    9   12
---  ---  ---  ---


Analysis of Kable results+ modifications of those
-------------------------------------------------



```r
k1 <- kable(mcol) # works ok
k1
```



 Col 1   Col 2   Col 3   Col 4
------  ------  ------  ------
     1       4       7      10
     2       5       8      11
     3       6       9      12

```r
# str(k1)
# Class 'knitr_kable'
# atomic [1:5]  | Col 1| Col 2| Col 3| Col 4|
#               |-----:|-----:|-----:|-----:
#               |     1|     4|     7|    10|
#               |     2|     5|     8|    11| ...
#
# ..- attr(*, "format")= chr "markdown"

# attr(k1, "format")

k1
```



 Col 1   Col 2   Col 3   Col 4
------  ------  ------  ------
     1       4       7      10
     2       5       8      11
     3       6       9      12

```r
k1[1]
```

```
## [1] " Col 1   Col 2   Col 3   Col 4"
```

```r
k1[2]
```

```
## [1] "------  ------  ------  ------"
```

```r
k1[3]
```

```
## [1] "     1       4       7      10"
```

```r
k1[4]
```

```
## [1] "     2       5       8      11"
```

```r
k1[5]
```

```
## [1] "     3       6       9      12"
```

```r
# ajouter une ligne ? OK
k1[6] <- k1[3]
k1[7] <- "|...|...|...|...|"

k1
```



 Col 1   Col 2   Col 3   Col 4
------  ------  ------  ------
     1       4       7      10
     2       5       8      11
     3       6       9      12
     1       4       7      10
|...|...|...|...|

```r
# Key attributes of a kable result
#
Kclass <- class(k1)
Kformat <- attr(k1, "format")


# make a kable result
# make lines
k2 <- c( "|  a|  b|", "|---|---|", "|2500|7000|")
k2
```

```
## [1] "|  a|  b|"   "|---|---|"   "|2500|7000|"
```

```r
class(k2) <- Kclass
attr(k2, "format") <-  Kformat

k2
```



|  a|  b|
|---|---|
|2500|7000|


---
title: "kable_tries.R"
author: "Bruno"
date: "Wed Jun 28 00:06:38 2017"
---
