#' ---
#' title: "Kable functions"
#' subtitle: Define Kable variants to extend possibilities
#' author:  "Bruno Fischer Colonimos"
#' date: "`r format(Sys.Date(), '%d %B %Y')`"
#' fontsize: 12pt
#' urlcolor: blue
#' documentclass: article
#' classoption: a4paper
#' geometry: "left=1.5cm,right=1.5cm,top=1.5cm,bottom=2cm,footskip=1cm"
#' output:
#'   pdf_document:
#'     highlight: monochrome
#'     number_sections: yes
#'     toc: yes
#'     toc_depth: 4
#'
#'   html_document:
#'     number_sections: yes
#'     theme: readable
#'     toc: yes
#' ---
#'
#' ----------------
#'



#' Auxiliary code
#' ==============


# def getlib ==> Works in scripts, but not in Rmd
getlib <- function(libname) {
        if (!require(libname, character.only = TRUE)) {
                install.packages(libname)
                library(libname, character.only = TRUE)

        }
}
#  get libraries
getlib("knitr")
getlib("psych")
getlib("reshape2")
getlib("kernlab")



#'
#' Example data
#' ============
#'
#'Data Frame
#'-----------
#'



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



#'
#' Example Matrices
#' ----------------
#'


# matrix without names
m0 <- matrix(1:12, nrow = 3)
m0

# matrix with column names
# mcol
mcol <- m0
colnames(mcol) <- rep("", ncol(mcol))
colnames(mcol) <- paste0("Col ", 1:ncol(mcol))
mcol

# matrix with row names
# mrow
mrow <- m0
rownames(mrow) <- rep("", nrow(mrow))
rownames(mrow) <- paste0("Row ", 1:nrow(mrow))
mrow


# matrix with column and rox names
# mcr
mcr <- m0
colnames(mcr) <- rep("", ncol(mcr))
colnames(mcr) <- paste0("Col ", 1:ncol(mcr))
rownames(mcr) <- rep("", nrow(mcol))
rownames(mcr) <- paste0("Row ", 1:nrow(mcol))
mcr


#'
#' Experiments with kable + matrices
#' =================================
#'



kable(m0) #fails : no header
kable(mrow) #fails  : no header
kable(mcol) # works ok
kable(mcr) # works ok

try(expr = kable(m0), silent = TRUE)

try
#'
#' Analysis of Kable results+ modifications of those
#' -------------------------------------------------
#'


k1 <- kable(mcol) # works ok
k1

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
k1[1]
k1[2]
k1[3]
k1[4]
k1[5]
# ajouter une ligne ? OK
k1[6] <- k1[3]
k1[7] <- "|...|...|...|...|"

k1

# Key attributes of a kable result
#
Kclass <- class(k1)
Kformat <- attr(k1, "format")


# make a kable result
# make lines
k2 <- c( "|  a|  b|", "|---|---|", "|2500|7000|")
k2

class(k2) <- Kclass
attr(k2, "format") <-  Kformat

k2


