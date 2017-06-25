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
#'   highlight: monochrome
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



# Auxiliary code
# ==============


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

df <- data.frame(
        fv <- fv,
        cfv <- cfv,
        cvals <- cvals,
        vals = rnorm(n = size, mean = 10, sd = 5)
)




# named vector
# ------------

# pour les vecteurs nommés
kable_nvect <- function(x, transpose = TRUE, namecol = c(""), ...) {
        if (transpose == TRUE) { kable(t(as.matrix(x)), ...)
        } else {
                mm <- as.matrix(x)
                colnames(mm) <- namecol
                kable(mm, ...)
        }
}

# explore other possibilities with a matrix
# horizontal display
xx <- c(u = 1, v = 2, w =  3)
mm <- matrix(data = xx, nrow=1)
colnames(mm) <- names(xx)
rownames(mm) <- c("key")
kable(mm)








#tests
#factor summary
sfv <- summary(df$fv)
# kable(sfv) fails
kable_nvect(sfv)
kable_nvect(sfv, t = FALSE, n = "Effectif")
kable_nvect(sfv, t = FALSE)
kable_nvect(sfv, t = FALSE, n = "Effectif")

#numeric summary
sumvals <- summary(dfdat$vals)
# kable(sumvals)
kable_nvect(sumvals, digits=2, align = "cllcll", caption = "zozo")
kable_nvect(sumvals, t = FALSE, digits=2,
            align = "r",
            name = "Valeur",caption = "Résumés")
















data("spam")
# head(spam)
colnames(spam)

table(spam$type)

prediction <- ifelse(spam$your > 0.5,"spam","nonspam")
# ptable <- table(prediction,spam$type)/length(spam$type)
# table
rtable <- table(prediction,spam$type)

str(rtable)

kable(rtable)

# proportion tables
rptable <- prop.table(rtable, margin = 1)
cptable <- prop.table(rtable, margin = 2)

rtable2 <- rtable
dim(rtable2)
dn1 <- dimnames(rtable2)
dn1

# dn <- attr(rtable2, "dimnames")
# dn[1]
# dn[2]
names(dn1)[2] <- "TrueStatus"
dimnames(rtable2) <- dn1

rtable2
kable(rtable2)


# as.matrix(kpt)
# mptable <- as.matrix(rtable)
# str(mptable)
# colnames(mptable)
# cat(kable(mptable), sep = "\n")
#
# kable(head(iris), caption = "Title of the table")
# kable(head(mtcars), format = "pandoc", caption = "Title of the table")
# # format numbers using , as decimal point, and ' as thousands separator
# x = as.data.frame(matrix(rnorm(60, 1e+06, 10000), 10))
# kable(x, format.args = list(decimal.mark = ",", big.mark = "'"))
#
# kable(mtcars[, 1:7], longtable=TRUE, booktabs=TRUE)

# --------------------
#
fptable <-as.data.frame(rtable)
kable( reshape(fptable,
               idvar = c("prediction"),
               timevar = c("Var2"),
               direction = "wide"),
       align = "c"
)

fptable <-as.data.frame(rtable2)
kable( reshape(fptable,
               idvar = c("prediction"),
               timevar = c("TrueStatus"),
               direction = "wide"),
       align = "c"
)

sessionInfo()
