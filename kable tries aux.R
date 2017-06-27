#
#' Matrices
#' ======

kable_mat <- function(mm, transpose = TRUE, namecol = "", namerow = NULL, ...) {
        if (transpose == TRUE) {mm <- (t(mm))}

        def_rownames <- paste("col", 1:ncol(mm))
        if (is.null(colnames(mm))) {colnames(mm) <- namecol}
        #         colnames(mm) <- namecol
        #         if (is.null(namerow)) {
        #                 kable(mm, ...)
        #         } else {
        #                 rownames(mm) <- namerow
        #                 kable(mm, row.names = TRUE, ...)
        #         }
        # }
        kable(mm, ...)
}




# named vector
# ------------

# pour les vecteurs nommés
kable_nvect <- function(x, transpose = TRUE, namecol = c(""), namerow = NULL, ...) {
        if (transpose == TRUE) { kable(t(as.matrix(x)), ...)
        } else {
                mm <- as.matrix(x)
                colnames(mm) <- namecol
                if (is.null(namerow)) {
                        kable(mm, ...)
                } else {
                        rownames(mm) <- namerow
                        kable(mm, row.names = TRUE, ...)
                }
        }
}

# explore other possibilities with a matrix
# horizontal display
xx <- c(u = 1, v = 2, w =  3)
mm <- matrix(data = xx, nrow=1)
colnames(mm) <- names(xx)
rownames(mm) <- c("key")
kable(mm)













# new def

kable_nvect <- function(x, transpose = TRUE, namecol = "", namerow = NULL, ...) {
        if (transpose == TRUE) {
                mm <- (t(as.matrix(x)))
        } else {
                mm <- as.matrix(x)
        }
        if (is.null(colnames(mm))) {colnames(mm) <- namecol}
        #         colnames(mm) <- namecol
        #         if (is.null(namerow)) {
        #                 kable(mm, ...)
        #         } else {
        #                 rownames(mm) <- namerow
        #                 kable(mm, row.names = TRUE, ...)
        #         }
        # }
        kable(mm, ...)
}




#tests
#factor summary
sfv <- summary(df$fv)
# kable(sfv) fails
kable_nvect(sfv)
kable_nvect(sfv, transpose = FALSE, namecol = "Effectif")
kable_nvect(sfv, transpose =  FALSE)
kable_nvect(sfv, transpose =  FALSE, namecol = "Effectif")
kable_nvect(sfv, transpose =  FALSE, row.names=FALSE, namecol = "Valeurs")
kable_nvect(sfv, transpose =  TRUE, row.names=FALSE, namecol = "Valeurs")
kable_nvect(sfv, transpose =  TRUE, namerow = "Valeurs")

#numeric summary
sumvals <- summary(dfdat$vals)
# kable(sumvals)
kable_nvect(sumvals, digits=2, align = "cllcll", caption = "zozo")
kable_nvect(sumvals, t = FALSE, digits=2,
            align = "r",
            name = "Valeur",caption = "Résumés")








#' Head of dataframe
#' ------------------
#'

hddf <- head(df)

hddf







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


# ---------------------------------------------------------------------------------


kable

knitr::kable_markdown
