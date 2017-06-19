
# def getlib
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

# getlib(libname = "xtable")



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
