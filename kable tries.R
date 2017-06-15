

if(!require(psych)) {
        install.packages("psych")
        library(psych)
}
if(!require(reshape2)) {
        install.packages("reshape2")
        library(reshape2)
}
if(!require(kernlab)) {
        install.packages("kernlab")
        library(kernlab)
}
data("spam")
# head(spam)
colnames(spam)

table(spam$type)

prediction <- ifelse(spam$your > 0.5,"spam","nonspam")
# ptable <- table(prediction,spam$type)/length(spam$type)
# table
rtable <- table(prediction,spam$type)
# proportion tables
rptable <- prop.table(rtable, margin = 1)
cptable <- prop.table(rtable, margin = 2)

# ptable
# names(ptable)
# colnames(ptable)
# rownames(ptable)
# str(ptable)
kable(ptable)
kpt <- kable(ptable, caption = "prediction (row) vs type (column)", digits = 3)
kpt[1]
print(kpt)
cat(kpt, sep = "\n")
str(kpt)

if(!require(xtable)) {
        install.packages("xtable")
        library(xtable)
}

xptable <- xtable(ptable, caption = "tryout")

str(xptable)
methods(xtable)
methods(kable)


fix(print.knitr_kable)
print(knitr::asis_output(kpt))
cat(kpt, sep = "\n")

as.matrix(kpt)
mptable <- as.matrix(ptable)
str(mptable)
colnames(mptable)
cat(kable(mptable), sep = "\n")

kable(head(iris), caption = "Title of the table")
kable(head(mtcars), format = "pandoc", caption = "Title of the table")
# format numbers using , as decimal point, and ' as thousands separator
x = as.data.frame(matrix(rnorm(60, 1e+06, 10000), 10))
kable(x, format.args = list(decimal.mark = ",", big.mark = "'"))

kable(mtcars[, 1:7], longtable=TRUE, booktabs=TRUE)

# --------------------
#
fptable <-as.data.frame(ptable)
kable( reshape(fptable,
               idvar = c("prediction"),
               timevar = c("Var2"),
               direction = "wide"),
       align = "c"
)

