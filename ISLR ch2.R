#' ---
#' title: 'ISLR chapter 2'
#' author: Bruno Fischer Colonimos
#' date: "`r format(Sys.Date(), '%d %B %Y')`"
#' output:
#'         html_document:
#'             theme: readable
#'         pdf_document:
#'             number_sections: yes
#'             urlcolor: blue
#'             fontsize: 10pt
#'             geometry: top=.9in, left=1in, right=1in, bottom = 1in, footskip = 0.3in
#' ---

# libs
library(pander)



#' Outer product of vector/arrays
#' =============================

# Multiplication Tables
x <- 1:9
names(x) <- x
x %o% x

# Power Tables
y <- 2:8;
names(y) <- paste(y,":", sep = "")
z <- outer(y, x, "^")
str(z)
pander(z)


# Other table
outer(month.abb, 1999:2003, FUN = "paste")

## three way multiplication table:
x %o% x %o% y[1:3]




#' Tries with 3D plots
#' ===================
x <- 1:10
y <- x

f <- outer(x,y,function (x,y) 1 / 2 * x + y + rnorm(1, 0, 2))

persp(x,y,f, col = "yellow", theta = 30, phi = 20)
persp(x,y,f, col = "yellow", theta = 30, phi = 70)
persp(x,y,f, col = "yellow", theta = 30, phi = 40)



#' Tries with  plots and identify()
#' ================================
library(ISLR)
data(Auto)
# fix(Auto)
attach(Auto)

plot(horsepower , mpg)
identify(horsepower ,mpg ,name)



#' Exercise 7: Classification
#' ==========================


# data
dt <- read.csv2(text =
                 "0;3;0;Red
                 2;0;0;Red
                 0;1;3;Red
                 0;1;2;Green
                 −1;0;1;Green
                 1;1;1;Red"
                 # , sep = ";"
                 , header = FALSE
                 # , col.names = c("X1", "X2","X3","Y" )
                 # , row.names = 1
)

colnames(dt) <- c("X1", "X2","X3","Y" )


# euclidean distance
# ------------------
eudist <- function(x, i, dat){
        # (x-as.vector(dt[i,1:3]))^2
        sqrt(sum((x-as.vector(dat[i,1:3]))^2))

}

# # test
# eudist(c(0,0,0), 1, dt)

#  vector of all distances
#  -----------------------

alldist <- function(x, dat) {
        sapply(1:nrow(dt),
               FUN=function(i) {
                       eudist(x, i, dat)
               } )
}

#test
alldist(c(0,0,0), dt)


# what are the k best distances

kmin <- function(values,k) {
        val2 <- values[order(values,decreasing = FALSE)][1:k]
        which(values %in% val2)
}


kmin(c(5, 3 , 7, 4),  2)

# most frequent value of a vector
mostfrequent <- function(values) {
        f <- table(values)
        names(f)[which(f == max(f))]
}

# tests
v <- c("a", "b", "a", "a", "b", "c", "d", "b" )
mostfrequent(v)

v <- c("b", "a", "a", "b", "c", "d", "b" )
mostfrequent(v)


# classification function
classify <- function(x, k, dat, classvarname){
        # get the k min distances (indices)
        kdist <- kmin(alldist(x, dat), k)
        # get the corresponding classes
        classes <- dat[[classvarname]][kdist]
        # return most frequent class
        mostfrequent(classes)

}


# test point
zro <- c(0,0,0)
classify(zro, 1, dt, "Y")
classify(zro, 3, dt, "Y")


#' Exercise 8: college
#' ===================
#'

data("College")
dim(College)
# Already done
rownames(College)

summary(College)

# Explore  .... keep for afterwards.
#
# 8 - Auto
#
# 10 - Boston
