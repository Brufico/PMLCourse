ctreeBag$fit


ctreeBag$pred
ldaBag$pred
plsBag$pred
nbBag$pred
svmBag$pred
nnetBag$pred


ctreeBag$aggregate
ldaBag$pred
plsBag$pred
nbBag$pred
svmBag$pred
nnetBag$pred

ctreeBag$



lapply
unlist





object@data@get

party::treeresponse

showMethods(treeresponse)

party::treeresponse.RandomForest


# some tries

m1 <-   data.frame(P1 = 10 + abs(rnorm(n=5, 1, 1)),
                   N1 = - (10 + abs(rnorm(n=5, 1, 1))))
m2 <-  data.frame(P2 = 20 + abs(rnorm(n=5, 1, 1)),
                  N2 = - (20 + abs(rnorm(n=5, 1, 1))))
m3 <-  data.frame(P3 = 30 + abs(rnorm(n=5,0,1)),
                  N3 = - (30 + abs(rnorm(n=5,1,1))))





# begin test
xx <- list(m1,m2,m3)


# step 1 only ------------------------------
if (is.matrix(xx[[1]]) | is.data.frame(xx[[1]])) {
        xpooled <- xx[[1]] & NA
        classes <- colnames(xpooled)

}

xpooled # copy of xx[[1]], with value NA everywhere
classes # the names of the first 2 columns of xx[[1]]

# step 1 + 2 ------------------------------

# if (is.matrix(xx[[1]]) | is.data.frame(xx[[1]])) {
        xpooled <- xx[[1]] & NA
        classes <- colnames(xpooled)
        message(xpooled)
        message(classes)
        for (i in 1:ncol(xpooled)) {
        # i <- 1 # the column index of dframes/matrices xx[[1]], ..., xx[[j]]
                message(paste("i=",i))
                tmp <- lapply(xx, function(y, col) {y[, col]}, col = i)
                tmp # the list of all columns number i of dframes/matrices xx[[j]],
                    # with each column of dframe/matrix==> one row of tmp

                tmp <- do.call("rbind", tmp) # make matrix with list,
                # get median of the ith each col of tmp, store in the ith
                xpooled[, i] <- apply(tmp, 2, median)
                # median across i, for each j,
                # and store in the ith column of xpooled

                message ("xpooled = ", xpooled)

                        }

# }

bag
