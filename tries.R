


#'
#' Related problems
#' ------------------
#'

#' You have multivariate variables $X_1,\ldots,X_n$ so $X_1 =
#' (X_{11},\ldots,X_{1m})$

#' Find a new set of multivariate variables that are uncorrelated and explain as much variance as possible.
#' If you put all the variables together in one matrix, find the best matrix created with fewer variables (lower rank) that explains the original data.
#'
#' The first goal is statistical and the second goal is data compression.
#'
#'
#' Related solutions - PCA/SVD
#' ---------------------------

#' ### SVD
#'

#' If $X$ is a matrix with each variable in a column and each
#' observation in a row then the SVD is a "matrix
#' decomposition"
#' $$X = UDV^T$$
#'
#' where the columns of $U$ are orthogonal (left singular
#' vectors), the columns of $V$ are orthogonal (right
#' singluar vectors) and $D$ is a diagonal matrix (singular
#' values).
#'
#' ### PCA
#'
#' The principal components are equal to the right singular
#' values if you first scale (subtract the mean, divide by
#' the standard deviation) the variables.

