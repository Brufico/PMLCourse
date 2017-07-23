
library(ggplot2)

is.even <- function(n){n %/% 2 == n/2}


# dummy data generation

datagen <- function(n = 50, beta0 = 2, beta1 = 0.1, betanoise = 0.5){
        ix <- 1:n
        vals <-  beta0 + beta1 * ix + betanoise*rnorm(n)
        data.frame(ix = ix, vals = vals)
}

set.seed(10)
df <- datagen()

# movavg function
ma <- function(x,order=5){
        n <- order
        if (!is.even(n)) {
                stats::filter(x,rep(1/n,n), sides=2)
        }else { stats::filter(x,
                              c( 1/ (2*n), rep(1/n,n - 1), 1/ (2*n)),
                              sides=2)} # bogus
}



smovavg <- function(n = 5){
        df$ma <- ma(df$vals, n)

        ggplot(df, aes(ix, vals)) +
                geom_point() + geom_line() +
                geom_point(aes(y = ma), color = "red") + geom_line(aes(y = ma), color = "red")

}


smovavg(12)









