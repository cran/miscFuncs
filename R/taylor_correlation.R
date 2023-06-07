##' cor_taylor function
##'
##' A function to compute Taylor's correlation coefficient ;-)
##'
##' @param X a numeric matrix with number of rows bigger than the number of columns
##' @return Taylor's correlation coefficient, a number between 0 and 1 expressing the amount of dependence between multiple variables.
##' @export


cor_taylor <- function(X){
    if(!inherits(X,"matrix")){
        stop("Input must be inherit 'matrix' class.")
    }
    n <- ncol(X)
    return((1/sqrt(n))*sd(eigen(cor(X))$values))
}
