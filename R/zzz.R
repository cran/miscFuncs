##' .onAttach function
##'
##' A function to print a welcome message on loading package
##'
##' @import roxygen2
##' @import mvtnorm
##' @importFrom grDevices dev.off pdf rgb
##' @importFrom graphics plot polygon lines matplot
##' @importFrom stats cor sd quantile rnorm
##' @importFrom extraDistr rinvgamma
##' @param libname libname argument
##' @param pkgname pkgname argument
##' @return ...
##' @export

##' @importFrom stats dnorm qnorm

.onAttach <- function(libname, pkgname)
{
	packageStartupMessage("\n Welcome to 'miscFuncs', B. Taylor. \n type 'vignette(\"miscFuncs\")' For help with Kalman Filtering.", appendLF=T)
}
