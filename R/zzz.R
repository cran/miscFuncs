##' .onAttach function
##'
##' A function to print a welcome message on loading package
##'
##' @import roxygen2
##' @import mvtnorm
##' @importFrom grDevices dev.off pdf
##' @importFrom graphics plot polygon
##' @importFrom stats cor sd
##' @param libname libname argument
##' @param pkgname pkgname argument
##' @return ...
##' @export

##' @importFrom stats dnorm qnorm

.onAttach <- function(libname, pkgname)
{
	packageStartupMessage("\n Welcome to 'miscFuncs', B. Taylor. \n type 'vignette(\"miscFuncs\")' For help with Kalman Filtering.", appendLF=T)
}
