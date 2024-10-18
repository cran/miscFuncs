##' .onAttach function
##'
##' A function to print a welcome message on loading package
##'
##' @import roxygen2
##' @import mvtnorm
##' @importFrom grDevices dev.off pdf rgb
##' @importFrom graphics plot polygon lines matplot abline par points text title
##' @importFrom stats cor sd quantile rnorm fitted lm.influence lowess residuals dnorm qnorm formula lm model.frame model.matrix shapiro.test
##' @importFrom extraDistr rinvgamma
##' @param libname libname argument
##' @param pkgname pkgname argument
##' @return ...
##' @export

.onAttach <- function(libname, pkgname)
{
	packageStartupMessage("\n Welcome to 'miscFuncs', B. Taylor. \n type 'vignette(\"miscFuncs\")' For help with Kalman Filtering.", appendLF=T)
}
