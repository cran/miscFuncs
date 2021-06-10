##' roxtext function
##'
##' A function to generate roxygen documentation templates for functions for example,
##'
##'
##' would generate a template for this function. Note that functions with default arguments that include quotes
##' will throw up an error at the moment, just delete these bits from the string, and if shold work.
##'
##' @param fname the name of a function as a character string or as a direct reference to the function
##' @return minimal roxygen template
##' @export

roxtext <- function(fname){
    mc = match.call()
    if(inherits(fname,"function")){
        fname = as.character(mc[[2]])
    }
    arglist = names(formals(fname))

    cat("##' ",fname," function\n",sep="")
    cat("##'\n")
    cat("##' A function to \n")
    cat("##'\n")
    for (a in arglist){
        cat("##' @param ",a," X \n",sep="")
    }
    cat("##' @return ...\n")
    cat("##' @export\n")
}
