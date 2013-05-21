##' roxbc function
##'
##' A function to build and check packages where documentation has been compiled with roxygen. Probably only works in Linux.
##' You will need to clean up old directorys <package_name>.roxygen and <package_name>.Rcheck
##'
##' @param name package name
##' @return builds and checks the package
##' @export

roxbc <- function(name){ # roxygenize, build and check a package

    roxygenize(name,roxygen.dir=paste(name,".roxygen",sep=""))
    out <- system(paste("R CMD build --compact-vignettes=gs ",name,".roxygen",sep=""),intern=TRUE)
    stuff <- out[length(out)]
    if (length(grep("building",stuff))==0){
        system(paste("R CMD build ",name,".roxygen",sep=""))
        stop("Error in building")
    }
    else{
        buildfn <- substr(stuff,gregexpr("building",stuff)[[1]][1] + 10,nchar(stuff)-1)
        system(paste("R CMD check ",buildfn,sep=""))
    }
}
