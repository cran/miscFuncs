##' qqci function
##'
##' A function to compare quantiles of a given vector against quantiles of a specified distribution. The function outputs
##' simulation-based confidence intervals too. The option of zero-ing the plot (rather than visualising a diagonal line (which
##' can be difficult to interpret) and also standardising (so that varying uncertainty around each quantile appears equal to the eye)
##' are also given.
##'
##' @param x a vector of values to compare
##' @param rfun a function accepting a single argument to generate samples from the comparison distribution, the default is rnorm
##' @param y an optional vector of samples to compare the quantiles against. In the case this is non-null, the function rfun will be automatically chosen as bootstrapping y with replacement and sample zise the same as the length of x. You must specify exactly one of rfun or y.
##' @param ns the number of simulations to generate: the more simulations, the more accurate the confidence bands. Default is 100
##' @param zero logical, whether to zero the plot across the x-axis. Default is FALSE
##' @param standardise logical, whether to standardise so that the variance around each quantile is made constant (this can help in situations where the confidence bands appear very tight in places)
##' @param qts vector of probabilities giving which sample-based empirical quantiles to add to the plot. Default is c(0.025,0.975)
##' @param llwd positive numeric, the width of line to plot, default is 2
##' @param lcol colour of line to plot, default is red
##' @param xlab character, the label for the x-axis
##' @param ylab character, the label for the y-axis
##' @param alpha controls transparency of samples (coloured blue)
##' @param cicol colour of confidence band lines, default is black
##' @param cilwd width of confidence band lines, default is 1
##' @param ... additional arguments to pass to matplot
##' @return Produces a QQ-plot with simulation-based confidence bands
##' @examples qqci(rnorm(1000))
##' @examples qqci(rnorm(1000),zero=TRUE)
##' @examples qqci(rnorm(1000),zero=TRUE,standardise=TRUE)
##' @export

qqci = function(x,rfun=NULL,y=NULL,ns=100,zero=FALSE,standardise=FALSE,qts=c(0.025,0.975),llwd=2,lcol="red",xlab="Theoretical",ylab="Sample",alpha=0.02,cicol="black",cilwd=1,...){

    if(!is.null(rfun) & !is.null(y)){
        stop("Must specify exactly one of 'rfun' or 'y'.")
    }

    if(!is.null(y)){
        rfun = function(n){
            return(sample(y,n,replace=TRUE))
        }
        if(length(y)<length(x)){
            warning("Length of y is less than length of x, proceed with caution!",immediate.=TRUE)
        }
    }
    else{
        if(is.null(rfun)){
            rfun = rnorm
        }
    }
    
    x = sort(x)
    n = length(x)
    smp = sapply(1:ns,function(i){sort(rfun(n))})
    mns = rowMeans(smp)
    sds = apply(smp,1,sd)
    if(!zero & !standardise){
        
    }
    if(zero){
        smp = smp - mns
        x = x - mns
    }
    if(standardise){
        smp = smp / sds
        x = x / sds
    }
    qt = apply(smp,1,quantile,probs=qts,type=1)

    matplot(mns,smp,type="l",col=rgb(0,0,1,alpha=alpha),lwd=llwd,lty="solid",xlab=xlab,ylab=ylab,...)
    lines(mns,x,lwd=llwd,col=lcol)
    matplot(mns,t(qt),type="l",lty="solid",col=cicol,lwd=cilwd,add=TRUE)

}
