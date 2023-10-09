##' qqci function
##'
##' A function to compare quantiles of a given vector against quantiles of a specified distribution. The function outputs
##' simulation-based confidence intervals too. The option of zero-ing the plot (rather than visualising a diagonal line (which
##' can be difficult to interpret) and also standardising (so that varying uncertainty around each quantile appears equal to the eye)
##' are also given.
##'
##' @param x a vector of values to compare
##' @param rfun a function accepting a single argument to generate samples from the comparison distribution, the default is rnorm 
##' @param ns the number of simulations to generate: the more simulations, the more accurate the confidence bands. Default is 100
##' @param zero logical, whether to zero the plot across the x-axis. Default is FALSE
##' @param standardise logical, whether to standardise so that the variance around each quantile is made constant (this can help in situations where the confidence bands appear very tight in places)
##' @param qts vector of probabilities giving which sample-based empirical quantiles to add to the plot. Default is c(0.025,0.975)
##' @param lwd positive numeric, the width of line to plot
##' @return Produces a QQ-plot with simulation-based confidence bands
##' @examples qqci(rnorm(1000))
##' @examples qqci(rnorm(1000),zero=TRUE)
##' @examples qqci(rnorm(1000),zero=TRUE,standardise=TRUE)
##' @export

qqci = function(x,rfun=rnorm,ns=100,zero=FALSE,standardise=FALSE,qts=c(0.025,0.975),lwd=2){
    x = sort(x)
    n = length(x)
    smp = sapply(1:ns,function(i){sort(rfun(n))})
    mns = rowMeans(smp)
    sds = apply(smp,1,sd)
    if(zero){
        smp = smp - mns
        x = x-mns
    }
    if(standardise){
        smp = smp / sds
        x = x / sds
    }
    qt = apply(smp,1,quantile,probs=qts,type=1)
    
    matplot(smp,type="l",col=rgb(0,0,1,alpha=0.1),lwd=lwd,lty="solid")
    lines(1:n,x,lwd=lwd,col="red")
    matplot(t(qt),type="l",lty="solid",col="green",lwd=lwd,add=TRUE)
}
