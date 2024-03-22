
##' hCreate function
##'
##' A function used in the forecasting of GARCH(1,1) models
##'
##' @param pars parameters for the GARCH model, these would come from an MCMC run
##' @param y vector of log returns 
##' @param T this is the length of y; allow this to be pre-computed 
##' @return vector of h's

hCreate = function(pars,y,T=length(y)){
    h = 0
    y = c(0,y)
    for(t in 2:(T+1)){
        h[t] = pars[1] + pars[2]*y[t-1]^2 + pars[3]*h[t-1]
    }
    return(h[T+1])
}


##' yhIterate function
##'
##' A function to perform forecasting of the series, used by fcastGARCH
##'
##' @param i the index of the forward lags
##' @param current current matrix of (y,h)
##' @param pars parameters for the GARCH model, these would come from an MCMC run
##' @param eps matrix of Gaussian noise, dimension equal to number of MCMC iterations by the number of forecast lags
##' @param omega matrix of Inverse Gamma noise, dimension equal to number of MCMC iterations by the number of forecast lags
##' @return two column matrix containing forecast y (1st column) and updated h (2nd column)

yhIterate = function(i,current,pars,eps,omega){
    y = eps[,i]*sqrt(((pars[,4]-2)/pars[,4])*omega[,i]*current[,2])
    h = pars[,1] + pars[,2]*current[,1]^2 + pars[,3]*current[,2]
    return(cbind(y,h))
}


##' fcastGARCH function
##'
##' A function to forecast forwards using MCMC samples from the bayesGARCH function from the bayesGARCH package.
##'
##' Suggest thinning MCMC samples to get, say 1000, posterior samples (this can be done post-hoc)
##'
##' See also the function lr2fact for converting log-returns to a factor. Apply this to the output of fcastGARCH
##' in order to undertake forecasting on the scale of the original series (i.e. not the log returns). Quantiles may
##' be computed across the MCMC iterations and then all one needs to do is to multiply the result by the last observed
##' value in the original series (again, not the log returns)
##'
##' @param y vector of log-returns used in fitting the model via bayesGARCH
##' @param parmat a matrix of MCMC samples from the bayesGARCH function e.g. "out$chain1" where "out" is the output of the fitted model and "chain1" is the desired chain 
##' @param l number of lags to forecast forward
##' @return forcast log returns and also forecast y
##' @export

fcastGARCH = function(y,parmat,l){
    T = length(y)
    nits = nrow(parmat)
    hlast = apply(parmat,1,hCreate,y=y,T=T)
    epsmat = matrix(rnorm(nits*l),nits,l)
    omegamat = matrix(rinvgamma(nits*l,parmat[,4]/2,parmat[,4]/2),nits,l)
    current = cbind(y[T],hlast)
    fcasty = matrix(NA,nits,l)
    fcasth = matrix(NA,nits,l)
    for (i in 1:l){
        current = yhIterate(i=i,current=current,pars=parmat,eps=epsmat,omega=omegamat)
        fcasty[,i] = current[,1]
        fcasth[,i] = current[,2]
    }
    return(list(ypred = fcasty,hpred=fcasth))
}



##' lr2fact function
##'
##' Apply this to the output of fcastGARCH in order to undertake forecasting on the scale of the original series (i.e. not the log returns). Quantiles may
##' be computed across the MCMC iterations and then all one needs to do is to multiply the result by the last observed
##' value in the original series (again, not the log returns)
##'
##' @param mod the output of fcastGARCH 
##' @return the multiplicative factors.
##' @export

lr2fact = function(mod){
    return(t(apply(exp(mod$ypred),1,cumprod)))
}
