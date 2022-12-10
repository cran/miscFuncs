##' daynames function
##'
##' A function to
##'
##' @return ...
##' @export

daynames = function(){
    return(c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))
}



##' monthnames function
##'
##' A function to
##'
##' @return ...
##' @export

monthnames = function(){
    return(c("January","February","March","April","May","June","July","August","September","October","November","December"))
}


##' sintri function
##'
##' A function to
##'
##' @param x X
##' @return ...
##' @export

sintri = function(x){
    div = floor(x / (2*pi))
    x = x - 2*pi*div # reset to interval $[0,2\pi]$
    y = rep(NA,length(x))
    gr = 1 / (pi/2)
    pi2 = pi/2
    pi32 = 3*pi/2
    y[x <= pi2] = x[x <= pi2] * gr
    y[x >= pi2 & x <= pi] = 1 - ((x[x >= pi2 & x <= pi]-pi2) * gr)
    y[x >= pi & x <= pi32] = - ((x[x >= pi & x <= pi32]-pi) * gr)
    y[x >= pi32 & x <= 2*pi] = -1 + ((x[x >= pi32 & x <= 2*pi]-pi32) * gr)
    return(y)
}



##' costri function
##'
##' A function to
##'
##' @param x X
##' @return ...
##' @export

costri = function(x){
    return(sintri(x+pi/2))
}


##' sinsaw function
##'
##' A function to
##'
##' @param x X
##' @return ...
##' @export

sinsaw = function(x){
    div = floor(x / (2*pi))
    x = x - 2*pi*div # reset to interval $[0,2\pi]$
    y = -1 + x/pi
    return(y)
}



##' cossaw function
##'
##' A function to
##'
##' @param x X
##' @return ...
##' @export

cossaw = function(x){
    return(sinsaw(x+pi/2))
}

##' sinrsaw function
##'
##' A function to
##'
##' @param x X
##' @return ...
##' @export

sinrsaw = function(x){
    div = floor(x / (2*pi))
    x = x - 2*pi*div # reset to interval $[0,2\pi]$
    y = 1 - x/pi
    return(y)
}



##' cosrsaw function
##'
##' A function to
##'
##' @param x X
##' @return ...
##' @export

cosrsaw = function(x){
    return(sinrsaw(x+pi/2))
}

##' sinpulse function
##'
##' A function to
##'
##' @param x X
##' @param tau pulse duration
##' @return ...
##' @export

sinpulse = function(x,tau=pi){
    div = floor(x / (2*pi))
    x = x - 2*pi*div # reset to interval $[0,2\pi]$
    y = rep(-1,length(x))
    y[x <= (tau/2) | x >= (2*pi-tau/2)] = 1
    return(y)
}



##' cospulse function
##'
##' A function to
##'
##' @param x X
##' @param tau pulse duration
##' @return ...
##' @export

cospulse = function(x,tau=pi){
    return(sinpulse(x+pi/2,tau=tau))
}


##' genharmonic function
##'
##' A function to create harmonic terms ready for a harmonic regression model to be fitted.
##'
##' @param df a data frame
##' @param tname a character string, the name of the time variable. Note this variable will be converted using the function as.numeric
##' @param base the period of the first harmonic e.g. for harmonics at the sub-weekly level, one might set base=7 if time is measured in days
##' @param num the number of harmonic terms to return
##' @param sinfun function to compute sin-like components in model. Default is sin, but alternatives include sintri, or any other periodic function defined on [0,2pi]
##' @param cosfun function to compute sin-like components in model. Default is cos, but alternatives include costri, or any other periodic function defined on [0,2pi] offset to sinfun by pi/2
##' @param sname the prefix of the sin terms, default 's' returns variables 's1', 's2', 's3' etc.
##' @param cname the prefix of the cos terms, default 's' returns variables 's1', 's2', 's3' etc.
##' @param power logical, if FALSE (the default) it will return the standard Fourier series with sub-harmonics at 1, 1/2, 1/3, 1/4 of the base periodicicy. If TRUE, a power series will be used instead, with harmonics 1, 1/2, 1/4, 1/8 etc. of the base frequency.
##' @return a data frame with the time variable in numeric form and the harmonic components
##' @export

genharmonic = function(df,tname,base,num,sinfun=sin,cosfun=cos,sname="s",cname="c",power=FALSE){
    t <- as.numeric(df[,tname])

    out <- data.frame(t=t)
    for(i in 1:num){
        if(power){
            out[,paste(sname,i,sep="")] <- sinfun(2*(2^(i-1))*pi*t/base)
            out[,paste(cname,i,sep="")] <- cosfun(2*(2^(i-1))*pi*t/base)
        }
        else{
            out[,paste(sname,i,sep="")] <- sinfun(2*i*pi*t/base)
            out[,paste(cname,i,sep="")] <- cosfun(2*i*pi*t/base)
        }
    }
    return(out)
}

##' genIntegratedharmonic function
##'
##' A function to generate basis vectors for integrated Fourier series.\cr
##'
##' If the non-integrated Fourier series is:\cr
##' \cr
##' f(t) = sum_k a_k sin(2 pi k t / P) + b_k cos(2 pi k t / P)\cr
##' \cr
##' then\cr
##' int_t1^t2 f(s) ds = sum_k a_k (base/(2 pi k))*(cos(2 pi k t1 / P) - cos(2 pi k t2 / P)) + \cr
##'                     b_k (base/(2 pi k))*(sin(2 pi k t2 / P)-sin(2 pi k t1 / P))
##' \cr
##' where P is the funcamental period, or 'base', as referred to in the function arguments
##'
##' @param df a data frame containing a numeric time variable of interest
##' @param t1name a character string, the name of the variable in df containing the start time of the intervals
##' @param t2name a character string, the name of the variable in df containing the end time of the intervals
##' @param base the fundamental period of the signal, e.g. if it repeats over 24 hours and time is measured in hours, then put 'base = 24'; if the period is 24 hours but time is measured in days, then use 'base = 1/7' 
##' @param num number of sin and cosine terms to compute
##' @param sname character string, name for cosine terms in Fourier series (not integrated)
##' @param cname character string, name for sine terms in Fourier series (not integrated)
##' @param power legacy functionality, not used here
##' @return a data frame containing the start and end time vectors, together with the sin and cosine terms
##' @export

genIntegratedharmonic = function(df,t1name,t2name,base,num,sname="bcoef",cname="acoef",power=FALSE){

    # Series: sum_i{acoef_i*sin(2*pi*i*t/P + bcoef_i*cos(2*pi*i*t/P))}-


    # t = as.numeric(df[,tname])
    t1 = as.numeric(df[,t1name])
    t2 = as.numeric(df[,t2name])

    out <- data.frame(t1=t1,t2=t2)
    for(i in 1:num){
        if(power){
            stop("Can't use power=TRUE yet")
            # out[,paste(sname,i,sep="")] = sin(2*(2^(i-1))*pi*t/base)
            # out[,paste(cname,i,sep="")] = cos(2*(2^(i-1))*pi*t/base)
        }
        else{
            out[,paste(cname,i,sep="")] = (base/(2*pi*i))*(cos(2*i*pi*t1/base)-cos(2*i*pi*t2/base))
            out[,paste(sname,i,sep="")] = (base/(2*pi*i))*(sin(2*i*pi*t2/base)-sin(2*i*pi*t1/base))
        }
    }
    return(out)
}

