##' dplot function
##'
##' Generic function for model diagnostics.
##'
##' @param mod an object
##' @param ... additional arguments
##' @return method dplot
##' @export

dplot = function(mod,...){
    UseMethod("dplot")
}





##' dplot.lm function
##'
##' Function for producing diagnostic plots for linear models. Points are identified as being outliers, of high leverage and high
##' influence. The QQ plot has a confidence band. A plot of leverage vs fitted is given. The plot of Studentised residuals versus
##' leverage includes along with standard thresholds (at Cook's distance 0.5 and 1) an additional band highlighting influential
##' observations, whose Cook's distance exceed 8/(n-2p), where n is the number of observations and p is the number of parameters.
##' The respective threshold for outliers are set, by default, as those observations whose standardised residuals exceed 2.
##' Obervations are declared as having high leverage if their value exceeds 2p/n.
##'
##' @method dplot lm
##' @param mod an object of class 'lm' 
##' @param pch the type of point to use, passed to 'plot', the default being 19
##' @param outlier.threshold threshold on standardised residuals to declare an outlier, default is 2
##' @param leverage_threshold threshold on leverage to be classed as "high leverage", a function of (n,p), the default being 2p/n
##' @param influence_threshold threshold on influence to be classed as "high influence", a function (n,p), the default being 2p/n
##' @param ibands specifying thresholds at which to discplay Cook's distance on the Studentised residuals vs leverage plot. Default is at 0.5 and 1
##' @param ... additional arguments, not used as yet
##' @return ...
##' @export

dplot.lm <- function(mod,
                     pch=19,
                     outlier.threshold=2,
                     leverage_threshold=function(n,p){return(2*p/n)},
                     influence_threshold=function(n,p){return(8/(n-2*p))},
                     ibands=c(0.5,1),...){
    p = length(mod$coefficients)
    n = nrow(mod$model)
    
    f = fitted(mod)
    r = residuals(mod)
    s = summary(mod)$sigma # residual standard deviation

    infl = lm.influence(mod) # influence statistics for the model
    
    rS = r / (s*sqrt(1-infl$hat)) # Studentized residuals
    rs = (r - mean(r)) / sd(r) # Standardised residuals
    
    D = rS^2*infl$hat / (p*(1-infl$hat))

    mod$model$r_standardised = rs
    mod$model$r_Studentised = rS
    mod$model$leverage = infl$hat
    mod$model$cooks = D

    outliers = NULL
    leverage = NULL
    influence = NULL

    par(mfrow=c(2,2))

    # Residuals versus fitted
    plot(f,r,xlab="Fitted",ylab="Residuals",main="Residuals vs Fitted",pch=pch,col="blue")
    idx = which(abs(rs)>outlier.threshold)
    if(length(idx)>0){
        cat(length(idx)," potential outliers.\n")
        points(f[idx],r[idx],pch=pch,col="red",cex=1.1)
        del = diff(range(f)) / 20
        text(f[idx]+del,r[idx],labels=idx,cex=0.5)
        title(sub="Red: Outliers")
        outliers = mod$model[idx,]
    }
    lines(lowess(f,r),col=rgb(1,0,0,alpha=0.5),lwd=2)
    abline(h=0,lty="dashed")

    # QQ plot of residuals
    qqci(rs,main="QQ Standardised Residuals")

    # Leverage
    lthresh = leverage_threshold(n,p)

    plot(f,infl$hat,pch=pch, main="Leverage vs Fitted",xlab="Fitted",ylab="Leverage",col="blue")
    abline(h=lthresh,lty="dashed",lwd=0.5,col="red")

    idxl = which(infl$hat>lthresh)
    if(length(idxl)>0){
        cat(length(idxl)," potential points of high leverage.\n")
        points(f[idxl],infl$hat[idxl],pch=pch,col="red",cex=1.1)
        del = diff(range(f)) / 20
        text(f[idxl]+del,infl$hat[idxl],labels=idxl,cex=0.5)
        title(sub="Red: Potential High Leverage")
        leverage = mod$model[idxl,]
    }

    # Influence
    
    ithresh = influence_threshold(n,p)

    
    lseq = seq(min(infl$hat),max(infl$hat),length.out=100) # sequence of leverages
    
    bt = sqrt(ithresh*p*(1-lseq)/lseq)

    plot(infl$hat,rS,xlab="Leverage",ylab="Studentized Residuals",pch=pch,col="blue",main = "Influential Observations")
    

    for(i in 1:length(ibands)){
        b = sqrt(ibands[i]*p*(1-lseq)/lseq)
        lines(lseq,b,lty="dashed",lwd=0.5)
        lines(lseq,-b,lty="dashed",lwd=0.5)
    }
    lines(lseq,bt,lty="dashed",col="red")
    lines(lseq,-bt,lty="dashed",col="red")

    idxi = which(D>ithresh)
    if(length(idxi)>0){
        cat(length(idxi)," potential points of high influence.\n")
        points(infl$hat[idxi],rS[idxi],pch=pch,col="red",cex=1.1)
        del = diff(range(infl$hat)) / 20
        text(infl$hat[idxi] + del,rS[idxi],labels=idxi,cex=0.5)
        title(sub="Red: Potential High Influence")
        influence = mod$model[idxi,]
    }
  
    return(list(outlier.threshold = outlier.threshold,
                outlier.idx = idx,
                outliers = outliers,
                leverage.threshold = lthresh,
                high.leverage.idx = idxl,
                leverage = leverage,
                influence.threshold = ithresh,
                influence.idx = idxi,
                influence = influence))
}


##' vif function
##'
##' Function to calculate the variance inflation factor for each variable in a linear regression model.
##'
##' @param mod an object of class 'lm'
##' @return ...
##' @export

vif = function(mod){
    X = data.frame(model.matrix(formula(mod),model.frame(mod)))
    nms = names(X)
    VIF = c()
    Shapiro.Wilk = c()
    for(i in 1:length(nms)){
        fm = paste(nms[i]," ~ ",paste(nms[-i],collapse=" + "),collapse="")
        m = try(lm(fm,data=X))
        if(!inherits(m,"try-error")){
            VIF[i] = 1 / (1 - summary(m)$r.squared)

            sw = try(shapiro.test(residuals(m)))
            if(!inherits(sw,"try-error")){
                sw = sw$p.value
            }
            else{
                sw = NA
            }
            Shapiro.Wilk[i] = sw
            
        }
        else{
            VIF[i] = NA
        }
    }
    names(VIF) = nms
    return(rbind(VIF,Shapiro.Wilk))
}

