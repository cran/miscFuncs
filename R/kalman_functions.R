modelSetup <- function(data,fixed,varying,prior_mean,prior_variance,paramTransform=identity){

    retlist <- list()
    retlist$data <- data 
    retlist$paramTransform <- paramTransform
    retlist$fixed <- fixed 
    retlist$fixed_list <- setAttr(retlist$fixed)   
    retlist$varying <- varying
    retlist$varying_list <- setAttr(retlist$varying)
    retlist$prior_mean <- prior_mean
    retlist$prior_variance <- prior_variance
    return(retlist)
}

fixedMatrices <- function(A=NULL,B=NULL,C=NULL,D=NULL,E=NULL,F=NULL,V=NULL,W=NULL){
    retlist <- list()
    if(!is.null(A)){
        retlist$A <- A
    }
    if(!is.null(B)){
        retlist$B <- B
    }
    if(!is.null(C)){
        retlist$C <- C
    }
    if(!is.null(D)){
        retlist$D <-D 
    }
    if(!is.null(E)){
        retlist$E <- E
    }
    if(!is.null(F)){
        retlist$F <- F
    }
    if(!is.null(W)){
        retlist$W <- W
    }
    if(!is.null(V)){
        retlist$V <- V
    }
    return(retlist)
}

varyingMatrices <- function(A=NULL,B=NULL,C=NULL,D=NULL,E=NULL,F=NULL,V=NULL,W=NULL){
    retlist <- list()
    if(!is.null(A)){
        retlist$A <- A
    }
    if(!is.null(B)){
        retlist$B <- B
    }
    if(!is.null(C)){
        retlist$C <- C
    }
    if(!is.null(D)){
        retlist$D <-D 
    }
    if(!is.null(E)){
        retlist$E <- E
    }
    if(!is.null(F)){
        retlist$F <- F
    }
    if(!is.null(W)){
        retlist$W <- W
    }
    if(!is.null(V)){
        retlist$V <- V
    }
    return(retlist)
}

setAttr <- function(x){
    charvec <- c()
    if(!is.null(x$A)){
        charvec <- c(charvec,"A")
    }
    if(!is.null(x$B)){
        charvec <- c(charvec,"B")
    }
    if(!is.null(x$C)){
        charvec <- c(charvec,"C")
    }
    if(!is.null(x$D)){
        charvec <- c(charvec,"D")
    }
    if(!is.null(x$E)){
        charvec <- c(charvec,"E")
    }
    if(!is.null(x$F)){
        charvec <- c(charvec,"F")
    }
    if(!is.null(x$V)){
        charvec <- c(charvec,"w")
    }
    if(!is.null(x$V)){
        charvec <- c(charvec,"V")
    }  
    return(charvec)
}