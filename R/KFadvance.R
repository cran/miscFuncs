##' KFadvance function
##'
##' A function to compute one step of the Kalman filter. Embed in a loop to run the filter on a set of data.
##'
##' The model is: (note that Y and theta are COLUMN VECTORS)
##'
##' theta(t) = A*theta(t-1) + B + C*W (state equation)
##'
##' Y(t) = D*theta(t) + E + F*V         (observation equation)
##'
##' W and V are the covariance matrices of the state and observation noise. Prior is normal, 
##'
##' N(mu(t-1),Sigma(t-1))
##'
##' Result is the posterior, N(mu(t),Sigma(t)), together with the likelihood contribution Prob(Y(t)|Y(t-1))
##'
##' @param obs Y(t)
##' @param oldmean mu(t-1) 
##' @param oldvar Sigma(t-1)
##' @param A matrix A
##' @param B column vector B
##' @param C matrix C
##' @param D matrix D
##' @param E column vector E
##' @param F matrix F
##' @param W state noise covariance
##' @param V observation noise covariance
##' @param marglik logical, whether to return the marginal likelihood contribution from this observation
##' @param log whether or not to return the log of the likelihood contribution.
##' @param na.rm na.rm logical, whether or not to handle NAs. Defult is FALSE. Set to TRUE if there are any missing values in the observed data.
##' @return list containing the new mean and variance, and if specified, the likelihood
##' @export
KFadvance <- function(obs,oldmean,oldvar,A,B,C,D,E,F,W,V,marglik=FALSE,log=TRUE,na.rm=FALSE){
    if(na.rm){
        if(any(is.na(obs))){
            if(all(is.na(obs))){
                if(log){
                    return(list(mean=A%*%oldmean + B,var=A%*%oldvar%*%t(A) + C%*%W%*%t(C),mlik=0))
                }
                else{
                    return(list(mean=A%*%oldmean + B,var=A%*%oldvar%*%t(A) + C%*%W%*%t(C),mlik=1))
                }
            }
            else{
                M <- diag(length(obs))
                M <- M[-which(is.na(obs)),]
                obs <- obs[which(!is.na(obs))]
                D <- M%*%D
                E <- M%*%E
                F <- M%*%F
            }
        }
    }
	T <- A %*% oldmean + B
	S <- A %*% oldvar %*% t(A) + C %*% W %*% t(C)
    thing1 <- D %*% S
    tD <- t(D)
	K <- thing1 %*% tD + F %*% V %*% t(F)
    
    margmean <- D %*% T + E
    resid <- obs-margmean
	
    if (marglik==TRUE){		
		if (all(dim(K)==1)){ 
            thing2 <- S %*% tD           
			newmean <- T + as.numeric(1/K)* thing2 %*% resid
			newvar <- S - as.numeric(1/K)*thing2 %*% thing1
			marginal <- dnorm(obs,as.numeric(margmean),sqrt(as.numeric(K)),log=log)
		}
		else{
			Kinv <- solve(K)
            thing3 <- tD %*% Kinv
            thing4 <- S %*% thing3
			newmean <- T + thing4 %*% resid
			newvar <- S - thing4 %*% thing1
            marginal <- dmvnorm(as.vector(obs),as.vector(margmean),K,log=log)
		}
		return(list(mean=newmean,var=newvar,mlik=marginal))
	}
	else{
		if (all(dim(K)==1)){
            thing2 <- S %*% tD           
			newmean <- T + as.numeric(1/K) * thing2 %*% resid
			newvar <- S - as.numeric(1/K) * thing2 %*% thing1
		}
		else{
			Kinv <- solve(K)
            thing3 <- tD %*% Kinv
            thing4 <- S %*% thing3
			newmean <- T + thing4 %*% resid
			newvar <- S - thing4 %*% thing1
		}
		return(list(mean=newmean,var=newvar))
	}
}
