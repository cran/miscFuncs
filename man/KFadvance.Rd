\name{KFadvance}
\alias{KFadvance}
\title{KFadvance function...}
\usage{KFadvance(obs, oldmean, oldvar, A, B, C, D, E, F, W, V, marglik=FALSE,
    log=TRUE)}
\description{KFadvance function}
\details{A function to compute one step of the Kalman filter. Embed in a loop to run the filter on a set of data.

The model is: (note that Y and theta are COLUMN VECTORS)

theta_t = A%*%theta_{t-1} + B + C%*%W (state equation)

Y_t = D%*%theta_t + E + F%*%V         (observation equation)

W and V are the covariance matrices of the state and observation noise. Prior is normal, 

N(mu_{t-1},Sigma_{t-1})

Result is the posterior, N(mu_t,Sigma_t), together with the likelihood contribution Prob(Y_t|Y_{t-1})}
\value{list containing the new mean and variance, and if specified, the likelihood}
\arguments{\item{obs}{Y_t}
\item{oldmean}{mu_{t-1}}
\item{oldvar}{Sigma_{t-1}}
\item{A}{matrix A}
\item{B}{column vector B}
\item{C}{matrix C}
\item{D}{matrix D}
\item{E}{column vector E}
\item{F}{matrix F}
\item{W}{state noise covariance}
\item{V}{observation noise covariance}
\item{marglik}{logical, whether to return the marginal likelihood contribution from this observation}
\item{log}{whether or not to return the log of the likelihood contribution.}}
