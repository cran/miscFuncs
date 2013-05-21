\name{EKFadvance}
\alias{EKFadvance}
\title{EKFadvance function}
\usage{
  EKFadvance(obs, oldmean, oldvar, phi, phi.arglist, psi,
    psi.arglist, W, V, loglik = FALSE, na.rm = FALSE)
}
\arguments{
  \item{obs}{observations}

  \item{oldmean}{old mean}

  \item{oldvar}{old variance}

  \item{phi}{Function computing a Taylor Series
  approximation of the system equation. Can include higher
  (ie 2nd order and above) terms.}

  \item{phi.arglist}{arguments for function phi}

  \item{psi}{Function computing a Taylor Series
  approximation of the observation equation. Can include
  higher (ie 2nd order and above) terms.}

  \item{psi.arglist}{arguments for function psi}

  \item{W}{system noise matrix}

  \item{V}{observation noise matrix}

  \item{loglik}{whether or not to compute the
  pseudo-likelihood}

  \item{na.rm}{logical, whether or not to handle NAs.
  Defult is FALSE. Set to TRUE if there are any missing
  values in the observed data.}
}
\value{
  list containing the new mean and variance, and if
  specified, the likelihood
}
\description{
  A function to perform one iteration of ther EKF.
  Currently UNDER DEVELOPMENT.
}

