\name{twotwoinfo}
\alias{twotwoinfo}
\title{twotwoinfo function}
\usage{
twotwoinfo(e1 = NA, u1 = NA, o1t = NA, e2 = NA, u2 = NA, o2t = NA,
  et = NA, ut = NA, T = NA, lev = 0.95, LaTeX = TRUE, digits = 3,
  scientific = -3, ...)
}
\arguments{
  \item{e1}{type print22() at the console}

  \item{u1}{type print22() at the console}

  \item{o1t}{type print22() at the console}

  \item{e2}{type print22() at the console}

  \item{u2}{type print22() at the console}

  \item{o2t}{type print22() at the console}

  \item{et}{type print22() at the console}

  \item{ut}{type print22() at the console}

  \item{T}{type print22() at the console}

  \item{lev}{significance level for confidence intervals.
  Default is 0.95}

  \item{LaTeX}{whether to print the 2 by 2 information as
  LaTeX text to the screen, including the table, odds
  ratio, relative risk and confidence intervals}

  \item{digits}{see ?format}

  \item{scientific}{see ?format}

  \item{...}{other arguments passed to function format}
}
\value{
Computes odds ratios and relative risks together with
confidence intervals for 2 by 2 table and prints to screen
in LaTeX format.
}
\description{
A function to compute and diplay information about 2 by 2
tables for copying into LaTeX documents. Computes odds
ratios and relative risks together with confidence
intervals for 2 by 2 table and prints to screen in LaTeX
format. The funciton will try to fill in any missing values
from the 2 by 2 table. Type print22() at the console to see
what each argument refers to.
}
\seealso{
\link{print22}
}

