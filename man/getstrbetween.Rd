\name{getstrbetween}
\alias{getstrbetween}
\title{getstrbetween function}
\usage{
getstrbetween(linedata, start, startmark, endmark, include = FALSE)
}
\arguments{
  \item{linedata}{a string}

  \item{start}{integer, where to start looking in linedata}

  \item{startmark}{character string. a pattern identifying
  the start mark}

  \item{endmark}{character string. a pattern identifying
  the end mark}

  \item{include}{include the start and end marks?}
}
\value{
the first string after start and between the start and end
marks
}
\description{
A function used in web scraping. Used to simplify the
searching of HTML strings for information.
}

