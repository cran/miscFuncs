\name{latextable}
\alias{latextable}
\title{latextable function...}
\usage{latextable(x, dp=3, colnames, rownames, caption, narep=" ", laststr="")}
\description{latextable function}
\details{A very useful function to create a LaTeX table from a matrix. Rounds numeric entries and also replaces
small numbers with standard index form equivalents.

To get a backslash to appear, use a double backslash

Just copy and paste the results into your LaTeX document.}
\value{prints the LaTeX table to screen, so it can be copied into reports}
\arguments{\item{x}{a matrix, can include mixed character and numeric entries}
\item{dp}{number of decimal places}
\item{colnames}{optional column names}
\item{rownames}{optional row names}
\item{caption}{optional caption, not normally used}
\item{narep}{string giving replacement for NA entries in the matrix}
\item{laststr}{string to write at end, eg note the double backslash!!}}
