\name{latextable}
\alias{latextable}
\title{latextable function}
\usage{
  latextable(x, digits = 3, scientific = -3,
    colnames = NULL, rownames = NULL, caption = NULL,
    narep = " ", laststr = "", ...)
}
\arguments{
  \item{x}{a matrix, or object that can be coerced to a
  matrix. x can include mixed character and numeric
  entries.}

  \item{digits}{see help file for format}

  \item{scientific}{see help file for format}

  \item{colnames}{optional column names}

  \item{rownames}{optional row names}

  \item{caption}{optional caption, not normally used}

  \item{narep}{string giving replacement for NA entries in
  the matrix}

  \item{laststr}{string to write at end, eg note the double
  backslash!!}

  \item{...}{additional arguments passed to format}
}
\value{
  prints the LaTeX table to screen, so it can be copied
  into reports
}
\description{
  A very useful function to create a LaTeX table from a
  matrix. Rounds numeric entries and also replaces small
  numbers with standard index form equivalents.
}
\details{
  To get a backslash to appear, use a double backslash

  Just copy and paste the results into your LaTeX document.
}

