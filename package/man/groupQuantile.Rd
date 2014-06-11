\name{groupQuantile}
\alias{groupQuantile}
\title{Compute Exact Quantiles by Group}
\usage{
groupQuantile(x, by, var = "Freq")
}
\arguments{
  \item{x}{data frame}

  \item{by}{name of the column of \code{x} that contains
  the grouping variable}

  \item{var}{name of the column that contains the variable
  to compute quantiles of}
}
\value{
data frame with additional \code{p} corresponding to the
within-group percentile for the record
}
\description{
Compute exact quantiles of data for each group specifyed by
"by"
}
\author{
Ryan Hafen
}

