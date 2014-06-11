#' Compute Exact Quantiles by Group
#' 
#' Compute exact quantiles of data for each group specifyed by "by"
#' 
#' @param x data frame
#' @param by name of the column of \code{x} that contains the grouping variable
#' @param var name of the column that contains the variable to compute quantiles of
#' 
#' @return data frame with additional \code{p} corresponding to the within-group percentile for the record
#' 
#' @author Ryan Hafen
#' 
#' @export
groupQuantile <- function(x, by, var = "Freq") {
   require(plyr)
   ddply(x, by, function(a) {
      a <- a[order(a[[var]]),]
      data.frame(a, p = ppoints(nrow(a)))
   })
}

