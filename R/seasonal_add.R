#' @title Additive Seasonality
#'
#' @author Athanasios Migdalas, \email{athmig59@gmail.com}
#'
#' @description 
#' Computes the additive seasonal component of a time series
#' 
#' @seealso seasonal_mult
#' 
#' @param{d} {numerical vector containing observed demand}
#' @param{pf} {the seasonal periodicity (frequency), e.g., 4 for the quarters, 12 for the months of a year}
#' 
#' @return  A numerical vector containing the additive seasonal components
#' 
#' 
#' @references
#'  - Axsäter, S. (2015) Inventory Control, 3d Edition, Springer
#'  - Snyder, L. V. and Shen, Z.-J., M. (2011) Fundamentals of Supply Chain Theory, Wiley
#'  - Cowpertwait, P. S. and Metcalfe, A. V. (2009) Introductory Time Series with R, Use R!, Springer
#'  - Hyndman, R. J. and Athanasopoulos, G. (2021) Forecasting Principles and Practive, 3d Edition, OTexts
#'
#' @examples
#' ## Load test data, for example
#' data("dTest3")
#' ## Run 
#' seasonal_add(d=dTest3, fp=4)
#' 
#' @export
seasonal_add <- function(d,fp){
  m <- length(d)
  s <- rep(NaN,fp)
  
  for(i in 1:fp){
    s[i] <- mean( d[seq(i,m,fp)], na.rm = TRUE)
  }
  s <- s - mean(s)
  return(s)
}
