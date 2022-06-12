#' @title Weighted Moving Average
#'
#' @author Athanasios Migdalas, \email{athmig59@gmail.com}
#'
#' @description 
#' Forecasting method of weighted moving average 
#' 
#' @seealso sma, ma, naive, ses, des, tes, ddes, dtes, lrtp
#' 
#' @param{d} {numerical vector containing observed demand}
#' @param{w}  {numerical vector of weights, which determines the order, i.e., number of periods to form the weighted average, the default value is c(1/3,1/3,1/3)}
#' @param{future} {number of future periods to forecast} 
#' 
#' @return A data frame consisting of three columns: 
#' \itemize{
#' \item\code{Demand} {it is the supplied \code{d}} 
#' \item\code{Forecast} {it contains the predicted values for each period, icluding \code{future} periods}
#' \item\code{Error} { its the residual values expressing the difference between \code{Demand} and \code{Forecast}}
#' }
#' It returns NULL if the supplied weights don't add to 1 or imply inappropriate order
#' 
#' 
#' @references
#'  - Axsäter, S. (2015) Inventory Control, 3d Edition, Springer
#'  - Snyder, L. V. and Shen, Z.-J., M. (2011) Fundamentals of Supply Chain Theory, Wiley
#'  - Cowpertwait, P. S. and Metcalfe, A. V. (2009) Introductory Time Series with R, Use R!, Springer
#'  - Hyndman, R. J. and Athanasopoulos, G. (2021) Forecasting Principles and Practive, 3d Edition, OTexts
#'
#'
#'@examples
#' ## Load test data, for example
#' data("dTest1")
#' ## Run with default parameter values
#' wma(d=dTest1)
#' ## Run for 5 future periods
#' wma(d=dTest1, future=5)
#' ## Run with moving average of order 5
#' wma(d=dTest1, w=c(1/5,1/5,1/5,1/5,1/5) )
#' ## Run with moving average of order 5 for 5 future periods
#' wma(d=dTest1, w=c(1/5,1/5,1/5,1/5,1/5), future=5)
#' 
#'
#' @export
wma <- function(d, w=c(1/3,1/3,1/3), future=1){
  if( sum(w) != 1) {return(NULL)}
  n <- length(w)
  m <- length(d)
  if( n > m) {return(NULL)}
  d <- c(d,rep(NaN, future))
  l <- length(d)
  f <- rep(NaN, l)
  
  for (t in (n+1):m) {
    f[t] <- weighted.mean( d[(t-n):(t-1)], w)
  }
  
  for (t in (m+1):l){
    f[t] <- weighted.mean(d[(m-n+1):m], w)
  }
  
  result <- data.frame(d,f, d-f)
  names(result) <-c("Demand","Forecast","Error")
  return(result)
}
