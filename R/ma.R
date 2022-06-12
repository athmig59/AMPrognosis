#' @title Moving Average
#'
#' @author Athanasios Migdalas, \email{athmig59@gmail.com}
#'
#' @description 
#' Forecasting method of moving average 
#' 
#' @seealso sma, wma, naive, ses, des, tes, ddes, dtes, lrtp
#' 
#' @param{d} {numerical vector containing observed demand}
#' @param{n}  {order, i.e., number of periods to form the average, the default value is 3}
#' @param{future} {number of future periods to forecast} 
#' 
#' @return A data frame consisting of three columns: 
#' \itemize{
#' \item\code{Demand} {it is the supplied \code{d}} 
#' \item\code{Forecast} {it contains the predicted values for each period, icluding \code{future} periods}
#' \item\code{Error} { its the residual values expressing the difference between \code{Demand} and \code{Forecast}}
#' }
#' It returns NULL if the supplied order is incosistent with the length of d
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
#' ma(d=dTest1)
#' ## Run for 5 future periods
#' ma(d=dTest1, future=5)
#' ## Run with moving average of order 5
#' ma(d=dTest1, n=5)
#' ## Run with moving average of order 5 for 5 future periods
#' ma(d=dTest1, n=5, future=5)
#' 
#' 
#' @export
ma <- function(d, n=3, future=1){
  m <- length(d)
  if( (n+1) > m ) { return(NULL) }
  
  if(future >= 1) {d <- c(d,rep(NaN, future))}
  l <- length(d)
  f <- rep(NaN, l)
  
  for (t in (n+1):m) {
    f[t] <- mean( d[(t-n):(t-1)])
  }
  
  if( (m+1) <= l){
    for (t in (m+1):l){
      f[t] <- mean(d[(m-n+1):m])
    }
  }
  
  result <- data.frame(d,f, d-f)
  names(result) <-c("Demand","Forecast","Error")
  return(result)
}

