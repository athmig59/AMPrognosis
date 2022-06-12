#' @title Simple Exponential Smoothing
#'
#' @author Athanasios Migdalas, \email{athmig59@gmail.com}
#'
#' @description 
#' Forecasting method of simple exponential smoothing
#' 
#' @seealso sma, wma, naive, ma, des, tes, ddes, dtes, lrtp
#' 
#' @param{d} {numerical vector containing observed demand}
#' @param{alpha}  {smoothing parameter between 0 and 1, default value is 0.3}
#' @param{f0} {initialization forecast (previous forecast for first period); if not supplied, the oldest observed demand is selected}
#' @param{future} {number of future periods to forecast} 
#' 
#' @return A data frame consisting of three columns: 
#' \itemize{
#' \item\code{Demand} {it is the supplied \code{d}} 
#' \item\code{Forecast} {it contains the predicted values for each period, icluding \code{future} periods}
#' \item\code{Error} {the residual values expressing the difference between \code{Demand} and \code{Forecast}}
#' }
#' It returns NULL if the supplied alpha is not between 0 and 1
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
#' ses(d=dTest1)
#' ## Run for 5 future periods
#' ses(d=dTest1, future=5)
#' ## Run with smoothing parameter 0.5
#' ses(d=dTest1, alpha=0.5)
#' ## Run with smoothing parameter 0.5 for 5 future periods
#' ses(d=dTest1, alpha=0.5, future=5)
#' ## Run with initialization forecast
#' ses(d=dTest1, alpha=0.5, future=5, f0=76)
#' 
#' 
#'@export
ses <- function(d, alpha=0.3, future=1, f0=0){
  if(alpha < 0) { return(NULL) }
  if(alpha > 1) { return(NULL) }
  
  m <- length(d)
  if(future >= 1){d <- c(d,rep(NaN, future))}
  l <- length(d)
  f <- rep(NaN, l)
  
  if(f0 == 0){f0 <- d[1]}
  f[1] <- f0
  
  for (t in 2:m) {
    f[t] <- alpha*d[t-1]+(1-alpha)*f[t-1]
  }
  
  if( (m+1) <= l){
    for (t in (m+1):l){
      f[t] <- f[t-1]
    }
  }
  
  result <- data.frame(d,f, d-f)
  names(result) <-c("Demand","Forecast","Error")
  return(result)
}
