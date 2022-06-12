#' @title Double Exponential Smoothing - Holt's algorithm
#'
#' @author Athanasios Migdalas, \email{athmig59@gmail.com}
#'
#' @description 
#' Forecasting method of double exponential smoothing for time series model with additive trend and no seasonality.
#' 
#' @seealso sma, wma, naive, ses, ma, tes, ddes, dtes, lrtp
#' 
#' @param{d} {numerical vector containing observed demand}
#' @param{alpha}  {smoothing parameter for level update; the default value is 0.3}
#' @param{beta}   {smoothing parameter for trend update; the default value 0.2}
#' @param{a0}     {initialization level (previous forecast for first period); if it is not supplied, the first observed demand is selected}
#' @param{b0}     {initialization trend; if it is not supplied, the difference between the first two observed demands is selected}
#' @param{future} {number of future periods to forecast} 
#' 
#' @return A data frame consisting of three columns: 
#' \itemize{
#' \item\code{Demand} {it is the supplied \code{d}} 
#' \item\code{Forecast} {it contains the predicted values for each period, icluding \code{future} periods}
#' \item\code{Level} {it contains the level updates produced during the process}
#' \item\code{Trend} {it contains the trend updates produced during the process}
#' \item\code{Error} {the residual values expressing the difference between \code{Demand} and \code{Forecast}}
#' }
#' 
#' 
#' @references
#'  - Axsäter, S. (2015) Inventory Control, 3d Edition, Springer
#'  - Snyder, L. V. and Shen, Z.-J., M. (2011) Fundamentals of Supply Chain Theory, Wiley
#'  - Cowpertwait, P. S. and Metcalfe, A. V. (2009) Introductory Time Series with R, Use R!, Springer
#'  - Hyndman, R. J. and Athanasopoulos, G. (2021) Forecasting Principles and Practive, 3d Edition, OTexts
#'
#'
#'
#' @examples
#' ## Load test data, for example
#' data("dTest1")
#' ## Run with default parameter values
#' des(d=dTest1)
#' ## Run for 5 future periods
#' des(d=dTest1, future=5)
#' ## Run with other parameter values
#' des(d=dTest1, future=5, alpha=0.6, beta=0.5)
#' ## Run with non default initialization values for level and trend
#' des(d=dTest1, future=5, alpha=0.6, beta=0.5, a0=95, b0=85)
#' 
#' @export
des <- function(d, alpha=0.3, beta=0.2, future=1, a0=0, b0=NaN){
  m <- length(d)
  if(future >= 1){d <- c(d,rep(NaN, future))}
  l <- length(d)
  f <- rep(NaN, l)
  a <- rep(NaN, l)
  b <- rep(NaN, l)
  
  if(a0 == 0){a0 <- d[1]}
  #a[1] <- a0
  
  if(is.na(b0)){b0 <- d[2]-d[1]}
  #b[1] <- b0
  
  #f[1] <- d[1]
  
  f[1] <- a0 + b0
  a[1] <-  alpha * d[1] + (1-alpha) * ( a0 + b0)
  b[1] <- beta * ( a[1] - a0) + (1-beta) * b0
  f[2] <- a[1] + b[1]
  
  for (t in 2:m) {   
    a[t] <- alpha * d[t] + (1-alpha) * ( a[t-1] + b[t-1])
    b[t] <- beta * ( a[t] - a[t-1]) + (1-beta) * b[t-1]
    if(t+1 > l){break}
    f[t+1] <- a[t]+b[t]
  }
  
  if( (m+1) < (l-1)){
    for (t in (m+1):(l-1)){
      a[t] <- a[t-1]
      b[t] <- b[t-1] + b[m]
      f[t+1] <- a[t] + b[t]
    }
  }
  
  result <- data.frame(d,f, a, b, d-f)
  names(result) <-c("Demand","Forecast","Level", "Trend", "Error")
  return(result)
}
