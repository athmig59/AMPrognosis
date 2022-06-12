#' @title Linear regression - Trend projection
#'
#' @author Athanasios Migdalas, \email{athmig59@gmail.com}
#'
#' @description 
#' Linear regression method to estimate the time series trend 
#' and project it into the future to perform forecasting
#' 
#' @seealso sma, wma, naive, ses, des, tes, ddes, dtes, ma
#' 
#' @param{d} {numerical vector containing observed demand}
#' @param{future} {number of future periods to forecast, default 1 period} 
#' 
#' @return A list containing three elements:
#' \itemize{
#' \item\code{Line} {a list containing the regression line coefficients; intercept, \code{a}, and slope, and \code{b}}
#' \item\code{Statistics} {a list containing the correlation measurements; correlation coefficent, \code{r}, and \code{r2}}
#' \item\code{Table} {a data frame consisting of three columns: 
#' \itemize{
#' \item\code{Demand} {it is the supplied \code{d}} 
#' \item\code{Forecast} {it contains the predicted values for each period, icluding \code{future} periods}
#' \item\code{Error} { its the residual values expressing the difference between \code{Demand} and \code{Forecast}}
#' }
#' }
#' }
#'
#'
#'@examples
#' ## Load test data, for example
#' data("dTest1")
#' ## Run with default parameter values
#' lrtp(d=dTest1)
#' ## Run for 5 future periods
#' lrtp(d=dTest1, future=5)
#'
#'
#'@export
lrtp <- function(d, future=1){
  m <- length(d)
  if(future >= 1) {d <- c(d,rep(NaN, future))}
  l <- length(d)
  f <- rep(NaN, l)
  
  tm <- mean(1:m)
  dm <- mean(d[1:m])
  ts <- sum( (1:m)**2)
  td <- sum( (1:m)*d[1:m])
  
  b = ( td - m * tm * dm ) / ( ts - m * tm **2)
  a = dm - b * tm
  
  st <- sum(1:m)
  ds <- sum( d[1:m]**2 )
  sd <- sum( d[1:m] )
  
  r <- ( m * td - st * sd) / sqrt( (m*ts - st**2 ) * ( m*ds - sd**2 ) )
  r2 <- r**2
  
  if(l >= 1){
    for(i in 1:l){
      f[i] = a + b * i
    }
  }
  
  line = list(a=a,b=b)
  statistics = list(R=r, R2=r2)
  result <- list( Line= line, Statistics=statistics, Table=data.frame(Demand = d, Forecast = f, Error = d-f))
  
  return(result)
  
}