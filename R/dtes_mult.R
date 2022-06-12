#' @title Dumped Triple Exponential (Mixed) Multiplicative Smoothing - Gardner-McKenzie-Holt-Winters algorithm
#'
#' @author Athanasios Migdalas, \email{athmig59@gmail.com}
#'
#' @description 
#' Forecasting method of triple exponential smoothing for time series model with 
#' additive trend and multiplicative seasonality. A Gardner-McKenzie dumping factor
#' is used to dump future trend
#' 
#' @details 
#' Requires AMPrognosis::seasonal_mult()
#' 
#' @seealso sma, wma, naive, ses, ma, tes, ddes, dtes, lrtp, tes_add, tes_mult, dtes_add
#' 
#' @param{d} {numerical vector containing observed demand}
#' @param{fp} {seasonal frequency, for instance, 4 for quarters}
#' @param{alpha}  {smoothing parameter for level update; the default value is 0.3}
#' @param{beta}   {smoothing parameter for trend update; the default value 0.2}
#' @param{gamma}  {smoothing parameter for seasonal update; the default value 0.1}
#' @param{phi}    {dumping factor; default value of 1 results in classic Holt-Winters algorithm}
#' @param{a0}     {initialization level (previous forecast for first period); if it is not supplied, the first observed demand is selected}
#' @param{b0}     {initialization trend; if it is not supplied, the difference between the first two observed demands is selected}
#'
#' @param{future} {number of future periods to forecast} 
#' 
#' @return A data frame consisting of three columns: 
#' \itemize{
#' \item\code{Demand} {it is the supplied \code{d}} 
#' \item\code{Forecast} {it contains the predicted values for each period, icluding \code{future} periods}
#' \item\code{Level} {it contains the level updates produced during the process}
#' \item\code{Trend} {it contains the trend updates produced during the process}
#' \item\code{Seasonal} {it contains the seasonal updates produced during the process}
#' \item\code{Error} {the residual values expressing the difference between \code{Demand} and \code{Forecast}}
#' }
#' A \code{NULL} is returned if \code{fp} is not supplied.
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
#' data("dTest6")
#' ## Run with default parameter values
#' dtes_mult(d=dTest6,fp=4)
#' ## Run for 5 future periods with dumping factor 0.8
#' dtes_mult(d=dTest6, fp=4, future=5, phi=0.8)
#' ## Run with other parameter values
#' dtes_mult(d=dTest6, fp=4, future=5, alpha=0.6, beta=0.5, gamma=0.2, phi=0.8)
#' ## Run with non default initialization values for level and trend
#' dtes_mult(d=dTest6, fp=4, alpha=0.6, beta=0.5, gamma=0.2, phi=0.8, future=5, a0=400, b0=110)
#' 
#' @export
dtes_mult <- function(d, fp=NaN, alpha=0.3, beta=0.2, gamma=0.1, phi=1, future=1, a0=NaN, b0=NaN){
  
  if(is.na(fp)){return(NULL)}
  m <- length(d)  
  if(m < fp){return(NULL)}
  
  s <- AMPrognosis::seasonal_mult(d,fp)
  
  if(future >= 1){d <- c(d,rep(NaN, future))}
  l <- length(d)
  f <- rep(NaN, l)
  a <- rep(NaN, l)
  b <- rep(NaN, l)
  
  if(future >= 1){s <- c(s, rep(NaN, future))}
  
  if(is.na(a0)){
    a[1] <- d[1]/s[1]
  }else{
    a[1] <- a0
  }
  
  
  if(is.na(b0)){
    b[1] <- d[2]/s[2]-d[1]/s[1]
  }else{
    b[1] <- b0
  }
  
  
  for(t in 2:fp){
    f[t] = ( a[t-1] + phi * b[t-1] ) * s[t]
    a[t] = alpha * d[t]/s[t] + ( 1 - alpha ) * ( a[t-1] + phi * b[t-1] )
    b[t] = beta * ( a[t] - a[t-1]) + ( 1- beta) * phi * b[t-1]
  }
  
  if((fp+1) <= m){  
    for (t in (fp+1):m) {
      f[t] = ( a[t-1] + phi * b[t-1]) * s[t-fp]
      a[t] = alpha * d[t]/s[t-fp] + ( 1 - alpha ) * ( a[t-1] + phi * b[t-1])
      b[t] = beta * ( a[t] - a[t-1] ) + ( 1 - beta ) * phi * b[t-1]
      s[t] = gamma * d[t]/a[t] + ( 1 - gamma ) * s[t-fp]
    }
  }
  
  if((m+1)<= l){
    for (t in (m+1):l){
      f[t] = ( a[t-1] + phi * b[t-1] ) * s[t-fp]
      a[t] = f[t] / s[t-fp]
      b[t] = phi * b[t-1]
      s[t] = s[t-fp]
    }
  }
  
  result <- data.frame(d,f, a, b, s, d-f)
  names(result) <-c("Demand","Forecast","Level", "Trend", "Seasonal", "Error")
  return(result)
}