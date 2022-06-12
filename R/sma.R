#' @title Symmetric Moving Average
#'
#' @author Athanasios Migdalas, \email{athmig59@gmail.com}
#'
#' @description 
#' The method of symmetric moving average to estimate the time series trend. 
#' 
#' @seealso ma, wma, naive, ses, des, tes, ddes, dtes, lrtp
#' 
#' @param{d} {numerical vector containing observed demand}
#' @param{n}  {order, i.e., number of periods to form the average, the default value is 3}
#' 
#' @return A data frame consisting of three columns: 
#' \itemize{
#' \item\code{Demand} {it is the supplied \code{d}} 
#' \item\code{Trend} {it contains the predicted values for each period}
#' }
#' It returns NULL if the supplied order is incosistent with the length of d
#' 
#' @details 
#' Note that the function does not produce  future forecast and that 
#' it does not determine the error (residual) \code{Demand - Trend} either, 
#' but this is easily
#' produced from the returned result, for instance,  as follows:
#' 
#' \code{df <- sma(d=dTest1)}
#' 
#' \code{error <- df$Demand - df$Trend}
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
#' sma(d=dTest1)
#' ## Run with symmetric moving average of order 5
#' sma(d=dTest1, n=5)
#' 
#' 
#' @export
sma <- function(d,n=3){
  
  if( n <= 1 ) { return(NULL) }
  m <- length(d)
  if( n > m ) { return(NULL) }
  if( ( 0.5 * n - floor( 0.5 * n ) ) == 0 ) { return(NULL) }
  
  f <- rep(NaN, m)
  wa <- 1
  wb <- n
  wm <- 0.5 * ( wa + wb )
  
  while(TRUE){
    f[wm] <- mean( d[wa:wb])
    wb <- wb + 1
    if( wb > m ){ break }
    wa <- wa + 1
    wm <- 0.5 * ( wa + wb )
  }
  
  df <- data.frame(Demand=d,Trend=f)
  
  return(df)
}

