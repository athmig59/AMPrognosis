#' @title Additively "seasonize" a linear regression forecast
#' 
#' @description 
#' Uses linear regression to project the trend into the future and determines the
#' additive seasonal factors to seasonally adjust the regression forecast. The assumed time
#' series model is additive.
#'   
#' @details 
#' Requires AMPrognosis::lrtp(),and  AMPrognosis::seasonal_add()
#' 
#' 
#' @return A data frame consisting of the columns: 
#' \itemize{
#' \item\code{Demand} {it is the supplied \code{d}} 
#' \item\code{Forecast} {it contains the predicted values for each period, icluding \code{future} periods}
#' \item\code{Error} { its the residual values expressing the difference between \code{Demand} and \code{Forecast}}
#' \item\code{SeasonedForecast} {it contains the seasonally adjusted forecast}   
#' \item\code{SeasonedError} {the residual values expressing the difference between \code{Demand} and \code{SeasonedForecast}}
#' }
#' 
#' @seealso lrtp_season_mult
#' 
#' 
#' @references
#'  - Axsäter, S. (2015) Inventory Control, 3d Edition, Springer
#'  - Snyder, L. V. and Shen, Z.-J., M. (2011) Fundamentals of Supply Chain Theory, Wiley
#'  - Cowpertwait, P. S. and Metcalfe, A. V. (2009) Introductory Time Series with R, Use R!, Springer
#'  - Hyndman, R. J. and Athanasopoulos, G. (2021) Forecasting Principles and Practive, 3d Edition, OTexts
#'
#'
#' @examples
#' ## Load test data, for example
#' data("dTest2")
#' ## Run 
#' lrtp_seasonize_add(d=dTest2, fp=4, future=6)
#' 
#' 
#' @export
lrtp_seasonize_add <- function(d, fp=4, future=1){
  
  df <- AMPrognosis::lrtp(d, future)
  df <- df$Table
  
  sfactor <- AMPrognosis::seasonal_add(d,fp)
  
  m <- length(df$Forecast)
  sforecast <- rep(NaN,m)
  
  
  for(i in 1:fp){
    sforecast[i] <- df$Forecast[i] + sfactor[i]
  }
  
  if( (fp+1) <= m){
    k <- 1
    for(i in (fp+1):m){
      sforecast[i] <- df$Forecast[i] + sfactor[k]
      k <- k + 1
      if(k > fp){ k <- 1 }
    }
  } 
  
  df$SeasonedForecast <- sforecast
  df$SeasonedError <- df$Demand - sforecast
  return(df)
}
