#' @title Additively "re-seasonize" a demand time series (forecast)
#' 
#' @author Athanasios Migdalas, \email{athmig59@gmail.com}
#'  
#' 
#' @description 
#' Given the additive seasonal components of a de-seasoned time series (forecast), re-seasonize it
#' by adding them to the given series. Clearly, it is assumed that the time
#' series model is additive.
#' 
#' 
#' @param{forecast} {the demand series (forecast) to be re-seasonized}
#' @param{sfactor}  {the additive seasonal components}
#' @param{fp} {the seasonal frequency, for example 4 for quarters, 12 for months}
#' 
#' 
#' @return A data frame wit two columns:
#' \itemize{
#' \item\code{Forecast} {the given time series}
#' \item\code{SeasonedForecast} {the additively seasonally adjusted series}
#' }
#' 
#' @seealso  seasonize_mult
#' 
#' @examples
#' ## *** Example 1 ***
#' ##
#' ## Load test data, for example
#' ##
#' data("dTest2")
#' ## Run de-seasoning the series
#' ##
#' res1 <- deseasonize_add(d=dTest2, fp=4)
#' ##
#' ## Run a forecast method for time series with trend on the de-seasoned series
#' ##
#' res2 <- des(d=dTest2, future=5)
#' ##
#' ## "Re-seasonize" the forecast 
#' ##
#' sfactor <-res1$Seasonal
#' forecast <-res2$Forecast
#' df1 <- seasonize_add(forecast,sfactor,fp=4)
#' ##
#' ## and plot it vs demand
#' ##
#' plot_demand_vs_forecast(df=data.frame(Demand=forecast, Forecast=df1$SeasonedForecast))
#' ## 
#' 
#' @export
seasonize_add <- function(forecast, sfactor, fp=NAN){
  
  if(is.na(fp)) {return(NULL)}

  m <- length(forecast)
  
  if(m < fp){ return(NULL) }
  
  sforecast <- rep(NaN,m)

  for(i in 1:fp){
    sforecast[i] <- forecast[i] + sfactor[i]
  }

  if( (fp+1) <= m){
    k <- 1
    for(i in (fp+1):m){
      sforecast[i] <- forecast[i] + sfactor[k]
      k <- k + 1
      if(k > fp){ k <- 1 }
     }
   } 

   df <- data.frame(Forecast=forecast, SeasonedForecast=sforecast)
   return(df)
}