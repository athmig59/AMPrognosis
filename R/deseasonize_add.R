#' @title Additively "de-seasonize" a demand time series (forecast)
#' 
#' @author Athanasios Migdalas, \email{athmig59@gmail.com}
#' 
#' @description 
#' Computes the additive seasonal components of a time series (forecast) and de-seasonize it
#' by subtracting them from the given series. Clearly, it is assumed that the time
#' series model is additive.
#'   
#' @details 
#' Requires AMPrognosis::seasonal_add()
#' 
#' 
#' @return  A list contaíning
#' \itemize{
#' \item\code{Seasonal} {A vector that contains the seasonal components}
#' \item\code{Table} {A data frame consisting of the columns: 
#' \itemize{
#' \item\code{Demand} {it is the supplied \code{d}} 
#' \item\code{Deseasoned} {it contains the deseasoned values of \code{d}}
#' \item\code{Seasonality} {it containes the seasonal component to the corresponding term of \code{d}}
#' }}
#' \item\code{Fig1} {Contains a plot of time series versus seasonality}
#' \item\code{Fig2} {Contains a plot of the given time series vs the deseason values}
#' }
#' 
#' @seealso deseasonize_mult
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
#' ## *** Example 1 ***
#' ##
#' ## Load test data, for example
#' ##
#' data("dTest2")
#' ##
#' ## Run de-seasoning the series
#' ##
#' res1 <- deseasonize_add(d=dTest2, fp=4)
#' ##
#' ## Plot the series vs its seasonality
#' ##
#' print(res1$Fig1)
#' ##
#' ## Plot the series vs its deseasoned counter part
#' ##
#' print(res1$Fig2)
#' ##
#' ## Run a forecast method for time series with trend on the de-seasoned series
#' ##
#' res2 <- des(d=dTest2, future=5)
#' ##
#' ## Plot demand vs trend
#' ##
#' plot_demand_vs_forecast(df=res2)
#' ##
#' ## Plot demand vs forecast
#' ##
#' plot_demand_vs_forecast(df=data.frame(Demand=res2$Demand, Forecast=res2$Forecast))
#' ##
#' ## "Re-seasonize" forecast and plot it vs demand
#' ##
#' sfactor <-res1$Seasonal
#' forecast <-res2$Forecast
#' df1 <- seasonize_add(forecast,sfactor,fp=4)
#' plot_demand_vs_forecast(df=data.frame(Demand=forecast, Forecast=df1$SeasonedForecast)
#' ## 
#' ## Compare with the next Example!!
#' ##
#' 
#' ## *** Example 2 (continues from the previous) ***
#' ##
#' ## Run a forecast method for time series with trend
#' ##
#' res0 <- des(d=dTest2, future=5)
#' ##
#' ## Run de-seasoning the forecast
#' ##
#' res <- deseasonize_add(d=res0$Forecast, fp=4)
#' ##
#' ## Plot the forecasted series vs its seasonality
#' ##
#' print(res$Fig1)
#' ##
#' ## Plot the forecasted series vs its deseasoned counter part
#' ##
#' print(res$Fig2)
#' 
#' @param{d} {numerical vector of demand series (forecast)}
#' @param{fp} {seasonal frequency, for example 4 for quarters, 12 for months}
#' 
#' @export
deseasonize_add <- function(d,fp=NaN){
  
  if( is.na(fp) ){ return(NULL) }
  
  m <- length(d)
  
  if(m < fp){ return(NULL) }
  
  dsd <- rep(NaN,m)  # de-seasoned demand
  ses <- rep(NaN,m)  # seasonality
  s   <- rep('NaN',1) # seasonal additive components
  
  
  
  s <- AMPrognosis::seasonal_add(d,fp) # seasonal factors
  
  for(i in 1:fp){
    dsd[i] <- d[i] - s[i]
    ses[i] <- s[i]
  }
  
  if( (fp+1) <= m){
    k <- 1
    for(i in (fp+1):m){
      dsd[i] <- d[i] - s[k]
      ses[i] <- s[k]
      k <- k + 1
      if(k > fp){ k <- 1 }
    }
  }
  
  df <- data.frame(Demand=d, DeSeasoned=dsd, Seasonality=ses)
  
  g1 <- ggplot2::ggplot(df)+
    geom_line(aes(x=1:m, y=Demand, color='Demand'))+
    geom_line(aes(x=1:m, y=Seasonality, color='Seasonality'))+
    ggtitle('Additive seasonality')+
    xlab('Time periods') + ylab('Seasonality / Demand')
  
  g2 <- ggplot2::ggplot(df)+
    geom_line(aes(x=1:m, y=Demand, color='Demand'))+
    geom_line(aes(x=1:m, y=DeSeasoned, color='DeSeasoned'))+
    xlab('Time periods') + ylab('Demand / Deseasoned')+
    ggtitle('Additive deseasoning')
  
  return( list(Seasonal=s, Table=df, Fig1=g1, Fig2=g2) )
  
}
