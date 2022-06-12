#' @title Multiplicatively "de-seasonize" a demand time series (forecast)
#' 
#' @author Athanasios Migdalas, \email{athmig59@gmail.com}
#' 
#' 
#' @description 
#' Computes the multiplicative seasonal factors of a time series (forecast) and de-seasonize it
#' by dividing the given series by them. Clearly, it is assumed that the time
#' series model is multiplicative.
#'   
#' @details 
#' Requires AMPrognosis::seasonal_mult()
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
#' @seealso deseasonize_add
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
#' res1 <- deseasonize_mult(d=dTest2, fp=4)
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
#' df1 <- seasonize_mult(forecast,sfactor,fp=4)
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
#' res <- deseasonize_mult(d=res0$Forecast, fp=4)
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
#' 
#'
deseasonize_mult <- function(d,fp=NaN){
  
  library(ggplot2)
  
  m <- length(d)
  dsd <- rep(NaN,m)  # de-seasoned demand
  ses <- rep(NaN,m)  # seasonality
  s   <- rep('NaN',1) # seasonal factors
  
  if( is.na(fp) ){ return(NULL) }
  
  s <- seasonal_mult(d,fp) # seasonal indices
  
  for(i in 1:fp){
    dsd[i] <- d[i] / s[i]
    ses[i] <- s[i]
  }
  
  if( (fp+1) <= m){
    k <- 1
    for(i in (fp+1):m){
      dsd[i] <- d[i] / s[k]
      ses[i] <- s[k]
      k <- k + 1
      if(k > fp){ k <- 1 }
    }
  }
  
  df <- data.frame(Demand=d, DeSeasoned=dsd, Seasonality=ses)
  
  g1 <- ggplot(df)+
    geom_line(aes(x=1:m, y=Demand, color='Demand'))+
    geom_line(aes(x=1:m, y=Seasonality, color='Seasonality'))+
    ggtitle('Multiplicative seasonality')+
    xlab('Time periods') + ylab('Seasonality / Trend')
  
  g2 <- ggplot(df)+
    geom_line(aes(x=1:m, y=Demand, color='Demand'))+
    geom_line(aes(x=1:m, y=DeSeasoned, color='DeSeasoned'))+
    ggtitle('Multiplicative deseasoning')+
    xlab('Time periods')
  
  return( list(Seasonal=s, Table=df, Fig1=g1, Fig2=g2) )
  
}
