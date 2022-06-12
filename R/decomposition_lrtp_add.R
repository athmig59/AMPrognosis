#' @title Time Series ecomposition based on Regression Trend Projection -- Additive Model
#'
#' @author Athanasios Migdalas, \email{athmig59@gmail.com}
#'
#' @description
#' Linear regression method is applied to estimate the time series trend
#' and subsequently, if seasonality is present, additive seasonal
#' components of the de-trended series
#' are computed and the de-trended series is de-seasonized.
#'
#' @details
#' Requires AMProgosis::seasonal_add(), AMPrognosis::lrtp()
#'
#' @seealso decomposition_lrtp_mult, decomposition_lrtp, decomposition_sma_add,
#' decomposition_sma_mult, decomposition_sma, plot_decomposition
#'
#'
#' @param{d} {a vector containing observed demand}
#' @param{fp} {seasonal period frequency, if any}
#'
#' @return A list containing:
#' \itemize{
#' \item\code{Seasonal}    {Seasonal additive components}
#' \item\code{TrendLine}   {Regression coefficients for the trend equation}
#' \item\code{Table} {data frame with columns:
#' \itemize{
#' \item\code{Demand} {the given demand}
#' \item\code{DeSeasoned} {de-seasoned demand}
#' \item\code{Residual} {de-seasoned and de-trended remains of demand}
#' \item\code{Seasonality} {the seasonal components spread over the entire horizon}
#' \item\code{Trend} {estimated trend of the demand}
#'  }}
#'}
#'
#' @examples
#' ## Load test data, for instance
#' ##
#' data("dTest6")
#' ##
#' ## Apply additive decomposition based on linear regression
#' ##
#' res<-decomposition_lrtp_add(dTest6, fp=4)
#' ##
#' ## Plot the time series trend
#' ##
#' plot_demand_vs_forecast(data.frame(Demand=res$Table$Demand,Trend=res$Table$Trend))
#' ##
#' ## Plot the decomposition components
#' ##
#' plot_decomposition(res$Table)
#'
#' @export
decomposition_lrtp_add <- function(d,fp=NaN){

  m <- length(d)
  dsd <- rep(NaN,m)  # de-seasoned  and de-trended residual
  ses <- rep(NaN,m)  # seasonality
  dd  <- rep(NaN,m)  # de-trended demand
  s   <- rep('NaN',1) # seasonal additive components

  df  <- AMPrognosis::lrtp(d, future=0) # apply regression

  trl <- df$Line           # equation for trend line
  trv <- df$Table
  trv <- trv['Forecast']   # trend values
  names(trv) <- 'Trend'

  dd <- d - trv$Trend  # de-trend

  if( !is.na(fp) ){

    s <- AMPrognosis::seasonal_add(dd,fp)  # seasonal components

    for(i in 1:fp){
      dsd[i] <- dd[i] - s[i]   # residuals
      ses[i] <- s[i]           # seasonality
    }

    if( (fp+1) <= m){
      k <- 1
      for(i in (fp+1):m){
        dsd[i] <- dd[i] - s[k]  # residuals
        ses[i] <- s[k]          # seasonality
        k <- k + 1
        if(k > fp){ k <- 1 }
      }
    }
  }

  names(dsd) <- 'Residual'

  df <- data.frame(Demand=d, DeTrended=dd, Residual=dsd,
                   Seasonality=ses,  Trend=trv)

  return( list(Seasonal=s, TrendLine=trl, Table=df) )

}
#' }
#'
