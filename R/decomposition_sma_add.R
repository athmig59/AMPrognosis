#' @title Decomposition based on Symmetric Moving Average estimation of the Trend -- Additive Model
#'
#' @author Athanasios Migdalas, \email{athmig59@gmail.com}
#'
#' @description
#' The Symmetric Moving Average method is applied to estimate the time series trend
#' and subsequently, if seasonality is present, additive seasonal
#' components of the de-trended series
#' are computed and the de-trended series is de-seasonized.
#'
#' @details
#' Requires AMProgosis::seasonal_add(), AMPrognosis::sma()
#'
#' @seealso decomposition_lrtp_mult, decomposition_lrtp, decomposition_lrtp_add,
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
#' res<-decomposition_sma_add(dTest6, fp=4)
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
decomposition_sma_add <- function(d, fp=NaN){

  m   <- length(d)   # Series horizon
  dsd <- rep(NaN,m)  # De-seasoned & de-trended residual
  ses <- rep(NaN,m)  # Seasonality
  dd  <- rep(NaN,m)  # De-trended demand
  s   <- rep(NaN,m)  # Seasonal additive components

  df <- AMPrognosis::sma(d)  # Apply symmetric moving averages of order 3

  trv <- df$Trend   # Trend values

  dd <- d - trv     # De-trend

  if( !is.na(fp) ) {

    s <- AMPrognosis::seasonal_add(dd,fp)  # Seasonal components

    for( i in 1:fp) {
      dsd[i] <- dd[i] - s[i]    # Residuals
      ses[i] <- s[i]            # Seasonality
    }

    if( (fp+1) <= m ) {
      k <- 1
      for( i in (fp+1):m ){
        dsd[i] <- dd[i] - s[k]    # Residuals
        ses[i] <- s[k]            # Seasonality
        k <- k+1
        if( k > fp ) { k <- 1 }
      }
    }

  }

  df <- data.frame( Demand=d, DeTrended=dd, Residual=dsd,
                    Seasonality = ses, Trend = trv )

  return( list(Seasonal=s, Table=df) )

}
