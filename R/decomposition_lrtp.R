#' @title Common Interface for Time Series Decomposition based on Regression Trend Projection
#'
#' @author Athanasios Migdalas, \email{athmig59@gmail.com}
#'
#' @description
#' Linear regression method is applied to estimate the time series trend
#' and subsequently, if seasonality is present, additive or multiplicative seasonal
#' components of the de-trended series
#' are computed and the de-trended series is de-seasonized.
#'
#' @details
#' Requires AMProgosis::decomposition_lrtp_add(), and AMPrognosis::decomposition_lrtp_mult()
#'
#' @param{d} {a vector containing observed demand}
#' @param{fp} {seasonal period frequency, if any}
#' @param{model} {choice of \code{"additive"} or \code{"multiplicative"}}
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
#'
#' @seealso decomposition_lrtp_mult, decomposition_lrtp_add,
#' decomposition_sma_mult, decomposition_sma, plot_decomposition
#'
#'
#' @examples
#' ## Load test data, for instance
#' ##
#' data("dTest6")
#' ##
#' ## Apply additive decomposition based on linear regression (default)
#' ##
#' res<-decomposition_lrtp(dTest6, fp=4)
#' ##
#' ## Plot the decomposition components
#' ##
#' plot_decomposition(res$Table)
#' ##
#' ## Apply multiplicative decomposition based on linear regression
#' ##
#' res<-decomposition_lrtp(dTest6, fp=4, model="multiplicative")
#' ##
#' ## Plot the decomposition components
#' ##
#' plot_decomposition(res$Table)
#'
#' @export
decomposition_lrtp <-function(d, fp=NaN, model=c("additive","multiplicative") ){
  if(model == "additive"){
    AMPrognosis::decomposition_lrtp_add(d=d, fp=fp)
  }else if(model == "multiplicative"){
    AMPrognosis::decomposition_lrtp_mult(d=d, fp=fp)
  }else{
    message("*** Error: decomposition_lrtp does not provide for such a model")
    return(NULL)
      }
}
