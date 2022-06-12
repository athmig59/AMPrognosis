#' @title Accuracy and Bias Indicators
#'
#' @author Athanasios Migdalas, \email{athmig59@gmail.com}
#'
#' @description
#' Measures the Forecast Error and provides Accuracy and Bias Indicators
#'
#'
#' @references
#'  - Axsäter, S. (2015) Inventory Control, 3d Edition, Springer
#'  - Snyder, L. V. and Shen, Z.-J., M. (2011) Fundamentals of Supply Chain Theory, Wiley
#'  - Cowpertwait, P. S. and Metcalfe, A. V. (2009) Introductory Time Series with R, Use R!, Springer
#'  - Hyndman, R. J. and Athanasopoulos, G. (2021) Forecasting Principles and Practive, 3d Edition, OTexts
#'
#' @param{df} {data frame containing, at least,  a column \code{Error} or \code{Residual}, and a
#'            column \code{Demand}}
#'
#' @return A list containing elements for the following indicators:
#' \itemize{
#' \item\code{Av. Error} {average error; a bias indicator}
#' \item\code{Scaled Av. Err.}  {scaled average error; bias expressed in percentage}
#' \item\code{MAD} {mean absolute deviation / error}
#' \item\code{Scaled MAD} {scaled mean absolute error}
#' \item\code{MAPE} {mean absolute percentage error}
#' \item\code{MSE} {mean square error}
#' \item\code{Scaled MSE} {scaled mean square error}
#' \item\code{RMSE} {root mean square error}
#' \item{Scaled RMSE} {scaled root mean square error}
#' }
#' It returns \code{NULL} if input does not match requirements
#'
#' @examples
#'  ## Forecast
#'  ##
#'  r0 <- des(dTest2, future=0)
#'  ##
#'  ## Compute accuracy and bias indicators
#'  ##
#'  accuracy_and_bias(r0)
#'
#'@export
accuracy_and_bias <- function(df){

  if(!("Demand" %in% names(df))){ return(NULL) }

  if(!("Error" %in% names(df))){
    if(!("Residual" %in% names(df))){
      return(NULL)
    }else{
        e <- df$Residual
    }
  }else{
    e <- df$Error
  }

  d <- df$Demand
  m <- length(e[!is.na(e)])
  ae <- sum(e, na.rm=TRUE)/m
  da <- sum(d[!is.na(e)])/m
  sae <- ae/da
  mad <- sum(abs(e[!is.na(e)]), na.rm=TRUE)/m
  smad <- mad/da
  mape <- sum(e[!is.na(e)]/d[!is.na(e)])/m
  mse <- sum(e[!is.na(e)]**2, na.rm=TRUE)/m
  smse <- mse/da
  rmse <- sqrt(mse)
  srmse <- sqrt(mse)/da
  result <- list(ae,sae,mad,smad,mape,mse,smse,rmse,srmse)
  names(result)<-c("Av. Error","Scaled Av. Err.", "MAD", "Scaled MAD", "MAPE", "MSE", "Scaled MSE", "RMSE", "Scaled RMSE")
  return(result)
}
