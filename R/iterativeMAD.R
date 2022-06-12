#' @title Iterative update of MAD and SD
#' 
#' @author Athanasios Migdalas, \email{athmig59@gmail.com}
#' 
#' @description 
#' Iterative update of MAD and estimation of standard deviation (SD) in accordance with
#' section 2.10.2 in Axsätter's book (see also section 6.3 in this book) 
#' 
#' 
#' @references
#'  - Axsäter, S. (2015) Inventory Control, 3d Edition, Springer
#'  - Snyder, L. V. and Shen, Z.-J., M. (2011) Fundamentals of Supply Chain Theory, Wiley
#'  - Cowpertwait, P. S. and Metcalfe, A. V. (2009) Introductory Time Series with R, Use R!, Springer
#'  - Hyndman, R. J. and Athanasopoulos, G. (2021) Forecasting Principles and Practive, 3d Edition, OTexts
#'
#' @param{df} {data frame containing at least columns \code{Demand} and \code{Forecast} and maybe \code{Error}. If \code{Error} is not present, it is created by the function.}
#' @param{alpha} {a smoothing parameter with value between 0 and 1}
#' @param{mad0} {initial MAD. If not given, the function will use the absolute value 
#' of the first forecast error (residual)}
#' 
#' @return A data frame wich extends the input \code{df} with two additional columns:
#' \itemize{
#' \item\code{MAD} {containing the successive updates of MAD}
#' \item\code{SD} {containing the successive updates of standard deviation}
#' }
#' 
#' @examples 
#' ## Perform a forecast
#' ##
#' r0 <- des(dTest2, future=0)
#' ##
#' ##
#' ## Generate iteratively MAD and SD approximations
#' ##
#' iterativeMAD(r0)
#' 
#' 
#' @export
iterativeMAD <- function(df, alpha=0.4, mad0 = 0){
  m <- which(is.na(df))[1]
  m <- m - 1
  if(is.na(m)){m=length(df$Demand)}
  n <- length(df$Demand)
  
  message("m:",m," n:",n)
  
  if( !('Error' %in% names(df) ) ){
    error <- rep(NaN,n)
    error[1:m] <- df$Demand[1:m] - d$Forecast[1:m]
    df$Error <- error 
  }
  
  if( mad0 == 0 ){ mad0 <- df$Error[1] }
  
  mad <- rep(NaN,n)
  sd  <- rep(NaN,n)
  mad[1] <- (1-alpha) * mad0 + alpha * abs(df$Error[1])
  sd[1] <- mad[1] * sqrt( 0.5 * pi )
  for(t in 2:m){
    mad[t] <- (1-alpha) * mad[t-1] + alpha * abs(df$Error[t])
    sd[t] <- mad[t] * sqrt( 0.5 * pi )
  }
  
  df$MAD <- mad
  df$SD  <- sd
  
  return(df)
}   