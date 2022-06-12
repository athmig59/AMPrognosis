#' @title Naive forecasting procedures
#'
#' @author Athanasios Migdalas, \email{athmig59@gmail.com}
#'
#' @description 
#' Naive forecasting procedures (see the \code{method} parameter)
#' 
#' @seealso sma, wma, ma, ses, des, tes, ddes, dtes, lrtp
#' 
#' @param{d} {numerical vector containing observed demand}
#' @param{future} {number of future periods to forecast, default 1} 
#' @param{fp} {needed if the \code{seasonal} procedure is pecified in order to provide the seasonal period frequency, dafault is 4, i.e., quarters} 
#' @param{method} {specifies which naive procedure to be used. The alternatives are:
#' \itemize{
#' \item\code{naive} {simply repeat the last observed value}
#' \item\code{mean} {the mean value of all historic data}
#' \item\code{trend} {naive trend projection, i.e., linear interpolation between first and last observation, and extrapolation into the future (cf. linear regression)}
#' \item\code{drift} {the amount of change over time is set to be the average change seen in the historical data (cf. naive trend)}
#' \item\code{seasonal} {naive consideration of seasonality, i.e., repeat for each time period the last observed value for the corresponding period in the previous season}
#' }}
#' 
#' @details 
#' Method \code{drift} is just a slightly different implementation of \code{trend}
#' and therefore both will produce the same forecast. You can verify this by
#' listing the results of the two procedures side-by-side, for instance, like this:
#' 
#' \code{df1<-naive(d,future=3, method="trend")}
#' 
#' \code{df2<-naive(d,future=3, method="drift")}
#' 
#' \code{data.frame(df1$Forecast,df2$Forecast)}
#' 
#' @return A data frame consisting of three columns: 
#' \itemize{
#' \item\code{Demand} {it is the supplied \code{d}} 
#' \item\code{Forecast} {it contains the predicted values for each period, icluding \code{future} periods}
#' \item\code{Error} { its the residual values expressing the difference between \code{Demand} and \code{Forecast}}
#' }
#' It returns NULL if the supplied order or seasonality is incosistent with the length of d
#' 
#' @references
#'  - Axsäter, S. (2015) Inventory Control, 3d Edition, Springer
#'  - Snyder, L. V. and Shen, Z.-J., M. (2011) Fundamentals of Supply Chain Theory, Wiley
#'  - Cowpertwait, P. S. and Metcalfe, A. V. (2009) Introductory Time Series with R, Use R!, Springer
#'  - Hyndman, R. J. and Athanasopoulos, G. (2021) Forecasting Principles and Practive, 3d Edition, OTexts
#'
#'@examples
#' ## Load test data, for example, 
#' data("dTest1")
#' ##
#' ## Run the default naive method for three future periods
#' ##
#' naive(d=dTest1,future=3)
#' ##
#' ## Run the mean method for three future periods 
#' ##
#' naive(d=dTest1,future=3, method="mean")
#' ##
#' ## Run the trend method for three future periods 
#' ##
#' naive(d=dTest1,future=3, method="trend") 
#' ##
#' ## Run the drift method for three future periods
#' ##
#' naive(d=dTest1,future=3, method="drift")
#' ##
#' ## Run the seasonal method for three future periods
#' ##
#' naive(d=dTest1,future=3, method="seasonal", fp=4)
#' 
##
## ---- Functions (Methods) ----
##
## Simple naive - repeat last observed value
##
#' @name naive
NULL
##
nnav <- function(d,future=1){
  m <- length(d)
  if(future >=1) {d <- c(d,rep(NaN, future))}
  l <- length(d)
  f <- rep(NaN, l)
  
  for( i in 1:m){
    if((i+1) > m){break}
    f[i+1] <- d[i]
  }
  
  if( (m+1) <= (l-1)){
    for( i in (m+1):(l-1)){
      f[i+1] = f[i]
    }
  }
  
  result <- data.frame(Demand=d, Forecast=f, Error=d-f)
  return(result)
}

##
## Mean naive - the mean value of all historic data
## 

mnav <- function(d,future=1){
  md <- mean(d)
  
  m <- length(d)
  if(future >=1) {d <- c(d,rep(NaN, future))}
  l <- length(d)
  f <- rep(NaN, l)
  
  if(1<=l){
    for( i in 1:l){
      f[i] <- md
    }
  }
  
  result <- data.frame(Demand=d, Forecast=f, Error=d-f)
  return(result)
}

##
## Naive trend projection - linear interpolation between first and last observation, 
##     and extrapolation into the future (cf. linear regression)
##

tnav <- function(d,future=1){
  m <- length(d)
  if(future >=1) {d <- c(d,rep(NaN, future))}
  l <- length(d)
  f <- rep(NaN, l)
  
  if(m > 1){
    slope = (d[m] - d[1]) / (m - 1)
    intercept = (d[1]*m - d[m]) / (m - 1)
    for( i in 1:l){
      f[i] <- intercept + i * slope
    }
  }else{
    f[1:l] = d[m]
  }
  
  result <- data.frame(Demand=d,Forecast=f,Error=d-f)
  return(result)
}

##
## Drift - the amount of change over time is set to be the  
##     average change seen in the historical data (cf. previous function)
##

dnav <- function(d,future=1){
  m <- length(d)
  if(future >=1) {d <- c(d,rep(NaN, future))}
  l <- length(d)
  f <- rep(NaN, l)
  
  if(m > 1){
    slope = (d[m] - d[1]) / (m - 1)
    f[1] <- d[1]
    for( i in 2:m){
      f[i] <- d[1] + (i-1)*slope
    }
    if( (m+1) <=l ){
      for( i in (m+1):l){
        f[i] <- d[m] + (i-m) * slope
      }
    }
  }else{
    f[1:l] = d[m]
  }
  
  result <- data.frame(Demand=d,Forecast=f,Error=d-f)
  
  return(result)
}

##
## Seasonal naive - repeat for each time period the last observed value
##                  for the corresponding period in the previous season
##

snav <- function(d,future=1, fp=4){
  m <- length(d)
  if(future >=1) {d <- c(d,rep(NaN, future))}
  l <- length(d)
  f <- rep(NaN, l)

  if ( m < fp){
    return(NULL)
  }
  
 
  if(m+fp > l){ 
    N <-l
  }else{
    N <- m + fp
  }
  for( i in (fp+1):N){
    f[i] = d[i-fp]
  }
  
  if(m+fp+1 <= l){
    for( i in (m+fp+1):l){
      f[i] <- f[i-fp]
    }
  }
  
  message("d:", length(d), " f:", length(f))
  
  result <- data.frame(Demand=d,Forecast=f,Error=d-f)
  return(result) 
}
##
## --- Interface ---
##
#' @rdname naive
#' @export
naive <- function(d,future=1,fp=4,method=c("naive","mean","trend","drift","seasonal")){
  if(method=="naive"){
    nnav(d=d,future=future)
  }else if(method=="mean"){
    mnav(d=d,future=future)
  }else if(method=="trend"){
    tnav(d=d,future=future)
  }else if(method=="drift"){
    dnav(d=d,future=future)
  }else if(method=="seasonal"){
    snav(d=d,future=future, fp=fp)
  }else{
    print("*** naive: Requested method is unavailable")
    return(NULL)
  }
}