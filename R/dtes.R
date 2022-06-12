#' @title Dumped Triple Exponential Smoothing - Gardner-McKenzie-Holt-Winters algorithm
#'
#' @author Athanasios Migdalas, \email{athmig59@gmail.com}
#'
#' @description 
#' Forecasting method of triple exponential smoothing for time series. 
#' Provides a common interface to  both additive and (mixed) multiplicative model 
#' A Gardner-McKenzie dumping factor is used to dump future trend
#' 
#' @details 
#' Requires AMPrognosis::dtes_add(), AMPrognosis::dtes_add(),
#' AMPrognosis::seasonal_mult(), AMPrognosis::seasonal_add()
#' 
#' @seealso sma, wma, naive, ses, ma, tes, ddes, lrtp, tes_add, tes_mult, dtes_add, dtes_mult
#' 
#' @param{d} {numerical vector containing observed demand}
#' @param{fp} {seasonal frequency, for instance, 4 for quarters}
#' @param{alpha}  {smoothing parameter for level update; the default value is 0.3}
#' @param{beta}   {smoothing parameter for trend update; the default value 0.2}
#' @param{gamma}  {smoothing parameter for seasonal update; the default value 0.1}
#' @param{phi}    {dumping factor; default value of 1 results in classic Holt-Winters algorithm}
#' @param{a0}     {initialization level (previous forecast for first period); if it is not supplied, the first observed demand is selected}
#' @param{b0}     {initialization trend; if it is not supplied, the difference between the first two observed demands is selected}
#' @param{future} {number of future periods to forecast} 
#' @param{model} {can be \code{"additive"} or \code{"multiplicative"}; the additive case is the default one}
#' 
#' @return A data frame consisting of three columns: 
#' \itemize{
#' \item\code{Demand} {it is the supplied \code{d}} 
#' \item\code{Forecast} {it contains the predicted values for each period, icluding \code{future} periods}
#' \item\code{Level} {it contains the level updates produced during the process}
#' \item\code{Trend} {it contains the trend updates produced during the process}
#' \item\code{Seasonal} {it contains the seasonal updates produced during the process}
#' \item\code{Error} {the residual values expressing the difference between \code{Demand} and \code{Forecast}}
#' }
#' A \code{NULL} is returned if \code{fp} is not supplied or if \code{method} is assigned wrong value.
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
#' @examples
#' ## Load test data, for example
#' data("dTest6")
#' ## Run for additive model (default)
#' dtes(d=dTest6,fp=4, phi=0.8)
#' ## Run for multiplicative model
#' dtes(d=dTest6,fp=4, phi=0.8, model="multiplicative")
#' 
#' @export
dtes <- function(d, fp=NaN, alpha=0.3, beta=0.2, gamma=0.1, phi=1, future=1, model=c("additive","multiplicative"), a0=0, b0=NaN){
  if ( model == "additive"){
    AMPrognosis::dtes_add(d, fp, alpha, beta, gamma, phi, future, a0, b0)
  } else if(model == "multiplicative"){
    AMPrognosis::dtes_mult(d, fp, alpha, beta, gamma, phi, future, a0, b0)
  } else{
    message("*** Error: dtes does not provide for such a model ***")
    return(NULL)
  }
}