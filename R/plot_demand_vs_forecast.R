#' @title Plot observed demand against a forecast (or against its trend)
#'
#' @description
#' The function plots the demand against estimated forecst or trend
#'
#' @details
#' Requires package \code{ggplot2}
#'
#' @param{df} {a data frame containing a column \code{Demand} and a column \code{Trend} or \code{Forecast}.
#' Optionally, it may contain a column \code{SeasonedForecast}}. Typically, the data frame is the resulting
#' output of other package funnctions such as \code{ma}, \code{sma}, \code{lrpt}, \code{ses}, etc.
#'
#' @return
#' A plot of \code{Demand} vs \code{Forecast} (or \code{Trend}), and, possibly, vs \code{SeasonedForecast}.
#' The plot can be stored in a variable and displayed by printing the variable
#'
#' @examples
#' ## Load test data, for example
#' data("dTest1")
#' ## Run a forecast procedure, for instance
#' df0 <- ma(d=dTest1, n=5, future=5)
#' ## Plot Demand vs Forecast
#' p1<-plot_demand_vs_forecast(df0)
#' print(p1)
#' ## Estimate the time series demand
#' res <- lrtp(d=dTest1)
#' ## Plot Demand vs Trend
#' p2 <- plot_demand_vs_forecast(df=res$Table)
#' print(p2)
#' ## or plot without storing in a variable
#' plot_demand_vs_forecast(df=res$Table)
#'
#' @importFrom ggplot2 ggplot geom_line aes xlab ylab ggtitle
#' @export
plot_demand_vs_forecast <- function(df){

  p1 <- NULL

  if('Trend' %in% names(df)){
    p1 <- ggplot2::ggplot(df)+
      geom_line(aes(x=1:length(Trend), y=Trend, color="Trend")) +
      xlab('Time periods')
    p1 <- p1 +
      geom_line(aes(x=1:length(Demand), y=Demand, color="Demand")) +
      xlab('Time periods')
    p1 <- p1 + ggtitle("Series VS Trend")
  }else{
    p1 <- ggplot2::ggplot(df)+
      geom_line(aes(x=1:length(Demand), y=Demand, color="Demand")) +
      xlab('Time periods')
    if('Forecast' %in% names(df)){
      p1 <- p1 +
        geom_line(aes(x=1:length(Forecast), y=Forecast, color="Forecast")) +
        xlab('Time periods') +
        ggtitle("Series VS Forecast")
    }
    if('SeasonedForecast' %in% names(df)){
      p1 <- p1 +
        geom_line(aes(x=1:length(SeasonedForecast), y=SeasonedForecast, color="SeasonedForecast"))+
        xlab('Time periods')
    }
  }

  return(p1)
}
