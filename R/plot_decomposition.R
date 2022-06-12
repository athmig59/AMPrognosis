#' @title Plots the decomposition of a demand time series
#'
#' @author Athanasios Migdalas, \email{athmig59@gmail.com}
#'
#' @description
#' Provided with the output of the decomposition functions, it
#' generates several plots in one figure to provide insight. It plots the figure on screen,
#' it can store it in a file, if a file name is specified, or it returns
#' it in variable, if specified. The variable can subsequently be
# 'printed' on screen using the print() function.
#'
#' @details
#' Requires the packages \code{ggplot2}, \code{gridExtra}, and \code{cowplot}
#'
#' @seealso decomposition_lrtp_mult, decomposition_lrtp, decomposition_sma_add,
#' decomposition_sma_mult, decomposition_sma
#'
#' @param{df} {adata frame containing the output of a decomposition function, that is, \code{Demand}, \code{DeTrended} demand, \code{Residual}, \code{Seasonality}, and \code{Trend}. \code{Seasonality} may be absent, of course,  in some cases.}
#' @param{figtitle}  {Title of the plot. If not supplied, "Time Series Decomposition" is used}
#' @param{figfil}    {Optionally, a file name to store the plot in A4 page. The extension determines the type of the plot; \code{.pdf} for PDF file, \code{.png} for PNG file, etc.}
#'
#' @return A figure in A4 page format containing plots of the decomposition components
#'
#'
#'#' @examples
#' ## Load test data, for instance
#' ##
#' data("dTest6")
#' ##
#' ## Apply additive decomposition based on linear regression
#' ##
#' res<-decomposition_lrtp_add(dTest6, fp=4)
#' ##
#' ## Plot the decomposition components
#' ##
#' plot_decomposition(res$Table)
#'
#'
#' @export
plot_decomposition <- function(df,figtitle=NaN, figfil=NaN){

  g0 <- ggplot2::ggplot(df)

  m  <- length(df$Demand)

  if(!is.character(figtitle)) {figtitle <- "Time Series Decomposition" }

  g1 <- g0 +
    ggplot2::geom_line(aes(x=1:m, y=Demand, color='Demand')) +
    xlab('Time periods') + ylab('Demand') + ggtitle(figtitle)

  g2 <- g0 +
    ggplot2::geom_line(aes(x=1:m, y=DeTrended, color='DeTrended')) +
    xlab('Time periods') + ylab('De-trended demand')

  g3 <- g0 +
    ggplot2::geom_line(aes(x=1:m, y=Trend, color='Trend')) +
    xlab('Time periods') + ylab('Trend')

  g5 <- g0 +
    ggplot2::geom_line(aes(x=1:m, y=Residual, color='Residual')) +
    xlab('Time periods') + ylab('Residual')

  if('Seasonality' %in% names(df)){

    g4 <- g0 +
      ggplot2::geom_line(aes(x=1:m, y=Seasonality, color='Seasonality')) +
      xlab('Time periods') + ylab('Seasonality')

    fig <- cowplot::plot_grid(g1,g2,g3,g4,g5, ncol=1, nrow=5)

  }else{

    fig <- cowplot::plot_grid(g1,g2,g3,g5, ncol=1, nrow=4)
  }

  #pdf("dec.pdf")
  #  print(fig)
  #dev.off()

  if(is.character(figfil)) {
    ggplot2::ggsave(figfil, dpi=300, width = 21, height = 29.7, units="cm")
  }

  return(fig)

}

