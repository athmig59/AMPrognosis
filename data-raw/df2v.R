#' @title Transforms a (range of columns of a) data frame into a vector 
#' 
#' @author Athanasios Migdalas, \email{athmig59@gmail.com}
#' 
#' @description
#'  Transforms a given data frame, or a spicified range of its columns,
#'  into a vector.
#'  
#'  @param{df} {a data frame}
#'  @param{col1} {the first column in a range specification, default value is 1}
#'  @param{col2} {the second column in a range specification, default value is the last column of the data frame}
#' 
#' @return  Avector consisting of the elemnts of the specified columns
#' 
#' @export
df2v <- function(df, col1=1, col2=ncol(df)) {
  v <- df[,col1]
  if(col2 > col1){
    for(i in (col1+1):col2){
      v <- append(v, df[,i])
    }
  }
  return(v)
}