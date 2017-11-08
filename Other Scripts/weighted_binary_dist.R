# Simple distance function used for alphanumeric vectors. 
#' @param x first alphanumeric array
#' @param y second alphanumeric array
#' @param w a vector with the same dimension as x and y, having positive real numbers indicating
#'        the different weight each coordinate takes.
#'
#' @return A positive real number indicading the distance 
#'
weighted_binary_dist <- function(x,y,w)
{
  return(sum(w*(x!=y)))
} # end weighted_binary_dist function
