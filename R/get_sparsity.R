#' Retrieve percentage of zeros from data set
#'
#' @param data_input data frame, \cr
#' the data from which the percentage of zeros should be calculated
#'
#' @return num, The calculated percentage of zeros in the data set
#' @export
#' @import checkmate
#' @examples 
#' # use gold data 
#' get_sparsity(sc_gold)
#' 
get_sparsity <- function(data_input){
  
  assertDataFrame(x = data_input)
  
  nc <- dim(data_input)[2]
  ng <- dim(data_input)[1]
  Tot <- nc * ng
  sparsity <- round(sum(data_input==0) / Tot *100,2)
  return(sparsity)
}