#' Generate a sparse data set from a reference
#'
#' @param lamdba_index numeric, range from 0 to 1, \cr
#' close to zero results in a more sparse data set
#'
#' @return A dataframe of sparsified data
#' @export
#' @importFrom stats runif
#' @examples 
#' # data(sc_gold)
#' sparse_data <- introduce_sparsity(0.1)
#' 
introduce_sparsity <- function(lamdba_index){
  
  
  assertNumeric(lamdba_index,lower = 0,upper = 1)
  # Parameter in the function
  # lamdba_index: controling for sparsity severety

  # select the index of cells with 800 cells from 2000 cells
  index_matrix <- t(matrix(1:2000,ncol = 20))
  
  set.seed(1)
  
  index_sample <- as.vector(t(index_matrix[sample(1:20,8),]))
  
  # define the true data
  
  data_true <- scorrgoldnet::data_noise[,index_sample]
  
  # define the dropout 
  p <- matrix(rep(exp(-lamdba_index*rowMeans(data_true)^2),
                 dim(data_true)[2]), ncol = dim(data_true)[2])
  
  set.seed(1)
  
  dropout_s <- matrix(runif(length(p)), nrow = dim(p)[1])
  
  dropmatrix <- dropout_s > p 
  
  # define the sparse data
  data_raw <- data_true*dropmatrix

  out <- as.data.frame(data_raw)
  
  return(out)
  
}
