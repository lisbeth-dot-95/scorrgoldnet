#' Plot distribution of expression values
#'
#' @param data A data frame (cells over genes) \cr
#' of log-transformed genes expression values. 
#'
#' @return A density plot of expression values
#' @export
#' @import ggplot2
#' @importFrom reshape2 melt
#' @examples
#' # use gold data
#' data(sc_gold)
#' Plot_distribution(data = sc_gold)
Plot_distribution <- function(data){
  
  assertDataFrame(x = data)
  
  input <- melt(data) 
  p <- ggplot(input, aes(x = input$value)) + 
  geom_density(size=1) +
  # coord_cartesian(xlim=c(0,0.2), ylim=c(0,15)) +
  xlab("Log Expression Value") +
  ylab("Density") +
  labs(title="Distribution of Expression values") + 
  theme_bw() %+replace%
  theme(
    panel.grid = element_blank(),
    complete = TRUE)
  return(p)
  }
  