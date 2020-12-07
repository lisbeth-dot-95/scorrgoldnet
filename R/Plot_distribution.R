#' Plot distribution of expression values
#'
#' @param data A data frame used to plot its distribution
#'
#' @return A density plot of expression values
#' @export
#' @import ggplot2
#' @importFrom reshape2 melt
#' @examples
#' # use gold data
#' Plot_distribution(data = sc_gold)
Plot_distribution <- function(data){
  #utils::globalVariables("value")
  input <- melt(data) 
  ggplot(input, aes(x = input$value)) + 
  geom_density(size=1.5) +
  # coord_cartesian(xlim=c(0,0.2), ylim=c(0,15)) +
  xlab("Log Expression Value") +
  ylab("Density") +
  labs(title="Distribution of Expression value") + 
  theme_bw() %+replace%
  theme(
    panel.grid = element_blank(),
    complete = TRUE)
  }
  