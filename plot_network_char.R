# plot_network_char.R

# A function to plot various network characteristics, with the hope of
# classifying the network.
# Input: 1) stats, a list of statistics, as outputted by the network_char
#           function.
# Output: 1) plots indicating the potential power law degree distributions
#            of the network
plot_network_char <- function(stats,weighted=TRUE,line=TRUE,linetype="loess"){
  # plot M(l) for the six length characteristics
  length.plots <- plot_length(stats,weighted=weighted)
  # plot probability distributions
  dist <- plot_prob_dist(stats,output="all",weighted=weighted,line=line,linetype=linetype)
  # plot knn(k) and C(k)
  spec <- plot_spectrum(stats,weighted=weighted,line=line,linetype=linetype)
  plots <- c(length.plots,dist,spec)
  plots
}


