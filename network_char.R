# network_char.R
# Created by: Nathan Wikle, 6 July 2015

# A function to calculate relevant characteristic statistics and
# plots for a given network. Most of these statistics and 
# plots were found in "Dynamical Processes on Complex Networks"
# by Barrat et al. (2008).
# Input: 1) graph: the network to be studied; should be 
#   represented as an igraph object.
#        2) weighted: a boolean specifying whether the graph
#   is weighted.  Default is TRUE.
# Output: 1) a pdf document, called "network_char.pdf"
network_char <- function(graph, weighted = TRUE) {
  require(igraph)
  if (!is.igraph(graph)){
    stop("not a graph object")
  }
  # statistics to calculate
  # l, shortest path
  # l.w, weighted shortest path
  # k, degree
  # k.in, in degree
  # k.out, out degree
  # b, betweenness centrality
  # s, strength
  # C, clustering coefficient
  # C_w, weighted clustering coefficient
  # k.nn, average nearest neighbors degree
  # k.nn.w.un, weighted average nearest neighbors degree, undirected
  # k.nn.w.in, weighted average nearest neighbors degree, in edges
  # k.nn.w.out, weighted average nearest neighbors degree, out edges
  
  l <- shortest.paths(graph,mode="all",weights=NA)
  l.in <- shortest.paths(graph,mode="in",weights=NA)
  l.out <- shortest.paths(graph,mode="out",weights=NA)
  if (weighted){
    l.w <- shortest.paths(graph,mode="all")
    l.w.in <- shortest.paths(graph,mode="in")
    l.w.out <- shortest.paths(graph,mode="out")
  }
  
  w <- E(graph)$weight
  
  k <- degree(graph,mode="all")
  k.in <- degree(graph,mode="in")
  k.out <- degree(graph,mode="out")
  if (weighted) {
    s <- graph.strength(graph,mode="all")
    s.in <- graph.strength(graph,mode="in")
    s.out <- graph.strength(graph,mode="out")
  }
  
  b.un <- betweenness(graph,directed=FALSE,weights=NA)
  b.dir <- betweenness(graph,weights=NA)
  if (weighted) {
    b.un.w <- betweenness(graph,directed=FALSE)
    b.dir.w <- betweenness(graph)
  }
  
  C.i <- transitivity(graph,type="local",weights=NA)
  if (weighted){
    C.i.w <- transitivity(graph,type="barrat")
  }
  
  k.nn <- graph.knn(graph,weights=NA)$knn
  k.nn.w <- barrat_knn(graph,mode="all")
  k.nn.w.in <- barrat_knn(graph,mode="in")
  k.nn.w.out <- barrat_knn(graph,mode="out")
  
  # calculate probability distributions for most of the above statistics
  P.k <- graph_distribution(k)
  P.k.in <- graph_distribution(k.in)
  P.k.out <- graph_distribution(k.out)
  
  P.s <- graph_distribution(s)
  P.s.in <- graph_distribution(s.in)
  P.s.out <- graph_distribution(s.out)
  
  P.b.un <- graph_distribution(b.un)
  P.b.dir <- graph_distribution(b.dir)
  P.b.un.w <- graph_distribution(b.un.w)
  P.b.dir.w <- graph_distribution(b.dir.w)
  
  P.c.i <- graph_distribution(C.i,all=FALSE)
  P.c.i.w <- graph_distribution(C.i.w,all=FALSE)
  
  # create the output table
  variable <- c("k","k.in","k.out","s","s.in","s.out","b.un","b.dir",
                "b.un.w","b.dir.w","C.i","C.i.w")
  expVal <- c(ExpVal(P.k$dist,P.k$points),ExpVal(P.k.in$dist,P.k.in$points),
              ExpVal(P.k.out$dist,P.k.out$points),ExpVal(P.s$dist,P.s$points),
              ExpVal(P.s.in$dist,P.s.in$points),ExpVal(P.s.out$dist,P.s.out$points),
              ExpVal(P.b.un$dist,P.b.un$points),ExpVal(P.b.dir$dist,P.b.dir$points),
              ExpVal(P.b.un.w$dist,P.b.un.w$points),ExpVal(P.b.dir.w$dist,P.b.dir.w$points),
              ExpVal(P.c.i$dist,P.c.i$points),ExpVal(P.c.i.w$dist,P.c.i.w$points))
  
  max <- c(max(k),max(k.in),max(k.out),max(s),max(s.in),max(s.out),
           max(b.un),max(b.dir),max(b.un.w),max(b.dir.w),max(C.i,na.rm=TRUE),
           max(C.i.w,na.rm=TRUE))
              
  twoSigma <- c(twoSigma(P.k$dist,P.k$points),twoSigma(P.k.in$dist,P.k.in$points),
                twoSigma(P.k.out$dist,P.k.out$points),twoSigma(P.s$dist,P.s$points),
                twoSigma(P.s.in$dist,P.s.in$points),twoSigma(P.s.out$dist,P.s.out$points),
                twoSigma(P.b.un$dist,P.b.un$points),twoSigma(P.b.dir$dist,P.b.dir$points),
                twoSigma(P.b.un.w$dist,P.b.un.w$points),twoSigma(P.b.dir.w$dist,P.b.dir.w$points),
                twoSigma(P.c.i$dist,P.c.i$points),twoSigma(P.c.i.w$dist,P.c.i.w$points))
  
  kappa <- c(kappa(P.k$dist,P.k$points),kappa(P.k.in$dist,P.k.in$points),
             kappa(P.k.out$dist,P.k.out$points),kappa(P.s$dist,P.s$points),
             kappa(P.s.in$dist,P.s.in$points),kappa(P.s.out$dist,P.s.out$points),
             kappa(P.b.un$dist,P.b.un$points),kappa(P.b.dir$dist,P.b.dir$points),
             kappa(P.b.un.w$dist,P.b.un.w$points),kappa(P.b.dir.w$dist,P.b.dir.w$points),
             kappa(P.c.i$dist,P.c.i$points),kappa(P.c.i.w$dist,P.c.i.w$points))
  
  table <- data.frame(variable,expVal,max,twoSigma,kappa)
  stats <- list(l,l.in,l.out,l.w,l.w.in,l.w.out,w,k,k.in,k.out,s,s.in,s.out,
                b.un,b.dir,b.un.w,b.dir.w,C.i,C.i.w,k.nn,k.nn.w,k.nn.w.in,
                k.nn.w.out)
  names(stats) <- c("l","l.in","l.out","l.w","l.w.in","l.w.out","w","k","k.in",
                    "k.out","s","s.in","s.out","b.un","b.dir","b.un.w","b.dir.w",
                    "C.i","C.i.w","k.nn","k.nn.w","k.nn.w.in","k.nn.w.out")
  results<-list(table,stats)
  names(results) <- c("table","stats")
  results
}

