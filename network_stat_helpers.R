# The following functions must be in the local environment for network_char()
# to work.


# A function to determine the expected value (first moment) of
# a discrete distribution.
# Input: 1) dist: a distribution of a network statistic 
#   (for example, degree)
# Output: 1) the expected value calculated on the distribution
ExpVal <- function(dist,pts){
  v<-sapply(pts,function(x) (x)*dist[which(pts==x)])
  e <- sum(v)
  e
}

# A function to calculate an approximately 95% confidence interval
# in the measurements.
# Input: 1) dist: a distribution of a network statistic 
#   (for example, degree)
# Output: 1) two times the standard deviation: 2*sqrt(E(x^2)-E(x)^2)
twoSigma <- function(dist,pts){
  x2<-momentCalc(dist,pts,2)
  x1<-ExpVal(dist,pts)
  twoSig <- 2*sqrt(x2 - (x1*x1))
  twoSig
}

# A function to determine the second moment of a discrete 
# distribution.
# Input: 1) dist: a distribution of a network statistic 
#   (for example, degree)
# Output: 1) the second moment calculated on the distribution
momentCalc <- function(dist,pts,k){
  v <- sapply(pts,function(x) dist[which(pts==x)]*(x^k))
  m <- sum(v)
  m
}

SecondMomentOld <- function(dist){
  v <- c(1:length(dist))
  v <- sapply(v,function(x) (x-1)*(x-1)*dist[x])
  m <- sum(v)
  m
}

# A function to calculate the heterogeneity parameter kappa
# from the given distribution.
# Input: 1) dist: a distribution of a network statistic 
#   (for example, degree)
# Output: 1) kappa = E(x^2)/E(x)
kappa <- function(dist,pts){
  k1 <- ExpVal(dist,pts)
  k2 <- momentCalc(dist,pts,2)
  kap <- k2/k1
  kap
}


# A function to calculate the weighted average nearest neighbors degree,
# as defined by Barrat (knnw_i = sum(x_ij*w_ij*k_j)from j=1 to N / s_i)
# Input: 1) g: the (weighted) graph in question
#        2) mode: a string specifying whether knn should be calculated for
#            incoming edges, outgoing edges, or both.
# Output: a vector of the weighted average nearest neighbors degree
barrat_knn <- function(g,mode=c("all","out","in")){
  if (mode=="all"){
    #collapsed graph so it is undirected
    g.und <- as.undirected(g,mode="collapse")
    e.l.und <- get.edgelist(g.und) #edgelist
    w.und <- E(g.und)$weight #weights
    und.m <- cbind(e.l.und,w.und) #matrix containing edgelist and weights
    und.m <- und.m[order(und.m[,1],und.m[,2]),] #matrix, ordered by first edge
    und.nn <- neighborhood(g.und,order=1) #neighbors of each vertex
    und.s <- graph.strength(g.und) #strength of each vertex
    und.k <- degree(g.und) #degree of each vertex
    knn.w<-rep(0,length(und.nn)) #vector of average nearest neighbors degree
  
    for (i in 1:length(und.nn)){
      # find edges containing node i
      i.first <- which(und.m[,1]==i) 
      i.second <- which(und.m[,2]==i)
      # use the indices to calculate elements in the sum
      if (length(i.first)>0){
        n1 <- sapply(i.first,function(x) und.m[x,3]*und.k[und.m[x,2]])
      } else {n1<-0}
      if (length(i.second)>0){
        n2 <- sapply(i.second,function(x) und.m[x,3]*und.k[und.m[x,1]])
      } else {n2<-0}
      n <- sum(n1)+sum(n2) #sum elements
      if (und.s[i]!=0){
        knn.w[i] <- n/und.s[i] #calculate knn.w
      } else {
        knn.w[i] <- 0
      }
    } 
  } else if (mode=="out"){ #repeat the above, only using outgoing edges
    el <- get.edgelist(g)
    w <- E(g)$weight
    edge.m <- cbind(el,w)
    edge.m <- edge.m[order(edge.m[,1],edge.m[,2]),] #matrix, ordered by first edge
    nn.out <- neighborhood(g,order=1,mode="out")
    s.out <- graph.strength(g,mode="out")
    k.out <- degree(g,mode="out")
    knn.w<-rep(0,length(nn.out))
    
    for (i in 1:length(nn.out)){
      ind <- which(edge.m[,1]==i)
      if (length(ind)>0){
        n1 <- sapply(ind,function(x) edge.m[x,3]*k.out[edge.m[x,2]])
      } else {
        n1<-0
      }
      n <- sum(n1)
      if (s.out[i]!=0){
        knn.w[i] <- n/s.out[i] 
      } else {
        knn.w[i] <- 0
      }
    }
  } else if (mode=="in"){ #repeat the above, only using incoming edges
    el <- get.edgelist(g)
    w <- E(g)$weight
    edge.m <- cbind(el,w)
    edge.m <- edge.m[order(edge.m[,2],edge.m[,1]),]
    nn.in <- neighborhood(g,order=1,mode="in")
    s.in <- graph.strength(g,mode="in")
    k.in <- degree(g,mode="in")
    knn.w<-rep(0,length(nn.in))
    
    for (i in 1:length(nn.in)){
      ind <- which(edge.m[,2]==i)
      if (length(ind)>0){
        n1 <- sapply(ind,function(x) edge.m[x,3]*k.in[edge.m[x,1]])
      } else {
        n1<-0
      }
      n <- sum(n1)
      if (s.in[i]!=0){
        knn.w[i] <- n/s.in[i] 
      } else {
        knn.w[i] <- 0
      }
    }
  } else {
    stop("Please specify mode as: all, out, or in.")
  }
  knn.w #return weighted average nearest neighbor degree
}


# A function that gives the probability distribution for a given statistic.
# Input: 1) stat: a vector of a specific statistic, typically returned from
#             a network analysis.
#        2) all: a boolean, default to TRUE, that specifies whether the 
#             distribution should be discretized into bins or not.
#        3) cumulative: a boolean, set to FALSE, that determines if the 
#             probability distribution should be cumulative or not.
# Output: a dataframe, with the probabilities, associated points, and
#           breaks (useful when the distribution has been discretized)
graph_distribution <- function (stat, all=TRUE, cumulative = FALSE, ...)
{
  # graph.strength() instead of degree()
  cs <- stat
  if (all) {
    hi <- hist(cs,0:(max(cs,na.rm=TRUE)+1),plot=FALSE,right=FALSE)$density
    hi <- hi/sum(hi)
    br<- hist(cs,0:(max(cs,na.rm=TRUE)+1),plot=FALSE,right=FALSE)$breaks
    breaks <- rep(NA,length(hi))
    points <- c(0:max(cs,na.rm=TRUE))
    for (i in 1:(length(br)-1)){
      breaks[i]<-paste("[",br[i],",",br[i+1],")",sep="")
    }
    if (!cumulative) {
      dist <- hi
    }
    else {
      dist <- rev(cumsum(rev(hi)))
    }
  } else {
    hi <- hist(cs,breaks="FD",plot=FALSE,right=FALSE)$density
    hi <- hi/sum(hi)
    br <- hist(cs,breaks="FD",plot=FALSE,right=FALSE)$breaks
    breaks <- rep(NA,length(hi))
    points<-rep(NA,length(hi))
    for (i in 1:(length(br)-1)){
      if(i!=(length(br)-1)){
        breaks[i]<-paste("[",br[i],",",br[i+1],")",sep="")
      } else{
        breaks[i]<-paste("[",br[i],",",br[i+1],"]",sep="")
      }
      points[i]<-mean(br[i:(i+1)])
    }
    if (!cumulative) {
      dist <- hi
    }
    else {
      dist <- rev(cumsum(rev(hi)))
    }
  }
  res <- data.frame(dist,points,breaks)
  res
}




