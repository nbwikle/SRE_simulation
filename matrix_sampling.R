
# function to sample from four weight matrices, creating a new
# transition matrix.
# input: M1,M2,M3,M3; four matrices, containing weights sent from j
#   to i
# output: a list, where $W is the new wieght matrix, and
#   $P is the new transition matrix

# note: this function was edited to sample column j from M, rather than element
#   m_ij.  This change was incorporated to ensure that the total amount of
#   commodity produced in state j remains consistent with the original sampled
#   year.
generateP <- function(M1,M2,M3,M4){
  dim <- nrow(M1)
  # initialize matrices to 0
  W<-matrix(rep(0,dim*dim),nrow=dim)
  P<-matrix(rep(0,dim*dim),nrow=dim)
  
  # sampling process
  for (j in 1:dim){
    s<-sample.int(4,size=1,replace=TRUE)
    if (s==1){
      W[,j]=M1[,j]
    } else if (s==2){
      W[,j]=M2[,j]
    } else if (s==3){
      W[,j]=M3[,j]
    } else {
      W[,j]=M4[,j]
    }
  }
  
  # calculate the sum of the weights of each column
  w_sum<-apply(W,2,function(x) sum(x))
  # calculate the new probabilities for the transition matrix
  for(j in 1:dim){
    P[,j]<-sapply(W[,j],function(x) div_w(x,w_sum,j))
  }
  
  l<-list(W,P)
  names(l)<-c("W","P") #name the elements of the list
  return(l)
}

# a helper function for generateP, it returns the p_i,j probability
# input: x, element w_i,j from W; w, a vector containing the sum of
#   the weights in each column; pos, the column number
# output: p_i,j , the probability that a given shipment goes from
#   element j to element i
div_w <- function(x,w,pos){
  return(x/w[pos])
}


