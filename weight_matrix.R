# a function to create a proper weight matrix, where w_i,j is the 
#   amount of weight shipped from area j to area i.
# input: 1) data: assumed to be in the same format as the csv downloaded
#   from FAF;
#   2) com and mode: default is true, specifies if a specific commodity 
#   has been selected. (This is needed for formating.  If all 
#   commodities or all modes of transportation have been selected,
#   make these false)  
#   3) w: specify if the weight should be tons (0), ton-miles (1),
#   or millions of dollars (2)
# output: a matrix W, consisting of weights from j to i
weight <- function(data,com=TRUE,mode=TRUE,w=0){
  # determine the position of the weights
  p<-5 + w
  if (!com) p <- v-1
  if (!mode) p <- v-1
  
  # determine the dimensions of the matrix
  a<-length(unique(data[,1]))
  b<-length(unique(data[,2]))
  if( a >= b){
    dim <- a
  } else {
    dim <- b
  }
  # initialize the matrix
  W <- matrix(rep(0,dim*dim),nrow=dim)
  # place weights in the appropriate places
  for(i in 1:nrow(data)){
    W[as.numeric(data[i,2]),as.numeric(data[i,1])]<-data[i,p]
  }
  return(W)
}

#a function to remove HI from a given matrix P
removeHI<-function(P){
  return(P[-(11),-(11)])
}

>>>>>>> 4a80029b93d09fd44e95ac4c4d56f495bad7bb46
