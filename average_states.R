# average_states.R
# created by: Nathan Wikle, 11 July 2015

# A function to calculate the average state vectors over many runs of 
# a simulation.
# Input: 1) m: a list of state vectors, outputted from runMany()
# Output: average state vectors for each time step
average_states <- function(m){
  runs <- length(m) # number of simulations
  iters <- length(m[[1]]) # number of timesteps per simulation
  sts <- length(m[[1]][[1]]) # number of states
  
  iter_ind <- c(1:iters) # indices representing the number of timesteps
  r_ind <- c(1:runs) # indices representing the number of simulations
  
  # determine the average state vectors
  avg_states <- lapply(iter_ind,function(x) find_avg_state(x,r_ind,m,sts))
  names(avg_states) <- paste("time",iter_ind,sep="") #name vectors by timestep
  avg_states # return
}

# A function that calculates the average state vector for a given timestep.
# Input: 1) time: the timestep being considered
#        2) r: a vector of indices indicating the number of simulations
#        3) lst: the list of output state vectors
#        4) N: the number of states in the simulation
# Output: A state vector, the average of all state vectors at the given timestep
find_avg_state <- function(time,r,lst,N){
  # now have all first state vectors
  new.l <- lapply(r,function(x) lst[[x]][time]) 
  n_state <- c(1:N)
  # now have average of those state vectors
  pos <- sapply(n_state, function(x) one_average(x,r,new.l))
  pos
}

# A function to calculate a specific average element in a state vector.
# Input: 1) i: the state vector element index
#        2) r: a vector of indices indicating the number of simulations
#        3) new: a list containing a set of state vectors for a specific
#            time step
# Output: the mean of element i from the state vectors, a numeric
one_average <- function(i,r,new){
  t<-sapply(r,function(x) new[[x]][[1]][i]) # obtain all i elements
  s<-mean(t) # determine the mean
  s 
}

