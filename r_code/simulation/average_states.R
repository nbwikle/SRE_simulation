# average_states.R
# created by: Nathan Wikle, 11 July 2015

# A function to calculate the average state vectors over many runs of 
# a simulation.
# Input: 1) m: a list of state vectors, outputted from runMany()
#        2) alpha: the apha*100% C.I. value
# Output: a list, containing the average state vectors for each time step,
#   the lower limit state vector for an alpha*100% C.I., and the upper limit
#   state vector for an alpha*100% C.I.
average_states <- function(m,alpha=0.95){
  runs <- length(m) # number of simulations
  iters <- length(m[[1]]) # number of timesteps per simulation
  sts <- length(m[[1]][[1]]) # number of states
  
  iter_ind <- c(1:iters) # indices representing the number of timesteps
  r_ind <- c(1:runs) # indices representing the number of simulations
  
  # determine the average state vectors
  avg_states <- lapply(iter_ind,function(x) find_avg_state(x,r_ind,m,sts))
  names(avg_states) <- paste("time",iter_ind,sep="") #name vectors by timestep
  
  # determine the lower limit of the alpha*100% confidence interval
  lower_ci <- lapply(iter_ind,function(x) find_lower_state(x,r_ind,m,sts,alpha))
  names(lower_ci) <- paste("time",iter_ind,sep="") #name vectors by timestep
  
  # determine the upper limit of the alpha*100% confidence interval
  upper_ci <- lapply(iter_ind,function(x) find_upper_state(x,r_ind,m,sts,alpha))
  names(upper_ci) <- paste("time",iter_ind,sep="") #name vectors by timestep
  
  # create a list, consisting of the average, lower, and upper c.i. state vectors
  output <- list(average,lower_ci,upper_ci)
  output
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


# A function to calculate a lower confidence limit state vector for a given time.
# Input: 1) t: the timestep of interest
#        2) r: a vector of indices indicating the number of simulations
#        3) lst: the list of ouptup state vectors
#        4) N: the number of states in the simulation
#        5) alpha: the alpha*100% C.I. value
# Output: A state vector, the lower limit of the alpha*100% C.I. for each
#     element of the state vectors
find_lower_state <- function(t,r,lst,N,alpha){
  # find all state vectors for time t
  new.l <- lapply(r,function(x) lst[[x]][t])
  n_state <- c(1:N)
  # now obtain lower alpha*100% c.i for state vectors
  lower <- sapply(n_state,function(x) lower_lim(x,r,new.l,alpha))
  lower
}

# A function to calculate the lower condidence limit for a specific element
# in a state vector.
# Input: 1) i: the state vector element index
#        2) r: a vector of indices indicating the number of simulations
#        3) new: a list containing a set of state vectors for a specific time
#        4) alpha: the apha*100% C.I. value
# Output: the lower limit of the alpha*100% C.I. for a given element
lower_lim <- function(i,r,new,alpha){
  t <- sapply(r,function(x) new[[x]][[1]][i])
  if (length(r) > 1){
    xbar <- mean(t)
    df <- length(r) - 1
    t_alpha <- qt(alpha,df)
    se <- sd(t)/sqrt(length(t))
    low.l <- mean - (t_alpha * se)
  } else {
    stop("only one simulation")
  }
  low.l
}


# A function to calculate a upper confidence limit state vector for a given time.
# Input: 1) t: the timestep of interest
#        2) r: a vector of indices indicating the number of simulations
#        3) lst: the list of ouptup state vectors
#        4) N: the number of states in the simulation
#        5) alpha: the alpha*100% C.I. value
# Output: A state vector, the upper limit of the alpha*100% C.I. for each
#     element of the state vectors
find_upper_state <- function(t,r,lst,N,alpha){
  # find all state vectors for time t
  new.l <- lapply(r,function(x) lst[[x]][t])
  n_state <- c(1:N)
  # now obtain lower alpha*100% c.i for state vectors
  upper <- sapply(n_state,function(x) upper_lim(x,r,new.l,alpha))
  upper
}

# A function to calculate the upper condidence limit for a specific element
# in a state vector.
# Input: 1) i: the state vector element index
#        2) r: a vector of indices indicating the number of simulations
#        3) new: a list containing a set of state vectors for a specific time
#        4) alpha: the apha*100% C.I. value
# Output: the upper limit of the alpha*100% C.I. for a given element
upper_lim <- function(i,r,new,alpha){
  t <- sapply(r,function(x) new[[x]][[1]][i])
  if (length(r) > 1){
    xbar <- mean(t)
    df <- length(r) - 1
    t_alpha <- qt(alpha,df)
    se <- sd(t)/sqrt(length(t))
    upper.l <- mean + (t_alpha * se)
  } else {
    stop("only one simulation")
  }
  upper.l
}
