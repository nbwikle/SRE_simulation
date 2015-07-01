# stats_helper_functions.R
# created by: Nathan Wikle, 30 June 2015
# last edited: 30 June 2015

# The following are helper functions for calc_stats(). They must 
# all be uploaded before calc_stats will correctly run.

# a function to give indices for multiple states. Used by inf_by().
# input: 1) v: a vector of states to find
#   2) s: a vector containing every possible state
# output: a vector of indices
factor_pos <- function(v,s){
  p<-vector()
  for(i in 1:length(v)){
    p<-cbind(p,which(s==v[i]))
  }
  as.vector(p)
}

# a function to determine which states were responsible for a 
# specific infection, given a dataframe. It is used by infected().
# input: 1) df: a dataframe from a single simulation output
#   2) pos: a position value to indicate the infected state
# output: 1) w: a dataframe containing states and whether they 
#   infected state pos (denoted by a 1)
inf_by <- function(df,pos) {
  # initialize the infected_by vector
  infect<-rep(0,nrow(df)) 
  # determine which states were responsible for infection
  s<-unlist(strsplit(as.character(df[pos,4]), split = ", "))
  # replace 0's by 1's if a state was responsible for infection
  infect[ factor_pos(s,df[,1]) ] <- 1 
  # create the output dataframe.
  w<-data.frame(state=df[,1],infected=infect)
  w
}

# a function to determine which states were responsible for a given
# state's infection, aggregated over a list of dataframes. It is used
# by calc_stat().
# input: 1) li: a list of dataframes
#   2) pos: the position of the infected state
# output: 1) a dataframe showing how often the input state was 
#   infected by other states
infected<-function(li,pos){
  # run inf_by() on all df's in li
  d<-lapply(li,function(x) inf_by(x,pos))
  # create a new dataframe to sum the results from d
  m<-data.frame(d[[1]])
  if (length(li) > 1) {
    for (i in 2:length(li)){
      m<-cbind(m,d[[i]]$infected)
    }
  }
  # sum the results from d
  infected_by<-apply(m[2:ncol(m)],1,function(x) sum(x))
  # calculate the percent each state is responsible for infection
  percent<-infected_by/sum(infected_by)
  # construct the output dataframe
  inf<-data.frame(state=d[[1]]$state,infected_by,percent)
  # order by times infected, decreasing
  inf<-inf[order(inf$infected_by,decreasing=TRUE),]
  inf
}

# a function to determine the indices of the states infected by 
# a particular state. Used by identify().
# input: 1) pattern: a vector of strings containing state names,
#   sometimes more than one at a time
#   2) state: the state of infection
# output: a vector of indices
indices<-function(pattern,state){
  c<-sapply(pattern,function(x) grepl(state,x))
  i<-which(c)
  i
}

# a function to identify which states were infected by state p, for
# a given dataframe. It is used by infects().
# input: 1) df: a dataframe, outputted from one simulation run
#   2) p: the position of the state in question
# output: a dataframe showing which states were infected by state p
identify<- function(df,p){
  states<-df$state # a list of the possible states
  s<-states[p] # state p
  c<-indices(df$origin,s) # indices of states infected by state p
  n<-rep(0,nrow(df)) # initialized vector to show infected states
  n[c]<-1 # add 1 for states infected by p
  d<-data.frame(state=df$state,infected=n) # create output df
  d 
}

# a function to determine which states were infected by the input
# state, given a list of dataframes from many_simulations(). This
# function is used by calc_stats.
# input: 1) li: a list of dataframes
#   2) pos: the position of the state transmitting the species
# output: a dataframe detailing which states were infected by 
#   the input state and the percentage of times that result occured.
infects<- function(li,pos){
  # run identify() on all df's in li
  d<-lapply(li,function(x) identify(x,pos))
  # create a new dataframe to sum the results from d
  m<-data.frame(d[[1]])
  if (length(li) > 1){
    for (i in 2:length(li)){
      m<-cbind(m,d[[i]]$infected)
    }
  }
  # sum the results from d to obtain # of times a state was infected
  times_infected<-apply(m[2:ncol(m)],1,function(x) sum(x))
  # calculate percents from times_infected
  percent_times<-times_infected/length(li)
  # create the output dataframe
  spread<-data.frame(state=d[[1]]$state,times_infected,percent_times)
  # order the output by times_infected, decreasing
  spread<-spread[order(spread$times_infected,decreasing=TRUE),]
  spread
}
