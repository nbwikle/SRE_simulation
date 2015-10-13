# many_simulations.R
# created by: Nathan Wikle, 30 June 2015
# last edited: 30 June 2015
# Update 10/10/15: THIS IS FOR AN OLD MODEL IMPLEMENTATION USING COIN FLIPS

# a function to perform many simulations, with statistical output
# included.
# input: 1) t: number of times teh simulation should be performed
#   2) initial: the initial state vector for the population
#   3) stps: number of time steps each simulation should be performed
#   4) est: the establishment rate for the population
#   5) w1,w2,w3,w4: weight matrices from four seperate years
#   6) n: a vector of state names (default = states)
#   7) trun: a boolean to determine if the states of infection
#   should be truncated. Default to TRUE. If set to false, statistics
#   will not be calculated.
# output: a list, consisting of: 
#   1) df: a list of data frames from each run of the simulation
#   2) statistics: statistics calculated on the resulting output
#   3) initial_infection: the state(s) that were originally infected
many_simulations <- function(t,initial,stps,est,w1,w2,w3,w4,n=states,trunc=TRUE) {
  df<-list(rep(data.frame(),times=t)) # a list to contain the dataframes
  p<-which(initial==1) # indices of infected state(s)
  init <- n[p] # vector of infected state names
  for(i in 1:t){
    # run simulation t times
    if (trunc){
      s<-truncate(runSimulation(initial,stps,est,df1=w1,df2=w2,df3=w3,df4=w4,names=n)) 
    } else {
      s<-runSimulation(initial,stps,est,df1=w1,df2=w2,df3=w3,df4=w4,names=n)
    }
    # populate data frames
    df[[i]]<-s
  }
  if (trunc){
    statistics<-calc_stats(df) # calculate statistics on the dataframe
    output<-list(df,statistics,init)
    names(output)<-c("df","statistics","initial_infection")
  } else {
    output<-list(df,init)
    names(output)<-c("df","initial_infection")
  }
  output # return output list
}
