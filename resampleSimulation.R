#

#Runs one step in the simulation. Takes a transition matrix with weights for each state to
#state connection. State_vec is the initial vector of infected states. This vector represents
#infection in states only as a binary variable: 1 is infected and 0 is not. Alpha is the
#establishment rate. The matrix is set up such that connections run from j to i.
output <- data.frame(state = 0, infection_status = 0, time_infected = 0, origin = 0)
weight_data <- c(p1, p2, p3, p4)

#Have to edit this based on which locations you're using.
states <- c("Alabama","Alaska","Arizona","Arkansas","California",
           "Colorado","Connecticut","Delaware","Florida","Georgia","Idaho",
           "Illinois","Indiana","Iowa","Kansas","Kentucky","Louisiana","Maine","Maryland",
           "Massachusetts","Michigan","Minnesota","Mississippi","Missouri","Montana",
           "Nebraska","Nevada","New Hampshire","New Jersey","New Mexico","New York",
           "North Carolina","North Dakota","Ohio","Oklahoma","Oregon","Pennsylvania",
           "Rhode Island","South Carolina","South Dakota","Tennessee","Texas","Utah",
           "Vermont","Virginia","Washington","Washington DC","West Virginia", "Wisconsin",
           "Wyoming")

stepOnce <- function(state_vec, transition_mat, alpha) {
        intro_vec <- transition_mat %*% state_vec  #creates vector of intro probabilities
        intro_vec <- alpha * intro_vec             #multiplies by establishment rate
        
        for(i in 1:length(intro_vec)) {
            if(runif(1) < intro_vec[i,]) {
                intro_vec[i,] <- 1
            }
            else {
                intro_vec[i,] <- 0
            }
        }
        
        new_vec <- intro_vec + state_vec           #sums the initial vector and the current one
        return(sanitize(new_vec))
}

#Give it a state_vec with any number of infected states. Splits it and runs 
splitVec <- function(state_vec) {
    for(i in 1:length(state_vec) {
        if(state_vec[i] == 1) {
            new_vec <- matrix(data = 0, ncol = ncol(state_vec), nrow = nrow(state_vec))
            new_vec[i] = 1
        }
    }
}

#Sanitize binary vector, sets all elements > 1 to 1.
sanitize <- function(vector) {
    for(i in 1:length(vector)) {            #Keeps the elements from being > 1
        if(vector[i,] > 1) {
            vector[i,] <- 1
        }
    }
    vector
}


recordInfo <- function(state_vec, orig_vec, time_step, data) {
    data$infection_status <- as.matrix(state_vec)
    data$time_infected[which(state_vec != orig_vec)] <- time_step
    data$origin[which(state_vec != orig_vec)] <- names[which(orig_vec == 1)]
    return(data)
}

#add, if steps = NA, while(sum(init) != length(init)) ie. run until all states are infected.
runSimulation <- function(init, steps, est_rate, 
                          df1 = p1, df2 = p2, df3 = p3, df4 = p4, names = states) {
    output <- data.frame(state = 0, infection_status = 0, time_infected = 0, origin = 0)
    
    for(step in 1:length(steps)) { #Run the simulation for a number of steps
        current_state = init                     #Keep track of new state_vector after each step
        indices <- which(init == 1)          #Find which indices to split vector into
        for(i in 1:sum(init)) {               #This loop splits the vector and runs a step for each
            vec <- rep(0, length(init))      
            vec[indices[i]] <- 1
            mat <- generateP(df1, df2, df3, df4)
            new_vec <- stepOnce(state_vec = vec, transition_mat = mat$P, alpha = est_rate)
            output <- recordInfo(state_vec = new_vec, orig_vec = vec, time_step = step, data = output)
            current_state = current_state + new_vec
        }
        init <- sanitize(current_state)
    }
    output
}