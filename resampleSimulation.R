#

#Runs one step in the simulation. Takes a transition matrix with weights for each state to
#state connection. State_vec is the initial vector of infected states. This vector represents
#infection in states only as a binary variable: 1 is infected and 0 is not. Alpha is the
#establishment rate. The matrix is set up such that connections run from j to i.
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

#Sanitize binary vector, sets all elements > 1 to 1.
sanitize <- function(vector) {
    for(i in 1:length(vector)) {            #Keeps the elements from being > 1
        if(vector[i,] > 1) {
            vector[i,] <- 1
        }
    }
    vector
}


recordInfo <- function(state_vec, orig_vec, singular_vec, time_step, names, data) {
    data$state <- eval(names)
    data$infection_status <- state_vec
    data$time_infected[which(state_vec != orig_vec)] <- time_step
    data$origin[which(state_vec != orig_vec)] <- names[which(singular_vec == 1)]
    return(data)
}

#add, if steps = NA, while(sum(init) != length(init)) ie. run until all states are infected.
#same time step, record both
#efficiency
runSimulation <- function(init, steps, est_rate, 
                          df1 = p1, df2 = p2, df3 = p3, df4 = p4, names = states) {
    cat("", file = "output.txt", append = FALSE)
    len <- length(init)
    output <- data.frame(state = rep(NA, len), infection_status = rep(NA, len),
                         time_infected = rep(NA, len), origin = rep(NA, len))
    
    for(step in 1:steps) { #Run the simulation for a number of steps
        current_state = init                     #Keep track of new state_vector after each step
        indices <- which(init == 1)          #Find which indices to split vector into
        for(i in 1:sum(init)) {               #This loop splits the vector and runs a step for each
            temp_vec <- rep(0, length(init))      
            temp_vec[indices[i]] <- 1
            mat <- generateP(df1, df2, df3, df4)
            new_temp_vec <- stepOnce(state_vec = temp_vec, transition_mat = mat$P, 
                                     alpha = est_rate)
            current_state <- sanitize(current_state + new_temp_vec)
            output <- recordInfo(state_vec = current_state, orig_vec = init, 
                                 singular_vec = temp_vec, time_step = step, names = names,
                                 data = output)
            lines <- paste(output$origin, collapse = ", ")
            cat(lines, file = "output.txt", append = TRUE, sep = "\n")
            init <- current_state
        }
    }
    output
}