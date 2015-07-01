#Load shipping data (trucks, logs)
d1<-read.csv("1997FAF_all_trucks_comb.csv",header=TRUE)
d2<-read.csv("2002FAF_all_trucks_comb.csv",header=TRUE)
d3<-read.csv("2007FAF_all_trucks_comb.csv",header=TRUE)
d4<-read.csv("2012FAF_all_trucks_comb.csv",header=TRUE)

names(d1)<-c("origin","destination","mode","weight","ton-mile","value1")
names(d2)<-c("origin","destination","mode","weight","ton-mile","value1")
names(d3)<-c("origin","destination","mode","weight","ton-mile","value1")
names(d4)<-c("origin","destination","mode","weight","ton-mile","value1")

w1 <- weight(d1)
w2 <- weight(d2)
w3 <- weight(d3)
w4 <- weight(d4)

#Have to edit this based on which locations you're using.
states <- c("Alabama","Alaska","Arizona","Arkansas","California",
           "Colorado","Connecticut","Delaware","Florida","Georgia", "Hawaii", "Idaho",
           "Illinois","Indiana","Iowa","Kansas","Kentucky","Louisiana","Maine","Maryland",
           "Massachusetts","Michigan","Minnesota","Mississippi","Missouri","Montana",
           "Nebraska","Nevada","New Hampshire","New Jersey","New Mexico","New York",
           "North Carolina","North Dakota","Ohio","Oklahoma","Oregon","Pennsylvania",
           "Rhode Island","South Carolina","South Dakota","Tennessee","Texas","Utah",
           "Vermont","Virginia","Washington","Washington DC","West Virginia", "Wisconsin",
           "Wyoming")

#Runs one step in the simulation. Takes a transition matrix with weights for each state to
#state connection. State_vec is the initial vector of infected states. This vector represents
#infection in states only as a binary variable: 1 is infected and 0 is not. Alpha is the
#establishment rate. The matrix is set up such that connections run from j to i.
stepOnce <- function(state_vec, transition_mat, alpha) {
        intro_vec <- transition_mat %*% state_vec  #creates vector of intro probabilities
        intro_vec <- alpha * intro_vec             #multiplies by establishment rate
        
        for(i in 1:length(intro_vec)) {            #Coin flip for each probability in
            if(runif(1) < intro_vec[i,]) {         #intro_vec
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
    for(i in 1:length(vector)) {
        if(vector[i,] > 1) {
            vector[i,] <- 1
        }
    }
    vector
}

#Given a correctly formatted data frame (produced by runSimulation()), fills it with data.
#This function looks for differences between the state_vec(current) and orig_vec(initial) 
#and counts each significant difference as an infection event, and records it, the time step
#where it occurs, and the state the infection came from.
recordInfo <- function(state_vec, orig_vec, singular_vec, time_step, names, data) {
    indices <- which(state_vec != orig_vec)        #indices of differences;
    orig_index <- which(singular_vec == 1)         #does not count the origin of
    indices <- indices[indices != orig_index]      #infection as a significant difference
    orig <- names[orig_index]
    data$state <- eval(names)
    data$infection_status <- sanitize(state_vec)   #fills in with newest state vector
    
    if(length(indices) > 0) {                      #for each significant difference
        for(i in 1:length(indices)) {            
            index <- indices[i]                    #adds time step and state of origin
            if(!is.na(data$origin[index])) {
                data$origin[index] <- paste(data$origin[index], orig, sep = ", ")
            }
            else {
                data$origin[index] <- orig
            }
            
            if(!is.na(data$time_infected[index])) {
                data$time_infected[index] <- paste(data$time_infected[index], time_step, sep = ", ")
            }
            else {
                data$time_infected[index] <- time_step
            }
        }
    }
    return(data)
}

#This function is called given an initial vector of infection, the number of steps (years)
#to run, and the establishment rate. By default it uses the log, truck, FAF data from all 
#four years to resample and form the transition matrix, but you can optionally, give it 
#different data matrices to sample from. If you do, and the dimensions are different, you must
#give it a fitting names vector (same number of rows as your data matrix).
runSimulation <- function(init, steps, est_rate, 
                          df1 = w1, df2 = w2, df3 = w3, df4 = w4, names = states) {
    cat("", file = "output.csv", append = FALSE)
    len <- length(init)
    orig_indices <- which(init == 1)
    
    #creates the output data frame
    output <- data.frame(state = rep(NA, len), infection_status = rep(NA, len),
                         time_infected = rep(NA, len), origin = rep(NA, len))
    
    for(step in 1:steps) {                    #Run the simulation for a number of steps
        current_state = init                  #Keep track of new state_vector after each step
        indices <- which(init == 1)           #Find which indices to split vector into
        for(i in 1:sum(init)) {               #This loop splits the vector and runs a step for each
            temp_vec <- rep(0, length(init))      
            temp_vec[indices[i]] <- 1         #Generates the split vector of the ith iteration
        
            mat <- generateP(df1, df2, df3, df4)$P       #Generates the resampled matrix
            
            new_temp_vec <- stepOnce(state_vec = temp_vec, transition_mat = mat, 
                                     alpha = est_rate)       #Steps once with the split vector
            current_state <- current_state + new_temp_vec    #Updates the current state vector
            output <- recordInfo(state_vec = current_state, orig_vec = init, 
                                 singular_vec = temp_vec, time_step = step, names = names,
                                 data = output)              #Records data at each iteration
            lines <- paste(output$origin, collapse = ", ")
            cat(lines, file = "output.csv", append = TRUE, sep = "\n")
            current_state <- sanitize(current_state)         #Update the current state vector
            init <- current_state                            #Update the initial vector
        }
    }
    for(i in 1:length(orig_indices)) {
        index <- orig_indices[i]
        output$time_infected[index] <- NA
        output$origin[index] <- NA
    }
    output
}

#Truncates the output df of runSimulation() to only include initial infection (and multiple
#simultaneous initial infections)
truncate <- function(df) {
    for(i in 1:nrow(df)) {                       #Go through and check each row
        infection <- df$time_infected[i]     
        
        #If there are multiple reinfections, the data is in the form "x, y, z...". This part 
        #replaces that string with the minimum integer value in it.
        #It also then changes "origin" column correspondingly, ie. gets rid of all
        #states except the ones that were a part of the initial infection.
        if(class(infection) == "character" & !is.na(infection)) {
            infection_times <- unlist(strsplit(infection, split = ", "))
            first <- min(as.numeric(infection_times))
            df$time_infected[i] <- first
            counts <- sum(infection_times == first)
            
            origin_states <- unlist(strsplit(df$origin[i], split = ", "))
            first_states <- origin_states[1:counts]
            df$origin[i] <- paste(first_states, collapse = ", ")
        }
    }
    df
}

#This is a test
testnames <- c("alabama", "arkansas", "mars")
test1 <- p1[1:3,1:3]
test2 <- p2[1:3,1:3]
test3 <- p3[1:3,1:3]
test4 <- p4[1:3,1:3]
runSimulation(initial, 10, 0.5, test1, test2, test3, test4, testnames)
runSimulation(init_ial, 20, 0.2, w1, w2, w3, w4)
