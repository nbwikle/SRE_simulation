#markov_model.R
#Model for invasive species spread. Uses a state vector of decimals representing the "fractional
#establishment" of an invasive species in a certain region.

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

#function to run the simulation one time step forward
#ALgorithm is 1) Load state vector. 2) Populate data matrix 3) Matrix multiply 4) Record
#infections 5) Bump fully established elements to 1. 6) Repeat until last time step

runModel <- function(S_0, N, m1, m2, m3, m4, names = states, out = "state") {
    S_n <- S_0
    len = length(names)
    if(out == "state") {
        output1 = list(0)
    }
    #else if(out == "establishment") {
    #    output = data.frame(state = names, invaded = 
    #}
    if(out == "state") {
        output = list(0)
    }
    
    for(n in 1:N) {
        indices <- which(S_n >= 1)
        P <- generateP(m1, m2, m3, m4)$P
        
        if(out == "state") {
            proportions = matrix(nrow = len, ncol = len)
            for(j in 1:len) {
                proportions[, j] <- P[, j]*S_n[j]
            }
            output1[[n]] <- proportions
        }
        
        
        S_n <- P%*%S_n
        S_n[indices] <- 1
        
        if(out == "state") {
            output[[n]] <- S_n
        }
    }
    out <- list(output1, output)
    out
}

generateStats <- function(out) {
    len <- dim(out[[1]])[1]
    total <- matrix(nrow = len, ncol = len, data = 0)
    for(i in 1:length(out)) {
        total <- total + out[[i]]
    }
    total
}

generateRisk <- function(state, time, levels = c(0.1, 0.2, 0.5)) {
    totals <- list(0)
    for(i in 1:100) {
        totals[[i]] <- runModel()
    }
}
