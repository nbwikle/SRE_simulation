#markov_model.R
#Model for invasive species spread. Uses a state vector of decimals representing the "fractional
#establishment" of an invasive species in a certain region.
library(ggplot2)
library(reshape2)

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
    if(out == "proportion" || out == "both") {
        out_proportions = list(0)
    }
    if(out == "state" || out == "both") {
        out_state = list(0)
    }
    
    for(n in 1:N) {
        indices <- which(S_n >= 1)
        P <- (generateP(m1, m2, m3, m4)$P) * norm_sim #Take out norm_sim to remove environ similarity matrix
        
        if(out == "proportion" || out == "both") {
            proportions = matrix(nrow = len, ncol = len)
            for(j in 1:len) {
                proportions[, j] <- P[, j]*S_n[j]
                proportions[, j] <- proportions[, j] / sum(proportions[, j])
            }
            out_proportions[[n]] <- proportions
        }
        
        S_n <- P%*%S_n
        S_n[indices] <- 1
        
        if(out == "state" || out == "both") {
            out_state[[n]] <- S_n
        }
    }
    if(out == "proportion") {
        out = out_proportions
    }
    else if(out == "state") {
        out = out_state
    }
    else if(out == "both") {
        out = list(proportions = out_proportions, state = out_state)
    }
    
    out
}

runGrowthModel <- function(S_0, N, m1, m2, m3, m4, names = states, out = "state", r_0) {
    S_n <- S_0
    len = length(names)
    if(out == "proportion" || out == "both") {
        out_proportions = list(0)
    }
    if(out == "state" || out == "both") {
        out_state = list(0)
    }
    
    for(n in 1:N) {
        indices <- which(S_n >= 1)
        P <- (generateP(m1, m2, m3, m4)$P) * norm_sim #Take out norm_sim to remove environ similarity matrix
        
        if(out == "proportion" || out == "both") {
            proportions = matrix(nrow = len, ncol = len)
            for(j in 1:len) {
                proportions[, j] <- P[, j]*S_n[j]
                proportions[, j] <- proportions[, j] / sum(proportions[, j])
            }
            out_proportions[[n]] <- proportions
        }
        
        S_n <- (1*S_n) / (S_n + (1 - S_n)*exp(-r_0*n))
        S_n <- P%*%S_n
        S_n[indices] <- 1
        
        if(out == "state" || out == "both") {
            out_state[[n]] <- S_n
        }
    }
    if(out == "proportion") {
        out = out_proportions
    }
    else if(out == "state") {
        out = out_state
    }
    else if(out == "both") {
        out = list(proportions = out_proportions, state = out_state)
    }
    
    out
}


runMany <- function(init, times, steps, m1, m2, m3, m4, names = states,
                    method = "state", growth = FALSE) {
    out = list(0)
    if(growth == TRUE) {
        for(i in 1:times) {
            out[[i]] <- runGrowthModel(S_0 = init, N = steps, m1 = m1, m2 = m2, m3 = m3, m4 = m4,
                                       names = names, out = method, r_0 = 0.01)
        }
    }
    else {
        for(i in 1:times) {
            out[[i]] <- runModel(S_0 = init, N = steps, m1 = m1, m2 = m2, m3 = m3, m4 = m4,
                                 names = names, out = method)
        }
    }
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

generateRisk <- function(data, state, levels = c(0.1, 0.2, 0.5)) {
    
    state_code <- which(states == state)
    time_steps <- length(data[[1]])
    num_levels <- length(levels)
    num_runs <- length(data)
    output_data <- list(0)
    
    for(i in 1:num_levels) {
        
        level = levels[i]
        prob <- numeric(time_steps)
        
        for(step in 1:time_steps) {
            
            count = 0
            
            for(run in 1:num_runs) {
                current_run <- data[[run]][[step]][state_code]
                if(current_run > level) {
                    count = count + 1
                }
            }
            
            prob[step] <- count / num_runs
        }
        df <- data.frame(probability = prob, time = 1:time_steps, level = as.factor(level),
                         state = state)
        output_data[[i]] <- df
    }
    output_data
}

plotRisk <- function(risk_data) {
    levels = length(risk_data)
    df <- do.call("rbind", risk_data)
    state = levels(df$state)
    title <- paste("Invasion Risk Over Time for", state)
    plot <- ggplot(df, aes(time, probability, col = level))
    plot <- plot + geom_smooth() + ggtitle(title)
    plot <- plot + xlab("Years") + ylab("Probability[Establishment > Level]")
    plot
}

createMaps <- function(init_state, years, simulations, growth = FALSE) {
    initial <- rep(0, 51)
    index <- which(states == init_state)
    initial[index] <- 1
    
    if(growth == TRUE) {
        state_data <- runMany(initial, simulations, years, w1, w2, w3, w4, growth = TRUE)
    }
    else {
        state_data <- runMany(initial, simulations, years, w1, w2, w3, w4)
    }
    
    average_data <- average_states(state_data)
    filename <- paste(init_state, "maps.pdf", sep = "_")
    many_maps(average_data, filename, initial = FALSE)
}