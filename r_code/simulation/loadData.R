#Loading data. Give filename in the format "*mode*_*commodity*" replacing the
#points in asterisks with the mode of transport and commodity, respectively.

# 1. Fire ants = "trucks_agriprod". Start in Alabama
# 2. EAB = "trucks_logs". Start in Michigan
# 3. EAB = "trucks_mach-minerals-wood"
# 4. Wheat pests = "all_cerealgrains". Start in Arizona
# 5. Gypsy moths = "trucks_all". Start in Massachusetts

# this function loads the weight data into the global environment, along with a variable
# called "info". Info is a string with a description of the data that is loaded. Info is 
# set to equal the filename input after the data is loaded. (ie. info = "all_cerealgrains")

# Be sure to set info = something before running the function, or else an error will occur
# since it doesn't exist.

loadData <- function(filename, com = TRUE, mode = TRUE, run_anyway = FALSE) {
    if(!exists("info")) {
        info <- "empty"
    }
    if(info == filename & run_anyway != TRUE) {
        return("That dataset is already loaded.");
    }
    else {
        one <- paste("./data/1997FAF_", filename, ".csv", sep = "")
        two <- paste("./data/2002FAF_", filename, ".csv", sep = "")
        three <- paste("./data/2007FAF_", filename, ".csv", sep = "")
        four <- paste("./data/2012FAF_", filename, ".csv", sep = "")
        
        d1 <- read.csv(one, header = TRUE)
        d2 <- read.csv(two, header = TRUE)
        d3 <- read.csv(three, header = TRUE)
        d4 <- read.csv(four, header = TRUE)
        
        names(d1)<-c("origin","destination","commodity", "mode","weight","ton-mile","value1")
        names(d2)<-c("origin","destination","commodity", "mode","weight","ton-mile","value1")
        names(d3)<-c("origin","destination","commodity", "mode","weight","ton-mile","value1")
        names(d4)<-c("origin","destination","commodity", "mode","weight","ton-mile","value1")
        
        w1 <<- weight(d1, com = com, mode = mode)
        w2 <<- weight(d2, com = com, mode = mode)
        w3 <<- weight(d3, com = com, mode = mode)
        w4 <<- weight(d4, com = com, mode = mode)
        info <<- filename
    }
}