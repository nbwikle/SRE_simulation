# calc_st_area.R

# A function to calculate the area, in square miles, for a given state.
# Input: 1) state: the state of interest. Note, it can be either the name as a 
#            string, the abbreviation as a string, or a numeric (representing the 
#            alphabetical order of the state)
#        2) water: a boolean, default to TRUE. Determines whether to include water
#            in the area calculation
# Output: a numeric, giving the area in square miles for the input state
calc_st_area <- function(state,fips=FALSE,water=TRUE){
  require(foreign)
  
  wd <- c("~/SRE_simulation/data")
  setwd(wd)
  stFIPS <- read.csv("stateFIPS.csv",header=TRUE)
  ctyArea <- read.dbf("tl_2014_us_county.dbf")
  ctyArea[,1]<-as.numeric(levels(ctyArea[,1]))[ctyArea[,1]]
  ctyArea[,2]<-as.numeric(levels(ctyArea[,2]))[ctyArea[,2]]
  ctyArea[,4]<-as.numeric(levels(ctyArea[,4]))[ctyArea[,4]]
  
  if(!fips){
    ind <- which((as.character(stFIPS$State)==state)|
                 (as.character(stFIPS$ST)==state)|
                 (as.numeric(stFIPS$State)==state))
  
    st.fips <- stFIPS$STATEFP[ind]
  } else {
    st.fips <- state
  }
  ctys <- ctyArea[which(ctyArea$STATEFP==st.fips),]
  
  area.sq.m <- sum(ctys$ALAND)
  if (water){
    area.sq.m <- area.sq.m + sum(ctys$AWATER)
  }
  
  area.sq.mi <- area.sq.m/2589988.110336
  area <- area.sq.mi
  area
}
