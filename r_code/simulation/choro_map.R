# choro_map.R
# created by: Nathan Wikle, 10 July 2015

# A function to plot a choropleth map, given an input state vector.
# Note that this function is for states only. If higher geographic
# resolution is needed, a different function must be used.
# Input: sv: a vectors of states, with invasion percentage values.
#        t: a string, used to make the title of the plot.
# Output: a choropleth map, created using ggplot2
choro_map <- function(sv, t) {
  require(ggplot2)
  require(maps)
  # a list of states, all lower case
  state<-c("alabama","alaska","arizona","arkansas","california",
           "colorado","connecticut","delaware","florida","georgia", "hawaii", "idaho",
           "illinois","indiana","iowa","kansas","kentucky","louisiana","maine","maryland",
           "massachusetts","michigan","minnesota","mississippi","missouri","montana",
           "nebraska","nevada","new hampshire","new jersey","new mexico","new york",
           "north carolina","north dakota","ohio","oklahoma","oregon","pennsylvania",
           "rhode island","south carolina","south dakota","tennessee","texas","utah",
           "vermont","virginia","washington","district of columbia","west virginia", "wisconsin",
           "wyoming")
  d <- data.frame(state,as.vector(sv)) # create a df of states and their respective values
  names(d) <- c("state","value")
  d$region <- d$state  # need to merge the df with the map coordinates
  
  all_states <- map_data("state") # obtain map coordinates for each state
  
  
  newD <- merge(all_states,d,by="region")  # merge the values and the coordinates
  newD <- newD[newD$region!="district of columbia",]
  p <- ggplot() # create a ggplot
  # populate the states, colors are variations on red
  p <- p + geom_polygon(data=newD,aes(x=long,y=lat,group=group,
            fill=value),color="white") + 
            scale_fill_continuous(low="thistle2",high="darkred",guide="colorbar") 
  # add titles to the graph and the legend
  p <- p + theme_bw() + labs(fill="Intensity of Invasion",title=t,x="",y="")
  # replace background
  p <- p + scale_y_continuous(breaks=c()) + scale_x_continuous(breaks=c()) + theme(panel.border = element_blank())
  # return ggplot object
  p
}

