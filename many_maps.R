# many_maps.R
# created by: Nathan Wikle, 10 July 2015

# A function to generate many choropleth maps, given a list of state vectors
# Input: 1) l: a list of state vectors, as given by run_model
#        2) pdfName: a file name, where the output maps will be stored. Include the .pdf extension
#        3) w: width of the pdf, in inches. Default it 10.77
#        4) h: height of the pdf, in inches. Default it 6.09
#        5) initial: a boolean set to TRUE, specifying if the list of state vectors includes
#            the initial input or not
# Output: 1) A pdf, stored in the current working directory, under pdfName
#         2) A list of ggplot objects
many_maps <- function(l,pdfName,w=10.77,h=6.09,initial=TRUE){
  N <- length(l) # number of time steps (+1 if initial step is included)
  if (initial) { # determine appropriate time steps to label
    t <- c(0:(N-1)) # vector consisting of number of plots to generate
  } else {
    t <- c(1:N)
  }
  # generate the ggplots, using make_map helper function (and choro_map)
  p <- lapply(t,function(x) make_map(l,x,initial))
  # print to pdf
  pdf(file=pdfName,width=w,height=h)
  for(i in 1:length(p)){
    plot(p[[i]])
  }
  dev.off()
  p # output list of ggplots
}

# A helper function to make a choropleth map, given a list of state
# vectors and an index.  Used by many_maps(), with a dependency on choro_map().
# Input: 1) l: a list of state vectors
#        2) t: an index to determine the state vector and year
#        3) initial: a boolean set to TRUE, specifying if the list of state vectors includes
#            the initial input or not
# Output: 1) a choropleth map, given as a ggplot object
make_map <- function(l,t,initial){
  if (initial){
    v<-l[[(t+1)]]
  } else {
    v<-l[[t]]
  }
  title <- paste("Year:",t) # generate the title
  g<-choro_map(v,title) # generate choropleth map
  g # output ggplot object
}


