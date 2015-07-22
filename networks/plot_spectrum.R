# plot_spectrum.R
# Created by Nathan Wikle, 21 July 2015

# A function to plot the clustering spectrum and degree correlation spectrum
# for a given graph.
# Input: 1) stats: network statistics, as outputted by network_char()
#        2) weighted: a boolean specifying if the statistics are from a 
#             weighted graph. If so, weighted length plots are also returned.
#        3) line: a boolean, specifiying whether lines should be fit to
#             scatterplots.
#        4) linetype: gives the method to be used in the linefitting. See 
#             ggplot2 for more details.
# Output: ggplot objects for the clustering and degree correlation spectrums
plot_spectrum <- function(stats,weighted=TRUE,line=TRUE,linetype="loess"){

  res <- list() # output list
  
  #start with C's
  #C.k
  C.k <- find_spectrum(stats$k,stats$C.i)
  C.k <- C.k[!is.na(C.k$avg),] 
  plot.C.k <- ggplot(C.k,aes(x=degree,y=avg)) + geom_point() + 
    scale_x_log10(breaks=c(1,5,10,20,30,40,100),
                  labels=c("1","5","10","20","30","40","100")) + 
    scale_y_log10(breaks=c(.01,0.05,.1,.15,.2,.25,.3,1),
                  labels=c("0.01","0.05","0.1","0.15","0.2","0.25","0.3","1")) + 
    xlab(expression(paste("degree (",italic(k),")"))) +
    ylab(expression(italic("C(k)")))
  
  if (line){
    plot.C.k <- plot.C.k + geom_smooth(method=linetype,se=FALSE) 
  }
  res$plot.C.k <- plot.C.k
  
  #C.w.k
  if (weighted){
    C.w.k <- find_spectrum(stats$k,stats$C.i.w)
    C.w.k <- C.w.k[!is.na(C.w.k$avg),]
    plot.C.w.k <- ggplot(C.w.k,aes(x=degree,y=avg)) + geom_point() + 
      scale_x_log10(breaks=c(1,5,10,20,30,40,100),
                    labels=c("1","5","10","20","30","40","100")) + 
      scale_y_log10(breaks=c(.01,0.05,.1,.15,.2,.25,.3,1),
                    labels=c("0.01","0.05","0.1","0.15","0.2","0.25","0.3","1")) + 
      xlab(expression(paste("degree (",italic(k),")"))) +
      ylab(expression(italic(paste(C^{"w"},"(k)"))))
      
    if (line){
      plot.C.w.k <- plot.C.w.k + geom_smooth(method=linetype,se=FALSE) 
    }
    res$plot.C.w.k <- plot.C.w.k
  }
  
  # now plot k.nn's
  knnk <- find_spectrum(stats$k,stats$k.nn)
  knnk <- knnk[!is.na(knnk$avg),]
  plot.knnk <- ggplot(knnk,aes(x=degree,y=avg)) + geom_point() + 
    scale_x_log10(breaks=c(1,5,10,20,30,40,100),
                  labels=c("1","5","10","20","30","40","100")) + 
    scale_y_log10(breaks=c(.01,0.05,.1,.15,.2,.25,.3,1),
                  labels=c("0.01","0.05","0.1","0.15","0.2","0.25","0.3","1")) + 
    xlab(expression(paste("degree (",italic(k),")"))) +
    ylab(expression(italic(paste(k["nn"],"(k)"))))
    
  if (line){
    plot.knnk <- plot.knnk + geom_smooth(method=linetype,se=FALSE) 
  }
  res$plot.knnk <- plot.knnk
  
  if (weighted){
    # k.nn.w
    knnk.w <- find_spectrum(stats$k,stats$k.nn.w)
    knnk.w <- knnk.w[!is.na(knnk.w$avg),]
    plot.knnk.w <- ggplot(knnk.w,aes(x=degree,y=avg)) + geom_point() + 
      scale_x_log10(breaks=c(1,5,10,20,30,40,100),
                    labels=c("1","5","10","20","30","40","100")) + 
      scale_y_log10(breaks=c(.01,0.05,.1,.15,.2,.25,.3,1),
                    labels=c("0.01","0.05","0.1","0.15","0.2","0.25","0.3","1")) + 
      xlab(expression(paste("degree (",italic(k),")"))) +
      ylab(expression(italic(paste(k["nn"]^{"w"},"(k)"))))
    
    # k.nn.w.in
    knnk.w.in <- find_spectrum(stats$k,stats$k.nn.w.in)
    knnk.w.in <- knnk.w.in[!is.na(knnk.w.in$avg),]
    plot.knnk.w.in <- ggplot(knnk.w.in,aes(x=degree,y=avg)) + geom_point() + 
      scale_x_log10(breaks=c(1,5,10,20,30,40,100),
                    labels=c("1","5","10","20","30","40","100")) + 
      scale_y_log10(breaks=c(.01,0.05,.1,.15,.2,.25,.3,1),
                    labels=c("0.01","0.05","0.1","0.15","0.2","0.25","0.3","1")) + 
      xlab(expression(paste("degree (",italic(k),")"))) +
      ylab(expression(italic(paste(k["nn"]^{"w"},"(k)"["in"]))))
    
    # k.nn.w.out
    knnk.w.out <- find_spectrum(stats$k,stats$k.nn.w.out)
    knnk.w.out <- knnk.w.out[!is.na(knnk.w.out$avg),]
    plot.knnk.w.out <- ggplot(knnk.w.out,aes(x=degree,y=avg)) + geom_point() + 
      scale_x_log10(breaks=c(1,5,10,20,30,40,100),
                    labels=c("1","5","10","20","30","40","100")) + 
      scale_y_log10(breaks=c(.01,0.05,.1,.15,.2,.25,.3,1),
                    labels=c("0.01","0.05","0.1","0.15","0.2","0.25","0.3","1")) + 
      xlab(expression(paste("degree (",italic(k),")"))) +
      ylab(expression(italic(paste(k["nn"]^{"w"},"(k)"["out"]))))
    
    if (line){
      plot.knnk.w <- plot.knnk.w + geom_smooth(method=linetype,se=FALSE)
      plot.knnk.w.in <- plot.knnk.w.in + geom_smooth(method=linetype,se=FALSE)
      plot.knnk.w.out <- plot.knnk.w.out + geom_smooth(method=linetype,se=FALSE)
    }
    res$plot.knnk.w <- plot.knnk.w
    res$plot.knnk.w.in <- plot.knnk.w.in
    res$plot.knnk.w.out <- plot.knnk.w.out
  }
  res
}


# A function to find either the degree correlation or clustering
# spectrum for a real-world network.
# Input: 1) k: a vector with degrees for each vertex of the graph
#        2) spec: either the avg. nearest neighbor degrees, or the 
#             clustering coefficents 
# Output: a dataframe, with degree and the average spectrum for the 
#           given statistic
find_spectrum <- function(k,spec){
  avg <- rep(0,max(k))
  spec[!is.finite(spec)]<-0
  for (i in 1:max(k)){
    t <- which(k==i)
    if (length(t)>0){
      v <- spec[t]
      avg[i] <- mean(v)
    } else {
      avg[i] <- NA
    }
  }
  degree <- c(1:max(k))
  spec.df <- data.frame(degree,avg)
  spec.df
}
