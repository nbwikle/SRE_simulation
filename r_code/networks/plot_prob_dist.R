# plot_prob_dist.R
# Created by Nathan Wikle, 21 July 2015

# A function to plot the densities and probability distributions of the following
# statistics: degree, betweenness, weight, and strength.
# Input: 1) stats: list of statistics, as outputted by network_char
#        2) output: a string determining which statistics to plot. Possibilities
#             are: all, degree, betweenness, weight, and strength
#        3) weighted: a boolean specifying if the statistics are from a 
#             weighted graph. If so, weighted length plots are also returned.
#        4) line: a boolean, specifiying whether lines should be fit to
#             scatterplots.
#        5) linetype: gives the method to be used in the linefitting. See 
#             ggplot2 for more details.
# Output: a list of plots, as specified by output.
plot_prob_dist <- function(stats,output=c("all","degree","betweenness","weight","strength"),
                           weighted=TRUE,line=TRUE,linetype="loess"){
  res <- list() # list to contain plot results
  # plot P(k)
  # first, plot density plots
  # plot of k density
  if ((output=="degree") | (output=="all")){
    k.df <- as.data.frame(stats$k)
    names(k.df) <- "k"
    density.k <- ggplot(k.df,aes(k))+geom_density(fill="grey40") + 
    xlab(expression(paste("degree (",italic(k),")")))
  res$density.k <- density.k
  
  # plot of k.in density
    k.in.df <- as.data.frame(stats$k.in)
    names(k.in.df) <- "k.in"
    density.k.in <- ggplot(k.in.df,aes(k.in))+geom_density(fill="grey40") +
      xlab(expression(paste("in degree (",italic(k["in"]),")")))  
  res$density.k.in <- density.k.in
  
  # plot of k.out density
    k.out.df <- as.data.frame(stats$k.out)
    names(k.out.df) <- "k.out"
    density.k.out <- ggplot(k.out.df,aes(k.out))+geom_density(fill="grey40") +
    xlab(expression(paste("out degree (",italic(k["out"]),")")))
  res$density.k.out <- density.k.out
  
  # plot of all three overlapping
    comb.k <- data.frame(k=stats$k,type=rep(1,length(stats$k)))
    comb.k.in <- data.frame(k=stats$k.in,type=rep(2,length(stats$k.in)))
    comb.k.out <- data.frame(k=stats$k.out,type=rep(3,length(stats$k.out)))
    all.k <- rbind(comb.k,comb.k.in,comb.k.out)
    all.k.density <- ggplot(all.k,aes(group=factor(type))) +
      geom_density(alpha=0.2,color=NA,aes(x=k,fill=factor(type))) +
      geom_density(alpha=0.001,aes(x=k,fill=factor(type)),show_guide=FALSE) +
      xlab("degree") +
      scale_fill_manual(name = "Degree",values=c("red","green","blue"),
                        labels = c("1" = expression(italic(k)),
                                   "2" = expression(italic(k["in"])),
                                   "3"= expression(italic(k["out"]))))
  res$all.k.density <- all.k.density
  
  # calc P(k) for all degrees
    P.k <- graph_distribution(stats$k,all=FALSE)
    P.k.in <- graph_distribution(stats$k.in,all=FALSE)
    P.k.out <- graph_distribution(stats$k.out,all=FALSE)
  
  # plot P(k)
    plot.k <- ggplot(P.k[which(P.k$dist!=0),],aes(x=points,y=dist)) + geom_point() + 
      scale_x_log10(breaks=c(1,5,10,20,40,100),labels=c("1","5","10","20","40","100")) + 
      scale_y_log10(breaks=c(.01,0.05,.1,.2,1),labels=c("0.01","0.05","0.1","0.2","1")) + 
      xlab(expression(paste("degree (",italic(k),")"))) +
      ylab(expression(italic("P(k)")))
  
  # plot P(k.in) 
    plot.k.in <- ggplot(P.k.in[which(P.k.in$dist!=0),],aes(x=points,y=dist)) + geom_point() + 
      scale_x_log10(breaks=c(1,5,10,20,40,100),labels=c("1","5","10","20","40","100")) + 
      scale_y_log10(breaks=c(.01,0.05,.1,.2,1),labels=c("0.01","0.05","0.1","0.2","1")) + 
      xlab(expression(paste("in degree (",italic(k["in"]),")"))) + 
      ylab(expression(italic(paste("P(",k["in"],")"))))
  
  # plot P(k.out)
    plot.k.out <- ggplot(P.k.out[which(P.k.out$dist!=0),],aes(x=points,y=dist)) + geom_point() + 
      scale_x_log10(breaks=c(1,5,10,20,40,100),labels=c("1","5","10","20","40","100")) + 
      scale_y_log10(breaks=c(.01,0.05,.1,.2,1),labels=c("0.01","0.05","0.1","0.2","1")) + 
      xlab(expression(paste("out degree (",italic(k["out"]),")"))) + 
      ylab(expression(italic(paste("P(",k["out"],")"))))
    
    if (line){
      plot.k <- plot.k + geom_smooth(method=linetype,se=FALSE) 
      plot.k.in <- plot.k.in + geom_smooth(method=linetype,se=FALSE)
      plot.k.out <- plot.k.out + geom_smooth(method=linetype,se=FALSE)
    }
  res$plot.k <- plot.k
  res$plot.k.in <- plot.k.in
  res$plot.k.out <- plot.k.out
  }
  if ((output=="betweenness") | (output=="all")){
  
  # plot P(b)
  # first, plot density plots
  # betweenness (undirected)
    b.un.df <- as.data.frame(stats$b.un)
    names(b.un.df) <- "b"
    density.b <- ggplot(b.un.df,aes(b))+geom_density(fill="grey40") +
    xlab(expression(paste("undirected (",italic(b),")")))
  res$density.b <- density.b
  
  # betweenness (directed)
    b.dir.df <- as.data.frame(stats$b.dir)
    names(b.dir.df) <- "b.dir"
    density.b.dir <- ggplot(b.dir.df,aes(b.dir))+geom_density(fill="grey40") + 
    xlab(expression(paste("directed (",italic(b["dir"]),")")))
  res$density.b.dir <- density.b.dir
  
  # plot of two unweighted  overlapping
    comb.b <- data.frame(b=stats$b.un,type=rep(1,length(stats$b.un)))
    comb.b.dir <- data.frame(b=stats$b.dir,type=rep(2,length(stats$b.dir)))
    comb.b.w <- data.frame(b=stats$b.un.w,type=rep(3,length(stats$b.un.w)))
    comb.b.w.dir <- data.frame(b=stats$b.dir.w,type=rep(4,length(stats$b.dir.w)))
    unw.b <- rbind(comb.b,comb.b.dir)
    unw.b.density <- ggplot(unw.b,aes(group=factor(type))) +
      geom_density(alpha=0.3,color=NA,aes(x=b,fill=factor(type))) +
      geom_density(alpha=0.001,aes(x=b,fill=factor(type)),show_guide=FALSE) +
      xlab("betweenness")+
      scale_fill_manual(name = "betweenness",values=c("orangered","blue"),
                        labels = c("1" = expression(italic(b)),
                                   "2" = expression(italic(b["dir"]))))  
  res$unw.b.density <- unw.b.density
    
    if (weighted){
    # weighted betweenness (undirected)
      b.w.df <- as.data.frame(stats$b.un.w)
      names(b.w.df) <- "b.w"
      density.b.w <- ggplot(b.w.df,aes(b.w))+geom_density(fill="grey40") + 
        xlab(expression(paste("undirected (",italic(b^{"w"}),")")))
    res$density.b.w <- density.b.w
    
    # weighted betweenness (directed)
      b.w.dir.df <- as.data.frame(stats$b.dir.w)
      names(b.w.dir.df) <- "b.w.dir"
      density.b.w.dir <- ggplot(b.w.dir.df,aes(b.w.dir))+geom_density(fill="grey40") +
        xlab(expression(paste("directed (",italic(b["dir"]^{"w"}),")")))
    res$density.b.w.dir <- density.b.w.dir
    
    # plot of two weighted overlapping
      w.b <- rbind(comb.b.w,comb.b.w.dir)
      w.b.density <- ggplot(w.b,aes(group=factor(type))) +
        geom_density(alpha=0.3,color=NA,aes(x=b,fill=factor(type))) +
        geom_density(alpha=0.001,aes(x=b,fill=factor(type)),show_guide=FALSE) +
        xlab("weighted betweenness")+
        scale_fill_manual(name = "betweenness",values=c("magenta4","chartreuse4"),
                          labels = c("3" = expression(italic(b^{"w"})), 
                                     "4" = expression(italic(b["dir"]^{"w"}))))
    res$w.b.density <- w.b.density
    }
  
  # calc P(b) for all b
    P.b <- graph_distribution(stats$b.un,all=FALSE)
    P.b.dir <- graph_distribution(stats$b.dir,all=FALSE)
  
  # plot P(b)
    plot.b <- ggplot(P.b[which(P.b$dist!=0),],aes(x=points,y=dist)) + geom_point() + 
      scale_x_log10(breaks=c(1,5,10,20,40,100),labels=c("1","5","10","20","40","100")) + 
      scale_y_log10(breaks=c(.01,0.05,.1,.2,1),labels=c("0.01","0.05","0.1","0.2","1")) + 
      xlab(expression(paste("undirected (",italic(b),")"))) +
      ylab(expression(italic(paste("P(",b,")"))))
  
  # plot P(b.dir)
    plot.b.dir <- ggplot(P.b.dir[which(P.b.dir$dist!=0),],aes(x=points,y=dist)) + geom_point() + 
      scale_x_log10(breaks=c(1,5,10,20,40,100),labels=c("1","5","10","20","40","100")) + 
      scale_y_log10(breaks=c(.01,0.05,.1,.2,1),labels=c("0.01","0.05","0.1","0.2","1")) + 
      xlab(expression(paste("directed (",italic(b["dir"]),")"))) +
      ylab(expression(italic(paste("P(",b["dir"],")"))))  
    
    if (line){
      plot.b <- plot.b + geom_smooth(method=linetype,se=FALSE) 
      plot.b.dir <- plot.b.dir + geom_smooth(method=linetype,se=FALSE)
    }
  res$plot.b <- plot.b
  res$plot.b.dir <- plot.b.dir
  
    if (weighted){
      P.b.w <- graph_distribution(stats$b.un.w,all=FALSE)
      P.b.dir.w <- graph_distribution(stats$b.dir.w,all=FALSE)
      
    # plot P(b.w)
      plot.b.w <- ggplot(P.b.w[which(P.b.w$dist!=0),],aes(x=points,y=dist)) + geom_point() + 
        scale_x_log10(breaks=c(1,5,10,20,40,100),labels=c("1","5","10","20","40","100")) + 
        scale_y_log10(breaks=c(.01,0.05,.1,.2,1),labels=c("0.01","0.05","0.1","0.2","1")) + 
        xlab(expression(paste("undirected (",italic(b^{"w"}),")"))) +
        ylab(expression(italic(paste("P(",b^{w},")"))))  
    
    # plot P(b.dir.w)
      plot.b.dir.w <- ggplot(P.b.dir.w[which(P.b.dir.w$dist!=0),],aes(x=points,y=dist)) + geom_point() + 
        scale_x_log10(breaks=c(1,5,10,20,40,100),labels=c("1","5","10","20","40","100")) + 
        scale_y_log10(breaks=c(.01,0.05,.1,.2,1),labels=c("0.01","0.05","0.1","0.2","1")) + 
        xlab(expression(paste("directed (",italic(b["dir"]^{"w"}),")"))) +
        ylab(expression(italic(paste("P(",b["dir"]^{"w"},")"))))  
  
      if (line){
        plot.b.w <- plot.b.w + geom_smooth(method=linetype,se=FALSE)
        plot.b.dir.w <- plot.b.dir.w + geom_smooth(method=linetype,se=FALSE)
      }
    res$plot.b.w <- plot.b.w
    res$plot.b.dir.w <- plot.b.dir.w
    }
  }
  
  if ((output=="weight") | (output=="all")){
    if (weighted){
    # plot P(w)
    # first, plot density plots
      w.df <- as.data.frame(stats$w)
      names(w.df) <- "w"
      density.w <- ggplot(w.df,aes(w))+geom_density(fill="grey40") + 
        xlab(expression(paste("weight (",italic(w),")")))
    res$density.w <- density.w
    
    # plot P(w)
      P.w <- graph_distribution(stats$w,all=FALSE)
      plot.w <- ggplot(P.w[which(P.w$dist!=0),],aes(x=points,y=dist)) + geom_point() + 
        scale_x_log10(breaks=c(1,5,10,20,40,100,1000),labels=c("1","5","10","20","40","100","1000")) + 
        scale_y_log10(breaks=c(.005,.01,0.05,.1,.2,1),labels=c("0.005","0.01","0.05","0.1","0.2","1")) + 
        xlab(expression(paste("weight (",italic(w),")"))) +
        ylab(expression(italic("P(w)")))
      
      if (line){
        plot.w <- plot.w + geom_smooth(method=linetype,se=FALSE) 
      }
    res$plot.w <- plot.w
    
    } else {
      if (output=="weight"){
        stop("Error: graph has not been designated as weighted")
      }
    }
  }
  
  if ((output=="strength") | (output=="all")){
    if (weighted){
    # plot P(s)
    # first, plot denisty plots
      s.df <- as.data.frame(stats$s)
      names(s.df)<-"s"
      density.s <- ggplot(s.df,aes(s))+geom_density(fill="grey40")+
        xlab(expression(paste("strength (",italic(s),")")))
    res$density.s <- density.s
    
    # plot s.in
      s.in.df <- as.data.frame(stats$s.in)
      names(s.in.df)<-"s.in"
      density.s.in <- ggplot(s.in.df,aes(s.in))+geom_density(fill="grey40")+
        xlab(expression(paste("in strength (",italic(s["in"]),")")))
    res$density.s.in <- density.s.in
    
    # plot s.out
      s.out.df <- as.data.frame(stats$s.out)
      names(s.out.df)<-"s.out"
      density.s.out <- ggplot(s.out.df,aes(s.out))+geom_density(fill="grey40")+
      xlab(expression(paste("out strength (",italic(s["out"]),")")))
    res$density.s.out <- density.s.out
    
    # plot all three overlapping
      comb.s <- data.frame(s=stats$s,type=rep(1,length(stats$s)))
      comb.s.in <- data.frame(s=stats$s.in,type=rep(2,length(stats$s.in)))
      comb.s.out <- data.frame(s=stats$s.out,type=rep(3,length(stats$s.out)))
      all.s <- rbind(comb.s,comb.s.in,comb.s.out)
      all.s.density <- ggplot(all.s,aes(group=factor(type))) +
        geom_density(alpha=0.2,color=NA,aes(x=s,fill=factor(type))) +
        geom_density(alpha=0.001,aes(x=s,fill=factor(type)),show_guide=FALSE) +
        xlab("strength") +
        scale_fill_manual(name = "strength",values=c("red","green","blue"),
                        labels = c("1" = expression(italic(s)),
                                   "2" = expression(italic(s["in"])),
                                   "3" = expression(italic(s["out"]))))
    res$all.s.density <- all.s.density
    
    # find P()
      P.s <- graph_distribution(stats$s,all=FALSE)
      P.s.in <- graph_distribution(stats$s.in,all=FALSE)
      P.s.out <- graph_distribution(stats$s.out,all=FALSE)
    
    # plot P(s)
      plot.s <- ggplot(P.s[which(P.s$dist!=0),],aes(x=points,y=dist))+geom_point()+
        scale_x_log10(breaks=c(1,5,10,20,40,100,1000,2000,5000,10000),
                      labels=c("1","5","10","20","40","100","1000","2000","5000","10000")) + 
        scale_y_log10(breaks=c(.005,.01,0.05,.1,.2,.5,1),
                      labels=c("0.005","0.01","0.05","0.1","0.2","0.5","1")) + 
        xlab(expression(paste("strength (",italic(s),")"))) + 
        ylab(expression(italic("P(s)")))
    
    # plot P(s.in)
      plot.s.in <- ggplot(P.s.in[which(P.s.in$dist!=0),],aes(x=points,y=dist))+geom_point()+
        scale_x_log10(breaks=c(1,5,10,20,40,100,1000,2000,5000,10000),
                      labels=c("1","5","10","20","40","100","1000","2000","5000","10000")) + 
        scale_y_log10(breaks=c(.005,.01,0.05,.1,.2,.5,1),
                      labels=c("0.005","0.01","0.05","0.1","0.2","0.5","1")) + 
        xlab(expression(paste("in strength (",italic(s["in"]),")"))) +
        ylab(expression(italic(paste("P(",s["in"],")"))))
    
    # plot P(s.out)
      plot.s.out <- ggplot(P.s.out[which(P.s.out$dist!=0),],aes(x=points,y=dist))+geom_point()+
        scale_x_log10(breaks=c(1,5,10,20,40,100,1000,2000,5000,10000),
                      labels=c("1","5","10","20","40","100","1000","2000","5000","10000")) + 
        scale_y_log10(breaks=c(.005,.01,0.05,.1,.2,.5,1),
                      labels=c("0.005","0.01","0.05","0.1","0.2","0.5","1")) + 
        xlab(expression(paste("out strength (",italic(s["out"]),")"))) +
        ylab(expression(italic(paste("P(",s["out"],")"))))
    
      if (line) {
        plot.s <- plot.s + geom_smooth(method=linetype,se=FALSE)
        plot.s.in <- plot.s.in + geom_smooth(method=linetype,se=FALSE)
        plot.s.out <- plot.s.out + geom_smooth(method=linetype,se=FALSE)
      }
    res$plot.s <- plot.s
    res$plot.s.in <- plot.s.in
    res$plot.s.out <- plot.s.out
    } else {
      if (output=="weight"){
        stop("Error: graph has not been designated as weighted")
      }
    }
  }
  res # output
}

