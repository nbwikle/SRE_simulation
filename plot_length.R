# plot_length.R
# Created by Nathan Wikle, 21 July 2015

# A function to plot length (l) verses M(l), defined as the average 
# number of nodes a distance less than or equal to l from any given
# vertex.
# Input: 1) stats: a list containing statistics, as outputted by network_char()
#        2) weighted: a boolean specifying if the statistics are from a 
#             weighted graph. If so, weighted length plots are also returned.
# Output: plots of length(l) vs. M(l)
plot_length <- function(stats,weighted=TRUE){
  # Find M(l), the average # of nodes within a distance less than or equal
  # to l from any given vertex
  M_l <- data.frame(length=c(0:(length(M.l(stats$l))-1)),M.l=M.l(stats$l))
  M.l.in <- data.frame(length=c(0:(length(M.l(stats$l.in))-1)),M.l=M.l(stats$l.in))
  M.l.out <- data.frame(length=c(0:(length(M.l(stats$l.out))-1)),M.l=M.l(stats$l.out))
  
  # Plot M(l), for all of the above
  # M(l)
  m.l.p <- ggplot(M_l,aes(x=length,y=M.l))+geom_point()+geom_path() +
    scale_y_log10(breaks=c(1,10,100,1000),labels=c("1","10","100","1000")) +
    scale_x_continuous(breaks=c(0,2,4,6,8),labels=c("0","2","4","6","8")) +
    xlab(expression(paste("length (",italic(l),")")))+
    ylab(expression(paste(italic("M("),italic(l),italic(")"))))
  
  # M(l.in)
  m.l.in.p <- ggplot(M.l.in,aes(x=length,y=M.l))+geom_point()+geom_path() +
    scale_y_log10(breaks=c(1,10,100,1000),labels=c("1","10","100","1000")) +
    scale_x_continuous(breaks=c(0,2,4,6,8),labels=c("0","2","4","6","8")) +
    xlab(expression(paste("in length (",italic(l["in"]),")"))) +
    ylab(expression(paste(italic("M("),italic(l["in"]),italic(")"))))
  
  # M(l.out)
  m.l.out.p <- ggplot(M.l.out,aes(x=length,y=M.l))+geom_point()+geom_path()+
    scale_y_log10(breaks=c(1,10,100,1000),labels=c("1","10","100","1000"))+
    scale_x_continuous(breaks=c(0,2,4,6,8),labels=c("0","2","4","6","8"))+
    xlab(expression(paste("out length (",italic(l["out"]),")"))) + 
    ylab(expression(paste(italic("M("),italic(l["out"]),italic(")"))))
  
  # add M(l)'s to same df
  type <- rep(1,nrow(M_l))
  M_l <- cbind(M_l,type)
  type <- rep(2,nrow(M.l.in))
  M.l.in <- cbind(M.l.in,type)
  type <- rep(3,nrow(M.l.out))
  M.l.out <- cbind(M.l.out,type)
  l.df <- rbind(M_l,M.l.in,M.l.out)
  
  # plot all M(l)'s on the same plot
  l.p <- ggplot(l.df,aes(x=length,y=M.l,group=type,color=factor(type)))+
    geom_point(size=3,alpha=0.9)+geom_path(size=1,alpha=0.25)+
    scale_y_log10(breaks=c(1,10,100,1000),labels=c("1","10","100","1000"))+
    xlab(expression(paste("length (",italic(l),")"))) + 
    ylab(expression(paste(italic("M("),italic(l),italic(")"))))+
    scale_color_manual("Length\n",labels=c(expression(italic("l")),
                                      expression(paste(italic(l["in"]))),
                                      expression(paste(italic(l["out"])))),
                                  values=c("red","blue","darkgreen"))
  # repeat for weighted stats
  if (weighted){
    M.l.w <- data.frame(length=c(0:(length(M.l(stats$l.w))-1)),M.l=M.l(stats$l.w))
    M.l.w.in <- data.frame(length=c(0:(length(M.l(stats$l.w.in))-1)),M.l=M.l(stats$l.w.in))
    M.l.w.out <- data.frame(length=c(0:(length(M.l(stats$l.w.out))-1)),M.l=M.l(stats$l.w.out))
    
    # plot M(l.w)
    m.l.w.p <- ggplot(M.l.w,aes(x=length,y=M.l))+geom_point()+geom_path()+
      scale_y_log10(breaks=c(1,10,100,1000),labels=c("1","10","100","1000"))+
      scale_x_continuous(breaks=c(0,2,4,6,8),labels=c("0","2","4","6","8"))+
      xlab(expression(paste("weighted length (",italic(l^{"w"}),")")))+
      ylab(expression(paste(italic("M("),italic(l^{"w"}),italic(")"))))
    
    # plot M(l.w.in)  
    m.l.w.in.p <- ggplot(M.l.w.in,aes(x=length,y=M.l))+geom_point()+geom_path()+
      scale_y_log10(breaks=c(1,10,100,1000),labels=c("1","10","100","1000"))+
      scale_x_continuous(breaks=c(0,2,4,6,8,10,12,14,16),
                         labels=c("0","2","4","6","8","10","12","14","16"))+
      xlab(expression(paste("weighted in length (",italic(l["in"]^{"w"}),")")))+
      ylab(expression(paste(italic("M("),italic(l["in"]^{"w"}),italic(")"))))
      
    # plot M(l.w.out)
    m.l.w.out.p <- ggplot(M.l.w.out,aes(x=length,y=M.l))+geom_point()+geom_path()+
      scale_y_log10(breaks=c(1,10,100,1000),labels=c("1","10","100","1000"))+
      scale_x_continuous(breaks=c(0,2,4,6,8,10,12,14,16),
                         labels=c("0","2","4","6","8","10","12","14","16"))+
      xlab(expression(paste("weighted out length (",italic(l["out"]^{"w"}),")")))+
      ylab(expression(paste(italic("M("),italic(l["out"]^{"w"}),italic(")"))))
      
    # add M(l)'s to the same df
    type <- rep(1,nrow(M.l.w))
    M.l.w <- cbind(M.l.w,type)
    type <- rep(2,nrow(M.l.w.in))
    M.l.w.in <- cbind(M.l.w.in,type)
    type <- rep(3,nrow(M.l.w.out))
    M.l.w.out <- cbind(M.l.w.out,type)
    l.w.df <- rbind(M.l.w,M.l.w.in,M.l.w.out)
    
    # plot all M(l)'s on the same plot
    l.w.p <- ggplot(l.w.df,aes(x=length,y=M.l,group=type,color=factor(type)))+
      geom_point(size=3,alpha=0.9)+geom_path(size=1,alpha=0.25)+
      scale_y_log10(breaks=c(1,10,100,1000),labels=c("1","10","100","1000"))+
      xlab(expression(paste("weighted length (",italic(l^{"w"}),")")))+
      ylab(expression(paste(italic("M("),italic(l^{"w"}),italic(")"))))+
      scale_color_manual("Length\n",
                  labels=c(expression(italic(l^{"w"})),
                           expression(italic(l["in"]^{"w"})),
                           expression(italic(l["out"]^{"w"}))),
                  values=c("red","blue","darkgreen"))
    output <- list(m.l.p,m.l.in.p,m.l.out.p,l.p,
                   m.l.w.p,m.l.w.in.p,m.l.w.out.p,l.w.p)
    names(output) <- c("m.l.p","m.l.in.p","m.l.out.p","l.p",
                       "m.l.w.p","m.l.w.in.p","m.l.w.out.p","l.w.p")
  } else {
    output <- list(m.l.p,m.l.in.p,m.l.out.p,l.p)
    names(output) <- c("m.l.p","m.l.in.p","m.l.out.p","l.p")
  }
  output # return plots
}

# A function to calculate M(l), the average number of nodes within a 
# distance less than or equal to l from any given vertex.
# Input: 1) l: a matrix of shortest path lengths
# Output: a vector consisting of M(l) values, for all possible
#  values of l
M.l <- function(l){
  l[which(l==Inf)]<-NA # replace Inf's with NA's
  elements <- nrow(l) # number of vertices
  biggest <- max(l,na.rm=TRUE)+5 # largest possible l value, plus five
  l.avg <- rep(NA,biggest+1) # vector to store M(l) values
  l.avg[1]<-1 # represents l=0
  for(i in 1:biggest){ # calculate M(l)
    s <- rep(NA,elements)
    for(j in 1:elements){ 
      ind <- which((l[j,]<=i)&(l[,j]!=0))
      s[j]<-sum(ind)
    }
    l.avg[i+1]<-mean(s)
  }
  l.avg # return M(l)
}
