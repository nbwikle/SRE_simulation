#Loading data

loadData <- function(filename, com = TRUE, mode = TRUE) {
    d1 <- read.csv
}
#Load shipping data (trucks, combined commodities)
d1<-read.csv("1997FAF_all_trucks_comb.csv",header=TRUE)
d2<-read.csv("2002FAF_all_trucks_comb.csv",header=TRUE)
d3<-read.csv("2007FAF_all_trucks_comb.csv",header=TRUE)
d4<-read.csv("2012FAF_all_trucks_comb.csv",header=TRUE)

names(d1)<-c("origin","destination","mode","weight","ton-mile","value1")
names(d2)<-c("origin","destination","mode","weight","ton-mile","value1")
names(d3)<-c("origin","destination","mode","weight","ton-mile","value1")
names(d4)<-c("origin","destination","mode","weight","ton-mile","value1")

w1 <- weight(d1, com = FALSE)
w2 <- weight(d2, com = FALSE)
w3 <- weight(d3, com = FALSE)
w4 <- weight(d4, com = FALSE)

#Loads shipping data on (trucks, logs)
d1<-read.csv("1997FAF_trucks_logs.csv",header=TRUE)
d2<-read.csv("2002FAF_trucks_logs.csv",header=TRUE)
d3<-read.csv("2007FAF_trucks_logs.csv",header=TRUE)
d4<-read.csv("2012FAF_trucks_logs.csv",header=TRUE)

names(d1)<-c("origin","destination","commodity", "mode","weight","ton-mile","value1")
names(d2)<-c("origin","destination","commodity", "mode","weight","ton-mile","value1")
names(d3)<-c("origin","destination","commodity", "mode","weight","ton-mile","value1")
names(d4)<-c("origin","destination","commodity", "mode","weight","ton-mile","value1")

w1 <- weight(d1)
w2 <- weight(d2)
w3 <- weight(d3)
w4 <- weight(d4)

#Loads shipping data on (trucks, machinery, non-metallic mineral products, wood products)

d1<-read.csv("1997FAF_trucks_mach-minerals-wood.csv",header=TRUE)
d2<-read.csv("2002FAF_trucks_mach-minerals-wood.csv",header=TRUE)
d3<-read.csv("2007FAF_trucks_mach-minerals-wood.csv",header=TRUE)
d4<-read.csv("2012FAF_trucks_mach-minerals-wood.csv",header=TRUE)

names(d1)<-c("origin","destination","commodity", "mode","weight","ton-mile","value1")
names(d2)<-c("origin","destination","commodity", "mode","weight","ton-mile","value1")
names(d3)<-c("origin","destination","commodity", "mode","weight","ton-mile","value1")
names(d4)<-c("origin","destination","commodity", "mode","weight","ton-mile","value1")

w1 <- weight(d1)
w2 <- weight(d2)
w3 <- weight(d3)
w4 <- weight(d4)

#Loads shipping data on (all modes, cereal grains)

d1<-read.csv("1997FAF_all_cerealgrains.csv",header=TRUE)
d2<-read.csv("2002FAF_all_cerealgrains.csv",header=TRUE)
d3<-read.csv("2007FAF_all_cerealgrains.csv",header=TRUE)
d4<-read.csv("2012FAF_all_cerealgrains.csv",header=TRUE)

names(d1)<-c("origin","destination","commodity", "mode","weight","ton-mile","value1")
names(d2)<-c("origin","destination","commodity", "mode","weight","ton-mile","value1")
names(d3)<-c("origin","destination","commodity", "mode","weight","ton-mile","value1")
names(d4)<-c("origin","destination","commodity", "mode","weight","ton-mile","value1")

w1 <- weight(d1)
w2 <- weight(d2)
w3 <- weight(d3)
w4 <- weight(d4)