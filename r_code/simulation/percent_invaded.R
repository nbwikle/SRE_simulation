# percent_invaded.R

# A function to calculate the percent of a state invaded, for all possible
# years. Note: this function assumes that the establishment data has
# been added to the environment, with the following attributes: 
# STATEFP, COUNTYFP, FIPS, YEAR.  In addition, the stateFIPS.csv and
# tl_2014_us_county.dbf files are used, as is the calc_st_area() function.
# Input:
# Output: 
percent_invaded <- function(data,water=TRUE){
  require(foreign)
  
  wd <- "~/SRE_simulation/data"
  setwd(wd)
  stFIPS <- read.csv("stateFIPS.csv",header=TRUE)
  ctyArea <- read.dbf("tl_2014_us_county.dbf")
  ctyArea[,1]<-as.numeric(levels(ctyArea[,1]))[ctyArea[,1]]
  ctyArea[,2]<-as.numeric(levels(ctyArea[,2]))[ctyArea[,2]]
  ctyArea[,4]<-as.numeric(levels(ctyArea[,4]))[ctyArea[,4]]
  
  ctyArea$ALAND <- sapply(ctyArea$ALAND,function(x) to_square_mile(x))
  ctyArea$AWATER <- sapply(ctyArea$AWATER,function(x) to_square_mile(x))
  
  all.s.ind <- c(1:nrow(stFIPS))
  total.area <- sapply(all.s.ind,function(x) calc_st_area(x,water=water))
  
  names(data) <- c("STATEFP","COUNTYFP","GEOID","YEAR")
  
  years <- unique(data$YEAR)
  years <- sort(years)
  output <- matrix(rep(rep(0,nrow(stFIPS)),length(years)),ncol=length(years))
  res <- data.frame(state=stFIPS$State,output)
  names(res) <- c("state",as.character(years))
  
  for (i in 1:length(years)){
    inv.area <- rep(0,51)
    #y.int <- years[which(years<=years[i])]
    cty.FIPS <-data[which(data$YEAR<=years[i]),]
    m.data <- merge(cty.FIPS,ctyArea,by=c("STATEFP","COUNTYFP"))
    
    for(j in 1:nrow(m.data)){
      s.ind <- which(stFIPS$STATEFP==m.data$STATEFP[j])
      inv.area[s.ind] <- inv.area[s.ind]+m.data$ALAND[j]
      if (water){
        inv.area[s.ind] <- inv.area[s.ind]+m.data$AWATER[j]
      }
    }
    prop.area <- inv.area/total.area
    res[,i+1] <- prop.area
  }
  res
}

to_square_mile <- function(x){
  new <- x/2589988.110336
  new
}

# switches District of Columbia to place 48, in keeping with it's classification
# as Washington D.C.
to_standard_output <- function(df){
  new.df <- df
  new.df[9:47,] <- df[10:48,]
  new.df[48,] <- df[9,]
  new.df
}

standard.perc.ecb <- to_standard_output(percent_ecb)
standard.perc.eab <- to_standard_output(percent_eab)
standard.perc.hwa <- to_standard_output(percent_hwa)
standard.perc.gm <- to_standard_output(percent_gm)
standard.perc.bbd <- to_standard_output(percent_bbd)
standard.perc.jb <- to_standard_output(percent_jb)
standard.perc.mb <- to_standard_output(percent_mb)
standard.perc.clb <- to_standard_output(percent_clb)

setwd("~/Documents/SRE2015/data/establishment_data")
write.csv(standard.perc.ecb,file="percent_ecb_with_water.csv")
write.csv(standard.perc.eab,file="percent_eab_with_water.csv")
write.csv(standard.perc.hwa,file="percent_hwa_with_water.csv")
write.csv(standard.perc.gm,file="percent_gm_with_water.csv")
write.csv(standard.perc.bbd,file="percent_bbd_with_water.csv")
write.csv(standard.perc.jb,file="percent_jb_with_water.csv")
write.csv(standard.perc.mb,file="percent_mb_with_water.csv")
write.csv(standard.perc.clb,file="percent_clb_with_water.csv")


map.ecb <- many_maps(percent_df_to_list(standard.perc.ecb),
                     pdfName="real_data_ecb_spread.pdf")
map.eab <- many_maps(percent_df_to_list(standard.perc.eab),
                     pdfName="real_data_eab_spread.pdf")
map.hwa <- many_maps(percent_df_to_list(standard.perc.hwa),
                     pdfName="real_data_hwa_spread.pdf")
map.gm <- many_maps(percent_df_to_list(standard.perc.gm),
                     pdfName="real_data_gm_spread.pdf")
map.bbd <- many_maps(percent_df_to_list(standard.perc.bbd),
                     pdfName="real_data_bbd_spread.pdf")
map.jb <- many_maps(percent_df_to_list(standard.perc.jb),
                     pdfName="real_data_jb_spread.pdf")
map.mb <- many_maps(percent_df_to_list(standard.perc.mb),
                     pdfName="real_data_mb_spread.pdf")
map.clb <- many_maps(percent_df_to_list(standard.perc.clb),
                     pdfName="real_data_clb_spread.pdf")



percent_df_to_list <- function(df){
  l <- list()
  data <- df[,-1]
  for(i in 1:ncol(data)){
    l[[i]]<-as.vector(data[,i])
  }
  l
}

many_maps(test,pdfName="test_real_eab_data.pdf")

