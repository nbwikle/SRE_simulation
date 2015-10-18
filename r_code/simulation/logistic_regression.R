#Logistic Regression using environmental variates to predict probability of establishment.
#Model trained on presence / absence data and environmental data. Each row of data supplies
#Presence or absence for a FIPS code and the climatic data associated with it

require(sqldf)

#Retrieves urls of environmental station data / reads them into R
zip_url <- "ftp://ftp.ncdc.noaa.gov/pub/data/normals/1981-2010/station-inventories/zipcodes-normals-stations.txt"
temp_url <- "ftp://ftp.ncdc.noaa.gov/pub/data/normals/1981-2010/products/temperature/mly-tavg-normal.txt"
maxtemp_url <- "ftp://ftp.ncdc.noaa.gov/pub/data/normals/1981-2010/products/temperature/mly-tmax-normal.txt"
mintemp_url <- "ftp://ftp.ncdc.noaa.gov/pub/data/normals/1981-2010/products/temperature/mly-tmin-normal.txt"
precip_url <- "ftp://ftp.ncdc.noaa.gov/pub/data/normals/1981-2010/products/precipitation/mly-prcp-normal.txt"

zip_table <- read.fwf(zip_url, width = c(11, 6), header = FALSE, skip = 26, 
                      stringsAsFactors = FALSE, sep = "", strip.white = TRUE)

temp_table <- read.table(temp_url, header = TRUE, skip = 56, stringsAsFactors = FALSE,
                         na.strings = c("-7777S", "-7777R", "-7777P", "-7777Q", "-7777C"))

maxtemp_table <- read.table(maxtemp_url, header = TRUE, skip = 56, stringsAsFactors = FALSE,
                         na.strings = c("-7777S", "-7777R", "-7777P", "-7777Q", "-7777C"))

mintemp_table <- read.table(mintemp_url, header = TRUE, skip = 56, stringsAsFactors = FALSE,
                         na.strings = c("-7777S", "-7777R", "-7777P", "-7777Q", "-7777C"))

precip_table <- read.table(precip_url, header = TRUE, sep = "", skip = 111, 
                           stringsAsFactors = FALSE, na.strings = c("-7777S", "-7777R", 
                                                                    "-7777P", "-7777Q", "-7777C"))
rename <- function(table) {
    names(table) <- c("Station", "Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                      "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
    table
}

names(zip_table) <- c("Station", "Zip")
precip_table <- rename(precip_table)
temp_table <- rename(temp_table)
maxtemp_table <- rename(maxtemp_table)
mintemp_table <- rename(mintemp_table)

#Sanitize data set. Gets rid of irrelevant notation and NAs.
sanitize <- function(table) {
    table <- na.omit(data.frame(Station = table$Station, 
                        as.data.frame(lapply(table[,2:13], FUN = function(x) gsub("[^0-9]", "", x)))))
    
}
precip_table <- sanitize(precip_table)
temp_table <- sanitize(temp_table)
maxtemp_table <- sanitize(maxtemp_table)
mintemp_table <- sanitize(mintemp_table)

#Groups the data by zip code
precip_table <- sqldf("select precip_table.Station, Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep,
                      Oct, Nov, Dec, Zip from precip_table join zip_table on 
                      precip_table.Station = zip_table.Station")

temp_table <- sqldf("select temp_table.Station, Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep,
                      Oct, Nov, Dec, Zip from temp_table join zip_table on 
                      temp_table.Station = zip_table.Station")

maxtemp_table <- sqldf("select maxtemp_table.Station, Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep,
                      Oct, Nov, Dec, Zip from maxtemp_table join zip_table on 
                      maxtemp_table.Station = zip_table.Station")

mintemp_table <- sqldf("select mintemp_table.Station, Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep,
                      Oct, Nov, Dec, Zip from mintemp_table join zip_table on 
                      mintemp_table.Station = zip_table.Station")

#Add fips codes corresponding to zip codes
precip_table <- sqldf("select precip_table.Station, Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep,
                      Oct, Nov, Dec, Zip, fips from precip_table join code on 
                      precip_table.Zip = code.zips")

temp_table <- sqldf("select temp_table.Station, Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep,
                      Oct, Nov, Dec, Zip, fips from temp_table join code on 
                      temp_table.Zip = code.zips")

maxtemp_table <- sqldf("select maxtemp_table.Station, Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep,
                      Oct, Nov, Dec, Zip, fips from maxtemp_table join code on 
                      maxtemp_table.Zip = code.zips")

mintemp_table <- sqldf("select mintemp_table.Station, Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep,
                      Oct, Nov, Dec, Zip, fips from mintemp_table join code on 
                      mintemp_table.Zip = code.zips")

#Load species data into the global environment
loadSpeciesData <- function(name) {
    filestring <- paste("./sandy_data/species_same_format/", name, "_estab.csv", sep = "")
    data <- read.csv(filestring)
    assign(eval(name), data, envir = .GlobalEnv)
}

toLoad <- c("beech_bark_disease", "cereal_leaf_beetle", "emerald_ash_borer", "european_corn_borer",
            "gypsy_moth", "hemlock_woolly_adelgid", "japanese_beetle", "mexican_bean")

lapply(toLoad, loadSpeciesData)

#Change all columns into character vectors
toInt <- function(table) {
    for(i in 2:dim(table)[2]) {
        table[,i] <- as.character(table[,i])
    }
    table
}

precip_table <- toInt(precip_table)
temp_table <- toInt(temp_table)
maxtemp_table <-toInt(maxtemp_table)
mintemp_table <- toInt(mintemp_table)

#The "createRegModel" function runs model training and output on specific species. 
#Give "species" parameter in format of example, e.g. "beech_bark_disease"

#Accessory function to apply presence and absence data
addPresence <- function(table, species) {
    table$Presence = 0
    table[table$fips %in% get(species)$FIPS,]$Presence = 1
    table
}

createRegModel <- function(species) {
    precip_table <- addPresence(precip_table, species)
    temp_table <- addPresence(temp_table, species)
    maxtemp_table <- addPresence(maxtemp_table, species)
    mintemp_table <- addPresence(mintemp_table, species)
    
    precip_table <<- transform(precip_table, 
                               Jan=as.numeric(Jan),
                               Feb=as.numeric(Feb),
                               Mar=as.numeric(Mar),
                               Apr=as.numeric(Apr),
                               May=as.numeric(May),
                               Jun=as.numeric(Jun),
                               Jul=as.numeric(Jul),
                               Aug=as.numeric(Aug),
                               Sep=as.numeric(Sep),
                               Oct=as.numeric(Oct),
                               Nov=as.numeric(Nov),
                               Dec=as.numeric(Dec))
    temp_table <<- transform(temp_table, 
                               Jan=as.numeric(Jan),
                               Feb=as.numeric(Feb),
                               Mar=as.numeric(Mar),
                               Apr=as.numeric(Apr),
                               May=as.numeric(May),
                               Jun=as.numeric(Jun),
                               Jul=as.numeric(Jul),
                               Aug=as.numeric(Aug),
                               Sep=as.numeric(Sep),
                               Oct=as.numeric(Oct),
                               Nov=as.numeric(Nov),
                               Dec=as.numeric(Dec))
    maxtemp_table <<- transform(maxtemp_table, 
                             Jan=as.numeric(Jan),
                             Feb=as.numeric(Feb),
                             Mar=as.numeric(Mar),
                             Apr=as.numeric(Apr),
                             May=as.numeric(May),
                             Jun=as.numeric(Jun),
                             Jul=as.numeric(Jul),
                             Aug=as.numeric(Aug),
                             Sep=as.numeric(Sep),
                             Oct=as.numeric(Oct),
                             Nov=as.numeric(Nov),
                             Dec=as.numeric(Dec))
    mintemp_table <<- transform(mintemp_table, 
                             Jan=as.numeric(Jan),
                             Feb=as.numeric(Feb),
                             Mar=as.numeric(Mar),
                             Apr=as.numeric(Apr),
                             May=as.numeric(May),
                             Jun=as.numeric(Jun),
                             Jul=as.numeric(Jul),
                             Aug=as.numeric(Aug),
                             Sep=as.numeric(Sep),
                             Oct=as.numeric(Oct),
                             Nov=as.numeric(Nov),
                             Dec=as.numeric(Dec))
    
    names(precip_table)[2:13] <- c("precipJan","precipFeb","precipMar","precipApr","precipMay",
    "precipJun","precipJul","precipAug","precipSep","precipOct",
    "precipNov","precipDec")
    names(temp_table)[2:13] <- c("tempJan","tempFeb","tempMar","tempApr","tempMay",
    "tempJun","tempJul","tempAug","tempSep","tempOct",
    "tempNov","tempDec")
    names(maxtemp_table)[2:13] <- c("maxtempJan","maxtempFeb","maxtempMar","maxtempApr","maxtempMay",
    "maxtempJun","maxtempJul","maxtempAug","maxtempSep","maxtempOct",
    "maxtempNov","maxtempDec")
    names(mintemp_table)[2:13] <- c("mintempJan","mintempFeb","mintempMar","mintempApr","mintempMay",
    "mintempJun","mintempJul","mintempAug","mintempSep","mintempOct",
    "mintempNov","mintempDec")
    
    comb1_table <- merge(x=precip_table,y=temp_table,by=c("Station","Zip","fips","Presence"))
    comb2_table <- merge(x=maxtemp_table,y=mintemp_table,by=c("Station","Zip","fips","Presence"))
    comball_table <- merge(x=comb1_table,y=comb2_table,by=c("Station","Zip","fips","Presence"))
    set.seed(27)
    training <- comball_table[sample(1:nrow(comball_table),replace=T,size=nrow(comball_table)/2),]
    training$Presence <- as.factor(training$Presence)
    training <- training[,-c(1:3)]
    test <- comball_table[,-c(1:4)]
    
    
    log_reg <- glm(Presence~.,data=training,family=binomial())
    predictions <- predict(log_reg,test,type="response")
    
    output <- data.frame(comball_table,predictions)
}

createRegModel("beech_bark_disease")
