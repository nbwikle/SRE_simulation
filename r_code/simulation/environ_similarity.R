#environ_similarity.R 
#Ryan Yan 
#This isn't really a function. Just make sure you have states.csv (state abbrev. to names)
#file in your working directory. Then run all the code. This creates the normalized
#similarity matrix "norm_sim" based on equally weighted temperature and precipitation
#between 50 states + washington DC. Uses euclidean distance between vectors to calculate 
#distance matrix which is converted to similarity matrix values. 
#Norm_sim is created by dividing each element in "sim" by the mean of all it's elements 
#This is done so when P is multiplied by the similarity matrix, it doesn't not affect
#the overall growth of the infection but only it's distribution (approximately)

library(sqldf)

#Reads in monthly normals of abiotic parameter data. 
#temp_table has temperature information, and precip_table has precipitation information

state_file <- "./data/states.csv"
temp_url <- "ftp://ftp.ncdc.noaa.gov/pub/data/normals/1981-2010/products/temperature/mly-tavg-normal.txt"
precip_url <- "ftp://ftp.ncdc.noaa.gov/pub/data/normals/1981-2010/products/precipitation/mly-prcp-normal.txt"
stations_url <- "ftp://ftp.ncdc.noaa.gov/pub/data/normals/1981-2010/station-inventories/prcp-inventory.txt"
temp_stations_url <- "ftp://ftp.ncdc.noaa.gov/pub/data/normals/1981-2010/station-inventories/temp-inventory.txt"

temp_table <- read.table(temp_url, header = TRUE, skip = 56, stringsAsFactors = FALSE,
                         na.strings = c("-7777S", "-7777R", "-7777P", "-7777Q", "-7777C"))
                         
precip_table <- read.table(precip_url, header = TRUE, sep = "", skip = 111, 
                           stringsAsFactors = FALSE, na.strings = c("-7777S", "-7777R", 
                                                                    "-7777P", "-7777Q", "-7777C"))

station_table <- read.table(stations_url, header = FALSE, stringsAsFactors = FALSE, fill = TRUE)
temp_station_table <- read.table(temp_stations_url, header = FALSE, stringsAsFactors = FALSE, fill = TRUE)
state_table <- read.csv(state_file, header = TRUE, stringsAsFactors = FALSE)

names(precip_table) <- c("Station", "Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                         "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
names(temp_table) <- c("Station", "Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                         "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")


#Delete non-numeric digits from the numeric string values
for(i in 1:dim(precip_table)[1]) {
    precip_table[i, 2:13] <- gsub("[^0-9]", "", precip_table[i, 2:13])
}
for(i in 1:dim(temp_table)[1]) {
    temp_table[i, 2:13] <- gsub("[^0-9]", "", temp_table[i, 2:13])
}

#Get rid of special values
precip_table[is.na(precip_table)] <- 0
temp_table[is.na(temp_table)] <- 0

#Sort stations by state abberviation
precip_table <- sqldf("select Station, Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep,
                      Oct, Nov, Dec, V5 from precip_table join station_table on 
                      station_table.V1 = precip_table.Station")
temp_table <- sqldf("select Station, Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep,
                      Oct, Nov, Dec, V5 from temp_table join station_table on 
                      station_table.V1 = temp_table.Station")

#Decode state abbreviations into state names
precip_table <- sqldf("select State, Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep,
                      Oct, Nov, Dec from precip_table join state_table on 
                      state_table.Abbreviation = precip_table.V5")
temp_table <- sqldf("select State, Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep,
                      Oct, Nov, Dec from temp_table join state_table on 
                      state_table.Abbreviation = temp_table.V5")

#Average by state
average_precip <- sqldf("select State, Avg(Jan), Avg(Feb), Avg(Mar), Avg(Apr), Avg(May),
                        Avg(Jun), Avg(Jul), Avg(Aug), Avg(Sep), Avg(Oct), Avg(Nov), Avg(Dec) 
                        from precip_table group by State")
average_temp <- sqldf("select State, Avg(Jan), Avg(Feb), Avg(Mar), Avg(Apr), Avg(May),
                        Avg(Jun), Avg(Jul), Avg(Aug), Avg(Sep), Avg(Oct), Avg(Nov), Avg(Dec) 
                        from temp_table group by State")

#Find average annual precipitation, change units to inches
average_precip$"Avg(Ann)" <- apply(average_precip[,-1], 1, sum)
average_precip[,-1] <- round((average_precip[,-1] / 100), 2)
names(average_precip) <- c("State", "Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                           "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Average")

dc_data <- (average_precip[46, -1] + average_precip[20, -1] + average_precip[8, -1]) / 3
rownames(dc_data) <- NULL
dc_row <- data.frame(State = "Washington DC", dc_data, stringsAsFactors = FALSE)
average_precip <- rbind(average_precip[1:8, ], dc_row, average_precip[9:50, ])
rownames(average_precip) <- NULL

precip_dist <- as.matrix(dist(average_precip[,2:13]), dimnames = list(average_precip$State, 
                                                                    average_precip$State))
precip_sim <- 1 - (precip_dist - min(precip_dist)) / (max(precip_dist) - min(precip_dist))

#Find average annual temp, change units to Fahrenheit
average_temp$"Avg(Ann)" <- apply(average_temp[,-1], 1, mean)
average_temp[,-1] <- round((average_temp[,-1] / 10), 2)
names(average_temp) <- c("State", "Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                           "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Average")

dc_data2 <- (average_temp[46, -1] + average_temp[20, -1] + average_temp[8, -1]) / 3
dc_row2 <- data.frame(State = "Washington DC", dc_data2, stringsAsFactors = FALSE)
average_temp <- rbind(average_temp[1:8, ], dc_row, average_temp[9:50, ])
rownames(average_temp) <- NULL

temp_dist <- as.matrix(dist(average_temp[,2:13]), dimnames = list(average_temp$State, 
                                                                    average_temp$State))

temp_sim <- 1 - (temp_dist - min(temp_dist)) / (max(temp_dist) - min(temp_dist))

sim <- (temp_sim + precip_sim) / 2



