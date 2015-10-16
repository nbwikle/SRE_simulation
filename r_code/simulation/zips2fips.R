# Import Zip to FIPS code decoder
require(rjson)

json_file <- "./zip2fips/zip2fips.json"
json_data <- fromJSON(paste(readLines(json_file), collapse = ""))

zips <- names(json_data)
fips <- as.character(json_data)
code <- data.frame(zips, fips)
