#--------------------------------------
# INITIALIZATIONS AND LIBRARIES
#--------------------------------------
library(data.table)
data <- fread("F:/BIG DATA/ISB/Assignments/Term 2/Practicum-3/Part2/data/FinalData-Part2/CleanedRawDataWithZones.csv", header = TRUE)
str(data)
  
#--------------------------------------
#           Revenue - Per Day
#--------------------------------------
revenue <- data[ , PerDayRevenue := sum(Fare_amount), by = list(pickup_date)]
revenue <- unique(revenue[,.(pickup_date,weekday,PerDayRevenue)])

outFile <- "F:/BIG DATA/ISB/Assignments/Term 2/Practicum-3/Part2/data/FinalData-Part2/Revenue.csv"
write.csv(revenue, outFile, row.names = F)

  

