#--------------------------------------
# INITIALIZATIONS AND LIBRARIES
#--------------------------------------

library(data.table)
library(udunits2)
library(geosphere)
library(ggplot2)
library(RColorBrewer)

asTimeStamp = function(strangeDateString) {
  formatString = '%Y-%m-%d %H:%M:%S'
  return(strptime(strangeDateString, formatString))
}

#----------------------------------------------------------------
# SET THE CURRENT DIRECTORY TO RAW DATASET (OBTAINED FROM NYC SITE)
#----------------------------------------------------------------
setwd("F:/BIG DATA/ISB/Assignments/Term 2/Practicum-3/Part2/data/2016") #Only for Jan - June
#setwd("F:/BIG DATA/ISB/Assignments/Term 2/Practicum-3/Part2/data/2015") 
#setwd("F:/BIG DATA/ISB/Assignments/Term 2/Practicum-3/Part2/data/2014") 

file_list <- list.files()
data <- data.table(NULL)

#----------------------------------------------------------------
# LOADING THE DATA SET TO FROM ALL FILES TO CREATE A SINGLE DATASET
# VERY HUGE, SINGLE COMBINED DATASET
#----------------------------------------------------------------

for (file in file_list){
  temp_dataset <- fread(file, header = TRUE)
  data <- rbind(data, temp_dataset)
  rm(temp_dataset)
}

data <- subset(data, select = c("lpep_pickup_datetime",
                                "Lpep_dropoff_datetime",
                                "Pickup_longitude",
                                "Pickup_latitude",
                                "Dropoff_longitude",
                                "Dropoff_latitude",
                                "Passenger_count",
                                "Trip_distance",
                                "Fare_amount"))

#----------------------------------------------------------------
#             CLEANING
#----------------------------------------------------------------

#Convert Trip_Distance from miles to kilometers
data <- data[,Trip_distance := ud.convert(Trip_distance, "mi", "km")]

#All records where trip distance is greater than 0
data <- data[Trip_distance > 0,]

#All records where the ratio is between 2 and 10
data <- data[Fare_amount/Trip_distance >= 2 &  Fare_amount/Trip_distance <= 10,]

#All records where latitude (both pickup and dropoff) is greater than 0.00 
data <- data[Pickup_latitude > 0,]
data <- data[Dropoff_latitude > 0,]

#Removing other spurious records
data <- data[Pickup_longitude > -115 & Pickup_longitude < 0,]

#Rounding the co-ordinates to 3 digit precision
data <- data[,Pickup_latitude := round(data$Pickup_latitude,3)]
data <- data[,Pickup_longitude := round(data$Pickup_longitude,3)]
data <- data[,Dropoff_latitude := round(data$Dropoff_latitude,3)]
data <- data[,Dropoff_longitude := round(data$Dropoff_longitude,3)]

#Pickup, dropoff time date and hour
data <- data[,pickup_date := as.Date(substr(lpep_pickup_datetime,1,10))]
data <- data[,pickup_time := substr(lpep_pickup_datetime,12,19)]
data <- data[,pickup_hour := as.numeric(substr(lpep_pickup_datetime,12,13))]
data <- data[,dropoff_date := substr(Lpep_dropoff_datetime,1,10)]
data <- data[,dropoff_time := substr(Lpep_dropoff_datetime,12,19)]
data <- data[,dropoff_hour := as.numeric(substr(Lpep_dropoff_datetime,12,13))]

# Assigning the slots to data 
# 0-4:1, 4-8:2, 8-12:3, 12-4:4, 4-8:5, 8-12:6
data <- data[,slot:=ifelse(pickup_hour < 4,1,
                           ifelse(pickup_hour >= 4 & pickup_hour <8, 2, 
                                  ifelse(pickup_hour >= 8 & pickup_hour < 12, 3, 
                                         ifelse(pickup_hour >= 12 & pickup_hour < 16, 4, 
                                                ifelse(pickup_hour >= 16 & pickup_hour < 20, 5, 6 )))))]

# Duration of each trip [THIS TAKES LOT OF TIME]
data <- data[, TripDuration:= as.numeric(difftime(asTimeStamp(Lpep_dropoff_datetime),
                                                  asTimeStamp(lpep_pickup_datetime),
                                                  units='mins'))]

#Day of the Week [THIS TAKES LOT OF TIME]
data <- data[,weekday := weekdays(pickup_date)]

summary(data[,-c("zone")])

#----------------------------------------------------------------
# CREATING ZONES
#----------------------------------------------------------------

# Spherical coordinates in radians
data <- data[,pickup.long.rad:=Pickup_longitude * (2 * pi)/360]
data <- data[,pickup.lat.rad:=Pickup_latitude * (2 * pi)/360]
R <- (6378 + 6356)/2

# Cartesian coordinates
data <- data[,x:=R * cos(data$pickup.lat.rad) * cos(data$pickup.long.rad)]
data <- data[,y:=R * cos(data$pickup.lat.rad) * sin(data$pickup.long.rad)]
data <- data[,z:=R * sin(data$pickup.lat.rad)]

# Perform kmeans
matrix <- data[, .(x, y, z)]
gc() #garbage collector
models <- list()
chs <- NULL

for (c in seq(2, 20, 1)) {
  model <- kmeans(x = matrix, center = c, nstart = 20)
  models <- append(models, list(model))
  
  support = c
  ch = (sum(model$betweenss)/(c - 1))/(sum(model$withinss)/(sum(model$size) - c))
  chs = rbind(chs, data.frame(support, ch))
}

windows()
# Plot the CH index
ggplot(data = chs) +
  geom_line(aes(x = support, y = ch), size = 2) +
  geom_point(aes(x = support, y = ch), size = 3) +
  ggtitle("Identifying the optimal number of Zones") +
  xlab("Number of clusters") + ylab("CH Index") +
  scale_x_continuous(breaks = seq(2,22,1)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_text(aes(x = support, y = ch, label=chs$support), hjust=1,vjust=-1, size = 5)


# Kmeans for All slots
t = Sys.time()
km.slot <- kmeans(x = matrix, center = 15, nstart = 20)
print(difftime(Sys.time(), t, units = 'mins'))

#Computing Center Data
centers.slot <- data.frame(km.slot$centers)

r = sqrt( I(centers.slot$x)^2 + I(centers.slot$y)^2 + I(centers.slot$z)^2 )
latitude.rad = asin((centers.slot$z)/r)
longitude.rad = atan2(centers.slot$y, centers.slot$x)

centers.slot$latitude <- latitude.rad * 180/(pi)
centers.slot$longitude <- longitude.rad * 180/(pi)
centers.slot$cluster.size <- km.slot$size
centers.slot$cluster <- row.names(centers.slot)

#Assigning Zone to the data set
data <- data[,zone:=km.slot$cluster]

# Calculating the summary statistics of each point distance from centroid in each clusters
#-------------------------------------------------------------------------------

getDistanceMatrix <- function(){
  listofdfs <- list()
  
  for (i in 1:nrow(centers.slot)) {
    center <- centers.slot[i,]
    df <- data[zone ==i,]
    #df <- as.data.frame(df)
    distances <- sqrt( (df$x - center$x)^2 + (df$y - center$y)^2 + (df$z - center$z)^2 )
    df <- cbind(df,distances)
    listofdfs[[i]] <- df
  }
  
  return(listofdfs)
}

DistanceMatrices <- getDistanceMatrix()
summary.slot <- sapply(DistanceMatrices, function(x) summary(x$distances))
summary.slot[3,]

centers.slot$median_distance <- summary.slot[3,]
centers.slot$mean_distance <- summary.slot[4,]


#----------------------------------------------------------------
# WRITING TO CSV
#----------------------------------------------------------------
write.csv(centers.slot, "F:/BIG DATA/ISB/Assignments/Term 2/Practicum-3/Part2/data/FinalData-Part2/centers.csv", row.names = F)

outFile <- "F:/BIG DATA/ISB/Assignments/Term 2/Practicum-3/Part2/data/FinalData-Part2/CleanedRawDataWithZones.csv"
fwrite(data, outFile)
  

# #------------------------------------------------------------------
# # HEAT MAP - VISUALLY INSPECTING THE TRAFFIC (PICKUPS) DURING THE WEEK FOR EACH HOUR
# #---------------------------------------------------------------------
# 
# Heat <- data[ , PerDayDemand := .N, by = list(pickup_date,weekday,pickup_hour)]
# Heat <- unique(Heat[,.(pickup_date,weekday,pickup_hour,PerDayDemand)])
# 
# Heat.median <- Heat[ , list(MedianDemandPerWeekday = round(median(PerDayDemand))), by = list(weekday,pickup_hour)]
# Heat.median <- unique(Heat.median[,.(weekday,pickup_hour,MedianDemandPerWeekday)])
# 
# Heat.median <- as.data.frame(Heat.median)
# Heat.median$weekday <- as.factor(Heat.median$weekday)
# 
# Heat.median$weekday <- factor(Heat.median$weekday,
#                               levels= c("Monday", "Tuesday", "Wednesday",
#                                         "Thursday", "Friday", "Saturday","Sunday"))
# 
# 
# 
# windows()
# hm.palette <- colorRampPalette(brewer.pal(9, 'YlOrRd'), space='Lab')
# ggplot(Heat.median, aes(x= pickup_hour, y=weekday, fill = MedianDemandPerWeekday)) +
#   geom_tile()+
#   scale_fill_gradientn("Demand",colours = hm.palette(100)) +
#   scale_x_continuous(breaks = seq(0,23,1))+
#   ggtitle("Distribution of Rides (Number of Pickups) For Entire City - Over the Week, for each Hour (Jan - June 2016)") +
#   labs(
#     x = "Hour",
#     y = "Day of the Week") +
#   theme(
#     axis.title = element_text(size=11, face="bold"),
#     axis.text = element_text(size=11, face="bold"),
#     plot.title=element_text(size=12, face="bold", hjust = 0.5),
#     legend.title = element_text(size=10, face="bold"),
#     legend.background = element_rect(colour = "black"),
#     legend.text=element_text(face="bold"),
#     # panel.grid.major = element_line(colour = "black", linetype = "dotted"),
#     axis.title.x = element_text(margin = margin(10,0,0,0)),
#     axis.title.y = element_text(margin = margin(0,10,0,0)) )

#------------------------------------------------------------------
# HEAT MAP - VISUALLY INSPECTING THE TRAFFIC (PICKUPS) DURING THE WEEK FOR EACH HOUR
#---------------------------------------------------------------------

Heat <- data[ , PerDayDemand := .N, by = list(zone,pickup_hour)]
Heat <- unique(Heat[,.(zone,pickup_hour,PerDayDemand)])

Heat.median <- Heat[ , list(MedianDemandPerWeekday = round(median(PerDayDemand))), by = list(zone,pickup_hour)]
Heat.median <- unique(Heat.median[,.(zone,pickup_hour,MedianDemandPerWeekday)])

# Heat.median <- as.data.frame(Heat.median)
# Heat.median$weekday <- as.factor(Heat.median$weekday)
# 
# Heat.median$weekday <- factor(Heat.median$weekday,
#                               levels= c("Monday", "Tuesday", "Wednesday",
#                                         "Thursday", "Friday", "Saturday","Sunday"))



windows()
hm.palette <- colorRampPalette(brewer.pal(9, 'YlOrRd'), space='Lab')
ggplot(Heat.median, aes(x= pickup_hour, y=zone, fill = MedianDemandPerWeekday)) +
  geom_tile()+
  scale_fill_gradientn("Demand",colours = hm.palette(100)) +
  scale_x_continuous(breaks = seq(0,23,1))+
  scale_y_continuous(breaks = seq(1,15,1))+
  ggtitle("Distribution of Demand For Entire City - Over the different Zones and Hour of the day (Jan - June 2016)") +
  labs(
    x = "Hour",
    y = "Zone") +
  theme(
    axis.title = element_text(size=11, face="bold"),
    axis.text = element_text(size=11, face="bold"),
    plot.title=element_text(size=12, face="bold", hjust = 0.5),
    legend.title = element_text(size=10, face="bold"),
    legend.background = element_rect(colour = "black"),
    legend.text=element_text(face="bold"),
    # panel.grid.major = element_line(colour = "black", linetype = "dotted"),
    axis.title.x = element_text(margin = margin(10,0,0,0)),
    axis.title.y = element_text(margin = margin(0,10,0,0)) )