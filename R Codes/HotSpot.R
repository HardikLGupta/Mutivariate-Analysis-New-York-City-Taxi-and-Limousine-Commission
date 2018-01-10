library(data.table)
library(ggplot2)
data <- fread("F:/BIG DATA/ISB/Assignments/Term 2/Practicum-3/Part2/data/FinalData-Part2/CleanedRawDataWithZones.csv", header = TRUE)
#str(data)

#sort(table(data$zone), decreasing = T)

#----------------------------------------------------------------------------
# SELECTING ZONE 2
#----------------------------------------------------------------------------
zonedf <- data[zone==2,]

#----------------------------------------------------------------------------
# INSPECTING THE ZONE DATA
#----------------------------------------------------------------------------
#sort(table(zonedf$pickup_hour), decreasing = T)
hourdf <- zonedf[pickup_hour==18,]


#Collecting data between time 0-30 mins in the selected zone, hour
#----------------------------------------------------------------------------
Before30 <- hourdf[pickup_time < "18:30:00",]


#Running K-means to get the hotspots
#----------------------------------------------------------------------------
Before30 <- Before30[,.(pickup_date,weekday,Dropoff_longitude,Dropoff_latitude),]

# Spherical coordinates in radians
Before30 <- Before30[,Dropoff.long.rad:=Dropoff_longitude * (2 * pi)/360]
Before30 <- Before30[,Dropoff.lat.rad:=Dropoff_latitude * (2 * pi)/360]
R <- (6378 + 6356)/2

# Cartesian coordinates
Before30 <- Before30[,x:=R * cos(Dropoff.lat.rad) * cos(Dropoff.long.rad)]
Before30 <- Before30[,y:=R * cos(Dropoff.lat.rad) * sin(Dropoff.long.rad)]
Before30 <- Before30[,z:=R * sin(Dropoff.lat.rad)]

# Perform kmeans
matrix <- Before30[, .(x, y, z)]
gc() #garbage collector
models <- list()
chs <- NULL

for (c in seq(2, 30, 1)) {
  model <- kmeans(x = matrix, center = c, nstart = 30)
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
  ggtitle("Identifying the optimal number of Dropoff Hotspots - Zone 2, Time 6 to 6:30 PM") +
  xlab("Number of clusters") + ylab("CH Index") +
  scale_x_continuous(breaks = seq(2,22,1)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_text(aes(x = support, y = ch, label=chs$support), hjust=1,vjust=-1, size = 5)


# Kmeans with the selected number of clusters
t = Sys.time()
km <- kmeans(x = matrix, center = 25, nstart = 20)
print(difftime(Sys.time(), t, units = 'mins'))

#Computing Center Data
centers <- data.frame(km$centers)

r = sqrt( I(centers$x)^2 + I(centers$y)^2 + I(centers$z)^2 )
latitude.rad = asin((centers$z)/r)
longitude.rad = atan2(centers$y, centers$x)

centers$latitude <- latitude.rad * 180/(pi)
centers$longitude <- longitude.rad * 180/(pi)
centers$cluster.size <- km$size
centers$cluster <- row.names(centers)

#Assigning Zone to the data set
Before30 <- Before30[,dropzone:=km$cluster]

getDistanceMatrix <- function(){
  listofdfs <- list()
  
  for (i in 1:nrow(centers)) {
    center <- centers[i,]
    d <- Before30[dropzone ==i,]
    #df <- as.data.frame(df)
    distances <- sqrt( (d$x - center$x)^2 + (d$y - center$y)^2 + (d$z - center$z)^2 )
    d <- cbind(d,distances)
    listofdfs[[i]] <- d
  }
  
  return(listofdfs)
}

DistanceMatrices <- getDistanceMatrix()
summary.slot <- sapply(DistanceMatrices, function(x) summary(x$distances))
summary.slot[3,]

centers$median_distance <- summary.slot[3,]
centers$mean_distance <- summary.slot[4,]

#-------------------------------------------------------------------------------
# PLOTTING
#-------------------------------------------------------------------------------

#----------------------------------------------------------------------------
#Plotting time series data for a particular hot spot to check the variation in Demand over time
#----------------------------------------------------------------------------

d <- Before30[dropzone==19,]

Aggdemand <- d[ , PickUpRides := .N, by = list(pickup_date,weekday)]
Aggdemand <- unique(Aggdemand[,.(pickup_date,weekday,PickUpRides)])
Aggdemand$pickup_date <- as.Date(Aggdemand$pickup_date)
Aggdemand$weekday <- as.factor(Aggdemand$weekday)

library(ggplot2)

windows()
ggplot(Aggdemand,
       aes(x = pickup_date,
           y = PickUpRides)) +
  geom_smooth() + 
  facet_wrap( ~ weekday, ncol=3) +
  labs(x = "Dates",
       y = "Rides",
       title = paste("Daily Rides from 55 W 116th St to 2067 5th Ave Between time 6-6:30pm")) +
  scale_x_date(date_breaks = "10 day", date_labels = "%b %d") +
  geom_line(size = 1) +
  geom_point(size = 1.5) +
  theme(
    axis.title = element_text(size=11, face="bold"),
    axis.text = element_text(size=10, face="bold"),
    plot.title=element_text(size=12, face="bold", hjust = 0.5),
    legend.title = element_text(size=10, face="bold"),
    legend.background = element_rect(colour = "black"),
    axis.text.x = element_text(angle = 90, hjust = 1),
    # panel.grid.major = element_line(colour = "black", linetype = "dotted"),
    axis.title.x = element_text(margin = margin(10,0,0,0)),
    axis.title.y = element_text(margin = margin(0,10,0,0))
  ) 

#--------------------------------------------------------------------
# EXPORTING CENTER DATA OF HOTSPOTS FOR ZONE 2 TO PLOT IN TABLEAU
#--------------------------------------------------------------------

names(centers)

hotspot_zone2_Before30 <- subset(centers, select = c("latitude", "longitude", 
                                                     "cluster.size", "median_distance", 
                                                     "mean_distance", "cluster") )

hotspot_zone2_Before30$Label <- c("")
hotspot_zone2_Before30$type <- c(1)

HotpostPickupCenter <- read.csv("F:/BIG DATA/ISB/Assignments/Term 2/Practicum-3/Part2/data/FinalData-Part2/centers.csv")
row <- HotpostPickupCenter[HotpostPickupCenter$cluster==2,c("latitude", "longitude", "cluster.size", "median_distance", "mean_distance", "cluster")]
row.names(row) <- NULL
row$Label <- c("CENTER")
row$type <- c(2)

hotspot_zone2_Before30 <- rbind(hotspot_zone2_Before30,row)

hotspot_zone2_Before30$textAddress <- mapply(FUN = function(lon, lat) revgeocode(c(lon, lat)), 
                                             hotspot_zone2_Before30$longitude, 
                                             hotspot_zone2_Before30$latitude)

hotspot_zone2_Before30$textAddress <- substr(hotspot_zone2_Before30$textAddress,1,
                                             (nchar(hotspot_zone2_Before30$textAddress)-5))

write.csv(hotspot_zone2_Before30, "F:/BIG DATA/ISB/Assignments/Term 2/Practicum-3/Part2/data/FinalData-Part2/Hotspot/zone2_Before30_hotspot1.csv", row.names = F)

