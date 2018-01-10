library(data.table)
data <- fread("F:/BIG DATA/ISB/Assignments/Term 2/Practicum-3/Part2/data/FinalData-Part2/CleanedRawDataWithZones.csv", header = TRUE)
#str(data)

#-----------------------------------------
# AGGREGATING DEMAND
#-----------------------------------------
Aggdemand <- data[ , PickUpRides := .N, by = list(pickup_date,weekday,zone,slot)]
Aggdemand <- unique(Aggdemand[,.(pickup_date,weekday,zone,slot,PickUpRides)])

# outFile <- "F:/BIG DATA/ISB/Assignments/Term 2/Practicum-3/Part2/data/FinalData-Part2/DemandBySlot.csv"
# fwrite(Aggdemand, outFile)


#-----------------------------------------
# MEDIAN DEMAND BY EACH ZONE AND SLOT
#-----------------------------------------

Mediandemand <- Aggdemand[ , list(MedianDemandPerWeekday = round(median(PickUpRides))), by = list(weekday,zone,slot)]
Mediandemand <- unique(Mediandemand[,.(weekday,zone,slot,MedianDemandPerWeekday)])

getDemand <- function(day) {
  
  DemandMatrixrix <- matrix(NA,6,15)
  
  for (i in 1:15) {
    for (j in 1:6) {
      if (length(Mediandemand[weekday == day & zone == i & slot == j, ]$MedianDemandPerWeekday) != 0) {
        DemandMatrixrix[j, i] <- Mediandemand[weekday == day & zone == i & slot == j, ]$MedianDemandPerWeekday
      } else{
        DemandMatrixrix[j, i] <- NA
      }
      
    }
  }
  
  return(DemandMatrixrix)
}

MedianDemandMatrixrix_Mon <- getDemand("Monday")
MedianDemandMatrixrix_Tue <- getDemand("Tuesday")
MedianDemandMatrixrix_Wed <- getDemand("Wednesday")
MedianDemandMatrixrix_Thu <- getDemand("Thursday")
MedianDemandMatrixrix_Fri <- getDemand("Friday")
MedianDemandMatrixrix_Sat <- getDemand("Saturday")
MedianDemandMatrixrix_Sun <- getDemand("Sunday")

writeDemand <- function(mat,day){
  colnames(mat) <- c("Zone1","Zone2","Zone3","Zone4","Zone5","Zone6","Zone7","Zone8","Zone9","Zone10","Zone11","Zone12","Zone13","Zone14","Zone15")
  rownames(mat) <- c("slot1","slot2","slot3","slot4","slot5","slot6")
  
  outfile <- paste("F:/BIG DATA/ISB/Assignments/Term 2/Practicum-3/Part2/data/FinalData-Part2/MedianDemandsMatrix/MedianDemand_",day,sep = "")
  outfile <- paste(outfile,".csv",sep = "")
  
  write.csv(mat,file=outfile) 
  
}

writeDemand(MedianDemandMatrixrix_Mon,"Mon")
writeDemand(MedianDemandMatrixrix_Tue,"Tue")
writeDemand(MedianDemandMatrixrix_Wed,"Wed")
writeDemand(MedianDemandMatrixrix_Thu,"Thu")
writeDemand(MedianDemandMatrixrix_Fri,"Fri")
writeDemand(MedianDemandMatrixrix_Sat,"Sat")
writeDemand(MedianDemandMatrixrix_Sun,"Sun")


#---------------------------------------------------------------------
# AVERAGE TRIP DURATION PER TRIP
#---------------------------------------------------------------------

#summary(data$TripDuration)
# AvgTripAvgTripMatrixrix <- matrix(NA,6,15)
# 
# for(i in 1:15){
#   for(j in 1:6){
#     df <- data[zone == i & slot == j,]
#     df <- data[zone == 5 & slot == 6,]
#     df <- df[TripDuration <100,]
#     hist <- hist(df$TripDuration, breaks=seq(0, 100, by = 5), freq=TRUE)
#     AvgTripMatrix[j,i] <- hist$mids[which.max(hist$counts)]
#   }
# }
# 
# colnames(AvgTripMatrix) <- c("Zone1","Zone2","Zone3","Zone4","Zone5","Zone6","Zone7","Zone8","Zone9","Zone10","Zone11","Zone12","Zone13","Zone14","Zone15")
# rownames(AvgTripMatrix) <- c("slot1","slot2","slot3","slot4","slot5","slot6")
# 
# write.csv(AvgTripMatrix,file="F:/BIG DATA/ISB/Assignments/Term 2/Practicum-3/Part2/data/FinalData-Part2/TripDuration.csv") 


getAvgTrip <- function(day) {
  
  TripMatrix <- matrix(NA,6,15)
  
  for (i in 1:15) {
    for (j in 1:6) {
      
      d <- data[weekday == day & zone == i & slot == j,]
      
      if (nrow(d) >  0) {
        TripMatrix[j,i] <- median(d$TripDuration)
      } else{
        TripMatrix[j, i] <- NA
      }
    }
  }
  
  return(TripMatrix)
}

AvgTrip_Mon <- getAvgTrip("Monday")
AvgTrip_Tue <- getAvgTrip("Tuesday")
AvgTrip_Wed <- getAvgTrip("Wednesday")
AvgTrip_Thu <- getAvgTrip("Thursday")
AvgTrip_Fri <- getAvgTrip("Friday")
AvgTrip_Sat <- getAvgTrip("Saturday")
AvgTrip_Sun <- getAvgTrip("Sunday")

writeDemand <- function(mat,day){
  colnames(mat) <- c("Zone1","Zone2","Zone3","Zone4","Zone5","Zone6","Zone7","Zone8","Zone9","Zone10","Zone11","Zone12","Zone13","Zone14","Zone15")
  rownames(mat) <- c("slot1","slot2","slot3","slot4","slot5","slot6")
  
  outfile <- paste("F:/BIG DATA/ISB/Assignments/Term 2/Practicum-3/Part2/data/FinalData-Part2/MedianTripDurationMatrix/MedianTD_",day,sep = "")
  outfile <- paste(outfile,".csv",sep = "")
  
  write.csv(mat,file=outfile) 
  
}


writeDemand(AvgTrip_Mon,"Mon")
writeDemand(AvgTrip_Tue,"Tue")
writeDemand(AvgTrip_Wed,"Wed")
writeDemand(AvgTrip_Thu,"Thu")
writeDemand(AvgTrip_Fri,"Fri")
writeDemand(AvgTrip_Sat,"Sat")
writeDemand(AvgTrip_Sun,"Sun")

#-----------------------------------------
# MEDIAN NUMBER OF CABS
#-----------------------------------------

getCabs <- function(demand,tripduration) {
  
  CabMatrix <- matrix(NA,6,15)
  
  for (i in 1:15) {
    for (j in 1:6) {
      if (!is.na(demand[j,i]) | !is.na(tripduration[j,i])) {
        CabMatrix[j,i] <- round(demand[j,i]/tripduration[j,i])
      } else{
        CabMatrix[j, i] <- NA
      }
    }
  }
  
  return(CabMatrix)
}

Cabs_Mon <- getCabs(MedianDemandMatrixrix_Mon, AvgTrip_Mon)
Cabs_Tue <- getCabs(MedianDemandMatrixrix_Tue, AvgTrip_Tue)
Cabs_Wed <- getCabs(MedianDemandMatrixrix_Wed, AvgTrip_Wed)
Cabs_Thu <- getCabs(MedianDemandMatrixrix_Thu, AvgTrip_Thu)
Cabs_Fri <- getCabs(MedianDemandMatrixrix_Fri, AvgTrip_Fri)
Cabs_Sat <- getCabs(MedianDemandMatrixrix_Sat, AvgTrip_Sat)
Cabs_Sun <- getCabs(MedianDemandMatrixrix_Sun, AvgTrip_Sun)

writeDemand <- function(mat,day){
  colnames(mat) <- c("Zone1","Zone2","Zone3","Zone4","Zone5","Zone6","Zone7","Zone8","Zone9","Zone10","Zone11","Zone12","Zone13","Zone14","Zone15")
  rownames(mat) <- c("slot1","slot2","slot3","slot4","slot5","slot6")
  
  outfile <- paste("F:/BIG DATA/ISB/Assignments/Term 2/Practicum-3/Part2/data/FinalData-Part2/MedianCabMatrix/MedianCab_",day,sep = "")
  outfile <- paste(outfile,".csv",sep = "")
  
  write.csv(mat,file=outfile) 
  
}


writeDemand(Cabs_Mon,"Mon")
writeDemand(Cabs_Tue,"Tue")
writeDemand(Cabs_Wed,"Wed")
writeDemand(Cabs_Thu,"Thu")
writeDemand(Cabs_Fri,"Fri")
writeDemand(Cabs_Sat,"Sat")
writeDemand(Cabs_Sun,"Sun")

#-----------------------------------------
# TOTAL REVENUE PER DAY, MEDIAN REVENUE PER DAY OF THE WEEK
#-----------------------------------------

revenue <- data[ , PerDayTotalRevenue := sum(Fare_amount), by = list(pickup_date,weekday,zone,slot)]
revenue <- unique(revenue[,.(pickup_date,weekday,zone,slot,PerDayTotalRevenue)])

MedianRevenue <- revenue[ , list(MedianRevenue = median(PerDayTotalRevenue)), by = list(weekday,zone,slot)]
MedianRevenue <- unique(MedianRevenue[,.(weekday,zone,slot,MedianRevenue)])

#-----------------------------------------
# REVENUE PER CAB
#-----------------------------------------

getRevenuePerCab <- function(cabs,day) {
  
  RevenuePerCabMatrix <- matrix(NA,6,15)
  
  for (i in 1:15) {
    for (j in 1:6) {
      
      rev <- as.numeric(MedianRevenue[weekday == day & zone == i & slot == j, "MedianRevenue"])
      c <- cabs[j,i]
      
      if (!is.na(rev) | !is.na(cabs[j,i]) ) {
        RevenuePerCabMatrix[j,i] <- round(rev/c)
        
      } else{
        RevenuePerCabMatrix[j, i] <- NA
      }
    }
  }
  
  return(RevenuePerCabMatrix)
}

RevenuePerCab_Mon <- getRevenuePerCab(Cabs_Mon, "Monday")
RevenuePerCab_Tue <- getRevenuePerCab(Cabs_Tue, "Tuesday")
RevenuePerCab_Wed <- getRevenuePerCab(Cabs_Wed, "Wednesday")
RevenuePerCab_Thu <- getRevenuePerCab(Cabs_Thu, "Thursday")
RevenuePerCab_Fri <- getRevenuePerCab(Cabs_Fri, "Friday")
RevenuePerCab_Sat <- getRevenuePerCab(Cabs_Sat, "Saturday")
RevenuePerCab_Sun <- getRevenuePerCab(Cabs_Sun, "Sunday")



writeDemand <- function(mat,day){
  colnames(mat) <- c("Zone1","Zone2","Zone3","Zone4","Zone5","Zone6","Zone7","Zone8","Zone9","Zone10","Zone11","Zone12","Zone13","Zone14","Zone15")
  rownames(mat) <- c("slot1","slot2","slot3","slot4","slot5","slot6")
  
  outfile <- paste("F:/BIG DATA/ISB/Assignments/Term 2/Practicum-3/Part2/data/FinalData-Part2/RevenuePerCab/RevenuePerCab_",day,sep = "")
  outfile <- paste(outfile,".csv",sep = "")
  
  write.csv(mat,file=outfile) 
  
}


writeDemand(RevenuePerCab_Mon,"Mon")
writeDemand(RevenuePerCab_Tue,"Tue")
writeDemand(RevenuePerCab_Wed,"Wed")
writeDemand(RevenuePerCab_Thu,"Thu")
writeDemand(RevenuePerCab_Fri,"Fri")
writeDemand(RevenuePerCab_Sat,"Sat")
writeDemand(RevenuePerCab_Sun,"Sun")


#-------------------------------------------------------------------
# REVENUE PER RIDE
#-------------------------------------------------------------------

getRevenuePerRide <- function(numberofPickups,day) {
  
  RevenuePerRideMatrix <- matrix(NA,6,15)
  
  for (i in 1:15) {
    for (j in 1:6) {
      
      rev <- as.numeric(MedianRevenue[weekday == day & zone == i & slot == j, "MedianRevenue"])
      c <- numberofPickups[j,i]
      
      if (!is.na(rev) | !is.na(c) ) {
        RevenuePerRideMatrix[j,i] <- round(rev/c)
        
      } else{
        RevenuePerRideMatrix[j, i] <- NA
      }
    }
  }
  
  return(RevenuePerRideMatrix)
}

RevenuePerRide_Mon <- getRevenuePerRide(MedianDemandMatrixrix_Mon, "Monday")
RevenuePerRide_Tue <- getRevenuePerRide(MedianDemandMatrixrix_Tue, "Tuesday")
RevenuePerRide_Wed <- getRevenuePerRide(MedianDemandMatrixrix_Wed, "Wednesday")
RevenuePerRide_Thu <- getRevenuePerRide(MedianDemandMatrixrix_Thu, "Thursday")
RevenuePerRide_Fri <- getRevenuePerRide(MedianDemandMatrixrix_Fri, "Friday")
RevenuePerRide_Sat <- getRevenuePerRide(MedianDemandMatrixrix_Sat, "Saturday")
RevenuePerRide_Sun <- getRevenuePerRide(MedianDemandMatrixrix_Sun, "Sunday")



writeDemand <- function(mat,day){
  colnames(mat) <- c("Zone1","Zone2","Zone3","Zone4","Zone5","Zone6","Zone7","Zone8","Zone9","Zone10","Zone11","Zone12","Zone13","Zone14","Zone15")
  rownames(mat) <- c("slot1","slot2","slot3","slot4","slot5","slot6")
  
  outfile <- paste("F:/BIG DATA/ISB/Assignments/Term 2/Practicum-3/Part2/data/FinalData-Part2/RevenuePerRide/RevenuePerRide_",day,sep = "")
  outfile <- paste(outfile,".csv",sep = "")
  
  write.csv(mat,file=outfile) 
  
}


writeDemand(RevenuePerRide_Mon,"Mon")
writeDemand(RevenuePerRide_Tue,"Tue")
writeDemand(RevenuePerRide_Wed,"Wed")
writeDemand(RevenuePerRide_Thu,"Thu")
writeDemand(RevenuePerRide_Fri,"Fri")
writeDemand(RevenuePerRide_Sat,"Sat")
writeDemand(RevenuePerRide_Sun,"Sun")
