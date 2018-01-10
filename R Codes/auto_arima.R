#uploading data
dem<-read.csv("D:/Practicum/Practicum 3/Practicum3_part2/DemandBySlot2.csv",header = T)
#dem<-subset(dem,(pickup_zone==zn[1]|pickup_zone==zn[2])&(slot==ts[1]|slot==ts[2]))
#summary(dem)
#colnames(dem)
zn<-as.list(unique(dem$pickup_zone))

length(zn)
zn[1]
ts<-as.list(unique(dem$slot))
length(ts)
#library(sqldf)
library("forecast")

mape_matrix<-matrix(,nrow =length(ts) ,ncol = length(zn))
rownames(mape_matrix)<-ts
colnames(mape_matrix)<-zn

day1<-matrix(,nrow =length(ts) ,ncol = length(zn))
rownames(day1)<-ts
colnames(day1)<-zn

day2<-matrix(,nrow =length(ts) ,ncol = length(zn))
rownames(day2)<-ts
colnames(day2)<-zn

day3<-matrix(,nrow =length(ts),ncol = length(zn))
rownames(day3)<-ts
colnames(day3)<-zn

day4<-matrix(,nrow =length(ts),ncol = length(zn))
rownames(day4)<-ts
colnames(day4)<-zn

day5<-matrix(,nrow =length(ts),ncol = length(zn))
rownames(day5)<-ts
colnames(day5)<-zn

day6<-matrix(,nrow =length(ts),ncol = length(zn))
rownames(day6)<-ts
colnames(day6)<-zn

day7<-matrix(,nrow =length(ts),ncol = length(zn))
rownames(day7)<-ts
colnames(day7)<-zn

day8<-matrix(,nrow =length(ts),ncol = length(zn))
rownames(day8)<-ts
colnames(day8)<-zn

day9<-matrix(,nrow =length(ts),ncol = length(zn))
rownames(day9)<-ts
colnames(day9)<-zn

day10<-matrix(,nrow =length(ts),ncol = length(zn))
rownames(day10)<-ts
colnames(day10)<-zn

day11<-matrix(,nrow =length(ts),ncol = length(zn))
rownames(day11)<-ts
colnames(day11)<-zn

day12<-matrix(,nrow =length(ts),ncol = length(zn))
rownames(day12)<-ts
colnames(day12)<-zn

day13<-matrix(,nrow =length(ts),ncol = length(zn))
rownames(day13)<-ts
colnames(day13)<-zn

day14<-matrix(,nrow =length(ts),ncol = length(zn))
rownames(day14)<-ts
colnames(day14)<-zn



for(i in 1:length(zn) )
{
  for(j in 1:length(ts))
  { 
    data<-subset(dem,pickup_zone==zn[i]&slot==ts[j])
    datats<-ts(data[,c(5)],frequency =7)
    d<-length(datats)
    #plot.ts(datats)
    arima_mod<- auto.arima(datats)
    ord<-arimaorder(arima_mod)
    
    p1<-ord[1]
    d1<-ord[2]
    q1<-ord[3]
    p2<-ord[4]
    d2<-ord[5]
    q2<-ord[6]
    ssn<-ord[7]
    
    if (length(ord) >3)
    {data_arima<-arima(datats,order=c(p1,d1,q1), seasonal = list(order=c(p2,d2,q2),period=ssn))
    } else {
      data_arima<-arima(datats,order=c(p1,d1,q1))
    }
    
    #plot.forecast(data_arima_forecast)
    data_arima_forecast<-forecast.Arima(data_arima,h=14)
    plot(data_arima_forecast)
    
    #calculating mape
    
    #replace na in residuals if any
    data_arima_forecast$fitted[is.na(data_arima_forecast$fitted)]<-0
    m1<- abs((data_arima_forecast$x-data_arima_forecast$fitted)/data_arima_forecast$x)
    m2<-as.matrix(m1)
    mape_arima<-(colMeans(m2)/d)*100
    
    mape_matrix[j,i]<-mape_arima
    
    #storing values
    day1[j,i]<-data_arima_forecast$mean[1]
    day2[j,i]<-data_arima_forecast$mean[2]
    day3[j,i]<-data_arima_forecast$mean[3]
    day4[j,i]<-data_arima_forecast$mean[4]
    day5[j,i]<-data_arima_forecast$mean[5]
    day6[j,i]<-data_arima_forecast$mean[6]
    day7[j,i]<-data_arima_forecast$mean[7]
    
    day8[j,i]<-data_arima_forecast$mean[8]
    day9[j,i]<-data_arima_forecast$mean[9]
    day10[j,i]<-data_arima_forecast$mean[10]
    day11[j,i]<-data_arima_forecast$mean[11]
    day12[j,i]<-data_arima_forecast$mean[12]
    day13[j,i]<-data_arima_forecast$mean[13]
    day14[j,i]<-data_arima_forecast$mean[14]
    
    
    message("j is- ",j)
  }
  message("i is-",i) 
}

delim1<-matrix("day1",nrow=1,ncol = length(zn))
delim2<-matrix("day2",nrow=1,ncol = length(zn))
delim3<-matrix("day3",nrow=1,ncol = length(zn))
delim4<-matrix("day4",nrow=1,ncol = length(zn))
delim5<-matrix("day5",nrow=1,ncol = length(zn))
delim6<-matrix("day6",nrow=1,ncol = length(zn))
delim7<-matrix("day7",nrow=1,ncol = length(zn))
delim8<-matrix("day8",nrow=1,ncol = length(zn))
delim9<-matrix("day9",nrow=1,ncol = length(zn))
delim10<-matrix("day10",nrow=1,ncol = length(zn))
delim11<-matrix("day11",nrow=1,ncol = length(zn))
delim12<-matrix("day12",nrow=1,ncol = length(zn))
delim13<-matrix("day13",nrow=1,ncol = length(zn))
delim14<-matrix("day14",nrow=1,ncol = length(zn))

delim0<-matrix("mape_values",nrow=1,ncol = length(zn))

forecast_fin<-rbind(delim0,mape_matrix ,delim1,day1,delim2,day2,delim3,day3,delim4,day4,delim5,day5,delim6,day6,delim7,day7,
                    delim8,day8,delim9,day9,delim10,day10,delim11,day11,delim12,day12,delim13,day13,delim14,day14) 

write.csv(forecast_fin, file = "D:/Practicum/Practicum 3/Practicum3_part2/arima_output.csv",row.names=TRUE)

