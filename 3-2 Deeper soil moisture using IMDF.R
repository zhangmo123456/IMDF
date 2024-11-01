##Deeper soil moisture mapping and 
##validation based on the IMDF

library(sp)
library(lattice)
library(rgdal)
library(raster)
library(maptools)
library(shapefiles)
library(corrplot)
library(gstat)
library(raster)
library(automap)
library(MASS)
library(soiltexture)
library(aqp)
library(plyr)
library(ithir)
library(hydroGOF)
library(ggplot2)
library(ggsci)
library(reshape2)
library(ncdf4) 
library(maptools) 
library(terra)
library(randomForest)
library(caret)

setwd("~/3 deep prediction/2 map")
df<-read.csv("DSM_file_all.csv")
df<-na.omit(df)
#Build a random forest model to spatially predict soil moisture in each layer
df$SM_5_10<-rowMeans(df[,6:10])
df$SM_10_20<-rowMeans(df[,11:20])
df$SM_17_18<-rowMeans(df[,17:18])

Model_5_10<-train(df[,28:56],df[,'SM_5_10'],method='rf',ntree=1000,importance=T,
                  trControl = trainControl(method = "none"))

Model_10_20<-train(df[,28:56],df[,'SM_10_20'],method='rf',ntree=1000,importance=T,
                  trControl = trainControl(method = "none"))

Model_17_18<-train(df[,28:56],df[,'SM_17_18'],method='rf',ntree=1000,importance=T,
                  trControl = trainControl(method = "none"))

time1<-Sys.time()

for(ss in 1: 122){

path<-"~/ECdata_1km/EC_no_Time"
ECfile_1km_no_time <- dir(path, pattern = ".tif$", full.names = TRUE)
for(i in 1 : length(ECfile_1km_no_time)){
  assign(paste("ec", i, sep = ""),raster(ECfile_1km_no_time[i]))
}
ec_raster_1km<-ec1
for(j in 2: length(ECfile_1km_no_time)){
  ec_raster_1km<-stack(ec_raster_1km,get(paste("ec",j,sep="")))
}

#Merge 1km other daily environment variables---------
path.lstd.1km<-"~ECdata_1km/EC_with_Time/LST_D"
lstd.file <- dir(path.lstd.1km, pattern = ".tif$", full.names = TRUE)
LST_D<-raster(lstd.file[ss])
LST_D@data@names<-"LST_D"
ec_raster_1km<-stack(ec_raster_1km,LST_D)

path.lstn.1km<-"~/ECdata_1km/EC_with_Time/LST_N"
lstn.file <- dir(path.lstn.1km, pattern = ".tif$", full.names = TRUE)
LST_N<-raster(lstn.file[ss])
LST_N@data@names<-"LST_N"
ec_raster_1km<-stack(ec_raster_1km,LST_N)

path.ndvi.1km<-"~/ECdata_1km/EC_with_Time/NDVI"
ndvi.file <- dir(path.ndvi.1km, pattern = ".tif$", full.names = TRUE)
NDVI<-raster(ndvi.file[ss])
NDVI@data@names<-"NDVI"
ec_raster_1km<-stack(ec_raster_1km,NDVI)

path.prcp.1km<-"~/ECdata_1km/EC_with_Time/Prcp"
prcp.file <- dir(path.prcp.1km, pattern = ".tif$", full.names = TRUE)
Prcp<-raster(prcp.file[ss])
Prcp@data@names<-"Prcp"
ec_raster_1km<-stack(ec_raster_1km,Prcp)

Pre_SM_5_10 <- predict(ec_raster_1km, Model_5_10)
print(paste("5-10 in ",ss,"is done"))
Pre_SM_10_20 <- predict(ec_raster_1km, Model_10_20)
print(paste("10-20 in ",ss,"is done"))
Pre_SM_17_18 <- predict(ec_raster_1km, Model_17_18)
print(paste("17-18 in ",ss,"is done"))
print("-------------------------------------------------------")

part.name<-  substring(prcp.file[ss],100,107)
output_SM_5_10<-paste("SM_5_10_",part.name,".tif",sep="")
output_SM_10_20<-paste("SM_10_20_",part.name,".tif",sep="")
output_SM_17_18<-paste("SM_17_18_",part.name,".tif",sep="")

writeRaster(Pre_SM_5_10,output_SM_5_10)
writeRaster(Pre_SM_10_20,output_SM_10_20)
writeRaster(Pre_SM_17_18,output_SM_17_18)
}

#========================================
#This version uses IMDF to perform sub-layer 
#soil moisture accuracy assessment (5-10cm)

ubrmse<-function(a,b){
(sum(((a-mean(a))-(b-mean(b)))^2)/length(a))^0.5
}
nse<-function(pre, obs){
1-(sum((obs-pre)^2)/sum((obs-mean(obs))^2))
}

k.process<-30
seed=c(1:k.process)
result_df<-data.frame(Pre=0 , Obs=0 , Day=0 ,Site=0 ,Network=0 ,Process=0)
setwd("~/3 deep prediction/1 validation")
df<-read.csv("DSM_file_all.csv")
df$SM_5_10<-rowMeans(df[,6:10])
df$SM_10_20<-rowMeans(df[,11:20])

for (i.validation in 1:k.process){

df_temp<-subset(df,Day==1)
df_temp_1<-subset(df_temp,Network=="Naqu")
df_temp_2<-subset(df_temp,Network=="Pagri")
df_temp_3<-subset(df_temp,Network=="uHRB")

set.seed(seed[i.validation])
index_1<-sample(1:nrow(df_temp_1),replace=F,0.6*nrow(df_temp_1))
index_2<-sample(1:nrow(df_temp_2),replace=F,0.6*nrow(df_temp_2))
index_3<-sample(1:nrow(df_temp_3),replace=F,0.6*nrow(df_temp_3))

train_site_1<-df_temp_1[index_1,]$Site
train_site_2<-df_temp_2[index_2,]$Site
train_site_3<-df_temp_3[index_3,]$Site

test_site_1<-df_temp_1[-index_1,]$Site
test_site_2<-df_temp_2[-index_2,]$Site
test_site_3<-df_temp_3[-index_3,]$Site

train_site<-c(train_site_1,train_site_2,train_site_3)
test_site<-c(test_site_1,test_site_2,test_site_3)
#point.train<- subset(df_temp, Site %in% train_site)
point.train<- subset(df, Site %in% train_site)
point.train<-na.omit(point.train)

## Random Forest Modeling
Model_5_10<-train(point.train[,28:56],point.train[,'SM_5_10'],method='rf',ntree=1000,importance=T,
                  trControl = trainControl(method = "none"))

##--After unified modeling, the results are obtained every day

for(i.day in 1:122){

df_temp<-subset(df,Day==i.day)
point.test<- subset(df_temp,Site %in% test_site)

pre_0_5 <- predict.train(Model_5_10,newdata=point.test[,28:56])
obs_5_10<-point.test$obs_10cm
result_df_temp<-data.frame(Pre=pre_0_5,Obs=obs_5_10,Day=point.test$Day,
                                      Site=point.test$Site,Network=point.test$Network,
                                      Process=rep(i.validation,length(pre_0_5)))
result_df<-rbind(result_df,result_df_temp)
print(paste("the",i.day,"day &",i.validation, "process..."))
}
}
result_df<-result_df[-1,]
#result_df<-na.omit(result_df)
write.csv(result_df, "validation_result_0426_5_10.csv")

##Calculate ME RMSE ubRMSE NSE

indicator_all<-data.frame(ME=0,RMSE=0,ubRMSE=0,Day=0,Porcess=0)
indicator_Naqu<-data.frame(ME=0,RMSE=0,ubRMSE=0,Day=0,Porcess=0)
indicator_Pagri<-data.frame(ME=0,RMSE=0,ubRMSE=0,Day=0,Porcess=0)
indicator_uHRB<-data.frame(ME=0,RMSE=0,ubRMSE=0,Day=0,Porcess=0)

for(i in 1:122){##day=122
vali_day<-subset(result_df,Day==i)
for(j in 1:30){##process=30
vali_day_process<-subset(vali_day,Process==j)
vali_day_process_Naqu<-subset(vali_day_process,Network=="Naqu")
vali_day_process_Pagri<-subset(vali_day_process,Network=="Pagri")
vali_day_process_uHRB<-subset(vali_day_process,Network=="uHRB")
vali_day_process<-na.omit(vali_day_process)
vali_day_process_Naqu<-na.omit(vali_day_process_Naqu)
vali_day_process_Pagri<-na.omit(vali_day_process_Pagri)
vali_day_process_uHRB<-na.omit(vali_day_process_uHRB)

ME_all_temp<-me(vali_day_process$Pre,vali_day_process$Obs)
RMSE_all_temp<-rmse(vali_day_process$Pre,vali_day_process$Obs)
ubRMSE_all_temp<-ubrmse(vali_day_process$Pre,vali_day_process$Obs)

ME_Naqu_temp<-me(vali_day_process_Naqu$Pre,vali_day_process_Naqu$Obs)
RMSE_Naqu_temp<-rmse(vali_day_process_Naqu$Pre,vali_day_process_Naqu$Obs)
ubRMSE_Naqu_temp<-ubrmse(vali_day_process_Naqu$Pre,vali_day_process_Naqu$Obs)

ME_Pagri_temp<-me(vali_day_process_Pagri$Pre,vali_day_process_Pagri$Obs)
RMSE_Pagri_temp<-rmse(vali_day_process_Pagri$Pre,vali_day_process_Pagri$Obs)
ubRMSE_Pagri_temp<-ubrmse(vali_day_process_Pagri$Pre,vali_day_process_Pagri$Obs)

ME_uHRB_temp<-me(vali_day_process_uHRB$Pre,vali_day_process_uHRB$Obs)
RMSE_uHRB_temp<-rmse(vali_day_process_uHRB$Pre,vali_day_process_uHRB$Obs)
ubRMSE_uHRB_temp<-ubrmse(vali_day_process_uHRB$Pre,vali_day_process_uHRB$Obs)

indicator_all<-rbind(indicator_all,c(ME_all_temp,RMSE_all_temp,ubRMSE_all_temp,i,j))
indicator_Naqu<-rbind(indicator_Naqu,c(ME_Naqu_temp,RMSE_Naqu_temp,ubRMSE_Naqu_temp,i,j))
indicator_Pagri<-rbind(indicator_Pagri,c(ME_Pagri_temp,RMSE_Pagri_temp,ubRMSE_Pagri_temp,i,j))
indicator_uHRB<-rbind(indicator_uHRB,c(ME_uHRB_temp,RMSE_uHRB_temp,ubRMSE_uHRB_temp,i,j))
}
}
indicator_all<-indicator_all[-1,]
indicator_Naqu<-indicator_Naqu[-1,]
indicator_Pagri<-indicator_Pagri[-1,]
indicator_uHRB<-indicator_uHRB[-1,]

nse_temp<-data.frame(NSE=0,Site=0,Process=0,Network=0)
for(i in 1:30){#process=30

vali_process<-subset(result_df,Process==i)
site_each<-levels(as.factor(vali_process$Site))

for(j in 1:length(site_each)){

vali_process_site<-subset(vali_process,Site==site_each[j])
network_temp<-levels(as.factor(vali_process_site$Network))
vali_process_site<-na.omit(vali_process_site)
nse_temp_site<-nse(vali_process_site$Pre,vali_process_site$Obs)
nse_temp<-rbind(nse_temp,c(nse_temp_site,site_each[j],i,network_temp))

}
}
nse_temp<-nse_temp[-1,]
nse_temp$NSE<-round(as.numeric(nse_temp$NSE),3)
nse_temp <- nse_temp[!is.nan(nse_temp$NSE), ]
nse_temp <- nse_temp[!is.infinite(nse_temp$NSE), ]

indicator_NSE<-data.frame(NSE_all=0,NSE_Naqu=0,NSE_Pagri=0,NSE_uHRB=0,Porcess=0)
for(i in 1:30){##process=30

indicator_NSE_temp<-subset(nse_temp,Process==i)
indicator_NSE_temp_Naqu<-subset(indicator_NSE_temp,Network=="Naqu")
indicator_NSE_temp_Pagri<-subset(indicator_NSE_temp,Network=="Pagri")
indicator_NSE_temp_uHRB<-subset(indicator_NSE_temp,Network=="uHRB")
NSE_mean_all<-mean(indicator_NSE_temp$NSE)
NSE_mean_Naqu<-mean(indicator_NSE_temp_Naqu$NSE)
NSE_mean_Pagri<-mean(indicator_NSE_temp_Pagri$NSE)
NSE_mean_uHRB<-mean(indicator_NSE_temp_uHRB$NSE)

indicator_NSE<-rbind(indicator_NSE,c(NSE_mean_all,NSE_mean_Naqu,
                                        NSE_mean_Pagri,NSE_mean_uHRB,i))
}
indicator_NSE<-indicator_NSE[-1,]

write.csv(indicator_all, "validation_all_0426_5_10.csv")
write.csv(indicator_Naqu, "validation_Naqu_0426_5_10.csv")
write.csv(indicator_Pagri, "validation_Pagri_0426_5_10.csv")
write.csv(indicator_uHRB, "validation_uHRB_0426_5_10.csv")
write.csv(indicator_NSE, "validation_NSE_0426_5_10.csv")

#========================================
#This version uses IMDF to perform sub-layer 
#soil moisture accuracy assessment (10-20cm)

k.process<-30
seed=c(1:k.process)
result_df<-data.frame(Pre=0 , Obs=0 , Day=0 ,Site=0 ,Network=0 ,Process=0)
setwd("~/3 deep prediction/1 validation")
df<-read.csv("DSM_file_all.csv")
df$SM_5_10<-rowMeans(df[,6:10])
df$SM_10_20<-rowMeans(df[,11:20])

for (i.validation in 1:k.process){# Repeat the verification cycle 30 times--------

df_temp<-subset(df,Day==1)
df_temp_1<-subset(df_temp,Network=="Naqu")
df_temp_2<-subset(df_temp,Network=="Pagri")
df_temp_3<-subset(df_temp,Network=="uHRB")

set.seed(seed[i.validation])
index_1<-sample(1:nrow(df_temp_1),replace=F,0.6*nrow(df_temp_1))
index_2<-sample(1:nrow(df_temp_2),replace=F,0.6*nrow(df_temp_2))
index_3<-sample(1:nrow(df_temp_3),replace=F,0.6*nrow(df_temp_3))

train_site_1<-df_temp_1[index_1,]$Site
train_site_2<-df_temp_2[index_2,]$Site
train_site_3<-df_temp_3[index_3,]$Site

test_site_1<-df_temp_1[-index_1,]$Site
test_site_2<-df_temp_2[-index_2,]$Site
test_site_3<-df_temp_3[-index_3,]$Site

train_site<-c(train_site_1,train_site_2,train_site_3)
test_site<-c(test_site_1,test_site_2,test_site_3)
#point.train<- subset(df_temp, Site %in% train_site)
point.train<- subset(df, Site %in% train_site)#Using unified modeling approach
point.train<-na.omit(point.train)
## Random Forest Modeling

Model_10_20<-train(point.train[,28:56],point.train[,'SM_10_20'],method='rf',ntree=1000,importance=T,
                  trControl = trainControl(method = "none"))

##----After unified modeling, the results are obtained every day
#i.day=1
for(i.day in 1:122){
#for(i.day in 1:2){

df_temp<-subset(df,Day==i.day)
point.test<- subset(df_temp,Site %in% test_site)

pre_0_5 <- predict.train(Model_10_20,newdata=point.test[,28:56])
obs_10_20<-point.test$obs_20cm
result_df_temp<-data.frame(Pre=pre_0_5,Obs=obs_10_20,Day=point.test$Day,
                                      Site=point.test$Site,Network=point.test$Network,
                                      Process=rep(i.validation,length(pre_0_5)))
result_df<-rbind(result_df,result_df_temp)
print(paste("the",i.day,"day &",i.validation, "process..."))
}
}
result_df<-result_df[-1,]
#result_df<-na.omit(result_df)
write.csv(result_df, "validation_result_0426_10_20.csv")

##Calculate ME RMSE ubRMSE NSE

indicator_all<-data.frame(ME=0,RMSE=0,ubRMSE=0,Day=0,Porcess=0)
indicator_Naqu<-data.frame(ME=0,RMSE=0,ubRMSE=0,Day=0,Porcess=0)
indicator_Pagri<-data.frame(ME=0,RMSE=0,ubRMSE=0,Day=0,Porcess=0)
indicator_uHRB<-data.frame(ME=0,RMSE=0,ubRMSE=0,Day=0,Porcess=0)

for(i in 1:122){##day=122
vali_day<-subset(result_df,Day==i)
for(j in 1:30){##process=30
vali_day_process<-subset(vali_day,Process==j)
vali_day_process_Naqu<-subset(vali_day_process,Network=="Naqu")
vali_day_process_Pagri<-subset(vali_day_process,Network=="Pagri")
vali_day_process_uHRB<-subset(vali_day_process,Network=="uHRB")
vali_day_process<-na.omit(vali_day_process)
vali_day_process_Naqu<-na.omit(vali_day_process_Naqu)
vali_day_process_Pagri<-na.omit(vali_day_process_Pagri)
vali_day_process_uHRB<-na.omit(vali_day_process_uHRB)

ME_all_temp<-me(vali_day_process$Pre,vali_day_process$Obs)
RMSE_all_temp<-rmse(vali_day_process$Pre,vali_day_process$Obs)
ubRMSE_all_temp<-ubrmse(vali_day_process$Pre,vali_day_process$Obs)

ME_Naqu_temp<-me(vali_day_process_Naqu$Pre,vali_day_process_Naqu$Obs)
RMSE_Naqu_temp<-rmse(vali_day_process_Naqu$Pre,vali_day_process_Naqu$Obs)
ubRMSE_Naqu_temp<-ubrmse(vali_day_process_Naqu$Pre,vali_day_process_Naqu$Obs)

ME_Pagri_temp<-me(vali_day_process_Pagri$Pre,vali_day_process_Pagri$Obs)
RMSE_Pagri_temp<-rmse(vali_day_process_Pagri$Pre,vali_day_process_Pagri$Obs)
ubRMSE_Pagri_temp<-ubrmse(vali_day_process_Pagri$Pre,vali_day_process_Pagri$Obs)

ME_uHRB_temp<-me(vali_day_process_uHRB$Pre,vali_day_process_uHRB$Obs)
RMSE_uHRB_temp<-rmse(vali_day_process_uHRB$Pre,vali_day_process_uHRB$Obs)
ubRMSE_uHRB_temp<-ubrmse(vali_day_process_uHRB$Pre,vali_day_process_uHRB$Obs)

indicator_all<-rbind(indicator_all,c(ME_all_temp,RMSE_all_temp,ubRMSE_all_temp,i,j))
indicator_Naqu<-rbind(indicator_Naqu,c(ME_Naqu_temp,RMSE_Naqu_temp,ubRMSE_Naqu_temp,i,j))
indicator_Pagri<-rbind(indicator_Pagri,c(ME_Pagri_temp,RMSE_Pagri_temp,ubRMSE_Pagri_temp,i,j))
indicator_uHRB<-rbind(indicator_uHRB,c(ME_uHRB_temp,RMSE_uHRB_temp,ubRMSE_uHRB_temp,i,j))
}
}
indicator_all<-indicator_all[-1,]
indicator_Naqu<-indicator_Naqu[-1,]
indicator_Pagri<-indicator_Pagri[-1,]
indicator_uHRB<-indicator_uHRB[-1,]

nse_temp<-data.frame(NSE=0,Site=0,Process=0,Network=0)
for(i in 1:30){#process=30

vali_process<-subset(result_df,Process==i)
site_each<-levels(as.factor(vali_process$Site))

for(j in 1:length(site_each)){

vali_process_site<-subset(vali_process,Site==site_each[j])
network_temp<-levels(as.factor(vali_process_site$Network))
vali_process_site<-na.omit(vali_process_site)
nse_temp_site<-nse(vali_process_site$Pre,vali_process_site$Obs)
nse_temp<-rbind(nse_temp,c(nse_temp_site,site_each[j],i,network_temp))

}
}
nse_temp<-nse_temp[-1,]
nse_temp$NSE<-round(as.numeric(nse_temp$NSE),3)
nse_temp <- nse_temp[!is.nan(nse_temp$NSE), ]
nse_temp <- nse_temp[!is.infinite(nse_temp$NSE), ]

indicator_NSE<-data.frame(NSE_all=0,NSE_Naqu=0,NSE_Pagri=0,NSE_uHRB=0,Porcess=0)
for(i in 1:30){##process=30

indicator_NSE_temp<-subset(nse_temp,Process==i)
indicator_NSE_temp_Naqu<-subset(indicator_NSE_temp,Network=="Naqu")
indicator_NSE_temp_Pagri<-subset(indicator_NSE_temp,Network=="Pagri")
indicator_NSE_temp_uHRB<-subset(indicator_NSE_temp,Network=="uHRB")
NSE_mean_all<-mean(indicator_NSE_temp$NSE)
NSE_mean_Naqu<-mean(indicator_NSE_temp_Naqu$NSE)
NSE_mean_Pagri<-mean(indicator_NSE_temp_Pagri$NSE)
NSE_mean_uHRB<-mean(indicator_NSE_temp_uHRB$NSE)

indicator_NSE<-rbind(indicator_NSE,c(NSE_mean_all,NSE_mean_Naqu,
                                        NSE_mean_Pagri,NSE_mean_uHRB,i))
}
indicator_NSE<-indicator_NSE[-1,]
write.csv(indicator_all, "validation_all_0426_10_20.csv")
write.csv(indicator_Naqu, "validation_Naqu_0426_10_20.csv")
write.csv(indicator_Pagri, "validation_Pagri_0426_10_20.csv")
write.csv(indicator_uHRB, "validation_uHRB_0426_10_20.csv")
write.csv(indicator_NSE, "validation_NSE_0426_10_20.csv")

