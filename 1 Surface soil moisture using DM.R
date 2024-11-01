#This code is for downscaling and validation of 
#surface soil moisture (0-5cm)

library(ncdf4) 
library(raster) 
library(rgdal) 
library(ggplot2) 
library(maptools) 
library(terra)
library(randomForest)
library(caret)
library(raster)

##Start daily downscaling

setwd(~/2 soil moisture product")
path.sm<-"~/2 soil moisture product"
SM_file_25km <- dir(path.sm, pattern = ".tif$", full.names = TRUE)

for(ss in 1:length(SM_file_25km)){

sys_time_1<-proc.time()
#Read soil moisture 25km data
setwd(~/2 soil moisture product")
path.sm<-"~2 soil moisture product"
SM_file_25km <- dir(path.sm, pattern = ".tif$", full.names = TRUE)

#--Read and merge 25km environment variables (non-time-varying)--------------
path<-"~/ECdata_25km/EC_no_Time"
ECfile_25km_no_time <- dir(path, pattern = ".tif$", full.names = TRUE)
for(i in 1 : length(ECfile_25km_no_time)){
  assign(paste("ec", i, sep = ""),raster(ECfile_25km_no_time[i]))
}
ec_raster_25km<-ec1
for(j in 2: length(ECfile_25km_no_time)){
  ec_raster_25km<-stack(ec_raster_25km,get(paste("ec",j,sep="")))
}
#--Read and merge 1km environment variables (non-time-varying)---------------
path<-"~/ECdata_1km/EC_no_Time"
ECfile_1km_no_time <- dir(path, pattern = ".tif$", full.names = TRUE)
for(i in 1 : length(ECfile_1km_no_time)){
  assign(paste("ec", i, sep = ""),raster(ECfile_1km_no_time[i]))
}
ec_raster_1km<-ec1
for(j in 2: length(ECfile_1km_no_time)){
  ec_raster_1km<-stack(ec_raster_1km,get(paste("ec",j,sep="")))
}

#Merge 25km other daily environment variables---------------------
path.lstd.25km<-"~/ECdata_25km/EC_with_Time/LST_D"
lstd.file <- dir(path.lstd.25km, pattern = ".tif$", full.names = TRUE)
LST_D<-raster(lstd.file[ss])
LST_D@data@names<-"LST_D"
ec_raster_25km<-stack(ec_raster_25km,LST_D)

path.lstn.25km<-"~/ECdata_25km/EC_with_Time/LST_N"
lstn.file <- dir(path.lstn.25km, pattern = ".tif$", full.names = TRUE)
LST_N<-raster(lstn.file[ss])
LST_N@data@names<-"LST_N"
ec_raster_25km<-stack(ec_raster_25km,LST_N)

path.ndvi.25km<-"~/ECdata_25km/EC_with_Time/NDVI"
ndvi.file <- dir(path.ndvi.25km, pattern = ".tif$", full.names = TRUE)
NDVI<-raster(ndvi.file[ss])
NDVI@data@names<-"NDVI"
ec_raster_25km<-stack(ec_raster_25km,NDVI)

path.prcp.25km<-"~/ECdata_25km/EC_with_Time/Prcp"
prcp.file <- dir(path.prcp.25km, pattern = ".tif$", full.names = TRUE)
Prcp<-raster(prcp.file[ss])
Prcp@data@names<-"Prcp"
ec_raster_25km<-stack(ec_raster_25km,Prcp)

#Merge 1km other daily environment variables--------------------
path.lstd.1km<-"~/ECdata_1km/EC_with_Time/LST_D"
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

#Perform downscaling modeling based on machine learning-------------------

SMdata<-readAll(raster(SM_file_25km[ss]))
df<-data.frame(SM=SMdata@data@values)
name.ec25km<-names(ec_raster_25km)

for(ec25.count in 1:length(name.ec25km)){
temp.ec25<-readAll(ec_raster_25km[[name.ec25km[ec25.count]]])
temp.ec25.values<-temp.ec25@data@values
df<-data.frame(df,temp.ec25.values)
colnames(df)[(ec25.count+1)]<-name.ec25km[ec25.count]
}
df<-na.omit(df)
#print("data frame build complete")
name.ec1km<-names(ec_raster_1km)
name.ec1km<-c("SM",name.ec1km)
colnames(df)<-name.ec1km

Model<-train( df[,2:ncol(df)],df[,'SM'],method='rf',ntree=1000,importance=T,
                  trControl = trainControl(method = "none"))
Pre <- predict(ec_raster_1km, Model)

setwd("~/1 downscaling/result")
writeRaster(Pre,paste("SM_1km_",substring(SM_file_25km[ss],61),sep=""))
print(paste("Mission complete for ",as.character(ss),
        " (",as.character(round(ss/length(SM_file_25km)*100,1)),"%)",sep=""))

sys_time_2<-proc.time()
sys_time_2-sys_time_1
rm(list = ls())

path.sm<-"~/2 soil moisture product"
SM_file_25km <- dir(path.sm, pattern = ".tif$", full.names = TRUE)
}

##============================================


#This code verifies the accuracy of the downscaling results of 
#the soil moisture surface layer 0-5 cm

library(sp)
library(GA)
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

ubrmse<-function(a,b){
(sum(((a-mean(a))-(b-mean(b)))^2)/length(a))^0.5
}

nse<-function(pre, obs){
1-(sum((obs-pre)^2)/sum((obs-mean(obs))^2))
}

k.process<-30
seed=c(1:k.process)
result_df<-data.frame(Pre=0 , Obs=0 , Day=0 ,Site=0 ,Network=0 ,Process=0)
setwd("~/1 in-situ soil moisture")
df<-read.csv("DSM_file.csv")
site_info<-read.csv("site_info.csv")

for(i.day in 1:122){

for (i.validation in 1:k.process){#---Repeat 30 times for verification

#Data segmentation-----------------------

df_temp<-subset(df,Day==i.day)
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
point.test<- subset(df_temp,Site %in% test_site)
site_info_test<-subset(site_info, Site %in%  point.test$Site)# Found the test set site

##Read the downscaled SM result map

setwd("~/1 downscaling/result")
path_sm_result<-"~/1 downscaling/result"
sm_result_file <- dir(path_sm_result, pattern = ".tif$", full.names = TRUE)
SM_down<-raster(sm_result_file [i.day])

coordinates(site_info_test)=~lon+lat
proj4string(site_info_test)<- proj4string(SM_down)
pre_0_5 <-extract(SM_down,site_info_test)
obs_0_5<-point.test$SM_5cm

result_df_temp<-data.frame(Pre=pre_0_5,Obs=obs_0_5,Day=point.test$Day,
                                      Site=point.test$Site,Network=point.test$Network,
                                      Process=rep(i.validation,length(pre_0_5)))
result_df<-rbind(result_df,result_df_temp)
print(paste("the",i.day,"day &",i.validation, "process..."))
}
}
result_df<-result_df[-1,]
write.csv(result_df, "validation_result3.csv")


##(1) Calculate the three indicators of ME, RMSE and ubRMSE, 
##and divide them into three network and overall indicators

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

##(2) Calculate the NSE, same as top 

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

write.csv(indicator_all, "validation_all2.csv")
write.csv(indicator_Naqu, "validation_Naqu2.csv")
write.csv(indicator_Pagri, "validation_Pagri2.csv")
write.csv(indicator_uHRB, "validation_uHRB2.csv")
write.csv(indicator_NSE, "validation_NSE2.csv")


