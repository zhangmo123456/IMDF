##Constructing a soil moisture dataset at 1 cm depth intervals
##using a non-parametric depth function

library(hydroGOF)
library(aqp)
library(plyr)
library(ithir)
ubrmse<-function(a,b){
(sum(((a-mean(a))-(b-mean(b)))^2)/length(a))^0.5
}
nse<-function(obs,pre){
1-(sum((obs-pre)^2)/sum((obs-mean(obs))^2))
}

point<-read.csv("site_all.csv")
df_update<-data.frame()

for(i in 1:nrow(point)){

df<-point[i,]
sm.1<-df$SM_5cm
sm.2<-df$SM_10cm
sm.3<-df$SM_20cm
data.sm<-{}
c<-c(sm.1,sm.2,sm.3)
data.sm<-c(data.sm,c)
n <- 3
id<-rep(1:1, each=n)
top<-rep( c(0,5,10),1)
bottom<-rep( c(5,10,20),1)
s.pt<-data.frame(id=as.character(id),top=top,bottom=bottom,sm=data.sm)

if(sum(is.na(s.pt$sm))<=0){
s.pt<-na.omit(s.pt)
depths(s.pt) <- id ~ top + bottom
sm.fit<- ea_spline(s.pt, var.name="sm", lam = 0.001, d = t(c(0,5,10,20)), 
                    vlow = 0, vhigh = 1000, show.progress=TRUE)
pre<-round(sm.fit$var.1cm,3)
pre<-c(t(pre),df$Date,df$Site,df$Day)
df_update<-rbind(df_update,pre)
}

else{
pre<-c(rep(NA,20),df$Date,df$Site,df$Day)
df_update<-rbind(df_update,pre)
}
}
name<-{}
for(m in 1:20){
name<-c(name,paste("SM_",m,"cm",sep=""))
}
name<-c(name,"Date","Site","Day")
colnames(df_update)<-name

obs_5cm<-point$SM_5cm
obs_10cm<-point$SM_10cm
obs_20cm<-point$SM_20cm

df_all<-cbind(df_update,obs_5cm,obs_10cm,obs_20cm)
#write.csv(df_update,"SM_spline.csv")

point<-read.csv("SM_spline.csv")

df_update<-data.frame(ME_5=0,ME_10=0,ME_20=0,RMSE_5=0,RMSE_10=0,RMSE_20=0,
                                         ubRMSE_5=0,ubRMSE_10=0,ubRMSE_20=0,Day=0)
for(i in 1:122){
df<-subset(point,Day==i)
df<-na.omit(df)
pre_5cm<-rowMeans(df[,1:5])
pre_10cm<-rowMeans(df[,6:10])
pre_20cm<-rowMeans(df[,11:20])
obs_5cm<-df$obs_5cm
obs_10cm<-df$obs_10cm
obs_20cm<-df$obs_20cm

ME_5_temp<-me(pre_5cm,obs_5cm)
ME_10_temp<-me(pre_10cm,obs_10cm)
ME_20_temp<-me(pre_20cm,obs_20cm)

RMSE_5_temp<-rmse(pre_5cm,obs_5cm)
RMSE_10_temp<-rmse(pre_10cm,obs_10cm)
RMSE_20_temp<-rmse(pre_20cm,obs_20cm)

ubRMSE_5_temp<-ubrmse(pre_5cm,obs_5cm)
ubRMSE_10_temp<-ubrmse(pre_10cm,obs_10cm)
ubRMSE_20_temp<-ubrmse(pre_20cm,obs_20cm)

df_update<-rbind(df_update,c(ME_5_temp,ME_10_temp,ME_20_temp,RMSE_5_temp,RMSE_10_temp,
                              RMSE_20_temp,ubRMSE_5_temp,ubRMSE_10_temp,ubRMSE_20_temp,i))
}

df_update<-df_update[-1,]


df_update_me<-df_update[,c(1,2,3,10)]
dfdf_new_plot<-melt(df_update_me,id="Day")

# Customize a formatting function to retain three decimal places
format_three_decimals <- function(x) {
  sprintf("%.3f", x)
}

#Visualization
ggplot(dfdf_new_plot, aes(x=variable, y=value, fill=variable)) +
  geom_violin(trim=FALSE,alpha=1)+
scale_fill_manual(values=c("#DFE7C4", "#CEBCD4","#A5CFE3"))+
  geom_boxplot(width=0.1, fill = "grey87")  + 
scale_y_continuous(labels = format_three_decimals)+
  #ylim(0,0.2)+
 labs(title="(a) ME",y =  expression("ME values (m"^3*"/m"^3*")"), x = "") +
    theme_bw() +
  theme(plot.title = element_text(size=20),
  panel.grid.minor = element_blank(),
  panel.grid.major.y = element_line(color = "gray",size = 0.5,linetype = 2),
   panel.grid.major.x = element_blank(),
        legend.position = "none",
        legend.key = element_rect(colour = NA, fill = NA),
        legend.justification=c(1, 0))+
  theme(axis.text.x = element_text( color="black",  size=20),axis.title.x=element_text(size=20),
          axis.text.y = element_text( color="black",  size=20),axis.title.y=element_text(size=20))+
   scale_x_discrete(labels = c("0-5 cm", "5-10 cm", "10-20 cm")) 

setwd("C:/Users/dell/Desktop") 
ggsave("violin_ME.jpg", dpi=600, width = 9, height = 5)


df_update_me<-df_update[,c(4,5,6,10)]
dfdf_new_plot<-melt(df_update_me,id="Day")

ggplot(dfdf_new_plot, aes(x=variable, y=value, fill=variable)) +
  geom_violin(trim=FALSE,alpha=1)+
scale_fill_manual(values=c("#DFE7C4", "#CEBCD4","#A5CFE3"))+
  geom_boxplot(width=0.1, fill = "grey87")  + 
scale_y_continuous(labels = format_three_decimals)+
  #scale_y_continuous(breaks = seq(0,0.15, by=0.05)) +
  #ylim(0,0.2)+
 labs(title="(b) RMSE",y = expression("RMSE values (m"^3*"/m"^3*")"), x = "") +
    theme_bw() +
  theme(plot.title = element_text(size=20),
  panel.grid.minor = element_blank(),
  panel.grid.major.y = element_line(color = "gray",size = 0.5,linetype = 2),
   panel.grid.major.x = element_blank(),
        legend.position = "none",
        legend.key = element_rect(colour = NA, fill = NA),
        legend.justification=c(1, 0))+
  theme(axis.text.x = element_text( color="black",  size=20),axis.title.x=element_text(size=20),
          axis.text.y = element_text( color="black",  size=20),axis.title.y=element_text(size=20))+
   scale_x_discrete(labels = c("0-5 cm", "5-10 cm", "10-20 cm")) 

setwd("C:/Users/dell/Desktop") 
ggsave("violin_RMSE.jpg", dpi=600, width = 9, height = 5)


df_update_me<-df_update[,c(7,8,9,10)]
dfdf_new_plot<-melt(df_update_me,id="Day")

ggplot(dfdf_new_plot, aes(x=variable, y=value, fill=variable)) +
  geom_violin(trim=FALSE,alpha=1)+
scale_fill_manual(values=c("#DFE7C4", "#CEBCD4","#A5CFE3"))+
  geom_boxplot(width=0.1, fill = "grey87")  + 
scale_y_continuous(labels = format_three_decimals)+
  #scale_y_continuous(breaks = seq(0,0.15, by=0.05)) +
  #ylim(0,0.2)+
 labs(title="(c) ubRMSE",y = expression("ubRMSE values (m"^3*"/m"^3*")"), x = "") +
    theme_bw() +
  theme(plot.title = element_text(size=20),
  panel.grid.minor = element_blank(),
  panel.grid.major.y = element_line(color = "gray",size = 0.5,linetype = 2),
   panel.grid.major.x = element_blank(),
        legend.position = "none",
        legend.key = element_rect(colour = NA, fill = NA),
        legend.justification=c(1, 0))+
  theme(axis.text.x = element_text( color="black",  size=20),axis.title.x=element_text(size=20),
          axis.text.y = element_text( color="black",  size=20),axis.title.y=element_text(size=20))+
   scale_x_discrete(labels = c("0-5 cm", "5-10 cm", "10-20 cm")) 

setwd("C:/Users/dell/Desktop") 
ggsave("violin_ubRMSE.jpg", dpi=600, width = 9, height = 5)


##Calculate NSE
point<-read.csv("SM_spline.csv")
df_update<-data.frame(NSE_5=0,NSE_10=0,NSE_20=0,Day=0)

site_name<-levels(as.factor(point$Site))

for(i in 1:length(site_name)){
df<-subset(point,Site==site_name[i])
df<-na.omit(df)
pre_5cm<-rowMeans(df[,1:5])
pre_10cm<-rowMeans(df[,6:10])
pre_20cm<-rowMeans(df[,11:20])
obs_5cm<-df$obs_5cm
obs_10cm<-df$obs_10cm
obs_20cm<-df$obs_20cm

NSE_5_temp<-nse(pre_5cm,obs_5cm)
NSE_10_temp<-nse(pre_10cm,obs_10cm)
NSE_20_temp<-nse(pre_20cm,obs_20cm)

df_update<-rbind(df_update,c(NSE_5_temp,NSE_10_temp,NSE_20_temp,i))
}

df_update<-df_update[-1,]


#df_update<-read.csv("df_update.csv")
dfdf_new_plot<-melt(df_update,id="Day")

ggplot(dfdf_new_plot, aes(x=variable, y=value, fill=variable)) +
  geom_violin(trim=FALSE,alpha=1)+
  #ylim(0.78,1.05)+
scale_fill_manual(values=c("#DFE7C4", "#CEBCD4","#A5CFE3"))+
  geom_boxplot(width=0.1, fill = "grey87")  + 
scale_y_continuous(labels = format_three_decimals,limits = c(0.78,1.05))+
  #scale_y_continuous(breaks = seq(0,0.15, by=0.05)) +
 labs(title="(d) NSE",y = "NSE values", x = "") +
    theme_bw() +
  theme(plot.title = element_text(size=20),
  panel.grid.minor = element_blank(),
  panel.grid.major.y = element_line(color = "gray",size = 0.5,linetype = 2),
   panel.grid.major.x = element_blank(),
        legend.position = "none",
#plot.margin = margin(1, 1, 1, 1, "cm") ,
        legend.key = element_rect(colour = NA, fill = NA),
        legend.justification=c(1, 0))+
  theme(axis.text.x = element_text( color="black",  size=20),axis.title.x=element_text(size=20),
          axis.text.y = element_text( color="black",  size=20),axis.title.y=element_text(size=20))+
   scale_x_discrete(labels = c("0-5 cm", "5-10 cm", "10-20 cm")) 

ggsave("violin_NSE.jpg", dpi=600, width = 9, height = 5)
#df_update <- data[!is.nan(df_update$NSE_5), ]

