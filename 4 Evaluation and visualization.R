
#Descriptive Statistics
#==============================================
library(psych)
setwd("~/1 in-situ soil moisture")
point<-read.csv("site_all.csv")

# Calculate the overall situation first, the main text figure
des_all<-data.frame(mean_5=0,mean_10=0,mean_20=0,medain_5=0,
                                      medain_10=0,medain_20=0,sd_5=0,sd_10=0,sd_20=0,
                                       cv_5=0,cv_10=0,cv_20=0,day=0)
for(i in 1:122){
df<-subset(point,Day==i)
df<-df[,1:3]
df<-na.omit(df)
z<-describe(df)
mean_temp<-z$mean
median_temp<-z$median
sd_temp<-z$sd
cv_temp<-z$sd/z$mean
des_all<-rbind(des_all,c(mean_temp,median_temp,sd_temp,cv_temp,i))
}
des_all<-des_all[-1,]

# Calculate the situation of each site separately, Appendix Figure
des_Naqu<-data.frame(mean_5=0,mean_10=0,mean_20=0,medain_5=0,
                                      medain_10=0,medain_20=0,sd_5=0,sd_10=0,sd_20=0,
                                       cv_5=0,cv_10=0,cv_20=0,day=0)
des_Pagri<-data.frame(mean_5=0,mean_10=0,mean_20=0,medain_5=0,
                                      medain_10=0,medain_20=0,sd_5=0,sd_10=0,sd_20=0,
                                       cv_5=0,cv_10=0,cv_20=0,day=0)
des_uHRB<-data.frame(mean_5=0,mean_10=0,mean_20=0,medain_5=0,
                                      medain_10=0,medain_20=0,sd_5=0,sd_10=0,sd_20=0,
                                       cv_5=0,cv_10=0,cv_20=0,day=0)
for(i in 1:122){

df<-subset(point,Day==i)
df<-na.omit(df)
df_Naqu<-subset(df,Network=="Naqu")
df_Pagri<-subset(df,Network=="Pagri")
df_uHRB<-subset(df,Network=="uHRB")
df_Naqu<-df_Naqu[,1:3]
df_Pagri<-df_Pagri[,1:3]
df_uHRB<-df_uHRB[,1:3]

z_Naqu<-describe(df_Naqu)
z_Pagri<-describe(df_Pagri)
z_uHRB<-describe(df_uHRB)

mean_temp_Naqu<-z_Naqu$mean
median_temp_Naqu<-z_Naqu$median
sd_temp_Naqu<-z_Naqu$sd
cv_temp_Naqu<-z_Naqu$sd/z_Naqu$mean
des_Naqu<-rbind(des_Naqu,c(mean_temp_Naqu,median_temp_Naqu,sd_temp_Naqu,cv_temp_Naqu,i))

mean_temp_Pagri<-z_Pagri$mean
median_temp_Pagri<-z_Pagri$median
sd_temp_Pagri<-z_Pagri$sd
cv_temp_Pagri<-z_Pagri$sd/z_Pagri$mean
des_Pagri<-rbind(des_Pagri,c(mean_temp_Pagri,median_temp_Pagri,sd_temp_Pagri,cv_temp_Pagri,i))

mean_temp_uHRB<-z_uHRB$mean
median_temp_uHRB<-z_uHRB$median
sd_temp_uHRB<-z_uHRB$sd
cv_temp_uHRB<-z_uHRB$sd/z_uHRB$mean
des_uHRB<-rbind(des_uHRB,c(mean_temp_uHRB,median_temp_uHRB,sd_temp_uHRB,cv_temp_uHRB,i))

}
des_Naqu<-des_Naqu[-1,]
des_Pagri<-des_Pagri[-1,]
des_uHRB<-des_uHRB[-1,]

write.csv(des_all,"des_all_site.csv")
write.csv(des_Naqu,"des_Naqu_site.csv")
write.csv(des_Pagri,"des_Pagri_site.csv")
write.csv(des_uHRB,"des_uHRB_site.csv")

library(ggplot2)
library(ggsci)
library(reshape2)
library(ggplot2)
library(babynames) 
library(dplyr)
library(hrbrthemes)
library(viridis)

df<-read.csv("des_all_site.csv")# as example for all sites
day<-df$day

dfplot_mean<-df[,c(1:3,13)]
dfplot_median<-df[,c(4:6,13)]
dfplot_sd<-df[,c(7:9,13)]
dfplot_cv<-df[,c(10:12,13)]

dfplot_mean<-melt(dfplot_mean,id="day")
dfplot_median<-melt(dfplot_median,id="day")
dfplot_sd<-melt(dfplot_sd,id="day")
dfplot_cv<-melt(dfplot_cv,id="day")

ggplot(dfplot_mean, aes(x=day, y=value, group=variable, color=variable)) +
    geom_line(size=0.8,alpha=1) +
    ggtitle("(a)") +
    #theme_ipsum() +
    theme_light()+
    #theme_bw() +
 labs(y = "Mean", x = "Day") +
 scale_x_continuous(breaks = seq(day[1],day[length(day)], by=30)) +
  theme(panel.grid.minor = element_blank(),
        legend.position = "none")+
theme(axis.text.x = element_text( color="black",  size=14,face="bold"),axis.title.x=element_text(size=14,face="bold"),
          axis.text.y = element_text( color="black",  size=14,face="bold"),axis.title.y=element_text(size=14,face="bold"),
plot.title = element_text(size = 14,face="bold"))+
scale_color_manual(values=c("palegreen4", "steelblue2","#ECB477"))
ggsave("mean.jpg", dpi=600, width = 18, height = 13, units = "cm")


ggplot(dfplot_median, aes(x=day, y=value, group=variable, color=variable)) +
    geom_line(size=0.8,alpha=1) +
    ggtitle("(b)") +
    #theme_ipsum() +
    theme_light()+
    #theme_bw() +
 labs(y = "Median", x = "Day") +
 scale_x_continuous(breaks = seq(day[1],day[length(day)], by=30)) +
  theme(panel.grid.minor = element_blank(),
        legend.position = "none")+
theme(axis.text.x = element_text( color="black",  size=14,face="bold"),axis.title.x=element_text(size=14,face="bold"),
          axis.text.y = element_text( color="black",  size=14,face="bold"),axis.title.y=element_text(size=14,face="bold"),
plot.title = element_text(size = 14,face="bold"))+
scale_color_manual(values=c("palegreen4", "steelblue2","#ECB477"))
ggsave("median.jpg", dpi=600, width = 18, height = 13, units = "cm")


ggplot(dfplot_sd, aes(x=day, y=value, group=variable, color=variable)) +
    geom_line(size=0.8,alpha=1) +
    ggtitle("(c)") +
    #theme_ipsum() +
    theme_light()+
    #theme_bw() +
 labs(y = "SD", x = "Day") +
 scale_x_continuous(breaks = seq(day[1],day[length(day)], by=30)) +
  theme(panel.grid.minor = element_blank(),
        legend.position = "none")+
theme(axis.text.x = element_text( color="black",  size=14,face="bold"),axis.title.x=element_text(size=14,face="bold"),
          axis.text.y = element_text( color="black",  size=14,face="bold"),axis.title.y=element_text(size=14,face="bold"),
plot.title = element_text(size = 14,face="bold"))+
scale_color_manual(values=c("palegreen4", "steelblue2","#ECB477"))
ggsave("sd.jpg", dpi=600, width = 18, height = 13, units = "cm")


ggplot(dfplot_cv, aes(x=day, y=value, group=variable, color=variable)) +
    geom_line(size=0.8,alpha=1) +
    ggtitle("(d)") +
    #theme_ipsum() +
    theme_light()+
    #theme_bw() +
 labs(y = "CV", x = "Day") +
 scale_x_continuous(breaks = seq(day[1],day[length(day)], by=30)) +
  theme(panel.grid.minor = element_blank(),
        legend.position = "none")+
theme(axis.text.x = element_text( color="black",  size=14,face="bold"),axis.title.x=element_text(size=14,face="bold"),
          axis.text.y = element_text( color="black",  size=14,face="bold"),axis.title.y=element_text(size=14,face="bold"),
plot.title = element_text(size = 14,face="bold"))+
scale_color_manual(values=c("palegreen4", "steelblue2","#ECB477"))
ggsave("cv.jpg", dpi=600, width = 18, height = 13, units = "cm")


plot_legend<-ggplot(dfplot_cv, aes(x=day, y=value, group=variable, color=variable)) +
    geom_line(size=0.8,alpha=1) +
    ggtitle("(d)") +
    #theme_ipsum() +
    theme_light()+
    #theme_bw() +
 labs(y = "Mean", x = "Day") +
 scale_x_continuous(breaks = seq(day[1],day[length(day)], by=30)) +
  theme(panel.grid.minor = element_blank(),
        legend.position = "bottom")+
theme(axis.text.x = element_text( color="black",  size=14,face="bold"),axis.title.x=element_text(size=14,face="bold"),
          axis.text.y = element_text( color="black",  size=14,face="bold"),axis.title.y=element_text(size=14,face="bold"),
plot.title = element_text(size = 14,face="bold"))+
scale_color_manual(values=c("palegreen4", "steelblue2","#ECB477"),
                                 labels=c('0-5 cm', '5-10 cm','10-20 cm'))
ggsave("legend.jpg", dpi=600, width = 18, height = 13, units = "cm")


#Comparison of surface soil moisture prediction
#==============================================
library(ggplot2)
library(ggsci)
library(reshape2)
library(ggplot2)
library(babynames) 
library(dplyr)
library(hrbrthemes)
library(viridis)

##Daily accuracy verification + overall violin plot
##Depending on the site, only violin plots are drawn

setwd("~/2 spatial prediction/1 validation")
df_dsm<-read.csv("validation_all.csv")
setwd("~/4 modeling/1 downscaling")
df_down<-read.csv("validation_all.csv")
day<-c(1:122)

dsm_new<-data.frame(ME=0,RMSE=0,ubRMSE=0,SE_ME=0,SE_RMSE=0,SE_ubRMSE=0,Day=0)
for(i in 1:122){
df_temp<-subset(df_dsm,Day==i)
temp_indicator<-c(mean(df_temp$ME),mean(df_temp$RMSE),mean(df_temp$ubRMSE),
                sd(df_temp$ME)/sqrt(30),sd(df_temp$RMSE)/sqrt(30),sd(df_temp$ubRMSE)/sqrt(30),i)
dsm_new<-rbind(dsm_new,temp_indicator)
}
dsm_new<-dsm_new[-1,]

down_new<-data.frame(ME=0,RMSE=0,ubRMSE=0,SE_ME=0,SE_RMSE=0,SE_ubRMSE=0,Day=0)
for(i in 1:122){
df_temp<-subset(df_down,Day==i)
temp_indicator<-c(mean(df_temp$ME),mean(df_temp$RMSE),mean(df_temp$ubRMSE),
                sd(df_temp$ME)/sqrt(30),sd(df_temp$RMSE)/sqrt(30),sd(df_temp$ubRMSE)/sqrt(30),i)
down_new<-rbind(down_new,temp_indicator)
}
down_new<-down_new[-1,]

##ME-------
df_me<-data.frame(DSM= dsm_new$ME, Down= down_new$ME, Day=c(1:122))
df_me_se<-data.frame(se_DSM= dsm_new$SE_ME, se_Down= down_new$SE_ME,Day=c(1:122))
df_me_plot1<-melt(df_me,id="Day")
df_me_plot2<-melt(df_me_se,id="Day")
df_me_plot<-cbind(df_me_plot1,se=df_me_plot2$value)

ggplot(df_me_plot, aes(x = Day, y = value,group = variable, color = variable)) +
  geom_line(size=0.6,alpha=1) +
  ggtitle("(a) ME") +
  geom_ribbon(aes(ymin = value - se, ymax = value + se,fill = variable), alpha = 0.3,size=0.1)+
  theme_bw()+
 labs(y =  expression("ME values (m"^3*"/m"^3*")"), x = "Time (observation days)") +
  ylim(-0.05,0.05)+
  scale_x_continuous(breaks = seq(day[1],day[length(day)], by=24)) +
  theme(panel.grid.minor = element_blank(),
              legend.position = "none")+
theme(axis.text.x = element_text( color="black",  size=18),axis.title.x=element_text(size=18),
          axis.text.y = element_text( color="black",  size=18),axis.title.y=element_text(size=18),
plot.title = element_text(size = 18))+
scale_color_manual(values=c("#F4CE91", "#6CBFBF"))+
scale_fill_manual(values = c("#F4CE91", "#6CBFBF"))

setwd("C:/Users/dell/Desktop")
ggsave("ME_day.jpg", dpi=600, width = 16, height =11, units = "cm")

##RMSE-------
df_rmse<-data.frame(DSM= dsm_new$RMSE, Down= down_new$RMSE, Day=c(1:122))
df_rmse_se<-data.frame(se_DSM= dsm_new$SE_RMSE, se_Down= down_new$SE_RMSE,Day=c(1:122))
df_rmse_plot1<-melt(df_rmse,id="Day")
df_rmse_plot2<-melt(df_rmse_se,id="Day")
df_rmse_plot<-cbind(df_rmse_plot1,se=df_rmse_plot2$value)

ggplot(df_rmse_plot, aes(x = Day, y = value,group = variable, color = variable)) +
  geom_line(size=0.6,alpha=1) +
  ggtitle("(b) RMSE") +
  geom_ribbon(aes(ymin = value - se, ymax = value + se,fill = variable), alpha = 0.3,size=0.1)+
  theme_bw()+
 labs(y =  expression("RMSE values (m"^3*"/m"^3*")"), x = "Time (observation days)") +
  scale_x_continuous(breaks = seq(day[1],day[length(day)], by=24)) +
  theme(panel.grid.minor = element_blank(),
              legend.position = "none")+
theme(axis.text.x = element_text( color="black",  size=18),axis.title.x=element_text(size=18),
          axis.text.y = element_text( color="black",  size=18),axis.title.y=element_text(size=18),
plot.title = element_text(size = 18))+
scale_color_manual(values=c("#F4CE91", "#6CBFBF"))+
scale_fill_manual(values = c("#F4CE91", "#6CBFBF"))

setwd("C:/Users/dell/Desktop")
ggsave("RMSE_day.jpg", dpi=600, width = 16, height =11, units = "cm")

ggplot(df_rmse_plot, aes(x = Day, y = value,group = variable, color = variable)) +
  geom_line(size=0.6,alpha=1) +
  ggtitle("(b) RMSE") +
  geom_ribbon(aes(ymin = value - se, ymax = value + se,fill = variable), alpha = 0.3,size=0.1)+
  theme_bw()+
 labs(y =  expression("RMSE values (m"^3*"/m"^3*")"), x = "Time (observation days)") +
  scale_x_continuous(breaks = seq(day[1],day[length(day)], by=24)) +
  theme(panel.grid.minor = element_blank(),
              legend.position = "bottom")+
theme(axis.text.x = element_text( color="black",  size=18),axis.title.x=element_text(size=18),
          axis.text.y = element_text( color="black",  size=18),axis.title.y=element_text(size=18),
legend.title = element_text(size = 18),  # 图例标题大小
        legend.text = element_text(size = 18), 
legend.key.width = unit(1, "cm"),
plot.title = element_text(size = 18))+
scale_color_manual(values=c("#F4CE91", "#6CBFBF"),
                                 labels=c('IM', 'DM'),name = "Methods")+
scale_fill_manual(values = c("#F4CE91", "#6CBFBF"),
                                 labels=c('IM', 'DM'),name = "Methods")

scale_color_manual(values=c("palegreen4", "steelblue2","#ECB477"),
                                 labels=c('0-5 cm', '5-10 cm','10-20 cm'),name = "Depth interval")
ggsave("legend.jpg", dpi=600, width = 16, height =11, units = "cm")

#----------------------------------

setwd("~/2 spatial prediction/1 validation")
df_dsm_Naqu<-read.csv("validation_Naqu.csv")
df_dsm_Pagri<-read.csv("validation_Pagri.csv")
df_dsm_uHRB<-read.csv("validation_uHRB.csv")

setwd("~/4 modeling/1 downscaling")
df_down_Naqu<-read.csv("validation_Naqu.csv")
df_down_Pagri<-read.csv("validation_Pagri.csv")
df_down_uHRB<-read.csv("validation_uHRB.csv")
day<-c(1:122)

##ME extraction and drawing

dfdf_new<-data.frame(IM_Naqu=0,DM_Naqu=0,IM_Pagri=0,DM_Pagri=0,IM_uHRB=0,DM_uHRB=0,Day=0)

for(i in 1:122){
temp_dsm_naqu<-mean(subset(df_dsm_Naqu,Day==i)$ME)
temp_down_naqu<-mean(subset(df_down_Naqu,Day==i)$ME)

temp_dsm_pagri<-mean(subset(df_dsm_Pagri,Day==i)$ME)
temp_down_pagri<-mean(subset(df_down_Pagri,Day==i)$ME)

temp_dsm_uhrb<-mean(subset(df_dsm_uHRB,Day==i)$ME)
temp_down_uhrb<-mean(subset(df_down_uHRB,Day==i)$ME)


temp_indicator<-c(temp_dsm_naqu,temp_down_naqu,temp_dsm_pagri,temp_down_pagri,
                                   temp_dsm_uhrb,temp_down_uhrb,i)
dfdf_new<-rbind(dfdf_new,temp_indicator)
}
dfdf_new<-dfdf_new[-1,]
dfdf_new_plot<-melt(dfdf_new,id="Day")

ggplot(dfdf_new_plot, aes(x=variable, y=value, fill=variable)) +
  geom_violin(trim=FALSE,alpha=0.7)+
scale_fill_manual(values=c("#F4CE91", "#6CBFBF","#F4CE91", "#6CBFBF","#F4CE91", "#6CBFBF"))+
  geom_boxplot(width=0.1, fill = "grey")  + 
  #scale_y_continuous(breaks = seq(0,0.15, by=0.05)) +
  #ylim(0,0.2)+
 labs(title="(a) ME",y = expression("ME values (m"^3*"/m"^3*")"), x = "") +
    theme_bw()+
  theme(plot.title = element_text(size=18),
  panel.grid.minor = element_blank(),
  panel.grid.major.y = element_line(color = "gray",size = 0.5,linetype = 2),
   panel.grid.major.x = element_blank(),
        legend.position = "none",
        legend.key = element_rect(colour = NA, fill = NA),
        legend.justification=c(1, 0))+
  theme(axis.text.x = element_text( color="black",  size=18),axis.title.x=element_text(size=18),
          axis.text.y = element_text( color="black",  size=18),axis.title.y=element_text(size=18))

setwd("C:/Users/dell/Desktop") 
ggsave("violin_ME.jpg", dpi=600, width = 9, height = 5)

##RMSE extraction and plotting

dfdf_new<-data.frame(IM_Naqu=0,DM_Naqu=0,IM_Pagri=0,DM_Pagri=0,IM_uHRB=0,DM_uHRB=0,Day=0)

for(i in 1:122){
temp_dsm_naqu<-mean(subset(df_dsm_Naqu,Day==i)$RMSE)
temp_down_naqu<-mean(subset(df_down_Naqu,Day==i)$RMSE)

temp_dsm_pagri<-mean(subset(df_dsm_Pagri,Day==i)$RMSE)
temp_down_pagri<-mean(subset(df_down_Pagri,Day==i)$RMSE)

temp_dsm_uhrb<-mean(subset(df_dsm_uHRB,Day==i)$RMSE)
temp_down_uhrb<-mean(subset(df_down_uHRB,Day==i)$RMSE)


temp_indicator<-c(temp_dsm_naqu,temp_down_naqu,temp_dsm_pagri,temp_down_pagri,
                                   temp_dsm_uhrb,temp_down_uhrb,i)
dfdf_new<-rbind(dfdf_new,temp_indicator)
}
dfdf_new<-dfdf_new[-1,]
dfdf_new_plot<-melt(dfdf_new,id="Day")

ggplot(dfdf_new_plot, aes(x=variable, y=value, fill=variable)) +
  geom_violin(trim=FALSE,alpha=0.7)+
scale_fill_manual(values=c("#F4CE91", "#6CBFBF","#F4CE91", "#6CBFBF","#F4CE91", "#6CBFBF"))+
  geom_boxplot(width=0.1, fill = "grey")  + 
  #scale_y_continuous(breaks = seq(0,0.2, by=0.06)) +
  ylim(0.04,0.18)+
 labs(title="(b) RMSE",y = expression("RMSE values (m"^3*"/m"^3*")"), x = "") +
    theme_bw()+
  theme(plot.title = element_text(size=18),
  panel.grid.minor = element_blank(),
  panel.grid.major.y = element_line(color = "gray",size = 0.5,linetype = 2),
   panel.grid.major.x = element_blank(),
        legend.position = "none",
        legend.key = element_rect(colour = NA, fill = NA),
        legend.justification=c(1, 0))+
  theme(axis.text.x = element_text( color="black",  size=18),axis.title.x=element_text(size=18),
          axis.text.y = element_text( color="black",  size=18),axis.title.y=element_text(size=18))

setwd("C:/Users/dell/Desktop") 
ggsave("violin_RMSE.jpg", dpi=600, width = 9, height = 5)

##ubRMSE extraction and plotting

dfdf_new<-data.frame(IM_Naqu=0,DM_Naqu=0,IM_Pagri=0,DM_Pagri=0,IM_uHRB=0,DM_uHRB=0,Day=0)

for(i in 1:122){
temp_dsm_naqu<-mean(subset(df_dsm_Naqu,Day==i)$ubRMSE)
temp_down_naqu<-mean(subset(df_down_Naqu,Day==i)$ubRMSE)

temp_dsm_pagri<-mean(subset(df_dsm_Pagri,Day==i)$ubRMSE)
temp_down_pagri<-mean(subset(df_down_Pagri,Day==i)$ubRMSE)

temp_dsm_uhrb<-mean(subset(df_dsm_uHRB,Day==i)$ubRMSE)
temp_down_uhrb<-mean(subset(df_down_uHRB,Day==i)$ubRMSE)


temp_indicator<-c(temp_dsm_naqu,temp_down_naqu,temp_dsm_pagri,temp_down_pagri,
                                   temp_dsm_uhrb,temp_down_uhrb,i)
dfdf_new<-rbind(dfdf_new,temp_indicator)
}
dfdf_new<-dfdf_new[-1,]
dfdf_new_plot<-melt(dfdf_new,id="Day")

ggplot(dfdf_new_plot, aes(x=variable, y=value, fill=variable)) +
  geom_violin(trim=FALSE,alpha=0.7)+
scale_fill_manual(values=c("#F4CE91", "#6CBFBF","#F4CE91", "#6CBFBF","#F4CE91", "#6CBFBF"))+
  geom_boxplot(width=0.1, fill = "grey")  + 
  #scale_y_continuous(breaks = seq(0,0.2, by=0.06)) +
  ylim(0.04,0.14)+
 labs(title="(c) ubRMSE",y = expression("ubRMSE values (m"^3*"/m"^3*")"), x = "") +
    theme_bw()+
  theme(plot.title = element_text(size=18),
  panel.grid.minor = element_blank(),
  panel.grid.major.y = element_line(color = "gray",size = 0.5,linetype = 2),
   panel.grid.major.x = element_blank(),
        legend.position = "none",
        legend.key = element_rect(colour = NA, fill = NA),
        legend.justification=c(1, 0))+
  theme(axis.text.x = element_text( color="black",  size=18),axis.title.x=element_text(size=18),
          axis.text.y = element_text( color="black",  size=18),axis.title.y=element_text(size=18))

setwd("C:/Users/dell/Desktop") 
ggsave("violin_ubRMSE.jpg", dpi=600, width = 9, height = 5)


##NSE
df_dsm_NSE<-read.csv("validation_NSE.csv")
df_down_NSE<-read.csv("validation_NSE.csv")
day<-c(1:122)

dfdf_nse<-data.frame(IM_Naqu=0,DM_Naqu=0,IM_Pagri=0,DM_Pagri=0,IM_uHRB=0,DM_uHRB=0,process=0)
temp_nse<-cbind(df_dsm_NSE[,2:4],df_down_NSE[,-1])
 colnames(temp_nse)<- colnames(dfdf_nse)
dfdf_nse<-rbind(dfdf_nse,temp_nse)
dfdf_nse<-dfdf_nse[-1,]

dfdf_new_plot<-melt(dfdf_nse,id="process")

ggplot(dfdf_new_plot, aes(x=variable, y=value, fill=variable)) +
  geom_violin(trim=FALSE,alpha=0.7)+
scale_fill_manual(values=c("#F4CE91", "#6CBFBF","#F4CE91", "#6CBFBF","#F4CE91", "#6CBFBF"))+
  geom_boxplot(width=0.1, fill = "grey")  + 
  #scale_y_continuous(breaks = seq(0,0.2, by=0.06)) +
  #ylim(0.04,0.14)+
 labs(title="(d) NSE",y = "NSE values", x = "") +
    theme_bw()+
  theme(plot.title = element_text(size=18),
  panel.grid.minor = element_blank(),
  panel.grid.major.y = element_line(color = "gray",size = 0.5,linetype = 2),
   panel.grid.major.x = element_blank(),
        legend.position = "none",
        legend.key = element_rect(colour = NA, fill = NA),
        legend.justification=c(1, 0))+
  theme(axis.text.x = element_text( color="black",  size=18),axis.title.x=element_text(size=18),
          axis.text.y = element_text( color="black",  size=18),axis.title.y=element_text(size=18))
ggsave("violin_NSE.jpg", dpi=600, width = 9, height = 5)

#Importance of environmental covariates (depth interval is 1cm)
#==============================================

#This version uses IMDF to perform layer-by-layer
# importance analysis of soil moisture 0-20cm

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
library(ggplot2)
library(ggsci)
library(reshape2)
library(ggplot2)
library(babynames) 
library(dplyr)
library(hrbrthemes)
library(viridis)

setwd("~/plot")
df_terrain<-read.csv("varImp_terrain.csv")
df_climate<-read.csv("varImp_climate.csv")
df_vegetation<-read.csv("varImp_vegetation.csv")
df_shp<-read.csv("varImp_shp.csv")
df_physical<-read.csv("varImp_physical.csv")
df_chemical<-read.csv("varImp_chemical.csv")

df_terrain_plot<-melt(df_terrain,id="Depth")
df_climate_plot<-melt(df_climate,id="Depth")
df_vegetation_plot<-melt(df_vegetation,id="Depth")
df_shp_plot<-melt(df_shp,id="Depth")
df_physical_plot<-melt(df_physical,id="Depth")
df_chemical_plot<-melt(df_chemical,id="Depth")
setwd("C:/Users/dell/Desktop")
#111
ggplot(df_terrain_plot, aes(x=Depth, y=value, group=variable, color=variable)) +
    geom_line(size=1,alpha=1) +
    geom_point(size=1.5,alpha=0.8) +
    ggtitle("(a) Terrain") +
    theme_bw() +
 labs(y = "Importance", x = "Soil depth (cm)",color = "") +
 ylim(0,40)+
 #scale_x_continuous(breaks = seq(day[1],day[length(day)], by=30)) +
  theme(panel.grid.minor = element_blank(),
        legend.position = c(0.5,0.2))+
theme(legend.key.size = unit(1.5, "lines"),  
            legend.text = element_text(size = 24),legend.title = element_text(size = 24),
          legend.background = element_rect(fill = "transparent", color = NA), 
          axis.text.x = element_text( color="black",  size=25),axis.title.x=element_text(size=27),
          axis.text.y = element_text( color="black",  size=25),axis.title.y=element_text(size=27),
         plot.title = element_text(size = 25))+
scale_color_manual(values=c("palegreen4", "steelblue2","#ECB477"))+
    guides(color = guide_legend(nrow = 1, byrow = TRUE)) 

ggsave("a.jpg", dpi=600, width = 23, height = 15, units = "cm")   

#222
ggplot(df_climate_plot, aes(x=Depth, y=value, group=variable, color=variable)) +
    geom_line(size=1,alpha=1) +
    geom_point(size=1.5,alpha=0.8) +
    ggtitle("(b) Climate") +
    theme_bw() +
 labs(y = "Importance", x = "Soil depth (cm)",color = "") +
 ylim(32,105)+
 #scale_x_continuous(breaks = seq(day[1],day[length(day)], by=30)) +
  theme(panel.grid.minor = element_blank(),
        legend.position = c(0.5,0.2))+
theme(legend.key.size = unit(1.5, "lines"),
            legend.text = element_text(size = 24),legend.title = element_text(size = 24),
          legend.background = element_rect(fill = "transparent", color = NA), 
          axis.text.x = element_text( color="black",  size=25),axis.title.x=element_text(size=27),
          axis.text.y = element_text( color="black",  size=25),axis.title.y=element_text(size=27),
         plot.title = element_text(size = 25))+
scale_color_manual(values=c("palegreen","palegreen4","skyblue1","royalblue1", " slateblue4"))+
    guides(color = guide_legend(nrow = 2, byrow = TRUE)) 
ggsave("b.jpg", dpi=600, width = 23, height = 15, units = "cm")   


#333
ggplot(df_vegetation_plot, aes(x=Depth, y=value, group=variable, color=variable)) +
    geom_line(size=1,alpha=1) +
    geom_point(size=1.5,alpha=0.8) +
    ggtitle("(c) Vegetation") +
    theme_bw() +
 labs(y = "Importance", x = "Soil depth (cm)",color = "") +
 ylim(30,90)+
 #scale_x_continuous(breaks = seq(day[1],day[length(day)], by=30)) +
  theme(panel.grid.minor = element_blank(),
        legend.position = c(0.5,0.2))+
theme(legend.key.size = unit(1.5, "lines"), 
            legend.text = element_text(size = 24),legend.title = element_text(size = 24),
          legend.background = element_rect(fill = "transparent", color = NA), 
          axis.text.x = element_text( color="black",  size=25),axis.title.x=element_text(size=27),
          axis.text.y = element_text( color="black",  size=25),axis.title.y=element_text(size=27),
         plot.title = element_text(size = 25))+
scale_color_manual(values=c("palegreen4"))+
    guides(color = guide_legend(nrow = 1, byrow = TRUE)) 
ggsave("c.jpg", dpi=600, width = 23, height = 15, units = "cm")   

#444
ggplot(df_shp_plot, aes(x=Depth, y=value, group=variable, color=variable)) +
    geom_line(size=1,alpha=1) +
    geom_point(size=1.5,alpha=0.8) +
    ggtitle("(d) Soil hydraulic parameters") +
    theme_bw() +
 labs(y = "Importance", x = "Soil depth (cm)",color = "") +
 ylim(0,30)+
 #scale_x_continuous(breaks = seq(day[1],day[length(day)], by=30)) +
  theme(panel.grid.minor = element_blank(),
        legend.position = c(0.5,0.2))+
theme(legend.key.size = unit(1.5, "lines"),  
            legend.text = element_text(size = 24),legend.title = element_text(size = 24),
          legend.background = element_rect(fill = "transparent", color = NA), 
          axis.text.x = element_text( color="black",  size=25),axis.title.x=element_text(size=27),
          axis.text.y = element_text( color="black",  size=25),axis.title.y=element_text(size=27),
         plot.title = element_text(size = 25))+
scale_color_manual(values=c("palegreen4","steelblue2","#ECB477"))+
    guides(color = guide_legend(nrow = 1, byrow = TRUE)) 
ggsave("d.jpg", dpi=600, width = 23, height = 15, units = "cm")   


#555
ggplot(df_physical_plot, aes(x=Depth, y=value, group=variable, color=variable)) +
    geom_line(size=1,alpha=1) +
    geom_point(size=1.5,alpha=0.8) +
    ggtitle("(e) Soil physical factors") +
    theme_bw() +
 labs(y = "Importance", x = "Soil depth (cm)",color = "") +
 ylim(0,30)+
 #scale_x_continuous(breaks = seq(day[1],day[length(day)], by=30)) +
  theme(panel.grid.minor = element_blank(),
        legend.position = c(0.5,0.2))+
theme(legend.key.size = unit(1.5, "lines"),  
            legend.text = element_text(size = 24),legend.title = element_text(size = 24),
          legend.background = element_rect(fill = "transparent", color = NA), 
          axis.text.x = element_text( color="black",  size=25),axis.title.x=element_text(size=27),
          axis.text.y = element_text( color="black",  size=25),axis.title.y=element_text(size=27),
         plot.title = element_text(size = 25))+
scale_color_manual(values=c("palegreen4","steelblue2","#ECB477","paleturquoise3"))+
    guides(color = guide_legend(nrow = 1, byrow = TRUE)) 
ggsave("e.jpg", dpi=600, width = 23, height = 15, units = "cm")   

#666
ggplot(df_chemical_plot, aes(x=Depth, y=value, group=variable, color=variable)) +
    geom_line(size=1,alpha=1) +
    geom_point(size=1.5,alpha=0.8) +
    ggtitle("(f) Soil chemical factors") +
    theme_bw() +
 labs(y = "Importance", x = "Soil depth (cm)",color = "") +
 ylim(0,30)+
 #scale_x_continuous(breaks = seq(day[1],day[length(day)], by=30)) +
  theme(panel.grid.minor = element_blank(),
        legend.position = c(0.5,0.2))+
theme(legend.key.size = unit(1.5, "lines"),  
            legend.text = element_text(size = 24),legend.title = element_text(size = 24),
          legend.background = element_rect(fill = "transparent", color = NA), 
          axis.text.x = element_text( color="black",  size=25),axis.title.x=element_text(size=27),
          axis.text.y = element_text( color="black",  size=25),axis.title.y=element_text(size=27),
         plot.title = element_text(size = 25))+
scale_color_manual(values=c("palegreen4","steelblue2","#ECB477" ))+
    guides(color = guide_legend(nrow = 1, byrow = TRUE)) 
ggsave("f.jpg",dpi=600, width = 23, height = 15, units = "cm")   

#Deeper soil moisture accuracy comparison visualization
#==============================================
library(ggplot2)
library(ggsci)
library(reshape2)
library(ggplot2)
library(babynames) 
library(dplyr)
library(hrbrthemes)
library(viridis)

## Overall violin plot
## By different sites, different depths

df_dsm_Naqu_510<-read.csv("validation_Naqu_0426_5_10.csv")
df_dsm_Pagri_510<-read.csv("validation_Pagri_0426_5_10.csv")
df_dsm_uHRB_510<-read.csv("validation_uHRB_0426_5_10.csv")
df_dsm_Naqu_1020<-read.csv("validation_Naqu_0426_10_20.csv")
df_dsm_Pagri_1020<-read.csv("validation_Pagri_0426_10_20.csv")
df_dsm_uHRB_1020<-read.csv("validation_uHRB_0426_10_20.csv")
day<-c(1:122)

dfdf_new<-data.frame(Naqu_5_10=0,Pagri_5_10=0,uHRB_5_10=0,
                                       Naqu_10_20=0,Pagri_10_20=0,uHRB_10_20=0,Day=0)

for(i in 1:122){
temp_dsm_naqu_510<-mean(subset(df_dsm_Naqu_510,Day==i)$ME)
temp_dsm_naqu_1020<-mean(subset(df_dsm_Naqu_1020,Day==i)$ME)

temp_dsm_pagri_510<-mean(subset(df_dsm_Pagri_510,Day==i)$ME)
temp_dsm_pagri_1020<-mean(subset(df_dsm_Pagri_1020,Day==i)$ME)

temp_dsm_uhrb_510<-mean(subset(df_dsm_uHRB_510,Day==i)$ME)
temp_dsm_uhrb_1020<-mean(subset(df_dsm_uHRB_1020,Day==i)$ME)

temp_indicator<-c(temp_dsm_naqu_510,temp_dsm_pagri_510,temp_dsm_uhrb_510,
                                 temp_dsm_naqu_1020 ,temp_dsm_pagri_1020, temp_dsm_uhrb_1020,i)
dfdf_new<-rbind(dfdf_new,temp_indicator)
}
dfdf_new<-dfdf_new[-1,]
dfdf_new_plot<-melt(dfdf_new,id="Day")

ggplot(dfdf_new_plot, aes(x=variable, y=value, fill=variable)) +
  geom_violin(trim=FALSE,alpha=1)+
  geom_boxplot(width=0.1, fill = "grey")  + 
  #scale_y_continuous(breaks = seq(0,0.15, by=0.05)) +
  #ylim(0,0.2)+
 labs(title="(a) ME",y = expression("ME values (m"^3*"/m"^3*")"), x = "") +
    theme_bw()+
  theme(plot.title = element_text(size=20),
  panel.grid.minor = element_blank(),
  panel.grid.major.y = element_line(color = "gray",size = 0.5,linetype = 2),
   panel.grid.major.x = element_blank(),
        legend.position = "none",
        legend.key = element_rect(colour = NA, fill = NA),
        legend.justification=c(1, 0))+
  theme(axis.text.x = element_text( color="black",  size=20),axis.title.x=element_text(size=20),
          axis.text.y = element_text( color="black",  size=20),axis.title.y=element_text(size=20))+
  scale_fill_manual(values=c("#B8DFB9","#B8DFB9","#B8DFB9", "#99DAEE","#99DAEE","#99DAEE"))+
   scale_x_discrete(labels = c("Naqu", "Pagri", "uHRB","Naqu", "Pagri", "uHRB")) 

ggsave("violin_ME_0426.jpg", dpi=600, width = 9, height = 5)

dfdf_new<-data.frame(Naqu_5_10=0,Pagri_5_10=0,uHRB_5_10=0,
                                       Naqu_10_20=0,Pagri_10_20=0,uHRB_10_20=0,Day=0)

for(i in 1:122){
temp_dsm_naqu_510<-mean(subset(df_dsm_Naqu_510,Day==i)$RMSE)
temp_dsm_naqu_1020<-mean(subset(df_dsm_Naqu_1020,Day==i)$RMSE)

temp_dsm_pagri_510<-mean(subset(df_dsm_Pagri_510,Day==i)$RMSE)
temp_dsm_pagri_1020<-mean(subset(df_dsm_Pagri_1020,Day==i)$RMSE)

temp_dsm_uhrb_510<-mean(subset(df_dsm_uHRB_510,Day==i)$RMSE)
temp_dsm_uhrb_1020<-mean(subset(df_dsm_uHRB_1020,Day==i)$RMSE)

temp_indicator<-c(temp_dsm_naqu_510,temp_dsm_pagri_510,temp_dsm_uhrb_510,
                                 temp_dsm_naqu_1020 ,temp_dsm_pagri_1020, temp_dsm_uhrb_1020,i)
dfdf_new<-rbind(dfdf_new,temp_indicator)
}
dfdf_new<-dfdf_new[-1,]
dfdf_new_plot<-melt(dfdf_new,id="Day")

ggplot(dfdf_new_plot, aes(x=variable, y=value, fill=variable)) +
  geom_violin(trim=FALSE,alpha=1)+
  geom_boxplot(width=0.1, fill = "grey")  + 
  #scale_y_continuous(breaks = seq(0,0.15, by=0.05)) +
  #ylim(0,0.2)+
 labs(title="(b) RMSE",y = expression("RMSE values (m"^3*"/m"^3*")"), x = "") +
    theme_bw()+
  theme(plot.title = element_text(size=20),
  panel.grid.minor = element_blank(),
  panel.grid.major.y = element_line(color = "gray",size = 0.5,linetype = 2),
   panel.grid.major.x = element_blank(),
        legend.position = "none",
        legend.key = element_rect(colour = NA, fill = NA),
        legend.justification=c(1, 0))+
  theme(axis.text.x = element_text( color="black",  size=20),axis.title.x=element_text(size=20),
          axis.text.y = element_text( color="black",  size=20),axis.title.y=element_text(size=20))+
  scale_fill_manual(values=c("#B8DFB9","#B8DFB9","#B8DFB9", "#99DAEE","#99DAEE","#99DAEE"))+
   scale_x_discrete(labels = c("Naqu", "Pagri", "uHRB","Naqu", "Pagri", "uHRB")) 

ggsave("violin_RMSE_0426.jpg", dpi=600, width = 9, height = 5)

dfdf_new<-data.frame(Naqu_5_10=0,Pagri_5_10=0,uHRB_5_10=0,
                                       Naqu_10_20=0,Pagri_10_20=0,uHRB_10_20=0,Day=0)

for(i in 1:122){
temp_dsm_naqu_510<-mean(subset(df_dsm_Naqu_510,Day==i)$ubRMSE)
temp_dsm_naqu_1020<-mean(subset(df_dsm_Naqu_1020,Day==i)$ubRMSE)

temp_dsm_pagri_510<-mean(subset(df_dsm_Pagri_510,Day==i)$ubRMSE)
temp_dsm_pagri_1020<-mean(subset(df_dsm_Pagri_1020,Day==i)$ubRMSE)

temp_dsm_uhrb_510<-mean(subset(df_dsm_uHRB_510,Day==i)$ubRMSE)
temp_dsm_uhrb_1020<-mean(subset(df_dsm_uHRB_1020,Day==i)$ubRMSE)

temp_indicator<-c(temp_dsm_naqu_510,temp_dsm_pagri_510,temp_dsm_uhrb_510,
                                 temp_dsm_naqu_1020 ,temp_dsm_pagri_1020, temp_dsm_uhrb_1020,i)
dfdf_new<-rbind(dfdf_new,temp_indicator)
}
dfdf_new<-dfdf_new[-1,]
dfdf_new_plot<-melt(dfdf_new,id="Day")

ggplot(dfdf_new_plot, aes(x=variable, y=value, fill=variable)) +
  geom_violin(trim=FALSE,alpha=1)+
  geom_boxplot(width=0.1, fill = "grey")  + 
  #scale_y_continuous(breaks = seq(0,0.15, by=0.05)) +
  #ylim(0,0.2)+
 labs(title="(c) ubRMSE",y = expression("ubRMSE values (m"^3*"/m"^3*")"), x = "") +
    theme_bw()+
  theme(plot.title = element_text(size=20),
  panel.grid.minor = element_blank(),
  panel.grid.major.y = element_line(color = "gray",size = 0.5,linetype = 2),
   panel.grid.major.x = element_blank(),
        legend.position = "none",
        legend.key = element_rect(colour = NA, fill = NA),
        legend.justification=c(1, 0))+
  theme(axis.text.x = element_text( color="black",  size=20),axis.title.x=element_text(size=20),
          axis.text.y = element_text( color="black",  size=20),axis.title.y=element_text(size=20))+
  scale_fill_manual(values=c("#B8DFB9","#B8DFB9","#B8DFB9", "#99DAEE","#99DAEE","#99DAEE"))+
   scale_x_discrete(labels = c("Naqu", "Pagri", "uHRB","Naqu", "Pagri", "uHRB")) 

ggsave("violin_ubRMSE_0426.jpg", dpi=600, width = 9, height = 5)

##NSE
df_dsm_NSE_510<-read.csv("validation_NSE_0426_5_10.csv")
df_dsm_NSE_1020<-read.csv("validation_NSE_0426_10_20.csv")
day<-c(1:122)

dfdf_nse<-data.frame(Naqu_5_10=0,Pagri_5_10=0,uHRB_5_10=0,
                                       Naqu_10_20=0,Pagri_10_20=0,uHRB_10_20=0,process=0)

temp_nse<-cbind(df_dsm_NSE_510[,2:4],df_dsm_NSE_1020[,2:5])
 colnames(temp_nse)<- colnames(dfdf_nse)
dfdf_nse<-rbind(dfdf_nse,temp_nse)
dfdf_nse<-dfdf_nse[-1,]

dfdf_new_plot<-melt(dfdf_nse,id="process")

ggplot(dfdf_new_plot, aes(x=variable, y=value, fill=variable)) +
  geom_violin(trim=FALSE,alpha=1)+
  geom_boxplot(width=0.1, fill = "grey")  + 
  #scale_y_continuous(breaks = seq(0,0.2, by=0.06)) +
  ylim(-30,5)+
 labs(title="(d) NSE",y = "NSE values", x = "") +
    theme_bw()+
  theme(plot.title = element_text(size=20),
  panel.grid.minor = element_blank(),
  panel.grid.major.y = element_line(color = "gray",size = 0.5,linetype = 2),
   panel.grid.major.x = element_blank(),
        legend.position = "none",
        legend.key = element_rect(colour = NA, fill = NA),
        legend.justification=c(1, 0))+
  theme(axis.text.x = element_text( color="black",  size=20),axis.title.x=element_text(size=20),
          axis.text.y = element_text( color="black",  size=20),axis.title.y=element_text(size=20))+
  scale_fill_manual(values=c("#B8DFB9","#B8DFB9","#B8DFB9", "#99DAEE","#99DAEE","#99DAEE"))+
   scale_x_discrete(labels = c("Naqu", "Pagri", "uHRB","Naqu", "Pagri", "uHRB")) 

ggsave("violin_NSE_0426.jpg", dpi=600, width = 9, height = 5)


#xy scatter plot visualization
#==============================================

##DM 0-5cm
##-----------------------------
library(ggplot2)
library(ggExtra)
library(ggpointdensity)

df<-read.csv("result.csv")
df<-df[df$Obs<0.55,]
df<-na.omit(df)
df$Network<-as.factor(df$Network)
df$Site<-as.factor(df$Site)

predicted <- {}
observed <- {}
site_name<-levels(df$Site)

for(i in 1: length(site_name)){
df_site<-subset(df,Site==site_name[i])
df_site$Day<-as.factor(df_site$Day)
day_name<-as.integer(levels(df_site$Day))
for(j in 1:length(day_name)){
df_site_day<-subset(df_site,Day==day_name[j])
pre_temp<-mean(df_site_day$Pre)
obs_temp<-mean(df_site_day$Obs)
if(is.na(pre_temp)){
print(i)
print(j)}
if(is.na(obs_temp)){
print(i)
print(j)}
predicted <- c(predicted,pre_temp)
observed <- c(observed,obs_temp)
}
}

data <- data.frame(predicted, observed)
fit <- lm(observed ~ predicted, data = data)
intercept <- coef(fit)[1]
slope <- coef(fit)[2]
correlation <- cor(predicted, observed)
formula <- paste("y = ", round(intercept, 2), " + ", round(slope, 2), "x\nR = ", round(correlation, 2),"  ", sep = "")

p <- ggplot(data, aes(x = predicted, y = observed)) +
  geom_pointdensity(alpha = 0.6,size=0.8) +
   scale_color_viridis_c(option = "magma") + 
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "red") +
  theme_light()+
  theme(axis.title = element_text(size=13,  color="black"),
    axis.text = element_text(size=13,  color="black"),
    legend.text = element_text(size=13,  color="black"),
    legend.title = element_text(size=13,  color="black")) +
  theme(plot.title = element_text(size=15,margin = margin(b = 1))) +
  annotate("text", x = Inf, y = -Inf, 
           label = formula, hjust = 1.1, vjust = -0.5, color = "black", size = 5) +
  labs(title="(a) Downscaling method at 0-5 cm",
       x = expression("1-km soil moisture prediction (m"^3*"/m"^3*")"), 
       y = expression("In-situ observations (m"^3*"/m"^3*")"),color = "Density") +
  xlim(0, 0.65) + ylim(0, 0.65)
p <- ggMarginal(p, type = "density", fill = "grey",alpha = 0.5)
print(p)
setwd("C:/Users/dell/Desktop")
ggsave("a.jpg",plot=p,dpi=600,width = 6.8,height = 5)


##IM 0-5cm
##-----------------------
library(ggplot2)
library(ggExtra)
#setwd("E:/000_论文写作/1_样条函数-高垂直分辨率预测/work/5 result/8 XY拟合图/1 表层降尺度")
setwd("E:/000_论文写作/1_样条函数-高垂直分辨率预测/work/5 result/8 XY拟合图/2 表层插值方法")
#setwd("E:/000_论文写作/1_样条函数-高垂直分辨率预测/work/5 result/8 XY拟合图/3 深层插值方法")

df<-read.csv("result.csv")
df<-df[df$Obs<0.55,]
df<-na.omit(df)
df$Network<-as.factor(df$Network)
df$Site<-as.factor(df$Site)

predicted <- {}
observed <- {}
site_name<-levels(df$Site)

for(i in 1: length(site_name)){
df_site<-subset(df,Site==site_name[i])
df_site$Day<-as.factor(df_site$Day)
day_name<-as.integer(levels(df_site$Day))
for(j in 1:length(day_name)){
df_site_day<-subset(df_site,Day==day_name[j])
pre_temp<-mean(df_site_day$Pre)
obs_temp<-mean(df_site_day$Obs)
if(is.na(pre_temp)){
print(i)
print(j)}
if(is.na(obs_temp)){
print(i)
print(j)}
predicted <- c(predicted,pre_temp)
observed <- c(observed,obs_temp)
}
}
data <- data.frame(predicted, observed)
fit <- lm(observed ~ predicted, data = data)
intercept <- coef(fit)[1]
slope <- coef(fit)[2]
correlation <- cor(predicted, observed)
formula <- paste("y = ", round(intercept, 2), " + ", round(slope, 2), "x\nR = ", round(correlation, 2),"  ", sep = "")

p <- ggplot(data, aes(x = predicted, y = observed)) +
  geom_pointdensity(alpha = 0.6,size=0.8) +
   scale_color_viridis_c(option = "magma") + 
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "red") +
  theme_light()+
  theme(axis.title = element_text(size=13,  color="black"),
    axis.text = element_text(size=13,  color="black"),
    legend.text = element_text(size=13,  color="black"),
    legend.title = element_text(size=13,  color="black")) +
  theme(plot.title = element_text(size=15,margin = margin(b = 1))) +
  annotate("text", x = Inf, y = -Inf, 
           label = formula, hjust = 1.1, vjust = -0.5, color = "black", size = 5) +
  labs(title="(b) Interpolation method at 0-5 cm",
       x = expression("1-km soil moisture prediction (m"^3*"/m"^3*")"), 
       y = expression("In-situ observations (m"^3*"/m"^3*")"),color = "Density") +
  xlim(0, 0.65) + ylim(0, 0.65)
p <- ggMarginal(p, type = "density", fill = "grey",alpha = 0.5)
print(p)
setwd("C:/Users/dell/Desktop")
ggsave("b.jpg",plot=p,dpi=600,width = 6.8,height = 5)


##IMDF 5-10cm
##-----------------------------------------------
library(ggplot2)
library(ggExtra)
df<-read.csv("result_5_10.csv")
df<-df[df$Obs<0.55,]
df<-na.omit(df)
df$Network<-as.factor(df$Network)
df$Site<-as.factor(df$Site)


predicted <- {}
observed <- {}
site_name<-levels(df$Site)

for(i in 1: length(site_name)){
df_site<-subset(df,Site==site_name[i])
df_site$Day<-as.factor(df_site$Day)
day_name<-as.integer(levels(df_site$Day))
for(j in 1:length(day_name)){
df_site_day<-subset(df_site,Day==day_name[j])
pre_temp<-mean(df_site_day$Pre)
obs_temp<-mean(df_site_day$Obs)
if(is.na(pre_temp)){
print(i)
print(j)}
if(is.na(obs_temp)){
print(i)
print(j)}
predicted <- c(predicted,pre_temp)
observed <- c(observed,obs_temp)
}
}
data <- data.frame(predicted, observed)
fit <- lm(observed ~ predicted, data = data)
intercept <- coef(fit)[1]
slope <- coef(fit)[2]
correlation <- cor(predicted, observed)
formula <- paste("y = ", round(intercept, 2), " + ", round(slope, 2), "x\nR = ", round(correlation, 2),"  ", sep = "")

p <- ggplot(data, aes(x = predicted, y = observed)) +
  geom_pointdensity(alpha = 0.6,size=0.8) +
   scale_color_viridis_c(option = "magma") + 
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "red") +
  theme_light()+
  theme(axis.title = element_text(size=13,  color="black"),
    axis.text = element_text(size=13,  color="black"),
    legend.text = element_text(size=13,  color="black"),
    legend.title = element_text(size=13,  color="black")) +
  theme(plot.title = element_text(size=15,margin = margin(b = 1))) +
  annotate("text", x = Inf, y = -Inf, 
           label = formula, hjust = 1.1, vjust = -0.5, color = "black", size = 5) +
  labs(title="(c) Interpolation method at 5-10 cm",
       x = expression("1-km soil moisture prediction (m"^3*"/m"^3*")"), 
       y = expression("In-situ observations (m"^3*"/m"^3*")"),color = "Density") +
  xlim(0, 0.65) + ylim(0, 0.65)
p <- ggMarginal(p, type = "density", fill = "grey",alpha = 0.5)
print(p)
ggsave("c.jpg",plot=p,dpi=600,width = 6.8,height = 5)

##IMDF 10-20cm
##----------------------------------------
library(ggplot2)
library(ggExtra)
df<-read.csv("result_10_20.csv")
df<-na.omit(df)
df$Network<-as.factor(df$Network)
df$Site<-as.factor(df$Site)

predicted <- {}
observed <- {}
site_name<-levels(df$Site)

for(i in 1: length(site_name)){
df_site<-subset(df,Site==site_name[i])
df_site$Day<-as.factor(df_site$Day)
day_name<-as.integer(levels(df_site$Day))
for(j in 1:length(day_name)){
df_site_day<-subset(df_site,Day==day_name[j])
pre_temp<-mean(df_site_day$Pre)
obs_temp<-mean(df_site_day$Obs)
if(is.na(pre_temp)){
print(i)
print(j)}
if(is.na(obs_temp)){
print(i)
print(j)}
predicted <- c(predicted,pre_temp)
observed <- c(observed,obs_temp)
}
}
data <- data.frame(predicted, observed)
fit <- lm(observed ~ predicted, data = data)
intercept <- coef(fit)[1]
slope <- coef(fit)[2]
correlation <- cor(predicted, observed)
formula <- paste("y = ", round(intercept, 2), " + ", round(slope, 2), "x\nR = ", round(correlation, 2),"  ", sep = "")

p <- ggplot(data, aes(x = predicted, y = observed)) +
  geom_pointdensity(alpha = 0.6,size=0.8) +
   scale_color_viridis_c(option = "magma") + 
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "red") +
  theme_light()+
  theme(axis.title = element_text(size=13,  color="black"),
    axis.text = element_text(size=13,  color="black"),
    legend.text = element_text(size=13,  color="black"),
    legend.title = element_text(size=13,  color="black")) +
  theme(plot.title = element_text(size=15,margin = margin(b = 1))) +
  annotate("text", x = Inf, y = -Inf, 
           label = formula, hjust = 1.1, vjust = -0.5, color = "black", size = 5) +
  labs(title="(d) Interpolation method at 10-20 cm",
       x = expression("1-km soil moisture prediction (m"^3*"/m"^3*")"), 
       y = expression("In-situ observations (m"^3*"/m"^3*")"),color = "Density") +
  xlim(0, 0.65) + ylim(0, 0.65)
p <- ggMarginal(p, type = "density", fill = "grey",alpha = 0.5)
print(p)
ggsave("d.jpg",plot=p,dpi=600,width = 6.8,height = 5)


