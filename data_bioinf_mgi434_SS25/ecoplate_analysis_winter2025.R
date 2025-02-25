
# MGI 434 Data Analysis
# Matt Schnizlein
# 2/23/24

# Goal: analyze Ecoplate data from student isolated fecal communities (from pets)

# Load packages
library(data.table)
library(ggplot2)
# library(mdthemes)
library(dplyr)
library(readxl)

setwd("C:/Users/mksch/Dropbox (Personal)/matt_data/crosson_lab_msu_schnizlein/students/MMG_434/MMG_434_WINTER2024")

# Create custom functions
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

substrLeft <- function(x, n){
  substr(x, 1, n)
}

'%!in%'<-function(x,y)!('%in%'(x,y))

substrLeft("dog",1)
substrRight("dog",1)

"a" %!in% "a"

# Grabbing our data
name_final<-c("SS24-ECO-G1-T24.xlsx","SS24-ECO-G1-T48.xlsx","SS24-ECO-G1-T72.xlsx","SS24-ECO-G1-T96.xlsx","SS24-ECO-G1-T120.xlsx","SS24-ECO-G1-T144.xlsx",
              "SS24-ECO-G5dog-T24.xlsx","SS24-ECO-G5dog-T48.xlsx","SS24-ECO-G5dog-T72.xlsx","SS24-ECO-G5dog-T96.xlsx","SS24-ECO-G5dog-T120.xlsx","SS24-ECO-G5dog-T144.xlsx",
              "SS24-ECO-IVY-T24.xlsx","SS24-ECO-IVY-T48.xlsx","SS24-ECO-IVY-T72.xlsx","SS24-ECO-IVY-T96.xlsx","SS24-ECO-IVY-T120.xlsx","SS24-ECO-IVY-T144.xlsx",
              "SS24-ECO-WLW-T24.xlsx","SS24-ECO-WLW-T48.xlsx","SS24-ECO-WLW-T96.xlsx","SS24-ECO-WLW-T72.xlsx","SS24-ECO-WLW-T120.xlsx","SS24-ECO-WLW-T144.xlsx",
              "SS24-ECO-G6BD-T24.xlsx","SS24-ECO-G6BD-T48.xlsx","SS24-ECO-G6BD-T72.xlsx","SS24-ECO-G6BD-T96.xlsx","SS24-ECO-G6BD-T120.xlsx","SS24-ECO-G6BD-T144.xlsx",
              "SS24-ECO-KIS-T24.xlsx","SS24-ECO-KIS-T48.xlsx","SS24-ECO-KIS-T72.xlsx","SS24-ECO-KIS-T96.xlsx","SS24-ECO-KIS-T120.xlsx","SS24-ECO-KIS-T144.xlsx",
              "ecoplate_carbonsource_template_key.csv")


urls<-paste0("https://raw.githubusercontent.com/mschnizlein/mmg434_bioinformatics/main/data_bioinf_mgi434_SS24/",name_final) # creates URLs to datafiles

destinations<-paste0("data/",name_final) # creates destinations for data files

download.file(urls[1],destfile=destinations[1],mode="wb")

for (i in 1:37){
  download.file(urls[i],destfile=destinations[i],mode="wb")
}

g1_t024<-as.data.frame(read_xlsx(path = "data/SS24-ECO-G1-T24.xlsx"))
g1_t048<-as.data.frame(read_xlsx(path = "data/SS24-ECO-G1-T48.xlsx"))
g1_t072<-as.data.frame(read_xlsx(path = "data/SS24-ECO-G1-T72.xlsx"))
g1_t096<-as.data.frame(read_xlsx(path = "data/SS24-ECO-G1-T96.xlsx"))
g1_t120<-as.data.frame(read_xlsx(path = "data/SS24-ECO-G1-T120.xlsx"))
g1_t144<-as.data.frame(read_xlsx(path = "data/SS24-ECO-G1-T144.xlsx"))

# Group IVY
iv_t024<-as.data.frame(read_xlsx(path = "data/SS24-ECO-IVY-T24.xlsx"))
iv_t048<-as.data.frame(read_xlsx(path = "data/SS24-ECO-IVY-T48.xlsx"))
iv_t072<-as.data.frame(read_xlsx(path = "data/SS24-ECO-IVY-T72.xlsx"))
iv_t096<-as.data.frame(read_xlsx(path = "data/SS24-ECO-IVY-T96.xlsx"))
iv_t120<-as.data.frame(read_xlsx(path = "data/SS24-ECO-IVY-T120.xlsx"))
iv_t144<-as.data.frame(read_xlsx(path = "data/SS24-ECO-IVY-T144.xlsx"))

# Group KIS
ki_t024<-as.data.frame(read_xlsx(path = "data/SS24-ECO-KIS-T24.xlsx"))
ki_t048<-as.data.frame(read_xlsx(path = "data/SS24-ECO-KIS-T48.xlsx"))
ki_t072<-as.data.frame(read_xlsx(path = "data/SS24-ECO-KIS-T72.xlsx"))
ki_t096<-as.data.frame(read_xlsx(path = "data/SS24-ECO-KIS-T96.xlsx"))
ki_t120<-as.data.frame(read_xlsx(path = "data/SS24-ECO-KIS-T120.xlsx"))
ki_t144<-as.data.frame(read_xlsx(path = "data/SS24-ECO-KIS-T144.xlsx"))

# Group WLW
wl_t024<-as.data.frame(read_xlsx(path = "data/SS24-ECO-WLW-T24.xlsx"))
wl_t048<-as.data.frame(read_xlsx(path = "data/SS24-ECO-WLW-T48.xlsx"))
wl_t072<-as.data.frame(read_xlsx(path = "data/SS24-ECO-WLW-T72.xlsx"))
wl_t096<-as.data.frame(read_xlsx(path = "data/SS24-ECO-WLW-T96.xlsx"))
wl_t120<-as.data.frame(read_xlsx(path = "data/SS24-ECO-WLW-T120.xlsx"))
wl_t144<-as.data.frame(read_xlsx(path = "data/SS24-ECO-WLW-T144.xlsx"))

# Group 5 Dog
g5_t024<-as.data.frame(read_xlsx(path = "data/SS24-ECO-G5dog-T24.xlsx"))
g5_t048<-as.data.frame(read_xlsx(path = "data/SS24-ECO-G5dog-T48.xlsx"))
g5_t072<-as.data.frame(read_xlsx(path = "data/SS24-ECO-G5dog-T72.xlsx"))
g5_t096<-as.data.frame(read_xlsx(path = "data/SS24-ECO-G5dog-T96.xlsx"))
g5_t120<-as.data.frame(read_xlsx(path = "data/SS24-ECO-G5dog-T120.xlsx"))
g5_t144<-as.data.frame(read_xlsx(path = "data/SS24-ECO-G5dog-T144.xlsx"))

# Group 6 BD
g6_t024<-as.data.frame(read_xlsx(path = "data/SS24-ECO-G6BD-T24.xlsx"))
g6_t048<-as.data.frame(read_xlsx(path = "data/SS24-ECO-G6BD-T48.xlsx"))
g6_t072<-as.data.frame(read_xlsx(path = "data/SS24-ECO-G6BD-T72.xlsx"))
g6_t096<-as.data.frame(read_xlsx(path = "data/SS24-ECO-G6BD-T96.xlsx"))
g6_t120<-as.data.frame(read_xlsx(path = "data/SS24-ECO-G6BD-T120.xlsx"))
g6_t144<-as.data.frame(read_xlsx(path = "data/SS24-ECO-G6BD-T144.xlsx"))
# A key to map carbon sources to particular wells
key<-read.csv(file="data/ecoplate_carbonsource_template_key.csv",header=TRUE)


# Data is loaded!!

# now lets make a list of all of our data frames

df_list<-list("g1_t024"=g1_t024,"g1_t048"=g1_t048,"g1_t072"=g1_t072,"g1_t096"=g1_t096,"g1_t120"=g1_t120,"g1_t144"=g1_t144,
              "iv_t024"=iv_t024,"iv_t048"=iv_t048,"iv_t072"=iv_t048,"iv_t096"=iv_t096,"iv_t120"=iv_t120,"iv_t144"=iv_t144,
              "ki_t024"=ki_t024,"ki_t048"=ki_t048,"ki_t072"=ki_t072,"ki_t096"=ki_t096,"ki_t120"=ki_t120,"ki_t144"=ki_t144,
              "wl_t024"=wl_t024,"wl_t048"=wl_t048,"wl_t072"=wl_t072,"wl_t096"=wl_t096,"wl_t120"=wl_t120,"wl_t144"=wl_t144,
              "g5_t024"=g5_t024,"g5_t048"=g5_t048,"g5_t072"=g5_t072,"g5_t096"=g5_t096, "g5_t120"=g5_t120,"g5_t144"=g5_t144, 
              "g6_t024"=g6_t024,"g6_t048"=g6_t048,"g6_t072"=g6_t072,"g6_t096"=g6_t096, "g6_t120"=g6_t120,"g6_t144"=g6_t144)

str(df_list)

df_list[1]

df_list[[1]][,1]

for (i in 1:36){
  df_list[[i]]<-df_list[[i]][24:119,2:4]
  colnames(df_list[[i]])<-c("wellID",
                            paste0(substrRight(names(df_list[i]),4),"_OD590"),
                            paste0(substrRight(names(df_list[i]),4),"_OD750")) # using `substrRight` we are renaming each of the columns in the data frames wellID, and then time_XXX. XXX will be the right-most three characters of the dataframe name which are the time point in hours)
}

# Group 1
g1_list <-df_list[substrLeft(names(df_list),2) %in% "g1"] # making a list of all of group 1's timepoints
g1_data<-Reduce(function(x,y) merge(x, y, by.all="wellID", all= TRUE), g1_list) # merging these together into one data frame

g1_data<-as.data.table(merge(g1_data, key, by.all = "wellID", sort=FALSE))


g1_data.m<-melt.data.table(g1_data,id.vars = c("wellID","carbon_source"))

g1_data.m$timepoint<-gsub("[^0-9.-]","",substrLeft(g1_data.m$variable,4)) # creating a timepoint column
g1_data.m$timepoint<-as.numeric(g1_data.m$timepoint) # formating that column as a number
g1_data.m$group_OD<-paste0("g1_",g1_data.m$wellID,"_",g1_data.m$variable)

g1_carbon<-unique(g1_data.m[g1_data.m$value >= 0.15,]$carbon_source)

print(g1_carbon)

g1_pos<-g1_data.m[g1_data.m$carbon_source %in% g1_carbon,]

sub.data<-function(x){
  temp_list <- df_list[substrLeft(names(df_list),2) %in% x]
  temp_data<-Reduce(function(x,y) merge(x, y, by.all="wellID", all= TRUE), temp_list)
  temp_data<-as.data.table(merge(temp_data, key, by.all = "wellID", sort=FALSE))
  temp_data.m<-melt.data.table(temp_data,id.vars = c("wellID","carbon_source"))
  temp_data.m$timepoint<-as.numeric(gsub("[^0-9.-]","",substrLeft(temp_data.m$variable,4)))
  temp_data.m$group_OD<-paste0(x,"_",temp_data.m$wellID,"_",temp_data.m$variable)
  print(summary(temp_data.m[temp_data.m$carbon_source %in% "water",]$value))
  temp_carbon<-unique(temp_data.m[temp_data.m$value >= 0.15,]$carbon_source)
  temp_pos<-temp_data.m[temp_data.m$carbon_source %in% temp_carbon,]
  assign(paste0(x,"_pos"),temp_pos,envir=.GlobalEnv)
  assign(paste0(x,"_all"),temp_data.m,envir=.GlobalEnv)
  assign(paste0(x,"_carbon"),temp_carbon,envir=.GlobalEnv)}

sub.data("iv")
sub.data("ki")
sub.data("wl")
sub.data("g5")
sub.data("g6")
# Selected Carbon Sources
data_together<-Reduce(function(x,y) merge(x, y, by.all="group_OD", all= TRUE),list(g1_pos,iv_pos,ki_pos,wl_pos,g5_pos,g6_pos))
data_together$OD_type<-substrRight(data_together$group_OD,5)
data_together$groupID<-substrLeft(data_together$group_OD,2)
data_together<-data_together[,c("groupID","carbon_source","wellID","timepoint","OD_type","value")]

data_f<-dcast(data_together,groupID+carbon_source+wellID+timepoint~OD_type,value.var = "value")


# All data
data_together_all<-Reduce(function(x,y) merge(x, y, by.all="group_OD", all= TRUE),list(g1_data.m,iv_all,ki_all,wl_all,g5_all,g6_all))
data_together_all$OD_type<-substrRight(data_together_all$group_OD,5)
data_together_all$groupID<-substrLeft(data_together_all$group_OD,2)
data_together_all<-data_together_all[,c("groupID","carbon_source","wellID","timepoint","OD_type","value")]
data_f_all<-dcast(data_together_all,groupID+carbon_source+wellID+timepoint~OD_type,value.var = "value")


my_theme<-theme_bw() + theme(axis.line=element_line(color="black"),
                             panel.grid.major = element_blank(),
                             panel.grid.minor = element_blank(),
                             panel.border = element_blank(),
                             panel.background = element_blank(),
                             text=element_text(size = 12))

ggplot(data_f_all,aes(x=OD590,y=OD750))+geom_point()

data_f_all$OD590_adj<-data_f_all$OD590 - data_f_all$OD750
data_f_all$OD590_adj<-data_f_all$OD590_adj - 0.04



growth_all.p<-ggplot(data_f_all[data_f_all$groupID %in% "g1"],aes(x=timepoint/24,y=OD590_adj,color=carbon_source,group=carbon_source,fill=carbon_source))+
  geom_jitter(width=0.2, height=0)

plot(growth_all.p)

####

growth_all.p<-ggplot(data_f_all[data_f_all$groupID %in% "g1"],aes(x=timepoint/24,y=OD590_adj,color=carbon_source,group=carbon_source,fill=carbon_source))+
  geom_jitter(width=0.2, height=0)+
  stat_summary(fun=mean, geom="line", linewidth=1)+
  stat_summary(fun.data = mean_se,geom="ribbon",color="transparent",alpha=0.3)

plot(growth_all.p)

### Facet wrap
growth_all.p<-ggplot(data_f_all[data_f_all$groupID %in% "g1"],aes(x=timepoint/24,y=OD590_adj,color=carbon_source,group=carbon_source,fill=carbon_source))+
  geom_jitter(width=0.2, height=0)+
  stat_summary(fun=mean, geom="line", linewidth=1)+
  stat_summary(fun.data = mean_se,geom="ribbon",color="transparent",alpha=0.3)+
  facet_wrap(~carbon_source)

plot(growth_all.p)

#####


growth_all.p<-ggplot(data_f_all[data_f_all$groupID %in% "g1"],aes(x=timepoint/24,y=OD590_adj,color=carbon_source,group=carbon_source,fill=carbon_source))+ # let's divide our time by 24 to make it hours
  geom_jitter(width=0.2, height=0)+ 
  stat_summary(fun=mean, geom="line", linewidth=1)+
  stat_summary(fun.data = mean_se, geom="ribbon", color="transparent", alpha=0.3)+
  ylim(0,2)+
  scale_fill_discrete()+ # this sets color parameters for the mean_se ribbon
  scale_color_discrete()+ # this sets color parameters for the lines
  as_md_theme(my_theme)+ # this applies our theme from above to the plot
  ylab("OD590") + xlab("Time (Days)") + # here are our axis labels
  labs(color="",fill="") + # these would label our legend
  ggtitle("")+ # this would label the plot itself
  facet_wrap(~carbon_source)

plot(growth_all.p)

# Plotting by group
growth_all_bygroup.p<-ggplot(data_f_all,aes(x=timepoint/24,y=OD590_adj,color=groupID,group=groupID,fill=groupID))+ 
  geom_jitter(width=0.2, height=0)+ 
  stat_summary(fun=mean, geom="line", linewidth=1)+
  stat_summary(fun.data = mean_se, geom="ribbon", color="transparent", alpha=0.3)+
  ylim(0,2)+
  scale_fill_discrete()+ # this sets color parameters for the mean_se ribbon
  scale_color_discrete()+ # this sets color parameters for the lines
  as_md_theme(my_theme)+ # this applies our theme from above to the plot
  ylab("OD590") + xlab("Time (Days)") + # here are our axis labels
  labs(color="",fill="") + # these would label our legend
  ggtitle("")+ # this would label the plot itself
  facet_wrap(~carbon_source)

plot(growth_all_bygroup.p)

# Manually setting color scheme
growth_all_bygroup.p<-ggplot(data_f_all,aes(x=timepoint/24,y=OD590_adj,color=groupID,group=groupID,fill=groupID))+ 
  geom_jitter(width=0.2, height=0)+ 
  stat_summary(fun=mean, geom="line", linewidth=1)+
  stat_summary(fun.data = mean_se, geom="ribbon", color="transparent", alpha=0.3)+
  ylim(0,2)+
  scale_fill_manual(values = c("#E69F00","#56B4E9","#CC79A7","#0072B2","#D55E00","#000000") )+
  scale_color_manual(values = c("#E69F00","#56B4E9","#CC79A7","#0072B2","#D55E00","#000000"))+
  as_md_theme(my_theme)+
  ylab("OD590") + xlab("Time (Days)") +
  labs(color="",fill="") +
  ggtitle("")+
  facet_wrap(~carbon_source)

plot(growth_all_bygroup.p)

# Positive hits (Carbon sources)
data_f_all.avg<-aggregate(OD590_adj~groupID+carbon_source+timepoint,data=data_f_all,FUN=mean)

pos.p<-ggplot(data_f_all.avg[data_f_all.avg$OD590_adj >= 0.1,], aes(x=timepoint/24))+geom_bar()+
  ylim(0,6)+
  as_md_theme(my_theme)+
  ylab("Positive Samples (OD590 > 0.1)") + xlab("Time (Days)") + labs() +
  ggtitle("")+
  facet_wrap(~carbon_source)

plot(pos.p)





