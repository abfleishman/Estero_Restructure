# Clear work space; the funtion below lists all objects in workspace so it is good to start with a blank slate
rm(list=ls());
# Load libraries; these are not all used
library(lubridate)
library(stringr)
library(timeDate)
library(doBy)
library(ggplot2)
library(lubridate)
library(gdata)
library(plyr)
library(data.table)
library(XLConnect)
library(gridExtra)
library(reshape2)
# Using the XLConnect library use the loop below to read in each spreadsheet from your data
# you must change the path to the data in the loadWorkbook function

# for(i in 1:37){assign(paste("ESC_In_Sheet",i,sep="_"), readWorksheet(loadWorkbook("~/Desktop/SantaCruzCensus2009-2011.xlsx"), sheet = i, startRow=1, endRow=400, header=F))}
for(i in 1:19){assign(paste("ESC_In_Sheet",i,sep="_"), readWorksheet(loadWorkbook("~/Desktop/Santa Cruz_Census_2013_2014.xlsx"), sheet = i, startRow=1, endRow=400, header=F))}
# for(i in 1:10){assign(paste("ESC_In_Sheet",i,sep="_"), readWorksheet(loadWorkbook("~/Desktop/SantaCruzCensus_2012_2013.xlsx"), sheet = i, startRow=1, endRow=400, header=F))}

ESC_list<-ls(pattern = "ESC_In_Sheet")
#  Temp1<-ESC_In_Sheet_1
# head(Temp1)
##################
Restructure<-function(WSlist,Estero, outPath){
  Temp3<-NULL
  for(j in 1:length(WSlist)){
    Temp1<-get(WSlist[j])
    Temp2<-NULL
    for(i in 2:length(Temp1[1,])){
      Temp<-cbind(Temp1[1],Temp1[i])
      Temp$Location  <-Temp[1,2]
      Temp$Date  <-as.Date(Temp[2,2])
      Temp$Date<- as.Date(as.POSIXct(Temp$Date,format="%Y-%m-%d"))                          
      Temp$Start  <-Temp[3,2]
      Temp$Start_Hour<-hour(Temp$Start)
      Temp$Start_Minute<-minute(Temp$Start)
      Temp$Start  <-strftime(Temp$Start, format="%H:%M")
      Temp$Start_Date_Time<-timeDate(paste(Temp$Date,Temp$Start_Hour, Temp$Start_Minute), format="%Y-%m-%d %H %M")
      Temp$End <-Temp[4,2]
      Temp$End_Hour<-hour(Temp$End)
      Temp$End_Minute<-minute(Temp$End)
      Temp$End  <-strftime(Temp$End, format="%H:%M")
      Temp$End_Date_Time<-timeDate(paste(Temp$Date,Temp$End_Hour, Temp$End_Minute), format="%Y-%m-%d %H %M")      
      Temp$Duration  <-Temp[5,2]
      Temp$Total_Count_Duration  <-Temp[6,2]
      Temp$Sky_cover_percent  <-Temp[7,2]
      Temp$Temp  <-Temp[8,2]
      Temp$Tide_Height  <-Temp[9,2]
      Temp$Tide_dir  <-Temp[10,2]
      Temp$Wind_Dir  <-Temp[11,2]
      Temp$Wind_speed_Beaufort  <-Temp[12,2]
      Temp$Wind_speed_MPH  <-NA
      Temp$Observers_num  <-Temp[13,2]
      Temp$Observers_initial  <-Temp[14,2]
      Temp$Notes  <-Temp[15,2]
      Temp$Grand_sp_total  <-Temp[16,2]

      names(Temp)[names(Temp)=="Col1"] <- "Species"
      names(Temp)[2] <- "Count"
      head(Temp,19)
      Temp<-Temp[18:length(Temp[,1]),]
      Temp$Count<-ifelse(is.na(Temp$Count),0,Temp$Count)
      Temp2<-rbind(Temp2, Temp)
    }
    Temp3<-rbind(Temp3,Temp2)

   }
    # Make a column for which estero is is    
    Temp3$Estero<-"Estero"
    # Fix location names so that they are standard 
    Temp3$Location<-gsub(x = Temp3$Location,pattern = "Estero La Cruz, ",replacement = "")
    Temp3$Location<-gsub(x = Temp3$Location,pattern = "Estero Santa Cruz, ",replacement = "")
    Temp3$Location<-gsub(x = Temp3$Location,pattern = "Punta la Cruz",replacement = "Punta Santa Cruz")
    Temp3$Location<-gsub(x = Temp3$Location,pattern = "Oyster farm",replacement = "Oyster Farm")
    Temp3$Location<-gsub(x = Temp3$Location,pattern = "Far levee site",replacement = "Far Levee Site")
    Temp3$Location<-gsub(x = Temp3$Location,pattern = "Crab Coop",replacement = "Crab Camp")
    Temp3$Location<-gsub(x = Temp3$Location,pattern = "Crab camp far",replacement = "Crab Camp, Far")
    
    Temp3$Loc_Code<-Temp3$Location
    Temp3$Loc_Code<-gsub(x = Temp3$Loc_Code,pattern = "Heron Island",replacement = "HI")
    Temp3$Loc_Code<-gsub(x = Temp3$Loc_Code,pattern = "Far Levee Site",replacement = "FLS")
    Temp3$Loc_Code<-gsub(x = Temp3$Loc_Code,pattern = "Levee Site",replacement = "LS")
    Temp3$Loc_Code<-gsub(x = Temp3$Loc_Code,pattern = "Crab Camp, Far",replacement = "CCF")
    Temp3$Loc_Code<-gsub(x = Temp3$Loc_Code,pattern = "Crab Camp",replacement = "CC")
    Temp3$Loc_Code<-gsub(x = Temp3$Loc_Code,pattern = "Middle Mud",replacement = "MM")
    Temp3$Loc_Code<-gsub(x = Temp3$Loc_Code,pattern = "Oyster Farm",replacement = "OF")
    Temp3$Loc_Code<-gsub(x = Temp3$Loc_Code,pattern = "Punta Santa Cruz",replacement = "PSC")
    Temp3$Loc_Code<-gsub(x = Temp3$Loc_Code,pattern = "Heron Colony",replacement = "HC")
    Temp3$Loc_Code<-gsub(x = Temp3$Loc_Code,pattern = "Montecristo",replacement = "M")
    Temp3$Tide_Height<-gsub(x = Temp3$Tide_Height,pattern = "Mid/high",replacement = "Mid/High")
  names(Temp3)  
  Temp3<-Temp3[,c("Location","Loc_Code","Estero","Date","Start_Date_Time","End_Date_Time","Start","Start_Hour","Start_Minute","End","End_Hour","End_Minute","Duration","Total_Count_Duration",
                    "Grand_sp_total","Sky_cover_percent","Temp","Tide_Height","Tide_dir","Wind_Dir","Wind_speed_Beaufort","Wind_speed_MPH","Observers_num","Observers_initial","Species","Count","Notes")]
    
    # split the notes from the counts
    Counts <- colsplit(Temp3$Count, ",", names=c("Count","Notes"))
    #fix a value with a "-" in the cell
    Counts$Count<-gsub("-","",Counts$Count)
    
    # add the fixed counts and notes to the data
    Temp3$Count<-as.numeric(Counts$Count)
    Temp3$Notes<-paste(Temp3$Notes,Counts$Notes, sep= " ; ")
    
    # Write the data to a CSV
  write.csv(Temp3,outPath,row.names = F)
}

Restructure(WSlist = ESC_list, Estero = "ESC", outPath = "~/ESCRestructured.csv")
ESCRestructured<-read.csv("~/ESCRestructured.csv")
# names(ESCRestructured)
unique(ESCRestructured$Date)
# 
# Counts <- colsplit(ESCRestructured$Count, ",", names=c("Count","Notes"))
# 
# #fix a value with a "-" in the cell
# Counts$Count<-gsub("-","",Counts$Count)
# unique(ESCRestructured$Count)
# unique(Counts$Count)
# unique(Counts$Notes)
# ESCRestructured$Notes<-paste( ESCRestructured$Notes,Counts$Notes, sep= " ; ")
# unique(hmm2)
# # add the fixed counts and notes to the data
# ESCRestructured$Count<-as.numeric(Counts$Count)
# ESCRestructured$Notes<-Counts$Notes
# 
# length(unique(ESCRestructured$Species))

#make list of species to check inconsistences in the names
birdList<-as.data.frame(sort(unique(ESCRestructured$Species)))
birdList$New<-birdList[,1]
birdList$Water<-"0"
 write.csv(birdList, "~/birdList2.csv")

#populate the correct names in the New column by opening in Excel
#also mark as waterbird "1" and non waterbird "0" in the Water column so that we can subset out the birds that we will not be analyzing. Do not include petrels and other birds that we have bever seen!

# read the data back in
birdList<-read.csv("~/birdList1.csv")
# ESCRestructured<-read.csv("~/ESCRestructured.csv")

# head(birdList)

#fix all the names
ESCRestructured$Species <- birdList$New[ match(ESCRestructured$Species , birdList$Species) ]
unique(ESCRestructured$Species)
unique(ESCRestructured$Date)
write.csv(ESCRestructured,"/Users/abramfleishman/Desktop/Research Fellow-Abram/Bird Monitoring Program/PC_ESC_2013_2014_restructured.csv",row.names = F)

names(ESC2013)==names(ESC2011)
names(ESC2013)==names(ESC2014)
names(ESC2014)==names(ESC2011)

names(ESC2013)
str(ESC2011)
ESC2014<-read.csv("/Users/abramfleishman/Desktop/Research Fellow-Abram/Bird Monitoring Program/PC_ESC_2013_2014_restructured.csv", stringsAsFactors=FALSE)
ESC2013<-read.csv("/Users/abramfleishman/Desktop/Research Fellow-Abram/Bird Monitoring Program/PC_ESC_2012_2013_restructured.csv",stringsAsFactors=FALSE)
ESC2011<-read.csv("/Users/abramfleishman/Desktop/Research Fellow-Abram/Bird Monitoring Program/PC_ESC_2009_2011_restructured.csv",stringsAsFactors=FALSE)
hmm<-rbind(ESC2011,ESC2013,ESC2014)
unique(hmm$Date)

birdList<-as.data.frame(sort(unique(hmm$Species)))
write.csv(hmm,"/Users/abramfleishman/Desktop/Research Fellow-Abram/Bird Monitoring Program/PC_ESC_2009_2014_restructured.csv")
# split the notes from the counts
Counts <- colsplit(ESCRestructured$Count, ",", names=c("Count","Notes"))

#fix a value with a "-" in the cell
Counts$Count<-gsub("-","",Counts$Count)

# add the fixed counts and notes to the data
ESCRestructured$Count<-as.numeric(Counts$Count)
ESCRestructured$Notes<-Counts$Notes

# subset by the waterbird column that we made earlier
Waterbirds<-subset(birdList,Water==1)

ESCRestructured_sub<-subset(ESCRestructured,(ESCRestructured$Species %in% Waterbirds$New))

sum(ESCRestructured$Count)

#you loos some because you are getting rid of some species
sum(ESCRestructured_sub$Count)
write.csv(ESCRestructured_sub,"/Users/abramfleishman/Desktop/Research Fellow-Abram/Bird Monitoring Program/PC_ESC_2009_2011_restructured.csv",row.names = F)

ggplot(ESCRestructured_sub, aes(x=as.Date(Date),y=Count))+facet_wrap(~Species)+
  ######
geom_line(stat="identity") +scale_fill_grey(start = 0, end = .6, na.value = "grey50")+
  geom_point(stat="identity") +
  #   geom_text(label=df1$Ind, size=3.5 , y=df1$Ind, x=df1$year, hjust=-0.4)+
#   scale_x_continuous(breaks=seq(2001,2013,2))+
  xlab(NULL)+
  ylab(NULL)+
  ylim(0,80)+
  guides(colour=FALSE)+
  #xlim(160,260)+
  ggtitle(NULL)+
  theme(legend.position=c(0.09, .85), 
        legend.text=element_text(face="plain", size=24),
        #         legend.background = element_rect(fill="grey90", size=2, linetype="solid"),
        legend.title = element_text(face="plain", size=24),
        plot.title = element_text(face="plain", size=24),
        axis.title.x = element_text(face="plain", size=24, vjust=0.05, hjust=0.5),
        axis.title.y = element_text(face="plain", size=24, angle=90,vjust=0.3, hjust=0.5),
        axis.text.x= element_text(face="plain",size=24, angle=90, colour="black",vjust=0.3, hjust=0.5),
        axis.text.y= element_text(face="plain",size=24, angle=0, colour="black"),        
        #axis.ticks.y = element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.background = element_rect(fill='white', colour='black'))













head(ESC_h_1)
escFun<-function(x){
  for(i in 1:length(x)){
    Temp<-as.data.frame(t(get(x[i])))
    Temp$Location<-row.names(Temp)
    row.names(Temp)<-NULL
    hmm<-t(Temp[1,])
    names(Temp)<-(hmm[,1])
    Temp2<-Temp[2:length(Temp),1:length(Temp)]
    Temp3 <- rowSums(is.na(Temp2)) < 10
    Temp2 <- Temp2[Temp3, ] 
    Temp4 <- colSums(is.na(Temp2)) < 10
    Temp2 <- Temp2[, Temp4] 
    Temp2$Date<- as.POSIXct(Temp2$Date)+(1462*60*60*24)
    names(Temp2)[1] <- "Location"
    Temp2<-Temp2[,-match(c("Col1"),names(Temp2))]
    if("Start" %in% names(Temp2)){names(Temp2)<-gsub("Start","Start_Time",names(Temp2))}else{print("Not there")}
    if("Start/end Time" %in% names(Temp2)){names(Temp2)<-gsub("Start/end Time","Start_Time", fixed=F,names(Temp2))}else{print("Not there")}
    if("Start Time" %in% names(Temp2)){names(Temp2)<-gsub("Start Time","Start_Time", fixed=F,names(Temp2))}else{print("Not there")}
    if("Diration of count" %in% names(Temp2)){names(Temp2)<-gsub("Diration of count","Duration", fixed=F,names(Temp2))}else{print("Not there")}
    if("Duration (min)" %in% names(Temp2)){names(Temp2)[names(Temp2)=="Duration (min)"]<-"Duration"}
    if("Num Observers" %in% names(Temp2)){names(Temp2)<-gsub("Num Observers","Obs", fixed=F,names(Temp2))}else{print("Not there")}
    if("Number of observers" %in% names(Temp2)){names(Temp2)<-gsub("Number of observers", "Obs", fixed=F,names(Temp2))}else{print("Not there")}
    if("sky" %in% names(Temp2)){names(Temp2)<-gsub("sky","Sky", fixed=F,names(Temp2))}else{print("Not there")}
    if("Sky cover" %in% names(Temp2)){names(Temp2)<-gsub("Sky cover","Sky", fixed=F,names(Temp2))}else{print("Not there")}
    if("tide" %in% names(Temp2)){names(Temp2)<-gsub("tide","Tide", fixed=F,names(Temp2))}else{print("Not there")}
    if("Wind (MPH)" %in% names(Temp2)){names(Temp2)[names(Temp2)=="Wind (MPH)"]<-"Wind_Speed"}    
    if("Wind Speed" %in% names(Temp2)){names(Temp2)<-gsub("Wind Speed","Wind_Speed", fixed=F,names(Temp2))}else{print("Not there")}
    if("Wind Dir." %in% names(Temp2)){names(Temp2)<-gsub("Wind Dir.","Wind_Dir", fixed=F,names(Temp2))}else{print("Not there")}
    if("Temperature" %in% names(Temp2)){names(Temp2)<-gsub("Temperature","Temp", fixed=F,names(Temp2))}else{print("Not there")}
    if("Wind direction" %in% names(Temp2)){names(Temp2)<-gsub("Wind direction","Wind_Dir", fixed=F,names(Temp2))}else{print("Not there")}   
    Temp2$Start_Time<-timeDate(Temp2$Start_Time, format="%H:%M")
    Temp2$hour<-hour(Temp2$Start_Time)
    Temp2$minute<-minute(Temp2$Start_Time)
    Temp2$Start_Time<-timeDate(paste(Temp2$Date,Temp2$hour, Temp2$minute), format="%Y-%m-%d %H %M")
  }
Temp2
}
for(i in 1:length(ESC_list)){assign(paste("tESC",i,sep=""), escFun(ESC_list[i]))}
theESC_list<-mget(ls(pattern="tESC"))

Effort<-do.call("rbind", theESC_list)
Effort$Key<-paste(Effort$Location,Effort$Date,Effort$Start_Time,sep="-")

###counts
for(i in 1:37){assign(paste("ESCcount",i,sep="_"), readWorksheet(loadWorkbook("~/Desktop/SantaCruzCensus2009-2011.xlsx"), sheet = i, startRow=1, endRow=300, header=F))}

ESCcount_list<-ls(pattern="ESCcount")
ESCcount_list[1]
head(ESCcount_1)
countFun<-function(x){
  for(i in 1:length(x)){
    Temp<-as.data.frame(t(get(x[i])))
    Temp$Location<-row.names(Temp)
    row.names(Temp)<-NULL
    hmm<-t(Temp[1,])
    names(Temp)<-(hmm[,1])
    Temp2<-Temp[2:length(Temp),1:length(Temp)]
    Temp3 <- rowSums(is.na(Temp2)) < length(Temp2[1,])
    Temp2 <- Temp2[Temp3, ] 
    Temp4 <- colSums(is.na(Temp2)) < length(Temp2[,1])
    Temp2 <- Temp2[, Temp4] 
    Temp2$Date<- as.POSIXct(Temp2$Date)+(1462*60*60*24)
    names(Temp2)[1] <- "Location"
    Temp2<-Temp2[,-match(c("Col1"),names(Temp2))]
    if("Start" %in% names(Temp2)){names(Temp2)<-gsub("Start","Start_Time",names(Temp2))}else{print("Not there")}
    if("Start/end Time" %in% names(Temp2)){names(Temp2)<-gsub("Start/end Time","Start_Time", fixed=F,names(Temp2))}else{print("Not there")}
    if("Start Time" %in% names(Temp2)){names(Temp2)<-gsub("Start Time","Start_Time", fixed=F,names(Temp2))}else{print("Not there")}
    if("Diration of count" %in% names(Temp2)){names(Temp2)<-gsub("Diration of count","Duration", fixed=F,names(Temp2))}else{print("Not there")}
    if("Duration (min)" %in% names(Temp2)){names(Temp2)[names(Temp2)=="Duration (min)"]<-"Duration"}
    if("Num Observers" %in% names(Temp2)){names(Temp2)<-gsub("Num Observers","Obs", fixed=F,names(Temp2))}else{print("Not there")}
    if("Number of observers" %in% names(Temp2)){names(Temp2)<-gsub("Number of observers", "Obs", fixed=F,names(Temp2))}else{print("Not there")}
    if("sky" %in% names(Temp2)){names(Temp2)<-gsub("sky","Sky", fixed=F,names(Temp2))}else{print("Not there")}
    if("Sky cover" %in% names(Temp2)){names(Temp2)<-gsub("Sky cover","Sky", fixed=F,names(Temp2))}else{print("Not there")}
    if("tide" %in% names(Temp2)){names(Temp2)<-gsub("tide","Tide", fixed=F,names(Temp2))}else{print("Not there")}
    if("Wind (MPH)" %in% names(Temp2)){names(Temp2)[names(Temp2)=="Wind (MPH)"]<-"Wind_Speed"}    
    if("Wind Speed" %in% names(Temp2)){names(Temp2)<-gsub("Wind Speed","Wind_Speed", fixed=F,names(Temp2))}else{print("Not there")}
    if("Wind Dir." %in% names(Temp2)){names(Temp2)<-gsub("Wind Dir.","Wind_Dir", fixed=F,names(Temp2))}else{print("Not there")}
    if("Temperature" %in% names(Temp2)){names(Temp2)<-gsub("Temperature","Temp", fixed=F,names(Temp2))}else{print("Not there")}
    if("Wind direction" %in% names(Temp2)){names(Temp2)<-gsub("Wind direction","Wind_Dir", fixed=F,names(Temp2))}else{print("Not there")}   
    Temp2$Start_Time<-timeDate(Temp2$Start_Time, format="%H:%M")
    Temp2$hour<-hour(Temp2$Start_Time)
    Temp2$minute<-minute(Temp2$Start_Time)
    Temp2$Start_Time<-timeDate(paste(Temp2$Date,Temp2$hour, Temp2$minute), format="%Y-%m-%d %H %M")
    Temp2$Key<-paste(Temp2$Location,Temp2$Date,Temp2$Start_Time,sep="-")
    
  }
  Temp2
}
for(i in 1:length(ESCcount_list)){assign(paste("countESC",i,sep=""), countFun(ESCcount_list[i]))}
dataESC_list<-mget(ls(pattern="countESC"))

head(countESC1)
countESC1[1,1:10]
Temp1<-cbind(countESC1[1,1:10],countESC1[1,11])
length(get("countESC1")[1,])
length(countESC1[1,])

mergeFun<-function(x){
  Temp<-get(x)
  for(i in 11:length(Temp[1,])){
    Temp1<-cbind(Temp[1,1:10],Temp[1,i])
    names(Temp1)[names(Temp1)=="countESC1[1, i]"]<-"Count"
    Temp1$Species<-colnames(countESC1[i])
#     Temp2<-cbind(Temp[2,1:10],Temp[2,i])
#     Temp3<-cbind(Temp[3,1:10],Temp[3,i])
#     Temp4<-cbind(Temp[4,1:10],Temp[4,i])
#     Temp5<-cbind(Temp[5,1:10],Temp[5,i])
#     Temp6<-cbind(Temp[6,1:10],Temp[6,i])
#     Temp7<-cbind(Temp[7,1:10],Temp[7,i])
#     Temp8<-cbind(Temp[8,1:10],Temp[8,i])

  Temp1}
}

fff<-mergeFun(x="countESC1")
countESC_list<-ls(pattern="countESC")
countESC_list[11]
for(j in 2:length(countESC_list)){assign(paste("hmmESC",j,sep=""), mergeFun(countESC_list[j]))}

Temp<-get(countESC_list[2])
Temp1<-cbind(Temp[1,1:10],Temp[1,11])
names(Temp1)[names(Temp1)=="Temp[1, 11]"]<-"Count"
Temp1$Species<-colnames(countESC1[11])
