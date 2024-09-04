rm(list=ls())
library(data.table)
library(dplyr)
library(plyr)
library(ipfr)
# Create scenarios directories
# OutputCopy<-function(Region){
#   dir.create(paste("Exported/A1/3. Analysis/",deparse(substitute(Region)),sep=""))
#   scenarios<-c("S1-bCMA","S2-bCSD","S3-bCMA_CSD","S4-bADA","S5-bCMA_ADA","S6-bCT","S7-bCMA_CT","S8-bDA","S9-bCMA_DA","S10-hCMA","S11-hCSD","S12-hCMA_CSD","S13-hADA","S14-hCMA_ADA","S15-hCT","S16-hCMA_CT","S17-hDA","S18-hCMA_DA")
#   for (i in 1:length(scenarios)){
#     Scenario<-scenarios[i]
#     dir.create(paste("Exported/A1/3. Analysis/",deparse(substitute(Region)),"/",Scenario,sep=""))
# 
#     current_folder <- paste("Exported/A1/2. Popgen/PopGen2.1 - AGE & INC less categories/",substr(deparse(substitute(Region)),1,1),Scenario,"/",list.files(paste("Exported/A1/2. Popgen/PopGen2.1 - AGE & INC less categories/",substr(deparse(substitute(Region)),1,1),Scenario,sep=""),Scenario),sep="")
#     new_folder <- paste("Exported/A1/3. Analysis/",deparse(substitute(Region)),"/",Scenario,sep="")
#     list_of_files <- list.files(current_folder,)
#     file.copy(file.path(current_folder,list_of_files), new_folder)
# 
#     current_folder <- paste("Exported/A1/2. Popgen/PopGen2.1 - AGE & INC less categories/",substr(deparse(substitute(Region)),1,1),Scenario,sep="")
#     new_folder <- paste("Exported/A1/3. Analysis/",deparse(substitute(Region)),"/",Scenario,sep="")
#     list_of_files <- list.files(current_folder,)
#     file.copy(file.path(current_folder,list_of_files), new_folder)
#   }
# }
# OutputCopy(Montreal)

setwd("D:/OneDrive - polymtl.ca/Studies/Civil engineering/POLY/Projet Doctorat/PSP")
#GIS
GIS<-fread("Imported/GIS/Canada/2016/GIS Canada 2016.csv")
GIS<-GIS[GIS$`CMAuid/RMRidu`==462,c(38,27,25,41,7)]
colnames(GIS)<-c("CMA","CSD","ADA","CT","DA")
GIS$DAr<-GIS$DA
#MAT
CMAMAT<-fread("Exported/A1/Montreal/Inputs/Basic Census Totals/CMAMAT.csv")
CMAnames<-as.numeric(substring(names(CMAMAT)[ncol(CMAMAT)],4))
CSDMAT<-fread("Exported/A1/Montreal/Inputs/Basic Census Totals/CSDMAT.csv")
CSDnames<-as.numeric(substring(names(CSDMAT)[grepl("CSD",names(CSDMAT))],4))
ADAMAT<-fread("Exported/A1/Montreal/Inputs/Basic Census Totals/ADAMAT.csv")
ADAnames<-as.numeric(substring(names(ADAMAT)[grepl("ADA",names(ADAMAT))],4))
CTMAT<-fread("Exported/A1/Montreal/Inputs/Basic Census Totals/CTMAT.csv")
CTnames<-as.numeric(substring(names(CTMAT)[grepl("CT",names(CTMAT))],3))
DAMAT<-fread("Exported/A1/Montreal/Inputs/Basic Census Totals/DAMAT.csv")
DAnames<-as.numeric(substring(names(DAMAT)[grepl("DA",names(DAMAT))],3))
rm(CMAMAT,CSDMAT,ADAMAT,CTMAT,DAMAT)
GIS<-GIS[GIS$DA%in%DAnames,]
#sample
PUMFhh<-fread("Exported/A1/3. Analysis/Montreal/S1-bCMA/household_sample.csv")
PUMFpp<-fread("Exported/A1/3. Analysis/Montreal/S1-bCMA/person_sample.csv")
PUMFhh<-PUMFhh[,!grepl("rH",colnames(PUMFhh)) ,with=FALSE]
PUMFpp<-PUMFpp[,!(grepl("rP",colnames(PUMFpp)) | grepl("PP_IND",colnames(PUMFpp))),with=FALSE]
#Variables
varcat<-c("HH_ID",
           "HH_SIZE.1","HH_SIZE.2","HH_SIZE.3","HH_SIZE.4","HH_SIZE.5",
           "HH_TYPE.1","HH_TYPE.2","HH_TYPE.3","HH_TYPE.4",
           "HH_TOTINCAT.1", "HH_TOTINCAT.2", "HH_TOTINCAT.3","HH_TOTINCAT.4",
           "HH_DNROOM.1", "HH_DNROOM.2", "HH_DNROOM.3", "HH_DNROOM.4", "HH_DNROOM.5",
           "HH_DTYPE.1", "HH_DTYPE.2", "HH_DTYPE.3",
           "HH_DBUILT.1", "HH_DBUILT.2", "HH_DBUILT.3", "HH_DBUILT.4","HH_DBUILT.5","HH_DBUILT.6","HH_DBUILT.7",
           "PP_ID",
           "PP_SEX.1", "PP_SEX.2",
           "PP_AGEGRP.1", "PP_AGEGRP.2", "PP_AGEGRP.3",
           "PP_MARST.1", "PP_MARST.2", "PP_MARST.3", "PP_MARST.4",
           "PP_JTWMODE.1","PP_JTWMODE.2","PP_JTWMODE.3","PP_JTWMODE.4","PP_JTWMODE.5","PP_JTWMODE.6","PP_JTWMODE.7",
           "PP_JTWDUR.1","PP_JTWDUR.2","PP_JTWDUR.3","PP_JTWDUR.4","PP_JTWDUR.5","PP_JTWDUR.6",
           "PP_JTWLEAVE.1","PP_JTWLEAVE.2","PP_JTWLEAVE.3","PP_JTWLEAVE.4","PP_JTWLEAVE.5","PP_JTWLEAVE.6"
)
#CMAmarginals
bCMAmarginalshh<-fread("Exported/A1/3. Analysis/Montreal/S1-bCMA/geo_household_marginals.csv")[-2,]
bCMAmarginalspp<-fread("Exported/A1/3. Analysis/Montreal/S1-bCMA/geo_person_marginals.csv")[-2,]
bCMAmarginals<-cbind(bCMAmarginalshh,bCMAmarginalspp[,-1])
bCMAmarginals<-bCMAmarginals[,-1]
names(bCMAmarginals)<-paste(names(bCMAmarginals),".",bCMAmarginals[1,],sep="")
names(bCMAmarginals)[grepl("99",names(bCMAmarginals))]<-c("PP_IND.12","PP_JTWLEAVE.6","PP_JTWDUR.6", "PP_JTWMODE.7")
names(bCMAmarginals)[grepl("ID",names(bCMAmarginals))]<-c("HH_ID","PP_ID")
bCMAmarginals<-bCMAmarginals[-1,]
setcolorder(bCMAmarginals,match(varcat,names(bCMAmarginals)))
bCMAmarginals<-bCMAmarginals[,!grepl("PP_IND",names(bCMAmarginals)),with=FALSE]
hCMAmarginalshh<-fread("Exported/A1/3. Analysis/Montreal/S10-hCMA/region_household_marginals.csv")[-2,]
hCMAmarginalspp<-fread("Exported/A1/3. Analysis/Montreal/S10-hCMA/region_person_marginals.csv")[-2,]
hCMAmarginals<-cbind(hCMAmarginalshh,hCMAmarginalspp[,-1])
names(hCMAmarginals)[-1]<-substring(names(hCMAmarginals),2)[-1]
hCMAmarginals<-hCMAmarginals[,-1]
names(hCMAmarginals)<-paste(names(hCMAmarginals),".",hCMAmarginals[1,],sep="")
names(hCMAmarginals)[grepl("99",names(hCMAmarginals))]<-c("PP_IND.12","PP_JTWLEAVE.6","PP_JTWDUR.6", "PP_JTWMODE.7")
names(hCMAmarginals)[grepl("ID",names(hCMAmarginals))]<-c("HH_ID","PP_ID")
hCMAmarginals<-hCMAmarginals[-1,]
setcolorder(hCMAmarginals,match(varcat,names(hCMAmarginals)))
rm(bCMAmarginalshh,bCMAmarginalspp,hCMAmarginalshh,hCMAmarginalspp)
hCMAmarginals<-hCMAmarginals[,!grepl("PP_IND",names(hCMAmarginals)),with=FALSE]
#DA marginals
bDAmarginalshh<-fread("Exported/A1/3. Analysis/Montreal/S8-bDA/geo_household_marginals.csv")[-2,]
bDAmarginalspp<-fread("Exported/A1/3. Analysis/Montreal/S8-bDA/geo_person_marginals.csv")[-2,]
bDAmarginals<-cbind(bDAmarginalshh,bDAmarginalspp[,-1])
names(bDAmarginals)[-1]<-paste(names(bDAmarginals)[-1],".",bDAmarginals[1,-1],sep="")
bDAmarginals<-bDAmarginals[-1,]
names(bDAmarginals)[grepl("99",names(bDAmarginals))]<-c("PP_IND.12","PP_JTWLEAVE.6","PP_JTWDUR.6", "PP_JTWMODE.7")
names(bDAmarginals)[grepl("ID",names(bDAmarginals))]<-c("HH_ID","PP_ID")
bDAmarginals[,1]<-list(DAnames)
names(bDAmarginals)[1]<-"DA"
temp<-bDAmarginals[,-1]
setcolorder(temp,match(varcat,names(temp)))
bDAmarginals<-data.table(cbind(bDAmarginals[,1],temp))
bDAmarginals<<-bDAmarginals[,!grepl("PP_IND",names(bDAmarginals)),with=FALSE]
bDAmarginals<-data.table(apply(bDAmarginals,2,as.numeric))
hDAmarginalshh<-fread("Exported/A1/3. Analysis/Montreal/S17-hDA/geo_household_marginals.csv")[-2,]
hDAmarginalspp<-fread("Exported/A1/3. Analysis/Montreal/S17-hDA/geo_person_marginals.csv")[-2,]
hDAmarginals<-cbind(hDAmarginalshh,hDAmarginalspp[,-1])
names(hDAmarginals)[-1]<-paste(names(hDAmarginals)[-1],".",hDAmarginals[1,-1],sep="")
hDAmarginals<-hDAmarginals[-1,]
names(hDAmarginals)[grepl("99",names(hDAmarginals))]<-c("PP_IND.12","PP_JTWLEAVE.6","PP_JTWDUR.6", "PP_JTWMODE.7")
names(hDAmarginals)[grepl("ID",names(hDAmarginals))]<-c("HH_ID","PP_ID")
hDAmarginals[,1]<-list(DAnames)
names(hDAmarginals)[1]<-"DA"
temp<-hDAmarginals[,-1]
setcolorder(temp,match(varcat,names(temp)))
hDAmarginals<-data.table(cbind(hDAmarginals[,1],temp))
hDAmarginals<<-hDAmarginals[,!grepl("PP_IND",names(hDAmarginals)),with=FALSE]
hDAmarginals<-data.table(apply(hDAmarginals,2,as.numeric))
rm(bDAmarginalshh,bDAmarginalspp,hDAmarginalshh,hDAmarginalspp)
#CMA_STf
CMA_STf<-function(hs,ps){
  CMA_ST<-cbind(hs[, .N, by = .(geo,HH_ID)] %>% dcast(geo ~ HH_ID),
                hs[, .N, by = .(geo,HH_SIZE)] %>% dcast(geo ~ HH_SIZE),
                hs[, .N, by = .(geo,HH_TYPE)] %>% dcast(geo ~ HH_TYPE),
                hs[, .N, by = .(geo,HH_TOTINCAT)] %>% dcast(geo ~ HH_TOTINCAT),
                hs[, .N, by = .(geo,HH_DNROOM)] %>% dcast(geo ~ HH_DNROOM),
                hs[, .N, by = .(geo,HH_DTYPE)] %>% dcast(geo ~ HH_DTYPE),
                hs[, .N, by = .(geo,HH_DBUILT)] %>% dcast(geo ~ HH_DBUILT),
                ps[, .N, by = .(geo,PP_ID)] %>% dcast(geo ~ PP_ID),
                ps[, .N, by = .(geo,PP_SEX)] %>% dcast(geo ~ PP_SEX),
                ps[, .N, by = .(geo,PP_AGEGRP)] %>% dcast(geo ~ PP_AGEGRP),
                ps[, .N, by = .(geo,PP_MARST)] %>% dcast(geo ~ PP_MARST),
                ps[, .N, by = .(geo,PP_JTWMODE)] %>% dcast(geo ~ PP_JTWMODE),
                ps[, .N, by = .(geo,PP_JTWDUR)] %>% dcast(geo ~ PP_JTWDUR),
                ps[, .N, by = .(geo,PP_JTWLEAVE)] %>% dcast(geo ~ PP_JTWLEAVE)
  )
  CMA_ST<-cbind(CMA_ST[,1],CMA_ST[,!colnames(CMA_ST)=="geo", with=FALSE])
  names(CMA_ST)<-c("geo", varcat)
  CMA_ST[is.na(CMA_ST)]<-0
  CMA_ST<-CMA_ST[,lapply(.SD,sum)]
  CMA_ST<-CMA_ST[,!c("geo")]
  return (CMA_ST)
}
#CMA_ETf
CMA_ETf<-function(weights){
  CMAweightshh<-data.table(weights[,1],weight=rowSums(weights[,-1]))
  CMAweightspp<-merge(PUMFpp,CMAweightshh,by.x = "hid",by.y = "hid")
  CMAweightshh<-merge(PUMFhh,CMAweightshh,by.x = "hid",by.y = "hid")
  CMA_ET<-data.table()
  for (i in 3:9){
    temp<-as.data.table(t(arrange(CMAweightshh[, sum(weight),by=c(names(CMAweightshh)[i])],CMAweightshh[, sum(weight),by=c(names(CMAweightshh)[i])][,1])))
    names(temp)<-paste(names(CMAweightshh)[i],".",temp[1,],sep="")
    temp<-temp[-1,]
    CMA_ET<-cbind(CMA_ET,temp)
  }
  for (i in 4:10){
    temp<-as.data.table(t(arrange(CMAweightspp[, sum(weight),by=c(names(CMAweightspp)[i])],CMAweightspp[, sum(weight),by=c(names(CMAweightspp)[i])][,1])))
    names(temp)<-paste(names(CMAweightspp)[i],".",temp[1,],sep="")
    temp<-temp[-1,]
    CMA_ET<-cbind(CMA_ET,temp)
  }
  names(CMA_ET)[grepl("99",names(CMA_ET))]<-c("PP_JTWLEAVE.6","PP_JTWDUR.6", "PP_JTWMODE.7")
  names(CMA_ET)[grepl("ID",names(CMA_ET))]<-c("HH_ID","PP_ID")
  setcolorder(CMA_ET,match(names(CMA_ST),names(CMA_ET)))
  return(CMA_ET)
}
#DA_STf
DA_STf<-function(x){
  internal1<-function(x){
    ifelse(substr(x,1,2)=="HH",sample<-hs,sample<-ps)
    temp<-sample[, .N, by = c(x)][order(sample[, .N, by = c(x)],unlist(sample[, .N, by = c(x)][,1]))]
    temp[,1]<-paste(names(temp)[1],".",1:nrow(temp),sep="")
    temp<-as.data.table(t(temp))
    names(temp)<-unlist(temp[1,])
    temp<-temp[-1,]
  }
  weight_tbl<-cbind(wPUMFhh[,1],x)
  weight_tbl[,c("avg_weight"):= as.numeric(bDAmarginals[DA==as.numeric(names(weight_tbl)[2]),HH_ID])/length(unlist(x)),with=FALSE]
  weight_tbl[,c("weight_factor"):= as.numeric(unlist(weight_tbl[,2]))/weight_tbl[,avg_weight],with=FALSE]
  names(weight_tbl)[2]<-"weight"
  hs<-merge(wPUMFhh[,1:9],synthesize(weight_tbl, group_by=NULL ,primary_id="hid"))
  ps<-merge(hs[,1],wPUMFpp[,1:11],by="hid")
  as.data.table(sapply(c(names(wPUMFhh)[9:3],names(wPUMFpp)[10:4]),internal1))
}
#Indicators
IndicatorsMean<-function(x){
  temp<-data.table(
    "hh"=round(mean(unlist(x[,1:29])),2),
    "pp"=round(mean(unlist(x[,30:58])),2),
    "tot"=round(mean(unlist(x[,1:58])),2),
    "hhcon"=round(mean(unlist(x[,1:14])),2),
    "ppcon"=round(mean(unlist(x[,30:39])),2),
    "totcon"=round(mean(unlist(x[,c(1:14,30:39)])),2),
    "hhunc"=round(mean(unlist(x[,15:29])),2),
    "ppunc"=round(mean(unlist(x[,40:58])),2),
    "totunc"=round(mean(unlist(x[,c(15:29,40:58)])),2)
  )
}
IndicatorsSum<-function(x){
  temp<-data.table(
    "hh"=round(sum(unlist(x[,1:29])),2),
    "pp"=round(sum(unlist(x[,30:58])),2),
    "tot"=round(sum(unlist(x[,1:58])),2),
    "hhcon"=round(sum(unlist(x[,1:14])),2),
    "ppcon"=round(sum(unlist(x[,30:39])),2),
    "totcon"=round(sum(unlist(x[,c(1:14,30:39)])),2),
    "hhunc"=round(sum(unlist(x[,15:29])),2),
    "ppunc"=round(sum(unlist(x[,40:58])),2),
    "totunc"=round(sum(unlist(x[,c(15:29,40:58)])),2)
  )
}

#S1-bCMA
##Import files
setwd("D:/OneDrive - polymtl.ca/Studies/Civil engineering/POLY/Projet Doctorat/PSP/Exported/A1/3. Analysis/Montreal/S1-bCMA")
#weights
weights<-fread("weights.csv")
names(weights)[-1]<-CMAnames
#synthetic population
hs<-fread("housing_synthetic.csv")
ps<-fread("person_synthetic.csv")
hs<-hs[,!grepl("rH",names(hs)),with=FALSE]
ps<-ps[,!grepl("rP",names(ps)),with=FALSE]
ps<-ps[,!grepl("PP_IND",names(ps)),with=FALSE]
#geo marginals
gmarginalshh<-fread("geo_household_marginals.csv")
gmarginalspp<-fread("geo_person_marginals.csv")
gmarginals<-cbind(gmarginalshh,gmarginalspp[,-1])
names(gmarginals)[-1]<-paste(names(gmarginals)[-1],".",gmarginals[1,-1],sep="")
gmarginals<-gmarginals[-1:-2,]
names(gmarginals)[grepl("99",names(gmarginals))]<-c("PP_IND.12","PP_JTWLEAVE.6","PP_JTWDUR.6", "PP_JTWMODE.7")
names(gmarginals)[grepl("ID",names(gmarginals))]<-c("HH_ID","PP_ID")
gmarginals[,1]<-list(CMAnames)
names(gmarginals)[1]<-"CMA"
gmarginals[,1]<-as.numeric(unlist(gmarginals[,1]))
temp<-gmarginals[,-1]
setcolorder(temp,match(varcat,names(temp)))
gmarginals<-data.table(cbind(gmarginals[,1],temp))
rm(gmarginalshh,gmarginalspp)
gmarginals<-gmarginals[,!grepl("PP_IND",names(gmarginals)),with=FALSE]
##Selection totals at CMA level
CMA_ST<-CMA_STf(hs,ps)
##Estimation totals at CMA level
CMA_ET<-CMA_ETf(weights)
###Estimation totals at DA level
weights<-fread("weights.csv")
weights<-cbind(c("CMA",CMAnames),data.table(t(weights)))
colnames(weights)<-unlist(weights[1,])
weights<-weights[-1,]
weights[,1]<-as.numeric(unlist(weights[,1]))
DAweights<-merge(distinct(GIS[,c("DA","CMA")]),weights,by.x="CMA",by.y="CMA")
DAweights<-merge(DAweights,gmarginals[,c("CMA","HH_ID")],by.x="CMA",by.y="CMA")
DAweights<-merge(DAweights,bDAmarginals[,c("DA","HH_ID")],by.x="DA",by.y="DA")
DAweights<-as.data.table(t(cbind(DAweights[,DA],DAweights[,c(-1,-2,-(ncol(DAweights)-1),-ncol(DAweights)),with=FALSE]*DAweights[,HH_ID.y/HH_ID.x])))
colnames(DAweights)<-as.character(unlist(DAweights[1,]))
DAweights<-DAweights[-1,]
DAweights<-cbind(PUMFhh[,1],DAweights)
wPUMFhh<-data.table(merge(PUMFhh,DAweights,by="hid"))
wPUMFpp<-data.table(merge(PUMFpp,DAweights,by="hid"))
DA_ET=copy(bDAmarginals)
for (i in 3:9){
  x<-names(wPUMFhh)[i]
  temp<-wPUMFhh[,lapply(.SD,sum),by=c(x),.SDcols=names(wPUMFhh)[10:ncol(wPUMFhh)]]
  temp<-temp[order(temp[,1])]
  temp[,1]<-paste(names(temp)[1],".",1:nrow(temp),sep="")
  temp<-as.data.table(t(temp))
  names(temp)<-unlist(temp[1,])
  temp<-temp[-1,]
  DA_ET[,c(names(DA_ET)[grepl(x,names(DA_ET))]):=temp,with=FALSE]
}
for (i in 4:10){
  x<-names(wPUMFpp)[i]
  temp<-wPUMFpp[,lapply(.SD,sum),by=c(x),.SDcols=names(wPUMFpp)[11:ncol(wPUMFpp)]]
  temp<-temp[order(temp[,1])]
  temp[,1]<-paste(names(temp)[1],".",1:nrow(temp),sep="")
  temp<-as.data.table(t(temp))
  names(temp)<-unlist(temp[1,])
  temp<-temp[-1,]
  DA_ET[,c(names(DA_ET)[grepl(x,names(DA_ET))]):=temp,with=FALSE]
}
DA_ET<-cbind("DA"=as.numeric(DAnames),DA_ET[,-1][,match(names(CMA_ET),names(DA_ET[,-1])),with=FALSE])
DA_ET<-data.table(apply(DA_ET,2,as.numeric))
#Selection totals at DA level
DA_ST<-apply(wPUMFhh[,10:ncol(wPUMFhh)],2,DA_STf)
DA_ST<-rbindlist(lapply(DA_ST, as.data.frame.list), fill=TRUE)
names(DA_ST)<-names(CMA_ST)
DA_ST<-cbind(DA_ET[,1],DA_ST)
DA_ST[is.na(DA_ST)]<-0
DA_ST<-data.table(apply(DA_ST,2,as.numeric))
#Population Proximity Index
PPI<-round(mean(gmarginals$PP_ID)*1000/bCMAmarginals$PP_ID,2)
#Census discrepancies
deltaCensus<-abs(colSums(gmarginals)[-1] - bCMAmarginals)
deltaCensusErr1<-round(IndicatorsMean(deltaCensus*1000/bCMAmarginals),2)
deltaCensusErr2<-round(IndicatorsSum(deltaCensus)*1000/rep(c(bCMAmarginals$HH_ID,bCMAmarginals$PP_ID,bCMAmarginals$HH_ID+bCMAmarginals$PP_ID),3),2)
#Estimation fitting errors
estFit<-abs(CMA_ET - bCMAmarginals)
estFitErr1<-round(IndicatorsMean(estFit*1000/bCMAmarginals),2)
estFitErr2<-round(IndicatorsSum(estFit)*1000/rep(c(bCMAmarginals$HH_ID,bCMAmarginals$PP_ID,bCMAmarginals$HH_ID+bCMAmarginals$PP_ID),3),2)
#Selection fitting errors
selFit<-abs(CMA_ST - bCMAmarginals)
selFitErr1<-round(IndicatorsMean(selFit*1000/bCMAmarginals),2)
selFitErr2<-round(IndicatorsSum(selFit)*1000/rep(c(bCMAmarginals$HH_ID,bCMAmarginals$PP_ID,bCMAmarginals$HH_ID+bCMAmarginals$PP_ID),3),2)
#Estimation spatialization errors
estSpa<-data.table(t(colSums(abs(DA_ET-bDAmarginals)[,-1])))
estSpaErr1<-round(IndicatorsMean(estSpa*1000/bCMAmarginals),2)
estSpaErr2<-round(IndicatorsSum(estSpa)*1000/rep(c(bCMAmarginals$HH_ID,bCMAmarginals$PP_ID,bCMAmarginals$HH_ID+bCMAmarginals$PP_ID),3),2)
#Selection spatialization errors
selSpa<-data.table(t(colSums(abs(DA_ST-bDAmarginals)[,-1])))
selSpaErr1<-round(IndicatorsMean(selSpa*1000/bCMAmarginals),2)
selSpaErr2<-round(IndicatorsSum(selSpa)*1000/rep(c(bCMAmarginals$HH_ID,bCMAmarginals$PP_ID,bCMAmarginals$HH_ID+bCMAmarginals$PP_ID),3),2)
#Results
Errors<-cbind("error"=c("deltaCensusErr1","estFitErr1","selFitErr1","estSpaErr1","selSpaErr1","deltaCensusErr2","estFitErr2","selFitErr2","estSpaErr2","selSpaErr2"),"Scenario"=rep("S1",10),"PPI"=rep(PPI,10),rbind(deltaCensusErr1,estFitErr1,selFitErr1,estSpaErr1,selSpaErr1,deltaCensusErr2,estFitErr2,selFitErr2,estSpaErr2,selSpaErr2))
Results=copy(Errors)

#S2-bCSD
##Import files
setwd("D:/OneDrive - polymtl.ca/Studies/Civil engineering/POLY/Projet Doctorat/PSP/Exported/A1/3. Analysis/Montreal/S2-bCSD")
#weights
weights<-fread("weights.csv")
names(weights)[-1]<-CSDnames
#synthetic population
hs<-fread("housing_synthetic.csv")
ps<-fread("person_synthetic.csv")
hs<-hs[,!grepl("rH",names(hs)),with=FALSE]
ps<-ps[,!grepl("rP",names(ps)),with=FALSE]
ps<-ps[,!grepl("PP_IND",names(ps)),with=FALSE]
#geo marginals
gmarginalshh<-fread("geo_household_marginals.csv")
gmarginalspp<-fread("geo_person_marginals.csv")
gmarginals<-cbind(gmarginalshh,gmarginalspp[,-1])
names(gmarginals)[-1]<-paste(names(gmarginals)[-1],".",gmarginals[1,-1],sep="")
gmarginals<-gmarginals[-1:-2,]
names(gmarginals)[grepl("99",names(gmarginals))]<-c("PP_IND.12","PP_JTWLEAVE.6","PP_JTWDUR.6", "PP_JTWMODE.7")
names(gmarginals)[grepl("ID",names(gmarginals))]<-c("HH_ID","PP_ID")
gmarginals[,1]<-list(CSDnames)
names(gmarginals)[1]<-"CSD"
gmarginals[,1]<-as.numeric(unlist(gmarginals[,1]))
temp<-gmarginals[,-1]
setcolorder(temp,match(varcat,names(temp)))
gmarginals<-data.table(cbind(gmarginals[,1],temp))
rm(gmarginalshh,gmarginalspp)
gmarginals<-gmarginals[,!grepl("PP_IND",names(gmarginals)),with=FALSE]
##Selection totals at CMA level
CMA_ST<-CMA_STf(hs,ps)
##Estimation totals at CMA level
CMA_ET<-CMA_ETf(weights)
###Estimation totals at DA level
weights<-fread("weights.csv")
weights<-cbind(c("CSD",CSDnames),data.table(t(weights)))
colnames(weights)<-unlist(weights[1,])
weights<-weights[-1,]
weights[,1]<-as.numeric(unlist(weights[,1]))
DAweights<-merge(distinct(GIS[,c("DA","CSD")]),weights,by.x="CSD",by.y="CSD")
DAweights<-merge(DAweights,gmarginals[,c("CSD","HH_ID")],by.x="CSD",by.y="CSD")
DAweights<-merge(DAweights,bDAmarginals[,c("DA","HH_ID")],by.x="DA",by.y="DA")
DAweights<-as.data.table(t(cbind(DAweights[,DA],DAweights[,c(-1,-2,-(ncol(DAweights)-1),-ncol(DAweights)),with=FALSE]*DAweights[,HH_ID.y/HH_ID.x])))
colnames(DAweights)<-as.character(unlist(DAweights[1,]))
DAweights<-DAweights[-1,]
DAweights<-cbind(PUMFhh[,1],DAweights)
wPUMFhh<-data.table(merge(PUMFhh,DAweights,by="hid"))
wPUMFpp<-data.table(merge(PUMFpp,DAweights,by="hid"))
DA_ET=copy(bDAmarginals)
for (i in 3:9){
  x<-names(wPUMFhh)[i]
  temp<-wPUMFhh[,lapply(.SD,sum),by=c(x),.SDcols=names(wPUMFhh)[10:ncol(wPUMFhh)]]
  temp<-temp[order(temp[,1])]
  temp[,1]<-paste(names(temp)[1],".",1:nrow(temp),sep="")
  temp<-as.data.table(t(temp))
  names(temp)<-unlist(temp[1,])
  temp<-temp[-1,]
  DA_ET[,c(names(DA_ET)[grepl(x,names(DA_ET))]):=temp,with=FALSE]
}
for (i in 4:10){
  x<-names(wPUMFpp)[i]
  temp<-wPUMFpp[,lapply(.SD,sum),by=c(x),.SDcols=names(wPUMFpp)[11:ncol(wPUMFpp)]]
  temp<-temp[order(temp[,1])]
  temp[,1]<-paste(names(temp)[1],".",1:nrow(temp),sep="")
  temp<-as.data.table(t(temp))
  names(temp)<-unlist(temp[1,])
  temp<-temp[-1,]
  DA_ET[,c(names(DA_ET)[grepl(x,names(DA_ET))]):=temp,with=FALSE]
}
DA_ET<-cbind("DA"=as.numeric(DAnames),DA_ET[,-1][,match(names(CMA_ET),names(DA_ET[,-1])),with=FALSE])
DA_ET<-data.table(apply(DA_ET,2,as.numeric))
#Selection totals at DA level
DA_ST<-apply(wPUMFhh[,10:ncol(wPUMFhh)],2,DA_STf)
DA_ST<-rbindlist(lapply(DA_ST, as.data.frame.list), fill=TRUE)
names(DA_ST)<-names(CMA_ST)
DA_ST<-cbind(DA_ET[,1],DA_ST)
DA_ST[is.na(DA_ST)]<-0
DA_ST<-data.table(apply(DA_ST,2,as.numeric))
#Population Proximity Index
PPI<-round(mean(gmarginals$PP_ID)*1000/bCMAmarginals$PP_ID,2)
#Census discrepancies
deltaCensus<-abs(colSums(gmarginals)[-1] - bCMAmarginals)
deltaCensusErr1<-round(IndicatorsMean(deltaCensus*1000/bCMAmarginals),2)
deltaCensusErr2<-round(IndicatorsSum(deltaCensus)*1000/rep(c(bCMAmarginals$HH_ID,bCMAmarginals$PP_ID,bCMAmarginals$HH_ID+bCMAmarginals$PP_ID),3),2)
#Estimation fitting errors
estFit<-abs(CMA_ET - bCMAmarginals)
estFitErr1<-round(IndicatorsMean(estFit*1000/bCMAmarginals),2)
estFitErr2<-round(IndicatorsSum(estFit)*1000/rep(c(bCMAmarginals$HH_ID,bCMAmarginals$PP_ID,bCMAmarginals$HH_ID+bCMAmarginals$PP_ID),3),2)
#Selection fitting errors
selFit<-abs(CMA_ST - bCMAmarginals)
selFitErr1<-round(IndicatorsMean(selFit*1000/bCMAmarginals),2)
selFitErr2<-round(IndicatorsSum(selFit)*1000/rep(c(bCMAmarginals$HH_ID,bCMAmarginals$PP_ID,bCMAmarginals$HH_ID+bCMAmarginals$PP_ID),3),2)
#Estimation spatialization errors
estSpa<-data.table(t(colSums(abs(DA_ET-bDAmarginals)[,-1])))
estSpaErr1<-round(IndicatorsMean(estSpa*1000/bCMAmarginals),2)
estSpaErr2<-round(IndicatorsSum(estSpa)*1000/rep(c(bCMAmarginals$HH_ID,bCMAmarginals$PP_ID,bCMAmarginals$HH_ID+bCMAmarginals$PP_ID),3),2)
#Selection spatialization errors
selSpa<-data.table(t(colSums(abs(DA_ST-bDAmarginals)[,-1])))
selSpaErr1<-round(IndicatorsMean(selSpa*1000/bCMAmarginals),2)
selSpaErr2<-round(IndicatorsSum(selSpa)*1000/rep(c(bCMAmarginals$HH_ID,bCMAmarginals$PP_ID,bCMAmarginals$HH_ID+bCMAmarginals$PP_ID),3),2)
#Results
Errors<-cbind("error"=c("deltaCensusErr1","estFitErr1","selFitErr1","estSpaErr1","selSpaErr1","deltaCensusErr2","estFitErr2","selFitErr2","estSpaErr2","selSpaErr2"),"Scenario"=rep("S2",10),"PPI"=rep(PPI,10),rbind(deltaCensusErr1,estFitErr1,selFitErr1,estSpaErr1,selSpaErr1,deltaCensusErr2,estFitErr2,selFitErr2,estSpaErr2,selSpaErr2))
Results<-rbind(Results,Errors)

#S4-bADA
##Import files
setwd("D:/OneDrive - polymtl.ca/Studies/Civil engineering/POLY/Projet Doctorat/PSP/Exported/A1/3. Analysis/Montreal/S4-bADA")
#weights
weights<-fread("weights.csv")
names(weights)[-1]<-ADAnames
#synthetic population
hs<-fread("housing_synthetic.csv")
ps<-fread("person_synthetic.csv")
hs<-hs[,!grepl("rH",names(hs)),with=FALSE]
ps<-ps[,!grepl("rP",names(ps)),with=FALSE]
ps<-ps[,!grepl("PP_IND",names(ps)),with=FALSE]
#geo marginals
gmarginalshh<-fread("geo_household_marginals.csv")
gmarginalspp<-fread("geo_person_marginals.csv")
gmarginals<-cbind(gmarginalshh,gmarginalspp[,-1])
names(gmarginals)[-1]<-paste(names(gmarginals)[-1],".",gmarginals[1,-1],sep="")
gmarginals<-gmarginals[-1:-2,]
names(gmarginals)[grepl("99",names(gmarginals))]<-c("PP_IND.12","PP_JTWLEAVE.6","PP_JTWDUR.6", "PP_JTWMODE.7")
names(gmarginals)[grepl("ID",names(gmarginals))]<-c("HH_ID","PP_ID")
gmarginals[,1]<-list(ADAnames)
names(gmarginals)[1]<-"ADA"
gmarginals[,1]<-as.numeric(unlist(gmarginals[,1]))
temp<-gmarginals[,-1]
setcolorder(temp,match(varcat,names(temp)))
gmarginals<-data.table(cbind(gmarginals[,1],temp))
rm(gmarginalshh,gmarginalspp)
gmarginals<-gmarginals[,!grepl("PP_IND",names(gmarginals)),with=FALSE]
##Selection totals at CMA level
CMA_ST<-CMA_STf(hs,ps)
##Estimation totals at CMA level
CMA_ET<-CMA_ETf(weights)
###Estimation totals at DA level
weights<-fread("weights.csv")
weights<-cbind(c("ADA",ADAnames),data.table(t(weights)))
colnames(weights)<-unlist(weights[1,])
weights<-weights[-1,]
weights[,1]<-as.numeric(unlist(weights[,1]))
DAweights<-merge(distinct(GIS[,c("DA","ADA")]),weights,by.x="ADA",by.y="ADA")
DAweights<-merge(DAweights,gmarginals[,c("ADA","HH_ID")],by.x="ADA",by.y="ADA")
DAweights<-merge(DAweights,bDAmarginals[,c("DA","HH_ID")],by.x="DA",by.y="DA")
DAweights<-as.data.table(t(cbind(DAweights[,DA],DAweights[,c(-1,-2,-(ncol(DAweights)-1),-ncol(DAweights)),with=FALSE]*DAweights[,HH_ID.y/HH_ID.x])))
colnames(DAweights)<-as.character(unlist(DAweights[1,]))
DAweights<-DAweights[-1,]
DAweights<-cbind(PUMFhh[,1],DAweights)
wPUMFhh<-data.table(merge(PUMFhh,DAweights,by="hid"))
wPUMFpp<-data.table(merge(PUMFpp,DAweights,by="hid"))
DA_ET=copy(bDAmarginals)
for (i in 3:9){
  x<-names(wPUMFhh)[i]
  temp<-wPUMFhh[,lapply(.SD,sum),by=c(x),.SDcols=names(wPUMFhh)[10:ncol(wPUMFhh)]]
  temp<-temp[order(temp[,1])]
  temp[,1]<-paste(names(temp)[1],".",1:nrow(temp),sep="")
  temp<-as.data.table(t(temp))
  names(temp)<-unlist(temp[1,])
  temp<-temp[-1,]
  DA_ET[,c(names(DA_ET)[grepl(x,names(DA_ET))]):=temp,with=FALSE]
}
for (i in 4:10){
  x<-names(wPUMFpp)[i]
  temp<-wPUMFpp[,lapply(.SD,sum),by=c(x),.SDcols=names(wPUMFpp)[11:ncol(wPUMFpp)]]
  temp<-temp[order(temp[,1])]
  temp[,1]<-paste(names(temp)[1],".",1:nrow(temp),sep="")
  temp<-as.data.table(t(temp))
  names(temp)<-unlist(temp[1,])
  temp<-temp[-1,]
  DA_ET[,c(names(DA_ET)[grepl(x,names(DA_ET))]):=temp,with=FALSE]
}
DA_ET<-cbind("DA"=as.numeric(DAnames),DA_ET[,-1][,match(names(CMA_ET),names(DA_ET[,-1])),with=FALSE])
DA_ET<-data.table(apply(DA_ET,2,as.numeric))
#Selection totals at DA level
DA_ST<-apply(wPUMFhh[,10:ncol(wPUMFhh)],2,DA_STf)
DA_ST<-rbindlist(lapply(DA_ST, as.data.frame.list), fill=TRUE)
names(DA_ST)<-names(CMA_ST)
DA_ST<-cbind(DA_ET[,1],DA_ST)
DA_ST[is.na(DA_ST)]<-0
DA_ST<-data.table(apply(DA_ST,2,as.numeric))
#Population Proximity Index
PPI<-round(mean(gmarginals$PP_ID)*1000/bCMAmarginals$PP_ID,2)
#Census discrepancies
deltaCensus<-abs(colSums(gmarginals)[-1] - bCMAmarginals)
deltaCensusErr1<-round(IndicatorsMean(deltaCensus*1000/bCMAmarginals),2)
deltaCensusErr2<-round(IndicatorsSum(deltaCensus)*1000/rep(c(bCMAmarginals$HH_ID,bCMAmarginals$PP_ID,bCMAmarginals$HH_ID+bCMAmarginals$PP_ID),3),2)
#Estimation fitting errors
estFit<-abs(CMA_ET - bCMAmarginals)
estFitErr1<-round(IndicatorsMean(estFit*1000/bCMAmarginals),2)
estFitErr2<-round(IndicatorsSum(estFit)*1000/rep(c(bCMAmarginals$HH_ID,bCMAmarginals$PP_ID,bCMAmarginals$HH_ID+bCMAmarginals$PP_ID),3),2)
#Selection fitting errors
selFit<-abs(CMA_ST - bCMAmarginals)
selFitErr1<-round(IndicatorsMean(selFit*1000/bCMAmarginals),2)
selFitErr2<-round(IndicatorsSum(selFit)*1000/rep(c(bCMAmarginals$HH_ID,bCMAmarginals$PP_ID,bCMAmarginals$HH_ID+bCMAmarginals$PP_ID),3),2)
#Estimation spatialization errors
estSpa<-data.table(t(colSums(abs(DA_ET-bDAmarginals)[,-1])))
estSpaErr1<-round(IndicatorsMean(estSpa*1000/bCMAmarginals),2)
estSpaErr2<-round(IndicatorsSum(estSpa)*1000/rep(c(bCMAmarginals$HH_ID,bCMAmarginals$PP_ID,bCMAmarginals$HH_ID+bCMAmarginals$PP_ID),3),2)
#Selection spatialization errors
selSpa<-data.table(t(colSums(abs(DA_ST-bDAmarginals)[,-1])))
selSpaErr1<-round(IndicatorsMean(selSpa*1000/bCMAmarginals),2)
selSpaErr2<-round(IndicatorsSum(selSpa)*1000/rep(c(bCMAmarginals$HH_ID,bCMAmarginals$PP_ID,bCMAmarginals$HH_ID+bCMAmarginals$PP_ID),3),2)
#Results
Errors<-cbind("error"=c("deltaCensusErr1","estFitErr1","selFitErr1","estSpaErr1","selSpaErr1","deltaCensusErr2","estFitErr2","selFitErr2","estSpaErr2","selSpaErr2"),"Scenario"=rep("S4",10),"PPI"=rep(PPI,10),rbind(deltaCensusErr1,estFitErr1,selFitErr1,estSpaErr1,selSpaErr1,deltaCensusErr2,estFitErr2,selFitErr2,estSpaErr2,selSpaErr2))
Results<-rbind(Results,Errors)

#S6-bCT
##Import files
setwd("D:/OneDrive - polymtl.ca/Studies/Civil engineering/POLY/Projet Doctorat/PSP/Exported/A1/3. Analysis/Montreal/S6-bCT")
#weights
weights<-fread("weights.csv")
names(weights)[-1]<-CTnames
#synthetic population
hs<-fread("housing_synthetic.csv")
ps<-fread("person_synthetic.csv")
hs<-hs[,!grepl("rH",names(hs)),with=FALSE]
ps<-ps[,!grepl("rP",names(ps)),with=FALSE]
ps<-ps[,!grepl("PP_IND",names(ps)),with=FALSE]
#geo marginals
gmarginalshh<-fread("geo_household_marginals.csv")
gmarginalspp<-fread("geo_person_marginals.csv")
gmarginals<-cbind(gmarginalshh,gmarginalspp[,-1])
names(gmarginals)[-1]<-paste(names(gmarginals)[-1],".",gmarginals[1,-1],sep="")
gmarginals<-gmarginals[-1:-2,]
names(gmarginals)[grepl("99",names(gmarginals))]<-c("PP_IND.12","PP_JTWLEAVE.6","PP_JTWDUR.6", "PP_JTWMODE.7")
names(gmarginals)[grepl("ID",names(gmarginals))]<-c("HH_ID","PP_ID")
gmarginals[,1]<-list(CTnames)
names(gmarginals)[1]<-"CT"
gmarginals[,1]<-as.numeric(unlist(gmarginals[,1]))
temp<-gmarginals[,-1]
setcolorder(temp,match(varcat,names(temp)))
gmarginals<-data.table(cbind(gmarginals[,1],temp))
rm(gmarginalshh,gmarginalspp)
gmarginals<-gmarginals[,!grepl("PP_IND",names(gmarginals)),with=FALSE]
##Selection totals at CMA level
CMA_ST<-CMA_STf(hs,ps)
##Estimation totals at CMA level
CMA_ET<-CMA_ETf(weights)
###Estimation totals at DA level
weights<-fread("weights.csv")
weights<-cbind(c("CT",CTnames),data.table(t(weights)))
colnames(weights)<-unlist(weights[1,])
weights<-weights[-1,]
weights[,1]<-as.numeric(unlist(weights[,1]))
DAweights<-merge(distinct(GIS[,c("DA","CT")]),weights,by.x="CT",by.y="CT")
DAweights<-merge(DAweights,gmarginals[,c("CT","HH_ID")],by.x="CT",by.y="CT")
DAweights<-merge(DAweights,bDAmarginals[,c("DA","HH_ID")],by.x="DA",by.y="DA")
DAweights<-as.data.table(t(cbind(DAweights[,DA],DAweights[,c(-1,-2,-(ncol(DAweights)-1),-ncol(DAweights)),with=FALSE]*DAweights[,HH_ID.y/HH_ID.x])))
colnames(DAweights)<-as.character(unlist(DAweights[1,]))
DAweights<-DAweights[-1,]
DAweights<-cbind(PUMFhh[,1],DAweights)
wPUMFhh<-data.table(merge(PUMFhh,DAweights,by="hid"))
wPUMFpp<-data.table(merge(PUMFpp,DAweights,by="hid"))
DA_ET=copy(bDAmarginals)
for (i in 3:9){
  x<-names(wPUMFhh)[i]
  temp<-wPUMFhh[,lapply(.SD,sum),by=c(x),.SDcols=names(wPUMFhh)[10:ncol(wPUMFhh)]]
  temp<-temp[order(temp[,1])]
  temp[,1]<-paste(names(temp)[1],".",1:nrow(temp),sep="")
  temp<-as.data.table(t(temp))
  names(temp)<-unlist(temp[1,])
  temp<-temp[-1,]
  DA_ET[,c(names(DA_ET)[grepl(x,names(DA_ET))]):=temp,with=FALSE]
}
for (i in 4:10){
  x<-names(wPUMFpp)[i]
  temp<-wPUMFpp[,lapply(.SD,sum),by=c(x),.SDcols=names(wPUMFpp)[11:ncol(wPUMFpp)]]
  temp<-temp[order(temp[,1])]
  temp[,1]<-paste(names(temp)[1],".",1:nrow(temp),sep="")
  temp<-as.data.table(t(temp))
  names(temp)<-unlist(temp[1,])
  temp<-temp[-1,]
  DA_ET[,c(names(DA_ET)[grepl(x,names(DA_ET))]):=temp,with=FALSE]
}
DA_ET<-cbind("DA"=as.numeric(DAnames),DA_ET[,-1][,match(names(CMA_ET),names(DA_ET[,-1])),with=FALSE])
DA_ET<-data.table(apply(DA_ET,2,as.numeric))
#Selection totals at DA level
DA_ST<-apply(wPUMFhh[,10:ncol(wPUMFhh)],2,DA_STf)
DA_ST<-rbindlist(lapply(DA_ST, as.data.frame.list), fill=TRUE)
names(DA_ST)<-names(CMA_ST)
DA_ST<-cbind(DA_ET[,1],DA_ST)
DA_ST[is.na(DA_ST)]<-0
DA_ST<-data.table(apply(DA_ST,2,as.numeric))
#Population Proximity Index
PPI<-round(mean(gmarginals$PP_ID)*1000/bCMAmarginals$PP_ID,2)
#Census discrepancies
deltaCensus<-abs(colSums(gmarginals)[-1] - bCMAmarginals)
deltaCensusErr1<-round(IndicatorsMean(deltaCensus*1000/bCMAmarginals),2)
deltaCensusErr2<-round(IndicatorsSum(deltaCensus)*1000/rep(c(bCMAmarginals$HH_ID,bCMAmarginals$PP_ID,bCMAmarginals$HH_ID+bCMAmarginals$PP_ID),3),2)
#Estimation fitting errors
estFit<-abs(CMA_ET - bCMAmarginals)
estFitErr1<-round(IndicatorsMean(estFit*1000/bCMAmarginals),2)
estFitErr2<-round(IndicatorsSum(estFit)*1000/rep(c(bCMAmarginals$HH_ID,bCMAmarginals$PP_ID,bCMAmarginals$HH_ID+bCMAmarginals$PP_ID),3),2)
#Selection fitting errors
selFit<-abs(CMA_ST - bCMAmarginals)
selFitErr1<-round(IndicatorsMean(selFit*1000/bCMAmarginals),2)
selFitErr2<-round(IndicatorsSum(selFit)*1000/rep(c(bCMAmarginals$HH_ID,bCMAmarginals$PP_ID,bCMAmarginals$HH_ID+bCMAmarginals$PP_ID),3),2)
#Estimation spatialization errors
estSpa<-data.table(t(colSums(abs(DA_ET-bDAmarginals)[,-1])))
estSpaErr1<-round(IndicatorsMean(estSpa*1000/bCMAmarginals),2)
estSpaErr2<-round(IndicatorsSum(estSpa)*1000/rep(c(bCMAmarginals$HH_ID,bCMAmarginals$PP_ID,bCMAmarginals$HH_ID+bCMAmarginals$PP_ID),3),2)
#Selection spatialization errors
selSpa<-data.table(t(colSums(abs(DA_ST-bDAmarginals)[,-1])))
selSpaErr1<-round(IndicatorsMean(selSpa*1000/bCMAmarginals),2)
selSpaErr2<-round(IndicatorsSum(selSpa)*1000/rep(c(bCMAmarginals$HH_ID,bCMAmarginals$PP_ID,bCMAmarginals$HH_ID+bCMAmarginals$PP_ID),3),2)
#Results
Errors<-cbind("error"=c("deltaCensusErr1","estFitErr1","selFitErr1","estSpaErr1","selSpaErr1","deltaCensusErr2","estFitErr2","selFitErr2","estSpaErr2","selSpaErr2"),"Scenario"=rep("S6",10),"PPI"=rep(PPI,10),rbind(deltaCensusErr1,estFitErr1,selFitErr1,estSpaErr1,selSpaErr1,deltaCensusErr2,estFitErr2,selFitErr2,estSpaErr2,selSpaErr2))
Results<-rbind(Results,Errors)

#S8-bDA
##Import files
setwd("D:/OneDrive - polymtl.ca/Studies/Civil engineering/POLY/Projet Doctorat/PSP/Exported/A1/3. Analysis/Montreal/S8-bDA")
#weights
weights<-fread("weights.csv")
names(weights)[-1]<-DAnames
#synthetic population
hs<-fread("housing_synthetic.csv")
ps<-fread("person_synthetic.csv")
hs<-hs[,!grepl("rH",names(hs)),with=FALSE]
ps<-ps[,!grepl("rP",names(ps)),with=FALSE]
ps<-ps[,!grepl("PP_IND",names(ps)),with=FALSE]
#geo marginals
gmarginalshh<-fread("geo_household_marginals.csv")
gmarginalspp<-fread("geo_person_marginals.csv")
gmarginals<-cbind(gmarginalshh,gmarginalspp[,-1])
names(gmarginals)[-1]<-paste(names(gmarginals)[-1],".",gmarginals[1,-1],sep="")
gmarginals<-gmarginals[-1:-2,]
names(gmarginals)[grepl("99",names(gmarginals))]<-c("PP_IND.12","PP_JTWLEAVE.6","PP_JTWDUR.6", "PP_JTWMODE.7")
names(gmarginals)[grepl("ID",names(gmarginals))]<-c("HH_ID","PP_ID")
gmarginals[,1]<-list(DAnames)
names(gmarginals)[1]<-"DAr"
gmarginals[,1]<-as.numeric(unlist(gmarginals[,1]))
temp<-gmarginals[,-1]
setcolorder(temp,match(varcat,names(temp)))
gmarginals<-data.table(cbind(gmarginals[,1],temp))
rm(gmarginalshh,gmarginalspp)
gmarginals<-gmarginals[,!grepl("PP_IND",names(gmarginals)),with=FALSE]
##Selection totals at CMA level
CMA_ST<-CMA_STf(hs,ps)
##Estimation totals at CMA level
CMA_ET<-CMA_ETf(weights)
###Estimation totals at DA level
weights<-fread("weights.csv")
weights<-cbind(c("DAr",DAnames),data.table(t(weights)))
colnames(weights)<-unlist(weights[1,])
weights<-weights[-1,]
weights[,1]<-as.numeric(unlist(weights[,1]))
DAweights<-merge(distinct(GIS[,c("DA","DAr")]),weights,by.x="DAr",by.y="DAr")
DAweights<-merge(DAweights,gmarginals[,c("DAr","HH_ID")],by.x="DAr",by.y="DAr")
DAweights<-merge(DAweights,bDAmarginals[,c("DA","HH_ID")],by.x="DA",by.y="DA")
DAweights<-as.data.table(t(cbind(DAweights[,DA],DAweights[,c(-1,-2,-(ncol(DAweights)-1),-ncol(DAweights)),with=FALSE]*DAweights[,HH_ID.y/HH_ID.x])))
colnames(DAweights)<-as.character(unlist(DAweights[1,]))
DAweights<-DAweights[-1,]
DAweights<-cbind(PUMFhh[,1],DAweights)
wPUMFhh<-data.table(merge(PUMFhh,DAweights,by="hid"))
wPUMFpp<-data.table(merge(PUMFpp,DAweights,by="hid"))
DA_ET=copy(bDAmarginals)
for (i in 3:9){
  x<-names(wPUMFhh)[i]
  temp<-wPUMFhh[,lapply(.SD,sum),by=c(x),.SDcols=names(wPUMFhh)[10:ncol(wPUMFhh)]]
  temp<-temp[order(temp[,1])]
  temp[,1]<-paste(names(temp)[1],".",1:nrow(temp),sep="")
  temp<-as.data.table(t(temp))
  names(temp)<-unlist(temp[1,])
  temp<-temp[-1,]
  DA_ET[,c(names(DA_ET)[grepl(x,names(DA_ET))]):=temp,with=FALSE]
}
for (i in 4:10){
  x<-names(wPUMFpp)[i]
  temp<-wPUMFpp[,lapply(.SD,sum),by=c(x),.SDcols=names(wPUMFpp)[11:ncol(wPUMFpp)]]
  temp<-temp[order(temp[,1])]
  temp[,1]<-paste(names(temp)[1],".",1:nrow(temp),sep="")
  temp<-as.data.table(t(temp))
  names(temp)<-unlist(temp[1,])
  temp<-temp[-1,]
  DA_ET[,c(names(DA_ET)[grepl(x,names(DA_ET))]):=temp,with=FALSE]
}
DA_ET<-cbind("DA"=as.numeric(DAnames),DA_ET[,-1][,match(names(CMA_ET),names(DA_ET[,-1])),with=FALSE])
DA_ET<-data.table(apply(DA_ET,2,as.numeric))
#Selection totals at DA level
DA_ST<-apply(wPUMFhh[,10:ncol(wPUMFhh)],2,DA_STf)
DA_ST<-rbindlist(lapply(DA_ST, as.data.frame.list), fill=TRUE)
names(DA_ST)<-names(CMA_ST)
DA_ST<-cbind(DA_ET[,1],DA_ST)
DA_ST[is.na(DA_ST)]<-0
DA_ST<-data.table(apply(DA_ST,2,as.numeric))
#Population Proximity Index
PPI<-round(mean(gmarginals$PP_ID)*1000/bCMAmarginals$PP_ID,2)
#Census discrepancies
deltaCensus<-abs(colSums(gmarginals)[-1] - bCMAmarginals)
deltaCensusErr1<-round(IndicatorsMean(deltaCensus*1000/bCMAmarginals),2)
deltaCensusErr2<-round(IndicatorsSum(deltaCensus)*1000/rep(c(bCMAmarginals$HH_ID,bCMAmarginals$PP_ID,bCMAmarginals$HH_ID+bCMAmarginals$PP_ID),3),2)
#Estimation fitting errors
estFit<-abs(CMA_ET - bCMAmarginals)
estFitErr1<-round(IndicatorsMean(estFit*1000/bCMAmarginals),2)
estFitErr2<-round(IndicatorsSum(estFit)*1000/rep(c(bCMAmarginals$HH_ID,bCMAmarginals$PP_ID,bCMAmarginals$HH_ID+bCMAmarginals$PP_ID),3),2)
#Selection fitting errors
selFit<-abs(CMA_ST - bCMAmarginals)
selFitErr1<-round(IndicatorsMean(selFit*1000/bCMAmarginals),2)
selFitErr2<-round(IndicatorsSum(selFit)*1000/rep(c(bCMAmarginals$HH_ID,bCMAmarginals$PP_ID,bCMAmarginals$HH_ID+bCMAmarginals$PP_ID),3),2)
#Estimation spatialization errors
estSpa<-data.table(t(colSums(abs(DA_ET-bDAmarginals)[,-1])))
estSpaErr1<-round(IndicatorsMean(estSpa*1000/bCMAmarginals),2)
estSpaErr2<-round(IndicatorsSum(estSpa)*1000/rep(c(bCMAmarginals$HH_ID,bCMAmarginals$PP_ID,bCMAmarginals$HH_ID+bCMAmarginals$PP_ID),3),2)
#Selection spatialization errors
selSpa<-data.table(t(colSums(abs(DA_ST-bDAmarginals)[,-1])))
selSpaErr1<-round(IndicatorsMean(selSpa*1000/bCMAmarginals),2)
selSpaErr2<-round(IndicatorsSum(selSpa)*1000/rep(c(bCMAmarginals$HH_ID,bCMAmarginals$PP_ID,bCMAmarginals$HH_ID+bCMAmarginals$PP_ID),3),2)
#Results
Errors<-cbind("error"=c("deltaCensusErr1","estFitErr1","selFitErr1","estSpaErr1","selSpaErr1","deltaCensusErr2","estFitErr2","selFitErr2","estSpaErr2","selSpaErr2"),"Scenario"=rep("S8",10),"PPI"=rep(PPI,10),rbind(deltaCensusErr1,estFitErr1,selFitErr1,estSpaErr1,selSpaErr1,deltaCensusErr2,estFitErr2,selFitErr2,estSpaErr2,selSpaErr2))
Results<-rbind(Results,Errors)





#S1-bCMA
##Import files
setwd("D:/OneDrive - polymtl.ca/Studies/Civil engineering/POLY/Projet Doctorat/PSP/Exported/A1/3. Analysis/Montreal/S1-bCMA")
#weights
weights<-fread("weights.csv")
names(weights)[-1]<-CMAnames
#synthetic population
hs<-fread("housing_synthetic.csv")
ps<-fread("person_synthetic.csv")
hs<-hs[,!grepl("rH",names(hs)),with=FALSE]
ps<-ps[,!grepl("rP",names(ps)),with=FALSE]
ps<-ps[,!grepl("PP_IND",names(ps)),with=FALSE]
#geo marginals
gmarginalshh<-fread("geo_household_marginals.csv")
gmarginalspp<-fread("geo_person_marginals.csv")
gmarginals<-cbind(gmarginalshh,gmarginalspp[,-1])
names(gmarginals)[-1]<-paste(names(gmarginals)[-1],".",gmarginals[1,-1],sep="")
gmarginals<-gmarginals[-1:-2,]
names(gmarginals)[grepl("99",names(gmarginals))]<-c("PP_IND.12","PP_JTWLEAVE.6","PP_JTWDUR.6", "PP_JTWMODE.7")
names(gmarginals)[grepl("ID",names(gmarginals))]<-c("HH_ID","PP_ID")
gmarginals[,1]<-list(CMAnames)
names(gmarginals)[1]<-"CMA"
gmarginals[,1]<-as.numeric(unlist(gmarginals[,1]))
temp<-gmarginals[,-1]
setcolorder(temp,match(varcat,names(temp)))
gmarginals<-data.table(cbind(gmarginals[,1],temp))
rm(gmarginalshh,gmarginalspp)
gmarginals<-gmarginals[,!grepl("PP_IND",names(gmarginals)),with=FALSE]
##Selection totals at CMA level
CMA_ST<-CMA_STf(hs,ps)
##Estimation totals at CMA level
CMA_ET<-CMA_ETf(weights)
###Estimation totals at DA level
weights<-fread("weights.csv")
weights<-cbind(c("CMA",CMAnames),data.table(t(weights)))
colnames(weights)<-unlist(weights[1,])
weights<-weights[-1,]
weights[,1]<-as.numeric(unlist(weights[,1]))
DAweights<-merge(distinct(GIS[,c("DA","CMA")]),weights,by.x="CMA",by.y="CMA")
DAweights<-merge(DAweights,gmarginals[,c("CMA","HH_ID")],by.x="CMA",by.y="CMA")
DAweights<-merge(DAweights,bDAmarginals[,c("DA","HH_ID")],by.x="DA",by.y="DA")
DAweights<-as.data.table(t(cbind(DAweights[,DA],DAweights[,c(-1,-2,-(ncol(DAweights)-1),-ncol(DAweights)),with=FALSE]*DAweights[,HH_ID.y/HH_ID.x])))
colnames(DAweights)<-as.character(unlist(DAweights[1,]))
DAweights<-DAweights[-1,]
DAweights<-cbind(PUMFhh[,1],DAweights)
wPUMFhh<-data.table(merge(PUMFhh,DAweights,by="hid"))
wPUMFpp<-data.table(merge(PUMFpp,DAweights,by="hid"))
DA_ET=copy(bDAmarginals)
for (i in 3:9){
  x<-names(wPUMFhh)[i]
  temp<-wPUMFhh[,lapply(.SD,sum),by=c(x),.SDcols=names(wPUMFhh)[10:ncol(wPUMFhh)]]
  temp<-temp[order(temp[,1])]
  temp[,1]<-paste(names(temp)[1],".",1:nrow(temp),sep="")
  temp<-as.data.table(t(temp))
  names(temp)<-unlist(temp[1,])
  temp<-temp[-1,]
  DA_ET[,c(names(DA_ET)[grepl(x,names(DA_ET))]):=temp,with=FALSE]
}
for (i in 4:10){
  x<-names(wPUMFpp)[i]
  temp<-wPUMFpp[,lapply(.SD,sum),by=c(x),.SDcols=names(wPUMFpp)[11:ncol(wPUMFpp)]]
  temp<-temp[order(temp[,1])]
  temp[,1]<-paste(names(temp)[1],".",1:nrow(temp),sep="")
  temp<-as.data.table(t(temp))
  names(temp)<-unlist(temp[1,])
  temp<-temp[-1,]
  DA_ET[,c(names(DA_ET)[grepl(x,names(DA_ET))]):=temp,with=FALSE]
}
DA_ET<-cbind("DA"=as.numeric(DAnames),DA_ET[,-1][,match(names(CMA_ET),names(DA_ET[,-1])),with=FALSE])
DA_ET<-data.table(apply(DA_ET,2,as.numeric))
#Selection totals at DA level
DA_ST<-apply(wPUMFhh[,10:ncol(wPUMFhh)],2,DA_STf)
DA_ST<-rbindlist(lapply(DA_ST, as.data.frame.list), fill=TRUE)
names(DA_ST)<-names(CMA_ST)
DA_ST<-cbind(DA_ET[,1],DA_ST)
DA_ST[is.na(DA_ST)]<-0
DA_ST<-data.table(apply(DA_ST,2,as.numeric))
#Population Proximity Index
PPI<-round(mean(gmarginals$PP_ID)*1000/bCMAmarginals$PP_ID,2)
#Census discrepancies
deltaCensus<-abs(colSums(gmarginals)[-1] - bCMAmarginals)
deltaCensusErr1<-round(IndicatorsMean(deltaCensus*1000/bCMAmarginals),2)
deltaCensusErr2<-round(IndicatorsSum(deltaCensus)*1000/rep(c(bCMAmarginals$HH_ID,bCMAmarginals$PP_ID,bCMAmarginals$HH_ID+bCMAmarginals$PP_ID),3),2)
#Estimation fitting errors
estFit<-abs(CMA_ET - bCMAmarginals)
estFitErr1<-round(IndicatorsMean(estFit*1000/bCMAmarginals),2)
estFitErr2<-round(IndicatorsSum(estFit)*1000/rep(c(bCMAmarginals$HH_ID,bCMAmarginals$PP_ID,bCMAmarginals$HH_ID+bCMAmarginals$PP_ID),3),2)
#Selection fitting errors
selFit<-abs(CMA_ST - bCMAmarginals)
selFitErr1<-round(IndicatorsMean(selFit*1000/bCMAmarginals),2)
selFitErr2<-round(IndicatorsSum(selFit)*1000/rep(c(bCMAmarginals$HH_ID,bCMAmarginals$PP_ID,bCMAmarginals$HH_ID+bCMAmarginals$PP_ID),3),2)
#Estimation spatialization errors
estSpa<-data.table(t(colSums(abs(DA_ET-bDAmarginals)[,-1])))
estSpaErr1<-round(IndicatorsMean(estSpa*1000/bCMAmarginals),2)
estSpaErr2<-round(IndicatorsSum(estSpa)*1000/rep(c(bCMAmarginals$HH_ID,bCMAmarginals$PP_ID,bCMAmarginals$HH_ID+bCMAmarginals$PP_ID),3),2)
#Selection spatialization errors
selSpa<-data.table(t(colSums(abs(DA_ST-bDAmarginals)[,-1])))
selSpaErr1<-round(IndicatorsMean(selSpa*1000/bCMAmarginals),2)
selSpaErr2<-round(IndicatorsSum(selSpa)*1000/rep(c(bCMAmarginals$HH_ID,bCMAmarginals$PP_ID,bCMAmarginals$HH_ID+bCMAmarginals$PP_ID),3),2)
#Results
Errors<-cbind("error"=c("deltaCensusErr1","estFitErr1","selFitErr1","estSpaErr1","selSpaErr1","deltaCensusErr2","estFitErr2","selFitErr2","estSpaErr2","selSpaErr2"),"Scenario"=rep("S1",10),"PPI"=rep(PPI,10),rbind(deltaCensusErr1,estFitErr1,selFitErr1,estSpaErr1,selSpaErr1,deltaCensusErr2,estFitErr2,selFitErr2,estSpaErr2,selSpaErr2))
Results<-rbind(Results,Errors)

#S3-bCMA_CSD
##Import files
setwd("D:/OneDrive - polymtl.ca/Studies/Civil engineering/POLY/Projet Doctorat/PSP/Exported/A1/3. Analysis/Montreal/S3-bCMA_CSD")
#weights
weights<-fread("weights.csv")
names(weights)[-1]<-CSDnames
#synthetic population
hs<-fread("housing_synthetic.csv")
ps<-fread("person_synthetic.csv")
hs<-hs[,!grepl("rH",names(hs)),with=FALSE]
ps<-ps[,!grepl("rP",names(ps)),with=FALSE]
ps<-ps[,!grepl("PP_IND",names(ps)),with=FALSE]
#geo marginals
gmarginalshh<-fread("geo_household_marginals.csv")
gmarginalspp<-fread("geo_person_marginals.csv")
gmarginals<-cbind(gmarginalshh,gmarginalspp[,-1])
names(gmarginals)[-1]<-paste(names(gmarginals)[-1],".",gmarginals[1,-1],sep="")
gmarginals<-gmarginals[-1:-2,]
names(gmarginals)[grepl("99",names(gmarginals))]<-c("PP_IND.12","PP_JTWLEAVE.6","PP_JTWDUR.6", "PP_JTWMODE.7")
names(gmarginals)[grepl("ID",names(gmarginals))]<-c("HH_ID","PP_ID")
gmarginals[,1]<-list(CSDnames)
names(gmarginals)[1]<-"CSD"
gmarginals[,1]<-as.numeric(unlist(gmarginals[,1]))
temp<-gmarginals[,-1]
setcolorder(temp,match(varcat,names(temp)))
gmarginals<-data.table(cbind(gmarginals[,1],temp))
rm(gmarginalshh,gmarginalspp)
gmarginals<-gmarginals[,!grepl("PP_IND",names(gmarginals)),with=FALSE]
##Selection totals at CMA level
CMA_ST<-CMA_STf(hs,ps)
##Estimation totals at CMA level
CMA_ET<-CMA_ETf(weights)
###Estimation totals at DA level
weights<-fread("weights.csv")
weights<-cbind(c("CSD",CSDnames),data.table(t(weights)))
colnames(weights)<-unlist(weights[1,])
weights<-weights[-1,]
weights[,1]<-as.numeric(unlist(weights[,1]))
DAweights<-merge(distinct(GIS[,c("DA","CSD")]),weights,by.x="CSD",by.y="CSD")
DAweights<-merge(DAweights,gmarginals[,c("CSD","HH_ID")],by.x="CSD",by.y="CSD")
DAweights<-merge(DAweights,bDAmarginals[,c("DA","HH_ID")],by.x="DA",by.y="DA")
DAweights<-as.data.table(t(cbind(DAweights[,DA],DAweights[,c(-1,-2,-(ncol(DAweights)-1),-ncol(DAweights)),with=FALSE]*DAweights[,HH_ID.y/HH_ID.x])))
colnames(DAweights)<-as.character(unlist(DAweights[1,]))
DAweights<-DAweights[-1,]
DAweights<-cbind(PUMFhh[,1],DAweights)
wPUMFhh<-data.table(merge(PUMFhh,DAweights,by="hid"))
wPUMFpp<-data.table(merge(PUMFpp,DAweights,by="hid"))
DA_ET=copy(bDAmarginals)
for (i in 3:9){
  x<-names(wPUMFhh)[i]
  temp<-wPUMFhh[,lapply(.SD,sum),by=c(x),.SDcols=names(wPUMFhh)[10:ncol(wPUMFhh)]]
  temp<-temp[order(temp[,1])]
  temp[,1]<-paste(names(temp)[1],".",1:nrow(temp),sep="")
  temp<-as.data.table(t(temp))
  names(temp)<-unlist(temp[1,])
  temp<-temp[-1,]
  DA_ET[,c(names(DA_ET)[grepl(x,names(DA_ET))]):=temp,with=FALSE]
}
for (i in 4:10){
  x<-names(wPUMFpp)[i]
  temp<-wPUMFpp[,lapply(.SD,sum),by=c(x),.SDcols=names(wPUMFpp)[11:ncol(wPUMFpp)]]
  temp<-temp[order(temp[,1])]
  temp[,1]<-paste(names(temp)[1],".",1:nrow(temp),sep="")
  temp<-as.data.table(t(temp))
  names(temp)<-unlist(temp[1,])
  temp<-temp[-1,]
  DA_ET[,c(names(DA_ET)[grepl(x,names(DA_ET))]):=temp,with=FALSE]
}
DA_ET<-cbind("DA"=as.numeric(DAnames),DA_ET[,-1][,match(names(CMA_ET),names(DA_ET[,-1])),with=FALSE])
DA_ET<-data.table(apply(DA_ET,2,as.numeric))
#Selection totals at DA level
DA_ST<-apply(wPUMFhh[,10:ncol(wPUMFhh)],2,DA_STf)
DA_ST<-rbindlist(lapply(DA_ST, as.data.frame.list), fill=TRUE)
names(DA_ST)<-names(CMA_ST)
DA_ST<-cbind(DA_ET[,1],DA_ST)
DA_ST[is.na(DA_ST)]<-0
DA_ST<-data.table(apply(DA_ST,2,as.numeric))
#Population Proximity Index
PPI<-round(mean(gmarginals$PP_ID)*1000/bCMAmarginals$PP_ID,2)
#Census discrepancies
deltaCensus<-abs(colSums(gmarginals)[-1] - bCMAmarginals)
deltaCensusErr1<-round(IndicatorsMean(deltaCensus*1000/bCMAmarginals),2)
deltaCensusErr2<-round(IndicatorsSum(deltaCensus)*1000/rep(c(bCMAmarginals$HH_ID,bCMAmarginals$PP_ID,bCMAmarginals$HH_ID+bCMAmarginals$PP_ID),3),2)
#Estimation fitting errors
estFit<-abs(CMA_ET - bCMAmarginals)
estFitErr1<-round(IndicatorsMean(estFit*1000/bCMAmarginals),2)
estFitErr2<-round(IndicatorsSum(estFit)*1000/rep(c(bCMAmarginals$HH_ID,bCMAmarginals$PP_ID,bCMAmarginals$HH_ID+bCMAmarginals$PP_ID),3),2)
#Selection fitting errors
selFit<-abs(CMA_ST - bCMAmarginals)
selFitErr1<-round(IndicatorsMean(selFit*1000/bCMAmarginals),2)
selFitErr2<-round(IndicatorsSum(selFit)*1000/rep(c(bCMAmarginals$HH_ID,bCMAmarginals$PP_ID,bCMAmarginals$HH_ID+bCMAmarginals$PP_ID),3),2)
#Estimation spatialization errors
estSpa<-data.table(t(colSums(abs(DA_ET-bDAmarginals)[,-1])))
estSpaErr1<-round(IndicatorsMean(estSpa*1000/bCMAmarginals),2)
estSpaErr2<-round(IndicatorsSum(estSpa)*1000/rep(c(bCMAmarginals$HH_ID,bCMAmarginals$PP_ID,bCMAmarginals$HH_ID+bCMAmarginals$PP_ID),3),2)
#Selection spatialization errors
selSpa<-data.table(t(colSums(abs(DA_ST-bDAmarginals)[,-1])))
selSpaErr1<-round(IndicatorsMean(selSpa*1000/bCMAmarginals),2)
selSpaErr2<-round(IndicatorsSum(selSpa)*1000/rep(c(bCMAmarginals$HH_ID,bCMAmarginals$PP_ID,bCMAmarginals$HH_ID+bCMAmarginals$PP_ID),3),2)
#Results
Errors<-cbind("error"=c("deltaCensusErr1","estFitErr1","selFitErr1","estSpaErr1","selSpaErr1","deltaCensusErr2","estFitErr2","selFitErr2","estSpaErr2","selSpaErr2"),"Scenario"=rep("S3",10),"PPI"=rep(PPI,10),rbind(deltaCensusErr1,estFitErr1,selFitErr1,estSpaErr1,selSpaErr1,deltaCensusErr2,estFitErr2,selFitErr2,estSpaErr2,selSpaErr2))
Results<-rbind(Results,Errors)

#S5-bCMA_ADA
##Import files
setwd("D:/OneDrive - polymtl.ca/Studies/Civil engineering/POLY/Projet Doctorat/PSP/Exported/A1/3. Analysis/Montreal/S5-bCMA_ADA")
#weights
weights<-fread("weights.csv")
names(weights)[-1]<-ADAnames
#synthetic population
hs<-fread("housing_synthetic.csv")
ps<-fread("person_synthetic.csv")
hs<-hs[,!grepl("rH",names(hs)),with=FALSE]
ps<-ps[,!grepl("rP",names(ps)),with=FALSE]
ps<-ps[,!grepl("PP_IND",names(ps)),with=FALSE]
#geo marginals
gmarginalshh<-fread("geo_household_marginals.csv")
gmarginalspp<-fread("geo_person_marginals.csv")
gmarginals<-cbind(gmarginalshh,gmarginalspp[,-1])
names(gmarginals)[-1]<-paste(names(gmarginals)[-1],".",gmarginals[1,-1],sep="")
gmarginals<-gmarginals[-1:-2,]
names(gmarginals)[grepl("99",names(gmarginals))]<-c("PP_IND.12","PP_JTWLEAVE.6","PP_JTWDUR.6", "PP_JTWMODE.7")
names(gmarginals)[grepl("ID",names(gmarginals))]<-c("HH_ID","PP_ID")
gmarginals[,1]<-list(ADAnames)
names(gmarginals)[1]<-"ADA"
gmarginals[,1]<-as.numeric(unlist(gmarginals[,1]))
temp<-gmarginals[,-1]
setcolorder(temp,match(varcat,names(temp)))
gmarginals<-data.table(cbind(gmarginals[,1],temp))
rm(gmarginalshh,gmarginalspp)
gmarginals<-gmarginals[,!grepl("PP_IND",names(gmarginals)),with=FALSE]
##Selection totals at CMA level
CMA_ST<-CMA_STf(hs,ps)
##Estimation totals at CMA level
CMA_ET<-CMA_ETf(weights)
###Estimation totals at DA level
weights<-fread("weights.csv")
weights<-cbind(c("ADA",ADAnames),data.table(t(weights)))
colnames(weights)<-unlist(weights[1,])
weights<-weights[-1,]
weights[,1]<-as.numeric(unlist(weights[,1]))
DAweights<-merge(distinct(GIS[,c("DA","ADA")]),weights,by.x="ADA",by.y="ADA")
DAweights<-merge(DAweights,gmarginals[,c("ADA","HH_ID")],by.x="ADA",by.y="ADA")
DAweights<-merge(DAweights,bDAmarginals[,c("DA","HH_ID")],by.x="DA",by.y="DA")
DAweights<-as.data.table(t(cbind(DAweights[,DA],DAweights[,c(-1,-2,-(ncol(DAweights)-1),-ncol(DAweights)),with=FALSE]*DAweights[,HH_ID.y/HH_ID.x])))
colnames(DAweights)<-as.character(unlist(DAweights[1,]))
DAweights<-DAweights[-1,]
DAweights<-cbind(PUMFhh[,1],DAweights)
wPUMFhh<-data.table(merge(PUMFhh,DAweights,by="hid"))
wPUMFpp<-data.table(merge(PUMFpp,DAweights,by="hid"))
DA_ET=copy(bDAmarginals)
for (i in 3:9){
  x<-names(wPUMFhh)[i]
  temp<-wPUMFhh[,lapply(.SD,sum),by=c(x),.SDcols=names(wPUMFhh)[10:ncol(wPUMFhh)]]
  temp<-temp[order(temp[,1])]
  temp[,1]<-paste(names(temp)[1],".",1:nrow(temp),sep="")
  temp<-as.data.table(t(temp))
  names(temp)<-unlist(temp[1,])
  temp<-temp[-1,]
  DA_ET[,c(names(DA_ET)[grepl(x,names(DA_ET))]):=temp,with=FALSE]
}
for (i in 4:10){
  x<-names(wPUMFpp)[i]
  temp<-wPUMFpp[,lapply(.SD,sum),by=c(x),.SDcols=names(wPUMFpp)[11:ncol(wPUMFpp)]]
  temp<-temp[order(temp[,1])]
  temp[,1]<-paste(names(temp)[1],".",1:nrow(temp),sep="")
  temp<-as.data.table(t(temp))
  names(temp)<-unlist(temp[1,])
  temp<-temp[-1,]
  DA_ET[,c(names(DA_ET)[grepl(x,names(DA_ET))]):=temp,with=FALSE]
}
DA_ET<-cbind("DA"=as.numeric(DAnames),DA_ET[,-1][,match(names(CMA_ET),names(DA_ET[,-1])),with=FALSE])
DA_ET<-data.table(apply(DA_ET,2,as.numeric))
#Selection totals at DA level
DA_ST<-apply(wPUMFhh[,10:ncol(wPUMFhh)],2,DA_STf)
DA_ST<-rbindlist(lapply(DA_ST, as.data.frame.list), fill=TRUE)
names(DA_ST)<-names(CMA_ST)
DA_ST<-cbind(DA_ET[,1],DA_ST)
DA_ST[is.na(DA_ST)]<-0
DA_ST<-data.table(apply(DA_ST,2,as.numeric))
#Population Proximity Index
PPI<-round(mean(gmarginals$PP_ID)*1000/bCMAmarginals$PP_ID,2)
#Census discrepancies
deltaCensus<-abs(colSums(gmarginals)[-1] - bCMAmarginals)
deltaCensusErr1<-round(IndicatorsMean(deltaCensus*1000/bCMAmarginals),2)
deltaCensusErr2<-round(IndicatorsSum(deltaCensus)*1000/rep(c(bCMAmarginals$HH_ID,bCMAmarginals$PP_ID,bCMAmarginals$HH_ID+bCMAmarginals$PP_ID),3),2)
#Estimation fitting errors
estFit<-abs(CMA_ET - bCMAmarginals)
estFitErr1<-round(IndicatorsMean(estFit*1000/bCMAmarginals),2)
estFitErr2<-round(IndicatorsSum(estFit)*1000/rep(c(bCMAmarginals$HH_ID,bCMAmarginals$PP_ID,bCMAmarginals$HH_ID+bCMAmarginals$PP_ID),3),2)
#Selection fitting errors
selFit<-abs(CMA_ST - bCMAmarginals)
selFitErr1<-round(IndicatorsMean(selFit*1000/bCMAmarginals),2)
selFitErr2<-round(IndicatorsSum(selFit)*1000/rep(c(bCMAmarginals$HH_ID,bCMAmarginals$PP_ID,bCMAmarginals$HH_ID+bCMAmarginals$PP_ID),3),2)
#Estimation spatialization errors
estSpa<-data.table(t(colSums(abs(DA_ET-bDAmarginals)[,-1])))
estSpaErr1<-round(IndicatorsMean(estSpa*1000/bCMAmarginals),2)
estSpaErr2<-round(IndicatorsSum(estSpa)*1000/rep(c(bCMAmarginals$HH_ID,bCMAmarginals$PP_ID,bCMAmarginals$HH_ID+bCMAmarginals$PP_ID),3),2)
#Selection spatialization errors
selSpa<-data.table(t(colSums(abs(DA_ST-bDAmarginals)[,-1])))
selSpaErr1<-round(IndicatorsMean(selSpa*1000/bCMAmarginals),2)
selSpaErr2<-round(IndicatorsSum(selSpa)*1000/rep(c(bCMAmarginals$HH_ID,bCMAmarginals$PP_ID,bCMAmarginals$HH_ID+bCMAmarginals$PP_ID),3),2)
#Results
Errors<-cbind("error"=c("deltaCensusErr1","estFitErr1","selFitErr1","estSpaErr1","selSpaErr1","deltaCensusErr2","estFitErr2","selFitErr2","estSpaErr2","selSpaErr2"),"Scenario"=rep("S5",10),"PPI"=rep(PPI,10),rbind(deltaCensusErr1,estFitErr1,selFitErr1,estSpaErr1,selSpaErr1,deltaCensusErr2,estFitErr2,selFitErr2,estSpaErr2,selSpaErr2))
Results<-rbind(Results,Errors)

#S7-bCMA_CT
##Import files
setwd("D:/OneDrive - polymtl.ca/Studies/Civil engineering/POLY/Projet Doctorat/PSP/Exported/A1/3. Analysis/Montreal/S7-bCMA_CT")
#weights
weights<-fread("weights.csv")
names(weights)[-1]<-CTnames
#synthetic population
hs<-fread("housing_synthetic.csv")
ps<-fread("person_synthetic.csv")
hs<-hs[,!grepl("rH",names(hs)),with=FALSE]
ps<-ps[,!grepl("rP",names(ps)),with=FALSE]
ps<-ps[,!grepl("PP_IND",names(ps)),with=FALSE]
#geo marginals
gmarginalshh<-fread("geo_household_marginals.csv")
gmarginalspp<-fread("geo_person_marginals.csv")
gmarginals<-cbind(gmarginalshh,gmarginalspp[,-1])
names(gmarginals)[-1]<-paste(names(gmarginals)[-1],".",gmarginals[1,-1],sep="")
gmarginals<-gmarginals[-1:-2,]
names(gmarginals)[grepl("99",names(gmarginals))]<-c("PP_IND.12","PP_JTWLEAVE.6","PP_JTWDUR.6", "PP_JTWMODE.7")
names(gmarginals)[grepl("ID",names(gmarginals))]<-c("HH_ID","PP_ID")
gmarginals[,1]<-list(CTnames)
names(gmarginals)[1]<-"CT"
gmarginals[,1]<-as.numeric(unlist(gmarginals[,1]))
temp<-gmarginals[,-1]
setcolorder(temp,match(varcat,names(temp)))
gmarginals<-data.table(cbind(gmarginals[,1],temp))
rm(gmarginalshh,gmarginalspp)
gmarginals<-gmarginals[,!grepl("PP_IND",names(gmarginals)),with=FALSE]
##Selection totals at CMA level
CMA_ST<-CMA_STf(hs,ps)
##Estimation totals at CMA level
CMA_ET<-CMA_ETf(weights)
###Estimation totals at DA level
weights<-fread("weights.csv")
weights<-cbind(c("CT",CTnames),data.table(t(weights)))
colnames(weights)<-unlist(weights[1,])
weights<-weights[-1,]
weights[,1]<-as.numeric(unlist(weights[,1]))
DAweights<-merge(distinct(GIS[,c("DA","CT")]),weights,by.x="CT",by.y="CT")
DAweights<-merge(DAweights,gmarginals[,c("CT","HH_ID")],by.x="CT",by.y="CT")
DAweights<-merge(DAweights,bDAmarginals[,c("DA","HH_ID")],by.x="DA",by.y="DA")
DAweights<-as.data.table(t(cbind(DAweights[,DA],DAweights[,c(-1,-2,-(ncol(DAweights)-1),-ncol(DAweights)),with=FALSE]*DAweights[,HH_ID.y/HH_ID.x])))
colnames(DAweights)<-as.character(unlist(DAweights[1,]))
DAweights<-DAweights[-1,]
DAweights<-cbind(PUMFhh[,1],DAweights)
wPUMFhh<-data.table(merge(PUMFhh,DAweights,by="hid"))
wPUMFpp<-data.table(merge(PUMFpp,DAweights,by="hid"))
DA_ET=copy(bDAmarginals)
for (i in 3:9){
  x<-names(wPUMFhh)[i]
  temp<-wPUMFhh[,lapply(.SD,sum),by=c(x),.SDcols=names(wPUMFhh)[10:ncol(wPUMFhh)]]
  temp<-temp[order(temp[,1])]
  temp[,1]<-paste(names(temp)[1],".",1:nrow(temp),sep="")
  temp<-as.data.table(t(temp))
  names(temp)<-unlist(temp[1,])
  temp<-temp[-1,]
  DA_ET[,c(names(DA_ET)[grepl(x,names(DA_ET))]):=temp,with=FALSE]
}
for (i in 4:10){
  x<-names(wPUMFpp)[i]
  temp<-wPUMFpp[,lapply(.SD,sum),by=c(x),.SDcols=names(wPUMFpp)[11:ncol(wPUMFpp)]]
  temp<-temp[order(temp[,1])]
  temp[,1]<-paste(names(temp)[1],".",1:nrow(temp),sep="")
  temp<-as.data.table(t(temp))
  names(temp)<-unlist(temp[1,])
  temp<-temp[-1,]
  DA_ET[,c(names(DA_ET)[grepl(x,names(DA_ET))]):=temp,with=FALSE]
}
DA_ET<-cbind("DA"=as.numeric(DAnames),DA_ET[,-1][,match(names(CMA_ET),names(DA_ET[,-1])),with=FALSE])
DA_ET<-data.table(apply(DA_ET,2,as.numeric))
#Selection totals at DA level
DA_ST<-apply(wPUMFhh[,10:ncol(wPUMFhh)],2,DA_STf)
DA_ST<-rbindlist(lapply(DA_ST, as.data.frame.list), fill=TRUE)
names(DA_ST)<-names(CMA_ST)
DA_ST<-cbind(DA_ET[,1],DA_ST)
DA_ST[is.na(DA_ST)]<-0
DA_ST<-data.table(apply(DA_ST,2,as.numeric))
#Population Proximity Index
PPI<-round(mean(gmarginals$PP_ID)*1000/bCMAmarginals$PP_ID,2)
#Census discrepancies
deltaCensus<-abs(colSums(gmarginals)[-1] - bCMAmarginals)
deltaCensusErr1<-round(IndicatorsMean(deltaCensus*1000/bCMAmarginals),2)
deltaCensusErr2<-round(IndicatorsSum(deltaCensus)*1000/rep(c(bCMAmarginals$HH_ID,bCMAmarginals$PP_ID,bCMAmarginals$HH_ID+bCMAmarginals$PP_ID),3),2)
#Estimation fitting errors
estFit<-abs(CMA_ET - bCMAmarginals)
estFitErr1<-round(IndicatorsMean(estFit*1000/bCMAmarginals),2)
estFitErr2<-round(IndicatorsSum(estFit)*1000/rep(c(bCMAmarginals$HH_ID,bCMAmarginals$PP_ID,bCMAmarginals$HH_ID+bCMAmarginals$PP_ID),3),2)
#Selection fitting errors
selFit<-abs(CMA_ST - bCMAmarginals)
selFitErr1<-round(IndicatorsMean(selFit*1000/bCMAmarginals),2)
selFitErr2<-round(IndicatorsSum(selFit)*1000/rep(c(bCMAmarginals$HH_ID,bCMAmarginals$PP_ID,bCMAmarginals$HH_ID+bCMAmarginals$PP_ID),3),2)
#Estimation spatialization errors
estSpa<-data.table(t(colSums(abs(DA_ET-bDAmarginals)[,-1])))
estSpaErr1<-round(IndicatorsMean(estSpa*1000/bCMAmarginals),2)
estSpaErr2<-round(IndicatorsSum(estSpa)*1000/rep(c(bCMAmarginals$HH_ID,bCMAmarginals$PP_ID,bCMAmarginals$HH_ID+bCMAmarginals$PP_ID),3),2)
#Selection spatialization errors
selSpa<-data.table(t(colSums(abs(DA_ST-bDAmarginals)[,-1])))
selSpaErr1<-round(IndicatorsMean(selSpa*1000/bCMAmarginals),2)
selSpaErr2<-round(IndicatorsSum(selSpa)*1000/rep(c(bCMAmarginals$HH_ID,bCMAmarginals$PP_ID,bCMAmarginals$HH_ID+bCMAmarginals$PP_ID),3),2)
#Results
Errors<-cbind("error"=c("deltaCensusErr1","estFitErr1","selFitErr1","estSpaErr1","selSpaErr1","deltaCensusErr2","estFitErr2","selFitErr2","estSpaErr2","selSpaErr2"),"Scenario"=rep("S7",10),"PPI"=rep(PPI,10),rbind(deltaCensusErr1,estFitErr1,selFitErr1,estSpaErr1,selSpaErr1,deltaCensusErr2,estFitErr2,selFitErr2,estSpaErr2,selSpaErr2))
Results<-rbind(Results,Errors)

#S9-bCMA_DA
##Import files
setwd("D:/OneDrive - polymtl.ca/Studies/Civil engineering/POLY/Projet Doctorat/PSP/Exported/A1/3. Analysis/Montreal/S9-bCMA_DA")
#weights
weights<-fread("weights.csv")
names(weights)[-1]<-DAnames
#synthetic population
hs<-fread("housing_synthetic.csv")
ps<-fread("person_synthetic.csv")
hs<-hs[,!grepl("rH",names(hs)),with=FALSE]
ps<-ps[,!grepl("rP",names(ps)),with=FALSE]
ps<-ps[,!grepl("PP_IND",names(ps)),with=FALSE]
#geo marginals
gmarginalshh<-fread("geo_household_marginals.csv")
gmarginalspp<-fread("geo_person_marginals.csv")
gmarginals<-cbind(gmarginalshh,gmarginalspp[,-1])
names(gmarginals)[-1]<-paste(names(gmarginals)[-1],".",gmarginals[1,-1],sep="")
gmarginals<-gmarginals[-1:-2,]
names(gmarginals)[grepl("99",names(gmarginals))]<-c("PP_IND.12","PP_JTWLEAVE.6","PP_JTWDUR.6", "PP_JTWMODE.7")
names(gmarginals)[grepl("ID",names(gmarginals))]<-c("HH_ID","PP_ID")
gmarginals[,1]<-list(DAnames)
names(gmarginals)[1]<-"DAr"
gmarginals[,1]<-as.numeric(unlist(gmarginals[,1]))
temp<-gmarginals[,-1]
setcolorder(temp,match(varcat,names(temp)))
gmarginals<-data.table(cbind(gmarginals[,1],temp))
rm(gmarginalshh,gmarginalspp)
gmarginals<-gmarginals[,!grepl("PP_IND",names(gmarginals)),with=FALSE]
##Selection totals at CMA level
CMA_ST<-CMA_STf(hs,ps)
##Estimation totals at CMA level
CMA_ET<-CMA_ETf(weights)
###Estimation totals at DA level
weights<-fread("weights.csv")
weights<-cbind(c("DAr",DAnames),data.table(t(weights)))
colnames(weights)<-unlist(weights[1,])
weights<-weights[-1,]
weights[,1]<-as.numeric(unlist(weights[,1]))
DAweights<-merge(distinct(GIS[,c("DA","DAr")]),weights,by.x="DAr",by.y="DAr")
DAweights<-merge(DAweights,gmarginals[,c("DAr","HH_ID")],by.x="DAr",by.y="DAr")
DAweights<-merge(DAweights,bDAmarginals[,c("DA","HH_ID")],by.x="DA",by.y="DA")
DAweights<-as.data.table(t(cbind(DAweights[,DA],DAweights[,c(-1,-2,-(ncol(DAweights)-1),-ncol(DAweights)),with=FALSE]*DAweights[,HH_ID.y/HH_ID.x])))
colnames(DAweights)<-as.character(unlist(DAweights[1,]))
DAweights<-DAweights[-1,]
DAweights<-cbind(PUMFhh[,1],DAweights)
wPUMFhh<-data.table(merge(PUMFhh,DAweights,by="hid"))
wPUMFpp<-data.table(merge(PUMFpp,DAweights,by="hid"))
DA_ET=copy(bDAmarginals)
for (i in 3:9){
  x<-names(wPUMFhh)[i]
  temp<-wPUMFhh[,lapply(.SD,sum),by=c(x),.SDcols=names(wPUMFhh)[10:ncol(wPUMFhh)]]
  temp<-temp[order(temp[,1])]
  temp[,1]<-paste(names(temp)[1],".",1:nrow(temp),sep="")
  temp<-as.data.table(t(temp))
  names(temp)<-unlist(temp[1,])
  temp<-temp[-1,]
  DA_ET[,c(names(DA_ET)[grepl(x,names(DA_ET))]):=temp,with=FALSE]
}
for (i in 4:10){
  x<-names(wPUMFpp)[i]
  temp<-wPUMFpp[,lapply(.SD,sum),by=c(x),.SDcols=names(wPUMFpp)[11:ncol(wPUMFpp)]]
  temp<-temp[order(temp[,1])]
  temp[,1]<-paste(names(temp)[1],".",1:nrow(temp),sep="")
  temp<-as.data.table(t(temp))
  names(temp)<-unlist(temp[1,])
  temp<-temp[-1,]
  DA_ET[,c(names(DA_ET)[grepl(x,names(DA_ET))]):=temp,with=FALSE]
}
DA_ET<-cbind("DA"=as.numeric(DAnames),DA_ET[,-1][,match(names(CMA_ET),names(DA_ET[,-1])),with=FALSE])
DA_ET<-data.table(apply(DA_ET,2,as.numeric))
#Selection totals at DA level
DA_ST<-apply(wPUMFhh[,10:ncol(wPUMFhh)],2,DA_STf)
DA_ST<-rbindlist(lapply(DA_ST, as.data.frame.list), fill=TRUE)
names(DA_ST)<-names(CMA_ST)
DA_ST<-cbind(DA_ET[,1],DA_ST)
DA_ST[is.na(DA_ST)]<-0
DA_ST<-data.table(apply(DA_ST,2,as.numeric))
#Population Proximity Index
PPI<-round(mean(gmarginals$PP_ID)*1000/bCMAmarginals$PP_ID,2)
#Census discrepancies
deltaCensus<-abs(colSums(gmarginals)[-1] - bCMAmarginals)
deltaCensusErr1<-round(IndicatorsMean(deltaCensus*1000/bCMAmarginals),2)
deltaCensusErr2<-round(IndicatorsSum(deltaCensus)*1000/rep(c(bCMAmarginals$HH_ID,bCMAmarginals$PP_ID,bCMAmarginals$HH_ID+bCMAmarginals$PP_ID),3),2)
#Estimation fitting errors
estFit<-abs(CMA_ET - bCMAmarginals)
estFitErr1<-round(IndicatorsMean(estFit*1000/bCMAmarginals),2)
estFitErr2<-round(IndicatorsSum(estFit)*1000/rep(c(bCMAmarginals$HH_ID,bCMAmarginals$PP_ID,bCMAmarginals$HH_ID+bCMAmarginals$PP_ID),3),2)
#Selection fitting errors
selFit<-abs(CMA_ST - bCMAmarginals)
selFitErr1<-round(IndicatorsMean(selFit*1000/bCMAmarginals),2)
selFitErr2<-round(IndicatorsSum(selFit)*1000/rep(c(bCMAmarginals$HH_ID,bCMAmarginals$PP_ID,bCMAmarginals$HH_ID+bCMAmarginals$PP_ID),3),2)
#Estimation spatialization errors
estSpa<-data.table(t(colSums(abs(DA_ET-bDAmarginals)[,-1])))
estSpaErr1<-round(IndicatorsMean(estSpa*1000/bCMAmarginals),2)
estSpaErr2<-round(IndicatorsSum(estSpa)*1000/rep(c(bCMAmarginals$HH_ID,bCMAmarginals$PP_ID,bCMAmarginals$HH_ID+bCMAmarginals$PP_ID),3),2)
#Selection spatialization errors
selSpa<-data.table(t(colSums(abs(DA_ST-bDAmarginals)[,-1])))
selSpaErr1<-round(IndicatorsMean(selSpa*1000/bCMAmarginals),2)
selSpaErr2<-round(IndicatorsSum(selSpa)*1000/rep(c(bCMAmarginals$HH_ID,bCMAmarginals$PP_ID,bCMAmarginals$HH_ID+bCMAmarginals$PP_ID),3),2)
#Results
Errors<-cbind("error"=c("deltaCensusErr1","estFitErr1","selFitErr1","estSpaErr1","selSpaErr1","deltaCensusErr2","estFitErr2","selFitErr2","estSpaErr2","selSpaErr2"),"Scenario"=rep("S9",10),"PPI"=rep(PPI,10),rbind(deltaCensusErr1,estFitErr1,selFitErr1,estSpaErr1,selSpaErr1,deltaCensusErr2,estFitErr2,selFitErr2,estSpaErr2,selSpaErr2))
Results<-rbind(Results,Errors)





#S10-hCMA
##Import files
setwd("D:/OneDrive - polymtl.ca/Studies/Civil engineering/POLY/Projet Doctorat/PSP/Exported/A1/3. Analysis/Montreal/S10-hCMA")
#weights
weights<-fread("weights.csv")
names(weights)[-1]<-CMAnames
#synthetic population
hs<-fread("housing_synthetic.csv")
ps<-fread("person_synthetic.csv")
hs<-hs[,!grepl("rH",names(hs)),with=FALSE]
ps<-ps[,!grepl("rP",names(ps)),with=FALSE]
ps<-ps[,!grepl("PP_IND",names(ps)),with=FALSE]
#geo marginals
gmarginalshh<-fread("geo_household_marginals.csv")
gmarginalspp<-fread("geo_person_marginals.csv")
gmarginals<-cbind(gmarginalshh,gmarginalspp[,-1])
names(gmarginals)[-1]<-paste(names(gmarginals)[-1],".",gmarginals[1,-1],sep="")
gmarginals<-gmarginals[-1:-2,]
names(gmarginals)[grepl("99",names(gmarginals))]<-c("PP_IND.12","PP_JTWLEAVE.6","PP_JTWDUR.6", "PP_JTWMODE.7")
names(gmarginals)[grepl("ID",names(gmarginals))]<-c("HH_ID","PP_ID")
gmarginals[,1]<-list(CMAnames)
names(gmarginals)[1]<-"CMA"
gmarginals[,1]<-as.numeric(unlist(gmarginals[,1]))
temp<-gmarginals[,-1]
setcolorder(temp,match(varcat,names(temp)))
gmarginals<-data.table(cbind(gmarginals[,1],temp))
rm(gmarginalshh,gmarginalspp)
gmarginals<-gmarginals[,!grepl("PP_IND",names(gmarginals)),with=FALSE]
##Selection totals at CMA level
CMA_ST<-CMA_STf(hs,ps)
##Estimation totals at CMA level
CMA_ET<-CMA_ETf(weights)
###Estimation totals at DA level
weights<-fread("weights.csv")
weights<-cbind(c("CMA",CMAnames),data.table(t(weights)))
colnames(weights)<-unlist(weights[1,])
weights<-weights[-1,]
weights[,1]<-as.numeric(unlist(weights[,1]))
DAweights<-merge(distinct(GIS[,c("DA","CMA")]),weights,by.x="CMA",by.y="CMA")
DAweights<-merge(DAweights,gmarginals[,c("CMA","HH_ID")],by.x="CMA",by.y="CMA")
DAweights<-merge(DAweights,hDAmarginals[,c("DA","HH_ID")],by.x="DA",by.y="DA")
DAweights<-as.data.table(t(cbind(DAweights[,DA],DAweights[,c(-1,-2,-(ncol(DAweights)-1),-ncol(DAweights)),with=FALSE]*DAweights[,HH_ID.y/HH_ID.x])))
colnames(DAweights)<-as.character(unlist(DAweights[1,]))
DAweights<-DAweights[-1,]
DAweights<-cbind(PUMFhh[,1],DAweights)
wPUMFhh<-data.table(merge(PUMFhh,DAweights,by="hid"))
wPUMFpp<-data.table(merge(PUMFpp,DAweights,by="hid"))
DA_ET=copy(hDAmarginals)
for (i in 3:9){
  x<-names(wPUMFhh)[i]
  temp<-wPUMFhh[,lapply(.SD,sum),by=c(x),.SDcols=names(wPUMFhh)[10:ncol(wPUMFhh)]]
  temp<-temp[order(temp[,1])]
  temp[,1]<-paste(names(temp)[1],".",1:nrow(temp),sep="")
  temp<-as.data.table(t(temp))
  names(temp)<-unlist(temp[1,])
  temp<-temp[-1,]
  DA_ET[,c(names(DA_ET)[grepl(x,names(DA_ET))]):=temp,with=FALSE]
}
for (i in 4:10){
  x<-names(wPUMFpp)[i]
  temp<-wPUMFpp[,lapply(.SD,sum),by=c(x),.SDcols=names(wPUMFpp)[11:ncol(wPUMFpp)]]
  temp<-temp[order(temp[,1])]
  temp[,1]<-paste(names(temp)[1],".",1:nrow(temp),sep="")
  temp<-as.data.table(t(temp))
  names(temp)<-unlist(temp[1,])
  temp<-temp[-1,]
  DA_ET[,c(names(DA_ET)[grepl(x,names(DA_ET))]):=temp,with=FALSE]
}
DA_ET<-cbind("DA"=as.numeric(DAnames),DA_ET[,-1][,match(names(CMA_ET),names(DA_ET[,-1])),with=FALSE])
DA_ET<-data.table(apply(DA_ET,2,as.numeric))
#Selection totals at DA level
DA_ST<-apply(wPUMFhh[,10:ncol(wPUMFhh)],2,DA_STf)
DA_ST<-rbindlist(lapply(DA_ST, as.data.frame.list), fill=TRUE)
names(DA_ST)<-names(CMA_ST)
DA_ST<-cbind(DA_ET[,1],DA_ST)
DA_ST[is.na(DA_ST)]<-0
DA_ST<-data.table(apply(DA_ST,2,as.numeric))
#Population Proximity Index
PPI<-round(mean(gmarginals$PP_ID)*1000/hCMAmarginals$PP_ID,2)
#Census discrepancies
deltaCensus<-abs(colSums(gmarginals)[-1] - hCMAmarginals)
deltaCensusErr1<-round(IndicatorsMean(deltaCensus*1000/hCMAmarginals),2)
deltaCensusErr2<-round(IndicatorsSum(deltaCensus)*1000/rep(c(hCMAmarginals$HH_ID,hCMAmarginals$PP_ID,hCMAmarginals$HH_ID+hCMAmarginals$PP_ID),3),2)
#Estimation fitting errors
estFit<-abs(CMA_ET - hCMAmarginals)
estFitErr1<-round(IndicatorsMean(estFit*1000/hCMAmarginals),2)
estFitErr2<-round(IndicatorsSum(estFit)*1000/rep(c(hCMAmarginals$HH_ID,hCMAmarginals$PP_ID,hCMAmarginals$HH_ID+hCMAmarginals$PP_ID),3),2)
#Selection fitting errors
selFit<-abs(CMA_ST - hCMAmarginals)
selFitErr1<-round(IndicatorsMean(selFit*1000/hCMAmarginals),2)
selFitErr2<-round(IndicatorsSum(selFit)*1000/rep(c(hCMAmarginals$HH_ID,hCMAmarginals$PP_ID,hCMAmarginals$HH_ID+hCMAmarginals$PP_ID),3),2)
#Estimation spatialization errors
estSpa<-data.table(t(colSums(abs(DA_ET-hDAmarginals)[,-1])))
estSpaErr1<-round(IndicatorsMean(estSpa*1000/hCMAmarginals),2)
estSpaErr2<-round(IndicatorsSum(estSpa)*1000/rep(c(hCMAmarginals$HH_ID,hCMAmarginals$PP_ID,hCMAmarginals$HH_ID+hCMAmarginals$PP_ID),3),2)
#Selection spatialization errors
selSpa<-data.table(t(colSums(abs(DA_ST-hDAmarginals)[,-1])))
selSpaErr1<-round(IndicatorsMean(selSpa*1000/hCMAmarginals),2)
selSpaErr2<-round(IndicatorsSum(selSpa)*1000/rep(c(hCMAmarginals$HH_ID,hCMAmarginals$PP_ID,hCMAmarginals$HH_ID+hCMAmarginals$PP_ID),3),2)
#Results
Errors<-cbind("error"=c("deltaCensusErr1","estFitErr1","selFitErr1","estSpaErr1","selSpaErr1","deltaCensusErr2","estFitErr2","selFitErr2","estSpaErr2","selSpaErr2"),"Scenario"=rep("S10",10),"PPI"=rep(PPI,10),rbind(deltaCensusErr1,estFitErr1,selFitErr1,estSpaErr1,selSpaErr1,deltaCensusErr2,estFitErr2,selFitErr2,estSpaErr2,selSpaErr2))
Results<-rbind(Results,Errors)

#S11-hCSD
##Import files
setwd("D:/OneDrive - polymtl.ca/Studies/Civil engineering/POLY/Projet Doctorat/PSP/Exported/A1/3. Analysis/Montreal/S11-hCSD")
#weights
weights<-fread("weights.csv")
names(weights)[-1]<-CSDnames
#synthetic population
hs<-fread("housing_synthetic.csv")
ps<-fread("person_synthetic.csv")
hs<-hs[,!grepl("rH",names(hs)),with=FALSE]
ps<-ps[,!grepl("rP",names(ps)),with=FALSE]
ps<-ps[,!grepl("PP_IND",names(ps)),with=FALSE]
#geo marginals
gmarginalshh<-fread("geo_household_marginals.csv")
gmarginalspp<-fread("geo_person_marginals.csv")
gmarginals<-cbind(gmarginalshh,gmarginalspp[,-1])
names(gmarginals)[-1]<-paste(names(gmarginals)[-1],".",gmarginals[1,-1],sep="")
gmarginals<-gmarginals[-1:-2,]
names(gmarginals)[grepl("99",names(gmarginals))]<-c("PP_IND.12","PP_JTWLEAVE.6","PP_JTWDUR.6", "PP_JTWMODE.7")
names(gmarginals)[grepl("ID",names(gmarginals))]<-c("HH_ID","PP_ID")
gmarginals[,1]<-list(CSDnames)
names(gmarginals)[1]<-"CSD"
gmarginals[,1]<-as.numeric(unlist(gmarginals[,1]))
temp<-gmarginals[,-1]
setcolorder(temp,match(varcat,names(temp)))
gmarginals<-data.table(cbind(gmarginals[,1],temp))
rm(gmarginalshh,gmarginalspp)
gmarginals<-gmarginals[,!grepl("PP_IND",names(gmarginals)),with=FALSE]
##Selection totals at CMA level
CMA_ST<-CMA_STf(hs,ps)
##Estimation totals at CMA level
CMA_ET<-CMA_ETf(weights)
###Estimation totals at DA level
weights<-fread("weights.csv")
weights<-cbind(c("CSD",CSDnames),data.table(t(weights)))
colnames(weights)<-unlist(weights[1,])
weights<-weights[-1,]
weights[,1]<-as.numeric(unlist(weights[,1]))
DAweights<-merge(distinct(GIS[,c("DA","CSD")]),weights,by.x="CSD",by.y="CSD")
DAweights<-merge(DAweights,gmarginals[,c("CSD","HH_ID")],by.x="CSD",by.y="CSD")
DAweights<-merge(DAweights,hDAmarginals[,c("DA","HH_ID")],by.x="DA",by.y="DA")
DAweights<-as.data.table(t(cbind(DAweights[,DA],DAweights[,c(-1,-2,-(ncol(DAweights)-1),-ncol(DAweights)),with=FALSE]*DAweights[,HH_ID.y/HH_ID.x])))
colnames(DAweights)<-as.character(unlist(DAweights[1,]))
DAweights<-DAweights[-1,]
DAweights<-cbind(PUMFhh[,1],DAweights)
wPUMFhh<-data.table(merge(PUMFhh,DAweights,by="hid"))
wPUMFpp<-data.table(merge(PUMFpp,DAweights,by="hid"))
DA_ET=copy(hDAmarginals)
for (i in 3:9){
  x<-names(wPUMFhh)[i]
  temp<-wPUMFhh[,lapply(.SD,sum),by=c(x),.SDcols=names(wPUMFhh)[10:ncol(wPUMFhh)]]
  temp<-temp[order(temp[,1])]
  temp[,1]<-paste(names(temp)[1],".",1:nrow(temp),sep="")
  temp<-as.data.table(t(temp))
  names(temp)<-unlist(temp[1,])
  temp<-temp[-1,]
  DA_ET[,c(names(DA_ET)[grepl(x,names(DA_ET))]):=temp,with=FALSE]
}
for (i in 4:10){
  x<-names(wPUMFpp)[i]
  temp<-wPUMFpp[,lapply(.SD,sum),by=c(x),.SDcols=names(wPUMFpp)[11:ncol(wPUMFpp)]]
  temp<-temp[order(temp[,1])]
  temp[,1]<-paste(names(temp)[1],".",1:nrow(temp),sep="")
  temp<-as.data.table(t(temp))
  names(temp)<-unlist(temp[1,])
  temp<-temp[-1,]
  DA_ET[,c(names(DA_ET)[grepl(x,names(DA_ET))]):=temp,with=FALSE]
}
DA_ET<-cbind("DA"=as.numeric(DAnames),DA_ET[,-1][,match(names(CMA_ET),names(DA_ET[,-1])),with=FALSE])
DA_ET<-data.table(apply(DA_ET,2,as.numeric))
#Selection totals at DA level
DA_ST<-apply(wPUMFhh[,10:ncol(wPUMFhh)],2,DA_STf)
DA_ST<-rbindlist(lapply(DA_ST, as.data.frame.list), fill=TRUE)
names(DA_ST)<-names(CMA_ST)
DA_ST<-cbind(DA_ET[,1],DA_ST)
DA_ST[is.na(DA_ST)]<-0
DA_ST<-data.table(apply(DA_ST,2,as.numeric))
#Population Proximity Index
PPI<-round(mean(gmarginals$PP_ID)*1000/hCMAmarginals$PP_ID,2)
#Census discrepancies
deltaCensus<-abs(colSums(gmarginals)[-1] - hCMAmarginals)
deltaCensusErr1<-round(IndicatorsMean(deltaCensus*1000/hCMAmarginals),2)
deltaCensusErr2<-round(IndicatorsSum(deltaCensus)*1000/rep(c(hCMAmarginals$HH_ID,hCMAmarginals$PP_ID,hCMAmarginals$HH_ID+hCMAmarginals$PP_ID),3),2)
#Estimation fitting errors
estFit<-abs(CMA_ET - hCMAmarginals)
estFitErr1<-round(IndicatorsMean(estFit*1000/hCMAmarginals),2)
estFitErr2<-round(IndicatorsSum(estFit)*1000/rep(c(hCMAmarginals$HH_ID,hCMAmarginals$PP_ID,hCMAmarginals$HH_ID+hCMAmarginals$PP_ID),3),2)
#Selection fitting errors
selFit<-abs(CMA_ST - hCMAmarginals)
selFitErr1<-round(IndicatorsMean(selFit*1000/hCMAmarginals),2)
selFitErr2<-round(IndicatorsSum(selFit)*1000/rep(c(hCMAmarginals$HH_ID,hCMAmarginals$PP_ID,hCMAmarginals$HH_ID+hCMAmarginals$PP_ID),3),2)
#Estimation spatialization errors
estSpa<-data.table(t(colSums(abs(DA_ET-hDAmarginals)[,-1])))
estSpaErr1<-round(IndicatorsMean(estSpa*1000/hCMAmarginals),2)
estSpaErr2<-round(IndicatorsSum(estSpa)*1000/rep(c(hCMAmarginals$HH_ID,hCMAmarginals$PP_ID,hCMAmarginals$HH_ID+hCMAmarginals$PP_ID),3),2)
#Selection spatialization errors
selSpa<-data.table(t(colSums(abs(DA_ST-hDAmarginals)[,-1])))
selSpaErr1<-round(IndicatorsMean(selSpa*1000/hCMAmarginals),2)
selSpaErr2<-round(IndicatorsSum(selSpa)*1000/rep(c(hCMAmarginals$HH_ID,hCMAmarginals$PP_ID,hCMAmarginals$HH_ID+hCMAmarginals$PP_ID),3),2)
#Results
Errors<-cbind("error"=c("deltaCensusErr1","estFitErr1","selFitErr1","estSpaErr1","selSpaErr1","deltaCensusErr2","estFitErr2","selFitErr2","estSpaErr2","selSpaErr2"),"Scenario"=rep("S11",10),"PPI"=rep(PPI,10),rbind(deltaCensusErr1,estFitErr1,selFitErr1,estSpaErr1,selSpaErr1,deltaCensusErr2,estFitErr2,selFitErr2,estSpaErr2,selSpaErr2))
Results<-rbind(Results,Errors)

#S13-hADA
##Import files
setwd("D:/OneDrive - polymtl.ca/Studies/Civil engineering/POLY/Projet Doctorat/PSP/Exported/A1/3. Analysis/Montreal/S13-hADA")
#weights
weights<-fread("weights.csv")
names(weights)[-1]<-ADAnames
#synthetic population
hs<-fread("housing_synthetic.csv")
ps<-fread("person_synthetic.csv")
hs<-hs[,!grepl("rH",names(hs)),with=FALSE]
ps<-ps[,!grepl("rP",names(ps)),with=FALSE]
ps<-ps[,!grepl("PP_IND",names(ps)),with=FALSE]
#geo marginals
gmarginalshh<-fread("geo_household_marginals.csv")
gmarginalspp<-fread("geo_person_marginals.csv")
gmarginals<-cbind(gmarginalshh,gmarginalspp[,-1])
names(gmarginals)[-1]<-paste(names(gmarginals)[-1],".",gmarginals[1,-1],sep="")
gmarginals<-gmarginals[-1:-2,]
names(gmarginals)[grepl("99",names(gmarginals))]<-c("PP_IND.12","PP_JTWLEAVE.6","PP_JTWDUR.6", "PP_JTWMODE.7")
names(gmarginals)[grepl("ID",names(gmarginals))]<-c("HH_ID","PP_ID")
gmarginals[,1]<-list(ADAnames)
names(gmarginals)[1]<-"ADA"
gmarginals[,1]<-as.numeric(unlist(gmarginals[,1]))
temp<-gmarginals[,-1]
setcolorder(temp,match(varcat,names(temp)))
gmarginals<-data.table(cbind(gmarginals[,1],temp))
rm(gmarginalshh,gmarginalspp)
gmarginals<-gmarginals[,!grepl("PP_IND",names(gmarginals)),with=FALSE]
##Selection totals at CMA level
CMA_ST<-CMA_STf(hs,ps)
##Estimation totals at CMA level
CMA_ET<-CMA_ETf(weights)
###Estimation totals at DA level
weights<-fread("weights.csv")
weights<-cbind(c("ADA",ADAnames),data.table(t(weights)))
colnames(weights)<-unlist(weights[1,])
weights<-weights[-1,]
weights[,1]<-as.numeric(unlist(weights[,1]))
DAweights<-merge(distinct(GIS[,c("DA","ADA")]),weights,by.x="ADA",by.y="ADA")
DAweights<-merge(DAweights,gmarginals[,c("ADA","HH_ID")],by.x="ADA",by.y="ADA")
DAweights<-merge(DAweights,hDAmarginals[,c("DA","HH_ID")],by.x="DA",by.y="DA")
DAweights<-as.data.table(t(cbind(DAweights[,DA],DAweights[,c(-1,-2,-(ncol(DAweights)-1),-ncol(DAweights)),with=FALSE]*DAweights[,HH_ID.y/HH_ID.x])))
colnames(DAweights)<-as.character(unlist(DAweights[1,]))
DAweights<-DAweights[-1,]
DAweights<-cbind(PUMFhh[,1],DAweights)
wPUMFhh<-data.table(merge(PUMFhh,DAweights,by="hid"))
wPUMFpp<-data.table(merge(PUMFpp,DAweights,by="hid"))
DA_ET=copy(hDAmarginals)
for (i in 3:9){
  x<-names(wPUMFhh)[i]
  temp<-wPUMFhh[,lapply(.SD,sum),by=c(x),.SDcols=names(wPUMFhh)[10:ncol(wPUMFhh)]]
  temp<-temp[order(temp[,1])]
  temp[,1]<-paste(names(temp)[1],".",1:nrow(temp),sep="")
  temp<-as.data.table(t(temp))
  names(temp)<-unlist(temp[1,])
  temp<-temp[-1,]
  DA_ET[,c(names(DA_ET)[grepl(x,names(DA_ET))]):=temp,with=FALSE]
}
for (i in 4:10){
  x<-names(wPUMFpp)[i]
  temp<-wPUMFpp[,lapply(.SD,sum),by=c(x),.SDcols=names(wPUMFpp)[11:ncol(wPUMFpp)]]
  temp<-temp[order(temp[,1])]
  temp[,1]<-paste(names(temp)[1],".",1:nrow(temp),sep="")
  temp<-as.data.table(t(temp))
  names(temp)<-unlist(temp[1,])
  temp<-temp[-1,]
  DA_ET[,c(names(DA_ET)[grepl(x,names(DA_ET))]):=temp,with=FALSE]
}
DA_ET<-cbind("DA"=as.numeric(DAnames),DA_ET[,-1][,match(names(CMA_ET),names(DA_ET[,-1])),with=FALSE])
DA_ET<-data.table(apply(DA_ET,2,as.numeric))
#Selection totals at DA level
DA_ST<-apply(wPUMFhh[,10:ncol(wPUMFhh)],2,DA_STf)
DA_ST<-rbindlist(lapply(DA_ST, as.data.frame.list), fill=TRUE)
names(DA_ST)<-names(CMA_ST)
DA_ST<-cbind(DA_ET[,1],DA_ST)
DA_ST[is.na(DA_ST)]<-0
DA_ST<-data.table(apply(DA_ST,2,as.numeric))
#Population Proximity Index
PPI<-round(mean(gmarginals$PP_ID)*1000/hCMAmarginals$PP_ID,2)
#Census discrepancies
deltaCensus<-abs(colSums(gmarginals)[-1] - hCMAmarginals)
deltaCensusErr1<-round(IndicatorsMean(deltaCensus*1000/hCMAmarginals),2)
deltaCensusErr2<-round(IndicatorsSum(deltaCensus)*1000/rep(c(hCMAmarginals$HH_ID,hCMAmarginals$PP_ID,hCMAmarginals$HH_ID+hCMAmarginals$PP_ID),3),2)
#Estimation fitting errors
estFit<-abs(CMA_ET - hCMAmarginals)
estFitErr1<-round(IndicatorsMean(estFit*1000/hCMAmarginals),2)
estFitErr2<-round(IndicatorsSum(estFit)*1000/rep(c(hCMAmarginals$HH_ID,hCMAmarginals$PP_ID,hCMAmarginals$HH_ID+hCMAmarginals$PP_ID),3),2)
#Selection fitting errors
selFit<-abs(CMA_ST - hCMAmarginals)
selFitErr1<-round(IndicatorsMean(selFit*1000/hCMAmarginals),2)
selFitErr2<-round(IndicatorsSum(selFit)*1000/rep(c(hCMAmarginals$HH_ID,hCMAmarginals$PP_ID,hCMAmarginals$HH_ID+hCMAmarginals$PP_ID),3),2)
#Estimation spatialization errors
estSpa<-data.table(t(colSums(abs(DA_ET-hDAmarginals)[,-1])))
estSpaErr1<-round(IndicatorsMean(estSpa*1000/hCMAmarginals),2)
estSpaErr2<-round(IndicatorsSum(estSpa)*1000/rep(c(hCMAmarginals$HH_ID,hCMAmarginals$PP_ID,hCMAmarginals$HH_ID+hCMAmarginals$PP_ID),3),2)
#Selection spatialization errors
selSpa<-data.table(t(colSums(abs(DA_ST-hDAmarginals)[,-1])))
selSpaErr1<-round(IndicatorsMean(selSpa*1000/hCMAmarginals),2)
selSpaErr2<-round(IndicatorsSum(selSpa)*1000/rep(c(hCMAmarginals$HH_ID,hCMAmarginals$PP_ID,hCMAmarginals$HH_ID+hCMAmarginals$PP_ID),3),2)
#Results
Errors<-cbind("error"=c("deltaCensusErr1","estFitErr1","selFitErr1","estSpaErr1","selSpaErr1","deltaCensusErr2","estFitErr2","selFitErr2","estSpaErr2","selSpaErr2"),"Scenario"=rep("S13",10),"PPI"=rep(PPI,10),rbind(deltaCensusErr1,estFitErr1,selFitErr1,estSpaErr1,selSpaErr1,deltaCensusErr2,estFitErr2,selFitErr2,estSpaErr2,selSpaErr2))
Results<-rbind(Results,Errors)

#S15-hCT
##Import files
setwd("D:/OneDrive - polymtl.ca/Studies/Civil engineering/POLY/Projet Doctorat/PSP/Exported/A1/3. Analysis/Montreal/S15-hCT")
#weights
weights<-fread("weights.csv")
names(weights)[-1]<-CTnames
#synthetic population
hs<-fread("housing_synthetic.csv")
ps<-fread("person_synthetic.csv")
hs<-hs[,!grepl("rH",names(hs)),with=FALSE]
ps<-ps[,!grepl("rP",names(ps)),with=FALSE]
ps<-ps[,!grepl("PP_IND",names(ps)),with=FALSE]
#geo marginals
gmarginalshh<-fread("geo_household_marginals.csv")
gmarginalspp<-fread("geo_person_marginals.csv")
gmarginals<-cbind(gmarginalshh,gmarginalspp[,-1])
names(gmarginals)[-1]<-paste(names(gmarginals)[-1],".",gmarginals[1,-1],sep="")
gmarginals<-gmarginals[-1:-2,]
names(gmarginals)[grepl("99",names(gmarginals))]<-c("PP_IND.12","PP_JTWLEAVE.6","PP_JTWDUR.6", "PP_JTWMODE.7")
names(gmarginals)[grepl("ID",names(gmarginals))]<-c("HH_ID","PP_ID")
gmarginals[,1]<-list(CTnames)
names(gmarginals)[1]<-"CT"
gmarginals[,1]<-as.numeric(unlist(gmarginals[,1]))
temp<-gmarginals[,-1]
setcolorder(temp,match(varcat,names(temp)))
gmarginals<-data.table(cbind(gmarginals[,1],temp))
rm(gmarginalshh,gmarginalspp)
gmarginals<-gmarginals[,!grepl("PP_IND",names(gmarginals)),with=FALSE]
##Selection totals at CMA level
CMA_ST<-CMA_STf(hs,ps)
##Estimation totals at CMA level
CMA_ET<-CMA_ETf(weights)
###Estimation totals at DA level
weights<-fread("weights.csv")
weights<-cbind(c("CT",CTnames),data.table(t(weights)))
colnames(weights)<-unlist(weights[1,])
weights<-weights[-1,]
weights[,1]<-as.numeric(unlist(weights[,1]))
DAweights<-merge(distinct(GIS[,c("DA","CT")]),weights,by.x="CT",by.y="CT")
DAweights<-merge(DAweights,gmarginals[,c("CT","HH_ID")],by.x="CT",by.y="CT")
DAweights<-merge(DAweights,hDAmarginals[,c("DA","HH_ID")],by.x="DA",by.y="DA")
DAweights<-as.data.table(t(cbind(DAweights[,DA],DAweights[,c(-1,-2,-(ncol(DAweights)-1),-ncol(DAweights)),with=FALSE]*DAweights[,HH_ID.y/HH_ID.x])))
colnames(DAweights)<-as.character(unlist(DAweights[1,]))
DAweights<-DAweights[-1,]
DAweights<-cbind(PUMFhh[,1],DAweights)
wPUMFhh<-data.table(merge(PUMFhh,DAweights,by="hid"))
wPUMFpp<-data.table(merge(PUMFpp,DAweights,by="hid"))
DA_ET=copy(hDAmarginals)
for (i in 3:9){
  x<-names(wPUMFhh)[i]
  temp<-wPUMFhh[,lapply(.SD,sum),by=c(x),.SDcols=names(wPUMFhh)[10:ncol(wPUMFhh)]]
  temp<-temp[order(temp[,1])]
  temp[,1]<-paste(names(temp)[1],".",1:nrow(temp),sep="")
  temp<-as.data.table(t(temp))
  names(temp)<-unlist(temp[1,])
  temp<-temp[-1,]
  DA_ET[,c(names(DA_ET)[grepl(x,names(DA_ET))]):=temp,with=FALSE]
}
for (i in 4:10){
  x<-names(wPUMFpp)[i]
  temp<-wPUMFpp[,lapply(.SD,sum),by=c(x),.SDcols=names(wPUMFpp)[11:ncol(wPUMFpp)]]
  temp<-temp[order(temp[,1])]
  temp[,1]<-paste(names(temp)[1],".",1:nrow(temp),sep="")
  temp<-as.data.table(t(temp))
  names(temp)<-unlist(temp[1,])
  temp<-temp[-1,]
  DA_ET[,c(names(DA_ET)[grepl(x,names(DA_ET))]):=temp,with=FALSE]
}
DA_ET<-cbind("DA"=as.numeric(DAnames),DA_ET[,-1][,match(names(CMA_ET),names(DA_ET[,-1])),with=FALSE])
DA_ET<-data.table(apply(DA_ET,2,as.numeric))
#Selection totals at DA level
DA_ST<-apply(wPUMFhh[,10:ncol(wPUMFhh)],2,DA_STf)
DA_ST<-rbindlist(lapply(DA_ST, as.data.frame.list), fill=TRUE)
names(DA_ST)<-names(CMA_ST)
DA_ST<-cbind(DA_ET[,1],DA_ST)
DA_ST[is.na(DA_ST)]<-0
DA_ST<-data.table(apply(DA_ST,2,as.numeric))
#Population Proximity Index
PPI<-round(mean(gmarginals$PP_ID)*1000/hCMAmarginals$PP_ID,2)
#Census discrepancies
deltaCensus<-abs(colSums(gmarginals)[-1] - hCMAmarginals)
deltaCensusErr1<-round(IndicatorsMean(deltaCensus*1000/hCMAmarginals),2)
deltaCensusErr2<-round(IndicatorsSum(deltaCensus)*1000/rep(c(hCMAmarginals$HH_ID,hCMAmarginals$PP_ID,hCMAmarginals$HH_ID+hCMAmarginals$PP_ID),3),2)
#Estimation fitting errors
estFit<-abs(CMA_ET - hCMAmarginals)
estFitErr1<-round(IndicatorsMean(estFit*1000/hCMAmarginals),2)
estFitErr2<-round(IndicatorsSum(estFit)*1000/rep(c(hCMAmarginals$HH_ID,hCMAmarginals$PP_ID,hCMAmarginals$HH_ID+hCMAmarginals$PP_ID),3),2)
#Selection fitting errors
selFit<-abs(CMA_ST - hCMAmarginals)
selFitErr1<-round(IndicatorsMean(selFit*1000/hCMAmarginals),2)
selFitErr2<-round(IndicatorsSum(selFit)*1000/rep(c(hCMAmarginals$HH_ID,hCMAmarginals$PP_ID,hCMAmarginals$HH_ID+hCMAmarginals$PP_ID),3),2)
#Estimation spatialization errors
estSpa<-data.table(t(colSums(abs(DA_ET-hDAmarginals)[,-1])))
estSpaErr1<-round(IndicatorsMean(estSpa*1000/hCMAmarginals),2)
estSpaErr2<-round(IndicatorsSum(estSpa)*1000/rep(c(hCMAmarginals$HH_ID,hCMAmarginals$PP_ID,hCMAmarginals$HH_ID+hCMAmarginals$PP_ID),3),2)
#Selection spatialization errors
selSpa<-data.table(t(colSums(abs(DA_ST-hDAmarginals)[,-1])))
selSpaErr1<-round(IndicatorsMean(selSpa*1000/hCMAmarginals),2)
selSpaErr2<-round(IndicatorsSum(selSpa)*1000/rep(c(hCMAmarginals$HH_ID,hCMAmarginals$PP_ID,hCMAmarginals$HH_ID+hCMAmarginals$PP_ID),3),2)
#Results
Errors<-cbind("error"=c("deltaCensusErr1","estFitErr1","selFitErr1","estSpaErr1","selSpaErr1","deltaCensusErr2","estFitErr2","selFitErr2","estSpaErr2","selSpaErr2"),"Scenario"=rep("S15",10),"PPI"=rep(PPI,10),rbind(deltaCensusErr1,estFitErr1,selFitErr1,estSpaErr1,selSpaErr1,deltaCensusErr2,estFitErr2,selFitErr2,estSpaErr2,selSpaErr2))
Results<-rbind(Results,Errors)

#S17-hDA
##Import files
setwd("D:/OneDrive - polymtl.ca/Studies/Civil engineering/POLY/Projet Doctorat/PSP/Exported/A1/3. Analysis/Montreal/S17-hDA")
#weights
weights<-fread("weights.csv")
names(weights)[-1]<-DAnames
#synthetic population
hs<-fread("housing_synthetic.csv")
ps<-fread("person_synthetic.csv")
hs<-hs[,!grepl("rH",names(hs)),with=FALSE]
ps<-ps[,!grepl("rP",names(ps)),with=FALSE]
ps<-ps[,!grepl("PP_IND",names(ps)),with=FALSE]
#geo marginals
gmarginalshh<-fread("geo_household_marginals.csv")
gmarginalspp<-fread("geo_person_marginals.csv")
gmarginals<-cbind(gmarginalshh,gmarginalspp[,-1])
names(gmarginals)[-1]<-paste(names(gmarginals)[-1],".",gmarginals[1,-1],sep="")
gmarginals<-gmarginals[-1:-2,]
names(gmarginals)[grepl("99",names(gmarginals))]<-c("PP_IND.12","PP_JTWLEAVE.6","PP_JTWDUR.6", "PP_JTWMODE.7")
names(gmarginals)[grepl("ID",names(gmarginals))]<-c("HH_ID","PP_ID")
gmarginals[,1]<-list(DAnames)
names(gmarginals)[1]<-"DAr"
gmarginals[,1]<-as.numeric(unlist(gmarginals[,1]))
temp<-gmarginals[,-1]
setcolorder(temp,match(varcat,names(temp)))
gmarginals<-data.table(cbind(gmarginals[,1],temp))
rm(gmarginalshh,gmarginalspp)
gmarginals<-gmarginals[,!grepl("PP_IND",names(gmarginals)),with=FALSE]
##Selection totals at CMA level
CMA_ST<-CMA_STf(hs,ps)
##Estimation totals at CMA level
CMA_ET<-CMA_ETf(weights)
###Estimation totals at DA level
weights<-fread("weights.csv")
weights<-cbind(c("DAr",DAnames),data.table(t(weights)))
colnames(weights)<-unlist(weights[1,])
weights<-weights[-1,]
weights[,1]<-as.numeric(unlist(weights[,1]))
DAweights<-merge(distinct(GIS[,c("DA","DAr")]),weights,by.x="DAr",by.y="DAr")
DAweights<-merge(DAweights,gmarginals[,c("DAr","HH_ID")],by.x="DAr",by.y="DAr")
DAweights<-merge(DAweights,bDAmarginals[,c("DA","HH_ID")],by.x="DA",by.y="DA")
DAweights<-as.data.table(t(cbind(DAweights[,DA],DAweights[,c(-1,-2,-(ncol(DAweights)-1),-ncol(DAweights)),with=FALSE]*DAweights[,HH_ID.y/HH_ID.x])))
colnames(DAweights)<-as.character(unlist(DAweights[1,]))
DAweights<-DAweights[-1,]
DAweights<-cbind(PUMFhh[,1],DAweights)
wPUMFhh<-data.table(merge(PUMFhh,DAweights,by="hid"))
wPUMFpp<-data.table(merge(PUMFpp,DAweights,by="hid"))
DA_ET=copy(hDAmarginals)
for (i in 3:9){
  x<-names(wPUMFhh)[i]
  temp<-wPUMFhh[,lapply(.SD,sum),by=c(x),.SDcols=names(wPUMFhh)[10:ncol(wPUMFhh)]]
  temp<-temp[order(temp[,1])]
  temp[,1]<-paste(names(temp)[1],".",1:nrow(temp),sep="")
  temp<-as.data.table(t(temp))
  names(temp)<-unlist(temp[1,])
  temp<-temp[-1,]
  DA_ET[,c(names(DA_ET)[grepl(x,names(DA_ET))]):=temp,with=FALSE]
}
for (i in 4:10){
  x<-names(wPUMFpp)[i]
  temp<-wPUMFpp[,lapply(.SD,sum),by=c(x),.SDcols=names(wPUMFpp)[11:ncol(wPUMFpp)]]
  temp<-temp[order(temp[,1])]
  temp[,1]<-paste(names(temp)[1],".",1:nrow(temp),sep="")
  temp<-as.data.table(t(temp))
  names(temp)<-unlist(temp[1,])
  temp<-temp[-1,]
  DA_ET[,c(names(DA_ET)[grepl(x,names(DA_ET))]):=temp,with=FALSE]
}
DA_ET<-cbind("DA"=as.numeric(DAnames),DA_ET[,-1][,match(names(CMA_ET),names(DA_ET[,-1])),with=FALSE])
DA_ET<-data.table(apply(DA_ET,2,as.numeric))
#Selection totals at DA level
DA_ST<-apply(wPUMFhh[,10:ncol(wPUMFhh)],2,DA_STf)
DA_ST<-rbindlist(lapply(DA_ST, as.data.frame.list), fill=TRUE)
names(DA_ST)<-names(CMA_ST)
DA_ST<-cbind(DA_ET[,1],DA_ST)
DA_ST[is.na(DA_ST)]<-0
DA_ST<-data.table(apply(DA_ST,2,as.numeric))
#Population Proximity Index
PPI<-round(mean(gmarginals$PP_ID)*1000/hCMAmarginals$PP_ID,2)
#Census discrepancies
deltaCensus<-abs(colSums(gmarginals)[-1] - hCMAmarginals)
deltaCensusErr1<-round(IndicatorsMean(deltaCensus*1000/hCMAmarginals),2)
deltaCensusErr2<-round(IndicatorsSum(deltaCensus)*1000/rep(c(hCMAmarginals$HH_ID,hCMAmarginals$PP_ID,hCMAmarginals$HH_ID+hCMAmarginals$PP_ID),3),2)
#Estimation fitting errors
estFit<-abs(CMA_ET - hCMAmarginals)
estFitErr1<-round(IndicatorsMean(estFit*1000/hCMAmarginals),2)
estFitErr2<-round(IndicatorsSum(estFit)*1000/rep(c(hCMAmarginals$HH_ID,hCMAmarginals$PP_ID,hCMAmarginals$HH_ID+hCMAmarginals$PP_ID),3),2)
#Selection fitting errors
selFit<-abs(CMA_ST - hCMAmarginals)
selFitErr1<-round(IndicatorsMean(selFit*1000/hCMAmarginals),2)
selFitErr2<-round(IndicatorsSum(selFit)*1000/rep(c(hCMAmarginals$HH_ID,hCMAmarginals$PP_ID,hCMAmarginals$HH_ID+hCMAmarginals$PP_ID),3),2)
#Estimation spatialization errors
estSpa<-data.table(t(colSums(abs(DA_ET-hDAmarginals)[,-1])))
estSpaErr1<-round(IndicatorsMean(estSpa*1000/hCMAmarginals),2)
estSpaErr2<-round(IndicatorsSum(estSpa)*1000/rep(c(hCMAmarginals$HH_ID,hCMAmarginals$PP_ID,hCMAmarginals$HH_ID+hCMAmarginals$PP_ID),3),2)
#Selection spatialization errors
selSpa<-data.table(t(colSums(abs(DA_ST-hDAmarginals)[,-1])))
selSpaErr1<-round(IndicatorsMean(selSpa*1000/hCMAmarginals),2)
selSpaErr2<-round(IndicatorsSum(selSpa)*1000/rep(c(hCMAmarginals$HH_ID,hCMAmarginals$PP_ID,hCMAmarginals$HH_ID+hCMAmarginals$PP_ID),3),2)
#Results
Errors<-cbind("error"=c("deltaCensusErr1","estFitErr1","selFitErr1","estSpaErr1","selSpaErr1","deltaCensusErr2","estFitErr2","selFitErr2","estSpaErr2","selSpaErr2"),"Scenario"=rep("S17",10),"PPI"=rep(PPI,10),rbind(deltaCensusErr1,estFitErr1,selFitErr1,estSpaErr1,selSpaErr1,deltaCensusErr2,estFitErr2,selFitErr2,estSpaErr2,selSpaErr2))
Results<-rbind(Results,Errors)





#S10-hCMA
##Import files
setwd("D:/OneDrive - polymtl.ca/Studies/Civil engineering/POLY/Projet Doctorat/PSP/Exported/A1/3. Analysis/Montreal/S10-hCMA")
#weights
weights<-fread("weights.csv")
names(weights)[-1]<-CMAnames
#synthetic population
hs<-fread("housing_synthetic.csv")
ps<-fread("person_synthetic.csv")
hs<-hs[,!grepl("rH",names(hs)),with=FALSE]
ps<-ps[,!grepl("rP",names(ps)),with=FALSE]
ps<-ps[,!grepl("PP_IND",names(ps)),with=FALSE]
#geo marginals
gmarginalshh<-fread("geo_household_marginals.csv")
gmarginalspp<-fread("geo_person_marginals.csv")
gmarginals<-cbind(gmarginalshh,gmarginalspp[,-1])
names(gmarginals)[-1]<-paste(names(gmarginals)[-1],".",gmarginals[1,-1],sep="")
gmarginals<-gmarginals[-1:-2,]
names(gmarginals)[grepl("99",names(gmarginals))]<-c("PP_IND.12","PP_JTWLEAVE.6","PP_JTWDUR.6", "PP_JTWMODE.7")
names(gmarginals)[grepl("ID",names(gmarginals))]<-c("HH_ID","PP_ID")
gmarginals[,1]<-list(CMAnames)
names(gmarginals)[1]<-"CMA"
gmarginals[,1]<-as.numeric(unlist(gmarginals[,1]))
temp<-gmarginals[,-1]
setcolorder(temp,match(varcat,names(temp)))
gmarginals<-data.table(cbind(gmarginals[,1],temp))
rm(gmarginalshh,gmarginalspp)
gmarginals<-gmarginals[,!grepl("PP_IND",names(gmarginals)),with=FALSE]
##Selection totals at CMA level
CMA_ST<-CMA_STf(hs,ps)
##Estimation totals at CMA level
CMA_ET<-CMA_ETf(weights)
###Estimation totals at DA level
weights<-fread("weights.csv")
weights<-cbind(c("CMA",CMAnames),data.table(t(weights)))
colnames(weights)<-unlist(weights[1,])
weights<-weights[-1,]
weights[,1]<-as.numeric(unlist(weights[,1]))
DAweights<-merge(distinct(GIS[,c("DA","CMA")]),weights,by.x="CMA",by.y="CMA")
DAweights<-merge(DAweights,gmarginals[,c("CMA","HH_ID")],by.x="CMA",by.y="CMA")
DAweights<-merge(DAweights,hDAmarginals[,c("DA","HH_ID")],by.x="DA",by.y="DA")
DAweights<-as.data.table(t(cbind(DAweights[,DA],DAweights[,c(-1,-2,-(ncol(DAweights)-1),-ncol(DAweights)),with=FALSE]*DAweights[,HH_ID.y/HH_ID.x])))
colnames(DAweights)<-as.character(unlist(DAweights[1,]))
DAweights<-DAweights[-1,]
DAweights<-cbind(PUMFhh[,1],DAweights)
wPUMFhh<-data.table(merge(PUMFhh,DAweights,by="hid"))
wPUMFpp<-data.table(merge(PUMFpp,DAweights,by="hid"))
DA_ET=copy(hDAmarginals)
for (i in 3:9){
  x<-names(wPUMFhh)[i]
  temp<-wPUMFhh[,lapply(.SD,sum),by=c(x),.SDcols=names(wPUMFhh)[10:ncol(wPUMFhh)]]
  temp<-temp[order(temp[,1])]
  temp[,1]<-paste(names(temp)[1],".",1:nrow(temp),sep="")
  temp<-as.data.table(t(temp))
  names(temp)<-unlist(temp[1,])
  temp<-temp[-1,]
  DA_ET[,c(names(DA_ET)[grepl(x,names(DA_ET))]):=temp,with=FALSE]
}
for (i in 4:10){
  x<-names(wPUMFpp)[i]
  temp<-wPUMFpp[,lapply(.SD,sum),by=c(x),.SDcols=names(wPUMFpp)[11:ncol(wPUMFpp)]]
  temp<-temp[order(temp[,1])]
  temp[,1]<-paste(names(temp)[1],".",1:nrow(temp),sep="")
  temp<-as.data.table(t(temp))
  names(temp)<-unlist(temp[1,])
  temp<-temp[-1,]
  DA_ET[,c(names(DA_ET)[grepl(x,names(DA_ET))]):=temp,with=FALSE]
}
DA_ET<-cbind("DA"=as.numeric(DAnames),DA_ET[,-1][,match(names(CMA_ET),names(DA_ET[,-1])),with=FALSE])
DA_ET<-data.table(apply(DA_ET,2,as.numeric))
#Selection totals at DA level
DA_ST<-apply(wPUMFhh[,10:ncol(wPUMFhh)],2,DA_STf)
DA_ST<-rbindlist(lapply(DA_ST, as.data.frame.list), fill=TRUE)
names(DA_ST)<-names(CMA_ST)
DA_ST<-cbind(DA_ET[,1],DA_ST)
DA_ST[is.na(DA_ST)]<-0
DA_ST<-data.table(apply(DA_ST,2,as.numeric))
#Population Proximity Index
PPI<-round(mean(gmarginals$PP_ID)*1000/hCMAmarginals$PP_ID,2)
#Census discrepancies
deltaCensus<-abs(colSums(gmarginals)[-1] - hCMAmarginals)
deltaCensusErr1<-round(IndicatorsMean(deltaCensus*1000/hCMAmarginals),2)
deltaCensusErr2<-round(IndicatorsSum(deltaCensus)*1000/rep(c(hCMAmarginals$HH_ID,hCMAmarginals$PP_ID,hCMAmarginals$HH_ID+hCMAmarginals$PP_ID),3),2)
#Estimation fitting errors
estFit<-abs(CMA_ET - hCMAmarginals)
estFitErr1<-round(IndicatorsMean(estFit*1000/hCMAmarginals),2)
estFitErr2<-round(IndicatorsSum(estFit)*1000/rep(c(hCMAmarginals$HH_ID,hCMAmarginals$PP_ID,hCMAmarginals$HH_ID+hCMAmarginals$PP_ID),3),2)
#Selection fitting errors
selFit<-abs(CMA_ST - hCMAmarginals)
selFitErr1<-round(IndicatorsMean(selFit*1000/hCMAmarginals),2)
selFitErr2<-round(IndicatorsSum(selFit)*1000/rep(c(hCMAmarginals$HH_ID,hCMAmarginals$PP_ID,hCMAmarginals$HH_ID+hCMAmarginals$PP_ID),3),2)
#Estimation spatialization errors
estSpa<-data.table(t(colSums(abs(DA_ET-hDAmarginals)[,-1])))
estSpaErr1<-round(IndicatorsMean(estSpa*1000/hCMAmarginals),2)
estSpaErr2<-round(IndicatorsSum(estSpa)*1000/rep(c(hCMAmarginals$HH_ID,hCMAmarginals$PP_ID,hCMAmarginals$HH_ID+hCMAmarginals$PP_ID),3),2)
#Selection spatialization errors
selSpa<-data.table(t(colSums(abs(DA_ST-hDAmarginals)[,-1])))
selSpaErr1<-round(IndicatorsMean(selSpa*1000/hCMAmarginals),2)
selSpaErr2<-round(IndicatorsSum(selSpa)*1000/rep(c(hCMAmarginals$HH_ID,hCMAmarginals$PP_ID,hCMAmarginals$HH_ID+hCMAmarginals$PP_ID),3),2)
#Results
Errors<-cbind("error"=c("deltaCensusErr1","estFitErr1","selFitErr1","estSpaErr1","selSpaErr1","deltaCensusErr2","estFitErr2","selFitErr2","estSpaErr2","selSpaErr2"),"Scenario"=rep("S10",10),"PPI"=rep(PPI,10),rbind(deltaCensusErr1,estFitErr1,selFitErr1,estSpaErr1,selSpaErr1,deltaCensusErr2,estFitErr2,selFitErr2,estSpaErr2,selSpaErr2))
Results<-rbind(Results,Errors)

#S12-hCMA_CSD
##Import files
setwd("D:/OneDrive - polymtl.ca/Studies/Civil engineering/POLY/Projet Doctorat/PSP/Exported/A1/3. Analysis/Montreal/S12-hCMA_CSD")
#weights
weights<-fread("weights.csv")
names(weights)[-1]<-CSDnames
#synthetic population
hs<-fread("housing_synthetic.csv")
ps<-fread("person_synthetic.csv")
hs<-hs[,!grepl("rH",names(hs)),with=FALSE]
ps<-ps[,!grepl("rP",names(ps)),with=FALSE]
ps<-ps[,!grepl("PP_IND",names(ps)),with=FALSE]
#geo marginals
gmarginalshh<-fread("geo_household_marginals.csv")
gmarginalspp<-fread("geo_person_marginals.csv")
gmarginals<-cbind(gmarginalshh,gmarginalspp[,-1])
names(gmarginals)[-1]<-paste(names(gmarginals)[-1],".",gmarginals[1,-1],sep="")
gmarginals<-gmarginals[-1:-2,]
names(gmarginals)[grepl("99",names(gmarginals))]<-c("PP_IND.12","PP_JTWLEAVE.6","PP_JTWDUR.6", "PP_JTWMODE.7")
names(gmarginals)[grepl("ID",names(gmarginals))]<-c("HH_ID","PP_ID")
gmarginals[,1]<-list(CSDnames)
names(gmarginals)[1]<-"CSD"
gmarginals[,1]<-as.numeric(unlist(gmarginals[,1]))
temp<-gmarginals[,-1]
setcolorder(temp,match(varcat,names(temp)))
gmarginals<-data.table(cbind(gmarginals[,1],temp))
rm(gmarginalshh,gmarginalspp)
gmarginals<-gmarginals[,!grepl("PP_IND",names(gmarginals)),with=FALSE]
##Selection totals at CMA level
CMA_ST<-CMA_STf(hs,ps)
##Estimation totals at CMA level
CMA_ET<-CMA_ETf(weights)
###Estimation totals at DA level
weights<-fread("weights.csv")
weights<-cbind(c("CSD",CSDnames),data.table(t(weights)))
colnames(weights)<-unlist(weights[1,])
weights<-weights[-1,]
weights[,1]<-as.numeric(unlist(weights[,1]))
DAweights<-merge(distinct(GIS[,c("DA","CSD")]),weights,by.x="CSD",by.y="CSD")
DAweights<-merge(DAweights,gmarginals[,c("CSD","HH_ID")],by.x="CSD",by.y="CSD")
DAweights<-merge(DAweights,hDAmarginals[,c("DA","HH_ID")],by.x="DA",by.y="DA")
DAweights<-as.data.table(t(cbind(DAweights[,DA],DAweights[,c(-1,-2,-(ncol(DAweights)-1),-ncol(DAweights)),with=FALSE]*DAweights[,HH_ID.y/HH_ID.x])))
colnames(DAweights)<-as.character(unlist(DAweights[1,]))
DAweights<-DAweights[-1,]
DAweights<-cbind(PUMFhh[,1],DAweights)
wPUMFhh<-data.table(merge(PUMFhh,DAweights,by="hid"))
wPUMFpp<-data.table(merge(PUMFpp,DAweights,by="hid"))
DA_ET=copy(hDAmarginals)
for (i in 3:9){
  x<-names(wPUMFhh)[i]
  temp<-wPUMFhh[,lapply(.SD,sum),by=c(x),.SDcols=names(wPUMFhh)[10:ncol(wPUMFhh)]]
  temp<-temp[order(temp[,1])]
  temp[,1]<-paste(names(temp)[1],".",1:nrow(temp),sep="")
  temp<-as.data.table(t(temp))
  names(temp)<-unlist(temp[1,])
  temp<-temp[-1,]
  DA_ET[,c(names(DA_ET)[grepl(x,names(DA_ET))]):=temp,with=FALSE]
}
for (i in 4:10){
  x<-names(wPUMFpp)[i]
  temp<-wPUMFpp[,lapply(.SD,sum),by=c(x),.SDcols=names(wPUMFpp)[11:ncol(wPUMFpp)]]
  temp<-temp[order(temp[,1])]
  temp[,1]<-paste(names(temp)[1],".",1:nrow(temp),sep="")
  temp<-as.data.table(t(temp))
  names(temp)<-unlist(temp[1,])
  temp<-temp[-1,]
  DA_ET[,c(names(DA_ET)[grepl(x,names(DA_ET))]):=temp,with=FALSE]
}
DA_ET<-cbind("DA"=as.numeric(DAnames),DA_ET[,-1][,match(names(CMA_ET),names(DA_ET[,-1])),with=FALSE])
DA_ET<-data.table(apply(DA_ET,2,as.numeric))
#Selection totals at DA level
DA_ST<-apply(wPUMFhh[,10:ncol(wPUMFhh)],2,DA_STf)
DA_ST<-rbindlist(lapply(DA_ST, as.data.frame.list), fill=TRUE)
names(DA_ST)<-names(CMA_ST)
DA_ST<-cbind(DA_ET[,1],DA_ST)
DA_ST[is.na(DA_ST)]<-0
DA_ST<-data.table(apply(DA_ST,2,as.numeric))
#Population Proximity Index
PPI<-round(mean(gmarginals$PP_ID)*1000/hCMAmarginals$PP_ID,2)
#Census discrepancies
deltaCensus<-abs(colSums(gmarginals)[-1] - hCMAmarginals)
deltaCensusErr1<-round(IndicatorsMean(deltaCensus*1000/hCMAmarginals),2)
deltaCensusErr2<-round(IndicatorsSum(deltaCensus)*1000/rep(c(hCMAmarginals$HH_ID,hCMAmarginals$PP_ID,hCMAmarginals$HH_ID+hCMAmarginals$PP_ID),3),2)
#Estimation fitting errors
estFit<-abs(CMA_ET - hCMAmarginals)
estFitErr1<-round(IndicatorsMean(estFit*1000/hCMAmarginals),2)
estFitErr2<-round(IndicatorsSum(estFit)*1000/rep(c(hCMAmarginals$HH_ID,hCMAmarginals$PP_ID,hCMAmarginals$HH_ID+hCMAmarginals$PP_ID),3),2)
#Selection fitting errors
selFit<-abs(CMA_ST - hCMAmarginals)
selFitErr1<-round(IndicatorsMean(selFit*1000/hCMAmarginals),2)
selFitErr2<-round(IndicatorsSum(selFit)*1000/rep(c(hCMAmarginals$HH_ID,hCMAmarginals$PP_ID,hCMAmarginals$HH_ID+hCMAmarginals$PP_ID),3),2)
#Estimation spatialization errors
estSpa<-data.table(t(colSums(abs(DA_ET-hDAmarginals)[,-1])))
estSpaErr1<-round(IndicatorsMean(estSpa*1000/hCMAmarginals),2)
estSpaErr2<-round(IndicatorsSum(estSpa)*1000/rep(c(hCMAmarginals$HH_ID,hCMAmarginals$PP_ID,hCMAmarginals$HH_ID+hCMAmarginals$PP_ID),3),2)
#Selection spatialization errors
selSpa<-data.table(t(colSums(abs(DA_ST-hDAmarginals)[,-1])))
selSpaErr1<-round(IndicatorsMean(selSpa*1000/hCMAmarginals),2)
selSpaErr2<-round(IndicatorsSum(selSpa)*1000/rep(c(hCMAmarginals$HH_ID,hCMAmarginals$PP_ID,hCMAmarginals$HH_ID+hCMAmarginals$PP_ID),3),2)
#Results
Errors<-cbind("error"=c("deltaCensusErr1","estFitErr1","selFitErr1","estSpaErr1","selSpaErr1","deltaCensusErr2","estFitErr2","selFitErr2","estSpaErr2","selSpaErr2"),"Scenario"=rep("S12",10),"PPI"=rep(PPI,10),rbind(deltaCensusErr1,estFitErr1,selFitErr1,estSpaErr1,selSpaErr1,deltaCensusErr2,estFitErr2,selFitErr2,estSpaErr2,selSpaErr2))
Results<-rbind(Results,Errors)

#S14-hCMA_ADA
##Import files
setwd("D:/OneDrive - polymtl.ca/Studies/Civil engineering/POLY/Projet Doctorat/PSP/Exported/A1/3. Analysis/Montreal/S14-hCMA_ADA")
#weights
weights<-fread("weights.csv")
names(weights)[-1]<-ADAnames
#synthetic population
hs<-fread("housing_synthetic.csv")
ps<-fread("person_synthetic.csv")
hs<-hs[,!grepl("rH",names(hs)),with=FALSE]
ps<-ps[,!grepl("rP",names(ps)),with=FALSE]
ps<-ps[,!grepl("PP_IND",names(ps)),with=FALSE]
#geo marginals
gmarginalshh<-fread("geo_household_marginals.csv")
gmarginalspp<-fread("geo_person_marginals.csv")
gmarginals<-cbind(gmarginalshh,gmarginalspp[,-1])
names(gmarginals)[-1]<-paste(names(gmarginals)[-1],".",gmarginals[1,-1],sep="")
gmarginals<-gmarginals[-1:-2,]
names(gmarginals)[grepl("99",names(gmarginals))]<-c("PP_IND.12","PP_JTWLEAVE.6","PP_JTWDUR.6", "PP_JTWMODE.7")
names(gmarginals)[grepl("ID",names(gmarginals))]<-c("HH_ID","PP_ID")
gmarginals[,1]<-list(ADAnames)
names(gmarginals)[1]<-"ADA"
gmarginals[,1]<-as.numeric(unlist(gmarginals[,1]))
temp<-gmarginals[,-1]
setcolorder(temp,match(varcat,names(temp)))
gmarginals<-data.table(cbind(gmarginals[,1],temp))
rm(gmarginalshh,gmarginalspp)
gmarginals<-gmarginals[,!grepl("PP_IND",names(gmarginals)),with=FALSE]
##Selection totals at CMA level
CMA_ST<-CMA_STf(hs,ps)
##Estimation totals at CMA level
CMA_ET<-CMA_ETf(weights)
###Estimation totals at DA level
weights<-fread("weights.csv")
weights<-cbind(c("ADA",ADAnames),data.table(t(weights)))
colnames(weights)<-unlist(weights[1,])
weights<-weights[-1,]
weights[,1]<-as.numeric(unlist(weights[,1]))
DAweights<-merge(distinct(GIS[,c("DA","ADA")]),weights,by.x="ADA",by.y="ADA")
DAweights<-merge(DAweights,gmarginals[,c("ADA","HH_ID")],by.x="ADA",by.y="ADA")
DAweights<-merge(DAweights,hDAmarginals[,c("DA","HH_ID")],by.x="DA",by.y="DA")
DAweights<-as.data.table(t(cbind(DAweights[,DA],DAweights[,c(-1,-2,-(ncol(DAweights)-1),-ncol(DAweights)),with=FALSE]*DAweights[,HH_ID.y/HH_ID.x])))
colnames(DAweights)<-as.character(unlist(DAweights[1,]))
DAweights<-DAweights[-1,]
DAweights<-cbind(PUMFhh[,1],DAweights)
wPUMFhh<-data.table(merge(PUMFhh,DAweights,by="hid"))
wPUMFpp<-data.table(merge(PUMFpp,DAweights,by="hid"))
DA_ET=copy(hDAmarginals)
for (i in 3:9){
  x<-names(wPUMFhh)[i]
  temp<-wPUMFhh[,lapply(.SD,sum),by=c(x),.SDcols=names(wPUMFhh)[10:ncol(wPUMFhh)]]
  temp<-temp[order(temp[,1])]
  temp[,1]<-paste(names(temp)[1],".",1:nrow(temp),sep="")
  temp<-as.data.table(t(temp))
  names(temp)<-unlist(temp[1,])
  temp<-temp[-1,]
  DA_ET[,c(names(DA_ET)[grepl(x,names(DA_ET))]):=temp,with=FALSE]
}
for (i in 4:10){
  x<-names(wPUMFpp)[i]
  temp<-wPUMFpp[,lapply(.SD,sum),by=c(x),.SDcols=names(wPUMFpp)[11:ncol(wPUMFpp)]]
  temp<-temp[order(temp[,1])]
  temp[,1]<-paste(names(temp)[1],".",1:nrow(temp),sep="")
  temp<-as.data.table(t(temp))
  names(temp)<-unlist(temp[1,])
  temp<-temp[-1,]
  DA_ET[,c(names(DA_ET)[grepl(x,names(DA_ET))]):=temp,with=FALSE]
}
DA_ET<-cbind("DA"=as.numeric(DAnames),DA_ET[,-1][,match(names(CMA_ET),names(DA_ET[,-1])),with=FALSE])
DA_ET<-data.table(apply(DA_ET,2,as.numeric))
#Selection totals at DA level
DA_ST<-apply(wPUMFhh[,10:ncol(wPUMFhh)],2,DA_STf)
DA_ST<-rbindlist(lapply(DA_ST, as.data.frame.list), fill=TRUE)
names(DA_ST)<-names(CMA_ST)
DA_ST<-cbind(DA_ET[,1],DA_ST)
DA_ST[is.na(DA_ST)]<-0
DA_ST<-data.table(apply(DA_ST,2,as.numeric))
#Population Proximity Index
PPI<-round(mean(gmarginals$PP_ID)*1000/hCMAmarginals$PP_ID,2)
#Census discrepancies
deltaCensus<-abs(colSums(gmarginals)[-1] - hCMAmarginals)
deltaCensusErr1<-round(IndicatorsMean(deltaCensus*1000/hCMAmarginals),2)
deltaCensusErr2<-round(IndicatorsSum(deltaCensus)*1000/rep(c(hCMAmarginals$HH_ID,hCMAmarginals$PP_ID,hCMAmarginals$HH_ID+hCMAmarginals$PP_ID),3),2)
#Estimation fitting errors
estFit<-abs(CMA_ET - hCMAmarginals)
estFitErr1<-round(IndicatorsMean(estFit*1000/hCMAmarginals),2)
estFitErr2<-round(IndicatorsSum(estFit)*1000/rep(c(hCMAmarginals$HH_ID,hCMAmarginals$PP_ID,hCMAmarginals$HH_ID+hCMAmarginals$PP_ID),3),2)
#Selection fitting errors
selFit<-abs(CMA_ST - hCMAmarginals)
selFitErr1<-round(IndicatorsMean(selFit*1000/hCMAmarginals),2)
selFitErr2<-round(IndicatorsSum(selFit)*1000/rep(c(hCMAmarginals$HH_ID,hCMAmarginals$PP_ID,hCMAmarginals$HH_ID+hCMAmarginals$PP_ID),3),2)
#Estimation spatialization errors
estSpa<-data.table(t(colSums(abs(DA_ET-hDAmarginals)[,-1])))
estSpaErr1<-round(IndicatorsMean(estSpa*1000/hCMAmarginals),2)
estSpaErr2<-round(IndicatorsSum(estSpa)*1000/rep(c(hCMAmarginals$HH_ID,hCMAmarginals$PP_ID,hCMAmarginals$HH_ID+hCMAmarginals$PP_ID),3),2)
#Selection spatialization errors
selSpa<-data.table(t(colSums(abs(DA_ST-hDAmarginals)[,-1])))
selSpaErr1<-round(IndicatorsMean(selSpa*1000/hCMAmarginals),2)
selSpaErr2<-round(IndicatorsSum(selSpa)*1000/rep(c(hCMAmarginals$HH_ID,hCMAmarginals$PP_ID,hCMAmarginals$HH_ID+hCMAmarginals$PP_ID),3),2)
#Results
Errors<-cbind("error"=c("deltaCensusErr1","estFitErr1","selFitErr1","estSpaErr1","selSpaErr1","deltaCensusErr2","estFitErr2","selFitErr2","estSpaErr2","selSpaErr2"),"Scenario"=rep("S14",10),"PPI"=rep(PPI,10),rbind(deltaCensusErr1,estFitErr1,selFitErr1,estSpaErr1,selSpaErr1,deltaCensusErr2,estFitErr2,selFitErr2,estSpaErr2,selSpaErr2))
Results<-rbind(Results,Errors)

#S16-hCMA_CT
##Import files
setwd("D:/OneDrive - polymtl.ca/Studies/Civil engineering/POLY/Projet Doctorat/PSP/Exported/A1/3. Analysis/Montreal/S16-hCMA_CT")
#weights
weights<-fread("weights.csv")
names(weights)[-1]<-CTnames
#synthetic population
hs<-fread("housing_synthetic.csv")
ps<-fread("person_synthetic.csv")
hs<-hs[,!grepl("rH",names(hs)),with=FALSE]
ps<-ps[,!grepl("rP",names(ps)),with=FALSE]
ps<-ps[,!grepl("PP_IND",names(ps)),with=FALSE]
#geo marginals
gmarginalshh<-fread("geo_household_marginals.csv")
gmarginalspp<-fread("geo_person_marginals.csv")
gmarginals<-cbind(gmarginalshh,gmarginalspp[,-1])
names(gmarginals)[-1]<-paste(names(gmarginals)[-1],".",gmarginals[1,-1],sep="")
gmarginals<-gmarginals[-1:-2,]
names(gmarginals)[grepl("99",names(gmarginals))]<-c("PP_IND.12","PP_JTWLEAVE.6","PP_JTWDUR.6", "PP_JTWMODE.7")
names(gmarginals)[grepl("ID",names(gmarginals))]<-c("HH_ID","PP_ID")
gmarginals[,1]<-list(CTnames)
names(gmarginals)[1]<-"CT"
gmarginals[,1]<-as.numeric(unlist(gmarginals[,1]))
temp<-gmarginals[,-1]
setcolorder(temp,match(varcat,names(temp)))
gmarginals<-data.table(cbind(gmarginals[,1],temp))
rm(gmarginalshh,gmarginalspp)
gmarginals<-gmarginals[,!grepl("PP_IND",names(gmarginals)),with=FALSE]
##Selection totals at CMA level
CMA_ST<-CMA_STf(hs,ps)
##Estimation totals at CMA level
CMA_ET<-CMA_ETf(weights)
###Estimation totals at DA level
weights<-fread("weights.csv")
weights<-cbind(c("CT",CTnames),data.table(t(weights)))
colnames(weights)<-unlist(weights[1,])
weights<-weights[-1,]
weights[,1]<-as.numeric(unlist(weights[,1]))
DAweights<-merge(distinct(GIS[,c("DA","CT")]),weights,by.x="CT",by.y="CT")
DAweights<-merge(DAweights,gmarginals[,c("CT","HH_ID")],by.x="CT",by.y="CT")
DAweights<-merge(DAweights,hDAmarginals[,c("DA","HH_ID")],by.x="DA",by.y="DA")
DAweights<-as.data.table(t(cbind(DAweights[,DA],DAweights[,c(-1,-2,-(ncol(DAweights)-1),-ncol(DAweights)),with=FALSE]*DAweights[,HH_ID.y/HH_ID.x])))
colnames(DAweights)<-as.character(unlist(DAweights[1,]))
DAweights<-DAweights[-1,]
DAweights<-cbind(PUMFhh[,1],DAweights)
wPUMFhh<-data.table(merge(PUMFhh,DAweights,by="hid"))
wPUMFpp<-data.table(merge(PUMFpp,DAweights,by="hid"))
DA_ET=copy(hDAmarginals)
for (i in 3:9){
  x<-names(wPUMFhh)[i]
  temp<-wPUMFhh[,lapply(.SD,sum),by=c(x),.SDcols=names(wPUMFhh)[10:ncol(wPUMFhh)]]
  temp<-temp[order(temp[,1])]
  temp[,1]<-paste(names(temp)[1],".",1:nrow(temp),sep="")
  temp<-as.data.table(t(temp))
  names(temp)<-unlist(temp[1,])
  temp<-temp[-1,]
  DA_ET[,c(names(DA_ET)[grepl(x,names(DA_ET))]):=temp,with=FALSE]
}
for (i in 4:10){
  x<-names(wPUMFpp)[i]
  temp<-wPUMFpp[,lapply(.SD,sum),by=c(x),.SDcols=names(wPUMFpp)[11:ncol(wPUMFpp)]]
  temp<-temp[order(temp[,1])]
  temp[,1]<-paste(names(temp)[1],".",1:nrow(temp),sep="")
  temp<-as.data.table(t(temp))
  names(temp)<-unlist(temp[1,])
  temp<-temp[-1,]
  DA_ET[,c(names(DA_ET)[grepl(x,names(DA_ET))]):=temp,with=FALSE]
}
DA_ET<-cbind("DA"=as.numeric(DAnames),DA_ET[,-1][,match(names(CMA_ET),names(DA_ET[,-1])),with=FALSE])
DA_ET<-data.table(apply(DA_ET,2,as.numeric))
#Selection totals at DA level
DA_ST<-apply(wPUMFhh[,10:ncol(wPUMFhh)],2,DA_STf)
DA_ST<-rbindlist(lapply(DA_ST, as.data.frame.list), fill=TRUE)
names(DA_ST)<-names(CMA_ST)
DA_ST<-cbind(DA_ET[,1],DA_ST)
DA_ST[is.na(DA_ST)]<-0
DA_ST<-data.table(apply(DA_ST,2,as.numeric))
#Population Proximity Index
PPI<-round(mean(gmarginals$PP_ID)*1000/hCMAmarginals$PP_ID,2)
#Census discrepancies
deltaCensus<-abs(colSums(gmarginals)[-1] - hCMAmarginals)
deltaCensusErr1<-round(IndicatorsMean(deltaCensus*1000/hCMAmarginals),2)
deltaCensusErr2<-round(IndicatorsSum(deltaCensus)*1000/rep(c(hCMAmarginals$HH_ID,hCMAmarginals$PP_ID,hCMAmarginals$HH_ID+hCMAmarginals$PP_ID),3),2)
#Estimation fitting errors
estFit<-abs(CMA_ET - hCMAmarginals)
estFitErr1<-round(IndicatorsMean(estFit*1000/hCMAmarginals),2)
estFitErr2<-round(IndicatorsSum(estFit)*1000/rep(c(hCMAmarginals$HH_ID,hCMAmarginals$PP_ID,hCMAmarginals$HH_ID+hCMAmarginals$PP_ID),3),2)
#Selection fitting errors
selFit<-abs(CMA_ST - hCMAmarginals)
selFitErr1<-round(IndicatorsMean(selFit*1000/hCMAmarginals),2)
selFitErr2<-round(IndicatorsSum(selFit)*1000/rep(c(hCMAmarginals$HH_ID,hCMAmarginals$PP_ID,hCMAmarginals$HH_ID+hCMAmarginals$PP_ID),3),2)
#Estimation spatialization errors
estSpa<-data.table(t(colSums(abs(DA_ET-hDAmarginals)[,-1])))
estSpaErr1<-round(IndicatorsMean(estSpa*1000/hCMAmarginals),2)
estSpaErr2<-round(IndicatorsSum(estSpa)*1000/rep(c(hCMAmarginals$HH_ID,hCMAmarginals$PP_ID,hCMAmarginals$HH_ID+hCMAmarginals$PP_ID),3),2)
#Selection spatialization errors
selSpa<-data.table(t(colSums(abs(DA_ST-hDAmarginals)[,-1])))
selSpaErr1<-round(IndicatorsMean(selSpa*1000/hCMAmarginals),2)
selSpaErr2<-round(IndicatorsSum(selSpa)*1000/rep(c(hCMAmarginals$HH_ID,hCMAmarginals$PP_ID,hCMAmarginals$HH_ID+hCMAmarginals$PP_ID),3),2)
#Results
Errors<-cbind("error"=c("deltaCensusErr1","estFitErr1","selFitErr1","estSpaErr1","selSpaErr1","deltaCensusErr2","estFitErr2","selFitErr2","estSpaErr2","selSpaErr2"),"Scenario"=rep("S16",10),"PPI"=rep(PPI,10),rbind(deltaCensusErr1,estFitErr1,selFitErr1,estSpaErr1,selSpaErr1,deltaCensusErr2,estFitErr2,selFitErr2,estSpaErr2,selSpaErr2))
Results<-rbind(Results,Errors)

#S18-hCMA_DA
##Import files
setwd("D:/OneDrive - polymtl.ca/Studies/Civil engineering/POLY/Projet Doctorat/PSP/Exported/A1/3. Analysis/Montreal/S18-hCMA_DA")
#weights
weights<-fread("weights.csv")
names(weights)[-1]<-DAnames
#synthetic population
hs<-fread("housing_synthetic.csv")
ps<-fread("person_synthetic.csv")
hs<-hs[,!grepl("rH",names(hs)),with=FALSE]
ps<-ps[,!grepl("rP",names(ps)),with=FALSE]
ps<-ps[,!grepl("PP_IND",names(ps)),with=FALSE]
#geo marginals
gmarginalshh<-fread("geo_household_marginals.csv")
gmarginalspp<-fread("geo_person_marginals.csv")
gmarginals<-cbind(gmarginalshh,gmarginalspp[,-1])
names(gmarginals)[-1]<-paste(names(gmarginals)[-1],".",gmarginals[1,-1],sep="")
gmarginals<-gmarginals[-1:-2,]
names(gmarginals)[grepl("99",names(gmarginals))]<-c("PP_IND.12","PP_JTWLEAVE.6","PP_JTWDUR.6", "PP_JTWMODE.7")
names(gmarginals)[grepl("ID",names(gmarginals))]<-c("HH_ID","PP_ID")
gmarginals[,1]<-list(DAnames)
names(gmarginals)[1]<-"DAr"
gmarginals[,1]<-as.numeric(unlist(gmarginals[,1]))
temp<-gmarginals[,-1]
setcolorder(temp,match(varcat,names(temp)))
gmarginals<-data.table(cbind(gmarginals[,1],temp))
rm(gmarginalshh,gmarginalspp)
gmarginals<-gmarginals[,!grepl("PP_IND",names(gmarginals)),with=FALSE]
##Selection totals at CMA level
CMA_ST<-CMA_STf(hs,ps)
##Estimation totals at CMA level
CMA_ET<-CMA_ETf(weights)
###Estimation totals at DA level
weights<-fread("weights.csv")
weights<-cbind(c("DAr",DAnames),data.table(t(weights)))
colnames(weights)<-unlist(weights[1,])
weights<-weights[-1,]
weights[,1]<-as.numeric(unlist(weights[,1]))
DAweights<-merge(distinct(GIS[,c("DA","DAr")]),weights,by.x="DAr",by.y="DAr")
DAweights<-merge(DAweights,gmarginals[,c("DAr","HH_ID")],by.x="DAr",by.y="DAr")
DAweights<-merge(DAweights,bDAmarginals[,c("DA","HH_ID")],by.x="DA",by.y="DA")
DAweights<-as.data.table(t(cbind(DAweights[,DA],DAweights[,c(-1,-2,-(ncol(DAweights)-1),-ncol(DAweights)),with=FALSE]*DAweights[,HH_ID.y/HH_ID.x])))
colnames(DAweights)<-as.character(unlist(DAweights[1,]))
DAweights<-DAweights[-1,]
DAweights<-cbind(PUMFhh[,1],DAweights)
wPUMFhh<-data.table(merge(PUMFhh,DAweights,by="hid"))
wPUMFpp<-data.table(merge(PUMFpp,DAweights,by="hid"))
DA_ET=copy(hDAmarginals)
for (i in 3:9){
  x<-names(wPUMFhh)[i]
  temp<-wPUMFhh[,lapply(.SD,sum),by=c(x),.SDcols=names(wPUMFhh)[10:ncol(wPUMFhh)]]
  temp<-temp[order(temp[,1])]
  temp[,1]<-paste(names(temp)[1],".",1:nrow(temp),sep="")
  temp<-as.data.table(t(temp))
  names(temp)<-unlist(temp[1,])
  temp<-temp[-1,]
  DA_ET[,c(names(DA_ET)[grepl(x,names(DA_ET))]):=temp,with=FALSE]
}
for (i in 4:10){
  x<-names(wPUMFpp)[i]
  temp<-wPUMFpp[,lapply(.SD,sum),by=c(x),.SDcols=names(wPUMFpp)[11:ncol(wPUMFpp)]]
  temp<-temp[order(temp[,1])]
  temp[,1]<-paste(names(temp)[1],".",1:nrow(temp),sep="")
  temp<-as.data.table(t(temp))
  names(temp)<-unlist(temp[1,])
  temp<-temp[-1,]
  DA_ET[,c(names(DA_ET)[grepl(x,names(DA_ET))]):=temp,with=FALSE]
}
DA_ET<-cbind("DA"=as.numeric(DAnames),DA_ET[,-1][,match(names(CMA_ET),names(DA_ET[,-1])),with=FALSE])
DA_ET<-data.table(apply(DA_ET,2,as.numeric))
#Selection totals at DA level
DA_ST<-apply(wPUMFhh[,10:ncol(wPUMFhh)],2,DA_STf)
DA_ST<-rbindlist(lapply(DA_ST, as.data.frame.list), fill=TRUE)
names(DA_ST)<-names(CMA_ST)
DA_ST<-cbind(DA_ET[,1],DA_ST)
DA_ST[is.na(DA_ST)]<-0
DA_ST<-data.table(apply(DA_ST,2,as.numeric))
#Population Proximity Index
PPI<-round(mean(gmarginals$PP_ID)*1000/hCMAmarginals$PP_ID,2)
#Census discrepancies
deltaCensus<-abs(colSums(gmarginals)[-1] - hCMAmarginals)
deltaCensusErr1<-round(IndicatorsMean(deltaCensus*1000/hCMAmarginals),2)
deltaCensusErr2<-round(IndicatorsSum(deltaCensus)*1000/rep(c(hCMAmarginals$HH_ID,hCMAmarginals$PP_ID,hCMAmarginals$HH_ID+hCMAmarginals$PP_ID),3),2)
#Estimation fitting errors
estFit<-abs(CMA_ET - hCMAmarginals)
estFitErr1<-round(IndicatorsMean(estFit*1000/hCMAmarginals),2)
estFitErr2<-round(IndicatorsSum(estFit)*1000/rep(c(hCMAmarginals$HH_ID,hCMAmarginals$PP_ID,hCMAmarginals$HH_ID+hCMAmarginals$PP_ID),3),2)
#Selection fitting errors
selFit<-abs(CMA_ST - hCMAmarginals)
selFitErr1<-round(IndicatorsMean(selFit*1000/hCMAmarginals),2)
selFitErr2<-round(IndicatorsSum(selFit)*1000/rep(c(hCMAmarginals$HH_ID,hCMAmarginals$PP_ID,hCMAmarginals$HH_ID+hCMAmarginals$PP_ID),3),2)
#Estimation spatialization errors
estSpa<-data.table(t(colSums(abs(DA_ET-hDAmarginals)[,-1])))
estSpaErr1<-round(IndicatorsMean(estSpa*1000/hCMAmarginals),2)
estSpaErr2<-round(IndicatorsSum(estSpa)*1000/rep(c(hCMAmarginals$HH_ID,hCMAmarginals$PP_ID,hCMAmarginals$HH_ID+hCMAmarginals$PP_ID),3),2)
#Selection spatialization errors
selSpa<-data.table(t(colSums(abs(DA_ST-hDAmarginals)[,-1])))
selSpaErr1<-round(IndicatorsMean(selSpa*1000/hCMAmarginals),2)
selSpaErr2<-round(IndicatorsSum(selSpa)*1000/rep(c(hCMAmarginals$HH_ID,hCMAmarginals$PP_ID,hCMAmarginals$HH_ID+hCMAmarginals$PP_ID),3),2)
#Results
Errors<-cbind("error"=c("deltaCensusErr1","estFitErr1","selFitErr1","estSpaErr1","selSpaErr1","deltaCensusErr2","estFitErr2","selFitErr2","estSpaErr2","selSpaErr2"),"Scenario"=rep("S18",10),"PPI"=rep(PPI,10),rbind(deltaCensusErr1,estFitErr1,selFitErr1,estSpaErr1,selSpaErr1,deltaCensusErr2,estFitErr2,selFitErr2,estSpaErr2,selSpaErr2))
Results<-rbind(Results,Errors)

write.csv(Results,"D:/OneDrive - polymtl.ca/Studies/Civil engineering/POLY/Projet Doctorat/PSP/Exported/A1/3. Analysis/Montreal/LastResults.csv")





#######################################################################################################################################
# #S1-bCMA
# #Import Results
# rhhmarginals<-read.csv("Exported/A1/3. Analysis/Toronto/S1-bCMA/region_household_marginals.csv")
# rppmarginals<-read.csv("Exported/A1/3. Analysis/Toronto/S1-bCMA/region_person_marginals.csv")
# rmarginals<-cbind(rhhmarginals,rppmarginals[,-1])
# rsynmarginals<-read.csv("Exported/A1/3. Analysis/Toronto/S1-bCMA/summary_region.csv")
# rsynmarginals<-rsynmarginals[,grepl("r",names(rsynmarginals))]
# rmarginals<-rmarginals[,names(rmarginals)%in%names(rsynmarginals)]
# rsynmarginals<-rsynmarginals[match(names(rmarginals),names(rsynmarginals))]
# ghhmarginals<-read.csv("Exported/A1/3. Analysis/Toronto/S1-bCMA/geo_household_marginals.csv")
# gppmarginals<-read.csv("Exported/A1/3. Analysis/Toronto/S1-bCMA/geo_person_marginals.csv")
# gmarginals<-cbind(ghhmarginals,gppmarginals[,-1])
# gsynmarginals<-read.csv("Exported/A1/3. Analysis/Toronto/S1-bCMA/summary_geo.csv")
# gsynmarginals<-gsynmarginals[,!grepl("r",names(gsynmarginals))]
# gmarginals<-gmarginals[,names(gmarginals)%in%names(gsynmarginals)]
# gsynmarginals<-gsynmarginals[match(names(gmarginals),names(gsynmarginals))]
# #Scenario
# Scenario<-"S1-bCMA"
# #Ip
# # Iph<-round(sum(as.numeric(ghhmarginals$HH_ID[-1:-2])*as.numeric(ghhmarginals$HH_ID[-1:-2])*100/rhhmarginals$rHH_ID[-1:-2])/(sum(ghhmarginals$HH_ID[-1:-2])),2)
# Ipp<-round(sum(as.numeric(gppmarginals$PP_ID[-1:-2])*as.numeric(gppmarginals$PP_ID[-1:-2])*100/rppmarginals$rPP_ID[-1:-2])/(sum(gppmarginals$PP_ID[-1:-2])),2)
# #REGION absolute differences on total households, persons and agents synthesized (gamma)
# rgammahh<-abs(rmarginals$rHH_ID[-1:-2]-rsynmarginals$rHH_ID[-1:-2])
# rgammapp<-abs(rmarginals$rPP_ID[-1:-2]-rsynmarginals$rPP_ID[-1:-2])
# rgammatot<-rgammahh + rgammapp
# #REGION relative differences on total households, persons and agents synthesized (relgamma)
# rrelgammahh<-round(abs(rmarginals$rHH_ID[-1:-2]-rsynmarginals$rHH_ID[-1:-2])*100/rmarginals$rHH_ID[-1:-2],5)
# rrelgammapp<-round(abs(rmarginals$rPP_ID[-1:-2]-rsynmarginals$rPP_ID[-1:-2])*100/rmarginals$rPP_ID[-1:-2],5)
# rrelgammatot<-round(rgammatot*100/(rmarginals$rHH_ID[-1:-2]+rmarginals$rPP_ID[-1:-2]),5)
# #REGION total absolute errors (beta)
# rbetahh<-sum(abs(rmarginals[-1:-2,grepl("rHH_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rHH_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))]))
# rbetapp<-sum(abs(rmarginals[-1:-2,grepl("rPP_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rPP_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))]))
# rbetatot<-rbetahh + rbetapp
# #REGION relative total absolute errors (rrelbeta)
# rrelbetahh<-round(sum(abs(rmarginals[-1:-2,grepl("rHH_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rHH_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))]))*100/rmarginals$rHH_ID[-1:-2],2)
# rrelbetapp<-round(sum(abs(rmarginals[-1:-2,grepl("rPP_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rPP_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))]))*100/rmarginals$rPP_ID[-1:-2],2)
# rrelbetatot<-round((sum(abs(rmarginals[-1:-2,grepl("rHH_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rHH_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))])) + sum(abs(rmarginals[-1:-2,grepl("rPP_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rPP_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))])))*100/(rmarginals$rHH_ID[-1:-2] + rmarginals$rPP_ID[-1:-2]),2)
# #REGION absolute errors on households, persons and agents per category (alpha)
# ralphahh<-abs(rmarginals[-1:-2,grepl("rHH_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rHH_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))])
# ralphapp<-abs(rmarginals[-1:-2,grepl("rPP_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rPP_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))])
# ralpha<-abs(rmarginals[-1:-2,]-rsynmarginals[-1:-2,])
# ralpha<-data.frame(t(ralpha))
# ralpha<-cbind(rownames(ralpha),ralpha)
# ralpha$`rownames(ralpha)`[grepl(".1",ralpha$`rownames(ralpha)`)]<-substr(ralpha$`rownames(ralpha)`[grepl(".1",ralpha$`rownames(ralpha)`)],1,nchar(ralpha$`rownames(ralpha)`[grepl(".1",ralpha$`rownames(ralpha)`)])-2)
# ralpha$`rownames(ralpha)`[grepl(".2",ralpha$`rownames(ralpha)`)]<-substr(ralpha$`rownames(ralpha)`[grepl(".2",ralpha$`rownames(ralpha)`)],1,nchar(ralpha$`rownames(ralpha)`[grepl(".2",ralpha$`rownames(ralpha)`)])-2)
# ralpha$`rownames(ralpha)`[grepl(".3",ralpha$`rownames(ralpha)`)]<-substr(ralpha$`rownames(ralpha)`[grepl(".3",ralpha$`rownames(ralpha)`)],1,nchar(ralpha$`rownames(ralpha)`[grepl(".3",ralpha$`rownames(ralpha)`)])-2)
# ralpha$`rownames(ralpha)`[grepl(".4",ralpha$`rownames(ralpha)`)]<-substr(ralpha$`rownames(ralpha)`[grepl(".4",ralpha$`rownames(ralpha)`)],1,nchar(ralpha$`rownames(ralpha)`[grepl(".4",ralpha$`rownames(ralpha)`)])-2)
# ralpha<-aggregate(ralpha$X3, by=list(ralpha$`rownames(ralpha)`), FUN=sum)
# names(ralpha)<-c("Variable","S1-bCMA")
# ralpha<-ralpha[!grepl("ID",ralpha$Variable),]
# #REGION relative absolute errors on households, persons and agents per category (rrelalpha)
# rrelalpha<-ralpha
# rrelalpha[,2]<-round(ralpha[,2]*100/rmarginals$rHH_ID[-1:-2],2)
# #Results
# Results1<-ralpha
# Results2<-rrelalpha
# Results3<-data.frame(Scenario, 
#                     Iph, Ipp, 
#                     rgammahh, rgammapp, rgammatot,
#                     rrelgammahh, rrelgammapp, rrelgammatot,
#                     rbetahh, rbetapp, rbetatot,
#                     rrelbetahh, rrelbetapp, rrelbetatot
# )
# 
# #S2-bCSD
# #Import Results
# rsynmarginals<-read.csv("Exported/A1/3. Analysis/Toronto/S2-bCSD/summary_region.csv")
# rsynmarginals[-1:-2,][is.na(rsynmarginals[-1:-2,])]<-0
# rsynmarginals<-rsynmarginals[,grepl("r",names(rsynmarginals))]
# temp<-rsynmarginals[1:3,]
# temp[3,]<-colSums(rsynmarginals[-1:-2,])
# rsynmarginals<-temp
# rsynmarginals<-rsynmarginals[match(names(rmarginals),names(rsynmarginals))]
# ghhmarginals<-read.csv("Exported/A1/3. Analysis/Toronto/S2-bCSD/geo_household_marginals.csv")
# gppmarginals<-read.csv("Exported/A1/3. Analysis/Toronto/S2-bCSD/geo_person_marginals.csv")
# gmarginals<-cbind(ghhmarginals,gppmarginals[,-1])
# gsynmarginals<-read.csv("Exported/A1/3. Analysis/Toronto/S2-bCSD/summary_geo.csv")
# gsynmarginals<-gsynmarginals[,!grepl("r",names(gsynmarginals))]
# gmarginals<-gmarginals[,names(gmarginals)%in%names(gsynmarginals)]
# gsynmarginals<-gsynmarginals[match(names(gmarginals),names(gsynmarginals))]
# #Scenario
# Scenario<-"S2-bCSD"
# #Ip
# Iph<-round(sum(as.numeric(ghhmarginals$HH_ID[-1:-2])*as.numeric(ghhmarginals$HH_ID[-1:-2])*100/rhhmarginals$rHH_ID[-1:-2])/(sum(ghhmarginals$HH_ID[-1:-2])),2)
# Ipp<-round(sum(as.numeric(gppmarginals$PP_ID[-1:-2])*as.numeric(gppmarginals$PP_ID[-1:-2])*100/rppmarginals$rPP_ID[-1:-2])/(sum(gppmarginals$PP_ID[-1:-2])),2)
# #REGION absolute differences on total households, persons and agents synthesized (gamma)
# rgammahh<-abs(rmarginals$rHH_ID[-1:-2]-rsynmarginals$rHH_ID[-1:-2])
# rgammapp<-abs(rmarginals$rPP_ID[-1:-2]-rsynmarginals$rPP_ID[-1:-2])
# rgammatot<-rgammahh + rgammapp
# #REGION relative differences on total households, persons and agents synthesized (relgamma)
# rrelgammahh<-round(abs(rmarginals$rHH_ID[-1:-2]-rsynmarginals$rHH_ID[-1:-2])*100/rmarginals$rHH_ID[-1:-2],5)
# rrelgammapp<-round(abs(rmarginals$rPP_ID[-1:-2]-rsynmarginals$rPP_ID[-1:-2])*100/rmarginals$rPP_ID[-1:-2],5)
# rrelgammatot<-round(rgammatot*100/(rmarginals$rHH_ID[-1:-2]+rmarginals$rPP_ID[-1:-2]),5)
# #REGION total absolute errors (beta)
# rbetahh<-sum(abs(rmarginals[-1:-2,grepl("rHH_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rHH_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))]))
# rbetapp<-sum(abs(rmarginals[-1:-2,grepl("rPP_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rPP_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))]))
# rbetatot<-rbetahh + rbetapp
# #REGION relative total absolute errors (rrelbeta)
# rrelbetahh<-round(sum(abs(rmarginals[-1:-2,grepl("rHH_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rHH_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))]))*100/rmarginals$rHH_ID[-1:-2],2)
# rrelbetapp<-round(sum(abs(rmarginals[-1:-2,grepl("rPP_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rPP_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))]))*100/rmarginals$rPP_ID[-1:-2],2)
# rrelbetatot<-round((sum(abs(rmarginals[-1:-2,grepl("rHH_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rHH_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))])) + sum(abs(rmarginals[-1:-2,grepl("rPP_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rPP_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))])))*100/(rmarginals$rHH_ID[-1:-2] + rmarginals$rPP_ID[-1:-2]),2)
# #REGION absolute errors on households, persons and agents per category (alpha)
# ralphahh<-abs(rmarginals[-1:-2,grepl("rHH_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rHH_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))])
# ralphapp<-abs(rmarginals[-1:-2,grepl("rPP_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rPP_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))])
# ralpha<-abs(rmarginals[-1:-2,]-rsynmarginals[-1:-2,])
# ralpha<-data.frame(t(ralpha))
# ralpha<-cbind(rownames(ralpha),ralpha)
# ralpha$`rownames(ralpha)`[grepl(".1",ralpha$`rownames(ralpha)`)]<-substr(ralpha$`rownames(ralpha)`[grepl(".1",ralpha$`rownames(ralpha)`)],1,nchar(ralpha$`rownames(ralpha)`[grepl(".1",ralpha$`rownames(ralpha)`)])-2)
# ralpha$`rownames(ralpha)`[grepl(".2",ralpha$`rownames(ralpha)`)]<-substr(ralpha$`rownames(ralpha)`[grepl(".2",ralpha$`rownames(ralpha)`)],1,nchar(ralpha$`rownames(ralpha)`[grepl(".2",ralpha$`rownames(ralpha)`)])-2)
# ralpha$`rownames(ralpha)`[grepl(".3",ralpha$`rownames(ralpha)`)]<-substr(ralpha$`rownames(ralpha)`[grepl(".3",ralpha$`rownames(ralpha)`)],1,nchar(ralpha$`rownames(ralpha)`[grepl(".3",ralpha$`rownames(ralpha)`)])-2)
# ralpha$`rownames(ralpha)`[grepl(".4",ralpha$`rownames(ralpha)`)]<-substr(ralpha$`rownames(ralpha)`[grepl(".4",ralpha$`rownames(ralpha)`)],1,nchar(ralpha$`rownames(ralpha)`[grepl(".4",ralpha$`rownames(ralpha)`)])-2)
# ralpha<-aggregate(ralpha$X3, by=list(ralpha$`rownames(ralpha)`), FUN=sum)
# names(ralpha)<-c("Variable","S2-bCSD")
# ralpha<-ralpha[!grepl("ID",ralpha$Variable),]
# #REGION relative absolute errors on households, persons and agents per category (rrelalpha)
# rrelalpha<-ralpha
# rrelalpha[,2]<-round(ralpha[,2]*100/rmarginals$rHH_ID[-1:-2],2)
# #Results
# Results1<-cbind(Results1,"S2-bCSD"=ralpha[,2])
# Results2<-cbind(Results2,"S2-bCSD"=rrelalpha[,2])
# Results3<-rbind(Results3,data.frame(Scenario, 
#                     Iph, Ipp, 
#                     rgammahh, rgammapp, rgammatot,
#                     rrelgammahh, rrelgammapp, rrelgammatot,
#                     rbetahh, rbetapp, rbetatot,
#                     rrelbetahh, rrelbetapp, rrelbetatot
# ))
# 
# #S3-bCMA_CSD
# #Import Results
# rsynmarginals<-read.csv("Exported/A1/3. Analysis/Toronto/S3-bCMA_CSD/summary_region.csv")
# rsynmarginals[-1:-2,][is.na(rsynmarginals[-1:-2,])]<-0
# rsynmarginals<-rsynmarginals[,grepl("r",names(rsynmarginals))]
# rsynmarginals<-rsynmarginals[match(names(rmarginals),names(rsynmarginals))]
# ghhmarginals<-read.csv("Exported/A1/3. Analysis/Toronto/S3-bCMA_CSD/geo_household_marginals.csv")
# gppmarginals<-read.csv("Exported/A1/3. Analysis/Toronto/S3-bCMA_CSD/geo_person_marginals.csv")
# gmarginals<-cbind(ghhmarginals,gppmarginals[,-1])
# gsynmarginals<-read.csv("Exported/A1/3. Analysis/Toronto/S3-bCMA_CSD/summary_geo.csv")
# gsynmarginals<-gsynmarginals[,!grepl("r",names(gsynmarginals))]
# gmarginals<-gmarginals[,names(gmarginals)%in%names(gsynmarginals)]
# gsynmarginals<-gsynmarginals[match(names(gmarginals),names(gsynmarginals))]
# #Scenario
# Scenario<-"S3-bCMA_CSD"
# #Ip
# Iph<-round(sum(as.numeric(ghhmarginals$HH_ID[-1:-2])*as.numeric(ghhmarginals$HH_ID[-1:-2])*100/rhhmarginals$rHH_ID[-1:-2])/(sum(ghhmarginals$HH_ID[-1:-2])),2)
# Ipp<-round(sum(as.numeric(gppmarginals$PP_ID[-1:-2])*as.numeric(gppmarginals$PP_ID[-1:-2])*100/rppmarginals$rPP_ID[-1:-2])/(sum(gppmarginals$PP_ID[-1:-2])),2)
# #REGION absolute differences on total households, persons and agents synthesized (gamma)
# rgammahh<-abs(rmarginals$rHH_ID[-1:-2]-rsynmarginals$rHH_ID[-1:-2])
# rgammapp<-abs(rmarginals$rPP_ID[-1:-2]-rsynmarginals$rPP_ID[-1:-2])
# rgammatot<-rgammahh + rgammapp
# #REGION relative differences on total households, persons and agents synthesized (relgamma)
# rrelgammahh<-round(abs(rmarginals$rHH_ID[-1:-2]-rsynmarginals$rHH_ID[-1:-2])*100/rmarginals$rHH_ID[-1:-2],5)
# rrelgammapp<-round(abs(rmarginals$rPP_ID[-1:-2]-rsynmarginals$rPP_ID[-1:-2])*100/rmarginals$rPP_ID[-1:-2],5)
# rrelgammatot<-round(rgammatot*100/(rmarginals$rHH_ID[-1:-2]+rmarginals$rPP_ID[-1:-2]),5)
# #REGION total absolute errors (beta)
# rbetahh<-sum(abs(rmarginals[-1:-2,grepl("rHH_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rHH_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))]))
# rbetapp<-sum(abs(rmarginals[-1:-2,grepl("rPP_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rPP_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))]))
# rbetatot<-rbetahh + rbetapp
# #REGION relative total absolute errors (rrelbeta)
# rrelbetahh<-round(sum(abs(rmarginals[-1:-2,grepl("rHH_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rHH_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))]))*100/rmarginals$rHH_ID[-1:-2],2)
# rrelbetapp<-round(sum(abs(rmarginals[-1:-2,grepl("rPP_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rPP_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))]))*100/rmarginals$rPP_ID[-1:-2],2)
# rrelbetatot<-round((sum(abs(rmarginals[-1:-2,grepl("rHH_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rHH_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))])) + sum(abs(rmarginals[-1:-2,grepl("rPP_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rPP_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))])))*100/(rmarginals$rHH_ID[-1:-2] + rmarginals$rPP_ID[-1:-2]),2)
# #REGION absolute errors on households, persons and agents per category (alpha)
# ralphahh<-abs(rmarginals[-1:-2,grepl("rHH_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rHH_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))])
# ralphapp<-abs(rmarginals[-1:-2,grepl("rPP_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rPP_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))])
# ralpha<-abs(rmarginals[-1:-2,]-rsynmarginals[-1:-2,])
# ralpha<-data.frame(t(ralpha))
# ralpha<-cbind(rownames(ralpha),ralpha)
# ralpha$`rownames(ralpha)`[grepl(".1",ralpha$`rownames(ralpha)`)]<-substr(ralpha$`rownames(ralpha)`[grepl(".1",ralpha$`rownames(ralpha)`)],1,nchar(ralpha$`rownames(ralpha)`[grepl(".1",ralpha$`rownames(ralpha)`)])-2)
# ralpha$`rownames(ralpha)`[grepl(".2",ralpha$`rownames(ralpha)`)]<-substr(ralpha$`rownames(ralpha)`[grepl(".2",ralpha$`rownames(ralpha)`)],1,nchar(ralpha$`rownames(ralpha)`[grepl(".2",ralpha$`rownames(ralpha)`)])-2)
# ralpha$`rownames(ralpha)`[grepl(".3",ralpha$`rownames(ralpha)`)]<-substr(ralpha$`rownames(ralpha)`[grepl(".3",ralpha$`rownames(ralpha)`)],1,nchar(ralpha$`rownames(ralpha)`[grepl(".3",ralpha$`rownames(ralpha)`)])-2)
# ralpha$`rownames(ralpha)`[grepl(".4",ralpha$`rownames(ralpha)`)]<-substr(ralpha$`rownames(ralpha)`[grepl(".4",ralpha$`rownames(ralpha)`)],1,nchar(ralpha$`rownames(ralpha)`[grepl(".4",ralpha$`rownames(ralpha)`)])-2)
# ralpha<-aggregate(ralpha$X3, by=list(ralpha$`rownames(ralpha)`), FUN=sum)
# names(ralpha)<-c("Variable","S3-bCMA_CSD")
# ralpha<-ralpha[!grepl("ID",ralpha$Variable),]
# #REGION relative absolute errors on households, persons and agents per category (rrelalpha)
# rrelalpha<-ralpha
# rrelalpha[,2]<-round(ralpha[,2]*100/rmarginals$rHH_ID[-1:-2],2)
# #Results
# Results1<-cbind(Results1,"S3-bCMA_CSD"=ralpha[,2])
# Results2<-cbind(Results2,"S3-bCMA_CSD"=rrelalpha[,2])
# Results3<-rbind(Results3,data.frame(Scenario, 
#                                     Iph, Ipp, 
#                                     rgammahh, rgammapp, rgammatot,
#                                     rrelgammahh, rrelgammapp, rrelgammatot,
#                                     rbetahh, rbetapp, rbetatot,
#                                     rrelbetahh, rrelbetapp, rrelbetatot
# ))
# 
# #S4-bADA
# #Import Results
# rsynmarginals<-read.csv("Exported/A1/3. Analysis/Toronto/S4-bADA/summary_region.csv")
# rsynmarginals[-1:-2,][is.na(rsynmarginals[-1:-2,])]<-0
# rsynmarginals<-rsynmarginals[,grepl("r",names(rsynmarginals))]
# temp<-rsynmarginals[1:3,]
# temp[3,]<-colSums(rsynmarginals[-1:-2,])
# rsynmarginals<-temp
# rsynmarginals<-rsynmarginals[match(names(rmarginals),names(rsynmarginals))]
# ghhmarginals<-read.csv("Exported/A1/3. Analysis/Toronto/S4-bADA/geo_household_marginals.csv")
# gppmarginals<-read.csv("Exported/A1/3. Analysis/Toronto/S4-bADA/geo_person_marginals.csv")
# gmarginals<-cbind(ghhmarginals,gppmarginals[,-1])
# gsynmarginals<-read.csv("Exported/A1/3. Analysis/Toronto/S4-bADA/summary_geo.csv")
# gsynmarginals<-gsynmarginals[,!grepl("r",names(gsynmarginals))]
# gmarginals<-gmarginals[,names(gmarginals)%in%names(gsynmarginals)]
# gsynmarginals<-gsynmarginals[match(names(gmarginals),names(gsynmarginals))]
# #Scenario
# Scenario<-"S4-bADA"
# #Ip
# Iph<-round(sum(as.numeric(ghhmarginals$HH_ID[-1:-2])*as.numeric(ghhmarginals$HH_ID[-1:-2])*100/rhhmarginals$rHH_ID[-1:-2])/(sum(ghhmarginals$HH_ID[-1:-2])),2)
# Ipp<-round(sum(as.numeric(gppmarginals$PP_ID[-1:-2])*as.numeric(gppmarginals$PP_ID[-1:-2])*100/rppmarginals$rPP_ID[-1:-2])/(sum(gppmarginals$PP_ID[-1:-2])),2)
# #REGION absolute differences on total households, persons and agents synthesized (gamma)
# rgammahh<-abs(rmarginals$rHH_ID[-1:-2]-rsynmarginals$rHH_ID[-1:-2])
# rgammapp<-abs(rmarginals$rPP_ID[-1:-2]-rsynmarginals$rPP_ID[-1:-2])
# rgammatot<-rgammahh + rgammapp
# #REGION relative differences on total households, persons and agents synthesized (relgamma)
# rrelgammahh<-round(abs(rmarginals$rHH_ID[-1:-2]-rsynmarginals$rHH_ID[-1:-2])*100/rmarginals$rHH_ID[-1:-2],5)
# rrelgammapp<-round(abs(rmarginals$rPP_ID[-1:-2]-rsynmarginals$rPP_ID[-1:-2])*100/rmarginals$rPP_ID[-1:-2],5)
# rrelgammatot<-round(rgammatot*100/(rmarginals$rHH_ID[-1:-2]+rmarginals$rPP_ID[-1:-2]),5)
# #REGION total absolute errors (beta)
# rbetahh<-sum(abs(rmarginals[-1:-2,grepl("rHH_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rHH_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))]))
# rbetapp<-sum(abs(rmarginals[-1:-2,grepl("rPP_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rPP_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))]))
# rbetatot<-rbetahh + rbetapp
# #REGION relative total absolute errors (rrelbeta)
# rrelbetahh<-round(sum(abs(rmarginals[-1:-2,grepl("rHH_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rHH_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))]))*100/rmarginals$rHH_ID[-1:-2],2)
# rrelbetapp<-round(sum(abs(rmarginals[-1:-2,grepl("rPP_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rPP_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))]))*100/rmarginals$rPP_ID[-1:-2],2)
# rrelbetatot<-round((sum(abs(rmarginals[-1:-2,grepl("rHH_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rHH_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))])) + sum(abs(rmarginals[-1:-2,grepl("rPP_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rPP_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))])))*100/(rmarginals$rHH_ID[-1:-2] + rmarginals$rPP_ID[-1:-2]),2)
# #REGION absolute errors on households, persons and agents per category (alpha)
# ralphahh<-abs(rmarginals[-1:-2,grepl("rHH_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rHH_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))])
# ralphapp<-abs(rmarginals[-1:-2,grepl("rPP_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rPP_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))])
# ralpha<-abs(rmarginals[-1:-2,]-rsynmarginals[-1:-2,])
# ralpha<-data.frame(t(ralpha))
# ralpha<-cbind(rownames(ralpha),ralpha)
# ralpha$`rownames(ralpha)`[grepl(".1",ralpha$`rownames(ralpha)`)]<-substr(ralpha$`rownames(ralpha)`[grepl(".1",ralpha$`rownames(ralpha)`)],1,nchar(ralpha$`rownames(ralpha)`[grepl(".1",ralpha$`rownames(ralpha)`)])-2)
# ralpha$`rownames(ralpha)`[grepl(".2",ralpha$`rownames(ralpha)`)]<-substr(ralpha$`rownames(ralpha)`[grepl(".2",ralpha$`rownames(ralpha)`)],1,nchar(ralpha$`rownames(ralpha)`[grepl(".2",ralpha$`rownames(ralpha)`)])-2)
# ralpha$`rownames(ralpha)`[grepl(".3",ralpha$`rownames(ralpha)`)]<-substr(ralpha$`rownames(ralpha)`[grepl(".3",ralpha$`rownames(ralpha)`)],1,nchar(ralpha$`rownames(ralpha)`[grepl(".3",ralpha$`rownames(ralpha)`)])-2)
# ralpha$`rownames(ralpha)`[grepl(".4",ralpha$`rownames(ralpha)`)]<-substr(ralpha$`rownames(ralpha)`[grepl(".4",ralpha$`rownames(ralpha)`)],1,nchar(ralpha$`rownames(ralpha)`[grepl(".4",ralpha$`rownames(ralpha)`)])-2)
# ralpha<-aggregate(ralpha$X3, by=list(ralpha$`rownames(ralpha)`), FUN=sum)
# names(ralpha)<-c("Variable","S4-bADA")
# ralpha<-ralpha[!grepl("ID",ralpha$Variable),]
# #REGION relative absolute errors on households, persons and agents per category (rrelalpha)
# rrelalpha<-ralpha
# rrelalpha[,2]<-round(ralpha[,2]*100/rmarginals$rHH_ID[-1:-2],2)
# #Results
# Results1<-cbind(Results1,"S4-bADA"=ralpha[,2])
# Results2<-cbind(Results2,"S4-bADA"=rrelalpha[,2])
# Results3<-rbind(Results3,data.frame(Scenario, 
#                                     Iph, Ipp, 
#                                     rgammahh, rgammapp, rgammatot,
#                                     rrelgammahh, rrelgammapp, rrelgammatot,
#                                     rbetahh, rbetapp, rbetatot,
#                                     rrelbetahh, rrelbetapp, rrelbetatot
# ))
# 
# #S5-bCMA_ADA
# #Import Results
# rsynmarginals<-read.csv("Exported/A1/3. Analysis/Toronto/S5-bCMA_ADA/summary_region.csv")
# rsynmarginals[-1:-2,][is.na(rsynmarginals[-1:-2,])]<-0
# rsynmarginals<-rsynmarginals[,grepl("r",names(rsynmarginals))]
# rsynmarginals<-rsynmarginals[match(names(rmarginals),names(rsynmarginals))]
# ghhmarginals<-read.csv("Exported/A1/3. Analysis/Toronto/S5-bCMA_ADA/geo_household_marginals.csv")
# gppmarginals<-read.csv("Exported/A1/3. Analysis/Toronto/S5-bCMA_ADA/geo_person_marginals.csv")
# gmarginals<-cbind(ghhmarginals,gppmarginals[,-1])
# gsynmarginals<-read.csv("Exported/A1/3. Analysis/Toronto/S5-bCMA_ADA/summary_geo.csv")
# gsynmarginals<-gsynmarginals[,!grepl("r",names(gsynmarginals))]
# gmarginals<-gmarginals[,names(gmarginals)%in%names(gsynmarginals)]
# gsynmarginals<-gsynmarginals[match(names(gmarginals),names(gsynmarginals))]
# #Scenario
# Scenario<-"S5-bCMA_ADA"
# #Ip
# Iph<-round(sum(as.numeric(ghhmarginals$HH_ID[-1:-2])*as.numeric(ghhmarginals$HH_ID[-1:-2])*100/rhhmarginals$rHH_ID[-1:-2])/(sum(ghhmarginals$HH_ID[-1:-2])),2)
# Ipp<-round(sum(as.numeric(gppmarginals$PP_ID[-1:-2])*as.numeric(gppmarginals$PP_ID[-1:-2])*100/rppmarginals$rPP_ID[-1:-2])/(sum(gppmarginals$PP_ID[-1:-2])),2)
# #REGION absolute differences on total households, persons and agents synthesized (gamma)
# rgammahh<-abs(rmarginals$rHH_ID[-1:-2]-rsynmarginals$rHH_ID[-1:-2])
# rgammapp<-abs(rmarginals$rPP_ID[-1:-2]-rsynmarginals$rPP_ID[-1:-2])
# rgammatot<-rgammahh + rgammapp
# #REGION relative differences on total households, persons and agents synthesized (relgamma)
# rrelgammahh<-round(abs(rmarginals$rHH_ID[-1:-2]-rsynmarginals$rHH_ID[-1:-2])*100/rmarginals$rHH_ID[-1:-2],5)
# rrelgammapp<-round(abs(rmarginals$rPP_ID[-1:-2]-rsynmarginals$rPP_ID[-1:-2])*100/rmarginals$rPP_ID[-1:-2],5)
# rrelgammatot<-round(rgammatot*100/(rmarginals$rHH_ID[-1:-2]+rmarginals$rPP_ID[-1:-2]),5)
# #REGION total absolute errors (beta)
# rbetahh<-sum(abs(rmarginals[-1:-2,grepl("rHH_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rHH_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))]))
# rbetapp<-sum(abs(rmarginals[-1:-2,grepl("rPP_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rPP_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))]))
# rbetatot<-rbetahh + rbetapp
# #REGION relative total absolute errors (rrelbeta)
# rrelbetahh<-round(sum(abs(rmarginals[-1:-2,grepl("rHH_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rHH_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))]))*100/rmarginals$rHH_ID[-1:-2],2)
# rrelbetapp<-round(sum(abs(rmarginals[-1:-2,grepl("rPP_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rPP_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))]))*100/rmarginals$rPP_ID[-1:-2],2)
# rrelbetatot<-round((sum(abs(rmarginals[-1:-2,grepl("rHH_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rHH_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))])) + sum(abs(rmarginals[-1:-2,grepl("rPP_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rPP_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))])))*100/(rmarginals$rHH_ID[-1:-2] + rmarginals$rPP_ID[-1:-2]),2)
# #REGION absolute errors on households, persons and agents per category (alpha)
# ralphahh<-abs(rmarginals[-1:-2,grepl("rHH_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rHH_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))])
# ralphapp<-abs(rmarginals[-1:-2,grepl("rPP_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rPP_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))])
# ralpha<-abs(rmarginals[-1:-2,]-rsynmarginals[-1:-2,])
# ralpha<-data.frame(t(ralpha))
# ralpha<-cbind(rownames(ralpha),ralpha)
# ralpha$`rownames(ralpha)`[grepl(".1",ralpha$`rownames(ralpha)`)]<-substr(ralpha$`rownames(ralpha)`[grepl(".1",ralpha$`rownames(ralpha)`)],1,nchar(ralpha$`rownames(ralpha)`[grepl(".1",ralpha$`rownames(ralpha)`)])-2)
# ralpha$`rownames(ralpha)`[grepl(".2",ralpha$`rownames(ralpha)`)]<-substr(ralpha$`rownames(ralpha)`[grepl(".2",ralpha$`rownames(ralpha)`)],1,nchar(ralpha$`rownames(ralpha)`[grepl(".2",ralpha$`rownames(ralpha)`)])-2)
# ralpha$`rownames(ralpha)`[grepl(".3",ralpha$`rownames(ralpha)`)]<-substr(ralpha$`rownames(ralpha)`[grepl(".3",ralpha$`rownames(ralpha)`)],1,nchar(ralpha$`rownames(ralpha)`[grepl(".3",ralpha$`rownames(ralpha)`)])-2)
# ralpha$`rownames(ralpha)`[grepl(".4",ralpha$`rownames(ralpha)`)]<-substr(ralpha$`rownames(ralpha)`[grepl(".4",ralpha$`rownames(ralpha)`)],1,nchar(ralpha$`rownames(ralpha)`[grepl(".4",ralpha$`rownames(ralpha)`)])-2)
# ralpha<-aggregate(ralpha$X3, by=list(ralpha$`rownames(ralpha)`), FUN=sum)
# names(ralpha)<-c("Variable","S5-bCMA_ADA")
# ralpha<-ralpha[!grepl("ID",ralpha$Variable),]
# #REGION relative absolute errors on households, persons and agents per category (rrelalpha)
# rrelalpha<-ralpha
# rrelalpha[,2]<-round(ralpha[,2]*100/rmarginals$rHH_ID[-1:-2],2)
# #Results
# Results1<-cbind(Results1,"S5-bCMA_ADA"=ralpha[,2])
# Results2<-cbind(Results2,"S5-bCMA_ADA"=rrelalpha[,2])
# Results3<-rbind(Results3,data.frame(Scenario, 
#                                     Iph, Ipp, 
#                                     rgammahh, rgammapp, rgammatot,
#                                     rrelgammahh, rrelgammapp, rrelgammatot,
#                                     rbetahh, rbetapp, rbetatot,
#                                     rrelbetahh, rrelbetapp, rrelbetatot
# ))
# 
# #S6-bCT
# #Import Results
# rsynmarginals<-read.csv("Exported/A1/3. Analysis/Toronto/S6-bCT/summary_region.csv")
# rsynmarginals[-1:-2,][is.na(rsynmarginals[-1:-2,])]<-0
# rsynmarginals<-rsynmarginals[,grepl("r",names(rsynmarginals))]
# temp<-rsynmarginals[1:3,]
# temp[3,]<-colSums(rsynmarginals[-1:-2,])
# rsynmarginals<-temp
# rsynmarginals<-rsynmarginals[match(names(rmarginals),names(rsynmarginals))]
# ghhmarginals<-read.csv("Exported/A1/3. Analysis/Toronto/S6-bCT/geo_household_marginals.csv")
# gppmarginals<-read.csv("Exported/A1/3. Analysis/Toronto/S6-bCT/geo_person_marginals.csv")
# gmarginals<-cbind(ghhmarginals,gppmarginals[,-1])
# gsynmarginals<-read.csv("Exported/A1/3. Analysis/Toronto/S6-bCT/summary_geo.csv")
# gsynmarginals<-gsynmarginals[,!grepl("r",names(gsynmarginals))]
# gmarginals<-gmarginals[,names(gmarginals)%in%names(gsynmarginals)]
# gsynmarginals<-gsynmarginals[match(names(gmarginals),names(gsynmarginals))]
# #Scenario
# Scenario<-"S6-bCT"
# #Ip
# Iph<-round(sum(as.numeric(ghhmarginals$HH_ID[-1:-2])*as.numeric(ghhmarginals$HH_ID[-1:-2])*100/rhhmarginals$rHH_ID[-1:-2])/(sum(ghhmarginals$HH_ID[-1:-2])),2)
# Ipp<-round(sum(as.numeric(gppmarginals$PP_ID[-1:-2])*as.numeric(gppmarginals$PP_ID[-1:-2])*100/rppmarginals$rPP_ID[-1:-2])/(sum(gppmarginals$PP_ID[-1:-2])),2)
# #REGION absolute differences on total households, persons and agents synthesized (gamma)
# rgammahh<-abs(rmarginals$rHH_ID[-1:-2]-rsynmarginals$rHH_ID[-1:-2])
# rgammapp<-abs(rmarginals$rPP_ID[-1:-2]-rsynmarginals$rPP_ID[-1:-2])
# rgammatot<-rgammahh + rgammapp
# #REGION relative differences on total households, persons and agents synthesized (relgamma)
# rrelgammahh<-round(abs(rmarginals$rHH_ID[-1:-2]-rsynmarginals$rHH_ID[-1:-2])*100/rmarginals$rHH_ID[-1:-2],5)
# rrelgammapp<-round(abs(rmarginals$rPP_ID[-1:-2]-rsynmarginals$rPP_ID[-1:-2])*100/rmarginals$rPP_ID[-1:-2],5)
# rrelgammatot<-round(rgammatot*100/(rmarginals$rHH_ID[-1:-2]+rmarginals$rPP_ID[-1:-2]),5)
# #REGION total absolute errors (beta)
# rbetahh<-sum(abs(rmarginals[-1:-2,grepl("rHH_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rHH_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))]))
# rbetapp<-sum(abs(rmarginals[-1:-2,grepl("rPP_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rPP_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))]))
# rbetatot<-rbetahh + rbetapp
# #REGION relative total absolute errors (rrelbeta)
# rrelbetahh<-round(sum(abs(rmarginals[-1:-2,grepl("rHH_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rHH_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))]))*100/rmarginals$rHH_ID[-1:-2],2)
# rrelbetapp<-round(sum(abs(rmarginals[-1:-2,grepl("rPP_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rPP_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))]))*100/rmarginals$rPP_ID[-1:-2],2)
# rrelbetatot<-round((sum(abs(rmarginals[-1:-2,grepl("rHH_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rHH_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))])) + sum(abs(rmarginals[-1:-2,grepl("rPP_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rPP_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))])))*100/(rmarginals$rHH_ID[-1:-2] + rmarginals$rPP_ID[-1:-2]),2)
# #REGION absolute errors on households, persons and agents per category (alpha)
# ralphahh<-abs(rmarginals[-1:-2,grepl("rHH_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rHH_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))])
# ralphapp<-abs(rmarginals[-1:-2,grepl("rPP_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rPP_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))])
# ralpha<-abs(rmarginals[-1:-2,]-rsynmarginals[-1:-2,])
# ralpha<-data.frame(t(ralpha))
# ralpha<-cbind(rownames(ralpha),ralpha)
# ralpha$`rownames(ralpha)`[grepl(".1",ralpha$`rownames(ralpha)`)]<-substr(ralpha$`rownames(ralpha)`[grepl(".1",ralpha$`rownames(ralpha)`)],1,nchar(ralpha$`rownames(ralpha)`[grepl(".1",ralpha$`rownames(ralpha)`)])-2)
# ralpha$`rownames(ralpha)`[grepl(".2",ralpha$`rownames(ralpha)`)]<-substr(ralpha$`rownames(ralpha)`[grepl(".2",ralpha$`rownames(ralpha)`)],1,nchar(ralpha$`rownames(ralpha)`[grepl(".2",ralpha$`rownames(ralpha)`)])-2)
# ralpha$`rownames(ralpha)`[grepl(".3",ralpha$`rownames(ralpha)`)]<-substr(ralpha$`rownames(ralpha)`[grepl(".3",ralpha$`rownames(ralpha)`)],1,nchar(ralpha$`rownames(ralpha)`[grepl(".3",ralpha$`rownames(ralpha)`)])-2)
# ralpha$`rownames(ralpha)`[grepl(".4",ralpha$`rownames(ralpha)`)]<-substr(ralpha$`rownames(ralpha)`[grepl(".4",ralpha$`rownames(ralpha)`)],1,nchar(ralpha$`rownames(ralpha)`[grepl(".4",ralpha$`rownames(ralpha)`)])-2)
# ralpha<-aggregate(ralpha$X3, by=list(ralpha$`rownames(ralpha)`), FUN=sum)
# names(ralpha)<-c("Variable","S6-bCT")
# ralpha<-ralpha[!grepl("ID",ralpha$Variable),]
# #REGION relative absolute errors on households, persons and agents per category (rrelalpha)
# rrelalpha<-ralpha
# rrelalpha[,2]<-round(ralpha[,2]*100/rmarginals$rHH_ID[-1:-2],2)
# #Results
# Results1<-cbind(Results1,"S6-bCT"=ralpha[,2])
# Results2<-cbind(Results2,"S6-bCT"=rrelalpha[,2])
# Results3<-rbind(Results3,data.frame(Scenario, 
#                                     Iph, Ipp, 
#                                     rgammahh, rgammapp, rgammatot,
#                                     rrelgammahh, rrelgammapp, rrelgammatot,
#                                     rbetahh, rbetapp, rbetatot,
#                                     rrelbetahh, rrelbetapp, rrelbetatot
# ))
# 
# #S7-bCMA_CT
# #Import Results
# rsynmarginals<-read.csv("Exported/A1/3. Analysis/Toronto/S7-bCMA_CT/summary_region.csv")
# rsynmarginals[-1:-2,][is.na(rsynmarginals[-1:-2,])]<-0
# rsynmarginals<-rsynmarginals[,grepl("r",names(rsynmarginals))]
# rsynmarginals<-rsynmarginals[match(names(rmarginals),names(rsynmarginals))]
# ghhmarginals<-read.csv("Exported/A1/3. Analysis/Toronto/S7-bCMA_CT/geo_household_marginals.csv")
# gppmarginals<-read.csv("Exported/A1/3. Analysis/Toronto/S7-bCMA_CT/geo_person_marginals.csv")
# gmarginals<-cbind(ghhmarginals,gppmarginals[,-1])
# gsynmarginals<-read.csv("Exported/A1/3. Analysis/Toronto/S7-bCMA_CT/summary_geo.csv")
# gsynmarginals<-gsynmarginals[,!grepl("r",names(gsynmarginals))]
# gmarginals<-gmarginals[,names(gmarginals)%in%names(gsynmarginals)]
# gsynmarginals<-gsynmarginals[match(names(gmarginals),names(gsynmarginals))]
# #Scenario
# Scenario<-"S7-bCMA_CT"
# #Ip
# Iph<-round(sum(as.numeric(ghhmarginals$HH_ID[-1:-2])*as.numeric(ghhmarginals$HH_ID[-1:-2])*100/rhhmarginals$rHH_ID[-1:-2])/(sum(ghhmarginals$HH_ID[-1:-2])),2)
# Ipp<-round(sum(as.numeric(gppmarginals$PP_ID[-1:-2])*as.numeric(gppmarginals$PP_ID[-1:-2])*100/rppmarginals$rPP_ID[-1:-2])/(sum(gppmarginals$PP_ID[-1:-2])),2)
# #REGION absolute differences on total households, persons and agents synthesized (gamma)
# rgammahh<-abs(rmarginals$rHH_ID[-1:-2]-rsynmarginals$rHH_ID[-1:-2])
# rgammapp<-abs(rmarginals$rPP_ID[-1:-2]-rsynmarginals$rPP_ID[-1:-2])
# rgammatot<-rgammahh + rgammapp
# #REGION relative differences on total households, persons and agents synthesized (relgamma)
# rrelgammahh<-round(abs(rmarginals$rHH_ID[-1:-2]-rsynmarginals$rHH_ID[-1:-2])*100/rmarginals$rHH_ID[-1:-2],5)
# rrelgammapp<-round(abs(rmarginals$rPP_ID[-1:-2]-rsynmarginals$rPP_ID[-1:-2])*100/rmarginals$rPP_ID[-1:-2],5)
# rrelgammatot<-round(rgammatot*100/(rmarginals$rHH_ID[-1:-2]+rmarginals$rPP_ID[-1:-2]),5)
# #REGION total absolute errors (beta)
# rbetahh<-sum(abs(rmarginals[-1:-2,grepl("rHH_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rHH_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))]))
# rbetapp<-sum(abs(rmarginals[-1:-2,grepl("rPP_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rPP_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))]))
# rbetatot<-rbetahh + rbetapp
# #REGION relative total absolute errors (rrelbeta)
# rrelbetahh<-round(sum(abs(rmarginals[-1:-2,grepl("rHH_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rHH_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))]))*100/rmarginals$rHH_ID[-1:-2],2)
# rrelbetapp<-round(sum(abs(rmarginals[-1:-2,grepl("rPP_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rPP_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))]))*100/rmarginals$rPP_ID[-1:-2],2)
# rrelbetatot<-round((sum(abs(rmarginals[-1:-2,grepl("rHH_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rHH_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))])) + sum(abs(rmarginals[-1:-2,grepl("rPP_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rPP_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))])))*100/(rmarginals$rHH_ID[-1:-2] + rmarginals$rPP_ID[-1:-2]),2)
# #REGION absolute errors on households, persons and agents per category (alpha)
# ralphahh<-abs(rmarginals[-1:-2,grepl("rHH_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rHH_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))])
# ralphapp<-abs(rmarginals[-1:-2,grepl("rPP_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rPP_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))])
# ralpha<-abs(rmarginals[-1:-2,]-rsynmarginals[-1:-2,])
# ralpha<-data.frame(t(ralpha))
# ralpha<-cbind(rownames(ralpha),ralpha)
# ralpha$`rownames(ralpha)`[grepl(".1",ralpha$`rownames(ralpha)`)]<-substr(ralpha$`rownames(ralpha)`[grepl(".1",ralpha$`rownames(ralpha)`)],1,nchar(ralpha$`rownames(ralpha)`[grepl(".1",ralpha$`rownames(ralpha)`)])-2)
# ralpha$`rownames(ralpha)`[grepl(".2",ralpha$`rownames(ralpha)`)]<-substr(ralpha$`rownames(ralpha)`[grepl(".2",ralpha$`rownames(ralpha)`)],1,nchar(ralpha$`rownames(ralpha)`[grepl(".2",ralpha$`rownames(ralpha)`)])-2)
# ralpha$`rownames(ralpha)`[grepl(".3",ralpha$`rownames(ralpha)`)]<-substr(ralpha$`rownames(ralpha)`[grepl(".3",ralpha$`rownames(ralpha)`)],1,nchar(ralpha$`rownames(ralpha)`[grepl(".3",ralpha$`rownames(ralpha)`)])-2)
# ralpha$`rownames(ralpha)`[grepl(".4",ralpha$`rownames(ralpha)`)]<-substr(ralpha$`rownames(ralpha)`[grepl(".4",ralpha$`rownames(ralpha)`)],1,nchar(ralpha$`rownames(ralpha)`[grepl(".4",ralpha$`rownames(ralpha)`)])-2)
# ralpha<-aggregate(ralpha$X3, by=list(ralpha$`rownames(ralpha)`), FUN=sum)
# names(ralpha)<-c("Variable","S7-bCMA_CT")
# ralpha<-ralpha[!grepl("ID",ralpha$Variable),]
# #REGION relative absolute errors on households, persons and agents per category (rrelalpha)
# rrelalpha<-ralpha
# rrelalpha[,2]<-round(ralpha[,2]*100/rmarginals$rHH_ID[-1:-2],2)
# #Results
# Results1<-cbind(Results1,"S7-bCMA_CT"=ralpha[,2])
# Results2<-cbind(Results2,"S7-bCMA_CT"=rrelalpha[,2])
# Results3<-rbind(Results3,data.frame(Scenario, 
#                                     Iph, Ipp, 
#                                     rgammahh, rgammapp, rgammatot,
#                                     rrelgammahh, rrelgammapp, rrelgammatot,
#                                     rbetahh, rbetapp, rbetatot,
#                                     rrelbetahh, rrelbetapp, rrelbetatot
# ))
# 
# #S8-bDA
# #Import Results
# rsynmarginals<-read.csv("Exported/A1/3. Analysis/Toronto/S8-bDA/summary_region.csv")
# rsynmarginals[-1:-2,][is.na(rsynmarginals[-1:-2,])]<-0
# rsynmarginals<-rsynmarginals[,grepl("r",names(rsynmarginals))]
# temp<-rsynmarginals[1:3,]
# temp[3,]<-colSums(rsynmarginals[-1:-2,])
# rsynmarginals<-temp
# rsynmarginals<-rsynmarginals[match(names(rmarginals),names(rsynmarginals))]
# ghhmarginals<-read.csv("Exported/A1/3. Analysis/Toronto/S8-bDA/geo_household_marginals.csv")
# gppmarginals<-read.csv("Exported/A1/3. Analysis/Toronto/S8-bDA/geo_person_marginals.csv")
# gmarginals<-cbind(ghhmarginals,gppmarginals[,-1])
# gsynmarginals<-read.csv("Exported/A1/3. Analysis/Toronto/S8-bDA/summary_geo.csv")
# gsynmarginals<-gsynmarginals[,!grepl("r",names(gsynmarginals))]
# gmarginals<-gmarginals[,names(gmarginals)%in%names(gsynmarginals)]
# gsynmarginals<-gsynmarginals[match(names(gmarginals),names(gsynmarginals))]
# #Scenario
# Scenario<-"S8-bDA"
# #Ip
# Iph<-round(sum(as.numeric(ghhmarginals$HH_ID[-1:-2])*as.numeric(ghhmarginals$HH_ID[-1:-2])*100/rhhmarginals$rHH_ID[-1:-2])/(sum(ghhmarginals$HH_ID[-1:-2])),2)
# Ipp<-round(sum(as.numeric(gppmarginals$PP_ID[-1:-2])*as.numeric(gppmarginals$PP_ID[-1:-2])*100/rppmarginals$rPP_ID[-1:-2])/(sum(gppmarginals$PP_ID[-1:-2])),2)
# #REGION absolute differences on total households, persons and agents synthesized (gamma)
# rgammahh<-abs(rmarginals$rHH_ID[-1:-2]-rsynmarginals$rHH_ID[-1:-2])
# rgammapp<-abs(rmarginals$rPP_ID[-1:-2]-rsynmarginals$rPP_ID[-1:-2])
# rgammatot<-rgammahh + rgammapp
# #REGION relative differences on total households, persons and agents synthesized (relgamma)
# rrelgammahh<-round(abs(rmarginals$rHH_ID[-1:-2]-rsynmarginals$rHH_ID[-1:-2])*100/rmarginals$rHH_ID[-1:-2],5)
# rrelgammapp<-round(abs(rmarginals$rPP_ID[-1:-2]-rsynmarginals$rPP_ID[-1:-2])*100/rmarginals$rPP_ID[-1:-2],5)
# rrelgammatot<-round(rgammatot*100/(rmarginals$rHH_ID[-1:-2]+rmarginals$rPP_ID[-1:-2]),5)
# #REGION total absolute errors (beta)
# rbetahh<-sum(abs(rmarginals[-1:-2,grepl("rHH_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rHH_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))]))
# rbetapp<-sum(abs(rmarginals[-1:-2,grepl("rPP_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rPP_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))]))
# rbetatot<-rbetahh + rbetapp
# #REGION relative total absolute errors (rrelbeta)
# rrelbetahh<-round(sum(abs(rmarginals[-1:-2,grepl("rHH_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rHH_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))]))*100/rmarginals$rHH_ID[-1:-2],2)
# rrelbetapp<-round(sum(abs(rmarginals[-1:-2,grepl("rPP_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rPP_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))]))*100/rmarginals$rPP_ID[-1:-2],2)
# rrelbetatot<-round((sum(abs(rmarginals[-1:-2,grepl("rHH_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rHH_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))])) + sum(abs(rmarginals[-1:-2,grepl("rPP_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rPP_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))])))*100/(rmarginals$rHH_ID[-1:-2] + rmarginals$rPP_ID[-1:-2]),2)
# #REGION absolute errors on households, persons and agents per category (alpha)
# ralphahh<-abs(rmarginals[-1:-2,grepl("rHH_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rHH_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))])
# ralphapp<-abs(rmarginals[-1:-2,grepl("rPP_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rPP_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))])
# ralpha<-abs(rmarginals[-1:-2,]-rsynmarginals[-1:-2,])
# ralpha<-data.frame(t(ralpha))
# ralpha<-cbind(rownames(ralpha),ralpha)
# ralpha$`rownames(ralpha)`[grepl(".1",ralpha$`rownames(ralpha)`)]<-substr(ralpha$`rownames(ralpha)`[grepl(".1",ralpha$`rownames(ralpha)`)],1,nchar(ralpha$`rownames(ralpha)`[grepl(".1",ralpha$`rownames(ralpha)`)])-2)
# ralpha$`rownames(ralpha)`[grepl(".2",ralpha$`rownames(ralpha)`)]<-substr(ralpha$`rownames(ralpha)`[grepl(".2",ralpha$`rownames(ralpha)`)],1,nchar(ralpha$`rownames(ralpha)`[grepl(".2",ralpha$`rownames(ralpha)`)])-2)
# ralpha$`rownames(ralpha)`[grepl(".3",ralpha$`rownames(ralpha)`)]<-substr(ralpha$`rownames(ralpha)`[grepl(".3",ralpha$`rownames(ralpha)`)],1,nchar(ralpha$`rownames(ralpha)`[grepl(".3",ralpha$`rownames(ralpha)`)])-2)
# ralpha$`rownames(ralpha)`[grepl(".4",ralpha$`rownames(ralpha)`)]<-substr(ralpha$`rownames(ralpha)`[grepl(".4",ralpha$`rownames(ralpha)`)],1,nchar(ralpha$`rownames(ralpha)`[grepl(".4",ralpha$`rownames(ralpha)`)])-2)
# ralpha<-aggregate(ralpha$X3, by=list(ralpha$`rownames(ralpha)`), FUN=sum)
# names(ralpha)<-c("Variable","S8-bDA")
# ralpha<-ralpha[!grepl("ID",ralpha$Variable),]
# #REGION relative absolute errors on households, persons and agents per category (rrelalpha)
# rrelalpha<-ralpha
# rrelalpha[,2]<-round(ralpha[,2]*100/rmarginals$rHH_ID[-1:-2],2)
# #Results
# Results1<-cbind(Results1,"S8-bDA"=ralpha[,2])
# Results2<-cbind(Results2,"S8-bDA"=rrelalpha[,2])
# Results3<-rbind(Results3,data.frame(Scenario, 
#                                     Iph, Ipp, 
#                                     rgammahh, rgammapp, rgammatot,
#                                     rrelgammahh, rrelgammapp, rrelgammatot,
#                                     rbetahh, rbetapp, rbetatot,
#                                     rrelbetahh, rrelbetapp, rrelbetatot
# ))
# 
# #S9-bCMA_DA
# #Import Results
# rsynmarginals<-read.csv("Exported/A1/3. Analysis/Toronto/S9-bCMA_DA/summary_region.csv")
# rsynmarginals[-1:-2,][is.na(rsynmarginals[-1:-2,])]<-0
# rsynmarginals<-rsynmarginals[,grepl("r",names(rsynmarginals))]
# rsynmarginals<-rsynmarginals[match(names(rmarginals),names(rsynmarginals))]
# ghhmarginals<-read.csv("Exported/A1/3. Analysis/Toronto/S9-bCMA_DA/geo_household_marginals.csv")
# gppmarginals<-read.csv("Exported/A1/3. Analysis/Toronto/S9-bCMA_DA/geo_person_marginals.csv")
# gmarginals<-cbind(ghhmarginals,gppmarginals[,-1])
# gsynmarginals<-read.csv("Exported/A1/3. Analysis/Toronto/S9-bCMA_DA/summary_geo.csv")
# gsynmarginals<-gsynmarginals[,!grepl("r",names(gsynmarginals))]
# gmarginals<-gmarginals[,names(gmarginals)%in%names(gsynmarginals)]
# gsynmarginals<-gsynmarginals[match(names(gmarginals),names(gsynmarginals))]
# #Scenario
# Scenario<-"S9-bCMA_DA"
# #Ip
# Iph<-round(sum(as.numeric(ghhmarginals$HH_ID[-1:-2])*as.numeric(ghhmarginals$HH_ID[-1:-2])*100/rhhmarginals$rHH_ID[-1:-2])/(sum(ghhmarginals$HH_ID[-1:-2])),2)
# Ipp<-round(sum(as.numeric(gppmarginals$PP_ID[-1:-2])*as.numeric(gppmarginals$PP_ID[-1:-2])*100/rppmarginals$rPP_ID[-1:-2])/(sum(gppmarginals$PP_ID[-1:-2])),2)
# #REGION absolute differences on total households, persons and agents synthesized (gamma)
# rgammahh<-abs(rmarginals$rHH_ID[-1:-2]-rsynmarginals$rHH_ID[-1:-2])
# rgammapp<-abs(rmarginals$rPP_ID[-1:-2]-rsynmarginals$rPP_ID[-1:-2])
# rgammatot<-rgammahh + rgammapp
# #REGION relative differences on total households, persons and agents synthesized (relgamma)
# rrelgammahh<-round(abs(rmarginals$rHH_ID[-1:-2]-rsynmarginals$rHH_ID[-1:-2])*100/rmarginals$rHH_ID[-1:-2],5)
# rrelgammapp<-round(abs(rmarginals$rPP_ID[-1:-2]-rsynmarginals$rPP_ID[-1:-2])*100/rmarginals$rPP_ID[-1:-2],5)
# rrelgammatot<-round(rgammatot*100/(rmarginals$rHH_ID[-1:-2]+rmarginals$rPP_ID[-1:-2]),5)
# #REGION total absolute errors (beta)
# rbetahh<-sum(abs(rmarginals[-1:-2,grepl("rHH_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rHH_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))]))
# rbetapp<-sum(abs(rmarginals[-1:-2,grepl("rPP_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rPP_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))]))
# rbetatot<-rbetahh + rbetapp
# #REGION relative total absolute errors (rrelbeta)
# rrelbetahh<-round(sum(abs(rmarginals[-1:-2,grepl("rHH_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rHH_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))]))*100/rmarginals$rHH_ID[-1:-2],2)
# rrelbetapp<-round(sum(abs(rmarginals[-1:-2,grepl("rPP_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rPP_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))]))*100/rmarginals$rPP_ID[-1:-2],2)
# rrelbetatot<-round((sum(abs(rmarginals[-1:-2,grepl("rHH_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rHH_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))])) + sum(abs(rmarginals[-1:-2,grepl("rPP_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rPP_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))])))*100/(rmarginals$rHH_ID[-1:-2] + rmarginals$rPP_ID[-1:-2]),2)
# #REGION absolute errors on households, persons and agents per category (alpha)
# ralphahh<-abs(rmarginals[-1:-2,grepl("rHH_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rHH_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))])
# ralphapp<-abs(rmarginals[-1:-2,grepl("rPP_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rPP_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))])
# ralpha<-abs(rmarginals[-1:-2,]-rsynmarginals[-1:-2,])
# ralpha<-data.frame(t(ralpha))
# ralpha<-cbind(rownames(ralpha),ralpha)
# ralpha$`rownames(ralpha)`[grepl(".1",ralpha$`rownames(ralpha)`)]<-substr(ralpha$`rownames(ralpha)`[grepl(".1",ralpha$`rownames(ralpha)`)],1,nchar(ralpha$`rownames(ralpha)`[grepl(".1",ralpha$`rownames(ralpha)`)])-2)
# ralpha$`rownames(ralpha)`[grepl(".2",ralpha$`rownames(ralpha)`)]<-substr(ralpha$`rownames(ralpha)`[grepl(".2",ralpha$`rownames(ralpha)`)],1,nchar(ralpha$`rownames(ralpha)`[grepl(".2",ralpha$`rownames(ralpha)`)])-2)
# ralpha$`rownames(ralpha)`[grepl(".3",ralpha$`rownames(ralpha)`)]<-substr(ralpha$`rownames(ralpha)`[grepl(".3",ralpha$`rownames(ralpha)`)],1,nchar(ralpha$`rownames(ralpha)`[grepl(".3",ralpha$`rownames(ralpha)`)])-2)
# ralpha$`rownames(ralpha)`[grepl(".4",ralpha$`rownames(ralpha)`)]<-substr(ralpha$`rownames(ralpha)`[grepl(".4",ralpha$`rownames(ralpha)`)],1,nchar(ralpha$`rownames(ralpha)`[grepl(".4",ralpha$`rownames(ralpha)`)])-2)
# ralpha<-aggregate(ralpha$X3, by=list(ralpha$`rownames(ralpha)`), FUN=sum)
# names(ralpha)<-c("Variable","S9-bCMA_DA")
# ralpha<-ralpha[!grepl("ID",ralpha$Variable),]
# #REGION relative absolute errors on households, persons and agents per category (rrelalpha)
# rrelalpha<-ralpha
# rrelalpha[,2]<-round(ralpha[,2]*100/rmarginals$rHH_ID[-1:-2],2)
# #Results
# Results1<-cbind(Results1,"S9-bCMA_DA"=ralpha[,2])
# Results2<-cbind(Results2,"S9-bCMA_DA"=rrelalpha[,2])
# Results3<-rbind(Results3,data.frame(Scenario, 
#                                     Iph, Ipp, 
#                                     rgammahh, rgammapp, rgammatot,
#                                     rrelgammahh, rrelgammapp, rrelgammatot,
#                                     rbetahh, rbetapp, rbetatot,
#                                     rrelbetahh, rrelbetapp, rrelbetatot
# ))
# 
# #S10-hCMA
# #Import Results
# rhhmarginals<-read.csv("Exported/A1/3. Analysis/Toronto/S10-hCMA/region_household_marginals.csv")
# rppmarginals<-read.csv("Exported/A1/3. Analysis/Toronto/S10-hCMA/region_person_marginals.csv")
# rmarginals<-cbind(rhhmarginals,rppmarginals[,-1])
# rsynmarginals<-read.csv("Exported/A1/3. Analysis/Toronto/S10-hCMA/summary_region.csv")
# rsynmarginals<-rsynmarginals[,grepl("r",names(rsynmarginals))]
# rmarginals<-rmarginals[,names(rmarginals)%in%names(rsynmarginals)]
# rsynmarginals<-rsynmarginals[match(names(rmarginals),names(rsynmarginals))]
# ghhmarginals<-read.csv("Exported/A1/3. Analysis/Toronto/S10-hCMA/geo_household_marginals.csv")
# gppmarginals<-read.csv("Exported/A1/3. Analysis/Toronto/S10-hCMA/geo_person_marginals.csv")
# gmarginals<-cbind(ghhmarginals,gppmarginals[,-1])
# gsynmarginals<-read.csv("Exported/A1/3. Analysis/Toronto/S10-hCMA/summary_geo.csv")
# gsynmarginals<-gsynmarginals[,!grepl("r",names(gsynmarginals))]
# gmarginals<-gmarginals[,names(gmarginals)%in%names(gsynmarginals)]
# gsynmarginals<-gsynmarginals[match(names(gmarginals),names(gsynmarginals))]
# #Scenario
# Scenario<-"S10-hCMA"
# #Ip
# Iph<-round(sum(as.numeric(ghhmarginals$HH_ID[-1:-2])*as.numeric(ghhmarginals$HH_ID[-1:-2])*100/rhhmarginals$rHH_ID[-1:-2])/(sum(ghhmarginals$HH_ID[-1:-2])),2)
# Ipp<-round(sum(as.numeric(gppmarginals$PP_ID[-1:-2])*as.numeric(gppmarginals$PP_ID[-1:-2])*100/rppmarginals$rPP_ID[-1:-2])/(sum(gppmarginals$PP_ID[-1:-2])),2)
# #REGION absolute differences on total households, persons and agents synthesized (gamma)
# rgammahh<-abs(rmarginals$rHH_ID[-1:-2]-rsynmarginals$rHH_ID[-1:-2])
# rgammapp<-abs(rmarginals$rPP_ID[-1:-2]-rsynmarginals$rPP_ID[-1:-2])
# rgammatot<-rgammahh + rgammapp
# #REGION relative differences on total households, persons and agents synthesized (relgamma)
# rrelgammahh<-round(abs(rmarginals$rHH_ID[-1:-2]-rsynmarginals$rHH_ID[-1:-2])*100/rmarginals$rHH_ID[-1:-2],5)
# rrelgammapp<-round(abs(rmarginals$rPP_ID[-1:-2]-rsynmarginals$rPP_ID[-1:-2])*100/rmarginals$rPP_ID[-1:-2],5)
# rrelgammatot<-round(rgammatot*100/(rmarginals$rHH_ID[-1:-2]+rmarginals$rPP_ID[-1:-2]),5)
# #REGION total absolute errors (beta)
# rbetahh<-sum(abs(rmarginals[-1:-2,grepl("rHH_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rHH_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))]))
# rbetapp<-sum(abs(rmarginals[-1:-2,grepl("rPP_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rPP_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))]))
# rbetatot<-rbetahh + rbetapp
# #REGION relative total absolute errors (rrelbeta)
# rrelbetahh<-round(sum(abs(rmarginals[-1:-2,grepl("rHH_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rHH_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))]))*100/rmarginals$rHH_ID[-1:-2],2)
# rrelbetapp<-round(sum(abs(rmarginals[-1:-2,grepl("rPP_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rPP_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))]))*100/rmarginals$rPP_ID[-1:-2],2)
# rrelbetatot<-round((sum(abs(rmarginals[-1:-2,grepl("rHH_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rHH_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))])) + sum(abs(rmarginals[-1:-2,grepl("rPP_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rPP_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))])))*100/(rmarginals$rHH_ID[-1:-2] + rmarginals$rPP_ID[-1:-2]),2)
# #REGION absolute errors on households, persons and agents per category (alpha)
# ralphahh<-abs(rmarginals[-1:-2,grepl("rHH_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rHH_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))])
# ralphapp<-abs(rmarginals[-1:-2,grepl("rPP_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rPP_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))])
# ralpha<-abs(rmarginals[-1:-2,]-rsynmarginals[-1:-2,])
# ralpha<-data.frame(t(ralpha))
# ralpha<-cbind(rownames(ralpha),ralpha)
# ralpha$`rownames(ralpha)`[grepl(".1",ralpha$`rownames(ralpha)`)]<-substr(ralpha$`rownames(ralpha)`[grepl(".1",ralpha$`rownames(ralpha)`)],1,nchar(ralpha$`rownames(ralpha)`[grepl(".1",ralpha$`rownames(ralpha)`)])-2)
# ralpha$`rownames(ralpha)`[grepl(".2",ralpha$`rownames(ralpha)`)]<-substr(ralpha$`rownames(ralpha)`[grepl(".2",ralpha$`rownames(ralpha)`)],1,nchar(ralpha$`rownames(ralpha)`[grepl(".2",ralpha$`rownames(ralpha)`)])-2)
# ralpha$`rownames(ralpha)`[grepl(".3",ralpha$`rownames(ralpha)`)]<-substr(ralpha$`rownames(ralpha)`[grepl(".3",ralpha$`rownames(ralpha)`)],1,nchar(ralpha$`rownames(ralpha)`[grepl(".3",ralpha$`rownames(ralpha)`)])-2)
# ralpha$`rownames(ralpha)`[grepl(".4",ralpha$`rownames(ralpha)`)]<-substr(ralpha$`rownames(ralpha)`[grepl(".4",ralpha$`rownames(ralpha)`)],1,nchar(ralpha$`rownames(ralpha)`[grepl(".4",ralpha$`rownames(ralpha)`)])-2)
# ralpha<-aggregate(ralpha$X3, by=list(ralpha$`rownames(ralpha)`), FUN=sum)
# names(ralpha)<-c("Variable","S10-hCMA")
# ralpha<-ralpha[!grepl("ID",ralpha$Variable),]
# #REGION relative absolute errors on households, persons and agents per category (rrelalpha)
# rrelalpha<-ralpha
# rrelalpha[,2]<-round(ralpha[,2]*100/rmarginals$rHH_ID[-1:-2],2)
# #Results
# Results1<-cbind(Results1,"S10-hCMA"=ralpha[,2])
# Results2<-cbind(Results2,"S10-hCMA"=rrelalpha[,2])
# Results3<-rbind(Results3,data.frame(Scenario, 
#                                     Iph, Ipp, 
#                                     rgammahh, rgammapp, rgammatot,
#                                     rrelgammahh, rrelgammapp, rrelgammatot,
#                                     rbetahh, rbetapp, rbetatot,
#                                     rrelbetahh, rrelbetapp, rrelbetatot
# ))
# 
# #S11-hCSD
# #Import Results
# rsynmarginals<-read.csv("Exported/A1/3. Analysis/Toronto/S11-hCSD/summary_region.csv")
# rsynmarginals[-1:-2,][is.na(rsynmarginals[-1:-2,])]<-0
# rsynmarginals<-rsynmarginals[,grepl("r",names(rsynmarginals))]
# temp<-rsynmarginals[1:3,]
# temp[3,]<-colSums(rsynmarginals[-1:-2,])
# rsynmarginals<-temp
# rsynmarginals<-rsynmarginals[match(names(rmarginals),names(rsynmarginals))]
# ghhmarginals<-read.csv("Exported/A1/3. Analysis/Toronto/S11-hCSD/geo_household_marginals.csv")
# gppmarginals<-read.csv("Exported/A1/3. Analysis/Toronto/S11-hCSD/geo_person_marginals.csv")
# gmarginals<-cbind(ghhmarginals,gppmarginals[,-1])
# gsynmarginals<-read.csv("Exported/A1/3. Analysis/Toronto/S11-hCSD/summary_geo.csv")
# gsynmarginals<-gsynmarginals[,!grepl("r",names(gsynmarginals))]
# gmarginals<-gmarginals[,names(gmarginals)%in%names(gsynmarginals)]
# gsynmarginals<-gsynmarginals[match(names(gmarginals),names(gsynmarginals))]
# #Scenario
# Scenario<-"S11-hCSD"
# #Ip
# Iph<-round(sum(as.numeric(ghhmarginals$HH_ID[-1:-2])*as.numeric(ghhmarginals$HH_ID[-1:-2])*100/rhhmarginals$rHH_ID[-1:-2])/(sum(ghhmarginals$HH_ID[-1:-2])),2)
# Ipp<-round(sum(as.numeric(gppmarginals$PP_ID[-1:-2])*as.numeric(gppmarginals$PP_ID[-1:-2])*100/rppmarginals$rPP_ID[-1:-2])/(sum(gppmarginals$PP_ID[-1:-2])),2)
# #REGION absolute differences on total households, persons and agents synthesized (gamma)
# rgammahh<-abs(rmarginals$rHH_ID[-1:-2]-rsynmarginals$rHH_ID[-1:-2])
# rgammapp<-abs(rmarginals$rPP_ID[-1:-2]-rsynmarginals$rPP_ID[-1:-2])
# rgammatot<-rgammahh + rgammapp
# #REGION relative differences on total households, persons and agents synthesized (relgamma)
# rrelgammahh<-round(abs(rmarginals$rHH_ID[-1:-2]-rsynmarginals$rHH_ID[-1:-2])*100/rmarginals$rHH_ID[-1:-2],5)
# rrelgammapp<-round(abs(rmarginals$rPP_ID[-1:-2]-rsynmarginals$rPP_ID[-1:-2])*100/rmarginals$rPP_ID[-1:-2],5)
# rrelgammatot<-round(rgammatot*100/(rmarginals$rHH_ID[-1:-2]+rmarginals$rPP_ID[-1:-2]),5)
# #REGION total absolute errors (beta)
# rbetahh<-sum(abs(rmarginals[-1:-2,grepl("rHH_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rHH_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))]))
# rbetapp<-sum(abs(rmarginals[-1:-2,grepl("rPP_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rPP_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))]))
# rbetatot<-rbetahh + rbetapp
# #REGION relative total absolute errors (rrelbeta)
# rrelbetahh<-round(sum(abs(rmarginals[-1:-2,grepl("rHH_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rHH_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))]))*100/rmarginals$rHH_ID[-1:-2],2)
# rrelbetapp<-round(sum(abs(rmarginals[-1:-2,grepl("rPP_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rPP_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))]))*100/rmarginals$rPP_ID[-1:-2],2)
# rrelbetatot<-round((sum(abs(rmarginals[-1:-2,grepl("rHH_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rHH_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))])) + sum(abs(rmarginals[-1:-2,grepl("rPP_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rPP_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))])))*100/(rmarginals$rHH_ID[-1:-2] + rmarginals$rPP_ID[-1:-2]),2)
# #REGION absolute errors on households, persons and agents per category (alpha)
# ralphahh<-abs(rmarginals[-1:-2,grepl("rHH_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rHH_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))])
# ralphapp<-abs(rmarginals[-1:-2,grepl("rPP_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rPP_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))])
# ralpha<-abs(rmarginals[-1:-2,]-rsynmarginals[-1:-2,])
# ralpha<-data.frame(t(ralpha))
# ralpha<-cbind(rownames(ralpha),ralpha)
# ralpha$`rownames(ralpha)`[grepl(".1",ralpha$`rownames(ralpha)`)]<-substr(ralpha$`rownames(ralpha)`[grepl(".1",ralpha$`rownames(ralpha)`)],1,nchar(ralpha$`rownames(ralpha)`[grepl(".1",ralpha$`rownames(ralpha)`)])-2)
# ralpha$`rownames(ralpha)`[grepl(".2",ralpha$`rownames(ralpha)`)]<-substr(ralpha$`rownames(ralpha)`[grepl(".2",ralpha$`rownames(ralpha)`)],1,nchar(ralpha$`rownames(ralpha)`[grepl(".2",ralpha$`rownames(ralpha)`)])-2)
# ralpha$`rownames(ralpha)`[grepl(".3",ralpha$`rownames(ralpha)`)]<-substr(ralpha$`rownames(ralpha)`[grepl(".3",ralpha$`rownames(ralpha)`)],1,nchar(ralpha$`rownames(ralpha)`[grepl(".3",ralpha$`rownames(ralpha)`)])-2)
# ralpha$`rownames(ralpha)`[grepl(".4",ralpha$`rownames(ralpha)`)]<-substr(ralpha$`rownames(ralpha)`[grepl(".4",ralpha$`rownames(ralpha)`)],1,nchar(ralpha$`rownames(ralpha)`[grepl(".4",ralpha$`rownames(ralpha)`)])-2)
# ralpha<-aggregate(ralpha$X3, by=list(ralpha$`rownames(ralpha)`), FUN=sum)
# names(ralpha)<-c("Variable","S11-hCSD")
# ralpha<-ralpha[!grepl("ID",ralpha$Variable),]
# #REGION relative absolute errors on households, persons and agents per category (rrelalpha)
# rrelalpha<-ralpha
# rrelalpha[,2]<-round(ralpha[,2]*100/rmarginals$rHH_ID[-1:-2],2)
# #Results
# Results1<-cbind(Results1,"S11-hCSD"=ralpha[,2])
# Results2<-cbind(Results2,"S11-hCSD"=rrelalpha[,2])
# Results3<-rbind(Results3,data.frame(Scenario, 
#                                     Iph, Ipp, 
#                                     rgammahh, rgammapp, rgammatot,
#                                     rrelgammahh, rrelgammapp, rrelgammatot,
#                                     rbetahh, rbetapp, rbetatot,
#                                     rrelbetahh, rrelbetapp, rrelbetatot
# ))
# 
# #S12-hCMA_CSD
# #Import Results
# rsynmarginals<-read.csv("Exported/A1/3. Analysis/Toronto/S12-hCMA_CSD/summary_region.csv")
# rsynmarginals[-1:-2,][is.na(rsynmarginals[-1:-2,])]<-0
# rsynmarginals<-rsynmarginals[,grepl("r",names(rsynmarginals))]
# rsynmarginals<-rsynmarginals[match(names(rmarginals),names(rsynmarginals))]
# ghhmarginals<-read.csv("Exported/A1/3. Analysis/Toronto/S12-hCMA_CSD/geo_household_marginals.csv")
# gppmarginals<-read.csv("Exported/A1/3. Analysis/Toronto/S12-hCMA_CSD/geo_person_marginals.csv")
# gmarginals<-cbind(ghhmarginals,gppmarginals[,-1])
# gsynmarginals<-read.csv("Exported/A1/3. Analysis/Toronto/S12-hCMA_CSD/summary_geo.csv")
# gsynmarginals<-gsynmarginals[,!grepl("r",names(gsynmarginals))]
# gmarginals<-gmarginals[,names(gmarginals)%in%names(gsynmarginals)]
# gsynmarginals<-gsynmarginals[match(names(gmarginals),names(gsynmarginals))]
# #Scenario
# Scenario<-"S12-hCMA_CSD"
# #Ip
# Iph<-round(sum(as.numeric(ghhmarginals$HH_ID[-1:-2])*as.numeric(ghhmarginals$HH_ID[-1:-2])*100/rhhmarginals$rHH_ID[-1:-2])/(sum(ghhmarginals$HH_ID[-1:-2])),2)
# Ipp<-round(sum(as.numeric(gppmarginals$PP_ID[-1:-2])*as.numeric(gppmarginals$PP_ID[-1:-2])*100/rppmarginals$rPP_ID[-1:-2])/(sum(gppmarginals$PP_ID[-1:-2])),2)
# #REGION absolute differences on total households, persons and agents synthesized (gamma)
# rgammahh<-abs(rmarginals$rHH_ID[-1:-2]-rsynmarginals$rHH_ID[-1:-2])
# rgammapp<-abs(rmarginals$rPP_ID[-1:-2]-rsynmarginals$rPP_ID[-1:-2])
# rgammatot<-rgammahh + rgammapp
# #REGION relative differences on total households, persons and agents synthesized (relgamma)
# rrelgammahh<-round(abs(rmarginals$rHH_ID[-1:-2]-rsynmarginals$rHH_ID[-1:-2])*100/rmarginals$rHH_ID[-1:-2],5)
# rrelgammapp<-round(abs(rmarginals$rPP_ID[-1:-2]-rsynmarginals$rPP_ID[-1:-2])*100/rmarginals$rPP_ID[-1:-2],5)
# rrelgammatot<-round(rgammatot*100/(rmarginals$rHH_ID[-1:-2]+rmarginals$rPP_ID[-1:-2]),5)
# #REGION total absolute errors (beta)
# rbetahh<-sum(abs(rmarginals[-1:-2,grepl("rHH_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rHH_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))]))
# rbetapp<-sum(abs(rmarginals[-1:-2,grepl("rPP_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rPP_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))]))
# rbetatot<-rbetahh + rbetapp
# #REGION relative total absolute errors (rrelbeta)
# rrelbetahh<-round(sum(abs(rmarginals[-1:-2,grepl("rHH_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rHH_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))]))*100/rmarginals$rHH_ID[-1:-2],2)
# rrelbetapp<-round(sum(abs(rmarginals[-1:-2,grepl("rPP_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rPP_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))]))*100/rmarginals$rPP_ID[-1:-2],2)
# rrelbetatot<-round((sum(abs(rmarginals[-1:-2,grepl("rHH_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rHH_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))])) + sum(abs(rmarginals[-1:-2,grepl("rPP_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rPP_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))])))*100/(rmarginals$rHH_ID[-1:-2] + rmarginals$rPP_ID[-1:-2]),2)
# #REGION absolute errors on households, persons and agents per category (alpha)
# ralphahh<-abs(rmarginals[-1:-2,grepl("rHH_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rHH_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))])
# ralphapp<-abs(rmarginals[-1:-2,grepl("rPP_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rPP_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))])
# ralpha<-abs(rmarginals[-1:-2,]-rsynmarginals[-1:-2,])
# ralpha<-data.frame(t(ralpha))
# ralpha<-cbind(rownames(ralpha),ralpha)
# ralpha$`rownames(ralpha)`[grepl(".1",ralpha$`rownames(ralpha)`)]<-substr(ralpha$`rownames(ralpha)`[grepl(".1",ralpha$`rownames(ralpha)`)],1,nchar(ralpha$`rownames(ralpha)`[grepl(".1",ralpha$`rownames(ralpha)`)])-2)
# ralpha$`rownames(ralpha)`[grepl(".2",ralpha$`rownames(ralpha)`)]<-substr(ralpha$`rownames(ralpha)`[grepl(".2",ralpha$`rownames(ralpha)`)],1,nchar(ralpha$`rownames(ralpha)`[grepl(".2",ralpha$`rownames(ralpha)`)])-2)
# ralpha$`rownames(ralpha)`[grepl(".3",ralpha$`rownames(ralpha)`)]<-substr(ralpha$`rownames(ralpha)`[grepl(".3",ralpha$`rownames(ralpha)`)],1,nchar(ralpha$`rownames(ralpha)`[grepl(".3",ralpha$`rownames(ralpha)`)])-2)
# ralpha$`rownames(ralpha)`[grepl(".4",ralpha$`rownames(ralpha)`)]<-substr(ralpha$`rownames(ralpha)`[grepl(".4",ralpha$`rownames(ralpha)`)],1,nchar(ralpha$`rownames(ralpha)`[grepl(".4",ralpha$`rownames(ralpha)`)])-2)
# ralpha<-aggregate(ralpha$X3, by=list(ralpha$`rownames(ralpha)`), FUN=sum)
# names(ralpha)<-c("Variable","S12-hCMA_CSD")
# ralpha<-ralpha[!grepl("ID",ralpha$Variable),]
# #REGION relative absolute errors on households, persons and agents per category (rrelalpha)
# rrelalpha<-ralpha
# rrelalpha[,2]<-round(ralpha[,2]*100/rmarginals$rHH_ID[-1:-2],2)
# #Results
# Results1<-cbind(Results1,"S12-hCMA_CSD"=ralpha[,2])
# Results2<-cbind(Results2,"S12-hCMA_CSD"=rrelalpha[,2])
# Results3<-rbind(Results3,data.frame(Scenario, 
#                                     Iph, Ipp, 
#                                     rgammahh, rgammapp, rgammatot,
#                                     rrelgammahh, rrelgammapp, rrelgammatot,
#                                     rbetahh, rbetapp, rbetatot,
#                                     rrelbetahh, rrelbetapp, rrelbetatot
# ))
# 
# #S13-hADA
# #Import Results
# rsynmarginals<-read.csv("Exported/A1/3. Analysis/Toronto/S13-hADA/summary_region.csv")
# rsynmarginals[-1:-2,][is.na(rsynmarginals[-1:-2,])]<-0
# rsynmarginals<-rsynmarginals[,grepl("r",names(rsynmarginals))]
# temp<-rsynmarginals[1:3,]
# temp[3,]<-colSums(rsynmarginals[-1:-2,])
# rsynmarginals<-temp
# rsynmarginals<-rsynmarginals[match(names(rmarginals),names(rsynmarginals))]
# ghhmarginals<-read.csv("Exported/A1/3. Analysis/Toronto/S13-hADA/geo_household_marginals.csv")
# gppmarginals<-read.csv("Exported/A1/3. Analysis/Toronto/S13-hADA/geo_person_marginals.csv")
# gmarginals<-cbind(ghhmarginals,gppmarginals[,-1])
# gsynmarginals<-read.csv("Exported/A1/3. Analysis/Toronto/S13-hADA/summary_geo.csv")
# gsynmarginals<-gsynmarginals[,!grepl("r",names(gsynmarginals))]
# gmarginals<-gmarginals[,names(gmarginals)%in%names(gsynmarginals)]
# gsynmarginals<-gsynmarginals[match(names(gmarginals),names(gsynmarginals))]
# #Scenario
# Scenario<-"S13-hADA"
# #Ip
# Iph<-round(sum(as.numeric(ghhmarginals$HH_ID[-1:-2])*as.numeric(ghhmarginals$HH_ID[-1:-2])*100/rhhmarginals$rHH_ID[-1:-2])/(sum(ghhmarginals$HH_ID[-1:-2])),2)
# Ipp<-round(sum(as.numeric(gppmarginals$PP_ID[-1:-2])*as.numeric(gppmarginals$PP_ID[-1:-2])*100/rppmarginals$rPP_ID[-1:-2])/(sum(gppmarginals$PP_ID[-1:-2])),2)
# #REGION absolute differences on total households, persons and agents synthesized (gamma)
# rgammahh<-abs(rmarginals$rHH_ID[-1:-2]-rsynmarginals$rHH_ID[-1:-2])
# rgammapp<-abs(rmarginals$rPP_ID[-1:-2]-rsynmarginals$rPP_ID[-1:-2])
# rgammatot<-rgammahh + rgammapp
# #REGION relative differences on total households, persons and agents synthesized (relgamma)
# rrelgammahh<-round(abs(rmarginals$rHH_ID[-1:-2]-rsynmarginals$rHH_ID[-1:-2])*100/rmarginals$rHH_ID[-1:-2],5)
# rrelgammapp<-round(abs(rmarginals$rPP_ID[-1:-2]-rsynmarginals$rPP_ID[-1:-2])*100/rmarginals$rPP_ID[-1:-2],5)
# rrelgammatot<-round(rgammatot*100/(rmarginals$rHH_ID[-1:-2]+rmarginals$rPP_ID[-1:-2]),5)
# #REGION total absolute errors (beta)
# rbetahh<-sum(abs(rmarginals[-1:-2,grepl("rHH_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rHH_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))]))
# rbetapp<-sum(abs(rmarginals[-1:-2,grepl("rPP_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rPP_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))]))
# rbetatot<-rbetahh + rbetapp
# #REGION relative total absolute errors (rrelbeta)
# rrelbetahh<-round(sum(abs(rmarginals[-1:-2,grepl("rHH_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rHH_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))]))*100/rmarginals$rHH_ID[-1:-2],2)
# rrelbetapp<-round(sum(abs(rmarginals[-1:-2,grepl("rPP_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rPP_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))]))*100/rmarginals$rPP_ID[-1:-2],2)
# rrelbetatot<-round((sum(abs(rmarginals[-1:-2,grepl("rHH_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rHH_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))])) + sum(abs(rmarginals[-1:-2,grepl("rPP_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rPP_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))])))*100/(rmarginals$rHH_ID[-1:-2] + rmarginals$rPP_ID[-1:-2]),2)
# #REGION absolute errors on households, persons and agents per category (alpha)
# ralphahh<-abs(rmarginals[-1:-2,grepl("rHH_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rHH_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))])
# ralphapp<-abs(rmarginals[-1:-2,grepl("rPP_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rPP_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))])
# ralpha<-abs(rmarginals[-1:-2,]-rsynmarginals[-1:-2,])
# ralpha<-data.frame(t(ralpha))
# ralpha<-cbind(rownames(ralpha),ralpha)
# ralpha$`rownames(ralpha)`[grepl(".1",ralpha$`rownames(ralpha)`)]<-substr(ralpha$`rownames(ralpha)`[grepl(".1",ralpha$`rownames(ralpha)`)],1,nchar(ralpha$`rownames(ralpha)`[grepl(".1",ralpha$`rownames(ralpha)`)])-2)
# ralpha$`rownames(ralpha)`[grepl(".2",ralpha$`rownames(ralpha)`)]<-substr(ralpha$`rownames(ralpha)`[grepl(".2",ralpha$`rownames(ralpha)`)],1,nchar(ralpha$`rownames(ralpha)`[grepl(".2",ralpha$`rownames(ralpha)`)])-2)
# ralpha$`rownames(ralpha)`[grepl(".3",ralpha$`rownames(ralpha)`)]<-substr(ralpha$`rownames(ralpha)`[grepl(".3",ralpha$`rownames(ralpha)`)],1,nchar(ralpha$`rownames(ralpha)`[grepl(".3",ralpha$`rownames(ralpha)`)])-2)
# ralpha$`rownames(ralpha)`[grepl(".4",ralpha$`rownames(ralpha)`)]<-substr(ralpha$`rownames(ralpha)`[grepl(".4",ralpha$`rownames(ralpha)`)],1,nchar(ralpha$`rownames(ralpha)`[grepl(".4",ralpha$`rownames(ralpha)`)])-2)
# ralpha<-aggregate(ralpha$X3, by=list(ralpha$`rownames(ralpha)`), FUN=sum)
# names(ralpha)<-c("Variable","S13-hADA")
# ralpha<-ralpha[!grepl("ID",ralpha$Variable),]
# #REGION relative absolute errors on households, persons and agents per category (rrelalpha)
# rrelalpha<-ralpha
# rrelalpha[,2]<-round(ralpha[,2]*100/rmarginals$rHH_ID[-1:-2],2)
# #Results
# Results1<-cbind(Results1,"S13-hADA"=ralpha[,2])
# Results2<-cbind(Results2,"S13-hADA"=rrelalpha[,2])
# Results3<-rbind(Results3,data.frame(Scenario, 
#                                     Iph, Ipp, 
#                                     rgammahh, rgammapp, rgammatot,
#                                     rrelgammahh, rrelgammapp, rrelgammatot,
#                                     rbetahh, rbetapp, rbetatot,
#                                     rrelbetahh, rrelbetapp, rrelbetatot
# ))
# 
# #S14-hCMA_ADA
# #Import Results
# rsynmarginals<-read.csv("Exported/A1/3. Analysis/Toronto/S14-hCMA_ADA/summary_region.csv")
# rsynmarginals[-1:-2,][is.na(rsynmarginals[-1:-2,])]<-0
# rsynmarginals<-rsynmarginals[,grepl("r",names(rsynmarginals))]
# rsynmarginals<-rsynmarginals[match(names(rmarginals),names(rsynmarginals))]
# ghhmarginals<-read.csv("Exported/A1/3. Analysis/Toronto/S14-hCMA_ADA/geo_household_marginals.csv")
# gppmarginals<-read.csv("Exported/A1/3. Analysis/Toronto/S14-hCMA_ADA/geo_person_marginals.csv")
# gmarginals<-cbind(ghhmarginals,gppmarginals[,-1])
# gsynmarginals<-read.csv("Exported/A1/3. Analysis/Toronto/S14-hCMA_ADA/summary_geo.csv")
# gsynmarginals<-gsynmarginals[,!grepl("r",names(gsynmarginals))]
# gmarginals<-gmarginals[,names(gmarginals)%in%names(gsynmarginals)]
# gsynmarginals<-gsynmarginals[match(names(gmarginals),names(gsynmarginals))]
# #Scenario
# Scenario<-"S14-hCMA_ADA"
# #Ip
# Iph<-round(sum(as.numeric(ghhmarginals$HH_ID[-1:-2])*as.numeric(ghhmarginals$HH_ID[-1:-2])*100/rhhmarginals$rHH_ID[-1:-2])/(sum(ghhmarginals$HH_ID[-1:-2])),2)
# Ipp<-round(sum(as.numeric(gppmarginals$PP_ID[-1:-2])*as.numeric(gppmarginals$PP_ID[-1:-2])*100/rppmarginals$rPP_ID[-1:-2])/(sum(gppmarginals$PP_ID[-1:-2])),2)
# #REGION absolute differences on total households, persons and agents synthesized (gamma)
# rgammahh<-abs(rmarginals$rHH_ID[-1:-2]-rsynmarginals$rHH_ID[-1:-2])
# rgammapp<-abs(rmarginals$rPP_ID[-1:-2]-rsynmarginals$rPP_ID[-1:-2])
# rgammatot<-rgammahh + rgammapp
# #REGION relative differences on total households, persons and agents synthesized (relgamma)
# rrelgammahh<-round(abs(rmarginals$rHH_ID[-1:-2]-rsynmarginals$rHH_ID[-1:-2])*100/rmarginals$rHH_ID[-1:-2],5)
# rrelgammapp<-round(abs(rmarginals$rPP_ID[-1:-2]-rsynmarginals$rPP_ID[-1:-2])*100/rmarginals$rPP_ID[-1:-2],5)
# rrelgammatot<-round(rgammatot*100/(rmarginals$rHH_ID[-1:-2]+rmarginals$rPP_ID[-1:-2]),5)
# #REGION total absolute errors (beta)
# rbetahh<-sum(abs(rmarginals[-1:-2,grepl("rHH_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rHH_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))]))
# rbetapp<-sum(abs(rmarginals[-1:-2,grepl("rPP_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rPP_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))]))
# rbetatot<-rbetahh + rbetapp
# #REGION relative total absolute errors (rrelbeta)
# rrelbetahh<-round(sum(abs(rmarginals[-1:-2,grepl("rHH_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rHH_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))]))*100/rmarginals$rHH_ID[-1:-2],2)
# rrelbetapp<-round(sum(abs(rmarginals[-1:-2,grepl("rPP_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rPP_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))]))*100/rmarginals$rPP_ID[-1:-2],2)
# rrelbetatot<-round((sum(abs(rmarginals[-1:-2,grepl("rHH_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rHH_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))])) + sum(abs(rmarginals[-1:-2,grepl("rPP_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rPP_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))])))*100/(rmarginals$rHH_ID[-1:-2] + rmarginals$rPP_ID[-1:-2]),2)
# #REGION absolute errors on households, persons and agents per category (alpha)
# ralphahh<-abs(rmarginals[-1:-2,grepl("rHH_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rHH_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))])
# ralphapp<-abs(rmarginals[-1:-2,grepl("rPP_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rPP_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))])
# ralpha<-abs(rmarginals[-1:-2,]-rsynmarginals[-1:-2,])
# ralpha<-data.frame(t(ralpha))
# ralpha<-cbind(rownames(ralpha),ralpha)
# ralpha$`rownames(ralpha)`[grepl(".1",ralpha$`rownames(ralpha)`)]<-substr(ralpha$`rownames(ralpha)`[grepl(".1",ralpha$`rownames(ralpha)`)],1,nchar(ralpha$`rownames(ralpha)`[grepl(".1",ralpha$`rownames(ralpha)`)])-2)
# ralpha$`rownames(ralpha)`[grepl(".2",ralpha$`rownames(ralpha)`)]<-substr(ralpha$`rownames(ralpha)`[grepl(".2",ralpha$`rownames(ralpha)`)],1,nchar(ralpha$`rownames(ralpha)`[grepl(".2",ralpha$`rownames(ralpha)`)])-2)
# ralpha$`rownames(ralpha)`[grepl(".3",ralpha$`rownames(ralpha)`)]<-substr(ralpha$`rownames(ralpha)`[grepl(".3",ralpha$`rownames(ralpha)`)],1,nchar(ralpha$`rownames(ralpha)`[grepl(".3",ralpha$`rownames(ralpha)`)])-2)
# ralpha$`rownames(ralpha)`[grepl(".4",ralpha$`rownames(ralpha)`)]<-substr(ralpha$`rownames(ralpha)`[grepl(".4",ralpha$`rownames(ralpha)`)],1,nchar(ralpha$`rownames(ralpha)`[grepl(".4",ralpha$`rownames(ralpha)`)])-2)
# ralpha<-aggregate(ralpha$X3, by=list(ralpha$`rownames(ralpha)`), FUN=sum)
# names(ralpha)<-c("Variable","S14-hCMA_ADA")
# ralpha<-ralpha[!grepl("ID",ralpha$Variable),]
# #REGION relative absolute errors on households, persons and agents per category (rrelalpha)
# rrelalpha<-ralpha
# rrelalpha[,2]<-round(ralpha[,2]*100/rmarginals$rHH_ID[-1:-2],2)
# #Results
# Results1<-cbind(Results1,"S14-hCMA_ADA"=ralpha[,2])
# Results2<-cbind(Results2,"S14-hCMA_ADA"=rrelalpha[,2])
# Results3<-rbind(Results3,data.frame(Scenario, 
#                                     Iph, Ipp, 
#                                     rgammahh, rgammapp, rgammatot,
#                                     rrelgammahh, rrelgammapp, rrelgammatot,
#                                     rbetahh, rbetapp, rbetatot,
#                                     rrelbetahh, rrelbetapp, rrelbetatot
# ))
# 
# #S15-hCT
# #Import Results
# rsynmarginals<-read.csv("Exported/A1/3. Analysis/Toronto/S15-hCT/summary_region.csv")
# rsynmarginals[-1:-2,][is.na(rsynmarginals[-1:-2,])]<-0
# rsynmarginals<-rsynmarginals[,grepl("r",names(rsynmarginals))]
# temp<-rsynmarginals[1:3,]
# temp[3,]<-colSums(rsynmarginals[-1:-2,])
# rsynmarginals<-temp
# rsynmarginals<-rsynmarginals[match(names(rmarginals),names(rsynmarginals))]
# ghhmarginals<-read.csv("Exported/A1/3. Analysis/Toronto/S15-hCT/geo_household_marginals.csv")
# gppmarginals<-read.csv("Exported/A1/3. Analysis/Toronto/S15-hCT/geo_person_marginals.csv")
# gmarginals<-cbind(ghhmarginals,gppmarginals[,-1])
# gsynmarginals<-read.csv("Exported/A1/3. Analysis/Toronto/S15-hCT/summary_geo.csv")
# gsynmarginals<-gsynmarginals[,!grepl("r",names(gsynmarginals))]
# gmarginals<-gmarginals[,names(gmarginals)%in%names(gsynmarginals)]
# gsynmarginals<-gsynmarginals[match(names(gmarginals),names(gsynmarginals))]
# #Scenario
# Scenario<-"S15-hCT"
# #Ip
# Iph<-round(sum(as.numeric(ghhmarginals$HH_ID[-1:-2])*as.numeric(ghhmarginals$HH_ID[-1:-2])*100/rhhmarginals$rHH_ID[-1:-2])/(sum(ghhmarginals$HH_ID[-1:-2])),2)
# Ipp<-round(sum(as.numeric(gppmarginals$PP_ID[-1:-2])*as.numeric(gppmarginals$PP_ID[-1:-2])*100/rppmarginals$rPP_ID[-1:-2])/(sum(gppmarginals$PP_ID[-1:-2])),2)
# #REGION absolute differences on total households, persons and agents synthesized (gamma)
# rgammahh<-abs(rmarginals$rHH_ID[-1:-2]-rsynmarginals$rHH_ID[-1:-2])
# rgammapp<-abs(rmarginals$rPP_ID[-1:-2]-rsynmarginals$rPP_ID[-1:-2])
# rgammatot<-rgammahh + rgammapp
# #REGION relative differences on total households, persons and agents synthesized (relgamma)
# rrelgammahh<-round(abs(rmarginals$rHH_ID[-1:-2]-rsynmarginals$rHH_ID[-1:-2])*100/rmarginals$rHH_ID[-1:-2],5)
# rrelgammapp<-round(abs(rmarginals$rPP_ID[-1:-2]-rsynmarginals$rPP_ID[-1:-2])*100/rmarginals$rPP_ID[-1:-2],5)
# rrelgammatot<-round(rgammatot*100/(rmarginals$rHH_ID[-1:-2]+rmarginals$rPP_ID[-1:-2]),5)
# #REGION total absolute errors (beta)
# rbetahh<-sum(abs(rmarginals[-1:-2,grepl("rHH_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rHH_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))]))
# rbetapp<-sum(abs(rmarginals[-1:-2,grepl("rPP_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rPP_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))]))
# rbetatot<-rbetahh + rbetapp
# #REGION relative total absolute errors (rrelbeta)
# rrelbetahh<-round(sum(abs(rmarginals[-1:-2,grepl("rHH_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rHH_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))]))*100/rmarginals$rHH_ID[-1:-2],2)
# rrelbetapp<-round(sum(abs(rmarginals[-1:-2,grepl("rPP_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rPP_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))]))*100/rmarginals$rPP_ID[-1:-2],2)
# rrelbetatot<-round((sum(abs(rmarginals[-1:-2,grepl("rHH_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rHH_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))])) + sum(abs(rmarginals[-1:-2,grepl("rPP_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rPP_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))])))*100/(rmarginals$rHH_ID[-1:-2] + rmarginals$rPP_ID[-1:-2]),2)
# #REGION absolute errors on households, persons and agents per category (alpha)
# ralphahh<-abs(rmarginals[-1:-2,grepl("rHH_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rHH_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))])
# ralphapp<-abs(rmarginals[-1:-2,grepl("rPP_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rPP_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))])
# ralpha<-abs(rmarginals[-1:-2,]-rsynmarginals[-1:-2,])
# ralpha<-data.frame(t(ralpha))
# ralpha<-cbind(rownames(ralpha),ralpha)
# ralpha$`rownames(ralpha)`[grepl(".1",ralpha$`rownames(ralpha)`)]<-substr(ralpha$`rownames(ralpha)`[grepl(".1",ralpha$`rownames(ralpha)`)],1,nchar(ralpha$`rownames(ralpha)`[grepl(".1",ralpha$`rownames(ralpha)`)])-2)
# ralpha$`rownames(ralpha)`[grepl(".2",ralpha$`rownames(ralpha)`)]<-substr(ralpha$`rownames(ralpha)`[grepl(".2",ralpha$`rownames(ralpha)`)],1,nchar(ralpha$`rownames(ralpha)`[grepl(".2",ralpha$`rownames(ralpha)`)])-2)
# ralpha$`rownames(ralpha)`[grepl(".3",ralpha$`rownames(ralpha)`)]<-substr(ralpha$`rownames(ralpha)`[grepl(".3",ralpha$`rownames(ralpha)`)],1,nchar(ralpha$`rownames(ralpha)`[grepl(".3",ralpha$`rownames(ralpha)`)])-2)
# ralpha$`rownames(ralpha)`[grepl(".4",ralpha$`rownames(ralpha)`)]<-substr(ralpha$`rownames(ralpha)`[grepl(".4",ralpha$`rownames(ralpha)`)],1,nchar(ralpha$`rownames(ralpha)`[grepl(".4",ralpha$`rownames(ralpha)`)])-2)
# ralpha<-aggregate(ralpha$X3, by=list(ralpha$`rownames(ralpha)`), FUN=sum)
# names(ralpha)<-c("Variable","S15-hCT")
# ralpha<-ralpha[!grepl("ID",ralpha$Variable),]
# #REGION relative absolute errors on households, persons and agents per category (rrelalpha)
# rrelalpha<-ralpha
# rrelalpha[,2]<-round(ralpha[,2]*100/rmarginals$rHH_ID[-1:-2],2)
# #Results
# Results1<-cbind(Results1,"S15-hCT"=ralpha[,2])
# Results2<-cbind(Results2,"S15-hCT"=rrelalpha[,2])
# Results3<-rbind(Results3,data.frame(Scenario, 
#                                     Iph, Ipp, 
#                                     rgammahh, rgammapp, rgammatot,
#                                     rrelgammahh, rrelgammapp, rrelgammatot,
#                                     rbetahh, rbetapp, rbetatot,
#                                     rrelbetahh, rrelbetapp, rrelbetatot
# ))
# 
# #S16-hCMA_CT
# #Import Results
# rsynmarginals<-read.csv("Exported/A1/3. Analysis/Toronto/S16-hCMA_CT/summary_region.csv")
# rsynmarginals[-1:-2,][is.na(rsynmarginals[-1:-2,])]<-0
# rsynmarginals<-rsynmarginals[,grepl("r",names(rsynmarginals))]
# rsynmarginals<-rsynmarginals[match(names(rmarginals),names(rsynmarginals))]
# ghhmarginals<-read.csv("Exported/A1/3. Analysis/Toronto/S16-hCMA_CT/geo_household_marginals.csv")
# gppmarginals<-read.csv("Exported/A1/3. Analysis/Toronto/S16-hCMA_CT/geo_person_marginals.csv")
# gmarginals<-cbind(ghhmarginals,gppmarginals[,-1])
# gsynmarginals<-read.csv("Exported/A1/3. Analysis/Toronto/S16-hCMA_CT/summary_geo.csv")
# gsynmarginals<-gsynmarginals[,!grepl("r",names(gsynmarginals))]
# gmarginals<-gmarginals[,names(gmarginals)%in%names(gsynmarginals)]
# gsynmarginals<-gsynmarginals[match(names(gmarginals),names(gsynmarginals))]
# #Scenario
# Scenario<-"S16-hCMA_CT"
# #Ip
# Iph<-round(sum(as.numeric(ghhmarginals$HH_ID[-1:-2])*as.numeric(ghhmarginals$HH_ID[-1:-2])*100/rhhmarginals$rHH_ID[-1:-2])/(sum(ghhmarginals$HH_ID[-1:-2])),2)
# Ipp<-round(sum(as.numeric(gppmarginals$PP_ID[-1:-2])*as.numeric(gppmarginals$PP_ID[-1:-2])*100/rppmarginals$rPP_ID[-1:-2])/(sum(gppmarginals$PP_ID[-1:-2])),2)
# #REGION absolute differences on total households, persons and agents synthesized (gamma)
# rgammahh<-abs(rmarginals$rHH_ID[-1:-2]-rsynmarginals$rHH_ID[-1:-2])
# rgammapp<-abs(rmarginals$rPP_ID[-1:-2]-rsynmarginals$rPP_ID[-1:-2])
# rgammatot<-rgammahh + rgammapp
# #REGION relative differences on total households, persons and agents synthesized (relgamma)
# rrelgammahh<-round(abs(rmarginals$rHH_ID[-1:-2]-rsynmarginals$rHH_ID[-1:-2])*100/rmarginals$rHH_ID[-1:-2],5)
# rrelgammapp<-round(abs(rmarginals$rPP_ID[-1:-2]-rsynmarginals$rPP_ID[-1:-2])*100/rmarginals$rPP_ID[-1:-2],5)
# rrelgammatot<-round(rgammatot*100/(rmarginals$rHH_ID[-1:-2]+rmarginals$rPP_ID[-1:-2]),5)
# #REGION total absolute errors (beta)
# rbetahh<-sum(abs(rmarginals[-1:-2,grepl("rHH_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rHH_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))]))
# rbetapp<-sum(abs(rmarginals[-1:-2,grepl("rPP_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rPP_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))]))
# rbetatot<-rbetahh + rbetapp
# #REGION relative total absolute errors (rrelbeta)
# rrelbetahh<-round(sum(abs(rmarginals[-1:-2,grepl("rHH_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rHH_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))]))*100/rmarginals$rHH_ID[-1:-2],2)
# rrelbetapp<-round(sum(abs(rmarginals[-1:-2,grepl("rPP_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rPP_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))]))*100/rmarginals$rPP_ID[-1:-2],2)
# rrelbetatot<-round((sum(abs(rmarginals[-1:-2,grepl("rHH_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rHH_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))])) + sum(abs(rmarginals[-1:-2,grepl("rPP_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rPP_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))])))*100/(rmarginals$rHH_ID[-1:-2] + rmarginals$rPP_ID[-1:-2]),2)
# #REGION absolute errors on households, persons and agents per category (alpha)
# ralphahh<-abs(rmarginals[-1:-2,grepl("rHH_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rHH_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))])
# ralphapp<-abs(rmarginals[-1:-2,grepl("rPP_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rPP_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))])
# ralpha<-abs(rmarginals[-1:-2,]-rsynmarginals[-1:-2,])
# ralpha<-data.frame(t(ralpha))
# ralpha<-cbind(rownames(ralpha),ralpha)
# ralpha$`rownames(ralpha)`[grepl(".1",ralpha$`rownames(ralpha)`)]<-substr(ralpha$`rownames(ralpha)`[grepl(".1",ralpha$`rownames(ralpha)`)],1,nchar(ralpha$`rownames(ralpha)`[grepl(".1",ralpha$`rownames(ralpha)`)])-2)
# ralpha$`rownames(ralpha)`[grepl(".2",ralpha$`rownames(ralpha)`)]<-substr(ralpha$`rownames(ralpha)`[grepl(".2",ralpha$`rownames(ralpha)`)],1,nchar(ralpha$`rownames(ralpha)`[grepl(".2",ralpha$`rownames(ralpha)`)])-2)
# ralpha$`rownames(ralpha)`[grepl(".3",ralpha$`rownames(ralpha)`)]<-substr(ralpha$`rownames(ralpha)`[grepl(".3",ralpha$`rownames(ralpha)`)],1,nchar(ralpha$`rownames(ralpha)`[grepl(".3",ralpha$`rownames(ralpha)`)])-2)
# ralpha$`rownames(ralpha)`[grepl(".4",ralpha$`rownames(ralpha)`)]<-substr(ralpha$`rownames(ralpha)`[grepl(".4",ralpha$`rownames(ralpha)`)],1,nchar(ralpha$`rownames(ralpha)`[grepl(".4",ralpha$`rownames(ralpha)`)])-2)
# ralpha<-aggregate(ralpha$X3, by=list(ralpha$`rownames(ralpha)`), FUN=sum)
# names(ralpha)<-c("Variable","S16-hCMA_CT")
# ralpha<-ralpha[!grepl("ID",ralpha$Variable),]
# #REGION relative absolute errors on households, persons and agents per category (rrelalpha)
# rrelalpha<-ralpha
# rrelalpha[,2]<-round(ralpha[,2]*100/rmarginals$rHH_ID[-1:-2],2)
# #Results
# Results1<-cbind(Results1,"S16-hCMA_CT"=ralpha[,2])
# Results2<-cbind(Results2,"S16-hCMA_CT"=rrelalpha[,2])
# Results3<-rbind(Results3,data.frame(Scenario, 
#                                     Iph, Ipp, 
#                                     rgammahh, rgammapp, rgammatot,
#                                     rrelgammahh, rrelgammapp, rrelgammatot,
#                                     rbetahh, rbetapp, rbetatot,
#                                     rrelbetahh, rrelbetapp, rrelbetatot
# ))
# 
# #S17-hDA
# #Import Results
# rsynmarginals<-read.csv("Exported/A1/3. Analysis/Toronto/S17-hDA/summary_region.csv")
# rsynmarginals[-1:-2,][is.na(rsynmarginals[-1:-2,])]<-0
# rsynmarginals<-rsynmarginals[,grepl("r",names(rsynmarginals))]
# temp<-rsynmarginals[1:3,]
# temp[3,]<-colSums(rsynmarginals[-1:-2,])
# rsynmarginals<-temp
# rsynmarginals<-rsynmarginals[match(names(rmarginals),names(rsynmarginals))]
# ghhmarginals<-read.csv("Exported/A1/3. Analysis/Toronto/S17-hDA/geo_household_marginals.csv")
# gppmarginals<-read.csv("Exported/A1/3. Analysis/Toronto/S17-hDA/geo_person_marginals.csv")
# gmarginals<-cbind(ghhmarginals,gppmarginals[,-1])
# gsynmarginals<-read.csv("Exported/A1/3. Analysis/Toronto/S17-hDA/summary_geo.csv")
# gsynmarginals<-gsynmarginals[,!grepl("r",names(gsynmarginals))]
# gmarginals<-gmarginals[,names(gmarginals)%in%names(gsynmarginals)]
# gsynmarginals<-gsynmarginals[match(names(gmarginals),names(gsynmarginals))]
# #Scenario
# Scenario<-"S17-hDA"
# #Ip
# Iph<-round(sum(as.numeric(ghhmarginals$HH_ID[-1:-2])*as.numeric(ghhmarginals$HH_ID[-1:-2])*100/rhhmarginals$rHH_ID[-1:-2])/(sum(ghhmarginals$HH_ID[-1:-2])),2)
# Ipp<-round(sum(as.numeric(gppmarginals$PP_ID[-1:-2])*as.numeric(gppmarginals$PP_ID[-1:-2])*100/rppmarginals$rPP_ID[-1:-2])/(sum(gppmarginals$PP_ID[-1:-2])),2)
# #REGION absolute differences on total households, persons and agents synthesized (gamma)
# rgammahh<-abs(rmarginals$rHH_ID[-1:-2]-rsynmarginals$rHH_ID[-1:-2])
# rgammapp<-abs(rmarginals$rPP_ID[-1:-2]-rsynmarginals$rPP_ID[-1:-2])
# rgammatot<-rgammahh + rgammapp
# #REGION relative differences on total households, persons and agents synthesized (relgamma)
# rrelgammahh<-round(abs(rmarginals$rHH_ID[-1:-2]-rsynmarginals$rHH_ID[-1:-2])*100/rmarginals$rHH_ID[-1:-2],5)
# rrelgammapp<-round(abs(rmarginals$rPP_ID[-1:-2]-rsynmarginals$rPP_ID[-1:-2])*100/rmarginals$rPP_ID[-1:-2],5)
# rrelgammatot<-round(rgammatot*100/(rmarginals$rHH_ID[-1:-2]+rmarginals$rPP_ID[-1:-2]),5)
# #REGION total absolute errors (beta)
# rbetahh<-sum(abs(rmarginals[-1:-2,grepl("rHH_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rHH_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))]))
# rbetapp<-sum(abs(rmarginals[-1:-2,grepl("rPP_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rPP_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))]))
# rbetatot<-rbetahh + rbetapp
# #REGION relative total absolute errors (rrelbeta)
# rrelbetahh<-round(sum(abs(rmarginals[-1:-2,grepl("rHH_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rHH_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))]))*100/rmarginals$rHH_ID[-1:-2],2)
# rrelbetapp<-round(sum(abs(rmarginals[-1:-2,grepl("rPP_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rPP_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))]))*100/rmarginals$rPP_ID[-1:-2],2)
# rrelbetatot<-round((sum(abs(rmarginals[-1:-2,grepl("rHH_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rHH_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))])) + sum(abs(rmarginals[-1:-2,grepl("rPP_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rPP_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))])))*100/(rmarginals$rHH_ID[-1:-2] + rmarginals$rPP_ID[-1:-2]),2)
# #REGION absolute errors on households, persons and agents per category (alpha)
# ralphahh<-abs(rmarginals[-1:-2,grepl("rHH_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rHH_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))])
# ralphapp<-abs(rmarginals[-1:-2,grepl("rPP_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rPP_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))])
# ralpha<-abs(rmarginals[-1:-2,]-rsynmarginals[-1:-2,])
# ralpha<-data.frame(t(ralpha))
# ralpha<-cbind(rownames(ralpha),ralpha)
# ralpha$`rownames(ralpha)`[grepl(".1",ralpha$`rownames(ralpha)`)]<-substr(ralpha$`rownames(ralpha)`[grepl(".1",ralpha$`rownames(ralpha)`)],1,nchar(ralpha$`rownames(ralpha)`[grepl(".1",ralpha$`rownames(ralpha)`)])-2)
# ralpha$`rownames(ralpha)`[grepl(".2",ralpha$`rownames(ralpha)`)]<-substr(ralpha$`rownames(ralpha)`[grepl(".2",ralpha$`rownames(ralpha)`)],1,nchar(ralpha$`rownames(ralpha)`[grepl(".2",ralpha$`rownames(ralpha)`)])-2)
# ralpha$`rownames(ralpha)`[grepl(".3",ralpha$`rownames(ralpha)`)]<-substr(ralpha$`rownames(ralpha)`[grepl(".3",ralpha$`rownames(ralpha)`)],1,nchar(ralpha$`rownames(ralpha)`[grepl(".3",ralpha$`rownames(ralpha)`)])-2)
# ralpha$`rownames(ralpha)`[grepl(".4",ralpha$`rownames(ralpha)`)]<-substr(ralpha$`rownames(ralpha)`[grepl(".4",ralpha$`rownames(ralpha)`)],1,nchar(ralpha$`rownames(ralpha)`[grepl(".4",ralpha$`rownames(ralpha)`)])-2)
# ralpha<-aggregate(ralpha$X3, by=list(ralpha$`rownames(ralpha)`), FUN=sum)
# names(ralpha)<-c("Variable","S17-hDA")
# ralpha<-ralpha[!grepl("ID",ralpha$Variable),]
# #REGION relative absolute errors on households, persons and agents per category (rrelalpha)
# rrelalpha<-ralpha
# rrelalpha[,2]<-round(ralpha[,2]*100/rmarginals$rHH_ID[-1:-2],2)
# #Results
# Results1<-cbind(Results1,"S17-hDA"=ralpha[,2])
# Results2<-cbind(Results2,"S17-hDA"=rrelalpha[,2])
# Results3<-rbind(Results3,data.frame(Scenario, 
#                                     Iph, Ipp, 
#                                     rgammahh, rgammapp, rgammatot,
#                                     rrelgammahh, rrelgammapp, rrelgammatot,
#                                     rbetahh, rbetapp, rbetatot,
#                                     rrelbetahh, rrelbetapp, rrelbetatot
# ))
# 
# #S18-hCMA_DA
# #Import Results
# rsynmarginals<-read.csv("Exported/A1/3. Analysis/Toronto/S18-hCMA_DA/summary_region.csv")
# rsynmarginals[-1:-2,][is.na(rsynmarginals[-1:-2,])]<-0
# rsynmarginals<-rsynmarginals[,grepl("r",names(rsynmarginals))]
# rsynmarginals<-rsynmarginals[match(names(rmarginals),names(rsynmarginals))]
# ghhmarginals<-read.csv("Exported/A1/3. Analysis/Toronto/S18-hCMA_DA/geo_household_marginals.csv")
# gppmarginals<-read.csv("Exported/A1/3. Analysis/Toronto/S18-hCMA_DA/geo_person_marginals.csv")
# gmarginals<-cbind(ghhmarginals,gppmarginals[,-1])
# gsynmarginals<-read.csv("Exported/A1/3. Analysis/Toronto/S18-hCMA_DA/summary_geo.csv")
# gsynmarginals<-gsynmarginals[,!grepl("r",names(gsynmarginals))]
# gmarginals<-gmarginals[,names(gmarginals)%in%names(gsynmarginals)]
# gsynmarginals<-gsynmarginals[match(names(gmarginals),names(gsynmarginals))]
# #Scenario
# Scenario<-"S18-hCMA_DA"
# #Ip
# Iph<-round(sum(as.numeric(ghhmarginals$HH_ID[-1:-2])*as.numeric(ghhmarginals$HH_ID[-1:-2])*100/rhhmarginals$rHH_ID[-1:-2])/(sum(ghhmarginals$HH_ID[-1:-2])),2)
# Ipp<-round(sum(as.numeric(gppmarginals$PP_ID[-1:-2])*as.numeric(gppmarginals$PP_ID[-1:-2])*100/rppmarginals$rPP_ID[-1:-2])/(sum(gppmarginals$PP_ID[-1:-2])),2)
# #REGION absolute differences on total households, persons and agents synthesized (gamma)
# rgammahh<-abs(rmarginals$rHH_ID[-1:-2]-rsynmarginals$rHH_ID[-1:-2])
# rgammapp<-abs(rmarginals$rPP_ID[-1:-2]-rsynmarginals$rPP_ID[-1:-2])
# rgammatot<-rgammahh + rgammapp
# #REGION relative differences on total households, persons and agents synthesized (relgamma)
# rrelgammahh<-round(abs(rmarginals$rHH_ID[-1:-2]-rsynmarginals$rHH_ID[-1:-2])*100/rmarginals$rHH_ID[-1:-2],5)
# rrelgammapp<-round(abs(rmarginals$rPP_ID[-1:-2]-rsynmarginals$rPP_ID[-1:-2])*100/rmarginals$rPP_ID[-1:-2],5)
# rrelgammatot<-round(rgammatot*100/(rmarginals$rHH_ID[-1:-2]+rmarginals$rPP_ID[-1:-2]),5)
# #REGION total absolute errors (beta)
# rbetahh<-sum(abs(rmarginals[-1:-2,grepl("rHH_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rHH_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))]))
# rbetapp<-sum(abs(rmarginals[-1:-2,grepl("rPP_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rPP_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))]))
# rbetatot<-rbetahh + rbetapp
# #REGION relative total absolute errors (rrelbeta)
# rrelbetahh<-round(sum(abs(rmarginals[-1:-2,grepl("rHH_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rHH_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))]))*100/rmarginals$rHH_ID[-1:-2],2)
# rrelbetapp<-round(sum(abs(rmarginals[-1:-2,grepl("rPP_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rPP_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))]))*100/rmarginals$rPP_ID[-1:-2],2)
# rrelbetatot<-round((sum(abs(rmarginals[-1:-2,grepl("rHH_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rHH_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))])) + sum(abs(rmarginals[-1:-2,grepl("rPP_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rPP_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))])))*100/(rmarginals$rHH_ID[-1:-2] + rmarginals$rPP_ID[-1:-2]),2)
# #REGION absolute errors on households, persons and agents per category (alpha)
# ralphahh<-abs(rmarginals[-1:-2,grepl("rHH_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rHH_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))])
# ralphapp<-abs(rmarginals[-1:-2,grepl("rPP_",names(rmarginals)) & !grepl("ID", names(rmarginals))]-rsynmarginals[-1:-2,grepl("rPP_",names(rsynmarginals)) & !grepl("ID", names(rsynmarginals))])
# ralpha<-abs(rmarginals[-1:-2,]-rsynmarginals[-1:-2,])
# ralpha<-data.frame(t(ralpha))
# ralpha<-cbind(rownames(ralpha),ralpha)
# ralpha$`rownames(ralpha)`[grepl(".1",ralpha$`rownames(ralpha)`)]<-substr(ralpha$`rownames(ralpha)`[grepl(".1",ralpha$`rownames(ralpha)`)],1,nchar(ralpha$`rownames(ralpha)`[grepl(".1",ralpha$`rownames(ralpha)`)])-2)
# ralpha$`rownames(ralpha)`[grepl(".2",ralpha$`rownames(ralpha)`)]<-substr(ralpha$`rownames(ralpha)`[grepl(".2",ralpha$`rownames(ralpha)`)],1,nchar(ralpha$`rownames(ralpha)`[grepl(".2",ralpha$`rownames(ralpha)`)])-2)
# ralpha$`rownames(ralpha)`[grepl(".3",ralpha$`rownames(ralpha)`)]<-substr(ralpha$`rownames(ralpha)`[grepl(".3",ralpha$`rownames(ralpha)`)],1,nchar(ralpha$`rownames(ralpha)`[grepl(".3",ralpha$`rownames(ralpha)`)])-2)
# ralpha$`rownames(ralpha)`[grepl(".4",ralpha$`rownames(ralpha)`)]<-substr(ralpha$`rownames(ralpha)`[grepl(".4",ralpha$`rownames(ralpha)`)],1,nchar(ralpha$`rownames(ralpha)`[grepl(".4",ralpha$`rownames(ralpha)`)])-2)
# ralpha<-aggregate(ralpha$X3, by=list(ralpha$`rownames(ralpha)`), FUN=sum)
# names(ralpha)<-c("Variable","S18-hCMA_DA")
# ralpha<-ralpha[!grepl("ID",ralpha$Variable),]
# #REGION relative absolute errors on households, persons and agents per category (rrelalpha)
# rrelalpha<-ralpha
# rrelalpha[,2]<-round(ralpha[,2]*100/rmarginals$rHH_ID[-1:-2],2)
# #Results
# Results1<-cbind(Results1,"S18-hCMA_DA"=ralpha[,2])
# Results2<-cbind(Results2,"S18-hCMA_DA"=rrelalpha[,2])
# Results3<-rbind(Results3,data.frame(Scenario, 
#                                     Iph, Ipp, 
#                                     rgammahh, rgammapp, rgammatot,
#                                     rrelgammahh, rrelgammapp, rrelgammatot,
#                                     rbetahh, rbetapp, rbetatot,
#                                     rrelbetahh, rrelbetapp, rrelbetatot
# ))
# 
# Results1
# Results2
# Results3
# write.csv(Results1,"Exported/A1/3. Analysis/Results1_Toronto.csv")
# write.csv(Results2,"Exported/A1/3. Analysis/Results2_Toronto.csv")
# write.csv(Results3,"Exported/A1/3. Analysis/Results3_Toronto.csv")

# #Create scenarios directories
# OutputCopy<-function(Region){
#   scenarios<-c("S1-bCMA","S2-bCSD","S3-bCMA_CSD","S4-bADA","S5-bCMA_ADA","S6-bCT","S7-bCMA_CT","S8-bDA","S9-bCMA_DA","S10-hCMA","S11-hCSD","S12-hCMA_CSD","S13-hADA","S14-hCMA_ADA","S15-hCT","S16-hCMA_CT","S17-hDA","S18-hCMA_DA")
#   for (i in 1:length(scenarios)){
#     Scenario<-scenarios[i]
#     dir.create(paste("Exported/A1/3. Analysis/",deparse(substitute(Region)),"/",Scenario,sep=""))
#     
#     current_folder <- paste("Exported/A1/2. Popgen/PopGen2.1 - AGE & INC less categories/",substr(deparse(substitute(Region)),1,1),Scenario,"/",list.files(paste("Exported/A1/2. Popgen/PopGen2.1 - AGE & INC less categories/",substr(deparse(substitute(Region)),1,1),Scenario,sep=""),Scenario),sep="")
#     new_folder <- paste("Exported/A1/3. Analysis/",deparse(substitute(Region)),"/",Scenario,sep="")
#     list_of_files <- list.files(current_folder,"summary")
#     file.copy(file.path(current_folder,list_of_files), new_folder)
#     
#     
#     current_folder <- paste("Exported/A1/2. Popgen/PopGen2.1 - AGE & INC less categories/",substr(deparse(substitute(Region)),1,1),Scenario,sep="")
#     new_folder <- paste("Exported/A1/3. Analysis/",deparse(substitute(Region)),"/",Scenario,sep="")
#     list_of_files <- list.files(current_folder,"marginals")
#     file.copy(file.path(current_folder,list_of_files), new_folder)
#     new_folder <- paste("Exported/A1/3. Analysis/",deparse(substitute(Region)),"/",Scenario,sep="")
#     list_of_files <- list.files(current_folder,"region_geo_mapping")
#     file.copy(file.path(current_folder,list_of_files), new_folder)
#   }
# }
# OutputCopy(Toronto)
# OutputCopy(Toronto)
# OutputCopy(Toronto)
#
#Test Results
# weightsMTLS4<-read.csv("C:/Users/mkhachman/Desktop/PopGen2.1/PopGen_Practice_Package/Toronto_S4/2021-02-21 03-16-57 S4_hCMA_DA/weights.csv")
# weightsMTLS5<-read.csv("C:/Users/mkhachman/Desktop/PopGen2.1/PopGen_Practice_Package/Toronto_S5/2021-02-21 03-43-05 S5_bCT_DA/weights.csv")
# weightsMTLS6<-read.csv("C:/Users/mkhachman/Desktop/PopGen2.1/PopGen_Practice_Package/Toronto_S6/2021-02-21 03-42-42 S6_hCT_DA/weights.csv")
# weightsMTLS4[is.na(weightsMTLS4)]
# weightsMTLS5[is.na(weightsMTLS5)]
# weightsMTLS6[is.na(weightsMTLS6)]
# 
# weightsTORS4<-read.csv("C:/Users/mkhachman/Desktop/PopGen2.1/PopGen_Practice_Package/Toronto_S4/2021-02-21 08-44-02 S4_hCMA_DA/weights.csv")
# weightsTORS5<-read.csv("C:/Users/mkhachman/Desktop/PopGen2.1/PopGen_Practice_Package/Toronto_S5/2021-02-21 09-43-43 S5_bCT_DA/weights.csv")
# weightsTORS6<-read.csv("C:/Users/mkhachman/Desktop/PopGen2.1/PopGen_Practice_Package/Toronto_S6/2021-02-21 09-40-29 S6_hCT_DA/weights.csv")
# weightsTORS4[is.na(weightsTORS4)]
# weightsTORS5[is.na(weightsTORS5)]
# weightsTORS6[is.na(weightsTORS6)]
# 
# weightsVANS4<-read.csv("C:/Users/mkhachman/Desktop/PopGen2.1/PopGen_Practice_Package/Toronto_S4/2021-02-21 11-53-17 S4_hCMA_DA/weights.csv")
# weightsVANS5<-read.csv("C:/Users/mkhachman/Desktop/PopGen2.1/PopGen_Practice_Package/Toronto_S5/2021-02-21 11-28-06 S5_bCT_DA/weights.csv")
# weightsVANS6<-read.csv("C:/Users/mkhachman/Desktop/PopGen2.1/PopGen_Practice_Package/Toronto_S6/2021-02-21 11-22-14 S6_hCT_DA/weights.csv")
# weightsVANS4[is.na(weightsVANS4)]
# weightsVANS5[is.na(weightsVANS5)]
# weightsVANS6[is.na(weightsVANS6)]
# 
# weightsMTLS1<-read.csv("C:/Users/mkhachman/Desktop/PopGen2.1/PopGen_Practice_Package/Toronto_S1/2021-02-21 06-22-48 S1_bCMA_CT/weights.csv")
# weightsMTLS2<-read.csv("C:/Users/mkhachman/Desktop/PopGen2.1/PopGen_Practice_Package/Toronto_S2/2021-02-21 06-23-11 S2_hCMA_CT/weights.csv")
# weightsMTLS3<-read.csv("C:/Users/mkhachman/Desktop/PopGen2.1/PopGen_Practice_Package/Toronto_S3/2021-02-21 08-54-41 S3_bCMA_DA/weights.csv")
# weightsMTLS1[is.na(weightsMTLS1)]
# weightsMTLS2[is.na(weightsMTLS2)]
# weightsMTLS3[is.na(weightsMTLS3)]
# 
# weightsTORS1<-read.csv("C:/Users/mkhachman/Desktop/PopGen2.1/PopGen_Practice_Package/Toronto_S1/2021-02-21 07-15-43 S1_bCMA_CT/weights.csv")
# weightsTORS2<-read.csv("C:/Users/mkhachman/Desktop/PopGen2.1/PopGen_Practice_Package/Toronto_S2/2021-02-21 08-21-09 S2_hCMA_CT/weights.csv")
# weightsTORS3<-read.csv("C:/Users/mkhachman/Desktop/PopGen2.1/PopGen_Practice_Package/Toronto_S3/2021-02-21 11-55-13 S3_bCMA_DA/weights.csv")
# weightsTORS1[is.na(weightsTORS1)]
# weightsTORS2[is.na(weightsTORS2)]
# weightsTORS3[is.na(weightsTORS3)]
# 
# weightsVANS1<-read.csv("/weights.csv")
# weightsVANS2<-read.csv("C:/Users/mkhachman/Desktop/PopGen2.1/PopGen_Practice_Package/Toronto_S2/2021-02-21 08-35-15 S2_hCMA_CT/weights.csv")
# weightsVANS3<-read.csv("C:/Users/mkhachman/Desktop/PopGen2.1/PopGen_Practice_Package/Toronto_S3/2021-02-21 12-41-03 S3_bCMA_DA/weights.csv")
# weightsVANS1[is.na(weightsVANS1)]
# weightsVANS2[is.na(weightsVANS2)]
# weightsVANS3[is.na(weightsVANS3)]
# 
# hsynthetic<-read.csv("C:/Users/mkhachman/Desktop/Toronto/Scenarios/S1 - bCMA_CT/2021-02-19 21-18-53 S1_bCMA_CT/housing_synthetic.csv")
# 
# max(hsynthetic$geo)
# 
# dim(hsynthetic)
# 
# View(weights)
# 
# weights[is.na(weights)]
# install.packages("ipfr")