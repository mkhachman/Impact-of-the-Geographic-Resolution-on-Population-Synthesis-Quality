###Load packages
suppressPackageStartupMessages({
  library(ipfr)
  library(dplyr)
  library(tidyverse)
  library(yaml)
})

setwd("D:/OneDrive - polymtl.ca/Studies/Civil engineering/POLY/Projet Doctorat/PSP")

###POPGEN
#Create scenarios directories
dir.create("Exported/A1/Montreal/Scenarios/S1-bCMA")
dir.create("Exported/A1/Montreal/Scenarios/S2-bCSD")
dir.create("Exported/A1/Montreal/Scenarios/S3-bCMA_CSD")
dir.create("Exported/A1/Montreal/Scenarios/S4-bADA")
dir.create("Exported/A1/Montreal/Scenarios/S5-bCMA_ADA")
dir.create("Exported/A1/Montreal/Scenarios/S6-bCT")
dir.create("Exported/A1/Montreal/Scenarios/S7-bCMA_CT")
dir.create("Exported/A1/Montreal/Scenarios/S8-bDA")
dir.create("Exported/A1/Montreal/Scenarios/S9-bCMA_DA")
dir.create("Exported/A1/Montreal/Scenarios/S10-hCMA")
dir.create("Exported/A1/Montreal/Scenarios/S11-hCSD")
dir.create("Exported/A1/Montreal/Scenarios/S12-hCMA_CSD")
dir.create("Exported/A1/Montreal/Scenarios/S13-hADA")
dir.create("Exported/A1/Montreal/Scenarios/S14-hCMA_ADA")
dir.create("Exported/A1/Montreal/Scenarios/S15-hCT")
dir.create("Exported/A1/Montreal/Scenarios/S16-hCMA_CT")
dir.create("Exported/A1/Montreal/Scenarios/S17-hDA")
dir.create("Exported/A1/Montreal/Scenarios/S18-hCMA_DA")

###Import inputs
CMAMAT<-read.csv("Exported/A1/Montreal/Inputs/Basic Census Totals/CMAMAT.csv")[,-1]
CSDMAT<-read.csv("Exported/A1/Montreal/Inputs/Basic Census Totals/CSDMAT.csv")[,-1]
ADAMAT<-read.csv("Exported/A1/Montreal/Inputs/Basic Census Totals/ADAMAT.csv")[,-1]
CTMAT<-read.csv("Exported/A1/Montreal/Inputs/Basic Census Totals/CTMAT.csv")[,-1]
DAMAT<-read.csv("Exported/A1/Montreal/Inputs/Basic Census Totals/DAMAT.csv")[,-1]
hPUMF<-read.csv("Exported/A1/Montreal/Inputs/Harmonized PUMF/PUMF2.0.csv")[,-1]
GIS_CMA_CSD<-read.csv("Exported/A1/Montreal/Inputs/GIS/GIS_CMA_CSD.csv")[,-1]
GIS_CMA_ADA<-read.csv("Exported/A1/Montreal/Inputs/GIS/GIS_CMA_ADA.csv")[,-1]
GIS_CMA_CT<-read.csv("Exported/A1/Montreal/Inputs/GIS/GIS_CMA_CT.csv")[,-1]
GIS_CMA_DA<-read.csv("Exported/A1/Montreal/Inputs/GIS/GIS_CMA_DA.csv")[,-1]

###Define variables importance
#Household variables
hh_variable_importance<-data.frame(Variable=unique(CMAMAT$Variable[grepl("HH_",CMAMAT$Variable)]),Importance=NA)
hh_variable_importance$Importance[hh_variable_importance$Variable=="HH_ID"]<-7
hh_variable_importance$Importance[hh_variable_importance$Variable=="HH_SIZE"]<-6
hh_variable_importance$Importance[hh_variable_importance$Variable=="HH_TYPE"]<-5
hh_variable_importance$Importance[hh_variable_importance$Variable=="HH_TOTINCAT"]<-4
hh_variable_importance$Importance[hh_variable_importance$Variable=="HH_NMAIN"]<-0
hh_variable_importance$Importance[hh_variable_importance$Variable=="HH_DBUILT"]<-1
hh_variable_importance$Importance[hh_variable_importance$Variable=="HH_DTYPE"]<-2
hh_variable_importance$Importance[hh_variable_importance$Variable=="HH_DNROOM"]<-3
hh_variable_importance$Importance[hh_variable_importance$Variable=="HH_DNBEDRM"]<-0
#Person variables
pp_variable_importance<-data.frame(Variable=unique(CMAMAT$Variable[grepl("PP_",CMAMAT$Variable)]),Importance=NA)
pp_variable_importance$Importance[pp_variable_importance$Variable=="PP_ID"]<-8
pp_variable_importance$Importance[pp_variable_importance$Variable=="PP_SEX"]<-7
pp_variable_importance$Importance[pp_variable_importance$Variable=="PP_AGEGRP"]<-6
pp_variable_importance$Importance[pp_variable_importance$Variable=="PP_MARST"]<-5
pp_variable_importance$Importance[pp_variable_importance$Variable=="PP_IMMSTAT"]<-0
pp_variable_importance$Importance[pp_variable_importance$Variable=="PP_MTN"]<-0
pp_variable_importance$Importance[pp_variable_importance$Variable=="PP_HDGREE"]<-0
pp_variable_importance$Importance[pp_variable_importance$Variable=="PP_LFST"]<-0
pp_variable_importance$Importance[pp_variable_importance$Variable=="PP_WRKACT"]<-0
pp_variable_importance$Importance[pp_variable_importance$Variable=="PP_IND"]<-1
pp_variable_importance$Importance[pp_variable_importance$Variable=="PP_INCDECILE"]<-0
pp_variable_importance$Importance[pp_variable_importance$Variable=="PP_JTWLEAVE"]<-2
pp_variable_importance$Importance[pp_variable_importance$Variable=="PP_JTWDUR"]<-3
pp_variable_importance$Importance[pp_variable_importance$Variable=="PP_JTWMODE"]<-4

###Samples
#household_sample
household_sample<-data.frame(hid=hPUMF$HH_ID,sample_geo=1)
for (i in 1:max(hh_variable_importance$Importance)){
  household_sample<-cbind(household_sample,hPUMF[,names(hPUMF)==hh_variable_importance$Variable[hh_variable_importance$Importance==i]],hPUMF[,names(hPUMF)==hh_variable_importance$Variable[hh_variable_importance$Importance==i]])
  names(household_sample)[ncol(household_sample)-1]<-hh_variable_importance$Variable[hh_variable_importance$Importance==i]
  names(household_sample)[ncol(household_sample)]<-paste("r",hh_variable_importance$Variable[hh_variable_importance$Importance==i],sep="")
}
household_sample$HH_ID<-1
household_sample$rHH_ID<-1
household_sample<-household_sample[!duplicated(household_sample$"hid"),]
temp<-household_sample
household_sample$HH_TOTINCAT[temp$HH_TOTINCAT%in%c(1:6)]<-1
household_sample$HH_TOTINCAT[temp$HH_TOTINCAT%in%c(7:11)]<-2
household_sample$HH_TOTINCAT[temp$HH_TOTINCAT%in%c(12:15)]<-3
household_sample$HH_TOTINCAT[temp$HH_TOTINCAT%in%c(16)]<-4
household_sample$rHH_TOTINCAT[temp$rHH_TOTINCAT%in%c(1:6)]<-1
household_sample$rHH_TOTINCAT[temp$rHH_TOTINCAT%in%c(7:11)]<-2
household_sample$rHH_TOTINCAT[temp$rHH_TOTINCAT%in%c(12:15)]<-3
household_sample$rHH_TOTINCAT[temp$rHH_TOTINCAT%in%c(16)]<-4
#person_sample
person_sample<-data.frame(hid=hPUMF$HH_ID,pid=hPUMF$PP_ID,sample_geo=1)
for (i in 1:max(pp_variable_importance$Importance)){
  person_sample<-cbind(person_sample,hPUMF[,names(hPUMF)==pp_variable_importance$Variable[pp_variable_importance$Importance==i]],hPUMF[,names(hPUMF)==pp_variable_importance$Variable[pp_variable_importance$Importance==i]])
  names(person_sample)[ncol(person_sample)-1]<-pp_variable_importance$Variable[pp_variable_importance$Importance==i]
  names(person_sample)[ncol(person_sample)]<-paste("r",pp_variable_importance$Variable[pp_variable_importance$Importance==i],sep="")
}
person_sample$PP_ID<-1
person_sample$rPP_ID<-1
temp<-person_sample
person_sample$PP_AGEGRP[temp$PP_AGEGRP%in%c(1,2)]<-1
person_sample$PP_AGEGRP[temp$PP_AGEGRP%in%c(3:11)]<-2
person_sample$PP_AGEGRP[temp$PP_AGEGRP%in%c(12,13)]<-3
person_sample$rPP_AGEGRP[temp$rPP_AGEGRP%in%c(1,2)]<-1
person_sample$rPP_AGEGRP[temp$rPP_AGEGRP%in%c(3:11)]<-2
person_sample$rPP_AGEGRP[temp$rPP_AGEGRP%in%c(12,13)]<-3

###S1-bCMA
GIS<-GIS_CMA_CSD
#region_geo_mapping
geo<-1:length(unique(GIS$CMA))
region<-1:length(unique(GIS$CMA))
region_geo_mapping<-data.frame(region,geo)
write.csv(region_geo_mapping,"Exported/A1/Montreal/Scenarios/S1-bCMA/region_geo_mapping.csv",row.names=FALSE)
#geo_sample_mapping
geo<-1:length(unique(GIS$CMA))
sample_geo<-1
geo_sample_mapping<-data.frame(geo,sample_geo)
write.csv(geo_sample_mapping,"Exported/A1/Montreal/Scenarios/S1-bCMA/geo_sample_mapping.csv",row.names=FALSE)
#region_sample_mapping
region<-1:length(unique(GIS$CMA))
sample_geo<--1
region_sample_mapping<-data.frame(region,sample_geo)
write.csv(region_sample_mapping,"Exported/A1/Montreal/Scenarios/S1-bCMA/region_sample_mapping.csv",row.names=FALSE)
#region_household_marginals
region_household_marginals<-merge(CMAMAT[grepl("HH_",CMAMAT$Variable),],hh_variable_importance,by="Variable",all.x=TRUE)
region_household_marginals<-region_household_marginals[order(region_household_marginals$Importance),]
region_household_marginals<-region_household_marginals[region_household_marginals$Importance!=0,!names(region_household_marginals)%in%c("X","Definition","Description","Frequency","Importance")]
region_household_marginals$Categories[region_household_marginals$Variable=="HH_ID"]<-1
region_household_marginals<-rbind(c("variable_names","variable_categories",1:length(unique(GIS$CMA))),region_household_marginals)
region_household_marginals<-cbind(region_household_marginals[,1:2],c("region",rep("",nrow(region_household_marginals)-1)),region_household_marginals[,3:ncol(region_household_marginals)])
region_household_marginals$Variable[-1]<-paste("r",region_household_marginals$Variable[-1],sep="")
region_household_marginals<-data.frame(t(region_household_marginals))
temp<-region_household_marginals
region_household_marginals<-temp[,!((temp[1,]%in%c("rHH_TOTINCAT") & temp[2,]%in%c(5:16)))]
region_household_marginals[-1:-3,(region_household_marginals[1,]=="rHH_TOTINCAT" & region_household_marginals[2,]==1)]<-sum(apply(temp[-1:-3,temp[1,]=="rHH_TOTINCAT" & temp[2,]%in%c(1:6)],2,as.numeric))
region_household_marginals[-1:-3,(region_household_marginals[1,]=="rHH_TOTINCAT" & region_household_marginals[2,]==2)]<-sum(apply(temp[-1:-3,temp[1,]=="rHH_TOTINCAT" & temp[2,]%in%c(7:11)],2,as.numeric))
region_household_marginals[-1:-3,(region_household_marginals[1,]=="rHH_TOTINCAT" & region_household_marginals[2,]==3)]<-sum(apply(temp[-1:-3,temp[1,]=="rHH_TOTINCAT" & temp[2,]%in%c(12:15)],2,as.numeric))
region_household_marginals[-1:-3,(region_household_marginals[1,]=="rHH_TOTINCAT" & region_household_marginals[2,]==4)]<-as.numeric(temp[-1:-3,temp[1,]=="rHH_TOTINCAT" & temp[2,]%in%c(16)])
write_csv(region_household_marginals,"Exported/A1/Montreal/Scenarios/S1-bCMA/region_household_marginals.csv",col_names = FALSE)
#region_person_marginals
region_person_marginals<-merge(CMAMAT[grepl("PP_",CMAMAT$Variable),],pp_variable_importance,by="Variable",all.x=TRUE)
region_person_marginals<-region_person_marginals[order(region_person_marginals$Importance),]
region_person_marginals<-region_person_marginals[region_person_marginals$Importance!=0,!names(region_person_marginals)%in%c("X","Definition","Description","Frequency","Importance")]
region_person_marginals$Categories[region_person_marginals$Variable=="PP_ID"]<-1
region_person_marginals<-rbind(c("variable_names","variable_categories",1:length(unique(GIS$CMA))),region_person_marginals)
region_person_marginals<-cbind(region_person_marginals[,1:2],c("region",rep("",nrow(region_person_marginals)-1)),region_person_marginals[,3:ncol(region_person_marginals)])
region_person_marginals$Variable[-1]<-paste("r",region_person_marginals$Variable[-1],sep="")
region_person_marginals<-data.frame(t(region_person_marginals))
temp<-region_person_marginals
region_person_marginals<-temp[,!((temp[1,]%in%c("rPP_AGEGRP") & temp[2,]%in%c(4:13)))]
region_person_marginals[-1:-3,(region_person_marginals[1,]=="rPP_AGEGRP" & region_person_marginals[2,]==1)]<-sum(apply(temp[-1:-3,temp[1,]=="rPP_AGEGRP" & temp[2,]%in%c(1:2)],2,as.numeric))
region_person_marginals[-1:-3,(region_person_marginals[1,]=="rPP_AGEGRP" & region_person_marginals[2,]==2)]<-sum(apply(temp[-1:-3,temp[1,]=="rPP_AGEGRP" & temp[2,]%in%c(3:11)],2,as.numeric))
region_person_marginals[-1:-3,(region_person_marginals[1,]=="rPP_AGEGRP" & region_person_marginals[2,]==3)]<-sum(apply(temp[-1:-3,temp[1,]=="rPP_AGEGRP" & temp[2,]%in%c(12:13)],2,as.numeric))
write_csv(region_person_marginals,"Exported/A1/Montreal/Scenarios/S1-bCMA/region_person_marginals.csv",col_names = FALSE)
#geo_household_marginals
geo_household_marginals<-merge(CMAMAT[grepl("HH_",CMAMAT$Variable),],hh_variable_importance,by="Variable",all.x=TRUE)
geo_household_marginals<-geo_household_marginals[order(geo_household_marginals$Importance),]
geo_household_marginals<-geo_household_marginals[geo_household_marginals$Importance!=0,!names(geo_household_marginals)%in%c("X","Definition","Description","Frequency","Importance")]
geo_household_marginals$Categories[geo_household_marginals$Variable=="HH_ID"]<-1
geo_household_marginals<-rbind(c("variable_names","variable_categories",1:length(unique(GIS$CMA))),geo_household_marginals)
geo_household_marginals<-cbind(geo_household_marginals[,1:2],c("geo",rep("",nrow(geo_household_marginals)-1)),geo_household_marginals[,3:ncol(geo_household_marginals)])
geo_household_marginals$Variable[-1]<-geo_household_marginals$Variable[-1]
geo_household_marginals<-data.frame(t(geo_household_marginals))
temp<-geo_household_marginals
geo_household_marginals<-temp[,!((temp[1,]%in%c("HH_TOTINCAT") & temp[2,]%in%c(5:16)))]
geo_household_marginals[-1:-3,(geo_household_marginals[1,]=="HH_TOTINCAT" & geo_household_marginals[2,]==1)]<-sum(apply(temp[-1:-3,temp[1,]=="HH_TOTINCAT" & temp[2,]%in%c(1:6)],2,as.numeric))
geo_household_marginals[-1:-3,(geo_household_marginals[1,]=="HH_TOTINCAT" & geo_household_marginals[2,]==2)]<-sum(apply(temp[-1:-3,temp[1,]=="HH_TOTINCAT" & temp[2,]%in%c(7:11)],2,as.numeric))
geo_household_marginals[-1:-3,(geo_household_marginals[1,]=="HH_TOTINCAT" & geo_household_marginals[2,]==3)]<-sum(apply(temp[-1:-3,temp[1,]=="HH_TOTINCAT" & temp[2,]%in%c(12:15)],2,as.numeric))
geo_household_marginals[-1:-3,(geo_household_marginals[1,]=="HH_TOTINCAT" & geo_household_marginals[2,]==4)]<-as.numeric(temp[-1:-3,temp[1,]=="HH_TOTINCAT" & temp[2,]%in%c(16)])
write_csv(geo_household_marginals,"Exported/A1/Montreal/Scenarios/S1-bCMA/geo_household_marginals.csv",col_names = FALSE)
#geo_person_marginals
geo_person_marginals<-merge(CMAMAT[grepl("PP_",CMAMAT$Variable),],pp_variable_importance,by="Variable",all.x=TRUE)
geo_person_marginals<-geo_person_marginals[order(geo_person_marginals$Importance),]
geo_person_marginals<-geo_person_marginals[geo_person_marginals$Importance!=0,!names(geo_person_marginals)%in%c("X","Definition","Description","Frequency","Importance")]
geo_person_marginals$Categories[geo_person_marginals$Variable=="PP_ID"]<-1
geo_person_marginals<-rbind(c("variable_names","variable_categories",1:length(unique(GIS$CMA))),geo_person_marginals)
geo_person_marginals<-cbind(geo_person_marginals[,1:2],c("geo",rep("",nrow(geo_person_marginals)-1)),geo_person_marginals[,3:ncol(geo_person_marginals)])
geo_person_marginals$Variable[-1]<-geo_person_marginals$Variable[-1]
geo_person_marginals<-data.frame(t(geo_person_marginals))
temp<-geo_person_marginals
geo_person_marginals<-temp[,!((temp[1,]%in%c("PP_AGEGRP") & temp[2,]%in%c(4:13)))]
geo_person_marginals[-1:-3,(geo_person_marginals[1,]=="PP_AGEGRP" & geo_person_marginals[2,]==1)]<-sum(apply(temp[-1:-3,temp[1,]=="PP_AGEGRP" & temp[2,]%in%c(1:2)],2,as.numeric))
geo_person_marginals[-1:-3,(geo_person_marginals[1,]=="PP_AGEGRP" & geo_person_marginals[2,]==2)]<-sum(apply(temp[-1:-3,temp[1,]=="PP_AGEGRP" & temp[2,]%in%c(3:11)],2,as.numeric))
geo_person_marginals[-1:-3,(geo_person_marginals[1,]=="PP_AGEGRP" & geo_person_marginals[2,]==3)]<-sum(apply(temp[-1:-3,temp[1,]=="PP_AGEGRP" & temp[2,]%in%c(12:13)],2,as.numeric))
write_csv(geo_person_marginals,"Exported/A1/Montreal/Scenarios/S1-bCMA/geo_person_marginals.csv",col_names = FALSE)
#household_sample
write_csv(household_sample,"Exported/A1/Montreal/Scenarios/S1-bCMA/household_sample.csv",col_names = TRUE)
#person_sample
write_csv(person_sample,"Exported/A1/Montreal/Scenarios/S1-bCMA/person_sample.csv",col_names = TRUE)

###S2-bCSD
GIS<-GIS_CMA_CSD
#region_geo_mapping
geo<-1:length(unique(GIS$CSD))
region<-1:length(unique(GIS$CSD))
region_geo_mapping<-data.frame(region,geo)
write.csv(region_geo_mapping,"Exported/A1/Montreal/Scenarios/S2-bCSD/region_geo_mapping.csv",row.names=FALSE)
#geo_sample_mapping
geo<-1:length(unique(GIS$CSD))
sample_geo<-1
geo_sample_mapping<-data.frame(geo,sample_geo)
write.csv(geo_sample_mapping,"Exported/A1/Montreal/Scenarios/S2-bCSD/geo_sample_mapping.csv",row.names=FALSE)
#region_sample_mapping
region<-1:length(unique(GIS$CSD))
sample_geo<--1
region_sample_mapping<-data.frame(region,sample_geo)
write.csv(region_sample_mapping,"Exported/A1/Montreal/Scenarios/S2-bCSD/region_sample_mapping.csv",row.names=FALSE)
#region_household_marginals
region_household_marginals<-merge(CSDMAT[grepl("HH_",CSDMAT$Variable),],hh_variable_importance,by="Variable",all.x=TRUE)
region_household_marginals<-region_household_marginals[order(region_household_marginals$Importance),]
region_household_marginals<-region_household_marginals[region_household_marginals$Importance!=0,!names(region_household_marginals)%in%c("X","Definition","Description","Frequency","Importance")]
region_household_marginals$Categories[region_household_marginals$Variable=="HH_ID"]<-1
region_household_marginals<-rbind(c("variable_names","variable_categories",1:length(unique(GIS$CSD))),region_household_marginals)
region_household_marginals<-cbind(region_household_marginals[,1:2],c("region",rep("",nrow(region_household_marginals)-1)),region_household_marginals[,3:ncol(region_household_marginals)])
region_household_marginals$Variable[-1]<-paste("r",region_household_marginals$Variable[-1],sep="")
region_household_marginals<-data.frame(t(region_household_marginals))
temp<-region_household_marginals
region_household_marginals<-temp[,!((temp[1,]%in%c("rHH_TOTINCAT") & temp[2,]%in%c(5:16)))]
region_household_marginals[-1:-3,(region_household_marginals[1,]=="rHH_TOTINCAT" & region_household_marginals[2,]==1)]<-rowSums(apply(temp[-1:-3,temp[1,]=="rHH_TOTINCAT" & temp[2,]%in%c(1:6)],2,as.numeric))
region_household_marginals[-1:-3,(region_household_marginals[1,]=="rHH_TOTINCAT" & region_household_marginals[2,]==2)]<-rowSums(apply(temp[-1:-3,temp[1,]=="rHH_TOTINCAT" & temp[2,]%in%c(7:11)],2,as.numeric))
region_household_marginals[-1:-3,(region_household_marginals[1,]=="rHH_TOTINCAT" & region_household_marginals[2,]==3)]<-rowSums(apply(temp[-1:-3,temp[1,]=="rHH_TOTINCAT" & temp[2,]%in%c(12:15)],2,as.numeric))
region_household_marginals[-1:-3,(region_household_marginals[1,]=="rHH_TOTINCAT" & region_household_marginals[2,]==4)]<-as.numeric(temp[-1:-3,temp[1,]=="rHH_TOTINCAT" & temp[2,]%in%c(16)])
write_csv(region_household_marginals,"Exported/A1/Montreal/Scenarios/S2-bCSD/region_household_marginals.csv",col_names = FALSE)
#region_person_marginals
region_person_marginals<-merge(CSDMAT[grepl("PP_",CSDMAT$Variable),],pp_variable_importance,by="Variable",all.x=TRUE)
region_person_marginals<-region_person_marginals[order(region_person_marginals$Importance),]
region_person_marginals<-region_person_marginals[region_person_marginals$Importance!=0,!names(region_person_marginals)%in%c("X","Definition","Description","Frequency","Importance")]
region_person_marginals$Categories[region_person_marginals$Variable=="PP_ID"]<-1
region_person_marginals<-rbind(c("variable_names","variable_categories",1:length(unique(GIS$CSD))),region_person_marginals)
region_person_marginals<-cbind(region_person_marginals[,1:2],c("region",rep("",nrow(region_person_marginals)-1)),region_person_marginals[,3:ncol(region_person_marginals)])
region_person_marginals$Variable[-1]<-paste("r",region_person_marginals$Variable[-1],sep="")
region_person_marginals<-data.frame(t(region_person_marginals))
temp<-region_person_marginals
region_person_marginals<-temp[,!((temp[1,]%in%c("rPP_AGEGRP") & temp[2,]%in%c(4:13)))]
region_person_marginals[-1:-3,(region_person_marginals[1,]=="rPP_AGEGRP" & region_person_marginals[2,]==1)]<-rowSums(apply(temp[-1:-3,temp[1,]=="rPP_AGEGRP" & temp[2,]%in%c(1:2)],2,as.numeric))
region_person_marginals[-1:-3,(region_person_marginals[1,]=="rPP_AGEGRP" & region_person_marginals[2,]==2)]<-rowSums(apply(temp[-1:-3,temp[1,]=="rPP_AGEGRP" & temp[2,]%in%c(3:11)],2,as.numeric))
region_person_marginals[-1:-3,(region_person_marginals[1,]=="rPP_AGEGRP" & region_person_marginals[2,]==3)]<-rowSums(apply(temp[-1:-3,temp[1,]=="rPP_AGEGRP" & temp[2,]%in%c(12:13)],2,as.numeric))
write_csv(region_person_marginals,"Exported/A1/Montreal/Scenarios/S2-bCSD/region_person_marginals.csv",col_names = FALSE)
#geo_household_marginals
geo_household_marginals<-merge(CSDMAT[grepl("HH_",CSDMAT$Variable),],hh_variable_importance,by="Variable",all.x=TRUE)
geo_household_marginals<-geo_household_marginals[order(geo_household_marginals$Importance),]
geo_household_marginals<-geo_household_marginals[geo_household_marginals$Importance!=0,!names(geo_household_marginals)%in%c("X","Definition","Description","Frequency","Importance")]
geo_household_marginals$Categories[geo_household_marginals$Variable=="HH_ID"]<-1
geo_household_marginals<-rbind(c("variable_names","variable_categories",1:length(unique(GIS$CSD))),geo_household_marginals)
geo_household_marginals<-cbind(geo_household_marginals[,1:2],c("geo",rep("",nrow(geo_household_marginals)-1)),geo_household_marginals[,3:ncol(geo_household_marginals)])
geo_household_marginals$Variable[-1]<-geo_household_marginals$Variable[-1]
geo_household_marginals<-data.frame(t(geo_household_marginals))
temp<-geo_household_marginals
geo_household_marginals<-temp[,!((temp[1,]%in%c("HH_TOTINCAT") & temp[2,]%in%c(5:16)))]
geo_household_marginals[-1:-3,(geo_household_marginals[1,]=="HH_TOTINCAT" & geo_household_marginals[2,]==1)]<-rowSums(apply(temp[-1:-3,temp[1,]=="HH_TOTINCAT" & temp[2,]%in%c(1:6)],2,as.numeric))
geo_household_marginals[-1:-3,(geo_household_marginals[1,]=="HH_TOTINCAT" & geo_household_marginals[2,]==2)]<-rowSums(apply(temp[-1:-3,temp[1,]=="HH_TOTINCAT" & temp[2,]%in%c(7:11)],2,as.numeric))
geo_household_marginals[-1:-3,(geo_household_marginals[1,]=="HH_TOTINCAT" & geo_household_marginals[2,]==3)]<-rowSums(apply(temp[-1:-3,temp[1,]=="HH_TOTINCAT" & temp[2,]%in%c(12:15)],2,as.numeric))
geo_household_marginals[-1:-3,(geo_household_marginals[1,]=="HH_TOTINCAT" & geo_household_marginals[2,]==4)]<-as.numeric(temp[-1:-3,temp[1,]=="HH_TOTINCAT" & temp[2,]%in%c(16)])
write_csv(geo_household_marginals,"Exported/A1/Montreal/Scenarios/S2-bCSD/geo_household_marginals.csv",col_names = FALSE)
#geo_person_marginals
geo_person_marginals<-merge(CSDMAT[grepl("PP_",CSDMAT$Variable),],pp_variable_importance,by="Variable",all.x=TRUE)
geo_person_marginals<-geo_person_marginals[order(geo_person_marginals$Importance),]
geo_person_marginals<-geo_person_marginals[geo_person_marginals$Importance!=0,!names(geo_person_marginals)%in%c("X","Definition","Description","Frequency","Importance")]
geo_person_marginals$Categories[geo_person_marginals$Variable=="PP_ID"]<-1
geo_person_marginals<-rbind(c("variable_names","variable_categories",1:length(unique(GIS$CSD))),geo_person_marginals)
geo_person_marginals<-cbind(geo_person_marginals[,1:2],c("geo",rep("",nrow(geo_person_marginals)-1)),geo_person_marginals[,3:ncol(geo_person_marginals)])
geo_person_marginals$Variable[-1]<-geo_person_marginals$Variable[-1]
geo_person_marginals<-data.frame(t(geo_person_marginals))
temp<-geo_person_marginals
geo_person_marginals<-temp[,!((temp[1,]%in%c("PP_AGEGRP") & temp[2,]%in%c(4:13)))]
geo_person_marginals[-1:-3,(geo_person_marginals[1,]=="PP_AGEGRP" & geo_person_marginals[2,]==1)]<-rowSums(apply(temp[-1:-3,temp[1,]=="PP_AGEGRP" & temp[2,]%in%c(1:2)],2,as.numeric))
geo_person_marginals[-1:-3,(geo_person_marginals[1,]=="PP_AGEGRP" & geo_person_marginals[2,]==2)]<-rowSums(apply(temp[-1:-3,temp[1,]=="PP_AGEGRP" & temp[2,]%in%c(3:11)],2,as.numeric))
geo_person_marginals[-1:-3,(geo_person_marginals[1,]=="PP_AGEGRP" & geo_person_marginals[2,]==3)]<-rowSums(apply(temp[-1:-3,temp[1,]=="PP_AGEGRP" & temp[2,]%in%c(12:13)],2,as.numeric))
write_csv(geo_person_marginals,"Exported/A1/Montreal/Scenarios/S2-bCSD/geo_person_marginals.csv",col_names = FALSE)
#household_sample
write_csv(household_sample,"Exported/A1/Montreal/Scenarios/S2-bCSD/household_sample.csv",col_names = TRUE)
#person_sample
write_csv(person_sample,"Exported/A1/Montreal/Scenarios/S2-bCSD/person_sample.csv",col_names = TRUE)

###S3-bCMA_CSD
GIS<-GIS_CMA_CSD
#region_geo_mapping
geo<-1:length(unique(GIS$CSD))
region<-1
region_geo_mapping<-data.frame(region,geo)
write.csv(region_geo_mapping,"Exported/A1/Montreal/Scenarios/S3-bCMA_CSD/region_geo_mapping.csv",row.names=FALSE)
#geo_sample_mapping
geo<-1:length(unique(GIS$CSD))
sample_geo<-1
geo_sample_mapping<-data.frame(geo,sample_geo)
write.csv(geo_sample_mapping,"Exported/A1/Montreal/Scenarios/S3-bCMA_CSD/geo_sample_mapping.csv",row.names=FALSE)
#region_sample_mapping
region<-1
sample_geo<--1
region_sample_mapping<-data.frame(region,sample_geo)
write.csv(region_sample_mapping,"Exported/A1/Montreal/Scenarios/S3-bCMA_CSD/region_sample_mapping.csv",row.names=FALSE)
#region_household_marginals
region_household_marginals<-merge(CMAMAT[grepl("HH_",CMAMAT$Variable),],hh_variable_importance,by="Variable",all.x=TRUE)
region_household_marginals<-region_household_marginals[order(region_household_marginals$Importance),]
region_household_marginals<-region_household_marginals[region_household_marginals$Importance!=0,!names(region_household_marginals)%in%c("X","Definition","Description","Frequency","Importance")]
region_household_marginals$Categories[region_household_marginals$Variable=="HH_ID"]<-1
region_household_marginals<-rbind(c("variable_names","variable_categories",1:length(unique(GIS$CMA))),region_household_marginals)
region_household_marginals<-cbind(region_household_marginals[,1:2],c("region",rep("",nrow(region_household_marginals)-1)),region_household_marginals[,3:ncol(region_household_marginals)])
region_household_marginals$Variable[-1]<-paste("r",region_household_marginals$Variable[-1],sep="")
region_household_marginals<-data.frame(t(region_household_marginals))
temp<-region_household_marginals
region_household_marginals<-temp[,!((temp[1,]%in%c("rHH_TOTINCAT") & temp[2,]%in%c(5:16)))]
region_household_marginals[-1:-3,(region_household_marginals[1,]=="rHH_TOTINCAT" & region_household_marginals[2,]==1)]<-sum(apply(temp[-1:-3,temp[1,]=="rHH_TOTINCAT" & temp[2,]%in%c(1:6)],2,as.numeric))
region_household_marginals[-1:-3,(region_household_marginals[1,]=="rHH_TOTINCAT" & region_household_marginals[2,]==2)]<-sum(apply(temp[-1:-3,temp[1,]=="rHH_TOTINCAT" & temp[2,]%in%c(7:11)],2,as.numeric))
region_household_marginals[-1:-3,(region_household_marginals[1,]=="rHH_TOTINCAT" & region_household_marginals[2,]==3)]<-sum(apply(temp[-1:-3,temp[1,]=="rHH_TOTINCAT" & temp[2,]%in%c(12:15)],2,as.numeric))
region_household_marginals[-1:-3,(region_household_marginals[1,]=="rHH_TOTINCAT" & region_household_marginals[2,]==4)]<-as.numeric(temp[-1:-3,temp[1,]=="rHH_TOTINCAT" & temp[2,]%in%c(16)])
write_csv(region_household_marginals,"Exported/A1/Montreal/Scenarios/S3-bCMA_CSD/region_household_marginals.csv",col_names = FALSE)
#region_person_marginals
region_person_marginals<-merge(CMAMAT[grepl("PP_",CMAMAT$Variable),],pp_variable_importance,by="Variable",all.x=TRUE)
region_person_marginals<-region_person_marginals[order(region_person_marginals$Importance),]
region_person_marginals<-region_person_marginals[region_person_marginals$Importance!=0,!names(region_person_marginals)%in%c("X","Definition","Description","Frequency","Importance")]
region_person_marginals$Categories[region_person_marginals$Variable=="PP_ID"]<-1
region_person_marginals<-rbind(c("variable_names","variable_categories",1:length(unique(GIS$CMA))),region_person_marginals)
region_person_marginals<-cbind(region_person_marginals[,1:2],c("region",rep("",nrow(region_person_marginals)-1)),region_person_marginals[,3:ncol(region_person_marginals)])
region_person_marginals$Variable[-1]<-paste("r",region_person_marginals$Variable[-1],sep="")
region_person_marginals<-data.frame(t(region_person_marginals))
temp<-region_person_marginals
region_person_marginals<-temp[,!((temp[1,]%in%c("rPP_AGEGRP") & temp[2,]%in%c(4:13)))]
region_person_marginals[-1:-3,(region_person_marginals[1,]=="rPP_AGEGRP" & region_person_marginals[2,]==1)]<-sum(apply(temp[-1:-3,temp[1,]=="rPP_AGEGRP" & temp[2,]%in%c(1:2)],2,as.numeric))
region_person_marginals[-1:-3,(region_person_marginals[1,]=="rPP_AGEGRP" & region_person_marginals[2,]==2)]<-sum(apply(temp[-1:-3,temp[1,]=="rPP_AGEGRP" & temp[2,]%in%c(3:11)],2,as.numeric))
region_person_marginals[-1:-3,(region_person_marginals[1,]=="rPP_AGEGRP" & region_person_marginals[2,]==3)]<-sum(apply(temp[-1:-3,temp[1,]=="rPP_AGEGRP" & temp[2,]%in%c(12:13)],2,as.numeric))
write_csv(region_person_marginals,"Exported/A1/Montreal/Scenarios/S3-bCMA_CSD/region_person_marginals.csv",col_names = FALSE)
#geo_household_marginals
geo_household_marginals<-merge(CSDMAT[grepl("HH_",CSDMAT$Variable),],hh_variable_importance,by="Variable",all.x=TRUE)
geo_household_marginals<-geo_household_marginals[order(geo_household_marginals$Importance),]
geo_household_marginals<-geo_household_marginals[geo_household_marginals$Importance!=0,!names(geo_household_marginals)%in%c("X","Definition","Description","Frequency","Importance")]
geo_household_marginals$Categories[geo_household_marginals$Variable=="HH_ID"]<-1
geo_household_marginals<-rbind(c("variable_names","variable_categories",1:length(unique(GIS$CSD))),geo_household_marginals)
geo_household_marginals<-cbind(geo_household_marginals[,1:2],c("geo",rep("",nrow(geo_household_marginals)-1)),geo_household_marginals[,3:ncol(geo_household_marginals)])
geo_household_marginals$Variable[-1]<-geo_household_marginals$Variable[-1]
geo_household_marginals<-data.frame(t(geo_household_marginals))
temp<-geo_household_marginals
geo_household_marginals<-temp[,!((temp[1,]%in%c("HH_TOTINCAT") & temp[2,]%in%c(5:16)))]
geo_household_marginals[-1:-3,(geo_household_marginals[1,]=="HH_TOTINCAT" & geo_household_marginals[2,]==1)]<-rowSums(apply(temp[-1:-3,temp[1,]=="HH_TOTINCAT" & temp[2,]%in%c(1:6)],2,as.numeric))
geo_household_marginals[-1:-3,(geo_household_marginals[1,]=="HH_TOTINCAT" & geo_household_marginals[2,]==2)]<-rowSums(apply(temp[-1:-3,temp[1,]=="HH_TOTINCAT" & temp[2,]%in%c(7:11)],2,as.numeric))
geo_household_marginals[-1:-3,(geo_household_marginals[1,]=="HH_TOTINCAT" & geo_household_marginals[2,]==3)]<-rowSums(apply(temp[-1:-3,temp[1,]=="HH_TOTINCAT" & temp[2,]%in%c(12:15)],2,as.numeric))
geo_household_marginals[-1:-3,(geo_household_marginals[1,]=="HH_TOTINCAT" & geo_household_marginals[2,]==4)]<-as.numeric(temp[-1:-3,temp[1,]=="HH_TOTINCAT" & temp[2,]%in%c(16)])
write_csv(geo_household_marginals,"Exported/A1/Montreal/Scenarios/S3-bCMA_CSD/geo_household_marginals.csv",col_names = FALSE)
#geo_person_marginals
geo_person_marginals<-merge(CSDMAT[grepl("PP_",CSDMAT$Variable),],pp_variable_importance,by="Variable",all.x=TRUE)
geo_person_marginals<-geo_person_marginals[order(geo_person_marginals$Importance),]
geo_person_marginals<-geo_person_marginals[geo_person_marginals$Importance!=0,!names(geo_person_marginals)%in%c("X","Definition","Description","Frequency","Importance")]
geo_person_marginals$Categories[geo_person_marginals$Variable=="PP_ID"]<-1
geo_person_marginals<-rbind(c("variable_names","variable_categories",1:length(unique(GIS$CSD))),geo_person_marginals)
geo_person_marginals<-cbind(geo_person_marginals[,1:2],c("geo",rep("",nrow(geo_person_marginals)-1)),geo_person_marginals[,3:ncol(geo_person_marginals)])
geo_person_marginals$Variable[-1]<-geo_person_marginals$Variable[-1]
geo_person_marginals<-data.frame(t(geo_person_marginals))
temp<-geo_person_marginals
geo_person_marginals<-temp[,!((temp[1,]%in%c("PP_AGEGRP") & temp[2,]%in%c(4:13)))]
geo_person_marginals[-1:-3,(geo_person_marginals[1,]=="PP_AGEGRP" & geo_person_marginals[2,]==1)]<-rowSums(apply(temp[-1:-3,temp[1,]=="PP_AGEGRP" & temp[2,]%in%c(1:2)],2,as.numeric))
geo_person_marginals[-1:-3,(geo_person_marginals[1,]=="PP_AGEGRP" & geo_person_marginals[2,]==2)]<-rowSums(apply(temp[-1:-3,temp[1,]=="PP_AGEGRP" & temp[2,]%in%c(3:11)],2,as.numeric))
geo_person_marginals[-1:-3,(geo_person_marginals[1,]=="PP_AGEGRP" & geo_person_marginals[2,]==3)]<-rowSums(apply(temp[-1:-3,temp[1,]=="PP_AGEGRP" & temp[2,]%in%c(12:13)],2,as.numeric))
write_csv(geo_person_marginals,"Exported/A1/Montreal/Scenarios/S3-bCMA_CSD/geo_person_marginals.csv",col_names = FALSE)
#household_sample
write_csv(household_sample,"Exported/A1/Montreal/Scenarios/S3-bCMA_CSD/household_sample.csv",col_names = TRUE)
#person_sample
write_csv(person_sample,"Exported/A1/Montreal/Scenarios/S3-bCMA_CSD/person_sample.csv",col_names = TRUE)

###S4-bADA
GIS<-GIS_CMA_ADA
#region_geo_mapping
geo<-1:length(unique(GIS$ADA))
region<-1:length(unique(GIS$ADA))
region_geo_mapping<-data.frame(region,geo)
write.csv(region_geo_mapping,"Exported/A1/Montreal/Scenarios/S4-bADA/region_geo_mapping.csv",row.names=FALSE)
#geo_sample_mapping
geo<-1:length(unique(GIS$ADA))
sample_geo<-1
geo_sample_mapping<-data.frame(geo,sample_geo)
write.csv(geo_sample_mapping,"Exported/A1/Montreal/Scenarios/S4-bADA/geo_sample_mapping.csv",row.names=FALSE)
#region_sample_mapping
region<-1:length(unique(GIS$ADA))
sample_geo<--1
region_sample_mapping<-data.frame(region,sample_geo)
write.csv(region_sample_mapping,"Exported/A1/Montreal/Scenarios/S4-bADA/region_sample_mapping.csv",row.names=FALSE)
#region_household_marginals
region_household_marginals<-merge(ADAMAT[grepl("HH_",ADAMAT$Variable),],hh_variable_importance,by="Variable",all.x=TRUE)
region_household_marginals<-region_household_marginals[order(region_household_marginals$Importance),]
region_household_marginals<-region_household_marginals[region_household_marginals$Importance!=0,!names(region_household_marginals)%in%c("X","Definition","Description","Frequency","Importance")]
region_household_marginals$Categories[region_household_marginals$Variable=="HH_ID"]<-1
region_household_marginals<-rbind(c("variable_names","variable_categories",1:length(unique(GIS$ADA))),region_household_marginals)
region_household_marginals<-cbind(region_household_marginals[,1:2],c("region",rep("",nrow(region_household_marginals)-1)),region_household_marginals[,3:ncol(region_household_marginals)])
region_household_marginals$Variable[-1]<-paste("r",region_household_marginals$Variable[-1],sep="")
region_household_marginals<-data.frame(t(region_household_marginals))
temp<-region_household_marginals
region_household_marginals<-temp[,!((temp[1,]%in%c("rHH_TOTINCAT") & temp[2,]%in%c(5:16)))]
region_household_marginals[-1:-3,(region_household_marginals[1,]=="rHH_TOTINCAT" & region_household_marginals[2,]==1)]<-rowSums(apply(temp[-1:-3,temp[1,]=="rHH_TOTINCAT" & temp[2,]%in%c(1:6)],2,as.numeric))
region_household_marginals[-1:-3,(region_household_marginals[1,]=="rHH_TOTINCAT" & region_household_marginals[2,]==2)]<-rowSums(apply(temp[-1:-3,temp[1,]=="rHH_TOTINCAT" & temp[2,]%in%c(7:11)],2,as.numeric))
region_household_marginals[-1:-3,(region_household_marginals[1,]=="rHH_TOTINCAT" & region_household_marginals[2,]==3)]<-rowSums(apply(temp[-1:-3,temp[1,]=="rHH_TOTINCAT" & temp[2,]%in%c(12:15)],2,as.numeric))
region_household_marginals[-1:-3,(region_household_marginals[1,]=="rHH_TOTINCAT" & region_household_marginals[2,]==4)]<-as.numeric(temp[-1:-3,temp[1,]=="rHH_TOTINCAT" & temp[2,]%in%c(16)])
write_csv(region_household_marginals,"Exported/A1/Montreal/Scenarios/S4-bADA/region_household_marginals.csv",col_names = FALSE)
#region_person_marginals
region_person_marginals<-merge(ADAMAT[grepl("PP_",ADAMAT$Variable),],pp_variable_importance,by="Variable",all.x=TRUE)
region_person_marginals<-region_person_marginals[order(region_person_marginals$Importance),]
region_person_marginals<-region_person_marginals[region_person_marginals$Importance!=0,!names(region_person_marginals)%in%c("X","Definition","Description","Frequency","Importance")]
region_person_marginals$Categories[region_person_marginals$Variable=="PP_ID"]<-1
region_person_marginals<-rbind(c("variable_names","variable_categories",1:length(unique(GIS$ADA))),region_person_marginals)
region_person_marginals<-cbind(region_person_marginals[,1:2],c("region",rep("",nrow(region_person_marginals)-1)),region_person_marginals[,3:ncol(region_person_marginals)])
region_person_marginals$Variable[-1]<-paste("r",region_person_marginals$Variable[-1],sep="")
region_person_marginals<-data.frame(t(region_person_marginals))
temp<-region_person_marginals
region_person_marginals<-temp[,!((temp[1,]%in%c("rPP_AGEGRP") & temp[2,]%in%c(4:13)))]
region_person_marginals[-1:-3,(region_person_marginals[1,]=="rPP_AGEGRP" & region_person_marginals[2,]==1)]<-rowSums(apply(temp[-1:-3,temp[1,]=="rPP_AGEGRP" & temp[2,]%in%c(1:2)],2,as.numeric))
region_person_marginals[-1:-3,(region_person_marginals[1,]=="rPP_AGEGRP" & region_person_marginals[2,]==2)]<-rowSums(apply(temp[-1:-3,temp[1,]=="rPP_AGEGRP" & temp[2,]%in%c(3:11)],2,as.numeric))
region_person_marginals[-1:-3,(region_person_marginals[1,]=="rPP_AGEGRP" & region_person_marginals[2,]==3)]<-rowSums(apply(temp[-1:-3,temp[1,]=="rPP_AGEGRP" & temp[2,]%in%c(12:13)],2,as.numeric))
write_csv(region_person_marginals,"Exported/A1/Montreal/Scenarios/S4-bADA/region_person_marginals.csv",col_names = FALSE)
#geo_household_marginals
geo_household_marginals<-merge(ADAMAT[grepl("HH_",ADAMAT$Variable),],hh_variable_importance,by="Variable",all.x=TRUE)
geo_household_marginals<-geo_household_marginals[order(geo_household_marginals$Importance),]
geo_household_marginals<-geo_household_marginals[geo_household_marginals$Importance!=0,!names(geo_household_marginals)%in%c("X","Definition","Description","Frequency","Importance")]
geo_household_marginals$Categories[geo_household_marginals$Variable=="HH_ID"]<-1
geo_household_marginals<-rbind(c("variable_names","variable_categories",1:length(unique(GIS$ADA))),geo_household_marginals)
geo_household_marginals<-cbind(geo_household_marginals[,1:2],c("geo",rep("",nrow(geo_household_marginals)-1)),geo_household_marginals[,3:ncol(geo_household_marginals)])
geo_household_marginals$Variable[-1]<-geo_household_marginals$Variable[-1]
geo_household_marginals<-data.frame(t(geo_household_marginals))
temp<-geo_household_marginals
geo_household_marginals<-temp[,!((temp[1,]%in%c("HH_TOTINCAT") & temp[2,]%in%c(5:16)))]
geo_household_marginals[-1:-3,(geo_household_marginals[1,]=="HH_TOTINCAT" & geo_household_marginals[2,]==1)]<-rowSums(apply(temp[-1:-3,temp[1,]=="HH_TOTINCAT" & temp[2,]%in%c(1:6)],2,as.numeric))
geo_household_marginals[-1:-3,(geo_household_marginals[1,]=="HH_TOTINCAT" & geo_household_marginals[2,]==2)]<-rowSums(apply(temp[-1:-3,temp[1,]=="HH_TOTINCAT" & temp[2,]%in%c(7:11)],2,as.numeric))
geo_household_marginals[-1:-3,(geo_household_marginals[1,]=="HH_TOTINCAT" & geo_household_marginals[2,]==3)]<-rowSums(apply(temp[-1:-3,temp[1,]=="HH_TOTINCAT" & temp[2,]%in%c(12:15)],2,as.numeric))
geo_household_marginals[-1:-3,(geo_household_marginals[1,]=="HH_TOTINCAT" & geo_household_marginals[2,]==4)]<-as.numeric(temp[-1:-3,temp[1,]=="HH_TOTINCAT" & temp[2,]%in%c(16)])
write_csv(geo_household_marginals,"Exported/A1/Montreal/Scenarios/S4-bADA/geo_household_marginals.csv",col_names = FALSE)
#geo_person_marginals
geo_person_marginals<-merge(ADAMAT[grepl("PP_",ADAMAT$Variable),],pp_variable_importance,by="Variable",all.x=TRUE)
geo_person_marginals<-geo_person_marginals[order(geo_person_marginals$Importance),]
geo_person_marginals<-geo_person_marginals[geo_person_marginals$Importance!=0,!names(geo_person_marginals)%in%c("X","Definition","Description","Frequency","Importance")]
geo_person_marginals$Categories[geo_person_marginals$Variable=="PP_ID"]<-1
geo_person_marginals<-rbind(c("variable_names","variable_categories",1:length(unique(GIS$ADA))),geo_person_marginals)
geo_person_marginals<-cbind(geo_person_marginals[,1:2],c("geo",rep("",nrow(geo_person_marginals)-1)),geo_person_marginals[,3:ncol(geo_person_marginals)])
geo_person_marginals$Variable[-1]<-geo_person_marginals$Variable[-1]
geo_person_marginals<-data.frame(t(geo_person_marginals))
temp<-geo_person_marginals
geo_person_marginals<-temp[,!((temp[1,]%in%c("PP_AGEGRP") & temp[2,]%in%c(4:13)))]
geo_person_marginals[-1:-3,(geo_person_marginals[1,]=="PP_AGEGRP" & geo_person_marginals[2,]==1)]<-rowSums(apply(temp[-1:-3,temp[1,]=="PP_AGEGRP" & temp[2,]%in%c(1:2)],2,as.numeric))
geo_person_marginals[-1:-3,(geo_person_marginals[1,]=="PP_AGEGRP" & geo_person_marginals[2,]==2)]<-rowSums(apply(temp[-1:-3,temp[1,]=="PP_AGEGRP" & temp[2,]%in%c(3:11)],2,as.numeric))
geo_person_marginals[-1:-3,(geo_person_marginals[1,]=="PP_AGEGRP" & geo_person_marginals[2,]==3)]<-rowSums(apply(temp[-1:-3,temp[1,]=="PP_AGEGRP" & temp[2,]%in%c(12:13)],2,as.numeric))
write_csv(geo_person_marginals,"Exported/A1/Montreal/Scenarios/S4-bADA/geo_person_marginals.csv",col_names = FALSE)
#household_sample
write_csv(household_sample,"Exported/A1/Montreal/Scenarios/S4-bADA/household_sample.csv",col_names = TRUE)
#person_sample
write_csv(person_sample,"Exported/A1/Montreal/Scenarios/S4-bADA/person_sample.csv",col_names = TRUE)

###S5-bCMA_ADA
GIS<-GIS_CMA_ADA
#region_geo_mapping
geo<-1:length(unique(GIS$ADA))
region<-1
region_geo_mapping<-data.frame(region,geo)
write.csv(region_geo_mapping,"Exported/A1/Montreal/Scenarios/S5-bCMA_ADA/region_geo_mapping.csv",row.names=FALSE)
#geo_sample_mapping
geo<-1:length(unique(GIS$ADA))
sample_geo<-1
geo_sample_mapping<-data.frame(geo,sample_geo)
write.csv(geo_sample_mapping,"Exported/A1/Montreal/Scenarios/S5-bCMA_ADA/geo_sample_mapping.csv",row.names=FALSE)
#region_sample_mapping
region<-1
sample_geo<--1
region_sample_mapping<-data.frame(region,sample_geo)
write.csv(region_sample_mapping,"Exported/A1/Montreal/Scenarios/S5-bCMA_ADA/region_sample_mapping.csv",row.names=FALSE)
#region_household_marginals
region_household_marginals<-merge(CMAMAT[grepl("HH_",CMAMAT$Variable),],hh_variable_importance,by="Variable",all.x=TRUE)
region_household_marginals<-region_household_marginals[order(region_household_marginals$Importance),]
region_household_marginals<-region_household_marginals[region_household_marginals$Importance!=0,!names(region_household_marginals)%in%c("X","Definition","Description","Frequency","Importance")]
region_household_marginals$Categories[region_household_marginals$Variable=="HH_ID"]<-1
region_household_marginals<-rbind(c("variable_names","variable_categories",1:length(unique(GIS$CMA))),region_household_marginals)
region_household_marginals<-cbind(region_household_marginals[,1:2],c("region",rep("",nrow(region_household_marginals)-1)),region_household_marginals[,3:ncol(region_household_marginals)])
region_household_marginals$Variable[-1]<-paste("r",region_household_marginals$Variable[-1],sep="")
region_household_marginals<-data.frame(t(region_household_marginals))
temp<-region_household_marginals
region_household_marginals<-temp[,!((temp[1,]%in%c("rHH_TOTINCAT") & temp[2,]%in%c(5:16)))]
region_household_marginals[-1:-3,(region_household_marginals[1,]=="rHH_TOTINCAT" & region_household_marginals[2,]==1)]<-sum(apply(temp[-1:-3,temp[1,]=="rHH_TOTINCAT" & temp[2,]%in%c(1:6)],2,as.numeric))
region_household_marginals[-1:-3,(region_household_marginals[1,]=="rHH_TOTINCAT" & region_household_marginals[2,]==2)]<-sum(apply(temp[-1:-3,temp[1,]=="rHH_TOTINCAT" & temp[2,]%in%c(7:11)],2,as.numeric))
region_household_marginals[-1:-3,(region_household_marginals[1,]=="rHH_TOTINCAT" & region_household_marginals[2,]==3)]<-sum(apply(temp[-1:-3,temp[1,]=="rHH_TOTINCAT" & temp[2,]%in%c(12:15)],2,as.numeric))
region_household_marginals[-1:-3,(region_household_marginals[1,]=="rHH_TOTINCAT" & region_household_marginals[2,]==4)]<-as.numeric(temp[-1:-3,temp[1,]=="rHH_TOTINCAT" & temp[2,]%in%c(16)])
write_csv(region_household_marginals,"Exported/A1/Montreal/Scenarios/S5-bCMA_ADA/region_household_marginals.csv",col_names = FALSE)
#region_person_marginals
region_person_marginals<-merge(CMAMAT[grepl("PP_",CMAMAT$Variable),],pp_variable_importance,by="Variable",all.x=TRUE)
region_person_marginals<-region_person_marginals[order(region_person_marginals$Importance),]
region_person_marginals<-region_person_marginals[region_person_marginals$Importance!=0,!names(region_person_marginals)%in%c("X","Definition","Description","Frequency","Importance")]
region_person_marginals$Categories[region_person_marginals$Variable=="PP_ID"]<-1
region_person_marginals<-rbind(c("variable_names","variable_categories",1:length(unique(GIS$CMA))),region_person_marginals)
region_person_marginals<-cbind(region_person_marginals[,1:2],c("region",rep("",nrow(region_person_marginals)-1)),region_person_marginals[,3:ncol(region_person_marginals)])
region_person_marginals$Variable[-1]<-paste("r",region_person_marginals$Variable[-1],sep="")
region_person_marginals<-data.frame(t(region_person_marginals))
temp<-region_person_marginals
region_person_marginals<-temp[,!((temp[1,]%in%c("rPP_AGEGRP") & temp[2,]%in%c(4:13)))]
region_person_marginals[-1:-3,(region_person_marginals[1,]=="rPP_AGEGRP" & region_person_marginals[2,]==1)]<-sum(apply(temp[-1:-3,temp[1,]=="rPP_AGEGRP" & temp[2,]%in%c(1:2)],2,as.numeric))
region_person_marginals[-1:-3,(region_person_marginals[1,]=="rPP_AGEGRP" & region_person_marginals[2,]==2)]<-sum(apply(temp[-1:-3,temp[1,]=="rPP_AGEGRP" & temp[2,]%in%c(3:11)],2,as.numeric))
region_person_marginals[-1:-3,(region_person_marginals[1,]=="rPP_AGEGRP" & region_person_marginals[2,]==3)]<-sum(apply(temp[-1:-3,temp[1,]=="rPP_AGEGRP" & temp[2,]%in%c(12:13)],2,as.numeric))
write_csv(region_person_marginals,"Exported/A1/Montreal/Scenarios/S5-bCMA_ADA/region_person_marginals.csv",col_names = FALSE)
#geo_household_marginals
geo_household_marginals<-merge(ADAMAT[grepl("HH_",ADAMAT$Variable),],hh_variable_importance,by="Variable",all.x=TRUE)
geo_household_marginals<-geo_household_marginals[order(geo_household_marginals$Importance),]
geo_household_marginals<-geo_household_marginals[geo_household_marginals$Importance!=0,!names(geo_household_marginals)%in%c("X","Definition","Description","Frequency","Importance")]
geo_household_marginals$Categories[geo_household_marginals$Variable=="HH_ID"]<-1
geo_household_marginals<-rbind(c("variable_names","variable_categories",1:length(unique(GIS$ADA))),geo_household_marginals)
geo_household_marginals<-cbind(geo_household_marginals[,1:2],c("geo",rep("",nrow(geo_household_marginals)-1)),geo_household_marginals[,3:ncol(geo_household_marginals)])
geo_household_marginals$Variable[-1]<-geo_household_marginals$Variable[-1]
geo_household_marginals<-data.frame(t(geo_household_marginals))
temp<-geo_household_marginals
geo_household_marginals<-temp[,!((temp[1,]%in%c("HH_TOTINCAT") & temp[2,]%in%c(5:16)))]
geo_household_marginals[-1:-3,(geo_household_marginals[1,]=="HH_TOTINCAT" & geo_household_marginals[2,]==1)]<-rowSums(apply(temp[-1:-3,temp[1,]=="HH_TOTINCAT" & temp[2,]%in%c(1:6)],2,as.numeric))
geo_household_marginals[-1:-3,(geo_household_marginals[1,]=="HH_TOTINCAT" & geo_household_marginals[2,]==2)]<-rowSums(apply(temp[-1:-3,temp[1,]=="HH_TOTINCAT" & temp[2,]%in%c(7:11)],2,as.numeric))
geo_household_marginals[-1:-3,(geo_household_marginals[1,]=="HH_TOTINCAT" & geo_household_marginals[2,]==3)]<-rowSums(apply(temp[-1:-3,temp[1,]=="HH_TOTINCAT" & temp[2,]%in%c(12:15)],2,as.numeric))
geo_household_marginals[-1:-3,(geo_household_marginals[1,]=="HH_TOTINCAT" & geo_household_marginals[2,]==4)]<-as.numeric(temp[-1:-3,temp[1,]=="HH_TOTINCAT" & temp[2,]%in%c(16)])
write_csv(geo_household_marginals,"Exported/A1/Montreal/Scenarios/S5-bCMA_ADA/geo_household_marginals.csv",col_names = FALSE)
#geo_person_marginals
geo_person_marginals<-merge(ADAMAT[grepl("PP_",ADAMAT$Variable),],pp_variable_importance,by="Variable",all.x=TRUE)
geo_person_marginals<-geo_person_marginals[order(geo_person_marginals$Importance),]
geo_person_marginals<-geo_person_marginals[geo_person_marginals$Importance!=0,!names(geo_person_marginals)%in%c("X","Definition","Description","Frequency","Importance")]
geo_person_marginals$Categories[geo_person_marginals$Variable=="PP_ID"]<-1
geo_person_marginals<-rbind(c("variable_names","variable_categories",1:length(unique(GIS$ADA))),geo_person_marginals)
geo_person_marginals<-cbind(geo_person_marginals[,1:2],c("geo",rep("",nrow(geo_person_marginals)-1)),geo_person_marginals[,3:ncol(geo_person_marginals)])
geo_person_marginals$Variable[-1]<-geo_person_marginals$Variable[-1]
geo_person_marginals<-data.frame(t(geo_person_marginals))
temp<-geo_person_marginals
geo_person_marginals<-temp[,!((temp[1,]%in%c("PP_AGEGRP") & temp[2,]%in%c(4:13)))]
geo_person_marginals[-1:-3,(geo_person_marginals[1,]=="PP_AGEGRP" & geo_person_marginals[2,]==1)]<-rowSums(apply(temp[-1:-3,temp[1,]=="PP_AGEGRP" & temp[2,]%in%c(1:2)],2,as.numeric))
geo_person_marginals[-1:-3,(geo_person_marginals[1,]=="PP_AGEGRP" & geo_person_marginals[2,]==2)]<-rowSums(apply(temp[-1:-3,temp[1,]=="PP_AGEGRP" & temp[2,]%in%c(3:11)],2,as.numeric))
geo_person_marginals[-1:-3,(geo_person_marginals[1,]=="PP_AGEGRP" & geo_person_marginals[2,]==3)]<-rowSums(apply(temp[-1:-3,temp[1,]=="PP_AGEGRP" & temp[2,]%in%c(12:13)],2,as.numeric))
write_csv(geo_person_marginals,"Exported/A1/Montreal/Scenarios/S5-bCMA_ADA/geo_person_marginals.csv",col_names = FALSE)
#household_sample
write_csv(household_sample,"Exported/A1/Montreal/Scenarios/S5-bCMA_ADA/household_sample.csv",col_names = TRUE)
#person_sample
write_csv(person_sample,"Exported/A1/Montreal/Scenarios/S5-bCMA_ADA/person_sample.csv",col_names = TRUE)

###S6-bCT
GIS<-GIS_CMA_CT
#region_geo_mapping
geo<-1:length(unique(GIS$CT))
region<-1:length(unique(GIS$CT))
region_geo_mapping<-data.frame(region,geo)
write.csv(region_geo_mapping,"Exported/A1/Montreal/Scenarios/S6-bCT/region_geo_mapping.csv",row.names=FALSE)
#geo_sample_mapping
geo<-1:length(unique(GIS$CT))
sample_geo<-1
geo_sample_mapping<-data.frame(geo,sample_geo)
write.csv(geo_sample_mapping,"Exported/A1/Montreal/Scenarios/S6-bCT/geo_sample_mapping.csv",row.names=FALSE)
#region_sample_mapping
region<-1:length(unique(GIS$CT))
sample_geo<--1
region_sample_mapping<-data.frame(region,sample_geo)
write.csv(region_sample_mapping,"Exported/A1/Montreal/Scenarios/S6-bCT/region_sample_mapping.csv",row.names=FALSE)
#region_household_marginals
region_household_marginals<-merge(CTMAT[grepl("HH_",CTMAT$Variable),],hh_variable_importance,by="Variable",all.x=TRUE)
region_household_marginals<-region_household_marginals[order(region_household_marginals$Importance),]
region_household_marginals<-region_household_marginals[region_household_marginals$Importance!=0,!names(region_household_marginals)%in%c("X","Definition","Description","Frequency","Importance")]
region_household_marginals$Categories[region_household_marginals$Variable=="HH_ID"]<-1
region_household_marginals<-rbind(c("variable_names","variable_categories",1:length(unique(GIS$CT))),region_household_marginals)
region_household_marginals<-cbind(region_household_marginals[,1:2],c("region",rep("",nrow(region_household_marginals)-1)),region_household_marginals[,3:ncol(region_household_marginals)])
region_household_marginals$Variable[-1]<-paste("r",region_household_marginals$Variable[-1],sep="")
region_household_marginals<-data.frame(t(region_household_marginals))
temp<-region_household_marginals
region_household_marginals<-temp[,!((temp[1,]%in%c("rHH_TOTINCAT") & temp[2,]%in%c(5:16)))]
region_household_marginals[-1:-3,(region_household_marginals[1,]=="rHH_TOTINCAT" & region_household_marginals[2,]==1)]<-rowSums(apply(temp[-1:-3,temp[1,]=="rHH_TOTINCAT" & temp[2,]%in%c(1:6)],2,as.numeric))
region_household_marginals[-1:-3,(region_household_marginals[1,]=="rHH_TOTINCAT" & region_household_marginals[2,]==2)]<-rowSums(apply(temp[-1:-3,temp[1,]=="rHH_TOTINCAT" & temp[2,]%in%c(7:11)],2,as.numeric))
region_household_marginals[-1:-3,(region_household_marginals[1,]=="rHH_TOTINCAT" & region_household_marginals[2,]==3)]<-rowSums(apply(temp[-1:-3,temp[1,]=="rHH_TOTINCAT" & temp[2,]%in%c(12:15)],2,as.numeric))
region_household_marginals[-1:-3,(region_household_marginals[1,]=="rHH_TOTINCAT" & region_household_marginals[2,]==4)]<-as.numeric(temp[-1:-3,temp[1,]=="rHH_TOTINCAT" & temp[2,]%in%c(16)])
write_csv(region_household_marginals,"Exported/A1/Montreal/Scenarios/S6-bCT/region_household_marginals.csv",col_names = FALSE)
#region_person_marginals
region_person_marginals<-merge(CTMAT[grepl("PP_",CTMAT$Variable),],pp_variable_importance,by="Variable",all.x=TRUE)
region_person_marginals<-region_person_marginals[order(region_person_marginals$Importance),]
region_person_marginals<-region_person_marginals[region_person_marginals$Importance!=0,!names(region_person_marginals)%in%c("X","Definition","Description","Frequency","Importance")]
region_person_marginals$Categories[region_person_marginals$Variable=="PP_ID"]<-1
region_person_marginals<-rbind(c("variable_names","variable_categories",1:length(unique(GIS$CT))),region_person_marginals)
region_person_marginals<-cbind(region_person_marginals[,1:2],c("region",rep("",nrow(region_person_marginals)-1)),region_person_marginals[,3:ncol(region_person_marginals)])
region_person_marginals$Variable[-1]<-paste("r",region_person_marginals$Variable[-1],sep="")
region_person_marginals<-data.frame(t(region_person_marginals))
temp<-region_person_marginals
region_person_marginals<-temp[,!((temp[1,]%in%c("rPP_AGEGRP") & temp[2,]%in%c(4:13)))]
region_person_marginals[-1:-3,(region_person_marginals[1,]=="rPP_AGEGRP" & region_person_marginals[2,]==1)]<-rowSums(apply(temp[-1:-3,temp[1,]=="rPP_AGEGRP" & temp[2,]%in%c(1:2)],2,as.numeric))
region_person_marginals[-1:-3,(region_person_marginals[1,]=="rPP_AGEGRP" & region_person_marginals[2,]==2)]<-rowSums(apply(temp[-1:-3,temp[1,]=="rPP_AGEGRP" & temp[2,]%in%c(3:11)],2,as.numeric))
region_person_marginals[-1:-3,(region_person_marginals[1,]=="rPP_AGEGRP" & region_person_marginals[2,]==3)]<-rowSums(apply(temp[-1:-3,temp[1,]=="rPP_AGEGRP" & temp[2,]%in%c(12:13)],2,as.numeric))
write_csv(region_person_marginals,"Exported/A1/Montreal/Scenarios/S6-bCT/region_person_marginals.csv",col_names = FALSE)
#geo_household_marginals
geo_household_marginals<-merge(CTMAT[grepl("HH_",CTMAT$Variable),],hh_variable_importance,by="Variable",all.x=TRUE)
geo_household_marginals<-geo_household_marginals[order(geo_household_marginals$Importance),]
geo_household_marginals<-geo_household_marginals[geo_household_marginals$Importance!=0,!names(geo_household_marginals)%in%c("X","Definition","Description","Frequency","Importance")]
geo_household_marginals$Categories[geo_household_marginals$Variable=="HH_ID"]<-1
geo_household_marginals<-rbind(c("variable_names","variable_categories",1:length(unique(GIS$CT))),geo_household_marginals)
geo_household_marginals<-cbind(geo_household_marginals[,1:2],c("geo",rep("",nrow(geo_household_marginals)-1)),geo_household_marginals[,3:ncol(geo_household_marginals)])
geo_household_marginals$Variable[-1]<-geo_household_marginals$Variable[-1]
geo_household_marginals<-data.frame(t(geo_household_marginals))
temp<-geo_household_marginals
geo_household_marginals<-temp[,!((temp[1,]%in%c("HH_TOTINCAT") & temp[2,]%in%c(5:16)))]
geo_household_marginals[-1:-3,(geo_household_marginals[1,]=="HH_TOTINCAT" & geo_household_marginals[2,]==1)]<-rowSums(apply(temp[-1:-3,temp[1,]=="HH_TOTINCAT" & temp[2,]%in%c(1:6)],2,as.numeric))
geo_household_marginals[-1:-3,(geo_household_marginals[1,]=="HH_TOTINCAT" & geo_household_marginals[2,]==2)]<-rowSums(apply(temp[-1:-3,temp[1,]=="HH_TOTINCAT" & temp[2,]%in%c(7:11)],2,as.numeric))
geo_household_marginals[-1:-3,(geo_household_marginals[1,]=="HH_TOTINCAT" & geo_household_marginals[2,]==3)]<-rowSums(apply(temp[-1:-3,temp[1,]=="HH_TOTINCAT" & temp[2,]%in%c(12:15)],2,as.numeric))
geo_household_marginals[-1:-3,(geo_household_marginals[1,]=="HH_TOTINCAT" & geo_household_marginals[2,]==4)]<-as.numeric(temp[-1:-3,temp[1,]=="HH_TOTINCAT" & temp[2,]%in%c(16)])
write_csv(geo_household_marginals,"Exported/A1/Montreal/Scenarios/S6-bCT/geo_household_marginals.csv",col_names = FALSE)
#geo_person_marginals
geo_person_marginals<-merge(CTMAT[grepl("PP_",CTMAT$Variable),],pp_variable_importance,by="Variable",all.x=TRUE)
geo_person_marginals<-geo_person_marginals[order(geo_person_marginals$Importance),]
geo_person_marginals<-geo_person_marginals[geo_person_marginals$Importance!=0,!names(geo_person_marginals)%in%c("X","Definition","Description","Frequency","Importance")]
geo_person_marginals$Categories[geo_person_marginals$Variable=="PP_ID"]<-1
geo_person_marginals<-rbind(c("variable_names","variable_categories",1:length(unique(GIS$CT))),geo_person_marginals)
geo_person_marginals<-cbind(geo_person_marginals[,1:2],c("geo",rep("",nrow(geo_person_marginals)-1)),geo_person_marginals[,3:ncol(geo_person_marginals)])
geo_person_marginals$Variable[-1]<-geo_person_marginals$Variable[-1]
geo_person_marginals<-data.frame(t(geo_person_marginals))
temp<-geo_person_marginals
geo_person_marginals<-temp[,!((temp[1,]%in%c("PP_AGEGRP") & temp[2,]%in%c(4:13)))]
geo_person_marginals[-1:-3,(geo_person_marginals[1,]=="PP_AGEGRP" & geo_person_marginals[2,]==1)]<-rowSums(apply(temp[-1:-3,temp[1,]=="PP_AGEGRP" & temp[2,]%in%c(1:2)],2,as.numeric))
geo_person_marginals[-1:-3,(geo_person_marginals[1,]=="PP_AGEGRP" & geo_person_marginals[2,]==2)]<-rowSums(apply(temp[-1:-3,temp[1,]=="PP_AGEGRP" & temp[2,]%in%c(3:11)],2,as.numeric))
geo_person_marginals[-1:-3,(geo_person_marginals[1,]=="PP_AGEGRP" & geo_person_marginals[2,]==3)]<-rowSums(apply(temp[-1:-3,temp[1,]=="PP_AGEGRP" & temp[2,]%in%c(12:13)],2,as.numeric))
write_csv(geo_person_marginals,"Exported/A1/Montreal/Scenarios/S6-bCT/geo_person_marginals.csv",col_names = FALSE)
#household_sample
write_csv(household_sample,"Exported/A1/Montreal/Scenarios/S6-bCT/household_sample.csv",col_names = TRUE)
#person_sample
write_csv(person_sample,"Exported/A1/Montreal/Scenarios/S6-bCT/person_sample.csv",col_names = TRUE)

###S7-bCMA_CT
GIS<-GIS_CMA_CT
#region_geo_mapping
geo<-1:length(unique(GIS$CT))
region<-1
region_geo_mapping<-data.frame(region,geo)
write.csv(region_geo_mapping,"Exported/A1/Montreal/Scenarios/S7-bCMA_CT/region_geo_mapping.csv",row.names=FALSE)
#geo_sample_mapping
geo<-1:length(unique(GIS$CT))
sample_geo<-1
geo_sample_mapping<-data.frame(geo,sample_geo)
write.csv(geo_sample_mapping,"Exported/A1/Montreal/Scenarios/S7-bCMA_CT/geo_sample_mapping.csv",row.names=FALSE)
#region_sample_mapping
region<-1
sample_geo<--1
region_sample_mapping<-data.frame(region,sample_geo)
write.csv(region_sample_mapping,"Exported/A1/Montreal/Scenarios/S7-bCMA_CT/region_sample_mapping.csv",row.names=FALSE)
#region_household_marginals
region_household_marginals<-merge(CMAMAT[grepl("HH_",CMAMAT$Variable),],hh_variable_importance,by="Variable",all.x=TRUE)
region_household_marginals<-region_household_marginals[order(region_household_marginals$Importance),]
region_household_marginals<-region_household_marginals[region_household_marginals$Importance!=0,!names(region_household_marginals)%in%c("X","Definition","Description","Frequency","Importance")]
region_household_marginals$Categories[region_household_marginals$Variable=="HH_ID"]<-1
region_household_marginals<-rbind(c("variable_names","variable_categories",1:length(unique(GIS$CMA))),region_household_marginals)
region_household_marginals<-cbind(region_household_marginals[,1:2],c("region",rep("",nrow(region_household_marginals)-1)),region_household_marginals[,3:ncol(region_household_marginals)])
region_household_marginals$Variable[-1]<-paste("r",region_household_marginals$Variable[-1],sep="")
region_household_marginals<-data.frame(t(region_household_marginals))
temp<-region_household_marginals
region_household_marginals<-temp[,!((temp[1,]%in%c("rHH_TOTINCAT") & temp[2,]%in%c(5:16)))]
region_household_marginals[-1:-3,(region_household_marginals[1,]=="rHH_TOTINCAT" & region_household_marginals[2,]==1)]<-sum(apply(temp[-1:-3,temp[1,]=="rHH_TOTINCAT" & temp[2,]%in%c(1:6)],2,as.numeric))
region_household_marginals[-1:-3,(region_household_marginals[1,]=="rHH_TOTINCAT" & region_household_marginals[2,]==2)]<-sum(apply(temp[-1:-3,temp[1,]=="rHH_TOTINCAT" & temp[2,]%in%c(7:11)],2,as.numeric))
region_household_marginals[-1:-3,(region_household_marginals[1,]=="rHH_TOTINCAT" & region_household_marginals[2,]==3)]<-sum(apply(temp[-1:-3,temp[1,]=="rHH_TOTINCAT" & temp[2,]%in%c(12:15)],2,as.numeric))
region_household_marginals[-1:-3,(region_household_marginals[1,]=="rHH_TOTINCAT" & region_household_marginals[2,]==4)]<-as.numeric(temp[-1:-3,temp[1,]=="rHH_TOTINCAT" & temp[2,]%in%c(16)])
write_csv(region_household_marginals,"Exported/A1/Montreal/Scenarios/S7-bCMA_CT/region_household_marginals.csv",col_names = FALSE)
#region_person_marginals
region_person_marginals<-merge(CMAMAT[grepl("PP_",CMAMAT$Variable),],pp_variable_importance,by="Variable",all.x=TRUE)
region_person_marginals<-region_person_marginals[order(region_person_marginals$Importance),]
region_person_marginals<-region_person_marginals[region_person_marginals$Importance!=0,!names(region_person_marginals)%in%c("X","Definition","Description","Frequency","Importance")]
region_person_marginals$Categories[region_person_marginals$Variable=="PP_ID"]<-1
region_person_marginals<-rbind(c("variable_names","variable_categories",1:length(unique(GIS$CMA))),region_person_marginals)
region_person_marginals<-cbind(region_person_marginals[,1:2],c("region",rep("",nrow(region_person_marginals)-1)),region_person_marginals[,3:ncol(region_person_marginals)])
region_person_marginals$Variable[-1]<-paste("r",region_person_marginals$Variable[-1],sep="")
region_person_marginals<-data.frame(t(region_person_marginals))
temp<-region_person_marginals
region_person_marginals<-temp[,!((temp[1,]%in%c("rPP_AGEGRP") & temp[2,]%in%c(4:13)))]
region_person_marginals[-1:-3,(region_person_marginals[1,]=="rPP_AGEGRP" & region_person_marginals[2,]==1)]<-sum(apply(temp[-1:-3,temp[1,]=="rPP_AGEGRP" & temp[2,]%in%c(1:2)],2,as.numeric))
region_person_marginals[-1:-3,(region_person_marginals[1,]=="rPP_AGEGRP" & region_person_marginals[2,]==2)]<-sum(apply(temp[-1:-3,temp[1,]=="rPP_AGEGRP" & temp[2,]%in%c(3:11)],2,as.numeric))
region_person_marginals[-1:-3,(region_person_marginals[1,]=="rPP_AGEGRP" & region_person_marginals[2,]==3)]<-sum(apply(temp[-1:-3,temp[1,]=="rPP_AGEGRP" & temp[2,]%in%c(12:13)],2,as.numeric))
write_csv(region_person_marginals,"Exported/A1/Montreal/Scenarios/S7-bCMA_CT/region_person_marginals.csv",col_names = FALSE)
#geo_household_marginals
geo_household_marginals<-merge(CTMAT[grepl("HH_",CTMAT$Variable),],hh_variable_importance,by="Variable",all.x=TRUE)
geo_household_marginals<-geo_household_marginals[order(geo_household_marginals$Importance),]
geo_household_marginals<-geo_household_marginals[geo_household_marginals$Importance!=0,!names(geo_household_marginals)%in%c("X","Definition","Description","Frequency","Importance")]
geo_household_marginals$Categories[geo_household_marginals$Variable=="HH_ID"]<-1
geo_household_marginals<-rbind(c("variable_names","variable_categories",1:length(unique(GIS$CT))),geo_household_marginals)
geo_household_marginals<-cbind(geo_household_marginals[,1:2],c("geo",rep("",nrow(geo_household_marginals)-1)),geo_household_marginals[,3:ncol(geo_household_marginals)])
geo_household_marginals$Variable[-1]<-geo_household_marginals$Variable[-1]
geo_household_marginals<-data.frame(t(geo_household_marginals))
temp<-geo_household_marginals
geo_household_marginals<-temp[,!((temp[1,]%in%c("HH_TOTINCAT") & temp[2,]%in%c(5:16)))]
geo_household_marginals[-1:-3,(geo_household_marginals[1,]=="HH_TOTINCAT" & geo_household_marginals[2,]==1)]<-rowSums(apply(temp[-1:-3,temp[1,]=="HH_TOTINCAT" & temp[2,]%in%c(1:6)],2,as.numeric))
geo_household_marginals[-1:-3,(geo_household_marginals[1,]=="HH_TOTINCAT" & geo_household_marginals[2,]==2)]<-rowSums(apply(temp[-1:-3,temp[1,]=="HH_TOTINCAT" & temp[2,]%in%c(7:11)],2,as.numeric))
geo_household_marginals[-1:-3,(geo_household_marginals[1,]=="HH_TOTINCAT" & geo_household_marginals[2,]==3)]<-rowSums(apply(temp[-1:-3,temp[1,]=="HH_TOTINCAT" & temp[2,]%in%c(12:15)],2,as.numeric))
geo_household_marginals[-1:-3,(geo_household_marginals[1,]=="HH_TOTINCAT" & geo_household_marginals[2,]==4)]<-as.numeric(temp[-1:-3,temp[1,]=="HH_TOTINCAT" & temp[2,]%in%c(16)])
write_csv(geo_household_marginals,"Exported/A1/Montreal/Scenarios/S7-bCMA_CT/geo_household_marginals.csv",col_names = FALSE)
#geo_person_marginals
geo_person_marginals<-merge(CTMAT[grepl("PP_",CTMAT$Variable),],pp_variable_importance,by="Variable",all.x=TRUE)
geo_person_marginals<-geo_person_marginals[order(geo_person_marginals$Importance),]
geo_person_marginals<-geo_person_marginals[geo_person_marginals$Importance!=0,!names(geo_person_marginals)%in%c("X","Definition","Description","Frequency","Importance")]
geo_person_marginals$Categories[geo_person_marginals$Variable=="PP_ID"]<-1
geo_person_marginals<-rbind(c("variable_names","variable_categories",1:length(unique(GIS$CT))),geo_person_marginals)
geo_person_marginals<-cbind(geo_person_marginals[,1:2],c("geo",rep("",nrow(geo_person_marginals)-1)),geo_person_marginals[,3:ncol(geo_person_marginals)])
geo_person_marginals$Variable[-1]<-geo_person_marginals$Variable[-1]
geo_person_marginals<-data.frame(t(geo_person_marginals))
temp<-geo_person_marginals
geo_person_marginals<-temp[,!((temp[1,]%in%c("PP_AGEGRP") & temp[2,]%in%c(4:13)))]
geo_person_marginals[-1:-3,(geo_person_marginals[1,]=="PP_AGEGRP" & geo_person_marginals[2,]==1)]<-rowSums(apply(temp[-1:-3,temp[1,]=="PP_AGEGRP" & temp[2,]%in%c(1:2)],2,as.numeric))
geo_person_marginals[-1:-3,(geo_person_marginals[1,]=="PP_AGEGRP" & geo_person_marginals[2,]==2)]<-rowSums(apply(temp[-1:-3,temp[1,]=="PP_AGEGRP" & temp[2,]%in%c(3:11)],2,as.numeric))
geo_person_marginals[-1:-3,(geo_person_marginals[1,]=="PP_AGEGRP" & geo_person_marginals[2,]==3)]<-rowSums(apply(temp[-1:-3,temp[1,]=="PP_AGEGRP" & temp[2,]%in%c(12:13)],2,as.numeric))
write_csv(geo_person_marginals,"Exported/A1/Montreal/Scenarios/S7-bCMA_CT/geo_person_marginals.csv",col_names = FALSE)
#household_sample
write_csv(household_sample,"Exported/A1/Montreal/Scenarios/S7-bCMA_CT/household_sample.csv",col_names = TRUE)
#person_sample
write_csv(person_sample,"Exported/A1/Montreal/Scenarios/S7-bCMA_CT/person_sample.csv",col_names = TRUE)

###S8-bDA
GIS<-GIS_CMA_DA
#region_geo_mapping
geo<-1:length(unique(GIS$DA))
region<-1:length(unique(GIS$DA))
region_geo_mapping<-data.frame(region,geo)
write.csv(region_geo_mapping,"Exported/A1/Montreal/Scenarios/S8-bDA/region_geo_mapping.csv",row.names=FALSE)
#geo_sample_mapping
geo<-1:length(unique(GIS$DA))
sample_geo<-1
geo_sample_mapping<-data.frame(geo,sample_geo)
write.csv(geo_sample_mapping,"Exported/A1/Montreal/Scenarios/S8-bDA/geo_sample_mapping.csv",row.names=FALSE)
#region_sample_mapping
region<-1:length(unique(GIS$DA))
sample_geo<--1
region_sample_mapping<-data.frame(region,sample_geo)
write.csv(region_sample_mapping,"Exported/A1/Montreal/Scenarios/S8-bDA/region_sample_mapping.csv",row.names=FALSE)
#region_household_marginals
region_household_marginals<-merge(DAMAT[grepl("HH_",DAMAT$Variable),],hh_variable_importance,by="Variable",all.x=TRUE)
region_household_marginals<-region_household_marginals[order(region_household_marginals$Importance),]
region_household_marginals<-region_household_marginals[region_household_marginals$Importance!=0,!names(region_household_marginals)%in%c("X","Definition","Description","Frequency","Importance")]
region_household_marginals$Categories[region_household_marginals$Variable=="HH_ID"]<-1
region_household_marginals<-rbind(c("variable_names","variable_categories",1:length(unique(GIS$DA))),region_household_marginals)
region_household_marginals<-cbind(region_household_marginals[,1:2],c("region",rep("",nrow(region_household_marginals)-1)),region_household_marginals[,3:ncol(region_household_marginals)])
region_household_marginals$Variable[-1]<-paste("r",region_household_marginals$Variable[-1],sep="")
region_household_marginals<-data.frame(t(region_household_marginals))
temp<-region_household_marginals
region_household_marginals<-temp[,!((temp[1,]%in%c("rHH_TOTINCAT") & temp[2,]%in%c(5:16)))]
region_household_marginals[-1:-3,(region_household_marginals[1,]=="rHH_TOTINCAT" & region_household_marginals[2,]==1)]<-rowSums(apply(temp[-1:-3,temp[1,]=="rHH_TOTINCAT" & temp[2,]%in%c(1:6)],2,as.numeric))
region_household_marginals[-1:-3,(region_household_marginals[1,]=="rHH_TOTINCAT" & region_household_marginals[2,]==2)]<-rowSums(apply(temp[-1:-3,temp[1,]=="rHH_TOTINCAT" & temp[2,]%in%c(7:11)],2,as.numeric))
region_household_marginals[-1:-3,(region_household_marginals[1,]=="rHH_TOTINCAT" & region_household_marginals[2,]==3)]<-rowSums(apply(temp[-1:-3,temp[1,]=="rHH_TOTINCAT" & temp[2,]%in%c(12:15)],2,as.numeric))
region_household_marginals[-1:-3,(region_household_marginals[1,]=="rHH_TOTINCAT" & region_household_marginals[2,]==4)]<-as.numeric(temp[-1:-3,temp[1,]=="rHH_TOTINCAT" & temp[2,]%in%c(16)])
write_csv(region_household_marginals,"Exported/A1/Montreal/Scenarios/S8-bDA/region_household_marginals.csv",col_names = FALSE)
#region_person_marginals
region_person_marginals<-merge(DAMAT[grepl("PP_",DAMAT$Variable),],pp_variable_importance,by="Variable",all.x=TRUE)
region_person_marginals<-region_person_marginals[order(region_person_marginals$Importance),]
region_person_marginals<-region_person_marginals[region_person_marginals$Importance!=0,!names(region_person_marginals)%in%c("X","Definition","Description","Frequency","Importance")]
region_person_marginals$Categories[region_person_marginals$Variable=="PP_ID"]<-1
region_person_marginals<-rbind(c("variable_names","variable_categories",1:length(unique(GIS$DA))),region_person_marginals)
region_person_marginals<-cbind(region_person_marginals[,1:2],c("region",rep("",nrow(region_person_marginals)-1)),region_person_marginals[,3:ncol(region_person_marginals)])
region_person_marginals$Variable[-1]<-paste("r",region_person_marginals$Variable[-1],sep="")
region_person_marginals<-data.frame(t(region_person_marginals))
temp<-region_person_marginals
region_person_marginals<-temp[,!((temp[1,]%in%c("rPP_AGEGRP") & temp[2,]%in%c(4:13)))]
region_person_marginals[-1:-3,(region_person_marginals[1,]=="rPP_AGEGRP" & region_person_marginals[2,]==1)]<-rowSums(apply(temp[-1:-3,temp[1,]=="rPP_AGEGRP" & temp[2,]%in%c(1:2)],2,as.numeric))
region_person_marginals[-1:-3,(region_person_marginals[1,]=="rPP_AGEGRP" & region_person_marginals[2,]==2)]<-rowSums(apply(temp[-1:-3,temp[1,]=="rPP_AGEGRP" & temp[2,]%in%c(3:11)],2,as.numeric))
region_person_marginals[-1:-3,(region_person_marginals[1,]=="rPP_AGEGRP" & region_person_marginals[2,]==3)]<-rowSums(apply(temp[-1:-3,temp[1,]=="rPP_AGEGRP" & temp[2,]%in%c(12:13)],2,as.numeric))
write_csv(region_person_marginals,"Exported/A1/Montreal/Scenarios/S8-bDA/region_person_marginals.csv",col_names = FALSE)
#geo_household_marginals
geo_household_marginals<-merge(DAMAT[grepl("HH_",DAMAT$Variable),],hh_variable_importance,by="Variable",all.x=TRUE)
geo_household_marginals<-geo_household_marginals[order(geo_household_marginals$Importance),]
geo_household_marginals<-geo_household_marginals[geo_household_marginals$Importance!=0,!names(geo_household_marginals)%in%c("X","Definition","Description","Frequency","Importance")]
geo_household_marginals$Categories[geo_household_marginals$Variable=="HH_ID"]<-1
geo_household_marginals<-rbind(c("variable_names","variable_categories",1:length(unique(GIS$DA))),geo_household_marginals)
geo_household_marginals<-cbind(geo_household_marginals[,1:2],c("geo",rep("",nrow(geo_household_marginals)-1)),geo_household_marginals[,3:ncol(geo_household_marginals)])
geo_household_marginals$Variable[-1]<-geo_household_marginals$Variable[-1]
geo_household_marginals<-data.frame(t(geo_household_marginals))
temp<-geo_household_marginals
geo_household_marginals<-temp[,!((temp[1,]%in%c("HH_TOTINCAT") & temp[2,]%in%c(5:16)))]
geo_household_marginals[-1:-3,(geo_household_marginals[1,]=="HH_TOTINCAT" & geo_household_marginals[2,]==1)]<-rowSums(apply(temp[-1:-3,temp[1,]=="HH_TOTINCAT" & temp[2,]%in%c(1:6)],2,as.numeric))
geo_household_marginals[-1:-3,(geo_household_marginals[1,]=="HH_TOTINCAT" & geo_household_marginals[2,]==2)]<-rowSums(apply(temp[-1:-3,temp[1,]=="HH_TOTINCAT" & temp[2,]%in%c(7:11)],2,as.numeric))
geo_household_marginals[-1:-3,(geo_household_marginals[1,]=="HH_TOTINCAT" & geo_household_marginals[2,]==3)]<-rowSums(apply(temp[-1:-3,temp[1,]=="HH_TOTINCAT" & temp[2,]%in%c(12:15)],2,as.numeric))
geo_household_marginals[-1:-3,(geo_household_marginals[1,]=="HH_TOTINCAT" & geo_household_marginals[2,]==4)]<-as.numeric(temp[-1:-3,temp[1,]=="HH_TOTINCAT" & temp[2,]%in%c(16)])
write_csv(geo_household_marginals,"Exported/A1/Montreal/Scenarios/S8-bDA/geo_household_marginals.csv",col_names = FALSE)
#geo_person_marginals
geo_person_marginals<-merge(DAMAT[grepl("PP_",DAMAT$Variable),],pp_variable_importance,by="Variable",all.x=TRUE)
geo_person_marginals<-geo_person_marginals[order(geo_person_marginals$Importance),]
geo_person_marginals<-geo_person_marginals[geo_person_marginals$Importance!=0,!names(geo_person_marginals)%in%c("X","Definition","Description","Frequency","Importance")]
geo_person_marginals$Categories[geo_person_marginals$Variable=="PP_ID"]<-1
geo_person_marginals<-rbind(c("variable_names","variable_categories",1:length(unique(GIS$DA))),geo_person_marginals)
geo_person_marginals<-cbind(geo_person_marginals[,1:2],c("geo",rep("",nrow(geo_person_marginals)-1)),geo_person_marginals[,3:ncol(geo_person_marginals)])
geo_person_marginals$Variable[-1]<-geo_person_marginals$Variable[-1]
geo_person_marginals<-data.frame(t(geo_person_marginals))
temp<-geo_person_marginals
geo_person_marginals<-temp[,!((temp[1,]%in%c("PP_AGEGRP") & temp[2,]%in%c(4:13)))]
geo_person_marginals[-1:-3,(geo_person_marginals[1,]=="PP_AGEGRP" & geo_person_marginals[2,]==1)]<-rowSums(apply(temp[-1:-3,temp[1,]=="PP_AGEGRP" & temp[2,]%in%c(1:2)],2,as.numeric))
geo_person_marginals[-1:-3,(geo_person_marginals[1,]=="PP_AGEGRP" & geo_person_marginals[2,]==2)]<-rowSums(apply(temp[-1:-3,temp[1,]=="PP_AGEGRP" & temp[2,]%in%c(3:11)],2,as.numeric))
geo_person_marginals[-1:-3,(geo_person_marginals[1,]=="PP_AGEGRP" & geo_person_marginals[2,]==3)]<-rowSums(apply(temp[-1:-3,temp[1,]=="PP_AGEGRP" & temp[2,]%in%c(12:13)],2,as.numeric))
write_csv(geo_person_marginals,"Exported/A1/Montreal/Scenarios/S8-bDA/geo_person_marginals.csv",col_names = FALSE)
#household_sample
write_csv(household_sample,"Exported/A1/Montreal/Scenarios/S8-bDA/household_sample.csv",col_names = TRUE)
#person_sample
write_csv(person_sample,"Exported/A1/Montreal/Scenarios/S8-bDA/person_sample.csv",col_names = TRUE)

###S9-bCMA_DA
GIS<-GIS_CMA_DA
#region_geo_mapping
geo<-1:length(unique(GIS$DA))
region<-1
region_geo_mapping<-data.frame(region,geo)
write.csv(region_geo_mapping,"Exported/A1/Montreal/Scenarios/S9-bCMA_DA/region_geo_mapping.csv",row.names=FALSE)
#geo_sample_mapping
geo<-1:length(unique(GIS$DA))
sample_geo<-1
geo_sample_mapping<-data.frame(geo,sample_geo)
write.csv(geo_sample_mapping,"Exported/A1/Montreal/Scenarios/S9-bCMA_DA/geo_sample_mapping.csv",row.names=FALSE)
#region_sample_mapping
region<-1
sample_geo<--1
region_sample_mapping<-data.frame(region,sample_geo)
write.csv(region_sample_mapping,"Exported/A1/Montreal/Scenarios/S9-bCMA_DA/region_sample_mapping.csv",row.names=FALSE)
#region_household_marginals
region_household_marginals<-merge(CMAMAT[grepl("HH_",CMAMAT$Variable),],hh_variable_importance,by="Variable",all.x=TRUE)
region_household_marginals<-region_household_marginals[order(region_household_marginals$Importance),]
region_household_marginals<-region_household_marginals[region_household_marginals$Importance!=0,!names(region_household_marginals)%in%c("X","Definition","Description","Frequency","Importance")]
region_household_marginals$Categories[region_household_marginals$Variable=="HH_ID"]<-1
region_household_marginals<-rbind(c("variable_names","variable_categories",1:length(unique(GIS$CMA))),region_household_marginals)
region_household_marginals<-cbind(region_household_marginals[,1:2],c("region",rep("",nrow(region_household_marginals)-1)),region_household_marginals[,3:ncol(region_household_marginals)])
region_household_marginals$Variable[-1]<-paste("r",region_household_marginals$Variable[-1],sep="")
region_household_marginals<-data.frame(t(region_household_marginals))
temp<-region_household_marginals
region_household_marginals<-temp[,!((temp[1,]%in%c("rHH_TOTINCAT") & temp[2,]%in%c(5:16)))]
region_household_marginals[-1:-3,(region_household_marginals[1,]=="rHH_TOTINCAT" & region_household_marginals[2,]==1)]<-sum(apply(temp[-1:-3,temp[1,]=="rHH_TOTINCAT" & temp[2,]%in%c(1:6)],2,as.numeric))
region_household_marginals[-1:-3,(region_household_marginals[1,]=="rHH_TOTINCAT" & region_household_marginals[2,]==2)]<-sum(apply(temp[-1:-3,temp[1,]=="rHH_TOTINCAT" & temp[2,]%in%c(7:11)],2,as.numeric))
region_household_marginals[-1:-3,(region_household_marginals[1,]=="rHH_TOTINCAT" & region_household_marginals[2,]==3)]<-sum(apply(temp[-1:-3,temp[1,]=="rHH_TOTINCAT" & temp[2,]%in%c(12:15)],2,as.numeric))
region_household_marginals[-1:-3,(region_household_marginals[1,]=="rHH_TOTINCAT" & region_household_marginals[2,]==4)]<-as.numeric(temp[-1:-3,temp[1,]=="rHH_TOTINCAT" & temp[2,]%in%c(16)])
write_csv(region_household_marginals,"Exported/A1/Montreal/Scenarios/S9-bCMA_DA/region_household_marginals.csv",col_names = FALSE)
#region_person_marginals
region_person_marginals<-merge(CMAMAT[grepl("PP_",CMAMAT$Variable),],pp_variable_importance,by="Variable",all.x=TRUE)
region_person_marginals<-region_person_marginals[order(region_person_marginals$Importance),]
region_person_marginals<-region_person_marginals[region_person_marginals$Importance!=0,!names(region_person_marginals)%in%c("X","Definition","Description","Frequency","Importance")]
region_person_marginals$Categories[region_person_marginals$Variable=="PP_ID"]<-1
region_person_marginals<-rbind(c("variable_names","variable_categories",1:length(unique(GIS$CMA))),region_person_marginals)
region_person_marginals<-cbind(region_person_marginals[,1:2],c("region",rep("",nrow(region_person_marginals)-1)),region_person_marginals[,3:ncol(region_person_marginals)])
region_person_marginals$Variable[-1]<-paste("r",region_person_marginals$Variable[-1],sep="")
region_person_marginals<-data.frame(t(region_person_marginals))
temp<-region_person_marginals
region_person_marginals<-temp[,!((temp[1,]%in%c("rPP_AGEGRP") & temp[2,]%in%c(4:13)))]
region_person_marginals[-1:-3,(region_person_marginals[1,]=="rPP_AGEGRP" & region_person_marginals[2,]==1)]<-sum(apply(temp[-1:-3,temp[1,]=="rPP_AGEGRP" & temp[2,]%in%c(1:2)],2,as.numeric))
region_person_marginals[-1:-3,(region_person_marginals[1,]=="rPP_AGEGRP" & region_person_marginals[2,]==2)]<-sum(apply(temp[-1:-3,temp[1,]=="rPP_AGEGRP" & temp[2,]%in%c(3:11)],2,as.numeric))
region_person_marginals[-1:-3,(region_person_marginals[1,]=="rPP_AGEGRP" & region_person_marginals[2,]==3)]<-sum(apply(temp[-1:-3,temp[1,]=="rPP_AGEGRP" & temp[2,]%in%c(12:13)],2,as.numeric))
write_csv(region_person_marginals,"Exported/A1/Montreal/Scenarios/S9-bCMA_DA/region_person_marginals.csv",col_names = FALSE)
#geo_household_marginals
geo_household_marginals<-merge(DAMAT[grepl("HH_",DAMAT$Variable),],hh_variable_importance,by="Variable",all.x=TRUE)
geo_household_marginals<-geo_household_marginals[order(geo_household_marginals$Importance),]
geo_household_marginals<-geo_household_marginals[geo_household_marginals$Importance!=0,!names(geo_household_marginals)%in%c("X","Definition","Description","Frequency","Importance")]
geo_household_marginals$Categories[geo_household_marginals$Variable=="HH_ID"]<-1
geo_household_marginals<-rbind(c("variable_names","variable_categories",1:length(unique(GIS$DA))),geo_household_marginals)
geo_household_marginals<-cbind(geo_household_marginals[,1:2],c("geo",rep("",nrow(geo_household_marginals)-1)),geo_household_marginals[,3:ncol(geo_household_marginals)])
geo_household_marginals$Variable[-1]<-geo_household_marginals$Variable[-1]
geo_household_marginals<-data.frame(t(geo_household_marginals))
temp<-geo_household_marginals
geo_household_marginals<-temp[,!((temp[1,]%in%c("HH_TOTINCAT") & temp[2,]%in%c(5:16)))]
geo_household_marginals[-1:-3,(geo_household_marginals[1,]=="HH_TOTINCAT" & geo_household_marginals[2,]==1)]<-rowSums(apply(temp[-1:-3,temp[1,]=="HH_TOTINCAT" & temp[2,]%in%c(1:6)],2,as.numeric))
geo_household_marginals[-1:-3,(geo_household_marginals[1,]=="HH_TOTINCAT" & geo_household_marginals[2,]==2)]<-rowSums(apply(temp[-1:-3,temp[1,]=="HH_TOTINCAT" & temp[2,]%in%c(7:11)],2,as.numeric))
geo_household_marginals[-1:-3,(geo_household_marginals[1,]=="HH_TOTINCAT" & geo_household_marginals[2,]==3)]<-rowSums(apply(temp[-1:-3,temp[1,]=="HH_TOTINCAT" & temp[2,]%in%c(12:15)],2,as.numeric))
geo_household_marginals[-1:-3,(geo_household_marginals[1,]=="HH_TOTINCAT" & geo_household_marginals[2,]==4)]<-as.numeric(temp[-1:-3,temp[1,]=="HH_TOTINCAT" & temp[2,]%in%c(16)])
write_csv(geo_household_marginals,"Exported/A1/Montreal/Scenarios/S9-bCMA_DA/geo_household_marginals.csv",col_names = FALSE)
#geo_person_marginals
geo_person_marginals<-merge(DAMAT[grepl("PP_",DAMAT$Variable),],pp_variable_importance,by="Variable",all.x=TRUE)
geo_person_marginals<-geo_person_marginals[order(geo_person_marginals$Importance),]
geo_person_marginals<-geo_person_marginals[geo_person_marginals$Importance!=0,!names(geo_person_marginals)%in%c("X","Definition","Description","Frequency","Importance")]
geo_person_marginals$Categories[geo_person_marginals$Variable=="PP_ID"]<-1
geo_person_marginals<-rbind(c("variable_names","variable_categories",1:length(unique(GIS$DA))),geo_person_marginals)
geo_person_marginals<-cbind(geo_person_marginals[,1:2],c("geo",rep("",nrow(geo_person_marginals)-1)),geo_person_marginals[,3:ncol(geo_person_marginals)])
geo_person_marginals$Variable[-1]<-geo_person_marginals$Variable[-1]
geo_person_marginals<-data.frame(t(geo_person_marginals))
temp<-geo_person_marginals
geo_person_marginals<-temp[,!((temp[1,]%in%c("PP_AGEGRP") & temp[2,]%in%c(4:13)))]
geo_person_marginals[-1:-3,(geo_person_marginals[1,]=="PP_AGEGRP" & geo_person_marginals[2,]==1)]<-rowSums(apply(temp[-1:-3,temp[1,]=="PP_AGEGRP" & temp[2,]%in%c(1:2)],2,as.numeric))
geo_person_marginals[-1:-3,(geo_person_marginals[1,]=="PP_AGEGRP" & geo_person_marginals[2,]==2)]<-rowSums(apply(temp[-1:-3,temp[1,]=="PP_AGEGRP" & temp[2,]%in%c(3:11)],2,as.numeric))
geo_person_marginals[-1:-3,(geo_person_marginals[1,]=="PP_AGEGRP" & geo_person_marginals[2,]==3)]<-rowSums(apply(temp[-1:-3,temp[1,]=="PP_AGEGRP" & temp[2,]%in%c(12:13)],2,as.numeric))
write_csv(geo_person_marginals,"Exported/A1/Montreal/Scenarios/S9-bCMA_DA/geo_person_marginals.csv",col_names = FALSE)
#household_sample
write_csv(household_sample,"Exported/A1/Montreal/Scenarios/S9-bCMA_DA/household_sample.csv",col_names = TRUE)
#person_sample
write_csv(person_sample,"Exported/A1/Montreal/Scenarios/S9-bCMA_DA/person_sample.csv",col_names = TRUE)

CMAMAT<-read.csv("Exported/A1/Montreal/Inputs/Harmonized Census Totals/hCMAMAT.csv")[,-1]
CSDMAT<-read.csv("Exported/A1/Montreal/Inputs/Harmonized Census Totals/hCSDMAT.csv")[,-1]
ADAMAT<-read.csv("Exported/A1/Montreal/Inputs/Harmonized Census Totals/hADAMAT.csv")[,-1]
CTMAT<-read.csv("Exported/A1/Montreal/Inputs/Harmonized Census Totals/hCTMAT.csv")[,-1]
DAMAT<-read.csv("Exported/A1/Montreal/Inputs/Harmonized Census Totals/hDAMAT.csv")[,-1]

###S10-hCMA
GIS<-GIS_CMA_CSD
#region_geo_mapping
geo<-1:length(unique(GIS$CMA))
region<-1:length(unique(GIS$CMA))
region_geo_mapping<-data.frame(region,geo)
write.csv(region_geo_mapping,"Exported/A1/Montreal/Scenarios/S10-hCMA/region_geo_mapping.csv",row.names=FALSE)
#geo_sample_mapping
geo<-1:length(unique(GIS$CMA))
sample_geo<-1
geo_sample_mapping<-data.frame(geo,sample_geo)
write.csv(geo_sample_mapping,"Exported/A1/Montreal/Scenarios/S10-hCMA/geo_sample_mapping.csv",row.names=FALSE)
#region_sample_mapping
region<-1:length(unique(GIS$CMA))
sample_geo<--1
region_sample_mapping<-data.frame(region,sample_geo)
write.csv(region_sample_mapping,"Exported/A1/Montreal/Scenarios/S10-hCMA/region_sample_mapping.csv",row.names=FALSE)
#region_household_marginals
region_household_marginals<-merge(CMAMAT[grepl("HH_",CMAMAT$Variable),],hh_variable_importance,by="Variable",all.x=TRUE)
region_household_marginals<-region_household_marginals[order(region_household_marginals$Importance),]
region_household_marginals<-region_household_marginals[region_household_marginals$Importance!=0,!names(region_household_marginals)%in%c("X","Definition","Description","Frequency","Importance")]
region_household_marginals$Categories[region_household_marginals$Variable=="HH_ID"]<-1
region_household_marginals<-rbind(c("variable_names","variable_categories",1:length(unique(GIS$CMA))),region_household_marginals)
region_household_marginals<-cbind(region_household_marginals[,1:2],c("region",rep("",nrow(region_household_marginals)-1)),region_household_marginals[,3:ncol(region_household_marginals)])
region_household_marginals$Variable[-1]<-paste("r",region_household_marginals$Variable[-1],sep="")
region_household_marginals<-data.frame(t(region_household_marginals))
temp<-region_household_marginals
region_household_marginals<-temp[,!((temp[1,]%in%c("rHH_TOTINCAT") & temp[2,]%in%c(5:16)))]
region_household_marginals[-1:-3,(region_household_marginals[1,]=="rHH_TOTINCAT" & region_household_marginals[2,]==1)]<-sum(apply(temp[-1:-3,temp[1,]=="rHH_TOTINCAT" & temp[2,]%in%c(1:6)],2,as.numeric))
region_household_marginals[-1:-3,(region_household_marginals[1,]=="rHH_TOTINCAT" & region_household_marginals[2,]==2)]<-sum(apply(temp[-1:-3,temp[1,]=="rHH_TOTINCAT" & temp[2,]%in%c(7:11)],2,as.numeric))
region_household_marginals[-1:-3,(region_household_marginals[1,]=="rHH_TOTINCAT" & region_household_marginals[2,]==3)]<-sum(apply(temp[-1:-3,temp[1,]=="rHH_TOTINCAT" & temp[2,]%in%c(12:15)],2,as.numeric))
region_household_marginals[-1:-3,(region_household_marginals[1,]=="rHH_TOTINCAT" & region_household_marginals[2,]==4)]<-as.numeric(temp[-1:-3,temp[1,]=="rHH_TOTINCAT" & temp[2,]%in%c(16)])
write_csv(region_household_marginals,"Exported/A1/Montreal/Scenarios/S10-hCMA/region_household_marginals.csv",col_names = FALSE)
#region_person_marginals
region_person_marginals<-merge(CMAMAT[grepl("PP_",CMAMAT$Variable),],pp_variable_importance,by="Variable",all.x=TRUE)
region_person_marginals<-region_person_marginals[order(region_person_marginals$Importance),]
region_person_marginals<-region_person_marginals[region_person_marginals$Importance!=0,!names(region_person_marginals)%in%c("X","Definition","Description","Frequency","Importance")]
region_person_marginals$Categories[region_person_marginals$Variable=="PP_ID"]<-1
region_person_marginals<-rbind(c("variable_names","variable_categories",1:length(unique(GIS$CMA))),region_person_marginals)
region_person_marginals<-cbind(region_person_marginals[,1:2],c("region",rep("",nrow(region_person_marginals)-1)),region_person_marginals[,3:ncol(region_person_marginals)])
region_person_marginals$Variable[-1]<-paste("r",region_person_marginals$Variable[-1],sep="")
region_person_marginals<-data.frame(t(region_person_marginals))
temp<-region_person_marginals
region_person_marginals<-temp[,!((temp[1,]%in%c("rPP_AGEGRP") & temp[2,]%in%c(4:13)))]
region_person_marginals[-1:-3,(region_person_marginals[1,]=="rPP_AGEGRP" & region_person_marginals[2,]==1)]<-sum(apply(temp[-1:-3,temp[1,]=="rPP_AGEGRP" & temp[2,]%in%c(1:2)],2,as.numeric))
region_person_marginals[-1:-3,(region_person_marginals[1,]=="rPP_AGEGRP" & region_person_marginals[2,]==2)]<-sum(apply(temp[-1:-3,temp[1,]=="rPP_AGEGRP" & temp[2,]%in%c(3:11)],2,as.numeric))
region_person_marginals[-1:-3,(region_person_marginals[1,]=="rPP_AGEGRP" & region_person_marginals[2,]==3)]<-sum(apply(temp[-1:-3,temp[1,]=="rPP_AGEGRP" & temp[2,]%in%c(12:13)],2,as.numeric))
write_csv(region_person_marginals,"Exported/A1/Montreal/Scenarios/S10-hCMA/region_person_marginals.csv",col_names = FALSE)
#geo_household_marginals
geo_household_marginals<-merge(CMAMAT[grepl("HH_",CMAMAT$Variable),],hh_variable_importance,by="Variable",all.x=TRUE)
geo_household_marginals<-geo_household_marginals[order(geo_household_marginals$Importance),]
geo_household_marginals<-geo_household_marginals[geo_household_marginals$Importance!=0,!names(geo_household_marginals)%in%c("X","Definition","Description","Frequency","Importance")]
geo_household_marginals$Categories[geo_household_marginals$Variable=="HH_ID"]<-1
geo_household_marginals<-rbind(c("variable_names","variable_categories",1:length(unique(GIS$CMA))),geo_household_marginals)
geo_household_marginals<-cbind(geo_household_marginals[,1:2],c("geo",rep("",nrow(geo_household_marginals)-1)),geo_household_marginals[,3:ncol(geo_household_marginals)])
geo_household_marginals$Variable[-1]<-geo_household_marginals$Variable[-1]
geo_household_marginals<-data.frame(t(geo_household_marginals))
temp<-geo_household_marginals
geo_household_marginals<-temp[,!((temp[1,]%in%c("HH_TOTINCAT") & temp[2,]%in%c(5:16)))]
geo_household_marginals[-1:-3,(geo_household_marginals[1,]=="HH_TOTINCAT" & geo_household_marginals[2,]==1)]<-sum(apply(temp[-1:-3,temp[1,]=="HH_TOTINCAT" & temp[2,]%in%c(1:6)],2,as.numeric))
geo_household_marginals[-1:-3,(geo_household_marginals[1,]=="HH_TOTINCAT" & geo_household_marginals[2,]==2)]<-sum(apply(temp[-1:-3,temp[1,]=="HH_TOTINCAT" & temp[2,]%in%c(7:11)],2,as.numeric))
geo_household_marginals[-1:-3,(geo_household_marginals[1,]=="HH_TOTINCAT" & geo_household_marginals[2,]==3)]<-sum(apply(temp[-1:-3,temp[1,]=="HH_TOTINCAT" & temp[2,]%in%c(12:15)],2,as.numeric))
geo_household_marginals[-1:-3,(geo_household_marginals[1,]=="HH_TOTINCAT" & geo_household_marginals[2,]==4)]<-as.numeric(temp[-1:-3,temp[1,]=="HH_TOTINCAT" & temp[2,]%in%c(16)])
write_csv(geo_household_marginals,"Exported/A1/Montreal/Scenarios/S10-hCMA/geo_household_marginals.csv",col_names = FALSE)
#geo_person_marginals
geo_person_marginals<-merge(CMAMAT[grepl("PP_",CMAMAT$Variable),],pp_variable_importance,by="Variable",all.x=TRUE)
geo_person_marginals<-geo_person_marginals[order(geo_person_marginals$Importance),]
geo_person_marginals<-geo_person_marginals[geo_person_marginals$Importance!=0,!names(geo_person_marginals)%in%c("X","Definition","Description","Frequency","Importance")]
geo_person_marginals$Categories[geo_person_marginals$Variable=="PP_ID"]<-1
geo_person_marginals<-rbind(c("variable_names","variable_categories",1:length(unique(GIS$CMA))),geo_person_marginals)
geo_person_marginals<-cbind(geo_person_marginals[,1:2],c("geo",rep("",nrow(geo_person_marginals)-1)),geo_person_marginals[,3:ncol(geo_person_marginals)])
geo_person_marginals$Variable[-1]<-geo_person_marginals$Variable[-1]
geo_person_marginals<-data.frame(t(geo_person_marginals))
temp<-geo_person_marginals
geo_person_marginals<-temp[,!((temp[1,]%in%c("PP_AGEGRP") & temp[2,]%in%c(4:13)))]
geo_person_marginals[-1:-3,(geo_person_marginals[1,]=="PP_AGEGRP" & geo_person_marginals[2,]==1)]<-sum(apply(temp[-1:-3,temp[1,]=="PP_AGEGRP" & temp[2,]%in%c(1:2)],2,as.numeric))
geo_person_marginals[-1:-3,(geo_person_marginals[1,]=="PP_AGEGRP" & geo_person_marginals[2,]==2)]<-sum(apply(temp[-1:-3,temp[1,]=="PP_AGEGRP" & temp[2,]%in%c(3:11)],2,as.numeric))
geo_person_marginals[-1:-3,(geo_person_marginals[1,]=="PP_AGEGRP" & geo_person_marginals[2,]==3)]<-sum(apply(temp[-1:-3,temp[1,]=="PP_AGEGRP" & temp[2,]%in%c(12:13)],2,as.numeric))
write_csv(geo_person_marginals,"Exported/A1/Montreal/Scenarios/S10-hCMA/geo_person_marginals.csv",col_names = FALSE)
#household_sample
write_csv(household_sample,"Exported/A1/Montreal/Scenarios/S10-hCMA/household_sample.csv",col_names = TRUE)
#person_sample
write_csv(person_sample,"Exported/A1/Montreal/Scenarios/S10-hCMA/person_sample.csv",col_names = TRUE)

###S11-hCSD
GIS<-GIS_CMA_CSD
#region_geo_mapping
geo<-1:length(unique(GIS$CSD))
region<-1:length(unique(GIS$CSD))
region_geo_mapping<-data.frame(region,geo)
write.csv(region_geo_mapping,"Exported/A1/Montreal/Scenarios/S11-hCSD/region_geo_mapping.csv",row.names=FALSE)
#geo_sample_mapping
geo<-1:length(unique(GIS$CSD))
sample_geo<-1
geo_sample_mapping<-data.frame(geo,sample_geo)
write.csv(geo_sample_mapping,"Exported/A1/Montreal/Scenarios/S11-hCSD/geo_sample_mapping.csv",row.names=FALSE)
#region_sample_mapping
region<-1:length(unique(GIS$CSD))
sample_geo<--1
region_sample_mapping<-data.frame(region,sample_geo)
write.csv(region_sample_mapping,"Exported/A1/Montreal/Scenarios/S11-hCSD/region_sample_mapping.csv",row.names=FALSE)
#region_household_marginals
region_household_marginals<-merge(CSDMAT[grepl("HH_",CSDMAT$Variable),],hh_variable_importance,by="Variable",all.x=TRUE)
region_household_marginals<-region_household_marginals[order(region_household_marginals$Importance),]
region_household_marginals<-region_household_marginals[region_household_marginals$Importance!=0,!names(region_household_marginals)%in%c("X","Definition","Description","Frequency","Importance")]
region_household_marginals$Categories[region_household_marginals$Variable=="HH_ID"]<-1
region_household_marginals<-rbind(c("variable_names","variable_categories",1:length(unique(GIS$CSD))),region_household_marginals)
region_household_marginals<-cbind(region_household_marginals[,1:2],c("region",rep("",nrow(region_household_marginals)-1)),region_household_marginals[,3:ncol(region_household_marginals)])
region_household_marginals$Variable[-1]<-paste("r",region_household_marginals$Variable[-1],sep="")
region_household_marginals<-data.frame(t(region_household_marginals))
temp<-region_household_marginals
region_household_marginals<-temp[,!((temp[1,]%in%c("rHH_TOTINCAT") & temp[2,]%in%c(5:16)))]
region_household_marginals[-1:-3,(region_household_marginals[1,]=="rHH_TOTINCAT" & region_household_marginals[2,]==1)]<-rowSums(apply(temp[-1:-3,temp[1,]=="rHH_TOTINCAT" & temp[2,]%in%c(1:6)],2,as.numeric))
region_household_marginals[-1:-3,(region_household_marginals[1,]=="rHH_TOTINCAT" & region_household_marginals[2,]==2)]<-rowSums(apply(temp[-1:-3,temp[1,]=="rHH_TOTINCAT" & temp[2,]%in%c(7:11)],2,as.numeric))
region_household_marginals[-1:-3,(region_household_marginals[1,]=="rHH_TOTINCAT" & region_household_marginals[2,]==3)]<-rowSums(apply(temp[-1:-3,temp[1,]=="rHH_TOTINCAT" & temp[2,]%in%c(12:15)],2,as.numeric))
region_household_marginals[-1:-3,(region_household_marginals[1,]=="rHH_TOTINCAT" & region_household_marginals[2,]==4)]<-as.numeric(temp[-1:-3,temp[1,]=="rHH_TOTINCAT" & temp[2,]%in%c(16)])
write_csv(region_household_marginals,"Exported/A1/Montreal/Scenarios/S11-hCSD/region_household_marginals.csv",col_names = FALSE)
#region_person_marginals
region_person_marginals<-merge(CSDMAT[grepl("PP_",CSDMAT$Variable),],pp_variable_importance,by="Variable",all.x=TRUE)
region_person_marginals<-region_person_marginals[order(region_person_marginals$Importance),]
region_person_marginals<-region_person_marginals[region_person_marginals$Importance!=0,!names(region_person_marginals)%in%c("X","Definition","Description","Frequency","Importance")]
region_person_marginals$Categories[region_person_marginals$Variable=="PP_ID"]<-1
region_person_marginals<-rbind(c("variable_names","variable_categories",1:length(unique(GIS$CSD))),region_person_marginals)
region_person_marginals<-cbind(region_person_marginals[,1:2],c("region",rep("",nrow(region_person_marginals)-1)),region_person_marginals[,3:ncol(region_person_marginals)])
region_person_marginals$Variable[-1]<-paste("r",region_person_marginals$Variable[-1],sep="")
region_person_marginals<-data.frame(t(region_person_marginals))
temp<-region_person_marginals
region_person_marginals<-temp[,!((temp[1,]%in%c("rPP_AGEGRP") & temp[2,]%in%c(4:13)))]
region_person_marginals[-1:-3,(region_person_marginals[1,]=="rPP_AGEGRP" & region_person_marginals[2,]==1)]<-rowSums(apply(temp[-1:-3,temp[1,]=="rPP_AGEGRP" & temp[2,]%in%c(1:2)],2,as.numeric))
region_person_marginals[-1:-3,(region_person_marginals[1,]=="rPP_AGEGRP" & region_person_marginals[2,]==2)]<-rowSums(apply(temp[-1:-3,temp[1,]=="rPP_AGEGRP" & temp[2,]%in%c(3:11)],2,as.numeric))
region_person_marginals[-1:-3,(region_person_marginals[1,]=="rPP_AGEGRP" & region_person_marginals[2,]==3)]<-rowSums(apply(temp[-1:-3,temp[1,]=="rPP_AGEGRP" & temp[2,]%in%c(12:13)],2,as.numeric))
write_csv(region_person_marginals,"Exported/A1/Montreal/Scenarios/S11-hCSD/region_person_marginals.csv",col_names = FALSE)
#geo_household_marginals
geo_household_marginals<-merge(CSDMAT[grepl("HH_",CSDMAT$Variable),],hh_variable_importance,by="Variable",all.x=TRUE)
geo_household_marginals<-geo_household_marginals[order(geo_household_marginals$Importance),]
geo_household_marginals<-geo_household_marginals[geo_household_marginals$Importance!=0,!names(geo_household_marginals)%in%c("X","Definition","Description","Frequency","Importance")]
geo_household_marginals$Categories[geo_household_marginals$Variable=="HH_ID"]<-1
geo_household_marginals<-rbind(c("variable_names","variable_categories",1:length(unique(GIS$CSD))),geo_household_marginals)
geo_household_marginals<-cbind(geo_household_marginals[,1:2],c("geo",rep("",nrow(geo_household_marginals)-1)),geo_household_marginals[,3:ncol(geo_household_marginals)])
geo_household_marginals$Variable[-1]<-geo_household_marginals$Variable[-1]
geo_household_marginals<-data.frame(t(geo_household_marginals))
temp<-geo_household_marginals
geo_household_marginals<-temp[,!((temp[1,]%in%c("HH_TOTINCAT") & temp[2,]%in%c(5:16)))]
geo_household_marginals[-1:-3,(geo_household_marginals[1,]=="HH_TOTINCAT" & geo_household_marginals[2,]==1)]<-rowSums(apply(temp[-1:-3,temp[1,]=="HH_TOTINCAT" & temp[2,]%in%c(1:6)],2,as.numeric))
geo_household_marginals[-1:-3,(geo_household_marginals[1,]=="HH_TOTINCAT" & geo_household_marginals[2,]==2)]<-rowSums(apply(temp[-1:-3,temp[1,]=="HH_TOTINCAT" & temp[2,]%in%c(7:11)],2,as.numeric))
geo_household_marginals[-1:-3,(geo_household_marginals[1,]=="HH_TOTINCAT" & geo_household_marginals[2,]==3)]<-rowSums(apply(temp[-1:-3,temp[1,]=="HH_TOTINCAT" & temp[2,]%in%c(12:15)],2,as.numeric))
geo_household_marginals[-1:-3,(geo_household_marginals[1,]=="HH_TOTINCAT" & geo_household_marginals[2,]==4)]<-as.numeric(temp[-1:-3,temp[1,]=="HH_TOTINCAT" & temp[2,]%in%c(16)])
write_csv(geo_household_marginals,"Exported/A1/Montreal/Scenarios/S11-hCSD/geo_household_marginals.csv",col_names = FALSE)
#geo_person_marginals
geo_person_marginals<-merge(CSDMAT[grepl("PP_",CSDMAT$Variable),],pp_variable_importance,by="Variable",all.x=TRUE)
geo_person_marginals<-geo_person_marginals[order(geo_person_marginals$Importance),]
geo_person_marginals<-geo_person_marginals[geo_person_marginals$Importance!=0,!names(geo_person_marginals)%in%c("X","Definition","Description","Frequency","Importance")]
geo_person_marginals$Categories[geo_person_marginals$Variable=="PP_ID"]<-1
geo_person_marginals<-rbind(c("variable_names","variable_categories",1:length(unique(GIS$CSD))),geo_person_marginals)
geo_person_marginals<-cbind(geo_person_marginals[,1:2],c("geo",rep("",nrow(geo_person_marginals)-1)),geo_person_marginals[,3:ncol(geo_person_marginals)])
geo_person_marginals$Variable[-1]<-geo_person_marginals$Variable[-1]
geo_person_marginals<-data.frame(t(geo_person_marginals))
temp<-geo_person_marginals
geo_person_marginals<-temp[,!((temp[1,]%in%c("PP_AGEGRP") & temp[2,]%in%c(4:13)))]
geo_person_marginals[-1:-3,(geo_person_marginals[1,]=="PP_AGEGRP" & geo_person_marginals[2,]==1)]<-rowSums(apply(temp[-1:-3,temp[1,]=="PP_AGEGRP" & temp[2,]%in%c(1:2)],2,as.numeric))
geo_person_marginals[-1:-3,(geo_person_marginals[1,]=="PP_AGEGRP" & geo_person_marginals[2,]==2)]<-rowSums(apply(temp[-1:-3,temp[1,]=="PP_AGEGRP" & temp[2,]%in%c(3:11)],2,as.numeric))
geo_person_marginals[-1:-3,(geo_person_marginals[1,]=="PP_AGEGRP" & geo_person_marginals[2,]==3)]<-rowSums(apply(temp[-1:-3,temp[1,]=="PP_AGEGRP" & temp[2,]%in%c(12:13)],2,as.numeric))
write_csv(geo_person_marginals,"Exported/A1/Montreal/Scenarios/S11-hCSD/geo_person_marginals.csv",col_names = FALSE)
#household_sample
write_csv(household_sample,"Exported/A1/Montreal/Scenarios/S11-hCSD/household_sample.csv",col_names = TRUE)
#person_sample
write_csv(person_sample,"Exported/A1/Montreal/Scenarios/S11-hCSD/person_sample.csv",col_names = TRUE)

###S12-hCMA_CSD
GIS<-GIS_CMA_CSD
#region_geo_mapping
geo<-1:length(unique(GIS$CSD))
region<-1
region_geo_mapping<-data.frame(region,geo)
write.csv(region_geo_mapping,"Exported/A1/Montreal/Scenarios/S12-hCMA_CSD/region_geo_mapping.csv",row.names=FALSE)
#geo_sample_mapping
geo<-1:length(unique(GIS$CSD))
sample_geo<-1
geo_sample_mapping<-data.frame(geo,sample_geo)
write.csv(geo_sample_mapping,"Exported/A1/Montreal/Scenarios/S12-hCMA_CSD/geo_sample_mapping.csv",row.names=FALSE)
#region_sample_mapping
region<-1
sample_geo<--1
region_sample_mapping<-data.frame(region,sample_geo)
write.csv(region_sample_mapping,"Exported/A1/Montreal/Scenarios/S12-hCMA_CSD/region_sample_mapping.csv",row.names=FALSE)
#region_household_marginals
region_household_marginals<-merge(CMAMAT[grepl("HH_",CMAMAT$Variable),],hh_variable_importance,by="Variable",all.x=TRUE)
region_household_marginals<-region_household_marginals[order(region_household_marginals$Importance),]
region_household_marginals<-region_household_marginals[region_household_marginals$Importance!=0,!names(region_household_marginals)%in%c("X","Definition","Description","Frequency","Importance")]
region_household_marginals$Categories[region_household_marginals$Variable=="HH_ID"]<-1
region_household_marginals<-rbind(c("variable_names","variable_categories",1:length(unique(GIS$CMA))),region_household_marginals)
region_household_marginals<-cbind(region_household_marginals[,1:2],c("region",rep("",nrow(region_household_marginals)-1)),region_household_marginals[,3:ncol(region_household_marginals)])
region_household_marginals$Variable[-1]<-paste("r",region_household_marginals$Variable[-1],sep="")
region_household_marginals<-data.frame(t(region_household_marginals))
temp<-region_household_marginals
region_household_marginals<-temp[,!((temp[1,]%in%c("rHH_TOTINCAT") & temp[2,]%in%c(5:16)))]
region_household_marginals[-1:-3,(region_household_marginals[1,]=="rHH_TOTINCAT" & region_household_marginals[2,]==1)]<-sum(apply(temp[-1:-3,temp[1,]=="rHH_TOTINCAT" & temp[2,]%in%c(1:6)],2,as.numeric))
region_household_marginals[-1:-3,(region_household_marginals[1,]=="rHH_TOTINCAT" & region_household_marginals[2,]==2)]<-sum(apply(temp[-1:-3,temp[1,]=="rHH_TOTINCAT" & temp[2,]%in%c(7:11)],2,as.numeric))
region_household_marginals[-1:-3,(region_household_marginals[1,]=="rHH_TOTINCAT" & region_household_marginals[2,]==3)]<-sum(apply(temp[-1:-3,temp[1,]=="rHH_TOTINCAT" & temp[2,]%in%c(12:15)],2,as.numeric))
region_household_marginals[-1:-3,(region_household_marginals[1,]=="rHH_TOTINCAT" & region_household_marginals[2,]==4)]<-as.numeric(temp[-1:-3,temp[1,]=="rHH_TOTINCAT" & temp[2,]%in%c(16)])
write_csv(region_household_marginals,"Exported/A1/Montreal/Scenarios/S12-hCMA_CSD/region_household_marginals.csv",col_names = FALSE)
#region_person_marginals
region_person_marginals<-merge(CMAMAT[grepl("PP_",CMAMAT$Variable),],pp_variable_importance,by="Variable",all.x=TRUE)
region_person_marginals<-region_person_marginals[order(region_person_marginals$Importance),]
region_person_marginals<-region_person_marginals[region_person_marginals$Importance!=0,!names(region_person_marginals)%in%c("X","Definition","Description","Frequency","Importance")]
region_person_marginals$Categories[region_person_marginals$Variable=="PP_ID"]<-1
region_person_marginals<-rbind(c("variable_names","variable_categories",1:length(unique(GIS$CMA))),region_person_marginals)
region_person_marginals<-cbind(region_person_marginals[,1:2],c("region",rep("",nrow(region_person_marginals)-1)),region_person_marginals[,3:ncol(region_person_marginals)])
region_person_marginals$Variable[-1]<-paste("r",region_person_marginals$Variable[-1],sep="")
region_person_marginals<-data.frame(t(region_person_marginals))
temp<-region_person_marginals
region_person_marginals<-temp[,!((temp[1,]%in%c("rPP_AGEGRP") & temp[2,]%in%c(4:13)))]
region_person_marginals[-1:-3,(region_person_marginals[1,]=="rPP_AGEGRP" & region_person_marginals[2,]==1)]<-sum(apply(temp[-1:-3,temp[1,]=="rPP_AGEGRP" & temp[2,]%in%c(1:2)],2,as.numeric))
region_person_marginals[-1:-3,(region_person_marginals[1,]=="rPP_AGEGRP" & region_person_marginals[2,]==2)]<-sum(apply(temp[-1:-3,temp[1,]=="rPP_AGEGRP" & temp[2,]%in%c(3:11)],2,as.numeric))
region_person_marginals[-1:-3,(region_person_marginals[1,]=="rPP_AGEGRP" & region_person_marginals[2,]==3)]<-sum(apply(temp[-1:-3,temp[1,]=="rPP_AGEGRP" & temp[2,]%in%c(12:13)],2,as.numeric))
write_csv(region_person_marginals,"Exported/A1/Montreal/Scenarios/S12-hCMA_CSD/region_person_marginals.csv",col_names = FALSE)
#geo_household_marginals
geo_household_marginals<-merge(CSDMAT[grepl("HH_",CSDMAT$Variable),],hh_variable_importance,by="Variable",all.x=TRUE)
geo_household_marginals<-geo_household_marginals[order(geo_household_marginals$Importance),]
geo_household_marginals<-geo_household_marginals[geo_household_marginals$Importance!=0,!names(geo_household_marginals)%in%c("X","Definition","Description","Frequency","Importance")]
geo_household_marginals$Categories[geo_household_marginals$Variable=="HH_ID"]<-1
geo_household_marginals<-rbind(c("variable_names","variable_categories",1:length(unique(GIS$CSD))),geo_household_marginals)
geo_household_marginals<-cbind(geo_household_marginals[,1:2],c("geo",rep("",nrow(geo_household_marginals)-1)),geo_household_marginals[,3:ncol(geo_household_marginals)])
geo_household_marginals$Variable[-1]<-geo_household_marginals$Variable[-1]
geo_household_marginals<-data.frame(t(geo_household_marginals))
temp<-geo_household_marginals
geo_household_marginals<-temp[,!((temp[1,]%in%c("HH_TOTINCAT") & temp[2,]%in%c(5:16)))]
geo_household_marginals[-1:-3,(geo_household_marginals[1,]=="HH_TOTINCAT" & geo_household_marginals[2,]==1)]<-rowSums(apply(temp[-1:-3,temp[1,]=="HH_TOTINCAT" & temp[2,]%in%c(1:6)],2,as.numeric))
geo_household_marginals[-1:-3,(geo_household_marginals[1,]=="HH_TOTINCAT" & geo_household_marginals[2,]==2)]<-rowSums(apply(temp[-1:-3,temp[1,]=="HH_TOTINCAT" & temp[2,]%in%c(7:11)],2,as.numeric))
geo_household_marginals[-1:-3,(geo_household_marginals[1,]=="HH_TOTINCAT" & geo_household_marginals[2,]==3)]<-rowSums(apply(temp[-1:-3,temp[1,]=="HH_TOTINCAT" & temp[2,]%in%c(12:15)],2,as.numeric))
geo_household_marginals[-1:-3,(geo_household_marginals[1,]=="HH_TOTINCAT" & geo_household_marginals[2,]==4)]<-as.numeric(temp[-1:-3,temp[1,]=="HH_TOTINCAT" & temp[2,]%in%c(16)])
write_csv(geo_household_marginals,"Exported/A1/Montreal/Scenarios/S12-hCMA_CSD/geo_household_marginals.csv",col_names = FALSE)
#geo_person_marginals
geo_person_marginals<-merge(CSDMAT[grepl("PP_",CSDMAT$Variable),],pp_variable_importance,by="Variable",all.x=TRUE)
geo_person_marginals<-geo_person_marginals[order(geo_person_marginals$Importance),]
geo_person_marginals<-geo_person_marginals[geo_person_marginals$Importance!=0,!names(geo_person_marginals)%in%c("X","Definition","Description","Frequency","Importance")]
geo_person_marginals$Categories[geo_person_marginals$Variable=="PP_ID"]<-1
geo_person_marginals<-rbind(c("variable_names","variable_categories",1:length(unique(GIS$CSD))),geo_person_marginals)
geo_person_marginals<-cbind(geo_person_marginals[,1:2],c("geo",rep("",nrow(geo_person_marginals)-1)),geo_person_marginals[,3:ncol(geo_person_marginals)])
geo_person_marginals$Variable[-1]<-geo_person_marginals$Variable[-1]
geo_person_marginals<-data.frame(t(geo_person_marginals))
temp<-geo_person_marginals
geo_person_marginals<-temp[,!((temp[1,]%in%c("PP_AGEGRP") & temp[2,]%in%c(4:13)))]
geo_person_marginals[-1:-3,(geo_person_marginals[1,]=="PP_AGEGRP" & geo_person_marginals[2,]==1)]<-rowSums(apply(temp[-1:-3,temp[1,]=="PP_AGEGRP" & temp[2,]%in%c(1:2)],2,as.numeric))
geo_person_marginals[-1:-3,(geo_person_marginals[1,]=="PP_AGEGRP" & geo_person_marginals[2,]==2)]<-rowSums(apply(temp[-1:-3,temp[1,]=="PP_AGEGRP" & temp[2,]%in%c(3:11)],2,as.numeric))
geo_person_marginals[-1:-3,(geo_person_marginals[1,]=="PP_AGEGRP" & geo_person_marginals[2,]==3)]<-rowSums(apply(temp[-1:-3,temp[1,]=="PP_AGEGRP" & temp[2,]%in%c(12:13)],2,as.numeric))
write_csv(geo_person_marginals,"Exported/A1/Montreal/Scenarios/S12-hCMA_CSD/geo_person_marginals.csv",col_names = FALSE)
#household_sample
write_csv(household_sample,"Exported/A1/Montreal/Scenarios/S12-hCMA_CSD/household_sample.csv",col_names = TRUE)
#person_sample
write_csv(person_sample,"Exported/A1/Montreal/Scenarios/S12-hCMA_CSD/person_sample.csv",col_names = TRUE)

###S13-hADA
GIS<-GIS_CMA_ADA
#region_geo_mapping
geo<-1:length(unique(GIS$ADA))
region<-1:length(unique(GIS$ADA))
region_geo_mapping<-data.frame(region,geo)
write.csv(region_geo_mapping,"Exported/A1/Montreal/Scenarios/S13-hADA/region_geo_mapping.csv",row.names=FALSE)
#geo_sample_mapping
geo<-1:length(unique(GIS$ADA))
sample_geo<-1
geo_sample_mapping<-data.frame(geo,sample_geo)
write.csv(geo_sample_mapping,"Exported/A1/Montreal/Scenarios/S13-hADA/geo_sample_mapping.csv",row.names=FALSE)
#region_sample_mapping
region<-1:length(unique(GIS$ADA))
sample_geo<--1
region_sample_mapping<-data.frame(region,sample_geo)
write.csv(region_sample_mapping,"Exported/A1/Montreal/Scenarios/S13-hADA/region_sample_mapping.csv",row.names=FALSE)
#region_household_marginals
region_household_marginals<-merge(ADAMAT[grepl("HH_",ADAMAT$Variable),],hh_variable_importance,by="Variable",all.x=TRUE)
region_household_marginals<-region_household_marginals[order(region_household_marginals$Importance),]
region_household_marginals<-region_household_marginals[region_household_marginals$Importance!=0,!names(region_household_marginals)%in%c("X","Definition","Description","Frequency","Importance")]
region_household_marginals$Categories[region_household_marginals$Variable=="HH_ID"]<-1
region_household_marginals<-rbind(c("variable_names","variable_categories",1:length(unique(GIS$ADA))),region_household_marginals)
region_household_marginals<-cbind(region_household_marginals[,1:2],c("region",rep("",nrow(region_household_marginals)-1)),region_household_marginals[,3:ncol(region_household_marginals)])
region_household_marginals$Variable[-1]<-paste("r",region_household_marginals$Variable[-1],sep="")
region_household_marginals<-data.frame(t(region_household_marginals))
temp<-region_household_marginals
region_household_marginals<-temp[,!((temp[1,]%in%c("rHH_TOTINCAT") & temp[2,]%in%c(5:16)))]
region_household_marginals[-1:-3,(region_household_marginals[1,]=="rHH_TOTINCAT" & region_household_marginals[2,]==1)]<-rowSums(apply(temp[-1:-3,temp[1,]=="rHH_TOTINCAT" & temp[2,]%in%c(1:6)],2,as.numeric))
region_household_marginals[-1:-3,(region_household_marginals[1,]=="rHH_TOTINCAT" & region_household_marginals[2,]==2)]<-rowSums(apply(temp[-1:-3,temp[1,]=="rHH_TOTINCAT" & temp[2,]%in%c(7:11)],2,as.numeric))
region_household_marginals[-1:-3,(region_household_marginals[1,]=="rHH_TOTINCAT" & region_household_marginals[2,]==3)]<-rowSums(apply(temp[-1:-3,temp[1,]=="rHH_TOTINCAT" & temp[2,]%in%c(12:15)],2,as.numeric))
region_household_marginals[-1:-3,(region_household_marginals[1,]=="rHH_TOTINCAT" & region_household_marginals[2,]==4)]<-as.numeric(temp[-1:-3,temp[1,]=="rHH_TOTINCAT" & temp[2,]%in%c(16)])
write_csv(region_household_marginals,"Exported/A1/Montreal/Scenarios/S13-hADA/region_household_marginals.csv",col_names = FALSE)
#region_person_marginals
region_person_marginals<-merge(ADAMAT[grepl("PP_",ADAMAT$Variable),],pp_variable_importance,by="Variable",all.x=TRUE)
region_person_marginals<-region_person_marginals[order(region_person_marginals$Importance),]
region_person_marginals<-region_person_marginals[region_person_marginals$Importance!=0,!names(region_person_marginals)%in%c("X","Definition","Description","Frequency","Importance")]
region_person_marginals$Categories[region_person_marginals$Variable=="PP_ID"]<-1
region_person_marginals<-rbind(c("variable_names","variable_categories",1:length(unique(GIS$ADA))),region_person_marginals)
region_person_marginals<-cbind(region_person_marginals[,1:2],c("region",rep("",nrow(region_person_marginals)-1)),region_person_marginals[,3:ncol(region_person_marginals)])
region_person_marginals$Variable[-1]<-paste("r",region_person_marginals$Variable[-1],sep="")
region_person_marginals<-data.frame(t(region_person_marginals))
temp<-region_person_marginals
region_person_marginals<-temp[,!((temp[1,]%in%c("rPP_AGEGRP") & temp[2,]%in%c(4:13)))]
region_person_marginals[-1:-3,(region_person_marginals[1,]=="rPP_AGEGRP" & region_person_marginals[2,]==1)]<-rowSums(apply(temp[-1:-3,temp[1,]=="rPP_AGEGRP" & temp[2,]%in%c(1:2)],2,as.numeric))
region_person_marginals[-1:-3,(region_person_marginals[1,]=="rPP_AGEGRP" & region_person_marginals[2,]==2)]<-rowSums(apply(temp[-1:-3,temp[1,]=="rPP_AGEGRP" & temp[2,]%in%c(3:11)],2,as.numeric))
region_person_marginals[-1:-3,(region_person_marginals[1,]=="rPP_AGEGRP" & region_person_marginals[2,]==3)]<-rowSums(apply(temp[-1:-3,temp[1,]=="rPP_AGEGRP" & temp[2,]%in%c(12:13)],2,as.numeric))
write_csv(region_person_marginals,"Exported/A1/Montreal/Scenarios/S13-hADA/region_person_marginals.csv",col_names = FALSE)
#geo_household_marginals
geo_household_marginals<-merge(ADAMAT[grepl("HH_",ADAMAT$Variable),],hh_variable_importance,by="Variable",all.x=TRUE)
geo_household_marginals<-geo_household_marginals[order(geo_household_marginals$Importance),]
geo_household_marginals<-geo_household_marginals[geo_household_marginals$Importance!=0,!names(geo_household_marginals)%in%c("X","Definition","Description","Frequency","Importance")]
geo_household_marginals$Categories[geo_household_marginals$Variable=="HH_ID"]<-1
geo_household_marginals<-rbind(c("variable_names","variable_categories",1:length(unique(GIS$ADA))),geo_household_marginals)
geo_household_marginals<-cbind(geo_household_marginals[,1:2],c("geo",rep("",nrow(geo_household_marginals)-1)),geo_household_marginals[,3:ncol(geo_household_marginals)])
geo_household_marginals$Variable[-1]<-geo_household_marginals$Variable[-1]
geo_household_marginals<-data.frame(t(geo_household_marginals))
temp<-geo_household_marginals
geo_household_marginals<-temp[,!((temp[1,]%in%c("HH_TOTINCAT") & temp[2,]%in%c(5:16)))]
geo_household_marginals[-1:-3,(geo_household_marginals[1,]=="HH_TOTINCAT" & geo_household_marginals[2,]==1)]<-rowSums(apply(temp[-1:-3,temp[1,]=="HH_TOTINCAT" & temp[2,]%in%c(1:6)],2,as.numeric))
geo_household_marginals[-1:-3,(geo_household_marginals[1,]=="HH_TOTINCAT" & geo_household_marginals[2,]==2)]<-rowSums(apply(temp[-1:-3,temp[1,]=="HH_TOTINCAT" & temp[2,]%in%c(7:11)],2,as.numeric))
geo_household_marginals[-1:-3,(geo_household_marginals[1,]=="HH_TOTINCAT" & geo_household_marginals[2,]==3)]<-rowSums(apply(temp[-1:-3,temp[1,]=="HH_TOTINCAT" & temp[2,]%in%c(12:15)],2,as.numeric))
geo_household_marginals[-1:-3,(geo_household_marginals[1,]=="HH_TOTINCAT" & geo_household_marginals[2,]==4)]<-as.numeric(temp[-1:-3,temp[1,]=="HH_TOTINCAT" & temp[2,]%in%c(16)])
write_csv(geo_household_marginals,"Exported/A1/Montreal/Scenarios/S13-hADA/geo_household_marginals.csv",col_names = FALSE)
#geo_person_marginals
geo_person_marginals<-merge(ADAMAT[grepl("PP_",ADAMAT$Variable),],pp_variable_importance,by="Variable",all.x=TRUE)
geo_person_marginals<-geo_person_marginals[order(geo_person_marginals$Importance),]
geo_person_marginals<-geo_person_marginals[geo_person_marginals$Importance!=0,!names(geo_person_marginals)%in%c("X","Definition","Description","Frequency","Importance")]
geo_person_marginals$Categories[geo_person_marginals$Variable=="PP_ID"]<-1
geo_person_marginals<-rbind(c("variable_names","variable_categories",1:length(unique(GIS$ADA))),geo_person_marginals)
geo_person_marginals<-cbind(geo_person_marginals[,1:2],c("geo",rep("",nrow(geo_person_marginals)-1)),geo_person_marginals[,3:ncol(geo_person_marginals)])
geo_person_marginals$Variable[-1]<-geo_person_marginals$Variable[-1]
geo_person_marginals<-data.frame(t(geo_person_marginals))
temp<-geo_person_marginals
geo_person_marginals<-temp[,!((temp[1,]%in%c("PP_AGEGRP") & temp[2,]%in%c(4:13)))]
geo_person_marginals[-1:-3,(geo_person_marginals[1,]=="PP_AGEGRP" & geo_person_marginals[2,]==1)]<-rowSums(apply(temp[-1:-3,temp[1,]=="PP_AGEGRP" & temp[2,]%in%c(1:2)],2,as.numeric))
geo_person_marginals[-1:-3,(geo_person_marginals[1,]=="PP_AGEGRP" & geo_person_marginals[2,]==2)]<-rowSums(apply(temp[-1:-3,temp[1,]=="PP_AGEGRP" & temp[2,]%in%c(3:11)],2,as.numeric))
geo_person_marginals[-1:-3,(geo_person_marginals[1,]=="PP_AGEGRP" & geo_person_marginals[2,]==3)]<-rowSums(apply(temp[-1:-3,temp[1,]=="PP_AGEGRP" & temp[2,]%in%c(12:13)],2,as.numeric))
write_csv(geo_person_marginals,"Exported/A1/Montreal/Scenarios/S13-hADA/geo_person_marginals.csv",col_names = FALSE)
#household_sample
write_csv(household_sample,"Exported/A1/Montreal/Scenarios/S13-hADA/household_sample.csv",col_names = TRUE)
#person_sample
write_csv(person_sample,"Exported/A1/Montreal/Scenarios/S13-hADA/person_sample.csv",col_names = TRUE)

###S14-hCMA_ADA
GIS<-GIS_CMA_ADA
#region_geo_mapping
geo<-1:length(unique(GIS$ADA))
region<-1
region_geo_mapping<-data.frame(region,geo)
write.csv(region_geo_mapping,"Exported/A1/Montreal/Scenarios/S14-hCMA_ADA/region_geo_mapping.csv",row.names=FALSE)
#geo_sample_mapping
geo<-1:length(unique(GIS$ADA))
sample_geo<-1
geo_sample_mapping<-data.frame(geo,sample_geo)
write.csv(geo_sample_mapping,"Exported/A1/Montreal/Scenarios/S14-hCMA_ADA/geo_sample_mapping.csv",row.names=FALSE)
#region_sample_mapping
region<-1
sample_geo<--1
region_sample_mapping<-data.frame(region,sample_geo)
write.csv(region_sample_mapping,"Exported/A1/Montreal/Scenarios/S14-hCMA_ADA/region_sample_mapping.csv",row.names=FALSE)
#region_household_marginals
region_household_marginals<-merge(CMAMAT[grepl("HH_",CMAMAT$Variable),],hh_variable_importance,by="Variable",all.x=TRUE)
region_household_marginals<-region_household_marginals[order(region_household_marginals$Importance),]
region_household_marginals<-region_household_marginals[region_household_marginals$Importance!=0,!names(region_household_marginals)%in%c("X","Definition","Description","Frequency","Importance")]
region_household_marginals$Categories[region_household_marginals$Variable=="HH_ID"]<-1
region_household_marginals<-rbind(c("variable_names","variable_categories",1:length(unique(GIS$CMA))),region_household_marginals)
region_household_marginals<-cbind(region_household_marginals[,1:2],c("region",rep("",nrow(region_household_marginals)-1)),region_household_marginals[,3:ncol(region_household_marginals)])
region_household_marginals$Variable[-1]<-paste("r",region_household_marginals$Variable[-1],sep="")
region_household_marginals<-data.frame(t(region_household_marginals))
temp<-region_household_marginals
region_household_marginals<-temp[,!((temp[1,]%in%c("rHH_TOTINCAT") & temp[2,]%in%c(5:16)))]
region_household_marginals[-1:-3,(region_household_marginals[1,]=="rHH_TOTINCAT" & region_household_marginals[2,]==1)]<-sum(apply(temp[-1:-3,temp[1,]=="rHH_TOTINCAT" & temp[2,]%in%c(1:6)],2,as.numeric))
region_household_marginals[-1:-3,(region_household_marginals[1,]=="rHH_TOTINCAT" & region_household_marginals[2,]==2)]<-sum(apply(temp[-1:-3,temp[1,]=="rHH_TOTINCAT" & temp[2,]%in%c(7:11)],2,as.numeric))
region_household_marginals[-1:-3,(region_household_marginals[1,]=="rHH_TOTINCAT" & region_household_marginals[2,]==3)]<-sum(apply(temp[-1:-3,temp[1,]=="rHH_TOTINCAT" & temp[2,]%in%c(12:15)],2,as.numeric))
region_household_marginals[-1:-3,(region_household_marginals[1,]=="rHH_TOTINCAT" & region_household_marginals[2,]==4)]<-as.numeric(temp[-1:-3,temp[1,]=="rHH_TOTINCAT" & temp[2,]%in%c(16)])
write_csv(region_household_marginals,"Exported/A1/Montreal/Scenarios/S14-hCMA_ADA/region_household_marginals.csv",col_names = FALSE)
#region_person_marginals
region_person_marginals<-merge(CMAMAT[grepl("PP_",CMAMAT$Variable),],pp_variable_importance,by="Variable",all.x=TRUE)
region_person_marginals<-region_person_marginals[order(region_person_marginals$Importance),]
region_person_marginals<-region_person_marginals[region_person_marginals$Importance!=0,!names(region_person_marginals)%in%c("X","Definition","Description","Frequency","Importance")]
region_person_marginals$Categories[region_person_marginals$Variable=="PP_ID"]<-1
region_person_marginals<-rbind(c("variable_names","variable_categories",1:length(unique(GIS$CMA))),region_person_marginals)
region_person_marginals<-cbind(region_person_marginals[,1:2],c("region",rep("",nrow(region_person_marginals)-1)),region_person_marginals[,3:ncol(region_person_marginals)])
region_person_marginals$Variable[-1]<-paste("r",region_person_marginals$Variable[-1],sep="")
region_person_marginals<-data.frame(t(region_person_marginals))
temp<-region_person_marginals
region_person_marginals<-temp[,!((temp[1,]%in%c("rPP_AGEGRP") & temp[2,]%in%c(4:13)))]
region_person_marginals[-1:-3,(region_person_marginals[1,]=="rPP_AGEGRP" & region_person_marginals[2,]==1)]<-sum(apply(temp[-1:-3,temp[1,]=="rPP_AGEGRP" & temp[2,]%in%c(1:2)],2,as.numeric))
region_person_marginals[-1:-3,(region_person_marginals[1,]=="rPP_AGEGRP" & region_person_marginals[2,]==2)]<-sum(apply(temp[-1:-3,temp[1,]=="rPP_AGEGRP" & temp[2,]%in%c(3:11)],2,as.numeric))
region_person_marginals[-1:-3,(region_person_marginals[1,]=="rPP_AGEGRP" & region_person_marginals[2,]==3)]<-sum(apply(temp[-1:-3,temp[1,]=="rPP_AGEGRP" & temp[2,]%in%c(12:13)],2,as.numeric))
write_csv(region_person_marginals,"Exported/A1/Montreal/Scenarios/S14-hCMA_ADA/region_person_marginals.csv",col_names = FALSE)
#geo_household_marginals
geo_household_marginals<-merge(ADAMAT[grepl("HH_",ADAMAT$Variable),],hh_variable_importance,by="Variable",all.x=TRUE)
geo_household_marginals<-geo_household_marginals[order(geo_household_marginals$Importance),]
geo_household_marginals<-geo_household_marginals[geo_household_marginals$Importance!=0,!names(geo_household_marginals)%in%c("X","Definition","Description","Frequency","Importance")]
geo_household_marginals$Categories[geo_household_marginals$Variable=="HH_ID"]<-1
geo_household_marginals<-rbind(c("variable_names","variable_categories",1:length(unique(GIS$ADA))),geo_household_marginals)
geo_household_marginals<-cbind(geo_household_marginals[,1:2],c("geo",rep("",nrow(geo_household_marginals)-1)),geo_household_marginals[,3:ncol(geo_household_marginals)])
geo_household_marginals$Variable[-1]<-geo_household_marginals$Variable[-1]
geo_household_marginals<-data.frame(t(geo_household_marginals))
temp<-geo_household_marginals
geo_household_marginals<-temp[,!((temp[1,]%in%c("HH_TOTINCAT") & temp[2,]%in%c(5:16)))]
geo_household_marginals[-1:-3,(geo_household_marginals[1,]=="HH_TOTINCAT" & geo_household_marginals[2,]==1)]<-rowSums(apply(temp[-1:-3,temp[1,]=="HH_TOTINCAT" & temp[2,]%in%c(1:6)],2,as.numeric))
geo_household_marginals[-1:-3,(geo_household_marginals[1,]=="HH_TOTINCAT" & geo_household_marginals[2,]==2)]<-rowSums(apply(temp[-1:-3,temp[1,]=="HH_TOTINCAT" & temp[2,]%in%c(7:11)],2,as.numeric))
geo_household_marginals[-1:-3,(geo_household_marginals[1,]=="HH_TOTINCAT" & geo_household_marginals[2,]==3)]<-rowSums(apply(temp[-1:-3,temp[1,]=="HH_TOTINCAT" & temp[2,]%in%c(12:15)],2,as.numeric))
geo_household_marginals[-1:-3,(geo_household_marginals[1,]=="HH_TOTINCAT" & geo_household_marginals[2,]==4)]<-as.numeric(temp[-1:-3,temp[1,]=="HH_TOTINCAT" & temp[2,]%in%c(16)])
write_csv(geo_household_marginals,"Exported/A1/Montreal/Scenarios/S14-hCMA_ADA/geo_household_marginals.csv",col_names = FALSE)
#geo_person_marginals
geo_person_marginals<-merge(ADAMAT[grepl("PP_",ADAMAT$Variable),],pp_variable_importance,by="Variable",all.x=TRUE)
geo_person_marginals<-geo_person_marginals[order(geo_person_marginals$Importance),]
geo_person_marginals<-geo_person_marginals[geo_person_marginals$Importance!=0,!names(geo_person_marginals)%in%c("X","Definition","Description","Frequency","Importance")]
geo_person_marginals$Categories[geo_person_marginals$Variable=="PP_ID"]<-1
geo_person_marginals<-rbind(c("variable_names","variable_categories",1:length(unique(GIS$ADA))),geo_person_marginals)
geo_person_marginals<-cbind(geo_person_marginals[,1:2],c("geo",rep("",nrow(geo_person_marginals)-1)),geo_person_marginals[,3:ncol(geo_person_marginals)])
geo_person_marginals$Variable[-1]<-geo_person_marginals$Variable[-1]
geo_person_marginals<-data.frame(t(geo_person_marginals))
temp<-geo_person_marginals
geo_person_marginals<-temp[,!((temp[1,]%in%c("PP_AGEGRP") & temp[2,]%in%c(4:13)))]
geo_person_marginals[-1:-3,(geo_person_marginals[1,]=="PP_AGEGRP" & geo_person_marginals[2,]==1)]<-rowSums(apply(temp[-1:-3,temp[1,]=="PP_AGEGRP" & temp[2,]%in%c(1:2)],2,as.numeric))
geo_person_marginals[-1:-3,(geo_person_marginals[1,]=="PP_AGEGRP" & geo_person_marginals[2,]==2)]<-rowSums(apply(temp[-1:-3,temp[1,]=="PP_AGEGRP" & temp[2,]%in%c(3:11)],2,as.numeric))
geo_person_marginals[-1:-3,(geo_person_marginals[1,]=="PP_AGEGRP" & geo_person_marginals[2,]==3)]<-rowSums(apply(temp[-1:-3,temp[1,]=="PP_AGEGRP" & temp[2,]%in%c(12:13)],2,as.numeric))
write_csv(geo_person_marginals,"Exported/A1/Montreal/Scenarios/S14-hCMA_ADA/geo_person_marginals.csv",col_names = FALSE)
#household_sample
write_csv(household_sample,"Exported/A1/Montreal/Scenarios/S14-hCMA_ADA/household_sample.csv",col_names = TRUE)
#person_sample
write_csv(person_sample,"Exported/A1/Montreal/Scenarios/S14-hCMA_ADA/person_sample.csv",col_names = TRUE)

###S15-hCT
GIS<-GIS_CMA_CT
#region_geo_mapping
geo<-1:length(unique(GIS$CT))
region<-1:length(unique(GIS$CT))
region_geo_mapping<-data.frame(region,geo)
write.csv(region_geo_mapping,"Exported/A1/Montreal/Scenarios/S15-hCT/region_geo_mapping.csv",row.names=FALSE)
#geo_sample_mapping
geo<-1:length(unique(GIS$CT))
sample_geo<-1
geo_sample_mapping<-data.frame(geo,sample_geo)
write.csv(geo_sample_mapping,"Exported/A1/Montreal/Scenarios/S15-hCT/geo_sample_mapping.csv",row.names=FALSE)
#region_sample_mapping
region<-1:length(unique(GIS$CT))
sample_geo<--1
region_sample_mapping<-data.frame(region,sample_geo)
write.csv(region_sample_mapping,"Exported/A1/Montreal/Scenarios/S15-hCT/region_sample_mapping.csv",row.names=FALSE)
#region_household_marginals
region_household_marginals<-merge(CTMAT[grepl("HH_",CTMAT$Variable),],hh_variable_importance,by="Variable",all.x=TRUE)
region_household_marginals<-region_household_marginals[order(region_household_marginals$Importance),]
region_household_marginals<-region_household_marginals[region_household_marginals$Importance!=0,!names(region_household_marginals)%in%c("X","Definition","Description","Frequency","Importance")]
region_household_marginals$Categories[region_household_marginals$Variable=="HH_ID"]<-1
region_household_marginals<-rbind(c("variable_names","variable_categories",1:length(unique(GIS$CT))),region_household_marginals)
region_household_marginals<-cbind(region_household_marginals[,1:2],c("region",rep("",nrow(region_household_marginals)-1)),region_household_marginals[,3:ncol(region_household_marginals)])
region_household_marginals$Variable[-1]<-paste("r",region_household_marginals$Variable[-1],sep="")
region_household_marginals<-data.frame(t(region_household_marginals))
temp<-region_household_marginals
region_household_marginals<-temp[,!((temp[1,]%in%c("rHH_TOTINCAT") & temp[2,]%in%c(5:16)))]
region_household_marginals[-1:-3,(region_household_marginals[1,]=="rHH_TOTINCAT" & region_household_marginals[2,]==1)]<-rowSums(apply(temp[-1:-3,temp[1,]=="rHH_TOTINCAT" & temp[2,]%in%c(1:6)],2,as.numeric))
region_household_marginals[-1:-3,(region_household_marginals[1,]=="rHH_TOTINCAT" & region_household_marginals[2,]==2)]<-rowSums(apply(temp[-1:-3,temp[1,]=="rHH_TOTINCAT" & temp[2,]%in%c(7:11)],2,as.numeric))
region_household_marginals[-1:-3,(region_household_marginals[1,]=="rHH_TOTINCAT" & region_household_marginals[2,]==3)]<-rowSums(apply(temp[-1:-3,temp[1,]=="rHH_TOTINCAT" & temp[2,]%in%c(12:15)],2,as.numeric))
region_household_marginals[-1:-3,(region_household_marginals[1,]=="rHH_TOTINCAT" & region_household_marginals[2,]==4)]<-as.numeric(temp[-1:-3,temp[1,]=="rHH_TOTINCAT" & temp[2,]%in%c(16)])
write_csv(region_household_marginals,"Exported/A1/Montreal/Scenarios/S15-hCT/region_household_marginals.csv",col_names = FALSE)
#region_person_marginals
region_person_marginals<-merge(CTMAT[grepl("PP_",CTMAT$Variable),],pp_variable_importance,by="Variable",all.x=TRUE)
region_person_marginals<-region_person_marginals[order(region_person_marginals$Importance),]
region_person_marginals<-region_person_marginals[region_person_marginals$Importance!=0,!names(region_person_marginals)%in%c("X","Definition","Description","Frequency","Importance")]
region_person_marginals$Categories[region_person_marginals$Variable=="PP_ID"]<-1
region_person_marginals<-rbind(c("variable_names","variable_categories",1:length(unique(GIS$CT))),region_person_marginals)
region_person_marginals<-cbind(region_person_marginals[,1:2],c("region",rep("",nrow(region_person_marginals)-1)),region_person_marginals[,3:ncol(region_person_marginals)])
region_person_marginals$Variable[-1]<-paste("r",region_person_marginals$Variable[-1],sep="")
region_person_marginals<-data.frame(t(region_person_marginals))
temp<-region_person_marginals
region_person_marginals<-temp[,!((temp[1,]%in%c("rPP_AGEGRP") & temp[2,]%in%c(4:13)))]
region_person_marginals[-1:-3,(region_person_marginals[1,]=="rPP_AGEGRP" & region_person_marginals[2,]==1)]<-rowSums(apply(temp[-1:-3,temp[1,]=="rPP_AGEGRP" & temp[2,]%in%c(1:2)],2,as.numeric))
region_person_marginals[-1:-3,(region_person_marginals[1,]=="rPP_AGEGRP" & region_person_marginals[2,]==2)]<-rowSums(apply(temp[-1:-3,temp[1,]=="rPP_AGEGRP" & temp[2,]%in%c(3:11)],2,as.numeric))
region_person_marginals[-1:-3,(region_person_marginals[1,]=="rPP_AGEGRP" & region_person_marginals[2,]==3)]<-rowSums(apply(temp[-1:-3,temp[1,]=="rPP_AGEGRP" & temp[2,]%in%c(12:13)],2,as.numeric))
write_csv(region_person_marginals,"Exported/A1/Montreal/Scenarios/S15-hCT/region_person_marginals.csv",col_names = FALSE)
#geo_household_marginals
geo_household_marginals<-merge(CTMAT[grepl("HH_",CTMAT$Variable),],hh_variable_importance,by="Variable",all.x=TRUE)
geo_household_marginals<-geo_household_marginals[order(geo_household_marginals$Importance),]
geo_household_marginals<-geo_household_marginals[geo_household_marginals$Importance!=0,!names(geo_household_marginals)%in%c("X","Definition","Description","Frequency","Importance")]
geo_household_marginals$Categories[geo_household_marginals$Variable=="HH_ID"]<-1
geo_household_marginals<-rbind(c("variable_names","variable_categories",1:length(unique(GIS$CT))),geo_household_marginals)
geo_household_marginals<-cbind(geo_household_marginals[,1:2],c("geo",rep("",nrow(geo_household_marginals)-1)),geo_household_marginals[,3:ncol(geo_household_marginals)])
geo_household_marginals$Variable[-1]<-geo_household_marginals$Variable[-1]
geo_household_marginals<-data.frame(t(geo_household_marginals))
temp<-geo_household_marginals
geo_household_marginals<-temp[,!((temp[1,]%in%c("HH_TOTINCAT") & temp[2,]%in%c(5:16)))]
geo_household_marginals[-1:-3,(geo_household_marginals[1,]=="HH_TOTINCAT" & geo_household_marginals[2,]==1)]<-rowSums(apply(temp[-1:-3,temp[1,]=="HH_TOTINCAT" & temp[2,]%in%c(1:6)],2,as.numeric))
geo_household_marginals[-1:-3,(geo_household_marginals[1,]=="HH_TOTINCAT" & geo_household_marginals[2,]==2)]<-rowSums(apply(temp[-1:-3,temp[1,]=="HH_TOTINCAT" & temp[2,]%in%c(7:11)],2,as.numeric))
geo_household_marginals[-1:-3,(geo_household_marginals[1,]=="HH_TOTINCAT" & geo_household_marginals[2,]==3)]<-rowSums(apply(temp[-1:-3,temp[1,]=="HH_TOTINCAT" & temp[2,]%in%c(12:15)],2,as.numeric))
geo_household_marginals[-1:-3,(geo_household_marginals[1,]=="HH_TOTINCAT" & geo_household_marginals[2,]==4)]<-as.numeric(temp[-1:-3,temp[1,]=="HH_TOTINCAT" & temp[2,]%in%c(16)])
write_csv(geo_household_marginals,"Exported/A1/Montreal/Scenarios/S15-hCT/geo_household_marginals.csv",col_names = FALSE)
#geo_person_marginals
geo_person_marginals<-merge(CTMAT[grepl("PP_",CTMAT$Variable),],pp_variable_importance,by="Variable",all.x=TRUE)
geo_person_marginals<-geo_person_marginals[order(geo_person_marginals$Importance),]
geo_person_marginals<-geo_person_marginals[geo_person_marginals$Importance!=0,!names(geo_person_marginals)%in%c("X","Definition","Description","Frequency","Importance")]
geo_person_marginals$Categories[geo_person_marginals$Variable=="PP_ID"]<-1
geo_person_marginals<-rbind(c("variable_names","variable_categories",1:length(unique(GIS$CT))),geo_person_marginals)
geo_person_marginals<-cbind(geo_person_marginals[,1:2],c("geo",rep("",nrow(geo_person_marginals)-1)),geo_person_marginals[,3:ncol(geo_person_marginals)])
geo_person_marginals$Variable[-1]<-geo_person_marginals$Variable[-1]
geo_person_marginals<-data.frame(t(geo_person_marginals))
temp<-geo_person_marginals
geo_person_marginals<-temp[,!((temp[1,]%in%c("PP_AGEGRP") & temp[2,]%in%c(4:13)))]
geo_person_marginals[-1:-3,(geo_person_marginals[1,]=="PP_AGEGRP" & geo_person_marginals[2,]==1)]<-rowSums(apply(temp[-1:-3,temp[1,]=="PP_AGEGRP" & temp[2,]%in%c(1:2)],2,as.numeric))
geo_person_marginals[-1:-3,(geo_person_marginals[1,]=="PP_AGEGRP" & geo_person_marginals[2,]==2)]<-rowSums(apply(temp[-1:-3,temp[1,]=="PP_AGEGRP" & temp[2,]%in%c(3:11)],2,as.numeric))
geo_person_marginals[-1:-3,(geo_person_marginals[1,]=="PP_AGEGRP" & geo_person_marginals[2,]==3)]<-rowSums(apply(temp[-1:-3,temp[1,]=="PP_AGEGRP" & temp[2,]%in%c(12:13)],2,as.numeric))
write_csv(geo_person_marginals,"Exported/A1/Montreal/Scenarios/S15-hCT/geo_person_marginals.csv",col_names = FALSE)
#household_sample
write_csv(household_sample,"Exported/A1/Montreal/Scenarios/S15-hCT/household_sample.csv",col_names = TRUE)
#person_sample
write_csv(person_sample,"Exported/A1/Montreal/Scenarios/S15-hCT/person_sample.csv",col_names = TRUE)

###S16-hCMA_CT
GIS<-GIS_CMA_CT
#region_geo_mapping
geo<-1:length(unique(GIS$CT))
region<-1
region_geo_mapping<-data.frame(region,geo)
write.csv(region_geo_mapping,"Exported/A1/Montreal/Scenarios/S16-hCMA_CT/region_geo_mapping.csv",row.names=FALSE)
#geo_sample_mapping
geo<-1:length(unique(GIS$CT))
sample_geo<-1
geo_sample_mapping<-data.frame(geo,sample_geo)
write.csv(geo_sample_mapping,"Exported/A1/Montreal/Scenarios/S16-hCMA_CT/geo_sample_mapping.csv",row.names=FALSE)
#region_sample_mapping
region<-1
sample_geo<--1
region_sample_mapping<-data.frame(region,sample_geo)
write.csv(region_sample_mapping,"Exported/A1/Montreal/Scenarios/S16-hCMA_CT/region_sample_mapping.csv",row.names=FALSE)
#region_household_marginals
region_household_marginals<-merge(CMAMAT[grepl("HH_",CMAMAT$Variable),],hh_variable_importance,by="Variable",all.x=TRUE)
region_household_marginals<-region_household_marginals[order(region_household_marginals$Importance),]
region_household_marginals<-region_household_marginals[region_household_marginals$Importance!=0,!names(region_household_marginals)%in%c("X","Definition","Description","Frequency","Importance")]
region_household_marginals$Categories[region_household_marginals$Variable=="HH_ID"]<-1
region_household_marginals<-rbind(c("variable_names","variable_categories",1:length(unique(GIS$CMA))),region_household_marginals)
region_household_marginals<-cbind(region_household_marginals[,1:2],c("region",rep("",nrow(region_household_marginals)-1)),region_household_marginals[,3:ncol(region_household_marginals)])
region_household_marginals$Variable[-1]<-paste("r",region_household_marginals$Variable[-1],sep="")
region_household_marginals<-data.frame(t(region_household_marginals))
temp<-region_household_marginals
region_household_marginals<-temp[,!((temp[1,]%in%c("rHH_TOTINCAT") & temp[2,]%in%c(5:16)))]
region_household_marginals[-1:-3,(region_household_marginals[1,]=="rHH_TOTINCAT" & region_household_marginals[2,]==1)]<-sum(apply(temp[-1:-3,temp[1,]=="rHH_TOTINCAT" & temp[2,]%in%c(1:6)],2,as.numeric))
region_household_marginals[-1:-3,(region_household_marginals[1,]=="rHH_TOTINCAT" & region_household_marginals[2,]==2)]<-sum(apply(temp[-1:-3,temp[1,]=="rHH_TOTINCAT" & temp[2,]%in%c(7:11)],2,as.numeric))
region_household_marginals[-1:-3,(region_household_marginals[1,]=="rHH_TOTINCAT" & region_household_marginals[2,]==3)]<-sum(apply(temp[-1:-3,temp[1,]=="rHH_TOTINCAT" & temp[2,]%in%c(12:15)],2,as.numeric))
region_household_marginals[-1:-3,(region_household_marginals[1,]=="rHH_TOTINCAT" & region_household_marginals[2,]==4)]<-as.numeric(temp[-1:-3,temp[1,]=="rHH_TOTINCAT" & temp[2,]%in%c(16)])
write_csv(region_household_marginals,"Exported/A1/Montreal/Scenarios/S16-hCMA_CT/region_household_marginals.csv",col_names = FALSE)
#region_person_marginals
region_person_marginals<-merge(CMAMAT[grepl("PP_",CMAMAT$Variable),],pp_variable_importance,by="Variable",all.x=TRUE)
region_person_marginals<-region_person_marginals[order(region_person_marginals$Importance),]
region_person_marginals<-region_person_marginals[region_person_marginals$Importance!=0,!names(region_person_marginals)%in%c("X","Definition","Description","Frequency","Importance")]
region_person_marginals$Categories[region_person_marginals$Variable=="PP_ID"]<-1
region_person_marginals<-rbind(c("variable_names","variable_categories",1:length(unique(GIS$CMA))),region_person_marginals)
region_person_marginals<-cbind(region_person_marginals[,1:2],c("region",rep("",nrow(region_person_marginals)-1)),region_person_marginals[,3:ncol(region_person_marginals)])
region_person_marginals$Variable[-1]<-paste("r",region_person_marginals$Variable[-1],sep="")
region_person_marginals<-data.frame(t(region_person_marginals))
temp<-region_person_marginals
region_person_marginals<-temp[,!((temp[1,]%in%c("rPP_AGEGRP") & temp[2,]%in%c(4:13)))]
region_person_marginals[-1:-3,(region_person_marginals[1,]=="rPP_AGEGRP" & region_person_marginals[2,]==1)]<-sum(apply(temp[-1:-3,temp[1,]=="rPP_AGEGRP" & temp[2,]%in%c(1:2)],2,as.numeric))
region_person_marginals[-1:-3,(region_person_marginals[1,]=="rPP_AGEGRP" & region_person_marginals[2,]==2)]<-sum(apply(temp[-1:-3,temp[1,]=="rPP_AGEGRP" & temp[2,]%in%c(3:11)],2,as.numeric))
region_person_marginals[-1:-3,(region_person_marginals[1,]=="rPP_AGEGRP" & region_person_marginals[2,]==3)]<-sum(apply(temp[-1:-3,temp[1,]=="rPP_AGEGRP" & temp[2,]%in%c(12:13)],2,as.numeric))
write_csv(region_person_marginals,"Exported/A1/Montreal/Scenarios/S16-hCMA_CT/region_person_marginals.csv",col_names = FALSE)
#geo_household_marginals
geo_household_marginals<-merge(CTMAT[grepl("HH_",CTMAT$Variable),],hh_variable_importance,by="Variable",all.x=TRUE)
geo_household_marginals<-geo_household_marginals[order(geo_household_marginals$Importance),]
geo_household_marginals<-geo_household_marginals[geo_household_marginals$Importance!=0,!names(geo_household_marginals)%in%c("X","Definition","Description","Frequency","Importance")]
geo_household_marginals$Categories[geo_household_marginals$Variable=="HH_ID"]<-1
geo_household_marginals<-rbind(c("variable_names","variable_categories",1:length(unique(GIS$CT))),geo_household_marginals)
geo_household_marginals<-cbind(geo_household_marginals[,1:2],c("geo",rep("",nrow(geo_household_marginals)-1)),geo_household_marginals[,3:ncol(geo_household_marginals)])
geo_household_marginals$Variable[-1]<-geo_household_marginals$Variable[-1]
geo_household_marginals<-data.frame(t(geo_household_marginals))
temp<-geo_household_marginals
geo_household_marginals<-temp[,!((temp[1,]%in%c("HH_TOTINCAT") & temp[2,]%in%c(5:16)))]
geo_household_marginals[-1:-3,(geo_household_marginals[1,]=="HH_TOTINCAT" & geo_household_marginals[2,]==1)]<-rowSums(apply(temp[-1:-3,temp[1,]=="HH_TOTINCAT" & temp[2,]%in%c(1:6)],2,as.numeric))
geo_household_marginals[-1:-3,(geo_household_marginals[1,]=="HH_TOTINCAT" & geo_household_marginals[2,]==2)]<-rowSums(apply(temp[-1:-3,temp[1,]=="HH_TOTINCAT" & temp[2,]%in%c(7:11)],2,as.numeric))
geo_household_marginals[-1:-3,(geo_household_marginals[1,]=="HH_TOTINCAT" & geo_household_marginals[2,]==3)]<-rowSums(apply(temp[-1:-3,temp[1,]=="HH_TOTINCAT" & temp[2,]%in%c(12:15)],2,as.numeric))
geo_household_marginals[-1:-3,(geo_household_marginals[1,]=="HH_TOTINCAT" & geo_household_marginals[2,]==4)]<-as.numeric(temp[-1:-3,temp[1,]=="HH_TOTINCAT" & temp[2,]%in%c(16)])
write_csv(geo_household_marginals,"Exported/A1/Montreal/Scenarios/S16-hCMA_CT/geo_household_marginals.csv",col_names = FALSE)
#geo_person_marginals
geo_person_marginals<-merge(CTMAT[grepl("PP_",CTMAT$Variable),],pp_variable_importance,by="Variable",all.x=TRUE)
geo_person_marginals<-geo_person_marginals[order(geo_person_marginals$Importance),]
geo_person_marginals<-geo_person_marginals[geo_person_marginals$Importance!=0,!names(geo_person_marginals)%in%c("X","Definition","Description","Frequency","Importance")]
geo_person_marginals$Categories[geo_person_marginals$Variable=="PP_ID"]<-1
geo_person_marginals<-rbind(c("variable_names","variable_categories",1:length(unique(GIS$CT))),geo_person_marginals)
geo_person_marginals<-cbind(geo_person_marginals[,1:2],c("geo",rep("",nrow(geo_person_marginals)-1)),geo_person_marginals[,3:ncol(geo_person_marginals)])
geo_person_marginals$Variable[-1]<-geo_person_marginals$Variable[-1]
geo_person_marginals<-data.frame(t(geo_person_marginals))
temp<-geo_person_marginals
geo_person_marginals<-temp[,!((temp[1,]%in%c("PP_AGEGRP") & temp[2,]%in%c(4:13)))]
geo_person_marginals[-1:-3,(geo_person_marginals[1,]=="PP_AGEGRP" & geo_person_marginals[2,]==1)]<-rowSums(apply(temp[-1:-3,temp[1,]=="PP_AGEGRP" & temp[2,]%in%c(1:2)],2,as.numeric))
geo_person_marginals[-1:-3,(geo_person_marginals[1,]=="PP_AGEGRP" & geo_person_marginals[2,]==2)]<-rowSums(apply(temp[-1:-3,temp[1,]=="PP_AGEGRP" & temp[2,]%in%c(3:11)],2,as.numeric))
geo_person_marginals[-1:-3,(geo_person_marginals[1,]=="PP_AGEGRP" & geo_person_marginals[2,]==3)]<-rowSums(apply(temp[-1:-3,temp[1,]=="PP_AGEGRP" & temp[2,]%in%c(12:13)],2,as.numeric))
write_csv(geo_person_marginals,"Exported/A1/Montreal/Scenarios/S16-hCMA_CT/geo_person_marginals.csv",col_names = FALSE)
#household_sample
write_csv(household_sample,"Exported/A1/Montreal/Scenarios/S16-hCMA_CT/household_sample.csv",col_names = TRUE)
#person_sample
write_csv(person_sample,"Exported/A1/Montreal/Scenarios/S16-hCMA_CT/person_sample.csv",col_names = TRUE)

###S17-hDA
GIS<-GIS_CMA_DA
#region_geo_mapping
geo<-1:length(unique(GIS$DA))
region<-1:length(unique(GIS$DA))
region_geo_mapping<-data.frame(region,geo)
write.csv(region_geo_mapping,"Exported/A1/Montreal/Scenarios/S17-hDA/region_geo_mapping.csv",row.names=FALSE)
#geo_sample_mapping
geo<-1:length(unique(GIS$DA))
sample_geo<-1
geo_sample_mapping<-data.frame(geo,sample_geo)
write.csv(geo_sample_mapping,"Exported/A1/Montreal/Scenarios/S17-hDA/geo_sample_mapping.csv",row.names=FALSE)
#region_sample_mapping
region<-1:length(unique(GIS$DA))
sample_geo<--1
region_sample_mapping<-data.frame(region,sample_geo)
write.csv(region_sample_mapping,"Exported/A1/Montreal/Scenarios/S17-hDA/region_sample_mapping.csv",row.names=FALSE)
#region_household_marginals
region_household_marginals<-merge(DAMAT[grepl("HH_",DAMAT$Variable),],hh_variable_importance,by="Variable",all.x=TRUE)
region_household_marginals<-region_household_marginals[order(region_household_marginals$Importance),]
region_household_marginals<-region_household_marginals[region_household_marginals$Importance!=0,!names(region_household_marginals)%in%c("X","Definition","Description","Frequency","Importance")]
region_household_marginals$Categories[region_household_marginals$Variable=="HH_ID"]<-1
region_household_marginals<-rbind(c("variable_names","variable_categories",1:length(unique(GIS$DA))),region_household_marginals)
region_household_marginals<-cbind(region_household_marginals[,1:2],c("region",rep("",nrow(region_household_marginals)-1)),region_household_marginals[,3:ncol(region_household_marginals)])
region_household_marginals$Variable[-1]<-paste("r",region_household_marginals$Variable[-1],sep="")
region_household_marginals<-data.frame(t(region_household_marginals))
temp<-region_household_marginals
region_household_marginals<-temp[,!((temp[1,]%in%c("rHH_TOTINCAT") & temp[2,]%in%c(5:16)))]
region_household_marginals[-1:-3,(region_household_marginals[1,]=="rHH_TOTINCAT" & region_household_marginals[2,]==1)]<-rowSums(apply(temp[-1:-3,temp[1,]=="rHH_TOTINCAT" & temp[2,]%in%c(1:6)],2,as.numeric))
region_household_marginals[-1:-3,(region_household_marginals[1,]=="rHH_TOTINCAT" & region_household_marginals[2,]==2)]<-rowSums(apply(temp[-1:-3,temp[1,]=="rHH_TOTINCAT" & temp[2,]%in%c(7:11)],2,as.numeric))
region_household_marginals[-1:-3,(region_household_marginals[1,]=="rHH_TOTINCAT" & region_household_marginals[2,]==3)]<-rowSums(apply(temp[-1:-3,temp[1,]=="rHH_TOTINCAT" & temp[2,]%in%c(12:15)],2,as.numeric))
region_household_marginals[-1:-3,(region_household_marginals[1,]=="rHH_TOTINCAT" & region_household_marginals[2,]==4)]<-as.numeric(temp[-1:-3,temp[1,]=="rHH_TOTINCAT" & temp[2,]%in%c(16)])
write_csv(region_household_marginals,"Exported/A1/Montreal/Scenarios/S17-hDA/region_household_marginals.csv",col_names = FALSE)
#region_person_marginals
region_person_marginals<-merge(DAMAT[grepl("PP_",DAMAT$Variable),],pp_variable_importance,by="Variable",all.x=TRUE)
region_person_marginals<-region_person_marginals[order(region_person_marginals$Importance),]
region_person_marginals<-region_person_marginals[region_person_marginals$Importance!=0,!names(region_person_marginals)%in%c("X","Definition","Description","Frequency","Importance")]
region_person_marginals$Categories[region_person_marginals$Variable=="PP_ID"]<-1
region_person_marginals<-rbind(c("variable_names","variable_categories",1:length(unique(GIS$DA))),region_person_marginals)
region_person_marginals<-cbind(region_person_marginals[,1:2],c("region",rep("",nrow(region_person_marginals)-1)),region_person_marginals[,3:ncol(region_person_marginals)])
region_person_marginals$Variable[-1]<-paste("r",region_person_marginals$Variable[-1],sep="")
region_person_marginals<-data.frame(t(region_person_marginals))
temp<-region_person_marginals
region_person_marginals<-temp[,!((temp[1,]%in%c("rPP_AGEGRP") & temp[2,]%in%c(4:13)))]
region_person_marginals[-1:-3,(region_person_marginals[1,]=="rPP_AGEGRP" & region_person_marginals[2,]==1)]<-rowSums(apply(temp[-1:-3,temp[1,]=="rPP_AGEGRP" & temp[2,]%in%c(1:2)],2,as.numeric))
region_person_marginals[-1:-3,(region_person_marginals[1,]=="rPP_AGEGRP" & region_person_marginals[2,]==2)]<-rowSums(apply(temp[-1:-3,temp[1,]=="rPP_AGEGRP" & temp[2,]%in%c(3:11)],2,as.numeric))
region_person_marginals[-1:-3,(region_person_marginals[1,]=="rPP_AGEGRP" & region_person_marginals[2,]==3)]<-rowSums(apply(temp[-1:-3,temp[1,]=="rPP_AGEGRP" & temp[2,]%in%c(12:13)],2,as.numeric))
write_csv(region_person_marginals,"Exported/A1/Montreal/Scenarios/S17-hDA/region_person_marginals.csv",col_names = FALSE)
#geo_household_marginals
geo_household_marginals<-merge(DAMAT[grepl("HH_",DAMAT$Variable),],hh_variable_importance,by="Variable",all.x=TRUE)
geo_household_marginals<-geo_household_marginals[order(geo_household_marginals$Importance),]
geo_household_marginals<-geo_household_marginals[geo_household_marginals$Importance!=0,!names(geo_household_marginals)%in%c("X","Definition","Description","Frequency","Importance")]
geo_household_marginals$Categories[geo_household_marginals$Variable=="HH_ID"]<-1
geo_household_marginals<-rbind(c("variable_names","variable_categories",1:length(unique(GIS$DA))),geo_household_marginals)
geo_household_marginals<-cbind(geo_household_marginals[,1:2],c("geo",rep("",nrow(geo_household_marginals)-1)),geo_household_marginals[,3:ncol(geo_household_marginals)])
geo_household_marginals$Variable[-1]<-geo_household_marginals$Variable[-1]
geo_household_marginals<-data.frame(t(geo_household_marginals))
temp<-geo_household_marginals
geo_household_marginals<-temp[,!((temp[1,]%in%c("HH_TOTINCAT") & temp[2,]%in%c(5:16)))]
geo_household_marginals[-1:-3,(geo_household_marginals[1,]=="HH_TOTINCAT" & geo_household_marginals[2,]==1)]<-rowSums(apply(temp[-1:-3,temp[1,]=="HH_TOTINCAT" & temp[2,]%in%c(1:6)],2,as.numeric))
geo_household_marginals[-1:-3,(geo_household_marginals[1,]=="HH_TOTINCAT" & geo_household_marginals[2,]==2)]<-rowSums(apply(temp[-1:-3,temp[1,]=="HH_TOTINCAT" & temp[2,]%in%c(7:11)],2,as.numeric))
geo_household_marginals[-1:-3,(geo_household_marginals[1,]=="HH_TOTINCAT" & geo_household_marginals[2,]==3)]<-rowSums(apply(temp[-1:-3,temp[1,]=="HH_TOTINCAT" & temp[2,]%in%c(12:15)],2,as.numeric))
geo_household_marginals[-1:-3,(geo_household_marginals[1,]=="HH_TOTINCAT" & geo_household_marginals[2,]==4)]<-as.numeric(temp[-1:-3,temp[1,]=="HH_TOTINCAT" & temp[2,]%in%c(16)])
write_csv(geo_household_marginals,"Exported/A1/Montreal/Scenarios/S17-hDA/geo_household_marginals.csv",col_names = FALSE)
#geo_person_marginals
geo_person_marginals<-merge(DAMAT[grepl("PP_",DAMAT$Variable),],pp_variable_importance,by="Variable",all.x=TRUE)
geo_person_marginals<-geo_person_marginals[order(geo_person_marginals$Importance),]
geo_person_marginals<-geo_person_marginals[geo_person_marginals$Importance!=0,!names(geo_person_marginals)%in%c("X","Definition","Description","Frequency","Importance")]
geo_person_marginals$Categories[geo_person_marginals$Variable=="PP_ID"]<-1
geo_person_marginals<-rbind(c("variable_names","variable_categories",1:length(unique(GIS$DA))),geo_person_marginals)
geo_person_marginals<-cbind(geo_person_marginals[,1:2],c("geo",rep("",nrow(geo_person_marginals)-1)),geo_person_marginals[,3:ncol(geo_person_marginals)])
geo_person_marginals$Variable[-1]<-geo_person_marginals$Variable[-1]
geo_person_marginals<-data.frame(t(geo_person_marginals))
temp<-geo_person_marginals
geo_person_marginals<-temp[,!((temp[1,]%in%c("PP_AGEGRP") & temp[2,]%in%c(4:13)))]
geo_person_marginals[-1:-3,(geo_person_marginals[1,]=="PP_AGEGRP" & geo_person_marginals[2,]==1)]<-rowSums(apply(temp[-1:-3,temp[1,]=="PP_AGEGRP" & temp[2,]%in%c(1:2)],2,as.numeric))
geo_person_marginals[-1:-3,(geo_person_marginals[1,]=="PP_AGEGRP" & geo_person_marginals[2,]==2)]<-rowSums(apply(temp[-1:-3,temp[1,]=="PP_AGEGRP" & temp[2,]%in%c(3:11)],2,as.numeric))
geo_person_marginals[-1:-3,(geo_person_marginals[1,]=="PP_AGEGRP" & geo_person_marginals[2,]==3)]<-rowSums(apply(temp[-1:-3,temp[1,]=="PP_AGEGRP" & temp[2,]%in%c(12:13)],2,as.numeric))
write_csv(geo_person_marginals,"Exported/A1/Montreal/Scenarios/S17-hDA/geo_person_marginals.csv",col_names = FALSE)
#household_sample
write_csv(household_sample,"Exported/A1/Montreal/Scenarios/S17-hDA/household_sample.csv",col_names = TRUE)
#person_sample
write_csv(person_sample,"Exported/A1/Montreal/Scenarios/S17-hDA/person_sample.csv",col_names = TRUE)

###S18-hCMA_DA
GIS<-GIS_CMA_DA
#region_geo_mapping
geo<-1:length(unique(GIS$DA))
region<-1
region_geo_mapping<-data.frame(region,geo)
write.csv(region_geo_mapping,"Exported/A1/Montreal/Scenarios/S18-hCMA_DA/region_geo_mapping.csv",row.names=FALSE)
#geo_sample_mapping
geo<-1:length(unique(GIS$DA))
sample_geo<-1
geo_sample_mapping<-data.frame(geo,sample_geo)
write.csv(geo_sample_mapping,"Exported/A1/Montreal/Scenarios/S18-hCMA_DA/geo_sample_mapping.csv",row.names=FALSE)
#region_sample_mapping
region<-1
sample_geo<--1
region_sample_mapping<-data.frame(region,sample_geo)
write.csv(region_sample_mapping,"Exported/A1/Montreal/Scenarios/S18-hCMA_DA/region_sample_mapping.csv",row.names=FALSE)
#region_household_marginals
region_household_marginals<-merge(CMAMAT[grepl("HH_",CMAMAT$Variable),],hh_variable_importance,by="Variable",all.x=TRUE)
region_household_marginals<-region_household_marginals[order(region_household_marginals$Importance),]
region_household_marginals<-region_household_marginals[region_household_marginals$Importance!=0,!names(region_household_marginals)%in%c("X","Definition","Description","Frequency","Importance")]
region_household_marginals$Categories[region_household_marginals$Variable=="HH_ID"]<-1
region_household_marginals<-rbind(c("variable_names","variable_categories",1:length(unique(GIS$CMA))),region_household_marginals)
region_household_marginals<-cbind(region_household_marginals[,1:2],c("region",rep("",nrow(region_household_marginals)-1)),region_household_marginals[,3:ncol(region_household_marginals)])
region_household_marginals$Variable[-1]<-paste("r",region_household_marginals$Variable[-1],sep="")
region_household_marginals<-data.frame(t(region_household_marginals))
temp<-region_household_marginals
region_household_marginals<-temp[,!((temp[1,]%in%c("rHH_TOTINCAT") & temp[2,]%in%c(5:16)))]
region_household_marginals[-1:-3,(region_household_marginals[1,]=="rHH_TOTINCAT" & region_household_marginals[2,]==1)]<-sum(apply(temp[-1:-3,temp[1,]=="rHH_TOTINCAT" & temp[2,]%in%c(1:6)],2,as.numeric))
region_household_marginals[-1:-3,(region_household_marginals[1,]=="rHH_TOTINCAT" & region_household_marginals[2,]==2)]<-sum(apply(temp[-1:-3,temp[1,]=="rHH_TOTINCAT" & temp[2,]%in%c(7:11)],2,as.numeric))
region_household_marginals[-1:-3,(region_household_marginals[1,]=="rHH_TOTINCAT" & region_household_marginals[2,]==3)]<-sum(apply(temp[-1:-3,temp[1,]=="rHH_TOTINCAT" & temp[2,]%in%c(12:15)],2,as.numeric))
region_household_marginals[-1:-3,(region_household_marginals[1,]=="rHH_TOTINCAT" & region_household_marginals[2,]==4)]<-as.numeric(temp[-1:-3,temp[1,]=="rHH_TOTINCAT" & temp[2,]%in%c(16)])
write_csv(region_household_marginals,"Exported/A1/Montreal/Scenarios/S18-hCMA_DA/region_household_marginals.csv",col_names = FALSE)
#region_person_marginals
region_person_marginals<-merge(CMAMAT[grepl("PP_",CMAMAT$Variable),],pp_variable_importance,by="Variable",all.x=TRUE)
region_person_marginals<-region_person_marginals[order(region_person_marginals$Importance),]
region_person_marginals<-region_person_marginals[region_person_marginals$Importance!=0,!names(region_person_marginals)%in%c("X","Definition","Description","Frequency","Importance")]
region_person_marginals$Categories[region_person_marginals$Variable=="PP_ID"]<-1
region_person_marginals<-rbind(c("variable_names","variable_categories",1:length(unique(GIS$CMA))),region_person_marginals)
region_person_marginals<-cbind(region_person_marginals[,1:2],c("region",rep("",nrow(region_person_marginals)-1)),region_person_marginals[,3:ncol(region_person_marginals)])
region_person_marginals$Variable[-1]<-paste("r",region_person_marginals$Variable[-1],sep="")
region_person_marginals<-data.frame(t(region_person_marginals))
temp<-region_person_marginals
region_person_marginals<-temp[,!((temp[1,]%in%c("rPP_AGEGRP") & temp[2,]%in%c(4:13)))]
region_person_marginals[-1:-3,(region_person_marginals[1,]=="rPP_AGEGRP" & region_person_marginals[2,]==1)]<-sum(apply(temp[-1:-3,temp[1,]=="rPP_AGEGRP" & temp[2,]%in%c(1:2)],2,as.numeric))
region_person_marginals[-1:-3,(region_person_marginals[1,]=="rPP_AGEGRP" & region_person_marginals[2,]==2)]<-sum(apply(temp[-1:-3,temp[1,]=="rPP_AGEGRP" & temp[2,]%in%c(3:11)],2,as.numeric))
region_person_marginals[-1:-3,(region_person_marginals[1,]=="rPP_AGEGRP" & region_person_marginals[2,]==3)]<-sum(apply(temp[-1:-3,temp[1,]=="rPP_AGEGRP" & temp[2,]%in%c(12:13)],2,as.numeric))
write_csv(region_person_marginals,"Exported/A1/Montreal/Scenarios/S18-hCMA_DA/region_person_marginals.csv",col_names = FALSE)
#geo_household_marginals
geo_household_marginals<-merge(DAMAT[grepl("HH_",DAMAT$Variable),],hh_variable_importance,by="Variable",all.x=TRUE)
geo_household_marginals<-geo_household_marginals[order(geo_household_marginals$Importance),]
geo_household_marginals<-geo_household_marginals[geo_household_marginals$Importance!=0,!names(geo_household_marginals)%in%c("X","Definition","Description","Frequency","Importance")]
geo_household_marginals$Categories[geo_household_marginals$Variable=="HH_ID"]<-1
geo_household_marginals<-rbind(c("variable_names","variable_categories",1:length(unique(GIS$DA))),geo_household_marginals)
geo_household_marginals<-cbind(geo_household_marginals[,1:2],c("geo",rep("",nrow(geo_household_marginals)-1)),geo_household_marginals[,3:ncol(geo_household_marginals)])
geo_household_marginals$Variable[-1]<-geo_household_marginals$Variable[-1]
geo_household_marginals<-data.frame(t(geo_household_marginals))
temp<-geo_household_marginals
geo_household_marginals<-temp[,!((temp[1,]%in%c("HH_TOTINCAT") & temp[2,]%in%c(5:16)))]
geo_household_marginals[-1:-3,(geo_household_marginals[1,]=="HH_TOTINCAT" & geo_household_marginals[2,]==1)]<-rowSums(apply(temp[-1:-3,temp[1,]=="HH_TOTINCAT" & temp[2,]%in%c(1:6)],2,as.numeric))
geo_household_marginals[-1:-3,(geo_household_marginals[1,]=="HH_TOTINCAT" & geo_household_marginals[2,]==2)]<-rowSums(apply(temp[-1:-3,temp[1,]=="HH_TOTINCAT" & temp[2,]%in%c(7:11)],2,as.numeric))
geo_household_marginals[-1:-3,(geo_household_marginals[1,]=="HH_TOTINCAT" & geo_household_marginals[2,]==3)]<-rowSums(apply(temp[-1:-3,temp[1,]=="HH_TOTINCAT" & temp[2,]%in%c(12:15)],2,as.numeric))
geo_household_marginals[-1:-3,(geo_household_marginals[1,]=="HH_TOTINCAT" & geo_household_marginals[2,]==4)]<-as.numeric(temp[-1:-3,temp[1,]=="HH_TOTINCAT" & temp[2,]%in%c(16)])
write_csv(geo_household_marginals,"Exported/A1/Montreal/Scenarios/S18-hCMA_DA/geo_household_marginals.csv",col_names = FALSE)
#geo_person_marginals
geo_person_marginals<-merge(DAMAT[grepl("PP_",DAMAT$Variable),],pp_variable_importance,by="Variable",all.x=TRUE)
geo_person_marginals<-geo_person_marginals[order(geo_person_marginals$Importance),]
geo_person_marginals<-geo_person_marginals[geo_person_marginals$Importance!=0,!names(geo_person_marginals)%in%c("X","Definition","Description","Frequency","Importance")]
geo_person_marginals$Categories[geo_person_marginals$Variable=="PP_ID"]<-1
geo_person_marginals<-rbind(c("variable_names","variable_categories",1:length(unique(GIS$DA))),geo_person_marginals)
geo_person_marginals<-cbind(geo_person_marginals[,1:2],c("geo",rep("",nrow(geo_person_marginals)-1)),geo_person_marginals[,3:ncol(geo_person_marginals)])
geo_person_marginals$Variable[-1]<-geo_person_marginals$Variable[-1]
geo_person_marginals<-data.frame(t(geo_person_marginals))
temp<-geo_person_marginals
geo_person_marginals<-temp[,!((temp[1,]%in%c("PP_AGEGRP") & temp[2,]%in%c(4:13)))]
geo_person_marginals[-1:-3,(geo_person_marginals[1,]=="PP_AGEGRP" & geo_person_marginals[2,]==1)]<-rowSums(apply(temp[-1:-3,temp[1,]=="PP_AGEGRP" & temp[2,]%in%c(1:2)],2,as.numeric))
geo_person_marginals[-1:-3,(geo_person_marginals[1,]=="PP_AGEGRP" & geo_person_marginals[2,]==2)]<-rowSums(apply(temp[-1:-3,temp[1,]=="PP_AGEGRP" & temp[2,]%in%c(3:11)],2,as.numeric))
geo_person_marginals[-1:-3,(geo_person_marginals[1,]=="PP_AGEGRP" & geo_person_marginals[2,]==3)]<-rowSums(apply(temp[-1:-3,temp[1,]=="PP_AGEGRP" & temp[2,]%in%c(12:13)],2,as.numeric))
write_csv(geo_person_marginals,"Exported/A1/Montreal/Scenarios/S18-hCMA_DA/geo_person_marginals.csv",col_names = FALSE)
#household_sample
write_csv(household_sample,"Exported/A1/Montreal/Scenarios/S18-hCMA_DA/household_sample.csv",col_names = TRUE)
#person_sample
write_csv(person_sample,"Exported/A1/Montreal/Scenarios/S18-hCMA_DA/person_sample.csv",col_names = TRUE)


dir.create("Exported/A1/2. Popgen/PopGen2.1 - AGE & INC less categories/MS1-bCMA")
dir.create("Exported/A1/2. Popgen/PopGen2.1 - AGE & INC less categories/MS2-bCSD")
dir.create("Exported/A1/2. Popgen/PopGen2.1 - AGE & INC less categories/MS3-bCMA_CSD")
dir.create("Exported/A1/2. Popgen/PopGen2.1 - AGE & INC less categories/MS4-bADA")
dir.create("Exported/A1/2. Popgen/PopGen2.1 - AGE & INC less categories/MS5-bCMA_ADA")
dir.create("Exported/A1/2. Popgen/PopGen2.1 - AGE & INC less categories/MS6-bCT")
dir.create("Exported/A1/2. Popgen/PopGen2.1 - AGE & INC less categories/MS7-bCMA_CT")
dir.create("Exported/A1/2. Popgen/PopGen2.1 - AGE & INC less categories/MS8-bDA")
dir.create("Exported/A1/2. Popgen/PopGen2.1 - AGE & INC less categories/MS9-bCMA_DA")
dir.create("Exported/A1/2. Popgen/PopGen2.1 - AGE & INC less categories/MS10-hCMA")
dir.create("Exported/A1/2. Popgen/PopGen2.1 - AGE & INC less categories/MS11-hCSD")
dir.create("Exported/A1/2. Popgen/PopGen2.1 - AGE & INC less categories/MS12-hCMA_CSD")
dir.create("Exported/A1/2. Popgen/PopGen2.1 - AGE & INC less categories/MS13-hADA")
dir.create("Exported/A1/2. Popgen/PopGen2.1 - AGE & INC less categories/MS14-hCMA_ADA")
dir.create("Exported/A1/2. Popgen/PopGen2.1 - AGE & INC less categories/MS15-hCT")
dir.create("Exported/A1/2. Popgen/PopGen2.1 - AGE & INC less categories/MS16-hCMA_CT")
dir.create("Exported/A1/2. Popgen/PopGen2.1 - AGE & INC less categories/MS17-hDA")
dir.create("Exported/A1/2. Popgen/PopGen2.1 - AGE & INC less categories/MS18-hCMA_DA")

current_folder <- "Exported/A1/Montreal/Scenarios/S1-bCMA"
new_folder <- "Exported/A1/2. Popgen/PopGen2.1 - AGE & INC less categories/MS1-bCMA"
list_of_files <- list.files(current_folder,) 
file.copy(file.path(current_folder,list_of_files), new_folder)

current_folder <- "Exported/A1/Montreal/Scenarios/S2-bCSD"
new_folder <- "Exported/A1/2. Popgen/PopGen2.1 - AGE & INC less categories/MS2-bCSD"
list_of_files <- list.files(current_folder,) 
file.copy(file.path(current_folder,list_of_files), new_folder)

current_folder <- "Exported/A1/Montreal/Scenarios/S3-bCMA_CSD"
new_folder <- "Exported/A1/2. Popgen/PopGen2.1 - AGE & INC less categories/MS3-bCMA_CSD"
list_of_files <- list.files(current_folder,) 
file.copy(file.path(current_folder,list_of_files), new_folder)

current_folder <- "Exported/A1/Montreal/Scenarios/S4-bADA"
new_folder <- "Exported/A1/2. Popgen/PopGen2.1 - AGE & INC less categories/MS4-bADA"
list_of_files <- list.files(current_folder,) 
file.copy(file.path(current_folder,list_of_files), new_folder)

current_folder <- "Exported/A1/Montreal/Scenarios/S5-bCMA_ADA"
new_folder <- "Exported/A1/2. Popgen/PopGen2.1 - AGE & INC less categories/MS5-bCMA_ADA"
list_of_files <- list.files(current_folder,) 
file.copy(file.path(current_folder,list_of_files), new_folder)

current_folder <- "Exported/A1/Montreal/Scenarios/S6-bCT"
new_folder <- "Exported/A1/2. Popgen/PopGen2.1 - AGE & INC less categories/MS6-bCT"
list_of_files <- list.files(current_folder,) 
file.copy(file.path(current_folder,list_of_files), new_folder)

current_folder <- "Exported/A1/Montreal/Scenarios/S7-bCMA_CT"
new_folder <- "Exported/A1/2. Popgen/PopGen2.1 - AGE & INC less categories/MS7-bCMA_CT"
list_of_files <- list.files(current_folder,) 
file.copy(file.path(current_folder,list_of_files), new_folder)

current_folder <- "Exported/A1/Montreal/Scenarios/S8-bDA"
new_folder <- "Exported/A1/2. Popgen/PopGen2.1 - AGE & INC less categories/MS8-bDA"
list_of_files <- list.files(current_folder,) 
file.copy(file.path(current_folder,list_of_files), new_folder)

current_folder <- "Exported/A1/Montreal/Scenarios/S9-bCMA_DA"
new_folder <- "Exported/A1/2. Popgen/PopGen2.1 - AGE & INC less categories/MS9-bCMA_DA"
list_of_files <- list.files(current_folder,) 
file.copy(file.path(current_folder,list_of_files), new_folder)

current_folder <- "Exported/A1/Montreal/Scenarios/S10-hCMA"
new_folder <- "Exported/A1/2. Popgen/PopGen2.1 - AGE & INC less categories/MS10-hCMA"
list_of_files <- list.files(current_folder,) 
file.copy(file.path(current_folder,list_of_files), new_folder)

current_folder <- "Exported/A1/Montreal/Scenarios/S11-hCSD"
new_folder <- "Exported/A1/2. Popgen/PopGen2.1 - AGE & INC less categories/MS11-hCSD"
list_of_files <- list.files(current_folder,) 
file.copy(file.path(current_folder,list_of_files), new_folder)

current_folder <- "Exported/A1/Montreal/Scenarios/S12-hCMA_CSD"
new_folder <- "Exported/A1/2. Popgen/PopGen2.1 - AGE & INC less categories/MS12-hCMA_CSD"
list_of_files <- list.files(current_folder,) 
file.copy(file.path(current_folder,list_of_files), new_folder)

current_folder <- "Exported/A1/Montreal/Scenarios/S13-hADA"
new_folder <- "Exported/A1/2. Popgen/PopGen2.1 - AGE & INC less categories/MS13-hADA"
list_of_files <- list.files(current_folder,) 
file.copy(file.path(current_folder,list_of_files), new_folder)

current_folder <- "Exported/A1/Montreal/Scenarios/S14-hCMA_ADA"
new_folder <- "Exported/A1/2. Popgen/PopGen2.1 - AGE & INC less categories/MS14-hCMA_ADA"
list_of_files <- list.files(current_folder,) 
file.copy(file.path(current_folder,list_of_files), new_folder)

current_folder <- "Exported/A1/Montreal/Scenarios/S15-hCT"
new_folder <- "Exported/A1/2. Popgen/PopGen2.1 - AGE & INC less categories/MS15-hCT"
list_of_files <- list.files(current_folder,) 
file.copy(file.path(current_folder,list_of_files), new_folder)

current_folder <- "Exported/A1/Montreal/Scenarios/S16-hCMA_CT"
new_folder <- "Exported/A1/2. Popgen/PopGen2.1 - AGE & INC less categories/MS16-hCMA_CT"
list_of_files <- list.files(current_folder,) 
file.copy(file.path(current_folder,list_of_files), new_folder)

current_folder <- "Exported/A1/Montreal/Scenarios/S17-hDA"
new_folder <- "Exported/A1/2. Popgen/PopGen2.1 - AGE & INC less categories/MS17-hDA"
list_of_files <- list.files(current_folder,) 
file.copy(file.path(current_folder,list_of_files), new_folder)

current_folder <- "Exported/A1/Montreal/Scenarios/S18-hCMA_DA"
new_folder <- "Exported/A1/2. Popgen/PopGen2.1 - AGE & INC less categories/MS18-hCMA_DA"
list_of_files <- list.files(current_folder,) 
file.copy(file.path(current_folder,list_of_files), new_folder)









# ###S5-hCMA_CSD
# GIS<-GIS_CMA_CSD
# #region_geo_mapping
# geo<-1:length(unique(GIS$CSD))
# region<-1
# region_geo_mapping<-data.frame(region,geo)
# write.csv(region_geo_mapping,"Exported/A1/Montreal/Scenarios/S5-hCMA_CSD/region_geo_mapping.csv",row.names=FALSE)
# #geo_sample_mapping
# geo<-1:length(unique(GIS$CSD))
# sample_geo<-1
# geo_sample_mapping<-data.frame(geo,sample_geo)
# write.csv(geo_sample_mapping,"Exported/A1/Montreal/Scenarios/S5-hCMA_CSD/geo_sample_mapping.csv",row.names=FALSE)
# #region_sample_mapping
# region<-1
# sample_geo<--1
# region_sample_mapping<-data.frame(region,sample_geo)
# write.csv(region_sample_mapping,"Exported/A1/Montreal/Scenarios/S5-hCMA_CSD/region_sample_mapping.csv",row.names=FALSE)
# #region_household_marginals
# region_household_marginals<-merge(CMAMAT[grepl("HH_",CMAMAT$Variable),],hh_variable_importance,by="Variable",all.x=TRUE)
# region_household_marginals<-region_household_marginals[order(region_household_marginals$Importance),]
# region_household_marginals<-region_household_marginals[region_household_marginals$Importance!=0,!names(region_household_marginals)%in%c("X","Definition","Description","Frequency","Importance")]
# region_household_marginals$Categories[region_household_marginals$Variable=="HH_ID"]<-1
# region_household_marginals<-rbind(c("variable_names","variable_categories",1:length(unique(GIS$CMA))),region_household_marginals)
# region_household_marginals<-cbind(region_household_marginals[,1:2],c("region",rep("",nrow(region_household_marginals)-1)),region_household_marginals[,3:ncol(region_household_marginals)])
# region_household_marginals$Variable[-1]<-paste("r",region_household_marginals$Variable[-1],sep="")
# region_household_marginals<-data.frame(t(region_household_marginals))
# temp<-region_household_marginals
# region_household_marginals<-temp[,!((temp[1,]%in%c("rHH_TOTINCAT") & temp[2,]%in%c(5:16)))]
# region_household_marginals[-1:-3,(region_household_marginals[1,]=="rHH_TOTINCAT" & region_household_marginals[2,]==1)]<-sum(apply(temp[-1:-3,temp[1,]=="rHH_TOTINCAT" & temp[2,]%in%c(1:6)],2,as.numeric))
# region_household_marginals[-1:-3,(region_household_marginals[1,]=="rHH_TOTINCAT" & region_household_marginals[2,]==2)]<-sum(apply(temp[-1:-3,temp[1,]=="rHH_TOTINCAT" & temp[2,]%in%c(7:11)],2,as.numeric))
# region_household_marginals[-1:-3,(region_household_marginals[1,]=="rHH_TOTINCAT" & region_household_marginals[2,]==3)]<-sum(apply(temp[-1:-3,temp[1,]=="rHH_TOTINCAT" & temp[2,]%in%c(12:15)],2,as.numeric))
# region_household_marginals[-1:-3,(region_household_marginals[1,]=="rHH_TOTINCAT" & region_household_marginals[2,]==4)]<-as.numeric(temp[-1:-3,temp[1,]=="rHH_TOTINCAT" & temp[2,]%in%c(16)])
# write_csv(region_household_marginals,"Exported/A1/Montreal/Scenarios/S5-hCMA_CSD/region_household_marginals.csv",col_names = FALSE)
# #region_person_marginals
# region_person_marginals<-merge(CMAMAT[grepl("PP_",CMAMAT$Variable),],pp_variable_importance,by="Variable",all.x=TRUE)
# region_person_marginals<-region_person_marginals[order(region_person_marginals$Importance),]
# region_person_marginals<-region_person_marginals[region_person_marginals$Importance!=0,!names(region_person_marginals)%in%c("X","Definition","Description","Frequency","Importance")]
# region_person_marginals$Categories[region_person_marginals$Variable=="PP_ID"]<-1
# region_person_marginals<-rbind(c("variable_names","variable_categories",1:length(unique(GIS$CMA))),region_person_marginals)
# region_person_marginals<-cbind(region_person_marginals[,1:2],c("region",rep("",nrow(region_person_marginals)-1)),region_person_marginals[,3:ncol(region_person_marginals)])
# region_person_marginals$Variable[-1]<-paste("r",region_person_marginals$Variable[-1],sep="")
# region_person_marginals<-data.frame(t(region_person_marginals))
# temp<-region_person_marginals
# region_person_marginals<-temp[,!((temp[1,]%in%c("rPP_AGEGRP") & temp[2,]%in%c(4:13)))]
# region_person_marginals[-1:-3,(region_person_marginals[1,]=="rPP_AGEGRP" & region_person_marginals[2,]==1)]<-sum(apply(temp[-1:-3,temp[1,]=="rPP_AGEGRP" & temp[2,]%in%c(1:2)],2,as.numeric))
# region_person_marginals[-1:-3,(region_person_marginals[1,]=="rPP_AGEGRP" & region_person_marginals[2,]==2)]<-sum(apply(temp[-1:-3,temp[1,]=="rPP_AGEGRP" & temp[2,]%in%c(3:11)],2,as.numeric))
# region_person_marginals[-1:-3,(region_person_marginals[1,]=="rPP_AGEGRP" & region_person_marginals[2,]==3)]<-sum(apply(temp[-1:-3,temp[1,]=="rPP_AGEGRP" & temp[2,]%in%c(12:13)],2,as.numeric))
# write_csv(region_person_marginals,"Exported/A1/Montreal/Scenarios/S5-hCMA_CSD/region_person_marginals.csv",col_names = FALSE)
# #geo_household_marginals
# geo_household_marginals<-merge(CSDMAT[grepl("HH_",CSDMAT$Variable),],hh_variable_importance,by="Variable",all.x=TRUE)
# geo_household_marginals<-geo_household_marginals[order(geo_household_marginals$Importance),]
# geo_household_marginals<-geo_household_marginals[geo_household_marginals$Importance!=0,!names(geo_household_marginals)%in%c("X","Definition","Description","Frequency","Importance")]
# geo_household_marginals$Categories[geo_household_marginals$Variable=="HH_ID"]<-1
# geo_household_marginals<-rbind(c("variable_names","variable_categories",1:length(unique(GIS$CSD))),geo_household_marginals)
# geo_household_marginals<-cbind(geo_household_marginals[,1:2],c("geo",rep("",nrow(geo_household_marginals)-1)),geo_household_marginals[,3:ncol(geo_household_marginals)])
# geo_household_marginals$Variable[-1]<-geo_household_marginals$Variable[-1]
# geo_household_marginals<-data.frame(t(geo_household_marginals))
# temp<-geo_household_marginals
# geo_household_marginals<-temp[,!((temp[1,]%in%c("HH_TOTINCAT") & temp[2,]%in%c(5:16)))]
# geo_household_marginals[-1:-3,(geo_household_marginals[1,]=="HH_TOTINCAT" & geo_household_marginals[2,]==1)]<-rowSums(apply(temp[-1:-3,temp[1,]=="HH_TOTINCAT" & temp[2,]%in%c(1:6)],2,as.numeric))
# geo_household_marginals[-1:-3,(geo_household_marginals[1,]=="HH_TOTINCAT" & geo_household_marginals[2,]==2)]<-rowSums(apply(temp[-1:-3,temp[1,]=="HH_TOTINCAT" & temp[2,]%in%c(7:11)],2,as.numeric))
# geo_household_marginals[-1:-3,(geo_household_marginals[1,]=="HH_TOTINCAT" & geo_household_marginals[2,]==3)]<-rowSums(apply(temp[-1:-3,temp[1,]=="HH_TOTINCAT" & temp[2,]%in%c(12:15)],2,as.numeric))
# geo_household_marginals[-1:-3,(geo_household_marginals[1,]=="HH_TOTINCAT" & geo_household_marginals[2,]==4)]<-as.numeric(temp[-1:-3,temp[1,]=="HH_TOTINCAT" & temp[2,]%in%c(16)])
# write_csv(geo_household_marginals,"Exported/A1/Montreal/Scenarios/S5-hCMA_CSD/geo_household_marginals.csv",col_names = FALSE)
# #geo_person_marginals
# geo_person_marginals<-merge(CSDMAT[grepl("PP_",CSDMAT$Variable),],pp_variable_importance,by="Variable",all.x=TRUE)
# geo_person_marginals<-geo_person_marginals[order(geo_person_marginals$Importance),]
# geo_person_marginals<-geo_person_marginals[geo_person_marginals$Importance!=0,!names(geo_person_marginals)%in%c("X","Definition","Description","Frequency","Importance")]
# geo_person_marginals$Categories[geo_person_marginals$Variable=="PP_ID"]<-1
# geo_person_marginals<-rbind(c("variable_names","variable_categories",1:length(unique(GIS$CSD))),geo_person_marginals)
# geo_person_marginals<-cbind(geo_person_marginals[,1:2],c("geo",rep("",nrow(geo_person_marginals)-1)),geo_person_marginals[,3:ncol(geo_person_marginals)])
# geo_person_marginals$Variable[-1]<-geo_person_marginals$Variable[-1]
# geo_person_marginals<-data.frame(t(geo_person_marginals))
# temp<-geo_person_marginals
# geo_person_marginals<-temp[,!((temp[1,]%in%c("PP_AGEGRP") & temp[2,]%in%c(4:13)))]
# geo_person_marginals[-1:-3,(geo_person_marginals[1,]=="PP_AGEGRP" & geo_person_marginals[2,]==1)]<-rowSums(apply(temp[-1:-3,temp[1,]=="PP_AGEGRP" & temp[2,]%in%c(1:2)],2,as.numeric))
# geo_person_marginals[-1:-3,(geo_person_marginals[1,]=="PP_AGEGRP" & geo_person_marginals[2,]==2)]<-rowSums(apply(temp[-1:-3,temp[1,]=="PP_AGEGRP" & temp[2,]%in%c(3:11)],2,as.numeric))
# geo_person_marginals[-1:-3,(geo_person_marginals[1,]=="PP_AGEGRP" & geo_person_marginals[2,]==3)]<-rowSums(apply(temp[-1:-3,temp[1,]=="PP_AGEGRP" & temp[2,]%in%c(12:13)],2,as.numeric))
# write_csv(geo_person_marginals,"Exported/A1/Montreal/Scenarios/S5-hCMA_CSD/geo_person_marginals.csv",col_names = FALSE)
# #household_sample
# write_csv(household_sample,"Exported/A1/Montreal/Scenarios/S5-hCMA_CSD/household_sample.csv",col_names = TRUE)
# #person_sample
# write_csv(person_sample,"Exported/A1/Montreal/Scenarios/S5-hCMA_CSD/person_sample.csv",col_names = TRUE)
# 
# ###S6-hCMA_ADA
# GIS<-GIS_CMA_ADA
# #region_geo_mapping
# geo<-1:length(unique(GIS$ADA))
# region<-1
# region_geo_mapping<-data.frame(region,geo)
# write.csv(region_geo_mapping,"Exported/A1/Montreal/Scenarios/S6-hCMA_ADA/region_geo_mapping.csv",row.names=FALSE)
# #geo_sample_mapping
# geo<-1:length(unique(GIS$ADA))
# sample_geo<-1
# geo_sample_mapping<-data.frame(geo,sample_geo)
# write.csv(geo_sample_mapping,"Exported/A1/Montreal/Scenarios/S6-hCMA_ADA/geo_sample_mapping.csv",row.names=FALSE)
# #region_sample_mapping
# region<-1
# sample_geo<--1
# region_sample_mapping<-data.frame(region,sample_geo)
# write.csv(region_sample_mapping,"Exported/A1/Montreal/Scenarios/S6-hCMA_ADA/region_sample_mapping.csv",row.names=FALSE)
# #region_household_marginals
# region_household_marginals<-merge(CMAMAT[grepl("HH_",CMAMAT$Variable),],hh_variable_importance,by="Variable",all.x=TRUE)
# region_household_marginals<-region_household_marginals[order(region_household_marginals$Importance),]
# region_household_marginals<-region_household_marginals[region_household_marginals$Importance!=0,!names(region_household_marginals)%in%c("X","Definition","Description","Frequency","Importance")]
# region_household_marginals$Categories[region_household_marginals$Variable=="HH_ID"]<-1
# region_household_marginals<-rbind(c("variable_names","variable_categories",1:length(unique(GIS$CMA))),region_household_marginals)
# region_household_marginals<-cbind(region_household_marginals[,1:2],c("region",rep("",nrow(region_household_marginals)-1)),region_household_marginals[,3:ncol(region_household_marginals)])
# region_household_marginals$Variable[-1]<-paste("r",region_household_marginals$Variable[-1],sep="")
# region_household_marginals<-data.frame(t(region_household_marginals))
# temp<-region_household_marginals
# region_household_marginals<-temp[,!((temp[1,]%in%c("rHH_TOTINCAT") & temp[2,]%in%c(5:16)))]
# region_household_marginals[-1:-3,(region_household_marginals[1,]=="rHH_TOTINCAT" & region_household_marginals[2,]==1)]<-sum(apply(temp[-1:-3,temp[1,]=="rHH_TOTINCAT" & temp[2,]%in%c(1:6)],2,as.numeric))
# region_household_marginals[-1:-3,(region_household_marginals[1,]=="rHH_TOTINCAT" & region_household_marginals[2,]==2)]<-sum(apply(temp[-1:-3,temp[1,]=="rHH_TOTINCAT" & temp[2,]%in%c(7:11)],2,as.numeric))
# region_household_marginals[-1:-3,(region_household_marginals[1,]=="rHH_TOTINCAT" & region_household_marginals[2,]==3)]<-sum(apply(temp[-1:-3,temp[1,]=="rHH_TOTINCAT" & temp[2,]%in%c(12:15)],2,as.numeric))
# region_household_marginals[-1:-3,(region_household_marginals[1,]=="rHH_TOTINCAT" & region_household_marginals[2,]==4)]<-as.numeric(temp[-1:-3,temp[1,]=="rHH_TOTINCAT" & temp[2,]%in%c(16)])
# write_csv(region_household_marginals,"Exported/A1/Montreal/Scenarios/S6-hCMA_ADA/region_household_marginals.csv",col_names = FALSE)
# #region_person_marginals
# region_person_marginals<-merge(CMAMAT[grepl("PP_",CMAMAT$Variable),],pp_variable_importance,by="Variable",all.x=TRUE)
# region_person_marginals<-region_person_marginals[order(region_person_marginals$Importance),]
# region_person_marginals<-region_person_marginals[region_person_marginals$Importance!=0,!names(region_person_marginals)%in%c("X","Definition","Description","Frequency","Importance")]
# region_person_marginals$Categories[region_person_marginals$Variable=="PP_ID"]<-1
# region_person_marginals<-rbind(c("variable_names","variable_categories",1:length(unique(GIS$CMA))),region_person_marginals)
# region_person_marginals<-cbind(region_person_marginals[,1:2],c("region",rep("",nrow(region_person_marginals)-1)),region_person_marginals[,3:ncol(region_person_marginals)])
# region_person_marginals$Variable[-1]<-paste("r",region_person_marginals$Variable[-1],sep="")
# region_person_marginals<-data.frame(t(region_person_marginals))
# temp<-region_person_marginals
# region_person_marginals<-temp[,!((temp[1,]%in%c("rPP_AGEGRP") & temp[2,]%in%c(4:13)))]
# region_person_marginals[-1:-3,(region_person_marginals[1,]=="rPP_AGEGRP" & region_person_marginals[2,]==1)]<-sum(apply(temp[-1:-3,temp[1,]=="rPP_AGEGRP" & temp[2,]%in%c(1:2)],2,as.numeric))
# region_person_marginals[-1:-3,(region_person_marginals[1,]=="rPP_AGEGRP" & region_person_marginals[2,]==2)]<-sum(apply(temp[-1:-3,temp[1,]=="rPP_AGEGRP" & temp[2,]%in%c(3:11)],2,as.numeric))
# region_person_marginals[-1:-3,(region_person_marginals[1,]=="rPP_AGEGRP" & region_person_marginals[2,]==3)]<-sum(apply(temp[-1:-3,temp[1,]=="rPP_AGEGRP" & temp[2,]%in%c(12:13)],2,as.numeric))
# write_csv(region_person_marginals,"Exported/A1/Montreal/Scenarios/S6-hCMA_ADA/region_person_marginals.csv",col_names = FALSE)
# #geo_household_marginals
# geo_household_marginals<-merge(ADAMAT[grepl("HH_",ADAMAT$Variable),],hh_variable_importance,by="Variable",all.x=TRUE)
# geo_household_marginals<-geo_household_marginals[order(geo_household_marginals$Importance),]
# geo_household_marginals<-geo_household_marginals[geo_household_marginals$Importance!=0,!names(geo_household_marginals)%in%c("X","Definition","Description","Frequency","Importance")]
# geo_household_marginals$Categories[geo_household_marginals$Variable=="HH_ID"]<-1
# geo_household_marginals<-rbind(c("variable_names","variable_categories",1:length(unique(GIS$ADA))),geo_household_marginals)
# geo_household_marginals<-cbind(geo_household_marginals[,1:2],c("geo",rep("",nrow(geo_household_marginals)-1)),geo_household_marginals[,3:ncol(geo_household_marginals)])
# geo_household_marginals$Variable[-1]<-geo_household_marginals$Variable[-1]
# geo_household_marginals<-data.frame(t(geo_household_marginals))
# temp<-geo_household_marginals
# geo_household_marginals<-temp[,!((temp[1,]%in%c("HH_TOTINCAT") & temp[2,]%in%c(5:16)))]
# geo_household_marginals[-1:-3,(geo_household_marginals[1,]=="HH_TOTINCAT" & geo_household_marginals[2,]==1)]<-rowSums(apply(temp[-1:-3,temp[1,]=="HH_TOTINCAT" & temp[2,]%in%c(1:6)],2,as.numeric))
# geo_household_marginals[-1:-3,(geo_household_marginals[1,]=="HH_TOTINCAT" & geo_household_marginals[2,]==2)]<-rowSums(apply(temp[-1:-3,temp[1,]=="HH_TOTINCAT" & temp[2,]%in%c(7:11)],2,as.numeric))
# geo_household_marginals[-1:-3,(geo_household_marginals[1,]=="HH_TOTINCAT" & geo_household_marginals[2,]==3)]<-rowSums(apply(temp[-1:-3,temp[1,]=="HH_TOTINCAT" & temp[2,]%in%c(12:15)],2,as.numeric))
# geo_household_marginals[-1:-3,(geo_household_marginals[1,]=="HH_TOTINCAT" & geo_household_marginals[2,]==4)]<-as.numeric(temp[-1:-3,temp[1,]=="HH_TOTINCAT" & temp[2,]%in%c(16)])
# write_csv(geo_household_marginals,"Exported/A1/Montreal/Scenarios/S6-hCMA_ADA/geo_household_marginals.csv",col_names = FALSE)
# #geo_person_marginals
# geo_person_marginals<-merge(ADAMAT[grepl("PP_",ADAMAT$Variable),],pp_variable_importance,by="Variable",all.x=TRUE)
# geo_person_marginals<-geo_person_marginals[order(geo_person_marginals$Importance),]
# geo_person_marginals<-geo_person_marginals[geo_person_marginals$Importance!=0,!names(geo_person_marginals)%in%c("X","Definition","Description","Frequency","Importance")]
# geo_person_marginals$Categories[geo_person_marginals$Variable=="PP_ID"]<-1
# geo_person_marginals<-rbind(c("variable_names","variable_categories",1:length(unique(GIS$ADA))),geo_person_marginals)
# geo_person_marginals<-cbind(geo_person_marginals[,1:2],c("geo",rep("",nrow(geo_person_marginals)-1)),geo_person_marginals[,3:ncol(geo_person_marginals)])
# geo_person_marginals$Variable[-1]<-geo_person_marginals$Variable[-1]
# geo_person_marginals<-data.frame(t(geo_person_marginals))
# temp<-geo_person_marginals
# geo_person_marginals<-temp[,!((temp[1,]%in%c("PP_AGEGRP") & temp[2,]%in%c(4:13)))]
# geo_person_marginals[-1:-3,(geo_person_marginals[1,]=="PP_AGEGRP" & geo_person_marginals[2,]==1)]<-rowSums(apply(temp[-1:-3,temp[1,]=="PP_AGEGRP" & temp[2,]%in%c(1:2)],2,as.numeric))
# geo_person_marginals[-1:-3,(geo_person_marginals[1,]=="PP_AGEGRP" & geo_person_marginals[2,]==2)]<-rowSums(apply(temp[-1:-3,temp[1,]=="PP_AGEGRP" & temp[2,]%in%c(3:11)],2,as.numeric))
# geo_person_marginals[-1:-3,(geo_person_marginals[1,]=="PP_AGEGRP" & geo_person_marginals[2,]==3)]<-rowSums(apply(temp[-1:-3,temp[1,]=="PP_AGEGRP" & temp[2,]%in%c(12:13)],2,as.numeric))
# write_csv(geo_person_marginals,"Exported/A1/Montreal/Scenarios/S6-hCMA_ADA/geo_person_marginals.csv",col_names = FALSE)
# #household_sample
# write_csv(household_sample,"Exported/A1/Montreal/Scenarios/S6-hCMA_ADA/household_sample.csv",col_names = TRUE)
# #person_sample
# write_csv(person_sample,"Exported/A1/Montreal/Scenarios/S6-hCMA_ADA/person_sample.csv",col_names = TRUE)
# 
# ###S7-hCMA_CT
# GIS<-GIS_CMA_CT
# #region_geo_mapping
# geo<-1:length(unique(GIS$CT))
# region<-1
# region_geo_mapping<-data.frame(region,geo)
# write.csv(region_geo_mapping,"Exported/A1/Montreal/Scenarios/S7-hCMA_CT/region_geo_mapping.csv",row.names=FALSE)
# #geo_sample_mapping
# geo<-1:length(unique(GIS$CT))
# sample_geo<-1
# geo_sample_mapping<-data.frame(geo,sample_geo)
# write.csv(geo_sample_mapping,"Exported/A1/Montreal/Scenarios/S7-hCMA_CT/geo_sample_mapping.csv",row.names=FALSE)
# #region_sample_mapping
# region<-1
# sample_geo<--1
# region_sample_mapping<-data.frame(region,sample_geo)
# write.csv(region_sample_mapping,"Exported/A1/Montreal/Scenarios/S7-hCMA_CT/region_sample_mapping.csv",row.names=FALSE)
# #region_household_marginals
# region_household_marginals<-merge(CMAMAT[grepl("HH_",CMAMAT$Variable),],hh_variable_importance,by="Variable",all.x=TRUE)
# region_household_marginals<-region_household_marginals[order(region_household_marginals$Importance),]
# region_household_marginals<-region_household_marginals[region_household_marginals$Importance!=0,!names(region_household_marginals)%in%c("X","Definition","Description","Frequency","Importance")]
# region_household_marginals$Categories[region_household_marginals$Variable=="HH_ID"]<-1
# region_household_marginals<-rbind(c("variable_names","variable_categories",1:length(unique(GIS$CMA))),region_household_marginals)
# region_household_marginals<-cbind(region_household_marginals[,1:2],c("region",rep("",nrow(region_household_marginals)-1)),region_household_marginals[,3:ncol(region_household_marginals)])
# region_household_marginals$Variable[-1]<-paste("r",region_household_marginals$Variable[-1],sep="")
# region_household_marginals<-data.frame(t(region_household_marginals))
# temp<-region_household_marginals
# region_household_marginals<-temp[,!((temp[1,]%in%c("rHH_TOTINCAT") & temp[2,]%in%c(5:16)))]
# region_household_marginals[-1:-3,(region_household_marginals[1,]=="rHH_TOTINCAT" & region_household_marginals[2,]==1)]<-sum(apply(temp[-1:-3,temp[1,]=="rHH_TOTINCAT" & temp[2,]%in%c(1:6)],2,as.numeric))
# region_household_marginals[-1:-3,(region_household_marginals[1,]=="rHH_TOTINCAT" & region_household_marginals[2,]==2)]<-sum(apply(temp[-1:-3,temp[1,]=="rHH_TOTINCAT" & temp[2,]%in%c(7:11)],2,as.numeric))
# region_household_marginals[-1:-3,(region_household_marginals[1,]=="rHH_TOTINCAT" & region_household_marginals[2,]==3)]<-sum(apply(temp[-1:-3,temp[1,]=="rHH_TOTINCAT" & temp[2,]%in%c(12:15)],2,as.numeric))
# region_household_marginals[-1:-3,(region_household_marginals[1,]=="rHH_TOTINCAT" & region_household_marginals[2,]==4)]<-as.numeric(temp[-1:-3,temp[1,]=="rHH_TOTINCAT" & temp[2,]%in%c(16)])
# write_csv(region_household_marginals,"Exported/A1/Montreal/Scenarios/S7-hCMA_CT/region_household_marginals.csv",col_names = FALSE)
# #region_person_marginals
# region_person_marginals<-merge(CMAMAT[grepl("PP_",CMAMAT$Variable),],pp_variable_importance,by="Variable",all.x=TRUE)
# region_person_marginals<-region_person_marginals[order(region_person_marginals$Importance),]
# region_person_marginals<-region_person_marginals[region_person_marginals$Importance!=0,!names(region_person_marginals)%in%c("X","Definition","Description","Frequency","Importance")]
# region_person_marginals$Categories[region_person_marginals$Variable=="PP_ID"]<-1
# region_person_marginals<-rbind(c("variable_names","variable_categories",1:length(unique(GIS$CMA))),region_person_marginals)
# region_person_marginals<-cbind(region_person_marginals[,1:2],c("region",rep("",nrow(region_person_marginals)-1)),region_person_marginals[,3:ncol(region_person_marginals)])
# region_person_marginals$Variable[-1]<-paste("r",region_person_marginals$Variable[-1],sep="")
# region_person_marginals<-data.frame(t(region_person_marginals))
# temp<-region_person_marginals
# region_person_marginals<-temp[,!((temp[1,]%in%c("rPP_AGEGRP") & temp[2,]%in%c(4:13)))]
# region_person_marginals[-1:-3,(region_person_marginals[1,]=="rPP_AGEGRP" & region_person_marginals[2,]==1)]<-sum(apply(temp[-1:-3,temp[1,]=="rPP_AGEGRP" & temp[2,]%in%c(1:2)],2,as.numeric))
# region_person_marginals[-1:-3,(region_person_marginals[1,]=="rPP_AGEGRP" & region_person_marginals[2,]==2)]<-sum(apply(temp[-1:-3,temp[1,]=="rPP_AGEGRP" & temp[2,]%in%c(3:11)],2,as.numeric))
# region_person_marginals[-1:-3,(region_person_marginals[1,]=="rPP_AGEGRP" & region_person_marginals[2,]==3)]<-sum(apply(temp[-1:-3,temp[1,]=="rPP_AGEGRP" & temp[2,]%in%c(12:13)],2,as.numeric))
# write_csv(region_person_marginals,"Exported/A1/Montreal/Scenarios/S7-hCMA_CT/region_person_marginals.csv",col_names = FALSE)
# #geo_household_marginals
# geo_household_marginals<-merge(CTMAT[grepl("HH_",CTMAT$Variable),],hh_variable_importance,by="Variable",all.x=TRUE)
# geo_household_marginals<-geo_household_marginals[order(geo_household_marginals$Importance),]
# geo_household_marginals<-geo_household_marginals[geo_household_marginals$Importance!=0,!names(geo_household_marginals)%in%c("X","Definition","Description","Frequency","Importance")]
# geo_household_marginals$Categories[geo_household_marginals$Variable=="HH_ID"]<-1
# geo_household_marginals<-rbind(c("variable_names","variable_categories",1:length(unique(GIS$CT))),geo_household_marginals)
# geo_household_marginals<-cbind(geo_household_marginals[,1:2],c("geo",rep("",nrow(geo_household_marginals)-1)),geo_household_marginals[,3:ncol(geo_household_marginals)])
# geo_household_marginals$Variable[-1]<-geo_household_marginals$Variable[-1]
# geo_household_marginals<-data.frame(t(geo_household_marginals))
# temp<-geo_household_marginals
# geo_household_marginals<-temp[,!((temp[1,]%in%c("HH_TOTINCAT") & temp[2,]%in%c(5:16)))]
# geo_household_marginals[-1:-3,(geo_household_marginals[1,]=="HH_TOTINCAT" & geo_household_marginals[2,]==1)]<-rowSums(apply(temp[-1:-3,temp[1,]=="HH_TOTINCAT" & temp[2,]%in%c(1:6)],2,as.numeric))
# geo_household_marginals[-1:-3,(geo_household_marginals[1,]=="HH_TOTINCAT" & geo_household_marginals[2,]==2)]<-rowSums(apply(temp[-1:-3,temp[1,]=="HH_TOTINCAT" & temp[2,]%in%c(7:11)],2,as.numeric))
# geo_household_marginals[-1:-3,(geo_household_marginals[1,]=="HH_TOTINCAT" & geo_household_marginals[2,]==3)]<-rowSums(apply(temp[-1:-3,temp[1,]=="HH_TOTINCAT" & temp[2,]%in%c(12:15)],2,as.numeric))
# geo_household_marginals[-1:-3,(geo_household_marginals[1,]=="HH_TOTINCAT" & geo_household_marginals[2,]==4)]<-as.numeric(temp[-1:-3,temp[1,]=="HH_TOTINCAT" & temp[2,]%in%c(16)])
# write_csv(geo_household_marginals,"Exported/A1/Montreal/Scenarios/S7-hCMA_CT/geo_household_marginals.csv",col_names = FALSE)
# #geo_person_marginals
# geo_person_marginals<-merge(CTMAT[grepl("PP_",CTMAT$Variable),],pp_variable_importance,by="Variable",all.x=TRUE)
# geo_person_marginals<-geo_person_marginals[order(geo_person_marginals$Importance),]
# geo_person_marginals<-geo_person_marginals[geo_person_marginals$Importance!=0,!names(geo_person_marginals)%in%c("X","Definition","Description","Frequency","Importance")]
# geo_person_marginals$Categories[geo_person_marginals$Variable=="PP_ID"]<-1
# geo_person_marginals<-rbind(c("variable_names","variable_categories",1:length(unique(GIS$CT))),geo_person_marginals)
# geo_person_marginals<-cbind(geo_person_marginals[,1:2],c("geo",rep("",nrow(geo_person_marginals)-1)),geo_person_marginals[,3:ncol(geo_person_marginals)])
# geo_person_marginals$Variable[-1]<-geo_person_marginals$Variable[-1]
# geo_person_marginals<-data.frame(t(geo_person_marginals))
# temp<-geo_person_marginals
# geo_person_marginals<-temp[,!((temp[1,]%in%c("PP_AGEGRP") & temp[2,]%in%c(4:13)))]
# geo_person_marginals[-1:-3,(geo_person_marginals[1,]=="PP_AGEGRP" & geo_person_marginals[2,]==1)]<-rowSums(apply(temp[-1:-3,temp[1,]=="PP_AGEGRP" & temp[2,]%in%c(1:2)],2,as.numeric))
# geo_person_marginals[-1:-3,(geo_person_marginals[1,]=="PP_AGEGRP" & geo_person_marginals[2,]==2)]<-rowSums(apply(temp[-1:-3,temp[1,]=="PP_AGEGRP" & temp[2,]%in%c(3:11)],2,as.numeric))
# geo_person_marginals[-1:-3,(geo_person_marginals[1,]=="PP_AGEGRP" & geo_person_marginals[2,]==3)]<-rowSums(apply(temp[-1:-3,temp[1,]=="PP_AGEGRP" & temp[2,]%in%c(12:13)],2,as.numeric))
# write_csv(geo_person_marginals,"Exported/A1/Montreal/Scenarios/S7-hCMA_CT/geo_person_marginals.csv",col_names = FALSE)
# #household_sample
# write_csv(household_sample,"Exported/A1/Montreal/Scenarios/S7-hCMA_CT/household_sample.csv",col_names = TRUE)
# #person_sample
# write_csv(person_sample,"Exported/A1/Montreal/Scenarios/S7-hCMA_CT/person_sample.csv",col_names = TRUE)
# 
# ###S8-hCMA_DA
# GIS<-GIS_CMA_DA
# #region_geo_mapping
# geo<-1:length(unique(GIS$DA))
# region<-1
# region_geo_mapping<-data.frame(region,geo)
# write.csv(region_geo_mapping,"Exported/A1/Montreal/Scenarios/S8-hCMA_DA/region_geo_mapping.csv",row.names=FALSE)
# #geo_sample_mapping
# geo<-1:length(unique(GIS$DA))
# sample_geo<-1
# geo_sample_mapping<-data.frame(geo,sample_geo)
# write.csv(geo_sample_mapping,"Exported/A1/Montreal/Scenarios/S8-hCMA_DA/geo_sample_mapping.csv",row.names=FALSE)
# #region_sample_mapping
# region<-1
# sample_geo<--1
# region_sample_mapping<-data.frame(region,sample_geo)
# write.csv(region_sample_mapping,"Exported/A1/Montreal/Scenarios/S8-hCMA_DA/region_sample_mapping.csv",row.names=FALSE)
# #region_household_marginals
# region_household_marginals<-merge(CMAMAT[grepl("HH_",CMAMAT$Variable),],hh_variable_importance,by="Variable",all.x=TRUE)
# region_household_marginals<-region_household_marginals[order(region_household_marginals$Importance),]
# region_household_marginals<-region_household_marginals[region_household_marginals$Importance!=0,!names(region_household_marginals)%in%c("X","Definition","Description","Frequency","Importance")]
# region_household_marginals$Categories[region_household_marginals$Variable=="HH_ID"]<-1
# region_household_marginals<-rbind(c("variable_names","variable_categories",1:length(unique(GIS$CMA))),region_household_marginals)
# region_household_marginals<-cbind(region_household_marginals[,1:2],c("region",rep("",nrow(region_household_marginals)-1)),region_household_marginals[,3:ncol(region_household_marginals)])
# region_household_marginals$Variable[-1]<-paste("r",region_household_marginals$Variable[-1],sep="")
# region_household_marginals<-data.frame(t(region_household_marginals))
# temp<-region_household_marginals
# region_household_marginals<-temp[,!((temp[1,]%in%c("rHH_TOTINCAT") & temp[2,]%in%c(5:16)))]
# region_household_marginals[-1:-3,(region_household_marginals[1,]=="rHH_TOTINCAT" & region_household_marginals[2,]==1)]<-sum(apply(temp[-1:-3,temp[1,]=="rHH_TOTINCAT" & temp[2,]%in%c(1:6)],2,as.numeric))
# region_household_marginals[-1:-3,(region_household_marginals[1,]=="rHH_TOTINCAT" & region_household_marginals[2,]==2)]<-sum(apply(temp[-1:-3,temp[1,]=="rHH_TOTINCAT" & temp[2,]%in%c(7:11)],2,as.numeric))
# region_household_marginals[-1:-3,(region_household_marginals[1,]=="rHH_TOTINCAT" & region_household_marginals[2,]==3)]<-sum(apply(temp[-1:-3,temp[1,]=="rHH_TOTINCAT" & temp[2,]%in%c(12:15)],2,as.numeric))
# region_household_marginals[-1:-3,(region_household_marginals[1,]=="rHH_TOTINCAT" & region_household_marginals[2,]==4)]<-as.numeric(temp[-1:-3,temp[1,]=="rHH_TOTINCAT" & temp[2,]%in%c(16)])
# write_csv(region_household_marginals,"Exported/A1/Montreal/Scenarios/S8-hCMA_DA/region_household_marginals.csv",col_names = FALSE)
# #region_person_marginals
# region_person_marginals<-merge(CMAMAT[grepl("PP_",CMAMAT$Variable),],pp_variable_importance,by="Variable",all.x=TRUE)
# region_person_marginals<-region_person_marginals[order(region_person_marginals$Importance),]
# region_person_marginals<-region_person_marginals[region_person_marginals$Importance!=0,!names(region_person_marginals)%in%c("X","Definition","Description","Frequency","Importance")]
# region_person_marginals$Categories[region_person_marginals$Variable=="PP_ID"]<-1
# region_person_marginals<-rbind(c("variable_names","variable_categories",1:length(unique(GIS$CMA))),region_person_marginals)
# region_person_marginals<-cbind(region_person_marginals[,1:2],c("region",rep("",nrow(region_person_marginals)-1)),region_person_marginals[,3:ncol(region_person_marginals)])
# region_person_marginals$Variable[-1]<-paste("r",region_person_marginals$Variable[-1],sep="")
# region_person_marginals<-data.frame(t(region_person_marginals))
# temp<-region_person_marginals
# region_person_marginals<-temp[,!((temp[1,]%in%c("rPP_AGEGRP") & temp[2,]%in%c(4:13)))]
# region_person_marginals[-1:-3,(region_person_marginals[1,]=="rPP_AGEGRP" & region_person_marginals[2,]==1)]<-sum(apply(temp[-1:-3,temp[1,]=="rPP_AGEGRP" & temp[2,]%in%c(1:2)],2,as.numeric))
# region_person_marginals[-1:-3,(region_person_marginals[1,]=="rPP_AGEGRP" & region_person_marginals[2,]==2)]<-sum(apply(temp[-1:-3,temp[1,]=="rPP_AGEGRP" & temp[2,]%in%c(3:11)],2,as.numeric))
# region_person_marginals[-1:-3,(region_person_marginals[1,]=="rPP_AGEGRP" & region_person_marginals[2,]==3)]<-sum(apply(temp[-1:-3,temp[1,]=="rPP_AGEGRP" & temp[2,]%in%c(12:13)],2,as.numeric))
# write_csv(region_person_marginals,"Exported/A1/Montreal/Scenarios/S8-hCMA_DA/region_person_marginals.csv",col_names = FALSE)
# #geo_household_marginals
# geo_household_marginals<-merge(DAMAT[grepl("HH_",DAMAT$Variable),],hh_variable_importance,by="Variable",all.x=TRUE)
# geo_household_marginals<-geo_household_marginals[order(geo_household_marginals$Importance),]
# geo_household_marginals<-geo_household_marginals[geo_household_marginals$Importance!=0,!names(geo_household_marginals)%in%c("X","Definition","Description","Frequency","Importance")]
# geo_household_marginals$Categories[geo_household_marginals$Variable=="HH_ID"]<-1
# geo_household_marginals<-rbind(c("variable_names","variable_categories",1:length(unique(GIS$DA))),geo_household_marginals)
# geo_household_marginals<-cbind(geo_household_marginals[,1:2],c("geo",rep("",nrow(geo_household_marginals)-1)),geo_household_marginals[,3:ncol(geo_household_marginals)])
# geo_household_marginals$Variable[-1]<-geo_household_marginals$Variable[-1]
# geo_household_marginals<-data.frame(t(geo_household_marginals))
# temp<-geo_household_marginals
# geo_household_marginals<-temp[,!((temp[1,]%in%c("HH_TOTINCAT") & temp[2,]%in%c(5:16)))]
# geo_household_marginals[-1:-3,(geo_household_marginals[1,]=="HH_TOTINCAT" & geo_household_marginals[2,]==1)]<-rowSums(apply(temp[-1:-3,temp[1,]=="HH_TOTINCAT" & temp[2,]%in%c(1:6)],2,as.numeric))
# geo_household_marginals[-1:-3,(geo_household_marginals[1,]=="HH_TOTINCAT" & geo_household_marginals[2,]==2)]<-rowSums(apply(temp[-1:-3,temp[1,]=="HH_TOTINCAT" & temp[2,]%in%c(7:11)],2,as.numeric))
# geo_household_marginals[-1:-3,(geo_household_marginals[1,]=="HH_TOTINCAT" & geo_household_marginals[2,]==3)]<-rowSums(apply(temp[-1:-3,temp[1,]=="HH_TOTINCAT" & temp[2,]%in%c(12:15)],2,as.numeric))
# geo_household_marginals[-1:-3,(geo_household_marginals[1,]=="HH_TOTINCAT" & geo_household_marginals[2,]==4)]<-as.numeric(temp[-1:-3,temp[1,]=="HH_TOTINCAT" & temp[2,]%in%c(16)])
# write_csv(geo_household_marginals,"Exported/A1/Montreal/Scenarios/S8-hCMA_DA/geo_household_marginals.csv",col_names = FALSE)
# #geo_person_marginals
# geo_person_marginals<-merge(DAMAT[grepl("PP_",DAMAT$Variable),],pp_variable_importance,by="Variable",all.x=TRUE)
# geo_person_marginals<-geo_person_marginals[order(geo_person_marginals$Importance),]
# geo_person_marginals<-geo_person_marginals[geo_person_marginals$Importance!=0,!names(geo_person_marginals)%in%c("X","Definition","Description","Frequency","Importance")]
# geo_person_marginals$Categories[geo_person_marginals$Variable=="PP_ID"]<-1
# geo_person_marginals<-rbind(c("variable_names","variable_categories",1:length(unique(GIS$DA))),geo_person_marginals)
# geo_person_marginals<-cbind(geo_person_marginals[,1:2],c("geo",rep("",nrow(geo_person_marginals)-1)),geo_person_marginals[,3:ncol(geo_person_marginals)])
# geo_person_marginals$Variable[-1]<-geo_person_marginals$Variable[-1]
# geo_person_marginals<-data.frame(t(geo_person_marginals))
# temp<-geo_person_marginals
# geo_person_marginals<-temp[,!((temp[1,]%in%c("PP_AGEGRP") & temp[2,]%in%c(4:13)))]
# geo_person_marginals[-1:-3,(geo_person_marginals[1,]=="PP_AGEGRP" & geo_person_marginals[2,]==1)]<-rowSums(apply(temp[-1:-3,temp[1,]=="PP_AGEGRP" & temp[2,]%in%c(1:2)],2,as.numeric))
# geo_person_marginals[-1:-3,(geo_person_marginals[1,]=="PP_AGEGRP" & geo_person_marginals[2,]==2)]<-rowSums(apply(temp[-1:-3,temp[1,]=="PP_AGEGRP" & temp[2,]%in%c(3:11)],2,as.numeric))
# geo_person_marginals[-1:-3,(geo_person_marginals[1,]=="PP_AGEGRP" & geo_person_marginals[2,]==3)]<-rowSums(apply(temp[-1:-3,temp[1,]=="PP_AGEGRP" & temp[2,]%in%c(12:13)],2,as.numeric))
# write_csv(geo_person_marginals,"Exported/A1/Montreal/Scenarios/S8-hCMA_DA/geo_person_marginals.csv",col_names = FALSE)
# #household_sample
# write_csv(household_sample,"Exported/A1/Montreal/Scenarios/S8-hCMA_DA/household_sample.csv",col_names = TRUE)
# #person_sample
# write_csv(person_sample,"Exported/A1/Montreal/Scenarios/S8-hCMA_DA/person_sample.csv",col_names = TRUE)







# ###S1 - bCMA_CT
# #region_geo_mapping
# geo<-1:length(unique(GIS$CT))
# region<-1
# region_geo_mapping<-data.frame(region,geo)
# write.csv(region_geo_mapping,"Exported/A1/Montreal/Scenarios/S1 - bCMA_CT/region_geo_mapping.csv",row.names=FALSE)
# #geo_sample_mapping
# geo<-1:length(unique(GIS$CT))
# sample_geo<-1
# geo_sample_mapping<-data.frame(geo,sample_geo)
# write.csv(geo_sample_mapping,"Exported/A1/Montreal/Scenarios/S1 - bCMA_CT/geo_sample_mapping.csv",row.names=FALSE)
# #region_sample_mapping
# region<-1
# sample_geo<--1
# region_sample_mapping<-data.frame(region,sample_geo)
# write.csv(region_sample_mapping,"Exported/A1/Montreal/Scenarios/S1 - bCMA_CT/region_sample_mapping.csv",row.names=FALSE)
# #region_household_marginals
# region_household_marginals<-merge(CMAMAT[grepl("HH_",CMAMAT$Variable),],hh_variable_importance,by="Variable",all.x=TRUE)
# region_household_marginals<-region_household_marginals[order(region_household_marginals$Importance),]
# region_household_marginals<-region_household_marginals[region_household_marginals$Importance!=0,!names(region_household_marginals)%in%c("X","Definition","Description","Frequency","Importance")]
# region_household_marginals$Categories[region_household_marginals$Variable=="HH_ID"]<-1
# region_household_marginals<-rbind(c("variable_names","variable_categories",1:length(unique(GIS$CMA))),region_household_marginals)
# region_household_marginals<-cbind(region_household_marginals[,1:2],c("region",rep("",nrow(region_household_marginals)-1)),region_household_marginals[,3:ncol(region_household_marginals)])
# region_household_marginals$Variable[-1]<-paste("r",region_household_marginals$Variable[-1],sep="")
# region_household_marginals<-data.frame(t(region_household_marginals))
# temp<-region_household_marginals
# region_household_marginals<-temp[,!((temp[1,]%in%c("rHH_TOTINCAT") & temp[2,]%in%c(5:16)))]
# region_household_marginals[-1:-3,(region_household_marginals[1,]=="rHH_TOTINCAT" & region_household_marginals[2,]==1)]<-sum(apply(temp[-1:-3,temp[1,]=="rHH_TOTINCAT" & temp[2,]%in%c(1:6)],2,as.numeric))
# region_household_marginals[-1:-3,(region_household_marginals[1,]=="rHH_TOTINCAT" & region_household_marginals[2,]==2)]<-sum(apply(temp[-1:-3,temp[1,]=="rHH_TOTINCAT" & temp[2,]%in%c(7:11)],2,as.numeric))
# region_household_marginals[-1:-3,(region_household_marginals[1,]=="rHH_TOTINCAT" & region_household_marginals[2,]==3)]<-sum(apply(temp[-1:-3,temp[1,]=="rHH_TOTINCAT" & temp[2,]%in%c(12:15)],2,as.numeric))
# region_household_marginals[-1:-3,(region_household_marginals[1,]=="rHH_TOTINCAT" & region_household_marginals[2,]==4)]<-as.numeric(temp[-1:-3,temp[1,]=="rHH_TOTINCAT" & temp[2,]%in%c(16)])
# write_csv(region_household_marginals,"Exported/A1/Montreal/Scenarios/S1 - bCMA_CT/region_household_marginals.csv",col_names = FALSE)
# #region_person_marginals
# region_person_marginals<-merge(CMAMAT[grepl("PP_",CMAMAT$Variable),],pp_variable_importance,by="Variable",all.x=TRUE)
# region_person_marginals<-region_person_marginals[order(region_person_marginals$Importance),]
# region_person_marginals<-region_person_marginals[region_person_marginals$Importance!=0,!names(region_person_marginals)%in%c("X","Definition","Description","Frequency","Importance")]
# region_person_marginals$Categories[region_person_marginals$Variable=="PP_ID"]<-1
# region_person_marginals<-rbind(c("variable_names","variable_categories",1:length(unique(GIS$CMA))),region_person_marginals)
# region_person_marginals<-cbind(region_person_marginals[,1:2],c("region",rep("",nrow(region_person_marginals)-1)),region_person_marginals[,3:ncol(region_person_marginals)])
# region_person_marginals$Variable[-1]<-paste("r",region_person_marginals$Variable[-1],sep="")
# region_person_marginals<-data.frame(t(region_person_marginals))
# temp<-region_person_marginals
# region_person_marginals<-temp[,!((temp[1,]%in%c("rPP_AGEGRP") & temp[2,]%in%c(4:13)))]
# region_person_marginals[-1:-3,(region_person_marginals[1,]=="rPP_AGEGRP" & region_person_marginals[2,]==1)]<-sum(apply(temp[-1:-3,temp[1,]=="rPP_AGEGRP" & temp[2,]%in%c(1:2)],2,as.numeric))
# region_person_marginals[-1:-3,(region_person_marginals[1,]=="rPP_AGEGRP" & region_person_marginals[2,]==2)]<-sum(apply(temp[-1:-3,temp[1,]=="rPP_AGEGRP" & temp[2,]%in%c(3:11)],2,as.numeric))
# region_person_marginals[-1:-3,(region_person_marginals[1,]=="rPP_AGEGRP" & region_person_marginals[2,]==3)]<-sum(apply(temp[-1:-3,temp[1,]=="rPP_AGEGRP" & temp[2,]%in%c(12:13)],2,as.numeric))
# write_csv(region_person_marginals,"Exported/A1/Montreal/Scenarios/S1 - bCMA_CT/region_person_marginals.csv",col_names = FALSE)
# #geo_household_marginals
# geo_household_marginals<-merge(CTMAT[grepl("HH_",CTMAT$Variable),],hh_variable_importance,by="Variable",all.x=TRUE)
# geo_household_marginals<-geo_household_marginals[order(geo_household_marginals$Importance),]
# geo_household_marginals<-geo_household_marginals[geo_household_marginals$Importance!=0,!names(geo_household_marginals)%in%c("X","Definition","Description","Frequency","Importance")]
# geo_household_marginals$Categories[geo_household_marginals$Variable=="HH_ID"]<-1
# geo_household_marginals<-rbind(c("variable_names","variable_categories",1:length(unique(GIS$CT))),geo_household_marginals)
# geo_household_marginals<-cbind(geo_household_marginals[,1:2],c("geo",rep("",nrow(geo_household_marginals)-1)),geo_household_marginals[,3:ncol(geo_household_marginals)])
# geo_household_marginals$Variable[-1]<-geo_household_marginals$Variable[-1]
# geo_household_marginals<-data.frame(t(geo_household_marginals))
# temp<-geo_household_marginals
# geo_household_marginals<-temp[,!((temp[1,]%in%c("HH_TOTINCAT") & temp[2,]%in%c(5:16)))]
# geo_household_marginals[-1:-3,(geo_household_marginals[1,]=="HH_TOTINCAT" & geo_household_marginals[2,]==1)]<-rowSums(apply(temp[-1:-3,temp[1,]=="HH_TOTINCAT" & temp[2,]%in%c(1:6)],2,as.numeric))
# geo_household_marginals[-1:-3,(geo_household_marginals[1,]=="HH_TOTINCAT" & geo_household_marginals[2,]==2)]<-rowSums(apply(temp[-1:-3,temp[1,]=="HH_TOTINCAT" & temp[2,]%in%c(7:11)],2,as.numeric))
# geo_household_marginals[-1:-3,(geo_household_marginals[1,]=="HH_TOTINCAT" & geo_household_marginals[2,]==3)]<-rowSums(apply(temp[-1:-3,temp[1,]=="HH_TOTINCAT" & temp[2,]%in%c(12:15)],2,as.numeric))
# geo_household_marginals[-1:-3,(geo_household_marginals[1,]=="HH_TOTINCAT" & geo_household_marginals[2,]==4)]<-as.numeric(temp[-1:-3,temp[1,]=="HH_TOTINCAT" & temp[2,]%in%c(16)])
# write_csv(geo_household_marginals,"Exported/A1/Montreal/Scenarios/S1 - bCMA_CT/geo_household_marginals.csv",col_names = FALSE)
# #geo_person_marginals
# geo_person_marginals<-merge(CTMAT[grepl("PP_",CTMAT$Variable),],pp_variable_importance,by="Variable",all.x=TRUE)
# geo_person_marginals<-geo_person_marginals[order(geo_person_marginals$Importance),]
# geo_person_marginals<-geo_person_marginals[geo_person_marginals$Importance!=0,!names(geo_person_marginals)%in%c("X","Definition","Description","Frequency","Importance")]
# geo_person_marginals$Categories[geo_person_marginals$Variable=="PP_ID"]<-1
# geo_person_marginals<-rbind(c("variable_names","variable_categories",1:length(unique(GIS$CT))),geo_person_marginals)
# geo_person_marginals<-cbind(geo_person_marginals[,1:2],c("geo",rep("",nrow(geo_person_marginals)-1)),geo_person_marginals[,3:ncol(geo_person_marginals)])
# geo_person_marginals$Variable[-1]<-geo_person_marginals$Variable[-1]
# geo_person_marginals<-data.frame(t(geo_person_marginals))
# temp<-geo_person_marginals
# geo_person_marginals<-temp[,!((temp[1,]%in%c("PP_AGEGRP") & temp[2,]%in%c(4:13)))]
# geo_person_marginals[-1:-3,(geo_person_marginals[1,]=="PP_AGEGRP" & geo_person_marginals[2,]==1)]<-rowSums(apply(temp[-1:-3,temp[1,]=="PP_AGEGRP" & temp[2,]%in%c(1:2)],2,as.numeric))
# geo_person_marginals[-1:-3,(geo_person_marginals[1,]=="PP_AGEGRP" & geo_person_marginals[2,]==2)]<-rowSums(apply(temp[-1:-3,temp[1,]=="PP_AGEGRP" & temp[2,]%in%c(3:11)],2,as.numeric))
# geo_person_marginals[-1:-3,(geo_person_marginals[1,]=="PP_AGEGRP" & geo_person_marginals[2,]==3)]<-rowSums(apply(temp[-1:-3,temp[1,]=="PP_AGEGRP" & temp[2,]%in%c(12:13)],2,as.numeric))
# write_csv(geo_person_marginals,"Exported/A1/Montreal/Scenarios/S1 - bCMA_CT/geo_person_marginals.csv",col_names = FALSE)
# #household_sample
# write_csv(household_sample,"Exported/A1/Montreal/Scenarios/S1 - bCMA_CT/household_sample.csv",col_names = TRUE)
# #person_sample
# write_csv(person_sample,"Exported/A1/Montreal/Scenarios/S1 - bCMA_CT/person_sample.csv",col_names = TRUE)
# 
# ###S2 - hCMA_CT
# #region_geo_mapping
# geo<-1:length(unique(GIS$CT))
# region<-1
# region_geo_mapping<-data.frame(region,geo)
# write.csv(region_geo_mapping,"Exported/A1/Montreal/Scenarios/S2 - hCMA_CT/region_geo_mapping.csv",row.names=FALSE)
# #geo_sample_mapping
# geo<-1:length(unique(GIS$CT))
# sample_geo<-1
# geo_sample_mapping<-data.frame(geo,sample_geo)
# write.csv(geo_sample_mapping,"Exported/A1/Montreal/Scenarios/S2 - hCMA_CT/geo_sample_mapping.csv",row.names=FALSE)
# #region_sample_mapping
# region<-1
# sample_geo<--1
# region_sample_mapping<-data.frame(region,sample_geo)
# write.csv(region_sample_mapping,"Exported/A1/Montreal/Scenarios/S2 - hCMA_CT/region_sample_mapping.csv",row.names=FALSE)
# #region_household_marginals
# region_household_marginals<-merge(hCMAMAT[grepl("HH_",hCMAMAT$Variable),],hh_variable_importance,by="Variable",all.x=TRUE)
# region_household_marginals<-region_household_marginals[order(region_household_marginals$Importance),]
# region_household_marginals<-region_household_marginals[region_household_marginals$Importance!=0,!names(region_household_marginals)%in%c("X","Definition","Description","Frequency","Importance")]
# region_household_marginals$Categories[region_household_marginals$Variable=="HH_ID"]<-1
# region_household_marginals<-rbind(c("variable_names","variable_categories",1:length(unique(GIS$CMA))),region_household_marginals)
# region_household_marginals<-cbind(region_household_marginals[,1:2],c("region",rep("",nrow(region_household_marginals)-1)),region_household_marginals[,3:ncol(region_household_marginals)])
# region_household_marginals$Variable[-1]<-paste("r",region_household_marginals$Variable[-1],sep="")
# region_household_marginals<-data.frame(t(region_household_marginals))
# temp<-region_household_marginals
# region_household_marginals<-temp[,!((temp[1,]%in%c("rHH_TOTINCAT") & temp[2,]%in%c(5:16)))]
# region_household_marginals[-1:-3,(region_household_marginals[1,]=="rHH_TOTINCAT" & region_household_marginals[2,]==1)]<-sum(apply(temp[-1:-3,temp[1,]=="rHH_TOTINCAT" & temp[2,]%in%c(1:6)],2,as.numeric))
# region_household_marginals[-1:-3,(region_household_marginals[1,]=="rHH_TOTINCAT" & region_household_marginals[2,]==2)]<-sum(apply(temp[-1:-3,temp[1,]=="rHH_TOTINCAT" & temp[2,]%in%c(7:11)],2,as.numeric))
# region_household_marginals[-1:-3,(region_household_marginals[1,]=="rHH_TOTINCAT" & region_household_marginals[2,]==3)]<-sum(apply(temp[-1:-3,temp[1,]=="rHH_TOTINCAT" & temp[2,]%in%c(12:15)],2,as.numeric))
# region_household_marginals[-1:-3,(region_household_marginals[1,]=="rHH_TOTINCAT" & region_household_marginals[2,]==4)]<-as.numeric(temp[-1:-3,temp[1,]=="rHH_TOTINCAT" & temp[2,]%in%c(16)])
# write_csv(region_household_marginals,"Exported/A1/Montreal/Scenarios/S2 - hCMA_CT/region_household_marginals.csv",col_names = FALSE)
# #region_person_marginals
# region_person_marginals<-merge(hCMAMAT[grepl("PP_",hCMAMAT$Variable),],pp_variable_importance,by="Variable",all.x=TRUE)
# region_person_marginals<-region_person_marginals[order(region_person_marginals$Importance),]
# region_person_marginals<-region_person_marginals[region_person_marginals$Importance!=0,!names(region_person_marginals)%in%c("X","Definition","Description","Frequency","Importance")]
# region_person_marginals$Categories[region_person_marginals$Variable=="PP_ID"]<-1
# region_person_marginals<-rbind(c("variable_names","variable_categories",1:length(unique(GIS$CMA))),region_person_marginals)
# region_person_marginals<-cbind(region_person_marginals[,1:2],c("region",rep("",nrow(region_person_marginals)-1)),region_person_marginals[,3:ncol(region_person_marginals)])
# region_person_marginals$Variable[-1]<-paste("r",region_person_marginals$Variable[-1],sep="")
# region_person_marginals<-data.frame(t(region_person_marginals))
# temp<-region_person_marginals
# region_person_marginals<-temp[,!((temp[1,]%in%c("rPP_AGEGRP") & temp[2,]%in%c(4:13)))]
# region_person_marginals[-1:-3,(region_person_marginals[1,]=="rPP_AGEGRP" & region_person_marginals[2,]==1)]<-sum(apply(temp[-1:-3,temp[1,]=="rPP_AGEGRP" & temp[2,]%in%c(1:2)],2,as.numeric))
# region_person_marginals[-1:-3,(region_person_marginals[1,]=="rPP_AGEGRP" & region_person_marginals[2,]==2)]<-sum(apply(temp[-1:-3,temp[1,]=="rPP_AGEGRP" & temp[2,]%in%c(3:11)],2,as.numeric))
# region_person_marginals[-1:-3,(region_person_marginals[1,]=="rPP_AGEGRP" & region_person_marginals[2,]==3)]<-sum(apply(temp[-1:-3,temp[1,]=="rPP_AGEGRP" & temp[2,]%in%c(12:13)],2,as.numeric))
# write_csv(region_person_marginals,"Exported/A1/Montreal/Scenarios/S2 - hCMA_CT/region_person_marginals.csv",col_names = FALSE)
# #geo_household_marginals
# geo_household_marginals<-merge(hCTMAT[grepl("HH_",hCTMAT$Variable),],hh_variable_importance,by="Variable",all.x=TRUE)
# geo_household_marginals<-geo_household_marginals[order(geo_household_marginals$Importance),]
# geo_household_marginals<-geo_household_marginals[geo_household_marginals$Importance!=0,!names(geo_household_marginals)%in%c("X","Definition","Description","Frequency","Importance")]
# geo_household_marginals$Categories[geo_household_marginals$Variable=="HH_ID"]<-1
# geo_household_marginals<-rbind(c("variable_names","variable_categories",1:length(unique(GIS$CT))),geo_household_marginals)
# geo_household_marginals<-cbind(geo_household_marginals[,1:2],c("geo",rep("",nrow(geo_household_marginals)-1)),geo_household_marginals[,3:ncol(geo_household_marginals)])
# geo_household_marginals$Variable[-1]<-geo_household_marginals$Variable[-1]
# geo_household_marginals<-data.frame(t(geo_household_marginals))
# temp<-geo_household_marginals
# geo_household_marginals<-temp[,!((temp[1,]%in%c("HH_TOTINCAT") & temp[2,]%in%c(5:16)))]
# geo_household_marginals[-1:-3,(geo_household_marginals[1,]=="HH_TOTINCAT" & geo_household_marginals[2,]==1)]<-rowSums(apply(temp[-1:-3,temp[1,]=="HH_TOTINCAT" & temp[2,]%in%c(1:6)],2,as.numeric))
# geo_household_marginals[-1:-3,(geo_household_marginals[1,]=="HH_TOTINCAT" & geo_household_marginals[2,]==2)]<-rowSums(apply(temp[-1:-3,temp[1,]=="HH_TOTINCAT" & temp[2,]%in%c(7:11)],2,as.numeric))
# geo_household_marginals[-1:-3,(geo_household_marginals[1,]=="HH_TOTINCAT" & geo_household_marginals[2,]==3)]<-rowSums(apply(temp[-1:-3,temp[1,]=="HH_TOTINCAT" & temp[2,]%in%c(12:15)],2,as.numeric))
# geo_household_marginals[-1:-3,(geo_household_marginals[1,]=="HH_TOTINCAT" & geo_household_marginals[2,]==4)]<-as.numeric(temp[-1:-3,temp[1,]=="HH_TOTINCAT" & temp[2,]%in%c(16)])
# write_csv(geo_household_marginals,"Exported/A1/Montreal/Scenarios/S2 - hCMA_CT/geo_household_marginals.csv",col_names = FALSE)
# #geo_person_marginals
# geo_person_marginals<-merge(hCTMAT[grepl("PP_",hCTMAT$Variable),],pp_variable_importance,by="Variable",all.x=TRUE)
# geo_person_marginals<-geo_person_marginals[order(geo_person_marginals$Importance),]
# geo_person_marginals<-geo_person_marginals[geo_person_marginals$Importance!=0,!names(geo_person_marginals)%in%c("X","Definition","Description","Frequency","Importance")]
# geo_person_marginals$Categories[geo_person_marginals$Variable=="PP_ID"]<-1
# geo_person_marginals<-rbind(c("variable_names","variable_categories",1:length(unique(GIS$CT))),geo_person_marginals)
# geo_person_marginals<-cbind(geo_person_marginals[,1:2],c("geo",rep("",nrow(geo_person_marginals)-1)),geo_person_marginals[,3:ncol(geo_person_marginals)])
# geo_person_marginals$Variable[-1]<-geo_person_marginals$Variable[-1]
# geo_person_marginals<-data.frame(t(geo_person_marginals))
# temp<-geo_person_marginals
# geo_person_marginals<-temp[,!((temp[1,]%in%c("PP_AGEGRP") & temp[2,]%in%c(4:13)))]
# geo_person_marginals[-1:-3,(geo_person_marginals[1,]=="PP_AGEGRP" & geo_person_marginals[2,]==1)]<-rowSums(apply(temp[-1:-3,temp[1,]=="PP_AGEGRP" & temp[2,]%in%c(1:2)],2,as.numeric))
# geo_person_marginals[-1:-3,(geo_person_marginals[1,]=="PP_AGEGRP" & geo_person_marginals[2,]==2)]<-rowSums(apply(temp[-1:-3,temp[1,]=="PP_AGEGRP" & temp[2,]%in%c(3:11)],2,as.numeric))
# geo_person_marginals[-1:-3,(geo_person_marginals[1,]=="PP_AGEGRP" & geo_person_marginals[2,]==3)]<-rowSums(apply(temp[-1:-3,temp[1,]=="PP_AGEGRP" & temp[2,]%in%c(12:13)],2,as.numeric))
# write_csv(geo_person_marginals,"Exported/A1/Montreal/Scenarios/S2 - hCMA_CT/geo_person_marginals.csv",col_names = FALSE)
# #household_sample
# write_csv(household_sample,"Exported/A1/Montreal/Scenarios/S2 - hCMA_CT/household_sample.csv",col_names = TRUE)
# #person_sample
# write_csv(person_sample,"Exported/A1/Montreal/Scenarios/S2 - hCMA_CT/person_sample.csv",col_names = TRUE)
# 
# ###S3 - bCMA_DA
# #region_geo_mapping
# geo<-1:length(unique(GIS$DA))
# region<-1
# region_geo_mapping<-data.frame(region,geo)
# write.csv(region_geo_mapping,"Exported/A1/Montreal/Scenarios/S3 - bCMA_DA/region_geo_mapping.csv",row.names=FALSE)
# #geo_sample_mapping
# geo<-1:length(unique(GIS$DA))
# sample_geo<-1
# geo_sample_mapping<-data.frame(geo,sample_geo)
# write.csv(geo_sample_mapping,"Exported/A1/Montreal/Scenarios/S3 - bCMA_DA/geo_sample_mapping.csv",row.names=FALSE)
# #region_sample_mapping
# region<-1
# sample_geo<--1
# region_sample_mapping<-data.frame(region,sample_geo)
# write.csv(region_sample_mapping,"Exported/A1/Montreal/Scenarios/S3 - bCMA_DA/region_sample_mapping.csv",row.names=FALSE)
# #region_household_marginals
# region_household_marginals<-merge(CMAMAT[grepl("HH_",CMAMAT$Variable),],hh_variable_importance,by="Variable",all.x=TRUE)
# region_household_marginals<-region_household_marginals[order(region_household_marginals$Importance),]
# region_household_marginals<-region_household_marginals[region_household_marginals$Importance!=0,!names(region_household_marginals)%in%c("X","Definition","Description","Frequency","Importance")]
# region_household_marginals$Categories[region_household_marginals$Variable=="HH_ID"]<-1
# region_household_marginals<-rbind(c("variable_names","variable_categories",1:length(unique(GIS$CMA))),region_household_marginals)
# region_household_marginals<-cbind(region_household_marginals[,1:2],c("region",rep("",nrow(region_household_marginals)-1)),region_household_marginals[,3:ncol(region_household_marginals)])
# region_household_marginals$Variable[-1]<-paste("r",region_household_marginals$Variable[-1],sep="")
# region_household_marginals<-data.frame(t(region_household_marginals))
# temp<-region_household_marginals
# region_household_marginals<-temp[,!((temp[1,]%in%c("rHH_TOTINCAT") & temp[2,]%in%c(5:16)))]
# region_household_marginals[-1:-3,(region_household_marginals[1,]=="rHH_TOTINCAT" & region_household_marginals[2,]==1)]<-sum(apply(temp[-1:-3,temp[1,]=="rHH_TOTINCAT" & temp[2,]%in%c(1:6)],2,as.numeric))
# region_household_marginals[-1:-3,(region_household_marginals[1,]=="rHH_TOTINCAT" & region_household_marginals[2,]==2)]<-sum(apply(temp[-1:-3,temp[1,]=="rHH_TOTINCAT" & temp[2,]%in%c(7:11)],2,as.numeric))
# region_household_marginals[-1:-3,(region_household_marginals[1,]=="rHH_TOTINCAT" & region_household_marginals[2,]==3)]<-sum(apply(temp[-1:-3,temp[1,]=="rHH_TOTINCAT" & temp[2,]%in%c(12:15)],2,as.numeric))
# region_household_marginals[-1:-3,(region_household_marginals[1,]=="rHH_TOTINCAT" & region_household_marginals[2,]==4)]<-as.numeric(temp[-1:-3,temp[1,]=="rHH_TOTINCAT" & temp[2,]%in%c(16)])
# write_csv(region_household_marginals,"Exported/A1/Montreal/Scenarios/S3 - bCMA_DA/region_household_marginals.csv",col_names = FALSE)
# #region_person_marginals
# region_person_marginals<-merge(CMAMAT[grepl("PP_",CMAMAT$Variable),],pp_variable_importance,by="Variable",all.x=TRUE)
# region_person_marginals<-region_person_marginals[order(region_person_marginals$Importance),]
# region_person_marginals<-region_person_marginals[region_person_marginals$Importance!=0,!names(region_person_marginals)%in%c("X","Definition","Description","Frequency","Importance")]
# region_person_marginals$Categories[region_person_marginals$Variable=="PP_ID"]<-1
# region_person_marginals<-rbind(c("variable_names","variable_categories",1:length(unique(GIS$CMA))),region_person_marginals)
# region_person_marginals<-cbind(region_person_marginals[,1:2],c("region",rep("",nrow(region_person_marginals)-1)),region_person_marginals[,3:ncol(region_person_marginals)])
# region_person_marginals$Variable[-1]<-paste("r",region_person_marginals$Variable[-1],sep="")
# region_person_marginals<-data.frame(t(region_person_marginals))
# temp<-region_person_marginals
# region_person_marginals<-temp[,!((temp[1,]%in%c("rPP_AGEGRP") & temp[2,]%in%c(4:13)))]
# region_person_marginals[-1:-3,(region_person_marginals[1,]=="rPP_AGEGRP" & region_person_marginals[2,]==1)]<-sum(apply(temp[-1:-3,temp[1,]=="rPP_AGEGRP" & temp[2,]%in%c(1:2)],2,as.numeric))
# region_person_marginals[-1:-3,(region_person_marginals[1,]=="rPP_AGEGRP" & region_person_marginals[2,]==2)]<-sum(apply(temp[-1:-3,temp[1,]=="rPP_AGEGRP" & temp[2,]%in%c(3:11)],2,as.numeric))
# region_person_marginals[-1:-3,(region_person_marginals[1,]=="rPP_AGEGRP" & region_person_marginals[2,]==3)]<-sum(apply(temp[-1:-3,temp[1,]=="rPP_AGEGRP" & temp[2,]%in%c(12:13)],2,as.numeric))
# write_csv(region_person_marginals,"Exported/A1/Montreal/Scenarios/S3 - bCMA_DA/region_person_marginals.csv",col_names = FALSE)
# #geo_household_marginals
# geo_household_marginals<-merge(DAMAT[grepl("HH_",DAMAT$Variable),],hh_variable_importance,by="Variable",all.x=TRUE)
# geo_household_marginals<-geo_household_marginals[order(geo_household_marginals$Importance),]
# geo_household_marginals<-geo_household_marginals[geo_household_marginals$Importance!=0,!names(geo_household_marginals)%in%c("X","Definition","Description","Frequency","Importance")]
# geo_household_marginals$Categories[geo_household_marginals$Variable=="HH_ID"]<-1
# geo_household_marginals<-rbind(c("variable_names","variable_categories",1:length(unique(GIS$DA))),geo_household_marginals)
# geo_household_marginals<-cbind(geo_household_marginals[,1:2],c("geo",rep("",nrow(geo_household_marginals)-1)),geo_household_marginals[,3:ncol(geo_household_marginals)])
# geo_household_marginals$Variable[-1]<-geo_household_marginals$Variable[-1]
# geo_household_marginals<-data.frame(t(geo_household_marginals))
# temp<-geo_household_marginals
# geo_household_marginals<-temp[,!((temp[1,]%in%c("HH_TOTINCAT") & temp[2,]%in%c(5:16)))]
# geo_household_marginals[-1:-3,(geo_household_marginals[1,]=="HH_TOTINCAT" & geo_household_marginals[2,]==1)]<-rowSums(apply(temp[-1:-3,temp[1,]=="HH_TOTINCAT" & temp[2,]%in%c(1:6)],2,as.numeric))
# geo_household_marginals[-1:-3,(geo_household_marginals[1,]=="HH_TOTINCAT" & geo_household_marginals[2,]==2)]<-rowSums(apply(temp[-1:-3,temp[1,]=="HH_TOTINCAT" & temp[2,]%in%c(7:11)],2,as.numeric))
# geo_household_marginals[-1:-3,(geo_household_marginals[1,]=="HH_TOTINCAT" & geo_household_marginals[2,]==3)]<-rowSums(apply(temp[-1:-3,temp[1,]=="HH_TOTINCAT" & temp[2,]%in%c(12:15)],2,as.numeric))
# geo_household_marginals[-1:-3,(geo_household_marginals[1,]=="HH_TOTINCAT" & geo_household_marginals[2,]==4)]<-as.numeric(temp[-1:-3,temp[1,]=="HH_TOTINCAT" & temp[2,]%in%c(16)])
# write_csv(geo_household_marginals,"Exported/A1/Montreal/Scenarios/S3 - bCMA_DA/geo_household_marginals.csv",col_names = FALSE)
# #geo_person_marginals
# geo_person_marginals<-merge(DAMAT[grepl("PP_",DAMAT$Variable),],pp_variable_importance,by="Variable",all.x=TRUE)
# geo_person_marginals<-geo_person_marginals[order(geo_person_marginals$Importance),]
# geo_person_marginals<-geo_person_marginals[geo_person_marginals$Importance!=0,!names(geo_person_marginals)%in%c("X","Definition","Description","Frequency","Importance")]
# geo_person_marginals$Categories[geo_person_marginals$Variable=="PP_ID"]<-1
# geo_person_marginals<-rbind(c("variable_names","variable_categories",1:length(unique(GIS$DA))),geo_person_marginals)
# geo_person_marginals<-cbind(geo_person_marginals[,1:2],c("geo",rep("",nrow(geo_person_marginals)-1)),geo_person_marginals[,3:ncol(geo_person_marginals)])
# geo_person_marginals$Variable[-1]<-geo_person_marginals$Variable[-1]
# geo_person_marginals<-data.frame(t(geo_person_marginals))
# temp<-geo_person_marginals
# geo_person_marginals<-temp[,!((temp[1,]%in%c("PP_AGEGRP") & temp[2,]%in%c(4:13)))]
# geo_person_marginals[-1:-3,(geo_person_marginals[1,]=="PP_AGEGRP" & geo_person_marginals[2,]==1)]<-rowSums(apply(temp[-1:-3,temp[1,]=="PP_AGEGRP" & temp[2,]%in%c(1:2)],2,as.numeric))
# geo_person_marginals[-1:-3,(geo_person_marginals[1,]=="PP_AGEGRP" & geo_person_marginals[2,]==2)]<-rowSums(apply(temp[-1:-3,temp[1,]=="PP_AGEGRP" & temp[2,]%in%c(3:11)],2,as.numeric))
# geo_person_marginals[-1:-3,(geo_person_marginals[1,]=="PP_AGEGRP" & geo_person_marginals[2,]==3)]<-rowSums(apply(temp[-1:-3,temp[1,]=="PP_AGEGRP" & temp[2,]%in%c(12:13)],2,as.numeric))
# write_csv(geo_person_marginals,"Exported/A1/Montreal/Scenarios/S3 - bCMA_DA/geo_person_marginals.csv",col_names = FALSE)
# #household_sample
# write_csv(household_sample,"Exported/A1/Montreal/Scenarios/S3 - bCMA_DA/household_sample.csv",col_names = TRUE)
# #person_sample
# write_csv(person_sample,"Exported/A1/Montreal/Scenarios/S3 - bCMA_DA/person_sample.csv",col_names = TRUE)
# 
# ###S4 - hCMA_DA
# #region_geo_mapping
# geo<-1:length(unique(GIS$DA))
# region<-1
# region_geo_mapping<-data.frame(region,geo)
# write.csv(region_geo_mapping,"Exported/A1/Montreal/Scenarios/S4 - hCMA_DA/region_geo_mapping.csv",row.names=FALSE)
# #geo_sample_mapping
# geo<-1:length(unique(GIS$DA))
# sample_geo<-1
# geo_sample_mapping<-data.frame(geo,sample_geo)
# write.csv(geo_sample_mapping,"Exported/A1/Montreal/Scenarios/S4 - hCMA_DA/geo_sample_mapping.csv",row.names=FALSE)
# #region_sample_mapping
# region<-1
# sample_geo<--1
# region_sample_mapping<-data.frame(region,sample_geo)
# write.csv(region_sample_mapping,"Exported/A1/Montreal/Scenarios/S4 - hCMA_DA/region_sample_mapping.csv",row.names=FALSE)
# #region_household_marginals
# region_household_marginals<-merge(hCMAMAT[grepl("HH_",hCMAMAT$Variable),],hh_variable_importance,by="Variable",all.x=TRUE)
# region_household_marginals<-region_household_marginals[order(region_household_marginals$Importance),]
# region_household_marginals<-region_household_marginals[region_household_marginals$Importance!=0,!names(region_household_marginals)%in%c("X","Definition","Description","Frequency","Importance")]
# region_household_marginals$Categories[region_household_marginals$Variable=="HH_ID"]<-1
# region_household_marginals<-rbind(c("variable_names","variable_categories",1:length(unique(GIS$CMA))),region_household_marginals)
# region_household_marginals<-cbind(region_household_marginals[,1:2],c("region",rep("",nrow(region_household_marginals)-1)),region_household_marginals[,3:ncol(region_household_marginals)])
# region_household_marginals$Variable[-1]<-paste("r",region_household_marginals$Variable[-1],sep="")
# region_household_marginals<-data.frame(t(region_household_marginals))
# temp<-region_household_marginals
# region_household_marginals<-temp[,!((temp[1,]%in%c("rHH_TOTINCAT") & temp[2,]%in%c(5:16)))]
# region_household_marginals[-1:-3,(region_household_marginals[1,]=="rHH_TOTINCAT" & region_household_marginals[2,]==1)]<-sum(apply(temp[-1:-3,temp[1,]=="rHH_TOTINCAT" & temp[2,]%in%c(1:6)],2,as.numeric))
# region_household_marginals[-1:-3,(region_household_marginals[1,]=="rHH_TOTINCAT" & region_household_marginals[2,]==2)]<-sum(apply(temp[-1:-3,temp[1,]=="rHH_TOTINCAT" & temp[2,]%in%c(7:11)],2,as.numeric))
# region_household_marginals[-1:-3,(region_household_marginals[1,]=="rHH_TOTINCAT" & region_household_marginals[2,]==3)]<-sum(apply(temp[-1:-3,temp[1,]=="rHH_TOTINCAT" & temp[2,]%in%c(12:15)],2,as.numeric))
# region_household_marginals[-1:-3,(region_household_marginals[1,]=="rHH_TOTINCAT" & region_household_marginals[2,]==4)]<-as.numeric(temp[-1:-3,temp[1,]=="rHH_TOTINCAT" & temp[2,]%in%c(16)])
# write_csv(region_household_marginals,"Exported/A1/Montreal/Scenarios/S4 - hCMA_DA/region_household_marginals.csv",col_names = FALSE)
# #region_person_marginals
# region_person_marginals<-merge(hCMAMAT[grepl("PP_",hCMAMAT$Variable),],pp_variable_importance,by="Variable",all.x=TRUE)
# region_person_marginals<-region_person_marginals[order(region_person_marginals$Importance),]
# region_person_marginals<-region_person_marginals[region_person_marginals$Importance!=0,!names(region_person_marginals)%in%c("X","Definition","Description","Frequency","Importance")]
# region_person_marginals$Categories[region_person_marginals$Variable=="PP_ID"]<-1
# region_person_marginals<-rbind(c("variable_names","variable_categories",1:length(unique(GIS$CMA))),region_person_marginals)
# region_person_marginals<-cbind(region_person_marginals[,1:2],c("region",rep("",nrow(region_person_marginals)-1)),region_person_marginals[,3:ncol(region_person_marginals)])
# region_person_marginals$Variable[-1]<-paste("r",region_person_marginals$Variable[-1],sep="")
# region_person_marginals<-data.frame(t(region_person_marginals))
# temp<-region_person_marginals
# region_person_marginals<-temp[,!((temp[1,]%in%c("rPP_AGEGRP") & temp[2,]%in%c(4:13)))]
# region_person_marginals[-1:-3,(region_person_marginals[1,]=="rPP_AGEGRP" & region_person_marginals[2,]==1)]<-sum(apply(temp[-1:-3,temp[1,]=="rPP_AGEGRP" & temp[2,]%in%c(1:2)],2,as.numeric))
# region_person_marginals[-1:-3,(region_person_marginals[1,]=="rPP_AGEGRP" & region_person_marginals[2,]==2)]<-sum(apply(temp[-1:-3,temp[1,]=="rPP_AGEGRP" & temp[2,]%in%c(3:11)],2,as.numeric))
# region_person_marginals[-1:-3,(region_person_marginals[1,]=="rPP_AGEGRP" & region_person_marginals[2,]==3)]<-sum(apply(temp[-1:-3,temp[1,]=="rPP_AGEGRP" & temp[2,]%in%c(12:13)],2,as.numeric))
# write_csv(region_person_marginals,"Exported/A1/Montreal/Scenarios/S4 - hCMA_DA/region_person_marginals.csv",col_names = FALSE)
# #geo_household_marginals
# geo_household_marginals<-merge(hDAMAT[grepl("HH_",hDAMAT$Variable),],hh_variable_importance,by="Variable",all.x=TRUE)
# geo_household_marginals<-geo_household_marginals[order(geo_household_marginals$Importance),]
# geo_household_marginals<-geo_household_marginals[geo_household_marginals$Importance!=0,!names(geo_household_marginals)%in%c("X","Definition","Description","Frequency","Importance")]
# geo_household_marginals$Categories[geo_household_marginals$Variable=="HH_ID"]<-1
# geo_household_marginals<-rbind(c("variable_names","variable_categories",1:length(unique(GIS$DA))),geo_household_marginals)
# geo_household_marginals<-cbind(geo_household_marginals[,1:2],c("geo",rep("",nrow(geo_household_marginals)-1)),geo_household_marginals[,3:ncol(geo_household_marginals)])
# geo_household_marginals$Variable[-1]<-geo_household_marginals$Variable[-1]
# geo_household_marginals<-data.frame(t(geo_household_marginals))
# temp<-geo_household_marginals
# geo_household_marginals<-temp[,!((temp[1,]%in%c("HH_TOTINCAT") & temp[2,]%in%c(5:16)))]
# geo_household_marginals[-1:-3,(geo_household_marginals[1,]=="HH_TOTINCAT" & geo_household_marginals[2,]==1)]<-rowSums(apply(temp[-1:-3,temp[1,]=="HH_TOTINCAT" & temp[2,]%in%c(1:6)],2,as.numeric))
# geo_household_marginals[-1:-3,(geo_household_marginals[1,]=="HH_TOTINCAT" & geo_household_marginals[2,]==2)]<-rowSums(apply(temp[-1:-3,temp[1,]=="HH_TOTINCAT" & temp[2,]%in%c(7:11)],2,as.numeric))
# geo_household_marginals[-1:-3,(geo_household_marginals[1,]=="HH_TOTINCAT" & geo_household_marginals[2,]==3)]<-rowSums(apply(temp[-1:-3,temp[1,]=="HH_TOTINCAT" & temp[2,]%in%c(12:15)],2,as.numeric))
# geo_household_marginals[-1:-3,(geo_household_marginals[1,]=="HH_TOTINCAT" & geo_household_marginals[2,]==4)]<-as.numeric(temp[-1:-3,temp[1,]=="HH_TOTINCAT" & temp[2,]%in%c(16)])
# write_csv(geo_household_marginals,"Exported/A1/Montreal/Scenarios/S4 - hCMA_DA/geo_household_marginals.csv",col_names = FALSE)
# #geo_person_marginals
# geo_person_marginals<-merge(hDAMAT[grepl("PP_",hDAMAT$Variable),],pp_variable_importance,by="Variable",all.x=TRUE)
# geo_person_marginals<-geo_person_marginals[order(geo_person_marginals$Importance),]
# geo_person_marginals<-geo_person_marginals[geo_person_marginals$Importance!=0,!names(geo_person_marginals)%in%c("X","Definition","Description","Frequency","Importance")]
# geo_person_marginals$Categories[geo_person_marginals$Variable=="PP_ID"]<-1
# geo_person_marginals<-rbind(c("variable_names","variable_categories",1:length(unique(GIS$DA))),geo_person_marginals)
# geo_person_marginals<-cbind(geo_person_marginals[,1:2],c("geo",rep("",nrow(geo_person_marginals)-1)),geo_person_marginals[,3:ncol(geo_person_marginals)])
# geo_person_marginals$Variable[-1]<-geo_person_marginals$Variable[-1]
# geo_person_marginals<-data.frame(t(geo_person_marginals))
# temp<-geo_person_marginals
# geo_person_marginals<-temp[,!((temp[1,]%in%c("PP_AGEGRP") & temp[2,]%in%c(4:13)))]
# geo_person_marginals[-1:-3,(geo_person_marginals[1,]=="PP_AGEGRP" & geo_person_marginals[2,]==1)]<-rowSums(apply(temp[-1:-3,temp[1,]=="PP_AGEGRP" & temp[2,]%in%c(1:2)],2,as.numeric))
# geo_person_marginals[-1:-3,(geo_person_marginals[1,]=="PP_AGEGRP" & geo_person_marginals[2,]==2)]<-rowSums(apply(temp[-1:-3,temp[1,]=="PP_AGEGRP" & temp[2,]%in%c(3:11)],2,as.numeric))
# geo_person_marginals[-1:-3,(geo_person_marginals[1,]=="PP_AGEGRP" & geo_person_marginals[2,]==3)]<-rowSums(apply(temp[-1:-3,temp[1,]=="PP_AGEGRP" & temp[2,]%in%c(12:13)],2,as.numeric))
# write_csv(geo_person_marginals,"Exported/A1/Montreal/Scenarios/S4 - hCMA_DA/geo_person_marginals.csv",col_names = FALSE)
# #household_sample
# write_csv(household_sample,"Exported/A1/Montreal/Scenarios/S4 - hCMA_DA/household_sample.csv",col_names = TRUE)
# #person_sample
# write_csv(person_sample,"Exported/A1/Montreal/Scenarios/S4 - hCMA_DA/person_sample.csv",col_names = TRUE)
# 
# ###S5 - bCT_DA
# #region_geo_mapping
# region_geo_mapping<-data.frame("region"=GIS$CT,"geo"=GIS$DA)
# temp<-region_geo_mapping
# region_geo_mapping$region[1]<-1
# for(i in 2:nrow(region_geo_mapping)){
#   ifelse(temp$region[i]==temp$region[i-1],region_geo_mapping$region[i]<-region_geo_mapping$region[i-1],region_geo_mapping$region[i]<-region_geo_mapping$region[i-1]+1)
# }
# region_geo_mapping[order(region_geo_mapping$geo),]$geo<-1:nrow(region_geo_mapping)
# write.csv(region_geo_mapping,"Exported/A1/Montreal/Scenarios/S5 - bCT_DA/region_geo_mapping.csv",row.names=FALSE)
# #geo_sample_mapping
# geo<-1:length(GIS$DA)
# sample_geo<-1
# geo_sample_mapping<-data.frame(geo,sample_geo)
# write.csv(geo_sample_mapping,"Exported/A1/Montreal/Scenarios/S5 - bCT_DA/geo_sample_mapping.csv",row.names=FALSE)
# #region_sample_mapping
# region<-1:length(unique(GIS$CT))
# sample_geo<--1
# region_sample_mapping<-data.frame(region,sample_geo)
# write.csv(region_sample_mapping,"Exported/A1/Montreal/Scenarios/S5 - bCT_DA/region_sample_mapping.csv",row.names=FALSE)
# #region_household_marginals
# region_household_marginals<-merge(CTMAT[grepl("HH_",CTMAT$Variable),],hh_variable_importance,by="Variable",all.x=TRUE)
# region_household_marginals<-region_household_marginals[order(region_household_marginals$Importance),]
# region_household_marginals<-region_household_marginals[region_household_marginals$Importance!=0,!names(region_household_marginals)%in%c("Definition","X","Description","Frequency","Importance")]
# region_household_marginals$Categories[region_household_marginals$Variable=="HH_ID"]<-1
# region_household_marginals<-rbind(c("variable_names","variable_categories",1:length(unique(GIS$CT))),region_household_marginals)
# region_household_marginals<-cbind(region_household_marginals[,1:2],c("region",rep("",nrow(region_household_marginals)-1)),region_household_marginals[,3:ncol(region_household_marginals)])
# region_household_marginals$Variable[-1]<-paste("r",region_household_marginals$Variable[-1],sep="")
# region_household_marginals<-data.frame(t(region_household_marginals))
# temp<-region_household_marginals
# region_household_marginals<-temp[,!((temp[1,]%in%c("rHH_TOTINCAT") & temp[2,]%in%c(5:16)))]
# region_household_marginals[-1:-3,(region_household_marginals[1,]=="rHH_TOTINCAT" & region_household_marginals[2,]==1)]<-rowSums(apply(temp[-1:-3,temp[1,]=="rHH_TOTINCAT" & temp[2,]%in%c(1:6)],2,as.numeric))
# region_household_marginals[-1:-3,(region_household_marginals[1,]=="rHH_TOTINCAT" & region_household_marginals[2,]==2)]<-rowSums(apply(temp[-1:-3,temp[1,]=="rHH_TOTINCAT" & temp[2,]%in%c(7:11)],2,as.numeric))
# region_household_marginals[-1:-3,(region_household_marginals[1,]=="rHH_TOTINCAT" & region_household_marginals[2,]==3)]<-rowSums(apply(temp[-1:-3,temp[1,]=="rHH_TOTINCAT" & temp[2,]%in%c(12:15)],2,as.numeric))
# region_household_marginals[-1:-3,(region_household_marginals[1,]=="rHH_TOTINCAT" & region_household_marginals[2,]==4)]<-as.numeric(temp[-1:-3,temp[1,]=="rHH_TOTINCAT" & temp[2,]%in%c(16)])
# write_csv(region_household_marginals,"Exported/A1/Montreal/Scenarios/S5 - bCT_DA/region_household_marginals.csv",col_names = FALSE)
# #region_person_marginals
# region_person_marginals<-merge(CTMAT[grepl("PP_",CTMAT$Variable),],pp_variable_importance,by="Variable",all.x=TRUE)
# region_person_marginals<-region_person_marginals[order(region_person_marginals$Importance),]
# region_person_marginals<-region_person_marginals[region_person_marginals$Importance!=0,!names(region_person_marginals)%in%c("Definition","X","Description","Frequency","Importance")]
# region_person_marginals$Categories[region_person_marginals$Variable=="PP_ID"]<-1
# region_person_marginals<-rbind(c("variable_names","variable_categories",1:length(unique(GIS$CT))),region_person_marginals)
# region_person_marginals<-cbind(region_person_marginals[,1:2],c("region",rep("",nrow(region_person_marginals)-1)),region_person_marginals[,3:ncol(region_person_marginals)])
# region_person_marginals$Variable[-1]<-paste("r",region_person_marginals$Variable[-1],sep="")
# region_person_marginals<-data.frame(t(region_person_marginals))
# temp<-region_person_marginals
# region_person_marginals<-temp[,!((temp[1,]%in%c("rPP_AGEGRP") & temp[2,]%in%c(4:13)))]
# region_person_marginals[-1:-3,(region_person_marginals[1,]=="rPP_AGEGRP" & region_person_marginals[2,]==1)]<-rowSums(apply(temp[-1:-3,temp[1,]=="rPP_AGEGRP" & temp[2,]%in%c(1:2)],2,as.numeric))
# region_person_marginals[-1:-3,(region_person_marginals[1,]=="rPP_AGEGRP" & region_person_marginals[2,]==2)]<-rowSums(apply(temp[-1:-3,temp[1,]=="rPP_AGEGRP" & temp[2,]%in%c(3:11)],2,as.numeric))
# region_person_marginals[-1:-3,(region_person_marginals[1,]=="rPP_AGEGRP" & region_person_marginals[2,]==3)]<-rowSums(apply(temp[-1:-3,temp[1,]=="rPP_AGEGRP" & temp[2,]%in%c(12:13)],2,as.numeric))
# write_csv(region_person_marginals,"Exported/A1/Montreal/Scenarios/S5 - bCT_DA/region_person_marginals.csv",col_names = FALSE)
# #geo_household_marginals
# geo_household_marginals<-merge(DAMAT[grepl("HH_",DAMAT$Variable),],hh_variable_importance,by="Variable",all.x=TRUE)
# geo_household_marginals<-geo_household_marginals[order(geo_household_marginals$Importance),]
# geo_household_marginals<-geo_household_marginals[geo_household_marginals$Importance!=0,!names(geo_household_marginals)%in%c("Definition","X","Description","Frequency","Importance")]
# geo_household_marginals$Categories[geo_household_marginals$Variable=="HH_ID"]<-1
# geo_household_marginals<-rbind(c("variable_names","variable_categories",1:length(unique(GIS$DA))),geo_household_marginals)
# geo_household_marginals<-cbind(geo_household_marginals[,1:2],c("geo",rep("",nrow(geo_household_marginals)-1)),geo_household_marginals[,3:ncol(geo_household_marginals)])
# geo_household_marginals$Variable[-1]<-geo_household_marginals$Variable[-1]
# geo_household_marginals<-data.frame(t(geo_household_marginals))
# temp<-geo_household_marginals
# geo_household_marginals<-temp[,!((temp[1,]%in%c("HH_TOTINCAT") & temp[2,]%in%c(5:16)))]
# geo_household_marginals[-1:-3,(geo_household_marginals[1,]=="HH_TOTINCAT" & geo_household_marginals[2,]==1)]<-rowSums(apply(temp[-1:-3,temp[1,]=="HH_TOTINCAT" & temp[2,]%in%c(1:6)],2,as.numeric))
# geo_household_marginals[-1:-3,(geo_household_marginals[1,]=="HH_TOTINCAT" & geo_household_marginals[2,]==2)]<-rowSums(apply(temp[-1:-3,temp[1,]=="HH_TOTINCAT" & temp[2,]%in%c(7:11)],2,as.numeric))
# geo_household_marginals[-1:-3,(geo_household_marginals[1,]=="HH_TOTINCAT" & geo_household_marginals[2,]==3)]<-rowSums(apply(temp[-1:-3,temp[1,]=="HH_TOTINCAT" & temp[2,]%in%c(12:15)],2,as.numeric))
# geo_household_marginals[-1:-3,(geo_household_marginals[1,]=="HH_TOTINCAT" & geo_household_marginals[2,]==4)]<-as.numeric(temp[-1:-3,temp[1,]=="HH_TOTINCAT" & temp[2,]%in%c(16)])
# write_csv(geo_household_marginals,"Exported/A1/Montreal/Scenarios/S5 - bCT_DA/geo_household_marginals.csv",col_names = FALSE)
# #geo_person_marginals
# geo_person_marginals<-merge(DAMAT[grepl("PP_",DAMAT$Variable),],pp_variable_importance,by="Variable",all.x=TRUE)
# geo_person_marginals<-geo_person_marginals[order(geo_person_marginals$Importance),]
# geo_person_marginals<-geo_person_marginals[geo_person_marginals$Importance!=0,!names(geo_person_marginals)%in%c("Definition","X","Description","Frequency","Importance")]
# geo_person_marginals$Categories[geo_person_marginals$Variable=="PP_ID"]<-1
# geo_person_marginals<-rbind(c("variable_names","variable_categories",1:length(unique(GIS$DA))),geo_person_marginals)
# geo_person_marginals<-cbind(geo_person_marginals[,1:2],c("geo",rep("",nrow(geo_person_marginals)-1)),geo_person_marginals[,3:ncol(geo_person_marginals)])
# geo_person_marginals$Variable[-1]<-geo_person_marginals$Variable[-1]
# geo_person_marginals<-data.frame(t(geo_person_marginals))
# temp<-geo_person_marginals
# geo_person_marginals<-temp[,!((temp[1,]%in%c("PP_AGEGRP") & temp[2,]%in%c(4:13)))]
# geo_person_marginals[-1:-3,(geo_person_marginals[1,]=="PP_AGEGRP" & geo_person_marginals[2,]==1)]<-rowSums(apply(temp[-1:-3,temp[1,]=="PP_AGEGRP" & temp[2,]%in%c(1:2)],2,as.numeric))
# geo_person_marginals[-1:-3,(geo_person_marginals[1,]=="PP_AGEGRP" & geo_person_marginals[2,]==2)]<-rowSums(apply(temp[-1:-3,temp[1,]=="PP_AGEGRP" & temp[2,]%in%c(3:11)],2,as.numeric))
# geo_person_marginals[-1:-3,(geo_person_marginals[1,]=="PP_AGEGRP" & geo_person_marginals[2,]==3)]<-rowSums(apply(temp[-1:-3,temp[1,]=="PP_AGEGRP" & temp[2,]%in%c(12:13)],2,as.numeric))
# write_csv(geo_person_marginals,"Exported/A1/Montreal/Scenarios/S5 - bCT_DA/geo_person_marginals.csv",col_names = FALSE)
# #household_sample
# write_csv(household_sample,"Exported/A1/Montreal/Scenarios/S5 - bCT_DA/household_sample.csv",col_names = TRUE)
# #person_sample
# write_csv(person_sample,"Exported/A1/Montreal/Scenarios/S5 - bCT_DA/person_sample.csv",col_names = TRUE)
# 
# 
# ###S6 - hCT_DA
# #region_geo_mapping
# region_geo_mapping<-data.frame("region"=GIS$CT,"geo"=GIS$DA)
# temp<-region_geo_mapping
# region_geo_mapping$region[1]<-1
# for(i in 2:nrow(region_geo_mapping)){
#   ifelse(temp$region[i]==temp$region[i-1],region_geo_mapping$region[i]<-region_geo_mapping$region[i-1],region_geo_mapping$region[i]<-region_geo_mapping$region[i-1]+1)
# }
# region_geo_mapping[order(region_geo_mapping$geo),]$geo<-1:nrow(region_geo_mapping)
# write.csv(region_geo_mapping,"Exported/A1/Montreal/Scenarios/S6 - hCT_DA/region_geo_mapping.csv",row.names=FALSE)
# #geo_sample_mapping
# geo<-1:length(GIS$DA)
# sample_geo<-1
# geo_sample_mapping<-data.frame(geo,sample_geo)
# write.csv(geo_sample_mapping,"Exported/A1/Montreal/Scenarios/S6 - hCT_DA/geo_sample_mapping.csv",row.names=FALSE)
# #region_sample_mapping
# region<-1:length(unique(GIS$CT))
# sample_geo<--1
# region_sample_mapping<-data.frame(region,sample_geo)
# write.csv(region_sample_mapping,"Exported/A1/Montreal/Scenarios/S6 - hCT_DA/region_sample_mapping.csv",row.names=FALSE)
# #region_household_marginals
# region_household_marginals<-merge(hCTMAT[grepl("HH_",hCTMAT$Variable),],hh_variable_importance,by="Variable",all.x=TRUE)
# region_household_marginals<-region_household_marginals[order(region_household_marginals$Importance),]
# region_household_marginals<-region_household_marginals[region_household_marginals$Importance!=0,!names(region_household_marginals)%in%c("X","Definition","Description","Frequency","Importance")]
# region_household_marginals$Categories[region_household_marginals$Variable=="HH_ID"]<-1
# region_household_marginals<-rbind(c("variable_names","variable_categories",1:length(unique(GIS$CT))),region_household_marginals)
# region_household_marginals<-cbind(region_household_marginals[,1:2],c("region",rep("",nrow(region_household_marginals)-1)),region_household_marginals[,3:ncol(region_household_marginals)])
# region_household_marginals$Variable[-1]<-paste("r",region_household_marginals$Variable[-1],sep="")
# region_household_marginals<-data.frame(t(region_household_marginals))
# temp<-region_household_marginals
# region_household_marginals<-temp[,!((temp[1,]%in%c("rHH_TOTINCAT") & temp[2,]%in%c(5:16)))]
# region_household_marginals[-1:-3,(region_household_marginals[1,]=="rHH_TOTINCAT" & region_household_marginals[2,]==1)]<-rowSums(apply(temp[-1:-3,temp[1,]=="rHH_TOTINCAT" & temp[2,]%in%c(1:6)],2,as.numeric))
# region_household_marginals[-1:-3,(region_household_marginals[1,]=="rHH_TOTINCAT" & region_household_marginals[2,]==2)]<-rowSums(apply(temp[-1:-3,temp[1,]=="rHH_TOTINCAT" & temp[2,]%in%c(7:11)],2,as.numeric))
# region_household_marginals[-1:-3,(region_household_marginals[1,]=="rHH_TOTINCAT" & region_household_marginals[2,]==3)]<-rowSums(apply(temp[-1:-3,temp[1,]=="rHH_TOTINCAT" & temp[2,]%in%c(12:15)],2,as.numeric))
# region_household_marginals[-1:-3,(region_household_marginals[1,]=="rHH_TOTINCAT" & region_household_marginals[2,]==4)]<-as.numeric(temp[-1:-3,temp[1,]=="rHH_TOTINCAT" & temp[2,]%in%c(16)])
# write_csv(region_household_marginals,"Exported/A1/Montreal/Scenarios/S6 - hCT_DA/region_household_marginals.csv",col_names = FALSE)
# #region_person_marginals
# region_person_marginals<-merge(hCTMAT[grepl("PP_",hCTMAT$Variable),],pp_variable_importance,by="Variable",all.x=TRUE)
# region_person_marginals<-region_person_marginals[order(region_person_marginals$Importance),]
# region_person_marginals<-region_person_marginals[region_person_marginals$Importance!=0,!names(region_person_marginals)%in%c("X","Definition","Description","Frequency","Importance")]
# region_person_marginals$Categories[region_person_marginals$Variable=="PP_ID"]<-1
# region_person_marginals<-rbind(c("variable_names","variable_categories",1:length(unique(GIS$CT))),region_person_marginals)
# region_person_marginals<-cbind(region_person_marginals[,1:2],c("region",rep("",nrow(region_person_marginals)-1)),region_person_marginals[,3:ncol(region_person_marginals)])
# region_person_marginals$Variable[-1]<-paste("r",region_person_marginals$Variable[-1],sep="")
# region_person_marginals<-data.frame(t(region_person_marginals))
# temp<-region_person_marginals
# region_person_marginals<-temp[,!((temp[1,]%in%c("rPP_AGEGRP") & temp[2,]%in%c(4:13)))]
# region_person_marginals[-1:-3,(region_person_marginals[1,]=="rPP_AGEGRP" & region_person_marginals[2,]==1)]<-rowSums(apply(temp[-1:-3,temp[1,]=="rPP_AGEGRP" & temp[2,]%in%c(1:2)],2,as.numeric))
# region_person_marginals[-1:-3,(region_person_marginals[1,]=="rPP_AGEGRP" & region_person_marginals[2,]==2)]<-rowSums(apply(temp[-1:-3,temp[1,]=="rPP_AGEGRP" & temp[2,]%in%c(3:11)],2,as.numeric))
# region_person_marginals[-1:-3,(region_person_marginals[1,]=="rPP_AGEGRP" & region_person_marginals[2,]==3)]<-rowSums(apply(temp[-1:-3,temp[1,]=="rPP_AGEGRP" & temp[2,]%in%c(12:13)],2,as.numeric))
# write_csv(region_person_marginals,"Exported/A1/Montreal/Scenarios/S6 - hCT_DA/region_person_marginals.csv",col_names = FALSE)
# #geo_household_marginals
# geo_household_marginals<-merge(hDAMAT[grepl("HH_",hDAMAT$Variable),],hh_variable_importance,by="Variable",all.x=TRUE)
# geo_household_marginals<-geo_household_marginals[order(geo_household_marginals$Importance),]
# geo_household_marginals<-geo_household_marginals[geo_household_marginals$Importance!=0,!names(geo_household_marginals)%in%c("X","Definition","Description","Frequency","Importance")]
# geo_household_marginals$Categories[geo_household_marginals$Variable=="HH_ID"]<-1
# geo_household_marginals<-rbind(c("variable_names","variable_categories",1:length(unique(GIS$DA))),geo_household_marginals)
# geo_household_marginals<-cbind(geo_household_marginals[,1:2],c("geo",rep("",nrow(geo_household_marginals)-1)),geo_household_marginals[,3:ncol(geo_household_marginals)])
# geo_household_marginals<-data.frame(t(geo_household_marginals))
# temp<-geo_household_marginals
# geo_household_marginals<-temp[,!((temp[1,]%in%c("HH_TOTINCAT") & temp[2,]%in%c(5:16)))]
# geo_household_marginals[-1:-3,(geo_household_marginals[1,]=="HH_TOTINCAT" & geo_household_marginals[2,]==1)]<-rowSums(apply(temp[-1:-3,temp[1,]=="HH_TOTINCAT" & temp[2,]%in%c(1:6)],2,as.numeric))
# geo_household_marginals[-1:-3,(geo_household_marginals[1,]=="HH_TOTINCAT" & geo_household_marginals[2,]==2)]<-rowSums(apply(temp[-1:-3,temp[1,]=="HH_TOTINCAT" & temp[2,]%in%c(7:11)],2,as.numeric))
# geo_household_marginals[-1:-3,(geo_household_marginals[1,]=="HH_TOTINCAT" & geo_household_marginals[2,]==3)]<-rowSums(apply(temp[-1:-3,temp[1,]=="HH_TOTINCAT" & temp[2,]%in%c(12:15)],2,as.numeric))
# geo_household_marginals[-1:-3,(geo_household_marginals[1,]=="HH_TOTINCAT" & geo_household_marginals[2,]==4)]<-as.numeric(temp[-1:-3,temp[1,]=="HH_TOTINCAT" & temp[2,]%in%c(16)])
# write_csv(geo_household_marginals,"Exported/A1/Montreal/Scenarios/S6 - hCT_DA/geo_household_marginals.csv",col_names = FALSE)
# #geo_person_marginals
# geo_person_marginals<-merge(hDAMAT[grepl("PP_",hDAMAT$Variable),],pp_variable_importance,by="Variable",all.x=TRUE)
# geo_person_marginals<-geo_person_marginals[order(geo_person_marginals$Importance),]
# geo_person_marginals<-geo_person_marginals[geo_person_marginals$Importance!=0,!names(geo_person_marginals)%in%c("X","Definition","Description","Frequency","Importance")]
# geo_person_marginals$Categories[geo_person_marginals$Variable=="PP_ID"]<-1
# geo_person_marginals<-rbind(c("variable_names","variable_categories",1:length(unique(GIS$DA))),geo_person_marginals)
# geo_person_marginals<-cbind(geo_person_marginals[,1:2],c("geo",rep("",nrow(geo_person_marginals)-1)),geo_person_marginals[,3:ncol(geo_person_marginals)])
# geo_person_marginals<-data.frame(t(geo_person_marginals))
# temp<-geo_person_marginals
# geo_person_marginals<-temp[,!((temp[1,]%in%c("PP_AGEGRP") & temp[2,]%in%c(4:13)))]
# geo_person_marginals[-1:-3,(geo_person_marginals[1,]=="PP_AGEGRP" & geo_person_marginals[2,]==1)]<-rowSums(apply(temp[-1:-3,temp[1,]=="PP_AGEGRP" & temp[2,]%in%c(1:2)],2,as.numeric))
# geo_person_marginals[-1:-3,(geo_person_marginals[1,]=="PP_AGEGRP" & geo_person_marginals[2,]==2)]<-rowSums(apply(temp[-1:-3,temp[1,]=="PP_AGEGRP" & temp[2,]%in%c(3:11)],2,as.numeric))
# geo_person_marginals[-1:-3,(geo_person_marginals[1,]=="PP_AGEGRP" & geo_person_marginals[2,]==3)]<-rowSums(apply(temp[-1:-3,temp[1,]=="PP_AGEGRP" & temp[2,]%in%c(12:13)],2,as.numeric))
# write_csv(geo_person_marginals,"Exported/A1/Montreal/Scenarios/S6 - hCT_DA/geo_person_marginals.csv",col_names = FALSE)
# #household_sample
# write_csv(household_sample,"Exported/A1/Montreal/Scenarios/S6 - hCT_DA/household_sample.csv",col_names = TRUE)
# #person_sample
# write_csv(person_sample,"Exported/A1/Montreal/Scenarios/S6 - hCT_DA/person_sample.csv",col_names = TRUE)
