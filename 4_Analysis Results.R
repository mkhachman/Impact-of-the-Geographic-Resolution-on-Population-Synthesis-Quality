library(data.table)
library(ggplot2)
MontrealR<-fread("D:/OneDrive - polymtl.ca/Studies/Civil engineering/POLY/Projet Doctorat/PSP/Exported/A1/3. Analysis/Montreal/LastResults.csv")
TorontoR<-fread("D:/OneDrive - polymtl.ca/Studies/Civil engineering/POLY/Projet Doctorat/PSP/Exported/A1/3. Analysis/Toronto/LastResults.csv")
VancouverR<-fread("D:/OneDrive - polymtl.ca/Studies/Civil engineering/POLY/Projet Doctorat/PSP/Exported/A1/3. Analysis/Vancouver/LastResults.csv")

MontrealR$DR<-round(rep(c(rep(c(1),10),rep(c(90),10),rep(c(519),10),rep(c(951),10),rep(c(6428),10)),4)*100/6428,2)

MontrealR_Raw1L<-MontrealR[1:50,]
MontrealR_Raw1L<-MontrealR_Raw1L[!grepl("est",MontrealR_Raw1L$error),]
MontrealR_Raw1L<-MontrealR_Raw1L[grepl("2",MontrealR_Raw1L$error),]
MontrealR_Raw1L<-MontrealR_Raw1L[order(MontrealR_Raw1L$error),]

MontrealR_Raw2L<-MontrealR[51:100,]
MontrealR_Raw2L<-MontrealR_Raw2L[!grepl("est",MontrealR_Raw2L$error),]
MontrealR_Raw2L<-MontrealR_Raw2L[grepl("2",MontrealR_Raw2L$error),]
MontrealR_Raw2L<-MontrealR_Raw2L[order(MontrealR_Raw2L$error),]

MontrealR_Har1L<-MontrealR[101:150,]
MontrealR_Har1L<-MontrealR_Har1L[!grepl("est",MontrealR_Har1L$error),]
MontrealR_Har1L<-MontrealR_Har1L[grepl("2",MontrealR_Har1L$error),]
MontrealR_Har1L<-MontrealR_Har1L[order(MontrealR_Har1L$error),]

MontrealR_Har2L<-MontrealR[151:200,]
MontrealR_Har2L<-MontrealR_Har2L[!grepl("est",MontrealR_Har2L$error),]
MontrealR_Har2L<-MontrealR_Har2L[grepl("2",MontrealR_Har2L$error),]
MontrealR_Har2L<-MontrealR_Har2L[order(MontrealR_Har2L$error),]

TorontoR$DR<-round(rep(c(rep(c(1),10),rep(c(24),10),rep(c(663),10),rep(c(1148),10),rep(c(7497),10)),4)*100/7497,2)
TorontoR_Raw1L<-TorontoR[1:50,]
TorontoR_Raw1L<-TorontoR_Raw1L[!grepl("est",TorontoR_Raw1L$error),]
TorontoR_Raw1L<-TorontoR_Raw1L[grepl("2",TorontoR_Raw1L$error),]
TorontoR_Raw1L<-TorontoR_Raw1L[order(TorontoR_Raw1L$error),]

TorontoR_Raw2L<-TorontoR[51:100,]
TorontoR_Raw2L<-TorontoR_Raw2L[!grepl("est",TorontoR_Raw2L$error),]
TorontoR_Raw2L<-TorontoR_Raw2L[grepl("2",TorontoR_Raw2L$error),]
TorontoR_Raw2L<-TorontoR_Raw2L[order(TorontoR_Raw2L$error),]

TorontoR_Har1L<-TorontoR[101:150,]
TorontoR_Har1L<-TorontoR_Har1L[!grepl("est",TorontoR_Har1L$error),]
TorontoR_Har1L<-TorontoR_Har1L[grepl("2",TorontoR_Har1L$error),]
TorontoR_Har1L<-TorontoR_Har1L[order(TorontoR_Har1L$error),]

TorontoR_Har2L<-TorontoR[151:200,]
TorontoR_Har2L<-TorontoR_Har2L[!grepl("est",TorontoR_Har2L$error),]
TorontoR_Har2L<-TorontoR_Har2L[grepl("2",TorontoR_Har2L$error),]
TorontoR_Har2L<-TorontoR_Har2L[order(TorontoR_Har2L$error),]

VancouverR$DR<-round(rep(c(rep(c(1),10),rep(c(34),10),rep(c(306),10),rep(c(472),10),rep(c(3435),10)),4)*100/3435,2)
VancouverR_Raw1L<-VancouverR[1:50,]
VancouverR_Raw1L<-VancouverR_Raw1L[!grepl("est",VancouverR_Raw1L$error),]
VancouverR_Raw1L<-VancouverR_Raw1L[grepl("2",VancouverR_Raw1L$error),]
VancouverR_Raw1L<-VancouverR_Raw1L[order(VancouverR_Raw1L$error),]

VancouverR_Raw2L<-VancouverR[51:100,]
VancouverR_Raw2L<-VancouverR_Raw2L[!grepl("est",VancouverR_Raw2L$error),]
VancouverR_Raw2L<-VancouverR_Raw2L[grepl("2",VancouverR_Raw2L$error),]
VancouverR_Raw2L<-VancouverR_Raw2L[order(VancouverR_Raw2L$error),]

VancouverR_Har1L<-VancouverR[101:150,]
VancouverR_Har1L<-VancouverR_Har1L[!grepl("est",VancouverR_Har1L$error),]
VancouverR_Har1L<-VancouverR_Har1L[grepl("2",VancouverR_Har1L$error),]
VancouverR_Har1L<-VancouverR_Har1L[order(VancouverR_Har1L$error),]

VancouverR_Har2L<-VancouverR[151:200,]
VancouverR_Har2L<-VancouverR_Har2L[!grepl("est",VancouverR_Har2L$error),]
VancouverR_Har2L<-VancouverR_Har2L[grepl("2",VancouverR_Har2L$error),]
VancouverR_Har2L<-VancouverR_Har2L[order(VancouverR_Har2L$error),]

# MontrealR_Raw1L[MontrealR_Raw1L$error=="selSpaErr2","hhcon"]<-MontrealR_Raw1L[MontrealR_Raw1L$error=="selSpaErr2",hhcon]*
#   max(MontrealR_Raw1L[MontrealR_Raw1L$error=="selFitErr2",hhcon])/
#   max(MontrealR_Raw1L[MontrealR_Raw1L$error=="selSpaErr2",hhcon])
# 
# MontrealR_Raw1L[MontrealR_Raw1L$error=="selSpaErr2","ppcon"]<-MontrealR_Raw1L[MontrealR_Raw1L$error=="selSpaErr2",ppcon]*
#   max(MontrealR_Raw1L[MontrealR_Raw1L$error=="selFitErr2",ppcon])/
#   max(MontrealR_Raw1L[MontrealR_Raw1L$error=="selSpaErr2",ppcon])
# 
# MontrealR_Raw1L[MontrealR_Raw1L$error=="selSpaErr2","totcon"]<-MontrealR_Raw1L[MontrealR_Raw1L$error=="selSpaErr2",totcon]*
#   max(MontrealR_Raw1L[MontrealR_Raw1L$error=="selFitErr2",totcon])/
#   max(MontrealR_Raw1L[MontrealR_Raw1L$error=="selSpaErr2",totcon])
# 
# 
# MontrealR_Raw1L[MontrealR_Raw1L$error%in%c("selFitErr2","selSpaErr2"),.(Scenario,error,DR,hhcon,ppcon,totcon)]

b<-data.table()

for (a in seq(0,1,0.1)){
  b<-rbind(b,
    cbind(rep(a,5),(1-a)*VancouverR_Har2L[VancouverR_Har2L$error=="selFitErr2",.(hhcon,ppcon,totcon)]+
    a*VancouverR_Har2L[VancouverR_Har2L$error=="selSpaErr2",.(hhcon,ppcon,totcon)])
    )
}
b<-cbind(RR=rep(c("CMA","CSD","ADA","CT","DA"),11),b)
names(b)[2]<-"SpaWeight"
b<-cbind(DR=rep(VancouverR_Har2L$DR[1:5],11),b)

b




















# 
# 2*MontrealR_Raw1L[MontrealR_Raw1L$error=="selFitErr2",.(hhcon,ppcon,totcon)]*
#   MontrealR_Raw1L[MontrealR_Raw1L$error=="selSpaErr2",.(hhcon,ppcon,totcon)]/
#   (MontrealR_Raw1L[MontrealR_Raw1L$error=="selFitErr2",.(hhcon,ppcon,totcon)]+
#      MontrealR_Raw1L[MontrealR_Raw1L$error=="selSpaErr2",.(hhcon,ppcon,totcon)])
# 
# 
# 0.9*round(sweep(MontrealR_Raw1L[MontrealR_Raw1L$error=="selFitErr2",.(hhcon,ppcon,totcon)],2,apply(MontrealR_Raw1L[MontrealR_Raw1L$error=="selFitErr2",.(hhcon,ppcon,totcon)],2,max),"/")*100,2) +
#   0.1*round(sweep(MontrealR_Raw1L[MontrealR_Raw1L$error=="selSpaErr2",.(hhcon,ppcon,totcon)],2,apply(MontrealR_Raw1L[MontrealR_Raw1L$error=="selSpaErr2",.(hhcon,ppcon,totcon)],2,max),"/")*100,2)
# 
# 
# 
# 
# 2*round(sweep(MontrealR_Raw1L[MontrealR_Raw1L$error=="selFitErr2",.(hhcon,ppcon,totcon)],2,apply(MontrealR_Raw1L[MontrealR_Raw1L$error=="selFitErr2",.(hhcon,ppcon,totcon)],2,max),"/")*100,2)*
#   round(sweep(MontrealR_Raw1L[MontrealR_Raw1L$error=="selSpaErr2",.(hhcon,ppcon,totcon)],2,apply(MontrealR_Raw1L[MontrealR_Raw1L$error=="selSpaErr2",.(hhcon,ppcon,totcon)],2,max),"/")*100,2)/
#   (round(sweep(MontrealR_Raw1L[MontrealR_Raw1L$error=="selFitErr2",.(hhcon,ppcon,totcon)],2,apply(MontrealR_Raw1L[MontrealR_Raw1L$error=="selFitErr2",.(hhcon,ppcon,totcon)],2,max),"/")*100,2)+
#      round(sweep(MontrealR_Raw1L[MontrealR_Raw1L$error=="selSpaErr2",.(hhcon,ppcon,totcon)],2,apply(MontrealR_Raw1L[MontrealR_Raw1L$error=="selSpaErr2",.(hhcon,ppcon,totcon)],2,max),"/")*100,2))
# 
# 
# 
# 
# 
#   
# 
# 
# 
# 
# 
# plot(y=MontrealR_Raw1L$hhcon[MontrealR_Raw1L$error=="deltaCensusErr2"], x=MontrealR_Raw1L$DR[MontrealR_Raw1L$error=="deltaCensusErr2"],type="o",col="blue", ylim=c(min(c(MontrealR_Raw1L$hhcon[MontrealR_Raw1L$error=="deltaCensusErr2"],MontrealR_Raw1L$ppcon[MontrealR_Raw1L$error=="deltaCensusErr2"],MontrealR_Raw1L$totcon[MontrealR_Raw1L$error=="deltaCensusErr2"])),max(c(MontrealR_Raw1L$hhcon[MontrealR_Raw1L$error=="deltaCensusErr2"],MontrealR_Raw1L$ppcon[MontrealR_Raw1L$error=="deltaCensusErr2"],MontrealR_Raw1L$totcon[MontrealR_Raw1L$error=="deltaCensusErr2"]))))
# lines(y=MontrealR_Raw1L$ppcon[MontrealR_Raw1L$error=="deltaCensusErr2"], x=MontrealR_Raw1L$DR[MontrealR_Raw1L$error=="deltaCensusErr2"],type="o", col="red")
# lines(y=MontrealR_Raw1L$totcon[MontrealR_Raw1L$error=="deltaCensusErr2"], x=MontrealR_Raw1L$DR[MontrealR_Raw1L$error=="deltaCensusErr2"],type="o", col="black")
# 
# 
# par(mfrow=c(3,2))
# 
# plot(y=MontrealR_Raw1L$hhcon[MontrealR_Raw1L$error=="selFitErr2"], x=MontrealR_Raw1L$DR[MontrealR_Raw1L$error=="selFitErr2"],type="o",col="blue", ylim=c(min(c(MontrealR_Raw1L$hhcon[MontrealR_Raw1L$error=="selFitErr2"],MontrealR_Raw1L$ppcon[MontrealR_Raw1L$error=="selFitErr2"],MontrealR_Raw1L$totcon[MontrealR_Raw1L$error=="selFitErr2"])),max(c(MontrealR_Raw1L$hhcon[MontrealR_Raw1L$error=="selFitErr2"],MontrealR_Raw1L$ppcon[MontrealR_Raw1L$error=="selFitErr2"],MontrealR_Raw1L$totcon[MontrealR_Raw1L$error=="selFitErr2"]))))
# lines(y=MontrealR_Raw1L$ppcon[MontrealR_Raw1L$error=="selFitErr2"], x=MontrealR_Raw1L$DR[MontrealR_Raw1L$error=="selFitErr2"],type="o", col="red")
# lines(y=MontrealR_Raw1L$totcon[MontrealR_Raw1L$error=="selFitErr2"], x=MontrealR_Raw1L$DR[MontrealR_Raw1L$error=="selFitErr2"],type="o", col="black")
# 
# plot(y=MontrealR_Raw1L$hhcon[MontrealR_Raw1L$error=="selSpaErr2"], x=MontrealR_Raw1L$DR[MontrealR_Raw1L$error=="selSpaErr2"],type="o",col="blue", ylim=c(min(c(MontrealR_Raw1L$hhcon[MontrealR_Raw1L$error=="selSpaErr2"],MontrealR_Raw1L$ppcon[MontrealR_Raw1L$error=="selSpaErr2"],MontrealR_Raw1L$totcon[MontrealR_Raw1L$error=="selSpaErr2"])),max(c(MontrealR_Raw1L$hhcon[MontrealR_Raw1L$error=="selSpaErr2"],MontrealR_Raw1L$ppcon[MontrealR_Raw1L$error=="selSpaErr2"],MontrealR_Raw1L$totcon[MontrealR_Raw1L$error=="selSpaErr2"]))))
# lines(y=MontrealR_Raw1L$ppcon[MontrealR_Raw1L$error=="selSpaErr2"], x=MontrealR_Raw1L$DR[MontrealR_Raw1L$error=="selSpaErr2"],type="o", col="red")
# lines(y=MontrealR_Raw1L$totcon[MontrealR_Raw1L$error=="selSpaErr2"], x=MontrealR_Raw1L$DR[MontrealR_Raw1L$error=="selSpaErr2"],type="o", col="black")
# 
# plot(y=TorontoR_Raw1L$hhcon[TorontoR_Raw1L$error=="selFitErr2"], x=TorontoR_Raw1L$DR[TorontoR_Raw1L$error=="selFitErr2"],type="o",col="blue", ylim=c(min(c(TorontoR_Raw1L$hhcon[TorontoR_Raw1L$error=="selFitErr2"],TorontoR_Raw1L$ppcon[TorontoR_Raw1L$error=="selFitErr2"],TorontoR_Raw1L$totcon[TorontoR_Raw1L$error=="selFitErr2"])),max(c(TorontoR_Raw1L$hhcon[TorontoR_Raw1L$error=="selFitErr2"],TorontoR_Raw1L$ppcon[TorontoR_Raw1L$error=="selFitErr2"],TorontoR_Raw1L$totcon[TorontoR_Raw1L$error=="selFitErr2"]))))
# lines(y=TorontoR_Raw1L$ppcon[TorontoR_Raw1L$error=="selFitErr2"], x=TorontoR_Raw1L$DR[TorontoR_Raw1L$error=="selFitErr2"],type="o", col="red")
# lines(y=TorontoR_Raw1L$totcon[TorontoR_Raw1L$error=="selFitErr2"], x=TorontoR_Raw1L$DR[TorontoR_Raw1L$error=="selFitErr2"],type="o", col="black")
# 
# plot(y=TorontoR_Raw1L$hhcon[TorontoR_Raw1L$error=="selSpaErr2"], x=TorontoR_Raw1L$DR[TorontoR_Raw1L$error=="selSpaErr2"],type="o",col="blue", ylim=c(min(c(TorontoR_Raw1L$hhcon[TorontoR_Raw1L$error=="selSpaErr2"],TorontoR_Raw1L$ppcon[TorontoR_Raw1L$error=="selSpaErr2"],TorontoR_Raw1L$totcon[TorontoR_Raw1L$error=="selSpaErr2"])),max(c(TorontoR_Raw1L$hhcon[TorontoR_Raw1L$error=="selSpaErr2"],TorontoR_Raw1L$ppcon[TorontoR_Raw1L$error=="selSpaErr2"],TorontoR_Raw1L$totcon[TorontoR_Raw1L$error=="selSpaErr2"]))))
# lines(y=TorontoR_Raw1L$ppcon[TorontoR_Raw1L$error=="selSpaErr2"], x=TorontoR_Raw1L$DR[TorontoR_Raw1L$error=="selSpaErr2"],type="o", col="red")
# lines(y=TorontoR_Raw1L$totcon[TorontoR_Raw1L$error=="selSpaErr2"], x=TorontoR_Raw1L$DR[TorontoR_Raw1L$error=="selSpaErr2"],type="o", col="black")
# 
# plot(y=VancouverR_Raw1L$hhcon[VancouverR_Raw1L$error=="selFitErr2"], x=VancouverR_Raw1L$DR[VancouverR_Raw1L$error=="selFitErr2"],type="o",col="blue", ylim=c(min(c(VancouverR_Raw1L$hhcon[VancouverR_Raw1L$error=="selFitErr2"],VancouverR_Raw1L$ppcon[VancouverR_Raw1L$error=="selFitErr2"],VancouverR_Raw1L$totcon[VancouverR_Raw1L$error=="selFitErr2"])),max(c(VancouverR_Raw1L$hhcon[VancouverR_Raw1L$error=="selFitErr2"],VancouverR_Raw1L$ppcon[VancouverR_Raw1L$error=="selFitErr2"],VancouverR_Raw1L$totcon[VancouverR_Raw1L$error=="selFitErr2"]))))
# lines(y=VancouverR_Raw1L$ppcon[VancouverR_Raw1L$error=="selFitErr2"], x=VancouverR_Raw1L$DR[VancouverR_Raw1L$error=="selFitErr2"],type="o", col="red")
# lines(y=VancouverR_Raw1L$totcon[VancouverR_Raw1L$error=="selFitErr2"], x=VancouverR_Raw1L$DR[VancouverR_Raw1L$error=="selFitErr2"],type="o", col="black")
# 
# plot(y=VancouverR_Raw1L$hhcon[VancouverR_Raw1L$error=="selSpaErr2"], x=VancouverR_Raw1L$DR[VancouverR_Raw1L$error=="selSpaErr2"],type="o",col="blue", ylim=c(min(c(VancouverR_Raw1L$hhcon[VancouverR_Raw1L$error=="selSpaErr2"],VancouverR_Raw1L$ppcon[VancouverR_Raw1L$error=="selSpaErr2"],VancouverR_Raw1L$totcon[VancouverR_Raw1L$error=="selSpaErr2"])),max(c(VancouverR_Raw1L$hhcon[VancouverR_Raw1L$error=="selSpaErr2"],VancouverR_Raw1L$ppcon[VancouverR_Raw1L$error=="selSpaErr2"],VancouverR_Raw1L$totcon[VancouverR_Raw1L$error=="selSpaErr2"]))))
# lines(y=VancouverR_Raw1L$ppcon[VancouverR_Raw1L$error=="selSpaErr2"], x=VancouverR_Raw1L$DR[VancouverR_Raw1L$error=="selSpaErr2"],type="o", col="red")
# lines(y=VancouverR_Raw1L$totcon[VancouverR_Raw1L$error=="selSpaErr2"], x=VancouverR_Raw1L$DR[VancouverR_Raw1L$error=="selSpaErr2"],type="o", col="black")


install.packages("TIMP")
install.packages("drc")
install.packages("aomisc")
library(drc)
library(nlme)
library(aomisc)
library(TIMP)

V1<-c(1,90,519,951,6428)
V2<-c(0.1,1.59,3.7,5.01,17.54)
DT1<-data.frame(V1,V2)

# f=fitModel(V2~A*V1^B,data=DT1)


# model <- drm(V2 ~ V1, fct = DRC.powerCurve(),data = DT1)
library(MASS)
nls(V2~b*V1^z,start = list(b = 0.1, z = 0.1),data=DT1)

