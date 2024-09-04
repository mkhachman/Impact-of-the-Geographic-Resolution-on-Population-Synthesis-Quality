#CMApp
Montrealpp<-read.csv("Exported/A1/3. Analysis/Montreal/S1-bCMA/region_person_marginals.csv")$rPP_ID[-1:-2]
Torontopp<-read.csv("Exported/A1/3. Analysis/Toronto/S1-bCMA/region_person_marginals.csv")$rPP_ID[-1:-2]
Vancouverpp<-read.csv("Exported/A1/3. Analysis/Vancouver/S1-bCMA/region_person_marginals.csv")$rPP_ID[-1:-2]
CMApp<-Montrealpp+Torontopp+Vancouverpp
PPI_CMA<-1
CMApp<-c(Montrealpp, Torontopp, Vancouverpp)
mCMApp<-mean(CMApp)

#CSDpp
Montrealpp<-read.csv("Exported/A1/3. Analysis/Montreal/S2-bCSD/region_person_marginals.csv")$rPP_ID[-1:-2]
Torontopp<-read.csv("Exported/A1/3. Analysis/Toronto/S2-bCSD/region_person_marginals.csv")$rPP_ID[-1:-2]
Vancouverpp<-read.csv("Exported/A1/3. Analysis/Vancouver/S2-bCSD/region_person_marginals.csv")$rPP_ID[-1:-2]
CSDpp<-c(Montrealpp,Torontopp,Vancouverpp)
PPI_CSD<-sum(as.numeric(CSDpp)*as.numeric(CSDpp)/CMApp)/CMApp
mCSDpp<-mean(CSDpp)

#ADA
Montrealpp<-read.csv("Exported/A1/3. Analysis/Montreal/S4-bADA/region_person_marginals.csv")$rPP_ID[-1:-2]
Torontopp<-read.csv("Exported/A1/3. Analysis/Toronto/S4-bADA/region_person_marginals.csv")$rPP_ID[-1:-2]
Vancouverpp<-read.csv("Exported/A1/3. Analysis/Vancouver/S4-bADA/region_person_marginals.csv")$rPP_ID[-1:-2]
ADApp<-c(Montrealpp,Torontopp,Vancouverpp)
PPI_ADA<-sum(as.numeric(ADApp)*as.numeric(ADApp)/CMApp)/CMApp
mADApp<-mean(ADApp)

#CT
Montrealpp<-read.csv("Exported/A1/3. Analysis/Montreal/S6-bCT/region_person_marginals.csv")$rPP_ID[-1:-2]
Torontopp<-read.csv("Exported/A1/3. Analysis/Toronto/S6-bCT/region_person_marginals.csv")$rPP_ID[-1:-2]
Vancouverpp<-read.csv("Exported/A1/3. Analysis/Vancouver/S6-bCT/region_person_marginals.csv")$rPP_ID[-1:-2]
CTpp<-c(Montrealpp,Torontopp,Vancouverpp)
PPI_CT<-sum(as.numeric(CTpp)*as.numeric(CTpp)/CMApp)/CMApp
PPI_CT
mCTpp<-mean(CTpp)

#DA
Montrealpp<-read.csv("Exported/A1/3. Analysis/Montreal/S8-bDA/region_person_marginals.csv")$rPP_ID[-1:-2]
Torontopp<-read.csv("Exported/A1/3. Analysis/Toronto/S8-bDA/region_person_marginals.csv")$rPP_ID[-1:-2]
Vancouverpp<-read.csv("Exported/A1/3. Analysis/Vancouver/S8-bDA/region_person_marginals.csv")$rPP_ID[-1:-2]
DApp<-c(Montrealpp,Torontopp,Vancouverpp)
PPI_DA<-sum(as.numeric(DApp)*as.numeric(DApp)/CMApp)/CMApp
mDApp<-mean(DApp)


PPI<-c(PPI_CMA,PPI_CSD,PPI_ADA,PPI_CT,PPI_DA)*100
PPI



mpp<-c(mCMApp, mCSDpp, mADApp, mCTpp, mDApp)

mCSDpp/mCMApp
mADApp/mCSDpp
mCTpp/mADApp
mDApp/mCTpp
