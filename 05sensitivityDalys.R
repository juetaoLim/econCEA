#parameters
rm(list=ls())
library(readxl)
load("~/cea/data/daly.Rdata")#Rdata file with individuals age and year as dataframe
costs <- read_excel("~/cea/data/CEADetails.xlsx")
deaths <- read.csv("~/cea/data/deaths.csv")
#get proportions hosp, ambulatory
total <- costs$CHAS_unique_patient+costs$POLY_unique_patient+costs$Hosp_unique_patient+costs$ED_unique_patient
AP    <- (costs$CHAS_unique_patient+costs$POLY_unique_patient)/total
EDP   <- costs$ED_unique_patient/total
HospP <- costs$Hosp_unique_patient/total
#match ap,edp,hosp  adjustments to each persons 
AP <- data.frame(AP=AP,ONSET=c(2010:2020))
EDP <- data.frame(EDP=EDP,ONSET=c(2010:2020))
HospP <- data.frame(HospP=HospP,ONSET=c(2010:2020))
dftemp <- merge(dftemp,AP,by="ONSET")
dftemp <- merge(dftemp,EDP,by="ONSET")
dftemp <- merge(dftemp,HospP,by="ONSET")

n <- length(dftemp$AGE)


#create dataframe

gen_Daly <- function(df=dftemp,
                     dfDeath=deaths,
                     weight){
  D1 <- NA
  D2 <- NA
  disDur <- NA
  df <- cbind(df,D1,D2,disDur)
  ind1 <- which(df$AGE<16)
  ind2 <- which(df$DIAG=="DHF")
  
  df$D1[ind1]      <- D_child
  df$D1[-ind1]     <- D_adult
  df$D2[ind2]      <- D_DF
  df$D2[-ind2]     <- D_DHF
  df$disDur[ind2]  <- DHF_durdis
  df$disDur[-ind2] <- reportedDF_durDis
  
  L <- df$disDur
  a <- df$AGE
  D1 <- df$D1
  D2 <- df$D2
  
  dalys <- exp(-(beta+r)*L)*(1+(beta+r)*(L+a)) - (1+(beta+r)*a)
  dalys_age <- -(D1*C*exp(-beta*a)/((beta+r)^2)) * dalys
  dalys_constant <- -(D2*C*exp(-beta*a)/((beta+r)^2)) * dalys
  
  #compute death dalys
  a <- dfDeath$AGE
  L <- 85 - a #death cases life years lost
  L[which(L<0)]<-0
  dalysDeath <- exp(-(beta+r)*L)*(1+(beta+r)*(L+a)) - (1+(beta+r)*a)
  dalysDeath <- -(1*C*exp(-beta*a)/((beta+r)^2)) * dalysDeath
  

  
  dalys_ageDW_efConst  <- dalys_age*ef + dalys_age*dftemp$HospP*HEF
  dalys_ageDW_efAgeDep <- dalys_age*ef1 + dalys_age*dftemp$HospP*HEF
  dalys_constDW_efConst  <- dalys_constant*ef + dalys_age*dftemp$HospP*HEF
  dalys_constDW_efAgeDep <- dalys_constant*ef1 + dalys_age*dftemp$HospP*HEF
  
  nTrue1 <- sum(ef) + sum(dftemp$HospP)
  nTrue2 <- sum(ef1) + sum(dftemp$HospP)
  
  dalys <- data.frame(dalys_ageDW_efConst=dalys_ageDW_efConst,
                      dalys_ageDW_efAgeDep=dalys_ageDW_efAgeDep,
                      dalys_constDW_efConst=dalys_constDW_efConst,
                      dalys_constDW_efAgeDep=dalys_constDW_efAgeDep,
                      age=dftemp$AGE,diag=dftemp$DIAG)
  out <- list(dalys=dalys,deathDaly = dalysDeath,nTrue1=nTrue1,nTrue2=nTrue2)
  return(out)
}




#expand dalys based on expansion factor and prop hosp
#constant AEF * proportion in that group
ef <- dftemp$AGE
scaler <- dftemp$AP
#compute disease burden, true number of infections, method 1
ef[ef < 24] <- 2.65 * scaler[ef < 24]
ef[ef %in% c(25:34)]  <- 6 * scaler[ef %in% c(25:34)]
ef[ef %in% c(35:44)]  <- 9.75 * scaler[ef %in% c(35:44)]
ef[ef %in% c(45:54)]  <- 17.65 * scaler[ef %in% c(45:54)]
ef[ef %in% c(55:999)] <- 19.35 * scaler[ef %in% c(55:999)]
#age-dependent AEF from data * proportion in that group
ef1 <- dftemp$AGE
#compute disease burden, true number of infections, method 2
ef1[ef1 < 24] <- 3.8 * scaler[ef1 < 24]
ef1[ef1 %in% c(25:34)]  <- 13.1*scaler[ef1 %in% c(25:34)]
ef1[ef1 %in% c(35:44)]  <- 24.3*scaler[ef1 %in% c(35:44)]
ef1[ef1 %in% c(45:54)]  <- 45.3*scaler[ef1 %in% c(45:54)]
ef1[ef1 %in% c(55:999)] <- 50*scaler[ef1 %in% c(55:999)]

params <- list(
D_child = 0.445,
D_adult = 0.475,
D_DF=0.5105,
D_DHF=0.675,
r=0.03 ,
C=0.16243,
beta=0.04,
reportedDF_durDis=10.4,
unreportedDF_durDis=4,
DHF_durdis=14,
propDHF=0.358,
propHospitalized=0.565,
ef=ef,ef1=ef1,
#symptomatic rate to adjust expansion factor
symp=0.385,
HEF=1) #hospital expansion factor, conservative

store <-  store2 <- storeN <- list()
for (j in 1:length(params)){
  temp <- params
  temp[[j]] <- temp[[j]] * 1.25
  list2env(temp,.GlobalEnv)
  
  dalys <- gen_Daly()
  store[[j]] <- colSums(dalys[[1]][,1:4])
  store2[[j]] <- sum(dalys[[2]])
  storeN[[j]] <- c(dalys$nTrue1,dalys$nTrue2)
}
store <- do.call(rbind,store)
colnames(store) <- c("Age Dependent Disability Weight, Constant Expansion",
                     "Age Dependent Disability Weight, Age Dependent ExpansionFactor",
                     "Constant Disability Weight, Constant Expansion",
                     "Constant Disability Weight, Age Dependent Expansion Factor")
rownames(store) <- paste(names(params),"+0.25%")
save(store,file="~/cea/sensitivityOutDalys.RData")
