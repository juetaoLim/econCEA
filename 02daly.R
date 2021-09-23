#parameters
rm(list=ls())
library(readxl)
library(dplyr)
load("~/cea/data/daly.Rdata")  #Rdata file with individuals age and year as dataframe
deaths <- read.csv("~/cea/data/deaths.csv")
costs <- read_excel("~/cea/data/CEADetails.xlsx")
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
r <- 0.03 
C <- 0.16243
beta <- 0.04
reportedDF_durDis <- 10.4/365
unreportedDF_durDis <- 4/365
DHF_durdis <- 14/365
propDHF <- 0.358
propHospitalized <- 0.565
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
  #params
  D_child <- runif(1,0.37,0.52)
  D_adult <- runif(1,0.42,0.53)
  D_DF <- runif(1,0.211,0.81)
  D_DHF <- runif(1,0.5,0.85)
  
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
  
  #symptomatic rate to adjust expansion factor
  symp <- runif(1,min=0.24,max=0.53)
  
  #expand dalys based on expansion factor and prop hosp
  #constant AEF * proportion in that group
  ef <- dftemp$AGE
  scaler <- dftemp$AP
  #compute disease burden, true number of infections, method 1
  ef[ef < 24] <- runif(ef[ef < 24],1.7,3.6) * scaler[ef < 24]
  ef[ef %in% c(25:34)]  <- runif(ef[ef %in% c(25:34)],3.8,8.2)* scaler[ef %in% c(25:34)]
  ef[ef %in% c(35:44)]  <- runif(ef[ef %in% c(35:44)],6.1,13.4)* scaler[ef %in% c(35:44)]
  ef[ef %in% c(45:54)]  <- runif(ef[ef %in% c(45:54)],11.1,24.2)* scaler[ef %in% c(45:54)]
  ef[ef %in% c(55:999)] <- runif(ef[ef %in% c(55:999)],12.2,26.5)* scaler[ef %in% c(55:999)]
  
  #age-dependent AEF from data * proportion in that group
  ef1 <- dftemp$AGE
  #compute disease burden, true number of infections, method 2
  ef1[ef1 < 24] <- 3.8 * scaler[ef1 < 24]
  ef1[ef1 %in% c(25:34)]  <- 13.1*scaler[ef1 %in% c(25:34)]
  ef1[ef1 %in% c(35:44)]  <- 24.3*scaler[ef1 %in% c(35:44)]
  ef1[ef1 %in% c(45:54)]  <- 45.3*scaler[ef1 %in% c(45:54)]
  ef1[ef1 %in% c(55:999)] <- 50*scaler[ef1 %in% c(55:999)]
  
  HEF <- 1 #hospital expansion factor, conservative
  
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
                      age=dftemp$AGE,diag=dftemp$DIAG,
                      onset=dftemp$ONSET)
  out <- list(dalys=dalys,deathDaly = dalysDeath,nTrue1=nTrue1,nTrue2=nTrue2)
  return(out)
}

iters <- 500
store <- list()
store2 <- list()
storeN <- list()

store_breakdown1 <- list()
store_breakdown2 <- list()
store_breakdown3 <- list()
store_breakdown4 <- list()

for (i in 1:iters){
days <- gen_Daly()
store[[i]] <- colSums(days[[1]][,1:4])
store2[[i]] <- sum(days[[2]])
storeN[[i]] <- c(days$nTrue1,days$nTrue2)

#by year
temp <- days$dalys
temp$age[which(temp$age >60)] <- ">60"
temp$age[which(temp$age %in% c(0:10))] <- "0-10"
temp$age[which(temp$age %in% c(11:20))] <- "11-20"
temp$age[which(temp$age %in% c(21:60))] <- "21-60"


store_breakdown1[[i]] <- xtabs(dalys_ageDW_efConst~age+onset,data=temp)
store_breakdown2[[i]] <- xtabs(dalys_ageDW_efAgeDep~age+onset,data=temp)
store_breakdown3[[i]] <- xtabs(dalys_constDW_efConst~age+onset,data=temp)
store_breakdown4[[i]] <- xtabs(dalys_constDW_efAgeDep~age+onset,data=temp)
}


store_breakdown1 <- Reduce("+", store_breakdown1)/length(store_breakdown1)
store_breakdown2 <- Reduce("+", store_breakdown2)/length(store_breakdown2)
store_breakdown3 <- Reduce("+", store_breakdown3)/length(store_breakdown3)
store_breakdown4 <- Reduce("+", store_breakdown4)/length(store_breakdown4)

outBreakdown <- list(dalys_ageDW_efConst=store_breakdown1,
                     dalys_ageDW_efAgeDep=store_breakdown2,
                     dalys_constDW_efConst=store_breakdown3,
                     dalys_constDW_efAgeDep=store_breakdown4)
save(outBreakdown,file="~/cea/DalysBreakdown.RData")

store <- do.call(rbind,store)
store <- apply(store, MARGIN=2,function(x) quantile(x,probs=c(0.5,0.025,0.975)))
colnames(store) <- c("Age Dependent Disability Weight, Constant Expansion",
                     "Age Dependent Disability Weight, Age Dependent ExpansionFactor",
                     "Constant Disability Weight, Constant Expansion",
                     "Constant Disability Weight, Age Dependent Expansion Factor")
save(store,file="~/cea/DalysMain.RData")
#breakeven computation#
# 40000000*11/store[1,1]
