rm(list=ls())
library(readxl)
load("~/cea/data/outCEA.Rdata")  #Rdata file with individuals age and year as dataframe
costs <- read_excel("~/cea/data/CEADetails.xlsx")
deaths <- read.csv("~/cea/data/deaths.csv")
#get base parameters#
#get base parameters#
#get base parameters#
PropChildCareGive <-  0.43
PropElderCareGive <-  0.073
DiscountRateDeath <- 0.03
daysAbsentAmb <- 4
AmbVisits <- 4.33 #number of ambulatory visits per episode
#base costs, all in usdsgd#
usdsgd <- costs$SGDUSD2010
transport            <- 3.7 #transport costs
HouseholdServiceLost <- 35
PriSchCost <- usdsgd*costs$PriSch/192 #2010 to 2020, per school day
SecSchCost <- usdsgd*costs$SecSch/192 #2010 to 2020, per school day
costs$CHAS_charge_case <- usdsgd*costs$CHAS_charge_case
costs$POLY_charge_case <- usdsgd*costs$POLY_charge_case
costs$Hosp_charge_case <- usdsgd*costs$Hosp_charge_case
costs$ED_charge_case   <- usdsgd*costs$ED_charge_case
#deaths life years lost
plly <- deaths$AGE - 65
plly[plly<0] <- 0
deaths$AGE <- plly
deaths$DIED <- NULL
#friction method counts number of productive indivs
deathsFric <- deaths
deathsFric$AGE[which(deathsFric$AGE>0)] <- 1
deathsFric <- table(deathsFric)
deathsFric <- deathsFric[2,]
#add no death years 2012,2017
deathsFric <- deathsFric[-c(1:3)]
deathsFric <- append(deathsFric,0,2)
deathsFric <- append(deathsFric,0,7)
#get productive life years lost
deaths$AGE <- plly * (0.97^plly) #discount rate 0.03
plly <- aggregate(.~ONSET,FUN=sum,data=deaths)
plly <- plly$AGE
plly <- plly[-c(1:3)] #productive years lost per year 
#add no death years 2012,2017
plly <- append(plly,0,2)
plly <- append(plly,0,7)

#get proportions ambulatory, ed and hosp, scale by capture rate
total <- costs$CHAS_unique_patient+costs$POLY_unique_patient+costs$Hosp_unique_patient+costs$ED_unique_patient
AP    <- (costs$CHAS_unique_patient+costs$POLY_unique_patient)/total
EDP   <- costs$ED_unique_patient/total
HospP <- costs$Hosp_unique_patient/total
#get ambulatory average costs
ambulatoryAvg <- (costs$CHAS_charge_case*costs$CHAS_unique_patient + costs$POLY_charge_case*costs$POLY_unique_patient)/(costs$CHAS_unique_patient+costs$POLY_unique_patient) 
ambulatoryAvg <- ambulatoryAvg

#incidence data#
df    <- as.matrix(table(dftemp))
df <- df[,-c(1:4)]  #use 2010:2020 
#indexes to refer to subgroups
childInd  <- 1
priSchInd <- 2
secSchInd  <- 3:4
adultInd <- 5:9
elderInd   <- nrow(df)

computeCost <- function(df,
                  AEF,
                  HEF,
                  Elasticity_LabourProduct,
                  methodIndirect){

# df_ex_Ambulatory <- AEF*df*AP*AmbVisits
# df_ex_Hosp       <- HEF*df*HospP
# df_ex_ED         <- HEF*df*EDP
  df_ex_Ambulatory <- t(AP * t(AEF*df)*AmbVisits)
  df_ex_Hosp <- t(HospP * t(HEF*df))
  df_ex_ED <- t(EDP * t(HEF*df))
#death data#

#get direct medical cost for each cat#    #not all captured under these categories, need to scale upwards to private hospitals#
tc_amb  <- t(t(df_ex_Ambulatory)*ambulatoryAvg)
tc_hosp <- t(t(df_ex_Hosp)*costs$Hosp_charge_case)  
tc_ED   <- t(t(df_ex_ED)*costs$ED_charge_case)
transport <- df_ex_Hosp*transport*2 + df_ex_Hosp*costs$LOS*transport*2 #cost of transport, 2 family visits per day
total_direct   <- tc_amb + tc_hosp + tc_ED + transport  
total_direct   <- colSums(total_direct) 

#indirect costs#
#indirect costs#
#indirect costs#
#indirect costs#

#compute indirect cost, friction method,
#hospital cases
ProdLost_1864    <-  costs$HouseholdMedianSG #2010 to 2020, constant 2010 US dollars
serviceLost_1864 <-  t(t(df_ex_Hosp[adultInd,])*costs$LOS*HouseholdServiceLost)

if (methodIndirect=="friction"){
friction <- t(t(df_ex_Hosp[adultInd,])*costs$LOS*ProdLost_1864) + 
            t(t(df_ex_Ambulatory[adultInd,])*daysAbsentAmb*ProdLost_1864)

death <- deathsFric * ProdLost_1864 * 30 * Elasticity_LabourProduct
productivityLoss_notDeath <-  Elasticity_LabourProduct*friction + serviceLost_1864
productivityLoss <- productivityLoss_notDeath + death
}

if (methodIndirect=="humanCapital"){
  friction <- t(t(df_ex_Hosp[adultInd,])*costs$LOS*ProdLost_1864) + 
    t(t(df_ex_Ambulatory[adultInd,])*daysAbsentAmb*ProdLost_1864)
  death <- plly * ProdLost_1864 * 365
  productivityLoss_notDeath <-  friction + serviceLost_1864
  productivityLoss <- productivityLoss_notDeath + death
}

#ambulatory cases
childCost   <- PropChildCareGive*df_ex_Ambulatory[childInd,]*daysAbsentAmb*ProdLost_1864 + #caregiver lost, ambulatory
               PropChildCareGive*df_ex_Hosp[childInd,]*costs$LOS*ProdLost_1864 #caregiver lost, hospital
               
priSchCost  <- PriSchCost*df_ex_Ambulatory[priSchInd,]*daysAbsentAmb +  #lost in sch, amb
               PriSchCost*df_ex_ED[priSchInd,]*daysAbsentAmb + #lost in sch, ED
               PriSchCost*df_ex_Hosp[priSchInd,]*costs$LOS +  #lost in sch, hosp
               PropChildCareGive*df_ex_Ambulatory[priSchInd,]*daysAbsentAmb*ProdLost_1864 + #caregiver lost, ambulatory
               PropChildCareGive*df_ex_Hosp[priSchInd,]*costs$LOS*ProdLost_1864 #caregiver lost, hospital

secSchCost  <- SecSchCost*df_ex_Ambulatory[secSchInd,]*daysAbsentAmb + #lost in sch, amb
               SecSchCost*df_ex_ED[secSchInd,]*daysAbsentAmb + #lost in sch, ED
               SecSchCost*df_ex_Hosp[secSchInd,]*costs$LOS +  #lost in sch, hosp
               t(PropChildCareGive*daysAbsentAmb*t(ProdLost_1864*df_ex_Ambulatory[secSchInd,])) + #caregiver lost, ambulatory
               PropChildCareGive*costs$LOS*t(ProdLost_1864*t(df_ex_Hosp[secSchInd,])) #caregiver lost, hospital

elderlyCost <- PropElderCareGive*t(t(df_ex_Ambulatory[elderInd,])*ProdLost_1864)*daysAbsentAmb + #caregiver lost, ambulatory
               PropElderCareGive*t(t(df_ex_Hosp[elderInd,])*ProdLost_1864)*costs$LOS #caregiver lost, hospital

total_indirect <- colSums(productivityLoss) +  childCost+ priSchCost +  colSums(secSchCost) +  elderlyCost

df_ex_Ambulatory <- t(AP * t(AEF*df))
df_ex_Hosp <- t(HospP * t(HEF*df))
df_ex_ED <- t(EDP * t(HEF*df))

ppl <- (colSums(df_ex_Ambulatory) + colSums(df_ex_Hosp)+ colSums(df_ex_Hosp))
out <- cbind(total_direct,
             total_indirect,
             colSums(productivityLoss),
             colSums(productivityLoss_notDeath),
             death,
             colSums(tc_amb),
             colSums(tc_hosp),
             colSums(tc_ED),ppl)
colnames(out) <- c("Direct cost",
                   "Indirect costs",
                   "Productivity loss total",
                   "Productivity loss no death",
                   "Productivity loss death",
                   "Ambulatory cost",
                   "Hospitalization cost",
                   "Emergency Cost",
                   "scaleup")
return(out)
}


###########SIMULATE COSTS BASED ON UNCERTAINTY#############
###########SIMULATE COSTS BASED ON UNCERTAINTY#############
###########SIMULATE COSTS BASED ON UNCERTAINTY#############
###########SIMULATE COSTS BASED ON UNCERTAINTY#############

store1 <- store2 <- store3 <-store4 <- list()


HEF <- 1  #hospital expansion factor, conservative
AEF1    <- c(3.8,3.8,3.8,3.8,3.8,13.1,24.3,45.3,50,50) #ambulatory EF, age-dept symptomatic

for (i in 1:2000){
Elasticity_LabourProduct=runif(1,0.6,0.9)
AEF2 <- c(runif(1,1.7,3.6),
         runif(1,1.7,3.6),
         runif(1,1.7,3.6),
         runif(1,1.7,3.6),
         runif(1,1.7,3.6),
         runif(1,3.8,8.2),
         runif(1,6.1,13.4),
         runif(1,11.1,24.2),
         runif(1,12.2,26.5),
         runif(1,12.2,26.5)) #ambulatory expansion factor, constant uncertain
store1[[i]] <- computeCost(df=df,
                      AEF=AEF2,
                      HEF=HEF,
                      Elasticity_LabourProduct=Elasticity_LabourProduct,
                      methodIndirect="humanCapital")/1000000000

store2[[i]] <- computeCost(df=df,
                           AEF=AEF2,
                           HEF=HEF,
                           Elasticity_LabourProduct=Elasticity_LabourProduct,
                           methodIndirect="friction")/1000000000

store3[[i]] <-  computeCost(df=df,
                      AEF=AEF1,
                      HEF=HEF,
                      Elasticity_LabourProduct=Elasticity_LabourProduct,
                      methodIndirect="humanCapital")/1000000000

store4[[i]] <- computeCost(df=df,
                      AEF=AEF1,
                      HEF=HEF,
                      Elasticity_LabourProduct=Elasticity_LabourProduct,
                      methodIndirect="friction")/1000000000

}

temp <- lapply(store2,function(x)x[,1]+x[,2])
temp <- do.call(rbind,temp)
quantile(temp[,11],probs=c(0.025,0.975))
temp <- colMeans(temp)

dim <- ncol(store1[[1]])

store1_allyears <- do.call(rbind,lapply(store1,colSums))
store2_allyears <- do.call(rbind,lapply(store2,colSums))
store3_allyears <- do.call(rbind,lapply(store3,colSums))
store4_allyears <- do.call(rbind,lapply(store4,colSums))

store1_allyears_q <- apply(store1_allyears,MARGIN=2,quantile,probs=c(0.5,0.025,0.975))
store2_allyears_q <- apply(store2_allyears,MARGIN=2,quantile,probs=c(0.5,0.025,0.975))
store3_allyears_q <- apply(store3_allyears,MARGIN=2,quantile,probs=c(0.5,0.025,0.975))
store4_allyears_q <- apply(store4_allyears,MARGIN=2,quantile,probs=c(0.5,0.025,0.975))

summary <- rbind(store1_allyears_q,store2_allyears_q,store3_allyears_q,store4_allyears_q)
rownames(summary) <- paste(rownames(summary),
                           c(rep("human capital, constant symptomatic",3),
                           rep("friction cost, constant symptomatic",3),
                           rep("human capital, age-dep symptomatic",3),
                           rep("friction cost, age-dep symptomatic",3)))
save(summary,file="~/cea/wolbOut.RData")
#table
realTable <- lapply(store2,function(x)x[,1:2])
realTable <- lapply(realTable,rowSums)
realTable <- do.call(rbind,realTable)
realTable <- colMeans(realTable)
realTable <- realTable* 1000
nominalTable <- realTable / costs$SGDUSD2010
number <-  table(dftemp$ONSET)[5:15]
numberPrevented <- number * 0.4
costSaveWolbachia1 <- realTable*0.4*0.8
costSaveWolbachia2 <- realTable*0.5*0.8
costSaveWolbachia3 <- realTable*0.6*0.8
costSaveWolbachia4 <- realTable*0.7*0.8
costSaveWolbachia5 <- realTable*0.8*0.8
costSaveWolbachia6 <- realTable*0.9*0.8
# costperDalyAverted <- 4000000*11/2077348
out <- rbind(realTable,nominalTable,numberPrevented,costSaveWolbachia1,
             costSaveWolbachia2,
             costSaveWolbachia3,
             costSaveWolbachia4,
             costSaveWolbachia5,
             costSaveWolbachia6)
rowSums(out)


#compute incidence prevention
temp <- lapply(store1 , function(x)x[,9])  
temp <- do.call(rbind,temp)
temp <- colMeans(temp)
incidencePrevention<- temp *1000000000 * 0.4 * 0.8 

rm(list=setdiff(ls(), c("store1","store2")))
save.image("~/cea/output/cea.RData")


#clean
#df #dengue case count file 
#deaths$ONSET #death file
#total cost, direct and indirect
rbind(paste(round(total,2)," (95% UI: ",round(total_d,2),",",round(total_id,2),")",sep=""),
paste(round(total1,2)," (95% UI: ",round(total_d1,2),",",round(total_id1,2),")",sep=""),
round(c(sum(colSums(total2[,1:2])),colSums(total2[,1:2])),2),
round(c(sum(colSums(total3[,1:2])),colSums(total3[,1:2])),2))
#rbind(total1,total_d1,total_id1)
#cost breakdown under methods
otherCosts1 <- do.call(rbind,lapply(store1,function(x)colSums(x[,1:8])))
otherCosts2 <- do.call(rbind,lapply(store2,function(x)colSums(x[,1:8])))
otherCosts1 <- colMeans(otherCosts1)
otherCosts2 <- colMeans(otherCosts2)
costFrame <- cbind(otherCosts1,otherCosts2,colSums(total2),colSums(total3))
rm(otherCosts1,otherCosts2)
colnames(costFrame) <- c("humanCapital constant symptomatic rates (2010USDBn)",
                         "friction constant symptomatic rates (2010USDBn)",
                         "humanCapital age dependent symptomatic rates (2010USDBn)",
                         "friction age dependent symptomatic rates (2010USDBn)")
