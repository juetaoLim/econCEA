rm(list=ls())
library(readxl)
load("~/cea/data/outCEA.Rdata") #Rdata file with individuals age and year as dataframe
costs <- read_excel("~/cea/data/CEADetails.xlsx")
deaths <- read.csv("~/cea/data/deaths.csv")


computeCost <- function(df,
                        AEF,
                        HEF,
                        Elasticity_LabourProduct,
                        methodIndirect){
  
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
  out <- cbind(total_direct,
               total_indirect,
               colSums(productivityLoss),
               colSums(productivityLoss_notDeath),
               death,
               colSums(tc_amb),
               colSums(tc_hosp),
               colSums(tc_ED))
  colnames(out) <- c("Direct cost",
                     "Indirect costs",
                     "Productivity loss total",
                     "Productivity loss no death",
                     "Productivity loss death",
                     "Ambulatory cost",
                     "Hospitalization cost",
                     "Emergency Cost")
  return(out)
}

#static parameters#
#static parameters#
#static parameters#
#static parameters#
#deaths life years lost
#all in usdsgd#
usdsgd <- costs$SGDUSD2010

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

costs$CHAS_charge_case <- usdsgd*costs$CHAS_charge_case
costs$POLY_charge_case <- usdsgd*costs$POLY_charge_case
costs$Hosp_charge_case <- usdsgd*costs$Hosp_charge_case
costs$ED_charge_case   <- usdsgd*costs$ED_charge_case

#get proportions ambulatory, ed and hosp, scale by capture rate
total <- costs$CHAS_unique_patient+costs$POLY_unique_patient+costs$Hosp_unique_patient+costs$ED_unique_patient
AP    <- (costs$CHAS_unique_patient+costs$POLY_unique_patient)/total
EDP   <- costs$ED_unique_patient/total
HospP <- costs$Hosp_unique_patient/total
#get ambulatory average costs
ambulatoryAvg <- (costs$CHAS_charge_case*costs$CHAS_unique_patient + costs$POLY_charge_case*costs$POLY_unique_patient)/(costs$CHAS_unique_patient+costs$POLY_unique_patient) 
ambulatoryAvg <- ambulatoryAvg

PriSchCost <- usdsgd*costs$PriSch/192 #2010 to 2020, per school day
SecSchCost <- usdsgd*costs$SecSch/192 #2010 to 2020, per school day

#incidence data#
df    <- as.matrix(table(dftemp))
df <- df[,-c(1:4)]  #use 2010:2020 
#indexes to refer to subgroups
childInd  <- 1
priSchInd <- 2
secSchInd  <- 3:4
adultInd <- 4:9
elderInd   <- nrow(df)

#get productive life years lost
deaths$AGE <- plly * (0.97^plly) #discount rate 0.03
plly <- aggregate(.~ONSET,FUN=sum,data=deaths)
plly <- plly$AGE
plly <- plly[-c(1:3)] #productive years lost per year 
#add no death years 2012,2017
plly <- append(plly,0,2)
plly <- append(plly,0,7)

#changing parameters#
#changing base parameters#
#changing base parameters#
#changing base parameters#
#changing base parameters#
#changing base parameters#
#changing base parameters#



###########SIMULATE COSTS BASED ON UNCERTAINTY#############
###########SIMULATE COSTS BASED ON UNCERTAINTY#############
###########SIMULATE COSTS BASED ON UNCERTAINTY#############
###########SIMULATE COSTS BASED ON UNCERTAINTY#############


params <- list(PropChildCareGive =   0.43,
               PropElderCareGive =  0.073,
               DiscountRateDeath = 0.03,
               daysAbsentAmb = 4,
               AmbVisits = 4.33 ,
               transport = 3.7 ,
               HouseholdServiceLost = 35,
               HEF = 1,  #hospital expansion factor, conservative
               AEF1 = c(3.8,3.8,3.8,3.8,3.8,13.1,24.3,45.3,50,50), #ambulatory EF, age-dept symptomatic
               Elasticity_LabourProduct = 0.75,
               AEF2 =  c(2.65,
                         2.65,
                         2.65,
                         2.65,
                         2.65,
                         6,
                         9.75,
                         17.65,
                         19.35,
                         19.35) 
)



store1 <- store2 <- store3 <-store4 <- list()
for (j in 1:length(params)){  
   temp <- params
   temp[[j]] <- temp[[j]] * 1.25
   list2env(temp,.GlobalEnv)
   store1[[j]] <- computeCost(df=df,
                             AEF=AEF2,
                             HEF=HEF,
                             Elasticity_LabourProduct=Elasticity_LabourProduct,
                             methodIndirect="humanCapital")/1000000000
  
  store2[[j]] <- computeCost(df=df,
                             AEF=AEF2,
                             HEF=HEF,
                             Elasticity_LabourProduct=Elasticity_LabourProduct,
                             methodIndirect="friction")/1000000000
  
  store3[[j]] <-  computeCost(df=df,
                              AEF=AEF1,
                              HEF=HEF,
                              Elasticity_LabourProduct=Elasticity_LabourProduct,
                              methodIndirect="humanCapital")/1000000000
  
  store4[[j]] <- computeCost(df=df,
                             AEF=AEF1,
                             HEF=HEF,
                             Elasticity_LabourProduct=Elasticity_LabourProduct,
                             methodIndirect="friction")/1000000000
  
}

names(store1) <- names(store2) <- names(store3) <- names(store4) <- paste(names(params),"+0.25%")
storeMaster <- list(store1,store2,store3,store4)
names(storeMaster) <- c("humanCapital constant symptomatic rates (2010USDBn)",
                        "friction constant symptomatic rates (2010USDBn)",
                        "humanCapital age dependent symptomatic rates (2010USDBn)",
                        "friction age dependent symptomatic rates (2010USDBn)")
save(storeMaster,file="~/cea/sensitivityOut.RData")

