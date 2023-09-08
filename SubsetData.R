# Run this file to create new csv files for Ethiopia, which is a subset of EthiopiaFinal

require(dplyr)
require(tidyr)

subsetData <- function(country){
  if(country == "Ethiopia"){
    # ETHIOPIA HOUSEHOLD DATASET
    ethiopia <- read.csv("../FinalData/ethiopiaFinal.csv", stringsAsFactors = FALSE)
    
    # select desired continuous variables for household level characteristics
    tempHouseCont <- ethiopia %>%
      select(quesID, relationHead, age, highestSchoolLevel, totalMale, totalFemale,
             headType, homeRooms, CLHouseNum, worstCLHouseNum, childTraffic,
             hazardLaborHouseNum, expendMonthHealth, expendYearHealth, totalIncome) %>%
      mutate(homeRooms = ifelse(is.na(homeRooms) | homeRooms == 0, 1, homeRooms)) %>%
      group_by(quesID) %>%
      summarise(ageMin = min(age), ageMax = max(age), ageAvg = mean(age),
                totalMale = mean(totalMale), totalFemale = mean(totalFemale),
                totalMembers = sum(totalMale, totalFemale), 
                homeRooms = mean(homeRooms),
                CLHouseNum = mean(CLHouseNum),
                worstCLHouseNum = mean(worstCLHouseNum), 
                hazardLaborHouseNum = mean(hazardLaborHouseNum),
                expendMonthHealth = mean(expendMonthHealth),
                expendYearHealth = mean(expendYearHealth),
                totalIncome = mean(totalIncome)) %>%
      mutate(worstCLHouseCat = ifelse(worstCLHouseNum > 0, 1, 0),
             hazardLaborHouseCat = ifelse(hazardLaborHouseNum > 0, 1, 0),
             CLHouseCat = ifelse(CLHouseNum > 0, 1, 0)) 
    
    # select desired categorical variables for household level characteristics
    # there are 2231 unique households (quesID)
    temp1 <- ethiopia %>%
      select(quesID, relationHead) %>%
      group_by(quesID) %>%
      mutate(total = n()) %>%
      count(quesID, relationHead, total) %>%
      mutate(propRelationHead = n/total) %>%
      slice(which.max(propRelationHead)) %>%
      select(quesID, relationHead, propRelationHead)
    
    # creating proportion that can't read and write adults
    temp2 <- ethiopia %>%
      select(quesID, readWrite, age) %>%
      group_by(quesID) %>%
      filter(age >= 18) %>%
      mutate(total = n()) %>%
      count(quesID, readWrite, total) %>%
      mutate(adultIlliterateProp = ifelse(readWrite == "Illiterate", n/total, 1-n/total)) %>%
      select(quesID, adultIlliterateProp) %>%
      filter(row_number(adultIlliterateProp) == 1)
    
    # creating proportion that can't read and write for children between 5 and 17
    temp3 <- ethiopia %>%
      select(quesID, readWrite, age) %>%
      group_by(quesID) %>%
      filter(age <= 17 & age >= 5) %>%
      mutate(total = n()) %>%
      count(quesID, readWrite, total) %>%
      mutate(child517IlliterateProp = ifelse(readWrite == "Illiterate", n/total, 1-n/total)) %>%
      select(quesID, child517IlliterateProp) %>%
      filter(row_number(child517IlliterateProp) == 1)
    
    # creating head type per household
    temp4 <- ethiopia %>%
      select(quesID, headType) %>%
      group_by(quesID, headType) %>%
      filter(row_number(quesID) == 1)
    
    # creating if child trafficking exists in household
    temp5 <- ethiopia %>%
      select(quesID, childTraffic) %>%
      group_by(quesID, childTraffic) %>%
      filter(row_number(quesID) == 1)
    
    # creating who owns home for household
    temp6 <- ethiopia %>%
      select(quesID, homeOwner) %>%
      group_by(quesID, homeOwner) %>%
      filter(row_number(quesID) == 1) %>%
      na.omit()
    
    # creating residency type per household
    temp7 <- ethiopia %>%
      select(quesID, residenceType = residence_urban_rural) %>%
      group_by(quesID, residenceType) %>%
      filter(row_number(quesID) == 1)
    
    # creating how many children and adults exist in household
    # as well as how many children between 5 to 17 exist in household
    temp8 <- ethiopia %>%
      select(quesID, age) %>%
      group_by(quesID) %>%
      mutate(totalChildren = ifelse(age < 18, 1, 0),
             totalAdults = ifelse(age >= 18, 1, 0),
             totalChild517 = ifelse(age <= 17 & age >= 5, 1, 0)) %>%
      summarize(totalChildren = sum(totalChildren), 
                totalAdults = sum(totalAdults),
                totalChild517 = sum(totalChild517))
    
    # creating average child and adult age per household
    temp9 <- ethiopia %>%
      select(quesID, age) %>%
      group_by(quesID) %>%
      mutate(ageCat = ifelse(age < 18, "child", "adult")) %>%
      group_by(quesID, ageCat) %>%
      summarize(avgAge = mean(age)) %>%
      ungroup() %>%
      spread(ageCat, avgAge) %>%
      select(quesID, avgChildAge = child, avgAdultAge = adult) %>%
      mutate(avgChildAge = ifelse(is.na(avgChildAge), 0, avgChildAge),
             avgAdultAge = ifelse(is.na(avgAdultAge), 0, avgAdultAge))
    
    # creating new head type variable
    temp10 <- ethiopia %>%
      select(quesID, age, sex, headType, relationHead, maritalStatus) %>%
      mutate(newHeadType = ifelse(headType == "Adult female" & sex == "Female" & maritalStatus == "Married" & 
                                    relationHead == "Household Head" & age >= 18,
                                  "Married Adult Female", 
                                  ifelse(headType == "Adult female" & sex == "Female" &
                                           maritalStatus != "Married" & 
                                           relationHead == "Household Head" & age >= 18,
                                         "Single Adult Female",
                                         ifelse(headType == "Adult female" & age < 18 & 
                                                  relationHead == "Household Head" & maritalStatus != "Married", 
                                                "Child (<18)", 
                                                ifelse(headType == "Adult female" & relationHead == "Household Head" &
                                                         sex == "Male" & age >= 18,
                                                       "Adult Male", 
                                                       ifelse(headType == "Adult female" & relationHead == "Household Head" &
                                                                sex == "Male" & age < 18, "Child (<18)", NA)))))) %>%
      mutate(newHeadType = ifelse(headType == "Adult male" & relationHead == "Household Head" &
                                    age < 18 & sex == "Male", "Child (<18)", 
                                  ifelse(headType == "Adult male" &
                                           relationHead == "Household Head" & 
                                           age >= 18 & sex == "Male", "Adult Male",
                                         newHeadType))) %>%
      filter(relationHead == "Household Head") %>%
      mutate(newHeadType = ifelse(is.na(newHeadType),
                                  ifelse(sex == "Male" & age >= 18, "Adult Male",
                                         ifelse(age < 18, "Child (<18)",
                                                ifelse(sex == "Female" & maritalStatus != "Married", "Single Adult Female", 
                                                       ifelse(sex == "Female" & maritalStatus == "Married", "Married Adult Female", 0)))), 
                                  newHeadType)) %>%
      select(quesID, newHeadType)
    
    # average child age for children eligible for child labor
    temp11 <- ethiopia %>%
      select(quesID, age) %>%
      mutate(age517 = ifelse(age <= 17 & age >= 5, "CLAge", "notCLAge")) %>%
      group_by(quesID, age517) %>%
      summarize(avgAge = mean(age)) %>%
      ungroup() %>%
      spread(age517, avgAge) %>%
      select(quesID, avg517Age = CLAge)
    
    # merging all temporary datasets for Ethiopia household data together
    allQuesID <- data.frame(quesID = unique(ethiopia$quesID))
    tempHouseCat <- full_join(allQuesID, temp1, by = "quesID") %>%
      full_join(temp2, by = "quesID") %>%
      full_join(temp3, by = "quesID") %>%
      full_join(temp4, by = "quesID") %>%
      full_join(temp5, by = "quesID") %>%
      full_join(temp6, by = "quesID") %>%
      full_join(temp7, by = "quesID") %>%
      full_join(temp8, by = "quesID") %>%
      full_join(temp9, by = "quesID") %>%
      full_join(temp10, by = "quesID") %>%
      full_join(temp11, by = "quesID")

    # final Ethiopia household predictors (and CLHouseNum and HazardLaborHouseNum)
    ethiopiaHousePredictors <- full_join(tempHouseCont, tempHouseCat, by = "quesID") %>%
      select(quesID, headType = newHeadType,
             ageMin, ageMax, ageAvg, avgChildAge, avg517Age, avgAdultAge, totalMale, totalFemale, 
             totalAdults, totalChildren, 
             totalChild517, totalMembers,
             adultIlliterateProp, child517IlliterateProp, residenceType,
             homeOwner, homeRooms, totalIncome, CLHouseNum, CLHouseCat, 
             worstCLHouseNum, worstCLHouseCat,
             hazardLaborHouseNum, hazardLaborHouseCat)
    
    # final Ethiopia outcome variables
    
    # definite indicators of hazardous labor
    outcomeDefiniteHazard <- ethiopia %>%
      select(quesID, X, age, poorPhysHarass, 
             probSexualAbuse, probExtremeFatigue, activityAgrProd, 
             activityHunt, activityMining, activityPrepFood, 
             activityCraft, activitySmallBusiness, activityRepair, 
             activityCarShoe, activityTransportGoods, activityConstruct, 
             activityFetch, activityServeFood, activityDomesticAnim, 
             activityProstitution, selfDACare, selfDATransportMemb, 
             workCondHandCont, workCondHandNonCont, workCondHandHillCont, 
             workCondPullDeepSite, operateMachEquip, expDustFume,
             expFireGasFlames, expNoiseVibration, expDangerousTools,
             expExposedChem, subjNightWorkForce, subjForceDangerousEquip) %>%
      mutate(ageCat = ifelse(age >= 5 & age <= 17, "child517", "notCLage")) %>%
      select(-age) %>%
      mutate(poorPhysHarass = ifelse(poorPhysHarass == "Yes", 1, ifelse(poorPhysHarass == "No", 0, poorPhysHarass)), 
             probSexualAbuse = ifelse(probSexualAbuse == "Yes", 1, ifelse(probSexualAbuse == "No", 0, probSexualAbuse)), 
             probExtremeFatigue = ifelse(probExtremeFatigue == "Yes", 1, ifelse(probExtremeFatigue == "No", 0, probExtremeFatigue)), 
             activityAgrProd = ifelse(activityAgrProd == "Yes", 1, ifelse(activityAgrProd == "No", 0, activityAgrProd)), 
             activityHunt = ifelse(activityHunt == "Yes", 1, ifelse(activityHunt == "No", 0, activityHunt)), 
             activityMining = ifelse(activityMining == "Yes", 1, ifelse(activityMining == "No", 0, activityMining)), 
             activityPrepFood = ifelse(activityPrepFood == "Yes", 1, ifelse(activityPrepFood == "No", 0, activityPrepFood)), 
             activityCraft = ifelse(activityCraft == "Yes", 1, ifelse(activityCraft == "No", 0, activityCraft)), 
             activitySmallBusiness = ifelse(activitySmallBusiness == "Yes", 1, ifelse(activitySmallBusiness == "No", 0, activitySmallBusiness)), 
             activityRepair = ifelse(activityRepair == "Yes", 1, ifelse(activityRepair == "No", 0, activityRepair)), 
             activityCarShoe = ifelse(activityCarShoe == "Yes", 1, ifelse(activityCarShoe == "No", 0, activityCarShoe)), 
             activityTransportGoods = ifelse(activityTransportGoods == "Yes", 1, ifelse(activityTransportGoods == "No", 0, activityTransportGoods)), 
             activityConstruct = ifelse(activityConstruct == "Yes", 1, ifelse(activityConstruct == "No", 0, activityConstruct)), 
             activityFetch = ifelse(activityFetch == "Yes", 1, ifelse(activityFetch == "No", 0, activityFetch)), 
             activityServeFood = ifelse(activityServeFood == "Yes", 1, ifelse(activityServeFood == "No", 0, activityServeFood)), 
             activityDomesticAnim = ifelse(activityDomesticAnim == "Yes", 1, ifelse(activityDomesticAnim == "No", 0, activityDomesticAnim)), 
             activityProstitution = ifelse(activityProstitution == "Yes", 1, ifelse(activityProstitution == "No", 0, activityProstitution)), 
             selfDACare = ifelse(selfDACare == "Yes", 1, ifelse(selfDACare == "No", 0, selfDACare)), 
             selfDATransportMemb = ifelse(selfDATransportMemb == "Yes", 1, ifelse(selfDATransportMemb == "No", 0, selfDATransportMemb)), 
             workCondHandCont = ifelse(workCondHandCont > 0, 1, workCondHandCont), 
             workCondHandNonCont = ifelse(workCondHandNonCont > 0, 1, workCondHandNonCont), 
             workCondHandHillCont = ifelse(workCondHandHillCont > 0, 1, workCondHandHillCont), 
             workCondPullDeepSite = ifelse(workCondPullDeepSite > 0, 1, workCondPullDeepSite), 
             operateMachEquip = ifelse(operateMachEquip == "Yes", 1, ifelse(operateMachEquip == "No", 0, operateMachEquip)), 
             expDustFume = ifelse(expDustFume == "Yes", 1, ifelse(expDustFume == "No", 0, expDustFume)),
             expFireGasFlames = ifelse(expFireGasFlames == "Yes", 1, ifelse(expFireGasFlames == "No", 0, expFireGasFlames)), 
             expNoiseVibration = ifelse(expNoiseVibration == "Yes", 1, ifelse(expNoiseVibration == "No", 0, expNoiseVibration)), 
             expDangerousTools = ifelse(expDangerousTools == "Yes", 1, ifelse(expDangerousTools == "No", 0, expDangerousTools)),
             expExposedChem = ifelse(expExposedChem == "Yes", 1, ifelse(expExposedChem == "No", 0, expExposedChem)), 
             subjNightWorkForce = ifelse(subjNightWorkForce == "Yes", 1, ifelse(subjNightWorkForce == "No", 0, subjNightWorkForce)), 
             subjForceDangerousEquip = ifelse(subjForceDangerousEquip == "Yes", 1, ifelse(subjForceDangerousEquip == "No", 0, subjForceDangerousEquip))) %>%
      group_by(quesID, X, ageCat) %>%
      replace(is.na(.), 0) %>%
      mutate_all(as.numeric) %>%
      summarize(childNumDefiniteHazard = sum(poorPhysHarass, 
                                        probSexualAbuse, probExtremeFatigue, activityAgrProd, 
                                        activityHunt, activityMining, activityPrepFood, 
                                        activityCraft, activitySmallBusiness, activityRepair, 
                                        activityCarShoe, activityTransportGoods, activityConstruct, 
                                        activityFetch, activityServeFood, activityDomesticAnim, 
                                        activityProstitution, selfDACare, selfDATransportMemb, 
                                        workCondHandCont, workCondHandNonCont, workCondHandHillCont, 
                                        workCondPullDeepSite, operateMachEquip, expDustFume,
                                        expFireGasFlames, expNoiseVibration, expDangerousTools,
                                        expExposedChem, subjNightWorkForce, subjForceDangerousEquip)) %>%
      ungroup() %>%
      mutate(childNumDefiniteHazard = ifelse(childNumDefiniteHazard > 0, 1, 0)) %>%
      select(-X) %>%
      group_by(quesID, ageCat, childNumDefiniteHazard) %>%
      summarize(n = n()) %>%
      ungroup() %>%
      filter(ageCat == "child517", childNumDefiniteHazard == 1) %>%
      select(quesID, childNumDefiniteHazard = n)
      
    # definite indicators of worst form of child labor
    outcomeDefiniteWorst <- ethiopia %>%
      select(quesID, X, age, poorEmotionHarass, poorPhysHarass, 
             probSexualAbuse, probExtremeFatigue, probNoSchoolTime, 
             expDustFume, expFireGasFlames, expNoiseVibration, 
             expExtremeTemp, expDangerousTools, expWorkUnderground, 
             expWorkHeights, expWorkWater, expDarkConfined, expNoVent, 
             expExposedChem, expExplosives, expAlcoholDrug, subjConstantShout, 
             subjRepeatedInsults, subjPhysAbuse, subjSexAbuse, subjTraffick, 
             subjBondage, subjCommercialSex, subjNightWorkForce, subjUnwillingWork, 
             subjDeniedEdu, subjDeniedHealthCare, subjForceDangerousEquip, 
             subjInsuffFoodDrink, subjUnsuitableLiving, 
             subjDeniedRest, subjDeniedParentContact, subjDeniedPeerContact, 
             subjDeniedSalary, subjForceReligion, subjForceWalk30Min) %>%
      mutate(ageCat = ifelse(age >= 5 & age <= 17, "child517", "notCLage")) %>%
      select(-age) %>%
      mutate(poorEmotionHarass = ifelse(poorEmotionHarass == "Yes", 1, ifelse(poorEmotionHarass == "No", 0, poorEmotionHarass)), 
             poorPhysHarass = ifelse(poorPhysHarass == "Yes", 1, ifelse(poorPhysHarass == "No", 0, poorPhysHarass)), 
             probSexualAbuse = ifelse(probSexualAbuse == "Yes", 1, ifelse(probSexualAbuse == "No", 0, probSexualAbuse)), 
             probExtremeFatigue = ifelse(probExtremeFatigue == "Yes", 1, ifelse(probExtremeFatigue == "No", 0, probExtremeFatigue)), 
             probNoSchoolTime = ifelse(probNoSchoolTime == "Yes", 1, ifelse(probNoSchoolTime == "No", 0, probNoSchoolTime)), 
             expDustFume = ifelse(expDustFume == "Yes", 1, ifelse(expDustFume == "No", 0, expDustFume)), 
             expFireGasFlames = ifelse(expFireGasFlames == "Yes", 1, ifelse(expFireGasFlames == "No", 0, expFireGasFlames)), 
             expNoiseVibration = ifelse(expNoiseVibration == "Yes", 1, ifelse(expNoiseVibration == "No", 0, expNoiseVibration)), 
             expExtremeTemp = ifelse(expExtremeTemp == "Yes", 1, ifelse(expExtremeTemp == "No", 0, expExtremeTemp)), 
             expDangerousTools = ifelse(expDangerousTools == "Yes", 1, ifelse(expDangerousTools == "No", 0, expDangerousTools)), 
             expWorkUnderground = ifelse(expWorkUnderground == "Yes", 1, ifelse(expWorkUnderground == "No", 0, expWorkUnderground)), 
             expWorkHeights = ifelse(expWorkHeights == "Yes", 1, ifelse(expWorkHeights == "No", 0, expWorkHeights)), 
             expWorkWater = ifelse(expWorkWater == "Yes", 1, ifelse(expWorkWater == "No", 0, expWorkWater)), 
             expDarkConfined = ifelse(expDarkConfined == "Yes", 1, ifelse(expDarkConfined == "No", 0, expDarkConfined)), 
             expNoVent = ifelse(expNoVent == "Yes", 1, ifelse(expNoVent == "No", 0, expNoVent)), 
             expExposedChem = ifelse(expExposedChem == "Yes", 1, ifelse(expExposedChem == "No", 0, expExposedChem)), 
             expExplosives = ifelse(expExplosives == "Yes", 1, ifelse(expExplosives == "No", 0, expExplosives)), 
             expAlcoholDrug = ifelse(expAlcoholDrug == "Yes", 1, ifelse(expAlcoholDrug == "No", 0, expAlcoholDrug)), 
             subjConstantShout = ifelse(subjConstantShout == "Yes", 1, ifelse(subjConstantShout == "No", 0, subjConstantShout)), 
             subjRepeatedInsults = ifelse(subjRepeatedInsults == "Yes", 1, ifelse(subjRepeatedInsults == "No", 0, subjRepeatedInsults)), 
             subjPhysAbuse = ifelse(subjPhysAbuse == "Yes", 1, ifelse(subjPhysAbuse == "No", 0, subjPhysAbuse)), 
             subjSexAbuse = ifelse(subjSexAbuse == "Yes", 1, ifelse(subjSexAbuse == "No", 0, subjSexAbuse)), 
             subjTraffick = ifelse(subjTraffick == "Yes", 1, ifelse(subjTraffick == "No", 0, subjTraffick)), 
             subjBondage = ifelse(subjBondage == "Yes", 1, ifelse(subjBondage == "No", 0, subjBondage)), 
             subjCommercialSex = ifelse(subjCommercialSex == "Yes", 1, ifelse(subjCommercialSex == "No", 0, subjCommercialSex)), 
             subjNightWorkForce = ifelse(subjNightWorkForce == "Yes", 1, ifelse(subjNightWorkForce == "No", 0, subjNightWorkForce)), 
             subjUnwillingWork = ifelse(subjUnwillingWork == "Yes", 1, ifelse(subjUnwillingWork == "No", 0, subjUnwillingWork)), 
             subjDeniedEdu = ifelse(subjDeniedEdu == "Yes", 1, ifelse(subjDeniedEdu == "No", 0, subjDeniedEdu)), 
             subjDeniedHealthCare = ifelse(subjDeniedHealthCare == "Yes", 1, ifelse(subjDeniedHealthCare == "No", 0, subjDeniedHealthCare)), 
             subjForceDangerousEquip = ifelse(subjForceDangerousEquip == "Yes", 1, ifelse(subjForceDangerousEquip == "No", 0, subjForceDangerousEquip)), 
             subjInsuffFoodDrink = ifelse(subjInsuffFoodDrink == "Yes", 1, ifelse(subjInsuffFoodDrink == "No", 0, subjInsuffFoodDrink)), 
             subjUnsuitableLiving = ifelse(subjUnsuitableLiving == "Yes", 1, ifelse(subjUnsuitableLiving == "No", 0, subjUnsuitableLiving)), 
             subjDeniedRest = ifelse(subjDeniedRest == "Yes", 1, ifelse(subjDeniedRest == "No", 0, subjDeniedRest)), 
             subjDeniedParentContact = ifelse(subjDeniedParentContact == "Yes", 1, ifelse(subjDeniedParentContact == "No", 0, subjDeniedParentContact)), 
             subjDeniedPeerContact = ifelse(subjDeniedPeerContact == "Yes", 1, ifelse(subjDeniedPeerContact == "No", 0, subjDeniedPeerContact)), 
             subjDeniedSalary = ifelse(subjDeniedSalary == "Yes", 1, ifelse(subjDeniedSalary == "No", 0, subjDeniedSalary)), 
             subjForceReligion = ifelse(subjForceReligion == "Yes", 1, ifelse(subjForceReligion == "No", 0, subjForceReligion)), 
             subjForceWalk30Min = ifelse(subjForceWalk30Min == "Yes", 1, ifelse(subjForceWalk30Min == "No", 0, subjForceWalk30Min))) %>%
      group_by(quesID, X, ageCat) %>%
      replace(is.na(.), 0) %>%
      mutate_all(as.numeric) %>%
      summarize(childNumDefiniteWorst = sum(poorEmotionHarass, poorPhysHarass, 
                                       probSexualAbuse, probExtremeFatigue, probNoSchoolTime, 
                                       expDustFume, expFireGasFlames, expNoiseVibration, 
                                       expExtremeTemp, expDangerousTools, expWorkUnderground, 
                                       expWorkHeights, expWorkWater, expDarkConfined, expNoVent, 
                                       expExposedChem, expExplosives, expAlcoholDrug, subjConstantShout, 
                                       subjRepeatedInsults, subjPhysAbuse, subjSexAbuse, subjTraffick, 
                                       subjBondage, subjCommercialSex, subjNightWorkForce, subjUnwillingWork, 
                                       subjDeniedEdu, subjDeniedHealthCare, subjForceDangerousEquip, 
                                       subjInsuffFoodDrink, subjUnsuitableLiving, 
                                       subjDeniedRest, subjDeniedParentContact, subjDeniedPeerContact, 
                                       subjDeniedSalary, subjForceReligion, subjForceWalk30Min)) %>%
      ungroup() %>%
      mutate(childNumDefiniteWorst = ifelse(childNumDefiniteWorst > 0, 1, 0)) %>%
      select(-X) %>%
      group_by(quesID, ageCat, childNumDefiniteWorst) %>%
      summarize(n = n()) %>%
      ungroup() %>%
      filter(ageCat == "child517", childNumDefiniteWorst == 1) %>%
      select(quesID, childNumDefiniteWorst = n)
      
    allQuesID <- data.frame(quesID = unique(ethiopia$quesID))
    
    ethiopiaHouseOutcomes <- full_join(allQuesID, outcomeDefiniteHazard, by = "quesID") %>%
      full_join(outcomeDefiniteWorst, by = "quesID") %>%
      replace(is.na(.), 0)
    
    # final Ethiopia household data
    ethiopiaHousehold <- full_join(ethiopiaHousePredictors, ethiopiaHouseOutcomes, by = "quesID")
    
    # ETHIOPIA INDIVIDUAL DATASET
    # select desired variables for individual level characteristics
    ethIndPredictors <- ethiopia %>%
      select(X, quesID, relationHead, sex, age, givenBirth, readWrite, 
             highestSchoolLevel, mealsYesterday, bioMomAlive, bioDadAlive,
             avgHourPerDay, workMore7Hours) %>%
      filter(age >= 5 & age <= 17)
    
    # all four individual outcome variables
    # definite hazard indicator Ethiopia individual
    outcomeDefiniteHazardInd <- ethiopia %>%
      select(X, age, poorPhysHarass, 
             probSexualAbuse, probExtremeFatigue, activityAgrProd, 
             activityHunt, activityMining, activityPrepFood, 
             activityCraft, activitySmallBusiness, activityRepair, 
             activityCarShoe, activityTransportGoods, activityConstruct, 
             activityFetch, activityServeFood, activityDomesticAnim, 
             activityProstitution, selfDACare, selfDATransportMemb, 
             workCondHandCont, workCondHandNonCont, workCondHandHillCont, 
             workCondPullDeepSite, operateMachEquip, expDustFume,
             expFireGasFlames, expNoiseVibration, expDangerousTools,
             expExposedChem, subjNightWorkForce, subjForceDangerousEquip) %>%
      filter(age >= 5 & age <= 17) %>%
      select(-age) %>%
      mutate(poorPhysHarass = ifelse(poorPhysHarass == "Yes", 1, ifelse(poorPhysHarass == "No", 0, poorPhysHarass)), 
             probSexualAbuse = ifelse(probSexualAbuse == "Yes", 1, ifelse(probSexualAbuse == "No", 0, probSexualAbuse)), 
             probExtremeFatigue = ifelse(probExtremeFatigue == "Yes", 1, ifelse(probExtremeFatigue == "No", 0, probExtremeFatigue)), 
             activityAgrProd = ifelse(activityAgrProd == "Yes", 1, ifelse(activityAgrProd == "No", 0, activityAgrProd)), 
             activityHunt = ifelse(activityHunt == "Yes", 1, ifelse(activityHunt == "No", 0, activityHunt)), 
             activityMining = ifelse(activityMining == "Yes", 1, ifelse(activityMining == "No", 0, activityMining)), 
             activityPrepFood = ifelse(activityPrepFood == "Yes", 1, ifelse(activityPrepFood == "No", 0, activityPrepFood)), 
             activityCraft = ifelse(activityCraft == "Yes", 1, ifelse(activityCraft == "No", 0, activityCraft)), 
             activitySmallBusiness = ifelse(activitySmallBusiness == "Yes", 1, ifelse(activitySmallBusiness == "No", 0, activitySmallBusiness)), 
             activityRepair = ifelse(activityRepair == "Yes", 1, ifelse(activityRepair == "No", 0, activityRepair)), 
             activityCarShoe = ifelse(activityCarShoe == "Yes", 1, ifelse(activityCarShoe == "No", 0, activityCarShoe)), 
             activityTransportGoods = ifelse(activityTransportGoods == "Yes", 1, ifelse(activityTransportGoods == "No", 0, activityTransportGoods)), 
             activityConstruct = ifelse(activityConstruct == "Yes", 1, ifelse(activityConstruct == "No", 0, activityConstruct)), 
             activityFetch = ifelse(activityFetch == "Yes", 1, ifelse(activityFetch == "No", 0, activityFetch)), 
             activityServeFood = ifelse(activityServeFood == "Yes", 1, ifelse(activityServeFood == "No", 0, activityServeFood)), 
             activityDomesticAnim = ifelse(activityDomesticAnim == "Yes", 1, ifelse(activityDomesticAnim == "No", 0, activityDomesticAnim)), 
             activityProstitution = ifelse(activityProstitution == "Yes", 1, ifelse(activityProstitution == "No", 0, activityProstitution)), 
             selfDACare = ifelse(selfDACare == "Yes", 1, ifelse(selfDACare == "No", 0, selfDACare)), 
             selfDATransportMemb = ifelse(selfDATransportMemb == "Yes", 1, ifelse(selfDATransportMemb == "No", 0, selfDATransportMemb)), 
             workCondHandCont = ifelse(workCondHandCont > 0, 1, workCondHandCont), 
             workCondHandNonCont = ifelse(workCondHandNonCont > 0, 1, workCondHandNonCont), 
             workCondHandHillCont = ifelse(workCondHandHillCont > 0, 1, workCondHandHillCont), 
             workCondPullDeepSite = ifelse(workCondPullDeepSite > 0, 1, workCondPullDeepSite), 
             operateMachEquip = ifelse(operateMachEquip == "Yes", 1, ifelse(operateMachEquip == "No", 0, operateMachEquip)), 
             expDustFume = ifelse(expDustFume == "Yes", 1, ifelse(expDustFume == "No", 0, expDustFume)),
             expFireGasFlames = ifelse(expFireGasFlames == "Yes", 1, ifelse(expFireGasFlames == "No", 0, expFireGasFlames)), 
             expNoiseVibration = ifelse(expNoiseVibration == "Yes", 1, ifelse(expNoiseVibration == "No", 0, expNoiseVibration)), 
             expDangerousTools = ifelse(expDangerousTools == "Yes", 1, ifelse(expDangerousTools == "No", 0, expDangerousTools)),
             expExposedChem = ifelse(expExposedChem == "Yes", 1, ifelse(expExposedChem == "No", 0, expExposedChem)), 
             subjNightWorkForce = ifelse(subjNightWorkForce == "Yes", 1, ifelse(subjNightWorkForce == "No", 0, subjNightWorkForce)), 
             subjForceDangerousEquip = ifelse(subjForceDangerousEquip == "Yes", 1, ifelse(subjForceDangerousEquip == "No", 0, subjForceDangerousEquip))) %>%
      group_by(X) %>%
      replace(is.na(.), 0) %>%
      mutate_all(as.numeric) %>%
      summarize(anyDefiniteHazardInd = sum(poorPhysHarass, 
                                        probSexualAbuse, probExtremeFatigue, activityAgrProd, 
                                        activityHunt, activityMining, activityPrepFood, 
                                        activityCraft, activitySmallBusiness, activityRepair, 
                                        activityCarShoe, activityTransportGoods, activityConstruct, 
                                        activityFetch, activityServeFood, activityDomesticAnim, 
                                        activityProstitution, selfDACare, selfDATransportMemb, 
                                        workCondHandCont, workCondHandNonCont, workCondHandHillCont, 
                                        workCondPullDeepSite, operateMachEquip, expDustFume,
                                        expFireGasFlames, expNoiseVibration, expDangerousTools,
                                        expExposedChem, subjNightWorkForce, subjForceDangerousEquip))
    
    # definite worst child labor indicator Ethiopia individual
    outcomeDefiniteWorstInd <- ethiopia %>%
      select(X, age, poorEmotionHarass, poorPhysHarass, 
             probSexualAbuse, probExtremeFatigue, probNoSchoolTime, 
             expDustFume, expFireGasFlames, expNoiseVibration, 
             expExtremeTemp, expDangerousTools, expWorkUnderground, 
             expWorkHeights, expWorkWater, expDarkConfined, expNoVent, 
             expExposedChem, expExplosives, expAlcoholDrug, subjConstantShout, 
             subjRepeatedInsults, subjPhysAbuse, subjSexAbuse, subjTraffick, 
             subjBondage, subjCommercialSex, subjNightWorkForce, subjUnwillingWork, 
             subjDeniedEdu, subjDeniedHealthCare, subjForceDangerousEquip, 
             subjInsuffFoodDrink, subjUnsuitableLiving, 
             subjDeniedRest, subjDeniedParentContact, subjDeniedPeerContact, 
             subjDeniedSalary, subjForceReligion, subjForceWalk30Min) %>%
      filter(age >= 5 & age <= 17) %>%
      select(-age) %>%
      mutate(poorEmotionHarass = ifelse(poorEmotionHarass == "Yes", 1, ifelse(poorEmotionHarass == "No", 0, poorEmotionHarass)), 
             poorPhysHarass = ifelse(poorPhysHarass == "Yes", 1, ifelse(poorPhysHarass == "No", 0, poorPhysHarass)), 
             probSexualAbuse = ifelse(probSexualAbuse == "Yes", 1, ifelse(probSexualAbuse == "No", 0, probSexualAbuse)), 
             probExtremeFatigue = ifelse(probExtremeFatigue == "Yes", 1, ifelse(probExtremeFatigue == "No", 0, probExtremeFatigue)), 
             probNoSchoolTime = ifelse(probNoSchoolTime == "Yes", 1, ifelse(probNoSchoolTime == "No", 0, probNoSchoolTime)), 
             expDustFume = ifelse(expDustFume == "Yes", 1, ifelse(expDustFume == "No", 0, expDustFume)), 
             expFireGasFlames = ifelse(expFireGasFlames == "Yes", 1, ifelse(expFireGasFlames == "No", 0, expFireGasFlames)), 
             expNoiseVibration = ifelse(expNoiseVibration == "Yes", 1, ifelse(expNoiseVibration == "No", 0, expNoiseVibration)), 
             expExtremeTemp = ifelse(expExtremeTemp == "Yes", 1, ifelse(expExtremeTemp == "No", 0, expExtremeTemp)), 
             expDangerousTools = ifelse(expDangerousTools == "Yes", 1, ifelse(expDangerousTools == "No", 0, expDangerousTools)), 
             expWorkUnderground = ifelse(expWorkUnderground == "Yes", 1, ifelse(expWorkUnderground == "No", 0, expWorkUnderground)), 
             expWorkHeights = ifelse(expWorkHeights == "Yes", 1, ifelse(expWorkHeights == "No", 0, expWorkHeights)), 
             expWorkWater = ifelse(expWorkWater == "Yes", 1, ifelse(expWorkWater == "No", 0, expWorkWater)), 
             expDarkConfined = ifelse(expDarkConfined == "Yes", 1, ifelse(expDarkConfined == "No", 0, expDarkConfined)), 
             expNoVent = ifelse(expNoVent == "Yes", 1, ifelse(expNoVent == "No", 0, expNoVent)), 
             expExposedChem = ifelse(expExposedChem == "Yes", 1, ifelse(expExposedChem == "No", 0, expExposedChem)), 
             expExplosives = ifelse(expExplosives == "Yes", 1, ifelse(expExplosives == "No", 0, expExplosives)), 
             expAlcoholDrug = ifelse(expAlcoholDrug == "Yes", 1, ifelse(expAlcoholDrug == "No", 0, expAlcoholDrug)), 
             subjConstantShout = ifelse(subjConstantShout == "Yes", 1, ifelse(subjConstantShout == "No", 0, subjConstantShout)), 
             subjRepeatedInsults = ifelse(subjRepeatedInsults == "Yes", 1, ifelse(subjRepeatedInsults == "No", 0, subjRepeatedInsults)), 
             subjPhysAbuse = ifelse(subjPhysAbuse == "Yes", 1, ifelse(subjPhysAbuse == "No", 0, subjPhysAbuse)), 
             subjSexAbuse = ifelse(subjSexAbuse == "Yes", 1, ifelse(subjSexAbuse == "No", 0, subjSexAbuse)), 
             subjTraffick = ifelse(subjTraffick == "Yes", 1, ifelse(subjTraffick == "No", 0, subjTraffick)), 
             subjBondage = ifelse(subjBondage == "Yes", 1, ifelse(subjBondage == "No", 0, subjBondage)), 
             subjCommercialSex = ifelse(subjCommercialSex == "Yes", 1, ifelse(subjCommercialSex == "No", 0, subjCommercialSex)), 
             subjNightWorkForce = ifelse(subjNightWorkForce == "Yes", 1, ifelse(subjNightWorkForce == "No", 0, subjNightWorkForce)), 
             subjUnwillingWork = ifelse(subjUnwillingWork == "Yes", 1, ifelse(subjUnwillingWork == "No", 0, subjUnwillingWork)), 
             subjDeniedEdu = ifelse(subjDeniedEdu == "Yes", 1, ifelse(subjDeniedEdu == "No", 0, subjDeniedEdu)), 
             subjDeniedHealthCare = ifelse(subjDeniedHealthCare == "Yes", 1, ifelse(subjDeniedHealthCare == "No", 0, subjDeniedHealthCare)), 
             subjForceDangerousEquip = ifelse(subjForceDangerousEquip == "Yes", 1, ifelse(subjForceDangerousEquip == "No", 0, subjForceDangerousEquip)), 
             subjInsuffFoodDrink = ifelse(subjInsuffFoodDrink == "Yes", 1, ifelse(subjInsuffFoodDrink == "No", 0, subjInsuffFoodDrink)), 
             subjUnsuitableLiving = ifelse(subjUnsuitableLiving == "Yes", 1, ifelse(subjUnsuitableLiving == "No", 0, subjUnsuitableLiving)), 
             subjDeniedRest = ifelse(subjDeniedRest == "Yes", 1, ifelse(subjDeniedRest == "No", 0, subjDeniedRest)), 
             subjDeniedParentContact = ifelse(subjDeniedParentContact == "Yes", 1, ifelse(subjDeniedParentContact == "No", 0, subjDeniedParentContact)), 
             subjDeniedPeerContact = ifelse(subjDeniedPeerContact == "Yes", 1, ifelse(subjDeniedPeerContact == "No", 0, subjDeniedPeerContact)), 
             subjDeniedSalary = ifelse(subjDeniedSalary == "Yes", 1, ifelse(subjDeniedSalary == "No", 0, subjDeniedSalary)), 
             subjForceReligion = ifelse(subjForceReligion == "Yes", 1, ifelse(subjForceReligion == "No", 0, subjForceReligion)), 
             subjForceWalk30Min = ifelse(subjForceWalk30Min == "Yes", 1, ifelse(subjForceWalk30Min == "No", 0, subjForceWalk30Min))) %>%
      group_by(X) %>%
      replace(is.na(.), 0) %>%
      mutate_all(as.numeric) %>%
      summarize(anyDefiniteWorstInd = sum(poorEmotionHarass, poorPhysHarass, 
                                       probSexualAbuse, probExtremeFatigue, probNoSchoolTime, 
                                       expDustFume, expFireGasFlames, expNoiseVibration, 
                                       expExtremeTemp, expDangerousTools, expWorkUnderground, 
                                       expWorkHeights, expWorkWater, expDarkConfined, expNoVent, 
                                       expExposedChem, expExplosives, expAlcoholDrug, subjConstantShout, 
                                       subjRepeatedInsults, subjPhysAbuse, subjSexAbuse, subjTraffick, 
                                       subjBondage, subjCommercialSex, subjNightWorkForce, subjUnwillingWork, 
                                       subjDeniedEdu, subjDeniedHealthCare, subjForceDangerousEquip, 
                                       subjInsuffFoodDrink, subjUnsuitableLiving, 
                                       subjDeniedRest, subjDeniedParentContact, subjDeniedPeerContact, 
                                       subjDeniedSalary, subjForceReligion, subjForceWalk30Min))
    
    # all outcomes for ethiopia individual dataset
    ethIndID <- data.frame("X" = unique(filter(ethiopia, age >= 5 & age <= 17)$X))
    ethIndOutcomes <- full_join(outcomeDefiniteHazardInd, ethIndID, by = "X") %>%
      full_join(outcomeDefiniteWorstInd, by = "X") %>%
      replace(is.na(.), 0)
    
    # final Ethiopia individual data
    ethiopiaIndividual <- full_join(ethIndPredictors, ethIndOutcomes, by = "X") %>%
      left_join(ethiopiaHousehold, by = "quesID")
    
    # writing household level data and individual level data 
    write.csv(ethiopiaHousehold, file = paste("../FinalData/ethiopiaHousehold.csv"), row.names=FALSE)
    write.csv(ethiopiaIndividual, file = paste("../FinalData/ethiopiaIndividual.csv"), row.names=FALSE)
  }
  
  if(country == "Uganda"){
    # UGANDA HOUSEHOLD DATSET
    uganda <- read.csv("../FinalData/ugandaFinal.csv", stringsAsFactors = FALSE)
    ugandaTemp <- uganda %>%
      # adding columns for female and male
      mutate(male = ifelse(sex == 1, 1, 0),
             female = ifelse(sex == 2, 1, 0))
    
    # add column for totalMale
    totalMale <- ugandaTemp %>%
      select(quesID, male) %>%
      group_by(quesID) %>%
      summarize(totalMale = sum(male))
    
    # add column for totalFemale
    totalFemale <- ugandaTemp %>%
      select(quesID, female) %>%
      group_by(quesID) %>%
      summarize(totalFemale = sum(female))
    
    # creating columns for total member ages
    totalAdultChild517 <- ugandaTemp %>%
      select(quesID, age) %>%
      group_by(quesID) %>%
      mutate(totalAdults = ifelse(age >= 18, 1, 0),
             totalChildren = ifelse(age < 18, 1, 0),
             totalChild517 = ifelse(age >= 5 & age <= 17, 1, 0)) %>%
      summarize(totalAdults = sum(totalAdults),
                totalChildren = sum(totalChildren),
                totalChild517 = sum(totalChild517))
    
    # totals dataset for Uganda household to be merged to final dataset
    totalsUganda <- full_join(totalMale, totalFemale, by = "quesID") %>%
      mutate(totalMembers = totalMale + totalFemale) %>%
      full_join(totalAdultChild517, by = "quesID")
    
    # head types
    ugandaTemp <- ugandaTemp %>%
      mutate(headType = ifelse(relationHead == 1 & sex == 1, 
                                  "Adult Male",
                                  ifelse(relationHead == 1 & sex == 2, 
                                         "Adult Female", 0)))
    
    allIDs <- unique(ugandaTemp$quesID)
    for (householdNum in allIDs) {
      realHeadTypeNum <- which(ugandaTemp$headType[which(ugandaTemp$quesID == householdNum)] != 0)
      realHeadType <- ugandaTemp$headType[which(ugandaTemp$quesID == householdNum)][realHeadTypeNum[1]]
      ugandaTemp$headType[which(ugandaTemp$quesID == householdNum)] <- realHeadType
    }
    
    # refactoring variables
    ugandaTemp <- ugandaTemp %>%
      mutate(residence = ifelse(residence == 1, "Urban", 
                                     ifelse(residence == 2, "Rural", NA)),
             CLHotSpot = ifelse(CLHotSpot == 1, "Yes",
                                      ifelse(CLHotSpot == 2, "No", NA)),
             consIncome = ifelse(consIncome == 1, "Yes",
                                       ifelse(consIncome == 2, "No", NA)),
             readWrite = ifelse(readWrite == 1, "Yes", 
                                      ifelse(readWrite == 2, "No", NA)),
             relationHead = as.character(relationHead),
             maritalStatus = ifelse(maritalStatus == 1, "Married monogamous",
                                          ifelse(maritalStatus == 2, 
                                                 "Married polygamous", 
                                                 ifelse(maritalStatus == 3,
                                                        "Cohabiting",
                                                        ifelse(maritalStatus == 4,
                                                               "Divorced/separated", 
                                                               ifelse(maritalStatus == 5, 
                                                                      "Widow/widower", 
                                                                      "Never married")))))) %>%
      mutate(sex = ifelse(sex == 1, "Male", "Female"),
             relationHead = ifelse(relationHead == 1, "Head",
                                         ifelse(relationHead == 2, 
                                                "Spouse", 
                                                ifelse(relationHead == 3,
                                                       "Son/daughter",
                                                       ifelse(relationHead == 4,
                                                              "Grandchild", 
                                                              ifelse(relationHead == 5, 
                                                                     "Stepchild",
                                                                     ifelse(relationHead == 6, 
                                                                            "Parent of head or spouse", 
                                                                            ifelse(relationHead == 7, 
                                                                                   "Sister/brother of head or spouse", 
                                                                                   ifelse(relationHead == 8,
                                                                                          "Nephew/niece",
                                                                                          ifelse(relationHead == 9, "Other relative", 
                                                                                                 ifelse(relationHead == 10, "Servant",
                                                                                                        ifelse(relationHead == 11, 
                                                                                                               "Non-relative", "Others"))))))))))))
    
    # select desired continuous variables for household level characteristics
    tempHouseCont <- ugandaTemp %>%
      select(quesID, age) %>%
      group_by(quesID) %>%
      summarize(ageMin = min(age), ageMax = max(age))
    
    # select desired categorical variables for household level characteristics
    # there are 617 unique households (quesID)
    # average child age and average adult age per household
    temp1 <- ugandaTemp %>%
      group_by(quesID) %>%
      mutate(ageCat = ifelse(age < 18, "child", "adult")) %>%
      group_by(quesID, ageCat) %>%
      summarize(avgAge = mean(age)) %>%
      ungroup() %>%
      spread(ageCat, avgAge) %>%
      select(quesID, avgChildAge = child, avgAdultAge = adult) %>%
      mutate(avgChildAge = ifelse(is.na(avgChildAge), 0, avgChildAge),
             avgAdultAge = ifelse(is.na(avgAdultAge), 0, avgAdultAge))
    
    # average child age between 5 and 17 per household
    temp2 <- ugandaTemp %>%
      select(quesID, age) %>%
      mutate(age517 = ifelse(age <= 17 & age >= 5, "CLAge", "notCLAge")) %>%
      group_by(quesID, age517) %>%
      summarize(avgAge = mean(age)) %>%
      ungroup() %>%
      spread(age517, avgAge) %>%
      select(quesID, avg517Age = CLAge)
    
    # creating variable to check the type of residence
    temp3 <- ugandaTemp %>%
      select(quesID, residence) %>%
      group_by(quesID) %>%
      filter(row_number(quesID) == 1)
    
    # prop illiterate for kids between 5 to 17, assuming NA is no
    temp4 <- ugandaTemp %>%
      select(quesID, readWrite, age) %>%
      group_by(quesID) %>%
      filter(age <= 17 & age >= 5) %>%
      mutate(total = n()) %>%
      count(quesID, readWrite, total) %>%
      mutate(child517IlliterateProp = ifelse(readWrite == "Yes", n/total, 1-n/total)) %>%
      select(quesID, child517IlliterateProp) %>%
      filter(row_number(child517IlliterateProp) == 1)
    
    # prop illiterate for adults, assuming NA is no
    temp5 <- ugandaTemp %>%
      select(quesID, readWrite, age) %>%
      group_by(quesID) %>%
      filter(age >= 18) %>%
      mutate(total = n()) %>%
      count(quesID, readWrite, total) %>%
      mutate(adultIlliterateProp = ifelse(readWrite == "Yes", n/total, 1-n/total)) %>%
      select(quesID, adultIlliterateProp) %>%
      filter(row_number(adultIlliterateProp) == 1)

    # creating new head type variable
    temp6 <- ugandaTemp %>%
      select(quesID, age, sex, headType, relationHead, maritalStatus) %>%
      mutate(newHeadType = ifelse(headType == "Adult Female" & sex == "Female" & 
                                    maritalStatus %in% c("Married monogamous", "Married polygamous", "Cohabiting") & 
                                    relationHead == "Head" & age >= 18,
                                  "Married Adult Female", 
                                  ifelse(headType == "Adult Female" & sex == "Female" &
                                           maritalStatus %in% c("Divorced/separated", "Never married", "Widow/widower") & 
                                           relationHead == "Head" & age >= 18,
                                         "Single Adult Female",
                                         ifelse(headType == "Adult Female" & age < 18 & 
                                                  relationHead == "Head", 
                                                "Child (<18)", 
                                                ifelse(headType == "Adult Female" & relationHead == "Head" &
                                                         sex == "Male" & age >= 18,
                                                       "Adult Male", 
                                                       ifelse(headType == "Adult Female" & relationHead == "Head" &
                                                                sex == "Male" & age < 18, "Child (<18)", NA)))))) %>%
      mutate(newHeadType = ifelse(headType == "Adult Male" & relationHead == "Head" &
                                    age < 18 & sex == "Male", "Child (<18)", 
                                  ifelse(headType == "Adult Male" &
                                           relationHead == "Head" & 
                                           age >= 18 & sex == "Male", "Adult Male",
                                         newHeadType))) %>%
      filter(relationHead == "Head") %>%
      select(quesID, headType = newHeadType)
      
    # merging predictor datasets for Uganda
    ugandaHousePredictors <- full_join(tempHouseCont, totalsUganda, by = "quesID") %>%
      full_join(temp1, by = "quesID") %>%
      full_join(temp2, by = "quesID") %>%
      full_join(temp3, by = "quesID") %>%
      full_join(temp4, by = "quesID") %>%
      full_join(temp5, by = "quesID") %>%
      full_join(temp6, by = "quesID")  %>%
      select(quesID, headType, ageMin, ageMax, avgChildAge, avgAdultAge, avg517Age, 
             totalMale, totalFemale,totalAdults, totalChildren, totalChild517, residence,
             child517IlliterateProp, adultIlliterateProp)

    # outcomes for uganda household dataset
    
    # definite indicators of HAZARDOUS labor Uganda house
    outcomeDefiniteHazard <- ugandaTemp %>%
      select(quesID, X, age, probInjuryIllness, probPhysHarass, probSexualAbuse, 
             probExtremeFatigue, workDayNight, carryLoad, operateMachEquip, exposedChem, 
             diveWater, extremeTemp, noiseVibration, dustFume, physAbuse, sexAbuse) %>%
      mutate(ageCat = ifelse(age >= 5 & age <= 17, "child517", "notCLage")) %>%
      select(-age) %>%
      mutate(probInjuryIllness = ifelse(probInjuryIllness == 1, 1, probInjuryIllness), 
             probPhysHarass = ifelse(probPhysHarass == 4, 1, probPhysHarass), 
             probSexualAbuse = ifelse(probSexualAbuse == 5, 1, probSexualAbuse), 
             probExtremeFatigue = ifelse(probExtremeFatigue == 6, 1, probExtremeFatigue), 
             workDayNight = ifelse(workDayNight == 1, 0, ifelse(workDayNight == 2, 1, ifelse(workDayNight == 3, 1, workDayNight))), 
             carryLoad = ifelse(carryLoad < 4, 1, ifelse(carryLoad == 4, 0, carryLoad)), 
             operateMachEquip = ifelse(operateMachEquip == 1, 1, ifelse(operateMachEquip == 2, 0, operateMachEquip)), 
             exposedChem = ifelse(exposedChem > 0, 1, ifelse(exposedChem == 0, 0, exposedChem)), 
             diveWater = ifelse(diveWater > 0, 1, ifelse(diveWater == 0, 0, diveWater)), 
             extremeTemp = ifelse(extremeTemp > 0, 1, ifelse(extremeTemp == 0, 0, extremeTemp)), 
             noiseVibration = ifelse(noiseVibration > 0, 1, ifelse(noiseVibration == 0, 0, noiseVibration)), 
             dustFume = ifelse(dustFume > 0, 1, ifelse(dustFume == 0, 0, dustFume)), 
             physAbuse = ifelse(physAbuse > 0, 1, ifelse(physAbuse == 0, 0, physAbuse)), 
             sexAbuse = ifelse(sexAbuse > 0, 1, ifelse(sexAbuse == 0, 0, sexAbuse))) %>%
      group_by(quesID, X, ageCat) %>%
      replace(is.na(.), 0) %>%
      mutate_all(as.numeric) %>%
      summarize(childNumDefiniteHazard = sum(probInjuryIllness, probPhysHarass, probSexualAbuse, 
                                             probExtremeFatigue, workDayNight, carryLoad, operateMachEquip, exposedChem, 
                                             diveWater, extremeTemp, noiseVibration, dustFume, physAbuse, sexAbuse)) %>%
      ungroup() %>%
      mutate(childNumDefiniteHazard = ifelse(childNumDefiniteHazard > 0, 1, 0)) %>%
      select(-X) %>%
      group_by(quesID, ageCat, childNumDefiniteHazard) %>%
      summarize(n = n()) %>%
      ungroup() %>%
      filter(ageCat == "child517", childNumDefiniteHazard == 1) %>%
      select(quesID, childNumDefiniteHazard = n)
    
    # definite indicators of worst form of child labor Uganda house
    outcomeDefiniteWorst <- ugandaTemp %>%
      select(quesID, X, age, probInjuryIllness, probEmotionHarass, 
             probPhysHarass, probSexualAbuse, probExtremeFatigue, probNoSchoolTime, 
             expSickness, markActivity, workDayNight, 
             exposedChem, diveWater, workUnderground, heightDangerous, extremeTemp, 
             noiseVibration, dustFume, physAbuse, sexAbuse, confined) %>%
      mutate(ageCat = ifelse(age >= 5 & age <= 17, "child517", "notCLage")) %>%
      select(-age) %>%
      mutate(probInjuryIllness = probInjuryIllness, 
             probEmotionHarass = ifelse(probEmotionHarass == 3, 1, probEmotionHarass), 
             probPhysHarass = ifelse(probPhysHarass == 4, 1, probPhysHarass), 
             probSexualAbuse = ifelse(probSexualAbuse == 5, 1, probSexualAbuse), 
             probExtremeFatigue = ifelse(probExtremeFatigue == 6, 1, probExtremeFatigue), 
             probNoSchoolTime = ifelse(probNoSchoolTime == 8, 1, probNoSchoolTime), 
             expSickness = ifelse(expSickness == 1, 1, ifelse(expSickness == 2, 0, expSickness)), 
             markActivity = ifelse(markActivity == 1, 1, ifelse(markActivity == 2, 0, markActivity)), 
             workDayNight = ifelse(workDayNight == 1, 0, ifelse(workDayNight == 2, 1, ifelse(workDayNight == 3, 1, workDayNight))), 
             exposedChem = ifelse(exposedChem > 0, 1, ifelse(exposedChem == 0, 0, exposedChem)), 
             diveWater = ifelse(diveWater > 0, 1, ifelse(diveWater == 0, 0, diveWater)), 
             workUnderground = ifelse(workUnderground > 0, 1, ifelse(workUnderground == 0, 0, workUnderground)), 
             heightDangerous = ifelse(heightDangerous > 0, 1, ifelse(heightDangerous == 0, 0, heightDangerous)), 
             extremeTemp = ifelse(extremeTemp > 0, 1, ifelse(extremeTemp == 0, 0, extremeTemp)), 
             noiseVibration = ifelse(noiseVibration > 0, 1, ifelse(noiseVibration == 0, 0, noiseVibration)), 
             dustFume = ifelse(dustFume > 0, 1, ifelse(dustFume == 0, 0, dustFume)), 
             physAbuse = ifelse(physAbuse > 0, 1, ifelse(physAbuse == 0, 0, physAbuse)), 
             sexAbuse = ifelse(sexAbuse > 0, 1, ifelse(sexAbuse == 0, 0, sexAbuse)), 
             confined = ifelse(confined > 0, 1, ifelse(confined == 0, 0, confined))) %>%
      group_by(quesID, X, ageCat) %>%
      replace(is.na(.), 0) %>%
      mutate_all(as.numeric) %>%
      summarize(childNumDefiniteWorst = sum(probInjuryIllness, probEmotionHarass, 
                                            probPhysHarass, probSexualAbuse, probExtremeFatigue, probNoSchoolTime, 
                                            expSickness, markActivity, workDayNight, 
                                            exposedChem, diveWater, workUnderground, heightDangerous, extremeTemp, 
                                            noiseVibration, dustFume, physAbuse, sexAbuse, confined)) %>%
      ungroup() %>%
      mutate(childNumDefiniteWorst = ifelse(childNumDefiniteWorst > 0, 1, 0)) %>%
      select(-X) %>%
      group_by(quesID, ageCat, childNumDefiniteWorst) %>%
      summarize(n = n()) %>%
      ungroup() %>%
      filter(ageCat == "child517", childNumDefiniteWorst == 1) %>%
      select(quesID, childNumDefiniteWorst = n)
 
    allHouseUganda <- data.frame(quesID = unique(uganda$quesID))
    
    # final outcome dataset uganda household
    ugandaHouseOutcomes <- full_join(allHouseUganda, outcomeDefiniteHazard, by = "quesID") %>%
      full_join(outcomeDefiniteWorst, by = "quesID") %>%
      replace(is.na(.), 0)
    
    # final Uganda household data
    ugandaHousehold <- full_join(ugandaHousePredictors, ugandaHouseOutcomes, by = "quesID")
    
    # UGANDA INDIVIDUAL DATASET
    # select desired variables for individual level characteristics
    ugandaIndPredictors <- ugandaTemp %>%
      select(quesID, X, relationHead, sex, age,
             fatherAlive, motherAlive, highestSchoolLevel, parentsAlive, liveParents, disabled,
             selfParent, enrolledSchool, sibling18, markActivity, primCareWork,
             mealsNum, clothing, shoes, 
             educAccess, healthAccess, schoolEnroll, 
             choreHoursSun, choreHoursMon, choreHoursTue, choreHoursWed, choreHoursThur,
             choreHoursFri, choreHoursSat) %>%
      filter(age >= 5 & age <= 17) %>%
      group_by(X) %>%
      mutate(maxChoreDay = pmax(choreHoursSun, choreHoursMon, 
                                      choreHoursTue, choreHoursWed, choreHoursThur,
                                      choreHoursFri, choreHoursSat, na.rm=TRUE)) %>%
      select(quesID, X, relationHead, sex, age,
             fatherAlive, motherAlive, highestSchoolLevel, parentsAlive, liveParents, disabled,
             selfParent, enrolledSchool, sibling18, markActivity, primCareWork,
             mealsNum, clothing, shoes, 
             educAccess, healthAccess, schoolEnroll, maxChoreDay)
    
    # all four individual outcome variables
    # definite hazard indicator Uganda
    outcomeDefiniteHazardInd <- ugandaTemp %>%
      select(X, age, probInjuryIllness, probPhysHarass, probSexualAbuse, 
             probExtremeFatigue, workDayNight, carryLoad, operateMachEquip, exposedChem, 
             diveWater, extremeTemp, noiseVibration, dustFume, physAbuse, sexAbuse) %>%
      filter(age >= 5 & age <= 17) %>%
      select(-age) %>%
      mutate(probInjuryIllness = ifelse(probInjuryIllness == 1, 1, probInjuryIllness), 
             probPhysHarass = ifelse(probPhysHarass == 4, 1, probPhysHarass), 
             probSexualAbuse = ifelse(probSexualAbuse == 5, 1, probSexualAbuse), 
             probExtremeFatigue = ifelse(probExtremeFatigue == 6, 1, probExtremeFatigue), 
             workDayNight = ifelse(workDayNight == 1, 0, ifelse(workDayNight == 2, 1, ifelse(workDayNight == 3, 1, workDayNight))), 
             carryLoad = ifelse(carryLoad < 4, 1, ifelse(carryLoad == 4, 0, carryLoad)), 
             operateMachEquip = ifelse(operateMachEquip == 1, 1, ifelse(operateMachEquip == 2, 0, operateMachEquip)), 
             exposedChem = ifelse(exposedChem > 0, 1, ifelse(exposedChem == 0, 0, exposedChem)), 
             diveWater = ifelse(diveWater > 0, 1, ifelse(diveWater == 0, 0, diveWater)), 
             extremeTemp = ifelse(extremeTemp > 0, 1, ifelse(extremeTemp == 0, 0, extremeTemp)), 
             noiseVibration = ifelse(noiseVibration > 0, 1, ifelse(noiseVibration == 0, 0, noiseVibration)), 
             dustFume = ifelse(dustFume > 0, 1, ifelse(dustFume == 0, 0, dustFume)), 
             physAbuse = ifelse(physAbuse > 0, 1, ifelse(physAbuse == 0, 0, physAbuse)), 
             sexAbuse = ifelse(sexAbuse > 0, 1, ifelse(sexAbuse == 0, 0, sexAbuse))) %>%
      group_by(X) %>%
      replace(is.na(.), 0) %>%
      mutate_all(as.numeric) %>%
      summarize(anyDefiniteHazardInd = sum(probInjuryIllness, probPhysHarass, probSexualAbuse, 
                                           probExtremeFatigue, workDayNight, carryLoad, operateMachEquip, exposedChem, 
                                           diveWater, extremeTemp, noiseVibration, dustFume, physAbuse, sexAbuse))
    
    # definite worst child labor indicator Uganda
    outcomeDefiniteWorstInd <- ugandaTemp %>%
      select(X, age, probInjuryIllness, probEmotionHarass, 
             probPhysHarass, probSexualAbuse, probExtremeFatigue, probNoSchoolTime, 
             expSickness, markActivity, workDayNight, 
             exposedChem, diveWater, workUnderground, heightDangerous, extremeTemp, 
             noiseVibration, dustFume, physAbuse, sexAbuse, confined) %>%
      filter(age >= 5 & age <= 17) %>%
      select(-age) %>%
      mutate(probInjuryIllness = probInjuryIllness, 
             probEmotionHarass = ifelse(probEmotionHarass == 3, 1, probEmotionHarass), 
             probPhysHarass = ifelse(probPhysHarass == 4, 1, probPhysHarass), 
             probSexualAbuse = ifelse(probSexualAbuse == 5, 1, probSexualAbuse), 
             probExtremeFatigue = ifelse(probExtremeFatigue == 6, 1, probExtremeFatigue), 
             probNoSchoolTime = ifelse(probNoSchoolTime == 8, 1, probNoSchoolTime), 
             expSickness = ifelse(expSickness == 1, 1, ifelse(expSickness == 2, 0, expSickness)), 
             markActivity = ifelse(markActivity == 1, 1, ifelse(markActivity == 2, 0, markActivity)), 
             workDayNight = ifelse(workDayNight == 1, 0, ifelse(workDayNight == 2, 1, ifelse(workDayNight == 3, 1, workDayNight))), 
             exposedChem = ifelse(exposedChem > 0, 1, ifelse(exposedChem == 0, 0, exposedChem)), 
             diveWater = ifelse(diveWater > 0, 1, ifelse(diveWater == 0, 0, diveWater)), 
             workUnderground = ifelse(workUnderground > 0, 1, ifelse(workUnderground == 0, 0, workUnderground)), 
             heightDangerous = ifelse(heightDangerous > 0, 1, ifelse(heightDangerous == 0, 0, heightDangerous)), 
             extremeTemp = ifelse(extremeTemp > 0, 1, ifelse(extremeTemp == 0, 0, extremeTemp)), 
             noiseVibration = ifelse(noiseVibration > 0, 1, ifelse(noiseVibration == 0, 0, noiseVibration)), 
             dustFume = ifelse(dustFume > 0, 1, ifelse(dustFume == 0, 0, dustFume)), 
             physAbuse = ifelse(physAbuse > 0, 1, ifelse(physAbuse == 0, 0, physAbuse)), 
             sexAbuse = ifelse(sexAbuse > 0, 1, ifelse(sexAbuse == 0, 0, sexAbuse)), 
             confined = ifelse(confined > 0, 1, ifelse(confined == 0, 0, confined))) %>%
      group_by(X) %>%
      replace(is.na(.), 0) %>%
      mutate_all(as.numeric) %>%
      summarize(anyDefiniteWorstInd = sum(probInjuryIllness, probEmotionHarass, 
                                          probPhysHarass, probSexualAbuse, probExtremeFatigue, probNoSchoolTime, 
                                          expSickness, markActivity, workDayNight, 
                                          exposedChem, diveWater, workUnderground, heightDangerous, extremeTemp, 
                                          noiseVibration, dustFume, physAbuse, sexAbuse, confined))
    
    
    # all outcomes for Uganda individual dataset
    ugIndvID <- data.frame("X" = unique(filter(uganda, age >= 5 & age <= 17)$X))
    ugandaIndOutcomes <- full_join(outcomeDefiniteHazardInd, ugIndvID, by = "X") %>%
      full_join(outcomeDefiniteWorstInd, by = "X") %>%
      replace(is.na(.), 0)

    # final Uganda individual data
    ugandaIndividual <- full_join(ugandaIndPredictors, ugandaIndOutcomes, by = "X") %>%
      left_join(ugandaHousehold, by = "quesID")

    write.csv(ugandaHousehold, file = paste("../FinalData/ugandaHousehold.csv"), row.names=FALSE)
    write.csv(ugandaIndividual, file = paste("../FinalData/ugandaIndividual.csv"), row.names=FALSE)
  }
  if(country != "Ethiopia" & country != "Uganda"){
    return(paste("Please pick either Ethiopia or Uganda"))
  }

}
