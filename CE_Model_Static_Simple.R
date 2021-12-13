###############################################
#
# Model for estimating and processing HAT RDT cost effectiveness analysis
# Requires accompanying scripts
# The model has a default setting RDT in a fixed health facility
# Submitted to Plos One April 2018
#
################################################

# Test parameters
source("ParameterSource_Paper_Simple.R")
# Cost parameters
source("CostSource_Revised_Simple.R")

# Options -----------------------------------------------------------------
# mobile: TRUE = mobile team screening; FALSE = fixed health facility
# catt: TRUE = screening with CATT; FALSE = RDT
# cattT: TRUE = CATT dilutions included
# participant: TRUE = participant costs included
# treatment: TRUE = tretament costs included
# mAECT: TRUE = mAECT used
# pprev: (numeric) != NA = forced prevalence
# presentN: (numeric) != NA = forced numbers screening
# additionalConf: = additional confirmation costs
# nurse: TRUE = clinical consultation wiht a nurse; FALSE = with a doctor
# Remaining parameters defined in attached script, but can also be forced

runModelE <- function(mobile = FALSE, catt = FALSE, cattT = FALSE, participant = TRUE, treatment = FALSE, mAECT = TRUE, pprev = NA, presentN = NA, additionalConf = 0, nurse = TRUE, rdtse = rdtMse, cattse = cattMse, cattdse = cattdMse, rdtsp = rdtMsp, cattsp = cattMsp, cattdsp = cattdMsp){
  #	Names of outputs
  dName <- c("tPresent", "tInf", "tnInf", "screenTP", "screenFN", "screenFP", "screenTN", "cattdTP", "cattdFN", "cattdFP", "cattdTN", "TPcrs", "FPcrs", "nlpFP", "nlpTP", "lpTP", "ctcTP", "maectTP", "crsPos", "stage1", "stage2", "stage1T", "stage2T", "treated", "totalCost", "CapitalCosts", "AnnualCosts", "DailyCosts", "upfrontCosts", "screeningCosts", "participantCosts", "treatCosts", "partTreat", "totalTreat", "crsCosts")
  
  # An output vector
  outputM <- vector(length = length(dName))
  
  # Numbers presenting
  presentM <- ifelse(!is.na(presentN) & mobile, presentN, presentM)
  presentF <- ifelse(!is.na(presentN) & !mobile, presentN, presentF)
  tPresent <- ifelse(mobile, presentM * daysSM, presentF * daysSF)
  pStage2 <- ifelse(mobile, pS2M, pS2F)
  # Define the prevalence
  if(is.na(pprev))  prev <- ifelse(mobile, getPrevM, getPrevF)
  if(!is.na(pprev))  prev <- pprev
  
  # Screening sensitivity
  screenSe <- ifelse(catt, cattse, rdtse)
  screenSp <- ifelse(catt, cattsp, rdtsp)
  
  #Number of cases
  tInf <- tPresent * prev
  # Number of non-cases
  tnInf <- tPresent - tInf
  
  # Screening results  
    screenTP <- tInf * screenSe
    screenFN <- tInf - screenTP
    screenFP <- tnInf * (1 - screenSp)
    screenTN <- tnInf - screenFP
  # For CATT dilutions
    cattdTP <- 0
    cattdFN <- 0
    cattdFP <- 0
    cattdTN <- 0
    
    if(cattT & catt){
      cattdTP <- screenTP * cattdMse
      cattdFN <- screenTP - cattdTP
      screenFN <- screenFN + cattdFN
      cattdFP <- screenFP * (1 - cattdsp)
      cattdTN <- screenFP - cattdFP
    }
    if(cattT & !catt){
      cattdTP <- screenTP * cattse * cattdMse
      cattdFN <- screenTP - cattdTP
      screenFN <- screenFN + cattdFN
      cattdFP <- screenFP * (1 - cattsp) * (1 - cattdsp)
      cattdTN <- screenFP - cattdFP
    }
    
    # True positives and false positives that proceed to microscopy
    TPcrs <- screenTP - cattdFN
    FPcrs <- screenFP - cattdTN
    
    # Microscopy
    nlpFP <- FPcrs * pPalpFP
    nlpTP <- TPcrs * pPalpTP
    lpTP <- nlpTP * lpse
    ctcTP <- (TPcrs - lpTP) * ctcse
    maectTP <- 0
    if(mAECT) maectTP <- (TPcrs - lpTP - ctcTP) * maectse
    
    # Positive by microscopy
    crsPos <- lpTP + ctcTP + maectTP
    
    stage2 <- (crsPos * pStage2)
    stage1 <- crsPos - stage2
    
    # Treatment efficacy
    stage1M <- stage1 * mS1
    stage1T <- (stage1 - stage1M) * eS1
    stage2M <- stage2 * mS2
    stage2T <- (stage2 - stage2M) * eS2
    
    treated <- stage2T + stage1T
    
    # Running costs
    runningPAM <- (allowances + runningM) * daysSM
    runningPAF <- runningF * daysSF
    
    upfrontCosts <- capitalF + annualF + runningPAF
    capitalCost <- capitalF
    annualCost <- annualF
    dailyCost <- runningPAF
    # Costs by screening algorithm
    if(mobile & !catt) {
      upfrontCosts <- capitalMR  + annualM + runningPAM
      capitalCost <- capitalMR
      annualCost <- annualM
      dailyCost <- runningPAM
    }
    if(mobile & catt){
      upfrontCosts <- capitalMC + annualM + runningPAM
      capitalCost <- capitalMC
      annualCost <- annualM
      dailyCost <- runningPAM
    }
    if(!mobile & catt){
      upfrontCosts <- capitalFC + annualF + runningPAF
      capitalCost <- capitalFC
      annualCost <- annualF
      dailyCost <- runningPAF
    }
    if(nurse) consultationC <- nurseH * (consultationT / 60)
    
    # Screening costs
    screeningCosts <- (rdtCF + consultationC) * tPresent
    if(mobile & !catt) screeningCosts <- rdtC * tPresent
    if(mobile & catt) screeningCosts <- cattC * tPresent
    if(!mobile & catt) screeningCosts <- (cattCF + consultationC) * tPresent
    
    if(mobile & cattT) screeningCosts <- screeningCosts + ((screenTP + screenFP) * cattDC)
    if(!mobile & cattT) screeningCosts <- screeningCosts + ((screenTP + screenFP) * cattDCF)
    
    # Costs for the participants
    participantCosts <- (screenTF + screenWF) * tPresent
    if(mobile) participantCosts <- (screenTM + screenWM) * tPresent
    if(!participant) participantCosts <- 0
    if(!mAECT){
      maectC <- 0
      maectCF <- 0
    }
    # Microscopy costs
    crsCosts <- ((nlpFP + nlpTP) * gpC) + ((TPcrs + FPcrs - lpTP) * ctcC) + ((TPcrs + FPcrs - lpTP - ctcTP) * maectC) + ((TPcrs + FPcrs) * additionalConf)
    if(!mobile) crsCosts <- ((nlpFP + nlpTP) * gpCF) + ((TPcrs + FPcrs - lpTP) * ctcCF) + ((TPcrs + FPcrs - lpTP - ctcTP) * maectCF) + ((TPcrs + FPcrs) * additionalConf)
    
    # Treatment costs
    treatCosts <- crsPos * lpC + stage2 * stage2TC
    if(!mobile) treatCosts <- crsPos * lpCF + (stage2 * stage2TC)
    
    partTreat <- stage1 * (treat1TM + treat1MM) + stage2 * (treat2TM + treat2MM) 
    if(!mobile) partTreat <- stage1 * (treat1TF + treat1MF) + stage2 * (treat2TF + treat2MF) 
    if(!participant) partTreat <- 0
    
    if(!treatment) treatCosts <- 0
    if(!treatment) partTreat <- 0
    treatCostsTotal <- treatCosts + partTreat
    
    # Overall costs
    totalCost <- upfrontCosts + screeningCosts + participantCosts + crsCosts + treatCosts + partTreat
    
    screenFP <- screenFP - cattdTN
    screenTP <- screenTP - cattdFN
    
    # The results
    outputM <- c(tPresent, tInf, tnInf, screenTP, screenFN,screenFP, screenTN, cattdTP, cattdFN, cattdFP, cattdTN, TPcrs, FPcrs, nlpFP, nlpTP, lpTP, ctcTP, maectTP, crsPos, stage1, stage2, stage1T, stage2T, treated, totalCost, capitalCost, annualCost, dailyCost, upfrontCosts, screeningCosts, participantCosts, treatCosts, partTreat, treatCostsTotal, crsCosts)
    names(outputM) <- dName
    
  return(outputM)
} 

# Processes the results for the tables in the paper
processResults <- function(model){
  test <- model
  tCases<- test["tInf"]
  tPresented <- test["tPresent"]
  testFull <- array(dim = 9)
  testFull[1:7] <- c(test["screenFP"], (test["screenFP"] / tPresented) * 100, test["crsPos"], (test["crsPos"] / tCases) * 100,  test["treated"], (test["treated"] / tCases) * 100, test["totalCost"])
  testFull[8] <- test["totalCost"] / (test["treated"] * dalyTP)
  testFull[9] <- test["totalCost"] / test["crsPos"]
  names(testFull) <- c("screenFP", "pScreenFP", "CRSPos", "pCRSPos", "treated", "pTreated", "totalCost", "Cost_DALY", "ACER")
  testFull
}

# Gives a breakdown of the costs
processCosts <- function(model){
  test <- model
  tCost<- test["totalCost"]
  testFull <- test[c("CapitalCosts", "AnnualCosts", "DailyCosts", "screeningCosts", "participantCosts", "crsCosts", "totalTreat")]
  testFullMat <- matrix(NA, nrow = length(testFull), ncol = 3)
  testFullMat[,1] <- testFull
  testFullMat[,2] <- (testFull / tCost) * 100
  testFullMat[,3] <- (testFull / test["treated"])
  colnames(testFullMat) <- c("Cost", "pCost", "CostPatient")
  testFullMat
}

