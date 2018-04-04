###############################################
#
# Script for costs for estimating and processing HAT RDT cost effectiveness analysis
# 
# Submitted to Plos One April 2018
#
################################################
#####
#=== Captial costs
# Depreciation function
depFunSL <- function(amount, salvageP, depP) (amount - (amount * salvageP)) / depP
salvV <- 0.0
depPeriodM <- 5
depPeriodF <- 20

generator <- 2500
camping <- 3984
vehicle <- 45000
furniture <- 840
rdtTransport <- 5000 # additioanl costs for transporting RDTs

fridge <- 1290
microscope <- 1992
centrifuge <- 2300
rotator <- 1100

labEquipment <- 1000

# Captial costs for fixed and mobile teams and for performing CATT or RDT
capitalF <- depFunSL(generator + furniture + fridge + microscope + centrifuge + labEquipment, salvV, depPeriodF)
capitalFC <- capitalF + depFunSL(rotator, salvV, depPeriodF)
capitalM <- depFunSL(generator + camping + vehicle + furniture + fridge + microscope + centrifuge + labEquipment, salvV, depPeriodM)
capitalMC <- capitalM + depFunSL(rotator, salvV, depPeriodM)
capitalMR <- capitalM + depFunSL(rdtTransport, salvV, depPeriodM)

#######
#=== Annual Costs
trainingF <- 235
trainingM <- 1375
insuranceF <- 200
insuranceM <- 2748
staffM <- 26184
shipmentF <- 0
shipmentM <- 0
annualF <- trainingF + insuranceF + shipmentF
annualM <- trainingM + insuranceM + staffM + shipmentM

####
#=== Daily costs
allowances <- 64.5 # Mobile team
runningM <- 32.5 # Running mobile team
runningF <- 3.5 # Fixed health facility


####
#== Staff wages
doctorA <- 9840
doctorH <- doctorA / staffDays

techA <- 1560 # Lab technician
techH <- techA / staffDays

nurseA <- 1260
nurseH <- nurseA / staffDays

###
#== Costs per test
consultationT <- 20
consultationC <- doctorH * (consultationT / 60)

rdtT <- 17 # Time for RDT
rdtC <- 0.6 # Cost of RDT
rdtCF <- rdtC + (techH * (rdtT / 60))

cattT <- 10 # Time for CATT
cattC <- 0.7 # Cost for CATT
cattFAdj <- 0.06 # Adjustment for wastage at fixed health facilities
cattCF <- cattC + (techH * (cattT / 60)) + cattFAdj

cattDT <- 15 # Time for CATT dilutions
cattDC <- 2.99 # Cost of CATT dilutions
cattDCF <- cattDC + (techH * (cattDT / 60))

gpT <- 15 # Time for LNP
gpC <- 0.38 # Cost of LNP
gpCF <- gpC + (techH * (gpT / 60))

ctcT <- 18 # Time for CTC
ctcC <- 1.54 # Cost of CTC
ctcCF <- ctcC + (techH * (ctcT / 60))

maectT <- 30 # Time for mAECT
maectC <- 7.2 # Cost of mAECT
maectCF <- maectC + (techH * (maectT / 60))

lpT <- 30 # Time for Lumbar puncture
lpC <- 2 # Cost of Lumbar puncture
lpCF <- lpC + (techH * (lpT / 60))

stage2TC <- 9 # Cost of stage 2 tretament

######
#=== Participant costs
screenTF <- 1.61 # Travel to fixed health facilities
screenTM <- 0.15 # Travel to mobile teams
screenWF <- 0.36 # Lost work for fixed health facilities
screenWM <- 0.17 # Lost work for mobile teams

# Treatment costs stage 1
treat1TF <- 1.61 # Travel to fixed health facilities
treat1TM <- 1.61 # Travel from mobile teams
treat1MF <- 0.36 # Lost work for fixed health facilities
treat1MM <- 0.36 # Lost work for mobile teams

# Treatment costs stage 2
treat2Supp <- 15 # Supplementary costs for stage 2 treatment

treat2TF <- 3.2 # Travel to fixed health facilities
treat2TM <- 3.2 # Travel from mobile teams
treat2MF <- 10.2 + treat2Supp
treat2MM <- 10.2 + treat2Supp



