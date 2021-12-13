###############################################
#
# Parameters for RDT cost effectiveness analysis
# Derived from Lumbala et al (2017) 10.1371/journal.pone.0180555
# Submitted to Plos One April 2018
#
################################################


# RDT ---------------------------------------------------------------------
# RDT specificity
rdtMsp <- 0.9713

# RDT sensitivity
rdtMse <- 0.9198


# CATT whole blood --------------------------------------------------------------------
# CATT specificity
cattMsp <- 0.9802

# CATT sensitivity
cattMse <- 0.6908


# CATT dilutions ----------------------------------------------------------
# CATT dilutions senstivity
cattdMse <- 0.8706

# CATT dilutions specificity
cattdMsp <- 0.7959


# Microscopy parameters ---------------------------------------------------
# Lymph node palpation ----------------------------------------------------
# Cases with palpable nodes
pPalpTP <- 0.3969
# Non-cases with palpable nodes
pPalpFP <- 0.1165
# Sensitiity of LNP
lpse <- 0.7115

# CTC sensitivity
ctcse <- 0.5521

# mAECT sensitivity
maectse <- 0.7833


# Prevalence --------------------------------------------------------------
# Mobile team
getPrevM <- 0.00817

# Fixed health facilities
getPrevF <- 0.0176

# Staging -----------------------------------------------------------------
#== Poportion stage 2 - fixed health facilities
pS2F <- 0.5

#== Poportion stage 2 - mobile teams
pS2M <- 0.2222

############
#====== Treatment

# Treatment parameters ----------------------------------------------------
# Effectiveness stage 1
eS1 <- 0.99
# Effectiveness stage 2
eS2 <- 0.96
# Mortality stage 1
mS1 <- 0.001
# Mortality stage 2
mS2 <- 0.01


###########
# DALYS
dalyTP <- 27.45

##########

# Fixed parameters --------------------------------------------------------
# Numbers presenting at mobile teams
presentM <- 250
# Numbers presenting at health facilities
presentF <- 10

# Days worked at health facilities
daysSF <- 5 * 52 - 10

# Days worked by mobile teams
daysSM <- 20 * 11

# Number of days worked by fixed health facility staff
staffDays <- (40 * 52) - 80
