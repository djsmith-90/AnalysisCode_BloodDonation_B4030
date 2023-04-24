### Script for paper 'Exploring causality from observational data: An example assessing whether religiosity promotes cooperation' using ALSPAC data (ALSPAC B-number B4030)
### Script 3e: Analysing mother's data using NARMICE assuming outcome and exposure are both MNAR
### Created 27/2/2023 by Dan Major-Smith
### R version 4.1.0

## Analysis plan for this paper has been accepted as a Registered Report for the journal Evolutionary Human Sciences, and the plan is available on the OSF: https://osf.io/z5gcm/

## Load in R packages (will assume packages are already installed on BP)
# Note: Have installed the 'NARMICE' package elsewhere so does not conflict with standard 'mice' package. To install the NARMICE package, first load the 'devtools' package and then run "install_github("moreno-betancur/mice")". For more details, see: https://raw.githack.com/moreno-betancur/NARFCS/master/Vignette.html
library(tidyverse)
library(mice, lib.loc = "/user/home/ds16565/NARMICE_package")


## In this script, will not include any missingness markers in the imputation (as with partner's data), to see if this impacts results


## Add the args command to take arguments from the sbatch script
args <- commandArgs(trailingOnly = TRUE)

# Save args/array number as numeric, and use this to set which exposure CSP to use and which seed to use
a <- as.numeric(args)
print(a)

exposure_CSP <- seq(-2, 0, by = 0.25)[a]
print(exposure_CSP)

seed <- c(183358, 815082, 824038, 959070, 916009, 557517, 364951, 469059, 771020)[a]


###############################################################################################
#### Read in the mother's processed data

load("/user/home/ds16565/BloodDonation/Data/data_mum_processed_B4030.RData")

## Also move exposures and outcome to end of dataset, to match synthesised data
data_mum <- data_mum %>%
  relocate(mum_belief, mum_identity, mum_attend, mum_donate, .after = mum_smkPreg)
  
  
## If using synthesised data here, read this dataset in instead - NOTE: The sample size is slightly smaller in the synthetic dataset, as unique replicates with identical data in both the observed and synthetic data have been removed for confidentiality reasons. Also, the statistics in the script below refer to the observed data, and will differ for the synthetic data (although ideally not by that much!)
#load("/user/home/ds16565/BloodDonation/Data/syntheticData_mum_B4030.RData")
#data_mum <- data_mum_syn_df


## Make a new dataframe to work with for these NARMICE analyses
data_mum_narmice <- data_mum

glimpse(data_mum_narmice)


###### NARMICE analysis 3: Exposure (religious attendance) and outcome (blood donation) MNAR

## Set up prediction matrix for the imputation
ini <- mice(data_mum_narmice, maxit = 0, print = TRUE)

# Specify the prediction matrix for the observable data
pred <- ini$predictorMatrix
pred

# Set-up the prediction matrix for the unidentifiable part of the model (i.e., the missing-not-at-random element)
# In this case the whole matrix should be zeroes because the unidentifiable part of the imputation model contains a single constant (CSP*M) rather than additional contributions from the other variables in the dataset
predSens <- ini$predictorMatrix
predSens

predSens[predSens == 1] <- 0
predSens

# Set up list with sensitivity parameter values - Fill in the exposure CSP here, will loop over outcome CSPs below
pSens <- rep(list(list("")), ncol(data_mum_narmice))
names(pSens) <- names(data_mum_narmice)
pSens

pSens[["mum_attend"]] <- list(c(exposure_CSP))
pSens

# Set up vector describing manner of imputation for each variable - As we want to run vary the association between both the outcome blood donation and it's misisngness, as well as the exposure religious attendance and it's missingness, we specify these as 'logregSens', which is a logistic sensitivity analysis - All others are the same as for the standard MI above
meth <- ini$method
meth

meth["mum_donate"] <- "logregSens"
meth["mum_attend"] <- "logregSens"
meth

# Choose number of imputations (50) and burn-in period (10)
narmice_numimps <- 50
narmice_numiter <- 10

# To collect the parameters of interest
tipping_outcomeExposureMNAR <- as.data.frame(array(dim = c(dim = length(seq.int(-2, 0, by = 0.25)), 23))) # Number of sensitivity values we're going to try (varying the CSP from -2 to 0, in steps of 0.25), plus the number of parameters we're going to store (here, is 23 [see row below])
colnames(tipping_outcomeExposureMNAR) <- c("csp_outcome", "msp_outcome", "imor_outcome", "sampprev_outcome", 
                                    "csp_exposure", "msp_exposure", "imor_exposure", "sampprev_exposure",
                                    "est_unadj", "se_unadj", "lci_unadj", "uci_unadj", "p_unadj", 
                                    "est_con", "se_con", "lci_con", "uci_con", "p_con",
                                    "est_conMed", "se_conMed", "lci_conMed", "uci_conMed", "p_conMed")
tipping_outcomeExposureMNAR

# Looping over delta/CSP values, for both outcome (i) and exposure (j)
set.seed(seed)
k <- 0
for (i in seq.int(-2, 0, by = 0.25)) {
  k <- k+1 
  print(paste0("CSP for outcome = ", i, ": CSP for exposure = ", exposure_CSP))
    
  # specify a delta/CSP value for the prediction equation for missing data for the outcome (exposure defined above)
  pSens[["mum_donate"]]<-list(c(i))
    
  # NARMICE imputation
  imp_NARMICE <- mice(data_mum_narmice, m = narmice_numimps, method = meth, predictorMatrix = pred,
                      predictorSens=predSens, parmSens=pSens, print = FALSE, maxit = narmice_numiter)
    
  # Derive the MSP for the given CSP value, for both outcome and exposure (convert to wide format for this, create a missingness marker, then estimate MSP for each imputation)
  imp_wide <- mice::complete(imp_NARMICE, "broad", inc=TRUE)
  imp_wide$M_donate <- ifelse(is.na(imp_wide$mum_donate.0), 1, 0)
  imp_wide$M_attend <- ifelse(is.na(imp_wide$mum_attend.0), 1, 0)
  msp_outcome <- 1:narmice_numimps
  msp_exposure <- 1:narmice_numimps
  for (z in 1:narmice_numimps) {
    tempvar_outcome <- paste0("mum_donate.", z)
    x_outcome <- glm(formula = get(tempvar_outcome) ~ M_donate, family = "binomial", data = imp_wide)
    msp_outcome[z] <- x_outcome$coefficients[2]
      
    tempvar_exposure <- paste0("mum_attend.", z)
    x_exposure <- glm(formula = get(tempvar_exposure) ~ M_attend, family = "binomial", data = imp_wide)
    msp_exposure[z] <- x_exposure$coefficients[2]
  }
    
  # Derive the prevalence of outcome and exposure in sample
  wholesampprev_outcome <- round(summary(pool(with(imp_NARMICE, 
                                           glm(mum_donate ~ 1, family = binomial(link = "identity"))))), 3)
  
  wholesampprev_exposure <- round(summary(pool(with(imp_NARMICE, 
                                                   glm(mum_attend ~ 1, family = binomial(link = "identity"))))), 3)
    
  ## Run the logistic models in in these NARMICE imputed datasets
  # Unadjusted
  res_unadj <- round(summary(pool(with(imp_NARMICE, 
                                       glm(mum_donate ~ mum_attend, family = "binomial")))), 3)
    
  # Confounder-only adjusted
  res_con <- round(summary(pool(with(imp_NARMICE, 
                                     glm(mum_donate ~ mum_attend + mum_age + mum_ethnicity + mum_edu + home + 
                                           imd + rural + mum_finDiffs + mum_employed + mum_compMonth + 
                                           comp_postPreg, family = "binomial")))), 3)
    
  # Confounder and/or mediator adjusted
  res_conMed <- round(summary(pool(with(imp_NARMICE, 
                                        glm(mum_donate ~ mum_attend + mum_age + mum_ethnicity + mum_edu + home + 
                                              imd + rural + mum_finDiffs + mum_employed + mum_compMonth + 
                                              comp_postPreg + mum_marital + parity + mum_locus + mum_health, 
                                            family = "binomial")))), 3)
    
  # Store these estimates in the 'tipping' dataframe
  tipping_outcomeExposureMNAR[k,"csp_outcome"] <- i
  tipping_outcomeExposureMNAR[k,"msp_outcome"] <- mean(msp_outcome)
  tipping_outcomeExposureMNAR[k,"imor_outcome"] <- exp(mean(msp_outcome))
  tipping_outcomeExposureMNAR[k,"sampprev_outcome"] <- wholesampprev_outcome["(Intercept)", "est"]
  tipping_outcomeExposureMNAR[k,"csp_exposure"] <- exposure_CSP
  tipping_outcomeExposureMNAR[k,"msp_exposure"] <- mean(msp_exposure)
  tipping_outcomeExposureMNAR[k,"imor_exposure"] <- exp(mean(msp_exposure))
  tipping_outcomeExposureMNAR[k,"sampprev_exposure"] <- wholesampprev_exposure["(Intercept)", "est"] 
  tipping_outcomeExposureMNAR[k,"est_unadj"] <- res_unadj["mum_attend2", "est"]
  tipping_outcomeExposureMNAR[k,"se_unadj"] <- res_unadj["mum_attend2", "se"]
  tipping_outcomeExposureMNAR[k,"lci_unadj"] <- res_unadj["mum_attend2", "lo 95"]
  tipping_outcomeExposureMNAR[k,"uci_unadj"] <- res_unadj["mum_attend2", "hi 95"]
  tipping_outcomeExposureMNAR[k,"p_unadj"] <- res_unadj["mum_attend2", "Pr(>|t|)"]
  tipping_outcomeExposureMNAR[k,"est_con"] <- res_con["mum_attend2", "est"]
  tipping_outcomeExposureMNAR[k,"se_con"] <- res_con["mum_attend2", "se"]
  tipping_outcomeExposureMNAR[k,"lci_con"] <- res_con["mum_attend2", "lo 95"]
  tipping_outcomeExposureMNAR[k,"uci_con"] <- res_con["mum_attend2", "hi 95"]
  tipping_outcomeExposureMNAR[k,"p_con"] <- res_con["mum_attend2", "Pr(>|t|)"]
  tipping_outcomeExposureMNAR[k,"est_conMed"] <- res_conMed["mum_attend2", "est"]
  tipping_outcomeExposureMNAR[k,"se_conMed"] <- res_conMed["mum_attend2", "se"]
  tipping_outcomeExposureMNAR[k,"lci_conMed"] <- res_conMed["mum_attend2", "lo 95"]
  tipping_outcomeExposureMNAR[k,"uci_conMed"] <- res_conMed["mum_attend2", "hi 95"]
  tipping_outcomeExposureMNAR[k,"p_conMed"] <- res_conMed["mum_attend2", "Pr(>|t|)"]
  
  print(tipping_outcomeExposureMNAR[k,])
  
}

# Look at the 'tipping' output which contains all the values/estimates
tipping_outcomeExposureMNAR

# Convert the log odds to ORs
tipping_outcomeExposureMNAR$or_unadj <- exp(tipping_outcomeExposureMNAR$est_unadj)
tipping_outcomeExposureMNAR$lci_or_unadj <- exp(tipping_outcomeExposureMNAR$lci_unadj)
tipping_outcomeExposureMNAR$uci_or_unadj <- exp(tipping_outcomeExposureMNAR$uci_unadj)
tipping_outcomeExposureMNAR$or_con <- exp(tipping_outcomeExposureMNAR$est_con)
tipping_outcomeExposureMNAR$lci_or_con <- exp(tipping_outcomeExposureMNAR$lci_con)
tipping_outcomeExposureMNAR$uci_or_con <- exp(tipping_outcomeExposureMNAR$uci_con)
tipping_outcomeExposureMNAR$or_conMed <- exp(tipping_outcomeExposureMNAR$est_conMed)
tipping_outcomeExposureMNAR$lci_or_conMed <- exp(tipping_outcomeExposureMNAR$lci_conMed)
tipping_outcomeExposureMNAR$uci_or_conMed <- exp(tipping_outcomeExposureMNAR$uci_conMed)
tipping_outcomeExposureMNAR

## Save this table
write_csv(tipping_outcomeExposureMNAR, paste0("/user/home/ds16565/BloodDonation/Results_mum/Results_mum_NARMICE_outcomeExposureMNAR_noM_", a, ".csv"))

