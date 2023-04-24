### Script for paper 'Exploring causality from observational data: An example assessing whether religiosity promotes cooperation' using ALSPAC data (ALSPAC B-number B4030)
### Script 3d: Analysing mother's data using NARMICE assuming outcome is MNAR
### Created 27/2/2023 by Dan Major-Smith
### R version 4.1.0

## Analysis plan for this paper has been accepted as a Registered Report for the journal Evolutionary Human Sciences, and the plan is available on the OSF: https://osf.io/z5gcm/

## Load in R packages (will assume packages are already installed on BP)
# Note: Have installed the 'NARMICE' package elsewhere so does not conflict with standard 'mice' package. To install the NARMICE package, first load the 'devtools' package and then run "install_github("moreno-betancur/mice")". For more details, see: https://raw.githack.com/moreno-betancur/NARFCS/master/Vignette.html
library(tidyverse)
library(mice, lib.loc = "/user/home/ds16565/NARMICE_package")



###############################################################################################
#### Read in the mother's processed data

load("/user/home/ds16565/BloodDonation/Data/data_mum_processed_B4030.RData")

## Also move exposures and outcome to end of dataset, to match synthesised data
data_mum <- data_mum %>%
  relocate(mum_belief, mum_identity, mum_attend, mum_donate, .after = mum_smkPreg)
  
  
## If using synthesised data here, read this dataset in instead - NOTE: The sample size is slightly smaller in the synthetic dataset, as unique replicates with identical data in both the observed and synthetic data have been removed for confidentiality reasons. Also, the statistics in the script below refer to the observed data, and will differ for the synthetic data (although ideally not by that much!)
#load("/user/home/ds16565/BloodDonation/Data/syntheticData_mum_B4030.RData")
#data_mum <- data_mum_syn_df


## Make a new dataframe to work with for these NARMICE analyses and add missingness indicators for each variable with missing data
data_mum_narmice <- data_mum %>%
  mutate(M_age = factor(ifelse(is.na(mum_age), "Yes", "No"))) %>%
  mutate(M_age = factor(M_age, levels = c("No", "Yes"))) %>%
  mutate(M_ethnicity = factor(ifelse(is.na(mum_ethnicity), "Yes", "No"))) %>%
  mutate(M_ethnicity = factor(M_ethnicity, levels = c("No", "Yes"))) %>%
  mutate(M_edu = factor(ifelse(is.na(mum_edu), "Yes", "No"))) %>%
  mutate(M_edu = factor(M_edu, levels = c("No", "Yes"))) %>%
  mutate(M_home = factor(ifelse(is.na(home), "Yes", "No"))) %>%
  mutate(M_home = factor(M_home, levels = c("No", "Yes"))) %>%
  mutate(M_imd = factor(ifelse(is.na(imd), "Yes", "No"))) %>%
  mutate(M_imd = factor(M_imd, levels = c("No", "Yes"))) %>%
  mutate(M_rural = factor(ifelse(is.na(rural), "Yes", "No"))) %>%
  mutate(M_rural = factor(M_rural, levels = c("No", "Yes"))) %>%
  mutate(M_marital = factor(ifelse(is.na(mum_marital), "Yes", "No"))) %>%
  mutate(M_marital = factor(M_marital, levels = c("No", "Yes"))) %>%
  mutate(M_parity = factor(ifelse(is.na(parity), "Yes", "No"))) %>%
  mutate(M_parity = factor(M_parity, levels = c("No", "Yes"))) %>%
  mutate(M_locus = factor(ifelse(is.na(mum_locus), "Yes", "No"))) %>%
  mutate(M_locus = factor(M_locus, levels = c("No", "Yes"))) %>%
  mutate(M_finDiffs = factor(ifelse(is.na(mum_finDiffs), "Yes", "No"))) %>%
  mutate(M_finDiffs = factor(M_finDiffs, levels = c("No", "Yes"))) %>%
  mutate(M_health = factor(ifelse(is.na(mum_health), "Yes", "No"))) %>%
  mutate(M_health = factor(M_health, levels = c("No", "Yes"))) %>%
  mutate(M_compMonth = factor(ifelse(is.na(mum_compMonth), "Yes", "No"))) %>%
  mutate(M_compMonth = factor(M_compMonth, levels = c("No", "Yes"))) %>%
  mutate(M_dep = factor(ifelse(is.na(mum_dep), "Yes", "No"))) %>%
  mutate(M_dep = factor(M_dep, levels = c("No", "Yes"))) %>%
  mutate(M_compPreg = factor(ifelse(is.na(comp_postPreg), "Yes", "No"))) %>%
  mutate(M_compPreg = factor(M_compPreg, levels = c("No", "Yes"))) %>%
  mutate(M_everSmk = factor(ifelse(is.na(mum_everSmk), "Yes", "No"))) %>%
  mutate(M_everSmk = factor(M_everSmk, levels = c("No", "Yes"))) %>%
  mutate(M_alcPrePreg = factor(ifelse(is.na(mum_alcPrePreg), "Yes", "No"))) %>%
  mutate(M_alcPrePreg = factor(M_alcPrePreg, levels = c("No", "Yes"))) %>%
  mutate(M_alcPreg = factor(ifelse(is.na(mum_alcPreg), "Yes", "No"))) %>%
  mutate(M_alcPreg = factor(M_alcPreg, levels = c("No", "Yes"))) %>%
  mutate(M_occSoc = factor(ifelse(is.na(mum_occSocClass), "Yes", "No"))) %>%
  mutate(M_occSoc = factor(M_occSoc, levels = c("No", "Yes"))) %>%
  mutate(M_car = factor(ifelse(is.na(carAccess), "Yes", "No"))) %>%
  mutate(M_car = factor(M_car, levels = c("No", "Yes"))) %>%
  mutate(M_employed = factor(ifelse(is.na(mum_employed), "Yes", "No"))) %>%
  mutate(M_employed = factor(M_employed, levels = c("No", "Yes"))) %>%
  mutate(M_smkPreg = factor(ifelse(is.na(mum_smkPreg), "Yes", "No"))) %>%
  mutate(M_smkPreg = factor(M_smkPreg, levels = c("No", "Yes"))) %>%
  mutate(M_belief = factor(ifelse(is.na(mum_belief), "Yes", "No"))) %>%
  mutate(M_belief = factor(M_belief, levels = c("No", "Yes"))) %>%
  mutate(M_identity = factor(ifelse(is.na(mum_identity), "Yes", "No"))) %>%
  mutate(M_identity = factor(M_identity, levels = c("No", "Yes"))) %>%
  mutate(M_attend = factor(ifelse(is.na(mum_attend), "Yes", "No"))) %>%
  mutate(M_attend = factor(M_attend, levels = c("No", "Yes"))) %>%
  mutate(M_donate = factor(ifelse(is.na(mum_donate), "Yes", "No"))) %>%
  mutate(M_donate = factor(M_donate, levels = c("No", "Yes")))

glimpse(data_mum_narmice)


###### NARMICE analysis 2: Outcome (blood donation) MNAR

## Set up prediction matrix for the imputation
ini <- mice(data_mum_narmice, maxit = 0, print = TRUE)

# Specify the prediction matrix for the observable data - And edit so that the missingness markers don't predict the variables they represent
pred <- ini$predictorMatrix
pred

pred["mum_age", "M_age"] <- 0
pred["mum_ethnicity", "M_ethnicity"] <- 0
pred["mum_edu", "M_edu"] <- 0
pred["home", "M_home"] <- 0
pred["imd", "M_imd"] <- 0
pred["rural", "M_rural"] <- 0
pred["mum_marital", "M_marital"] <- 0
pred["parity", "M_parity"] <- 0
pred["mum_locus", "M_locus"] <- 0
pred["mum_finDiffs", "M_finDiffs"] <- 0
pred["mum_health", "M_health"] <- 0
pred["mum_compMonth", "M_compMonth"] <- 0
pred["mum_dep", "M_dep"] <- 0
pred["comp_postPreg", "M_compPreg"] <- 0
pred["mum_everSmk", "M_everSmk"] <- 0
pred["mum_alcPrePreg", "M_alcPrePreg"] <- 0
pred["mum_alcPreg", "M_alcPreg"] <- 0
pred["mum_occSocClass", "M_occSoc"] <- 0
pred["carAccess", "M_car"] <- 0
pred["mum_employed", "M_employed"] <- 0
pred["mum_smkPreg", "M_smkPreg"] <- 0
pred["mum_belief", "M_belief"] <- 0
pred["mum_identity", "M_identity"] <- 0
pred["mum_attend", "M_attend"] <- 0
pred["mum_donate", "M_donate"] <- 0
pred

# Set-up the prediction matrix for the unidentifiable part of the model (i.e., the missing-not-at-random element)
# In this case the whole matrix should be zeroes because the unidentifiable part of the imputation model contains a single constant (CSP*M) rather than additional contributions from the other variables in the dataset
predSens <- ini$predictorMatrix
predSens

predSens[predSens == 1] <- 0
predSens

# Set up list with sensitivity parameter values (currently all blank - to be filled in below)
pSens <- rep(list(list("")), ncol(data_mum_narmice))
names(pSens) <- names(data_mum_narmice)
pSens

# Set up vector describing manner of imputation for each variable - As we want to run vary the association between donation and it's missingness, we specify this as 'logregSens', which is a logistic sensitivity analysis - All others are the same as for the standard MI above
meth <- ini$method
meth

meth["mum_donate"] <- "logregSens"
meth

# Choose number of imputations (50) and burn-in period (10)
narmice_numimps <- 50
narmice_numiter <- 10

# To collect the parameters of interest
tipping_outcomeMNAR <- as.data.frame(array(dim = c(dim = length(seq.int(-2, 0, by = 0.25)), 19))) # Number of sensitivity values we're going to try (varying the CSP from -2 to 0, in steps of 0.25), plus the number of parameters we're going to store (here, is 19 [see row below])
colnames(tipping_outcomeMNAR) <- c("csp", "msp", "imor", "sampprev", "est_unadj", "se_unadj", "lci_unadj",
                                   "uci_unadj", "p_unadj", "est_con", "se_con", "lci_con", "uci_con", "p_con",
                                   "est_conMed", "se_conMed", "lci_conMed", "uci_conMed", "p_conMed")
tipping_outcomeMNAR

# Looping over delta/CSP values (i) - A CSP of -3 means that individuals with missing blood donation data have -3 lower log-odds of having given blood, compared to those with data (conditional on all other covariates); the converse applies to positive CSPs (i.e., those with missing data having greater likelihood of having given blood), while a CSP of 0 should approximately correspond to a standard MI model, as there is no adjustment for data potentially being missing-not-at-random.
set.seed(720455)
k <- 0
for (i in seq.int(-2, 0, by = 0.25)) {
  k <- k+1 
  print(paste0("CSP = ", i))
  
  # specify a delta/CSP value for the prediction equation for missing data
  pSens[["mum_donate"]]<-list(c(i))
  
  # NARMICE imputation
  imp_NARMICE <- mice(data_mum_narmice, m = narmice_numimps, method = meth, predictorMatrix = pred,
                      predictorSens=predSens, parmSens=pSens, print = FALSE, maxit = narmice_numiter)
  
  # Derive the MSP for the given CSP value
  msp <- round(summary(pool(with(imp_NARMICE, 
                                 glm(mum_donate ~ M_donate, family = "binomial")))), 3)
  
  # Derive the prevalence of blood donation in sample
  wholesampprev <- round(summary(pool(with(imp_NARMICE, 
                                           glm(mum_donate ~ 1, family = binomial(link = "identity"))))), 3)
  
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
  tipping_outcomeMNAR[k,"csp"] <- i
  tipping_outcomeMNAR[k,"msp"] <- msp["M_donate2", "est"]
  tipping_outcomeMNAR[k,"imor"] <- exp(msp["M_donate2", "est"])
  tipping_outcomeMNAR[k,"sampprev"] <- wholesampprev["(Intercept)", "est"]  
  tipping_outcomeMNAR[k,"est_unadj"] <- res_unadj["mum_attend2", "est"]
  tipping_outcomeMNAR[k,"se_unadj"] <- res_unadj["mum_attend2", "se"]
  tipping_outcomeMNAR[k,"lci_unadj"] <- res_unadj["mum_attend2", "lo 95"]
  tipping_outcomeMNAR[k,"uci_unadj"] <- res_unadj["mum_attend2", "hi 95"]
  tipping_outcomeMNAR[k,"p_unadj"] <- res_unadj["mum_attend2", "Pr(>|t|)"]
  tipping_outcomeMNAR[k,"est_con"] <- res_con["mum_attend2", "est"]
  tipping_outcomeMNAR[k,"se_con"] <- res_con["mum_attend2", "se"]
  tipping_outcomeMNAR[k,"lci_con"] <- res_con["mum_attend2", "lo 95"]
  tipping_outcomeMNAR[k,"uci_con"] <- res_con["mum_attend2", "hi 95"]
  tipping_outcomeMNAR[k,"p_con"] <- res_con["mum_attend2", "Pr(>|t|)"]
  tipping_outcomeMNAR[k,"est_conMed"] <- res_conMed["mum_attend2", "est"]
  tipping_outcomeMNAR[k,"se_conMed"] <- res_conMed["mum_attend2", "se"]
  tipping_outcomeMNAR[k,"lci_conMed"] <- res_conMed["mum_attend2", "lo 95"]
  tipping_outcomeMNAR[k,"uci_conMed"] <- res_conMed["mum_attend2", "hi 95"]
  tipping_outcomeMNAR[k,"p_conMed"] <- res_conMed["mum_attend2", "Pr(>|t|)"]
  
  print(tipping_outcomeMNAR[k,])
}

# Look at the 'tipping' output which contains all the values/estimates
tipping_outcomeMNAR

# Convert the log odds to ORs
tipping_outcomeMNAR$or_unadj <- exp(tipping_outcomeMNAR$est_unadj)
tipping_outcomeMNAR$lci_or_unadj <- exp(tipping_outcomeMNAR$lci_unadj)
tipping_outcomeMNAR$uci_or_unadj <- exp(tipping_outcomeMNAR$uci_unadj)
tipping_outcomeMNAR$or_con <- exp(tipping_outcomeMNAR$est_con)
tipping_outcomeMNAR$lci_or_con <- exp(tipping_outcomeMNAR$lci_con)
tipping_outcomeMNAR$uci_or_con <- exp(tipping_outcomeMNAR$uci_con)
tipping_outcomeMNAR$or_conMed <- exp(tipping_outcomeMNAR$est_conMed)
tipping_outcomeMNAR$lci_or_conMed <- exp(tipping_outcomeMNAR$lci_conMed)
tipping_outcomeMNAR$uci_or_conMed <- exp(tipping_outcomeMNAR$uci_conMed)
tipping_outcomeMNAR

## Save this table
write_csv(tipping_outcomeMNAR, file = "/user/home/ds16565/BloodDonation/Results_mum/Results_mum_NARMICE_outcomeMNAR.csv")

