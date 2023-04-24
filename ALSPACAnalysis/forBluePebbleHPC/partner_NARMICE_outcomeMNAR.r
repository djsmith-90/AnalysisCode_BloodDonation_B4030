### Script for paper 'Exploring causality from observational data: An example assessing whether religiosity promotes cooperation' using ALSPAC data (ALSPAC B-number B4030)
### Script 4d: Analysing partner's data using NARMICE assuming outcome is MNAR
### Created 28/2/2023 by Dan Major-Smith
### R version 4.1.0

## Analysis plan for this paper has been accepted as a Registered Report for the journal Evolutionary Human Sciences, and the plan is available on the OSF: https://osf.io/z5gcm/

## Load in R packages (will assume packages are already installed on BP)
# Note: Have installed the 'NARMICE' package elsewhere so does not conflict with standard 'mice' package. To install the NARMICE package, first load the 'devtools' package and then run "install_github("moreno-betancur/mice")". For more details, see: https://raw.githack.com/moreno-betancur/NARFCS/master/Vignette.html
library(tidyverse)
library(mice, lib.loc = "/user/home/ds16565/NARMICE_package")


### NOTE: The recommendation to include missingness indicators for for variables with missing data is made by Tompsett et al. in their NARMICE paper (https://onlinelibrary.wiley.com/doi/pdf/10.1002/sim.7643). While this approach appeared to work fine for the mother's data, when testing the partner's data the inclusion of these missingness markers resulted in some implausible imputations, especially of the partner_attend exposure (most partner's with missing data on this variable were imputed as 'regular attendees', which is unlikely [as those with missing data would be expected to be *less* likely to attend], and is at odds with with standard MI analysis, which did not produce such results). Given this, will perform the partner's NARMICE analyses without these additional missingness markers (will keep the original code, with some parts commented out, if want to re-create this using the missingness markers).



###############################################################################################
#### Read in the partner's processed data

load("/user/home/ds16565/BloodDonation/Data/data_partner_processed_B4030.RData")

## Also move exposures and outcome to end of dataset, to match synthesised data
data_partner <- data_partner %>%
  relocate(mum_belief:mum_donate, partner_age:partner_ethnicity_byMum, partner_employed, comp_postPreg, 
           partner_smk, partner_belief:partner_donate, .after = mum_employed)
  
  
## If using synthesised data here, read this dataset in instead - NOTE: The sample size is slightly smaller in the synthetic dataset, as unique replicates with identical data in both the observed and synthetic data have been removed for confidentiality reasons. Also, the statistics in the script below refer to the observed data, and will differ for the synthetic data (although ideally not by that much!)
#load("/user/home/ds16565/BloodDonation/Data/syntheticData_partner_B4030.RData")
#data_partner <- data_partner_syn_df


## Unlike for mothers, the time of questionnaire completion (during vs after pregnancy) seems to have no association with outcome or exposures, so will not adjust for this in any models below, as unlikely to be a potential source of confounding - Will just drop this variable
data_partner <- data_partner %>%
  select(-comp_postPreg)


## Make a new dataframe to work with for these NARMICE analyses and add missingness indicators for each variable with missing data
data_partner_narmice <- data_partner

# If wanted to create missingness markers for all variables (but will comment out here)
# data_partner_narmice <- data_partner %>%
#   mutate(M_mum_age = factor(ifelse(is.na(mum_age), "Yes", "No"))) %>%
#   mutate(M_mum_age = factor(M_mum_age, levels = c("No", "Yes"))) %>%
#   mutate(M_mum_ethnicity = factor(ifelse(is.na(mum_ethnicity), "Yes", "No"))) %>%
#   mutate(M_mum_ethnicity = factor(M_mum_ethnicity, levels = c("No", "Yes"))) %>%
#   mutate(M_mum_edu = factor(ifelse(is.na(mum_edu), "Yes", "No"))) %>%
#   mutate(M_mum_edu = factor(M_mum_edu, levels = c("No", "Yes"))) %>%
#   mutate(M_home = factor(ifelse(is.na(home), "Yes", "No"))) %>%
#   mutate(M_home = factor(M_home, levels = c("No", "Yes"))) %>%
#   mutate(M_imd = factor(ifelse(is.na(imd), "Yes", "No"))) %>%
#   mutate(M_imd = factor(M_imd, levels = c("No", "Yes"))) %>%
#   mutate(M_rural = factor(ifelse(is.na(rural), "Yes", "No"))) %>%
#   mutate(M_rural = factor(M_rural, levels = c("No", "Yes"))) %>%
#   mutate(M_mum_marital = factor(ifelse(is.na(mum_marital), "Yes", "No"))) %>%
#   mutate(M_mum_marital = factor(M_mum_marital, levels = c("No", "Yes"))) %>%
#   mutate(M_parity = factor(ifelse(is.na(parity), "Yes", "No"))) %>%
#   mutate(M_parity = factor(M_parity, levels = c("No", "Yes"))) %>%
#   mutate(M_mum_locus = factor(ifelse(is.na(mum_locus), "Yes", "No"))) %>%
#   mutate(M_mum_locus = factor(M_mum_locus, levels = c("No", "Yes"))) %>%
#   mutate(M_mum_finDiffs = factor(ifelse(is.na(mum_finDiffs), "Yes", "No"))) %>%
#   mutate(M_mum_finDiffs = factor(M_mum_finDiffs, levels = c("No", "Yes"))) %>%
#   mutate(M_mum_health = factor(ifelse(is.na(mum_health), "Yes", "No"))) %>%
#   mutate(M_mum_health = factor(M_mum_health, levels = c("No", "Yes"))) %>%
#   mutate(M_car = factor(ifelse(is.na(carAccess), "Yes", "No"))) %>%
#   mutate(M_car = factor(M_car, levels = c("No", "Yes"))) %>%
#   mutate(M_mum_employed = factor(ifelse(is.na(mum_employed), "Yes", "No"))) %>%
#   mutate(M_mum_employed = factor(M_mum_employed, levels = c("No", "Yes"))) %>%
#   mutate(M_employed = factor(ifelse(is.na(partner_employed), "Yes", "No"))) %>%
#   mutate(M_employed = factor(M_employed, levels = c("No", "Yes"))) %>%
#   mutate(M_mum_belief = factor(ifelse(is.na(mum_belief), "Yes", "No"))) %>%
#   mutate(M_mum_belief = factor(M_mum_belief, levels = c("No", "Yes"))) %>%
#   mutate(M_mum_identity = factor(ifelse(is.na(mum_identity), "Yes", "No"))) %>%
#   mutate(M_mum_identity = factor(M_mum_identity, levels = c("No", "Yes"))) %>%
#   mutate(M_mum_attend = factor(ifelse(is.na(mum_attend), "Yes", "No"))) %>%
#   mutate(M_mum_attend = factor(M_mum_attend, levels = c("No", "Yes"))) %>%
#   mutate(M_mum_donate = factor(ifelse(is.na(mum_donate), "Yes", "No"))) %>%
#   mutate(M_mum_donate = factor(M_mum_donate, levels = c("No", "Yes"))) %>%
#   mutate(M_age = factor(ifelse(is.na(partner_age), "Yes", "No"))) %>%
#   mutate(M_age = factor(M_age, levels = c("No", "Yes"))) %>%
#   mutate(M_ethnicity = factor(ifelse(is.na(partner_ethnicity), "Yes", "No"))) %>%
#   mutate(M_ethnicity = factor(M_ethnicity, levels = c("No", "Yes"))) %>%
#   mutate(M_edu = factor(ifelse(is.na(partner_edu), "Yes", "No"))) %>%
#   mutate(M_edu = factor(M_edu, levels = c("No", "Yes"))) %>%
#   mutate(M_marital = factor(ifelse(is.na(partner_marital), "Yes", "No"))) %>%
#   mutate(M_marital = factor(M_marital, levels = c("No", "Yes"))) %>%
#   mutate(M_locus = factor(ifelse(is.na(partner_locus), "Yes", "No"))) %>%
#   mutate(M_locus = factor(M_locus, levels = c("No", "Yes"))) %>%
#   mutate(M_finDiffs = factor(ifelse(is.na(partner_finDiffs), "Yes", "No"))) %>%
#   mutate(M_finDiffs = factor(M_finDiffs, levels = c("No", "Yes"))) %>%
#   mutate(M_health = factor(ifelse(is.na(partner_health), "Yes", "No"))) %>%
#   mutate(M_health = factor(M_health, levels = c("No", "Yes"))) %>%
#   mutate(M_compMonth = factor(ifelse(is.na(partner_compMonth), "Yes", "No"))) %>%
#   mutate(M_compMonth = factor(M_compMonth, levels = c("No", "Yes"))) %>%
#   mutate(M_dep = factor(ifelse(is.na(partner_dep), "Yes", "No"))) %>%
#   mutate(M_dep = factor(M_dep, levels = c("No", "Yes"))) %>%
#   mutate(M_alc = factor(ifelse(is.na(partner_alc), "Yes", "No"))) %>%
#   mutate(M_alc = factor(M_alc, levels = c("No", "Yes"))) %>%
#   mutate(M_occSoc = factor(ifelse(is.na(partner_occSocClass), "Yes", "No"))) %>%
#   mutate(M_occSoc = factor(M_occSoc, levels = c("No", "Yes"))) %>%
#   mutate(M_edu_byMum = factor(ifelse(is.na(partner_edu_byMum), "Yes", "No"))) %>%
#   mutate(M_edu_byMum = factor(M_edu_byMum, levels = c("No", "Yes"))) %>%
#   mutate(M_ethnicity_byMum = factor(ifelse(is.na(partner_ethnicity_byMum), "Yes", "No"))) %>%
#   mutate(M_ethnicity_byMum = factor(M_ethnicity_byMum, levels = c("No", "Yes"))) %>%
#   mutate(M_employed_byMum = factor(ifelse(is.na(partner_employed_byMum), "Yes", "No"))) %>%
#   mutate(M_employed_byMum = factor(M_employed_byMum, levels = c("No", "Yes"))) %>%
#   mutate(M_smk = factor(ifelse(is.na(partner_smk), "Yes", "No"))) %>%
#   mutate(M_smk = factor(M_smk, levels = c("No", "Yes"))) %>%
#   mutate(M_belief = factor(ifelse(is.na(partner_belief), "Yes", "No"))) %>%
#   mutate(M_belief = factor(M_belief, levels = c("No", "Yes"))) %>%
#   mutate(M_identity = factor(ifelse(is.na(partner_identity), "Yes", "No"))) %>%
#   mutate(M_identity = factor(M_identity, levels = c("No", "Yes"))) %>%
#   mutate(M_attend = factor(ifelse(is.na(partner_attend), "Yes", "No"))) %>%
#   mutate(M_attend = factor(M_attend, levels = c("No", "Yes"))) %>%
#   mutate(M_donate = factor(ifelse(is.na(partner_donate), "Yes", "No"))) %>%
#   mutate(M_donate = factor(M_donate, levels = c("No", "Yes")))

glimpse(data_partner_narmice)


###### NARMICE analysis 2: Outcome (blood donation) MNAR

## Set up prediction matrix for the imputation
ini <- mice(data_partner_narmice, maxit = 0, print = TRUE)

# Specify the prediction matrix for the observable data - And edit so that the missingness markers don't predict the variables they represent (now commented out, for reasons above)
pred <- ini$predictorMatrix
pred

# pred["mum_age", "M_mum_age"] <- 0
# pred["mum_ethnicity", "M_mum_ethnicity"] <- 0
# pred["mum_edu", "M_mum_edu"] <- 0
# pred["home", "M_home"] <- 0
# pred["imd", "M_imd"] <- 0
# pred["rural", "M_rural"] <- 0
# pred["mum_marital", "M_mum_marital"] <- 0
# pred["parity", "M_parity"] <- 0
# pred["mum_locus", "M_mum_locus"] <- 0
# pred["mum_finDiffs", "M_mum_finDiffs"] <- 0
# pred["mum_health", "M_mum_health"] <- 0
# pred["carAccess", "M_car"] <- 0
# pred["partner_employed_byMum", "M_employed_byMum"] <- 0
# pred["mum_employed", "M_mum_employed"] <- 0
# pred["partner_employed", "M_employed"] <- 0
# pred["mum_belief", "M_mum_belief"] <- 0
# pred["mum_identity", "M_mum_identity"] <- 0
# pred["mum_attend", "M_mum_attend"] <- 0
# pred["mum_donate", "M_mum_donate"] <- 0
# pred["partner_age", "M_age"] <- 0
# pred["partner_ethnicity", "M_ethnicity"] <- 0
# pred["partner_edu", "M_edu"] <- 0
# pred["partner_marital", "M_marital"] <- 0
# pred["partner_locus", "M_locus"] <- 0
# pred["partner_finDiffs", "M_finDiffs"] <- 0
# pred["partner_health", "M_health"] <- 0
# pred["partner_compMonth", "M_compMonth"] <- 0
# pred["partner_dep", "M_dep"] <- 0
# pred["partner_alc", "M_alc"] <- 0
# pred["partner_occSocClass", "M_occSoc"] <- 0
# pred["partner_edu_byMum", "M_edu_byMum"] <- 0
# pred["partner_ethnicity_byMum", "M_ethnicity_byMum"] <- 0
# pred["partner_smk", "M_smk"] <- 0
# pred["partner_belief", "M_belief"] <- 0
# pred["partner_identity", "M_identity"] <- 0
# pred["partner_attend", "M_attend"] <- 0
# pred["partner_donate", "M_donate"] <- 0
# pred

# Set-up the prediction matrix for the unidentifiable part of the model (i.e., the missing-not-at-random element)
# In this case the whole matrix should be zeroes because the unidentifiable part of the imputation model contains a single constant (CSP*M) rather than additional contributions from the other variables in the dataset
predSens <- ini$predictorMatrix
predSens

predSens[predSens == 1] <- 0
predSens

# Set up list with sensitivity parameter values (currently all blank - to be filled in below)
pSens <- rep(list(list("")), ncol(data_partner_narmice))
names(pSens) <- names(data_partner_narmice)
pSens

# Set up vector describing manner of imputation for each variable - As we want to run vary the association between donation and it's missingness, we specify this as 'logregSens', which is a logistic sensitivity analysis - All others are the same as for the standard MI above
meth <- ini$method
meth

meth["partner_donate"] <- "logregSens"
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
set.seed(854174)
k <- 0
for (i in seq.int(-2, 0, by = 0.25)) {
  k <- k+1 
  print(paste0("CSP = ", i))
  
  # specify a delta/CSP value for the prediction equation for missing data
  pSens[["partner_donate"]]<-list(c(i))
  
  # NARMICE imputation
  imp_NARMICE <- mice(data_partner_narmice, m = narmice_numimps, method = meth, predictorMatrix = pred,
                      predictorSens=predSens, parmSens=pSens, print = FALSE, maxit = narmice_numiter)
  
  # Derive the MSP for the given CSP value (convert to wide format for this, create a missingness marker, then estimate MSP for each imputation)
  imp_wide <- mice::complete(imp_NARMICE, "broad", inc=TRUE)
  imp_wide$M_donate <- ifelse(is.na(imp_wide$partner_donate.0), 1, 0)
  msp <- 1:narmice_numimps
  for (z in 1:narmice_numimps) {
    tempvar <- paste0("partner_donate.", z)
    x <- glm(formula = get(tempvar) ~ M_donate, family = "binomial", data = imp_wide)
    msp[z] <- x$coefficients[2]
  }
  
  ## This is the other way of estimating the MSP, if missingness indicators are included in the imputation
  #msp <- round(summary(pool(with(imp_NARMICE, 
  #                               glm(partner_donate ~ M_donate, family = "binomial")))), 3)
  
  # Derive the prevalence of blood donation in sample
  wholesampprev <- round(summary(pool(with(imp_NARMICE, 
                                           glm(partner_donate ~ 1, family = binomial(link = "identity"))))), 3)
  
  ## Run the logistic models in in these NARMICE imputed datasets
  # Unadjusted
  res_unadj <- round(summary(pool(with(imp_NARMICE, 
                                    glm(partner_donate ~ partner_attend, family = "binomial")))), 3)
  
  # Confounder-only adjusted
  res_con <- round(summary(pool(with(imp_NARMICE, 
                                       glm(partner_donate ~ partner_attend + partner_age + partner_ethnicity + 
                                             partner_edu + home + imd + rural + partner_finDiffs + 
                                             partner_employed + partner_compMonth, 
                                           family = "binomial")))), 3)
  
  # Confounder and/or mediator adjusted
  res_conMed <- round(summary(pool(with(imp_NARMICE, 
                                       glm(partner_donate ~ partner_attend + partner_age + partner_ethnicity + 
                                             partner_edu + home + imd + rural + partner_finDiffs + 
                                             partner_employed + partner_compMonth + 
                                             partner_marital + parity + partner_locus + partner_health, 
                                           family = "binomial")))), 3)
  
  # Store these estimates in the 'tipping' dataframe (with other MSP esimation method commented out)
  tipping_outcomeMNAR[k,"csp"] <- i
  #tipping_outcomeMNAR[k,"msp"] <- msp["M_donate2", "est"]
  tipping_outcomeMNAR[k,"msp"] <- mean(msp)
  #tipping_outcomeMNAR[k,"imor"] <- exp(msp["M_donate2", "est"])
  tipping_outcomeMNAR[k,"imor"] <- exp(mean(msp))
  tipping_outcomeMNAR[k,"sampprev"] <- wholesampprev["(Intercept)", "est"]  
  tipping_outcomeMNAR[k,"est_unadj"] <- res_unadj["partner_attend2", "est"]
  tipping_outcomeMNAR[k,"se_unadj"] <- res_unadj["partner_attend2", "se"]
  tipping_outcomeMNAR[k,"lci_unadj"] <- res_unadj["partner_attend2", "lo 95"]
  tipping_outcomeMNAR[k,"uci_unadj"] <- res_unadj["partner_attend2", "hi 95"]
  tipping_outcomeMNAR[k,"p_unadj"] <- res_unadj["partner_attend2", "Pr(>|t|)"]
  tipping_outcomeMNAR[k,"est_con"] <- res_con["partner_attend2", "est"]
  tipping_outcomeMNAR[k,"se_con"] <- res_con["partner_attend2", "se"]
  tipping_outcomeMNAR[k,"lci_con"] <- res_con["partner_attend2", "lo 95"]
  tipping_outcomeMNAR[k,"uci_con"] <- res_con["partner_attend2", "hi 95"]
  tipping_outcomeMNAR[k,"p_con"] <- res_con["partner_attend2", "Pr(>|t|)"]
  tipping_outcomeMNAR[k,"est_conMed"] <- res_conMed["partner_attend2", "est"]
  tipping_outcomeMNAR[k,"se_conMed"] <- res_conMed["partner_attend2", "se"]
  tipping_outcomeMNAR[k,"lci_conMed"] <- res_conMed["partner_attend2", "lo 95"]
  tipping_outcomeMNAR[k,"uci_conMed"] <- res_conMed["partner_attend2", "hi 95"]
  tipping_outcomeMNAR[k,"p_conMed"] <- res_conMed["partner_attend2", "Pr(>|t|)"]
  
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
write_csv(tipping_outcomeMNAR, file = "/user/home/ds16565/BloodDonation/Results_partner/Results_partner_NARMICE_outcomeMNAR.csv")
