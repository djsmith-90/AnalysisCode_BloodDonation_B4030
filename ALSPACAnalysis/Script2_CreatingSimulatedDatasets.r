### Script for paper 'Exploring causality from observational data: An example assessing whether religiosity promotes cooperation' using ALSPAC data (ALSPAC B-number B4030)
### Script 2: Synthesising simulated data
### Created 16/2/2023 by Dan Major-Smith
### R version 4.0.4

## Analysis plan for this paper has been accepted as a Registered Report for the journal Evolutionary Human Sciences, and the plan is available on the OSF: https://osf.io/z5gcm/


###########################################################################################
#### Clear workspace, install/load packages, and set working directory
rm(list = ls())

setwd("X:\\Studies\\RSBB Team\\Dan\\B4030 - RSBB and cooperation")

#install.packages("tidyverse")
library(tidyverse)

#install.packages("synthpop")
library(synthpop)

#install.packages("haven")
library(haven)


###########################################################################################
#### Read in the mother's processed data, and create synthetic/anonymous data using the 'synthpop' package (https://www.synthpop.org.uk/get-started.html). As ALSPAC data cannot be shared, creating synthetic simulated data which maintains the relations between variables, but anonymises the data, is a neat way of sharing data and improving reproducibility, without breaking data-sharing confidentiality rules.

load("data_mum_processed_B4030.RData")

# Will move exposures and outcome to end of dataset, so they get synthesised last (need do to this to maintain relations between the variables)
data_mum <- data_mum %>%
  relocate(mum_belief, mum_identity, mum_attend, mum_donate, .after = mum_smkPreg)

# Get information about variables in the dataset
codebook.syn(data_mum)$tab

# Create a synthetic dataset using default options (which are non-parametric/CART [classification and regression trees])
data_mum_syn <- syn(data_mum, seed = 446473)

# Use the 'sdc' command (statistical disclosure control) to identify and remove any cases that are unique in both synthetic and observed data (i.e., cases which may be disclosive) - Here, 47 observations have been dropped (0.35% of data)
replicated.uniques(data_mum_syn, data_mum)
data_mum_syn <- sdc(data_mum_syn, data_mum, rm.replicated.uniques = TRUE)

# Explore this synthetic dataset
data_mum_syn
summary(data_mum_syn)

# Compare between actual and synthetic datasets - This provides tables and plots comparing distribution of variables between the two datasets (correspondence is fairly good). Save this as a PDF
compare(data_mum_syn, data_mum, stat = "counts")

pdf("./Results/ComparingObservedVsSyntheticData/ComparingDescStats_mothers.pdf", height = 6, width = 10)
compare(data_mum_syn, data_mum, stat = "counts")
dev.off()


# Extract the actual dataset (rather than it being stored within a list)
data_mum_syn_df <- data_mum_syn$syn
head(data_mum_syn_df)
glimpse(data_mum_syn_df)
summary(data_mum_syn_df)


## Simple analysis of blood donation as outcome and religious belief and exposure to show that get similar results in both datasets (i.e., that the structures of the dataset are preserved)
model.real <- glm(mum_donate ~ mum_belief, family = "binomial", data = data_mum)
summary(model.real)

model.syn <- glm.synds(mum_donate ~ mum_belief, family = "binomial", data = data_mum_syn)
summary(model.syn)

# Get comparable pattern of results (and store as PDF)
compare(model.syn, data_mum)

pdf("./Results/ComparingObservedVsSyntheticData/ComparingUnadjustedModel_mothers.pdf", height = 8, width = 12)
compare(model.syn, data_mum)
dev.off()


## Test this with a more complex model, with additional covariates
model.real2 <- glm(mum_donate ~ mum_belief + mum_age + mum_ethnicity + mum_edu + imd, 
                   family = "binomial", data = data_mum)
summary(model.real2)

model.syn2 <- glm.synds(mum_donate ~ mum_belief + mum_age + mum_ethnicity + mum_edu + imd, 
                        family = "binomial", data = data_mum_syn)
summary(model.syn2)

# Again, get comparable pattern of results, this time for all of the additional coefficients in the model as well (again, store as PDF)
compare(model.syn2, data_mum)

pdf("./Results/ComparingObservedVsSyntheticData/ComparingAdjustedModel_mothers.pdf", height = 8, width = 12)
compare(model.syn2, data_mum)
dev.off()


### Store the synthetic dataset for others to use
save(data_mum_syn_df, file = "./AnalysisCode_BloodDonation_B4030/ALSPACAnalysis/syntheticData_mum_B4030.RData")
write_csv(data_mum_syn_df, file = "./AnalysisCode_BloodDonation_B4030/ALSPACAnalysis/syntheticData_mum_B4030.csv")
write_dta(data_mum_syn_df, "./AnalysisCode_BloodDonation_B4030/ALSPACAnalysis/syntheticData_mum_B4030.dta")



####################################################################################################################
###### Repeat the above, creating a synthetic dataset for the partners data

load("data_partner_processed_B4030.RData")

# Will move partner variables to after the mother variables, so they get synthesised last
data_partner <- data_partner %>%
  relocate(mum_belief:mum_donate, partner_age:partner_ethnicity_byMum, partner_employed, comp_postPreg, 
           partner_smk, partner_belief:partner_donate, .after = mum_employed)

# Get information about variables in the dataset
codebook.syn(data_partner)$tab

# Create a synthetic dataset using default options
data_partner_syn <- syn(data_partner, seed = 736578)

# Use the 'sdc' command (statistical disclosure control) to identify and remove any cases that are unique in both synthetic and observed data (i.e., cases which may be disclosive) - Here, 45 observations have been dropped (0.34% of data)
replicated.uniques(data_partner_syn, data_partner)
data_partner_syn <- sdc(data_partner_syn, data_partner, rm.replicated.uniques = TRUE)

# Explore this synthetic dataset
data_partner_syn
summary(data_partner_syn)

# Compare between actual and synthetic datasets - This provides tables and plots comparing distribution of variables between the two datasets (correspondence is fairly good). Save this as a PDF
compare(data_partner_syn, data_partner, stat = "counts")

pdf("./Results/ComparingObservedVsSyntheticData/ComparingDescStats_partners.pdf", height = 6, width = 10)
compare(data_partner_syn, data_partner, stat = "counts")
dev.off()


# Extract the actual dataset (rather than it being stored within a list)
data_partner_syn_df <- data_partner_syn$syn
head(data_partner_syn_df)
glimpse(data_partner_syn_df)
summary(data_partner_syn_df)


## Simple analysis of blood donation as outcome and religious belief and exposure to show that get similar results in both datasets (i.e., that the structures of the dataset are preserved)
model.real_partner <- glm(partner_donate ~ partner_belief, family = "binomial", data = data_partner)
summary(model.real_partner)

model.syn_partner <- glm.synds(partner_donate ~ partner_belief, family = "binomial", data = data_partner_syn)
summary(model.syn_partner)

# Get comparable pattern of results (and store as PDF)
compare(model.syn_partner, data_partner)

pdf("./Results/ComparingObservedVsSyntheticData/ComparingUnadjustedModel_partners.pdf", height = 8, width = 12)
compare(model.syn_partner, data_partner)
dev.off()


## Test this with a more complex model, with additional covariates
model.real2_partner <- glm(partner_donate ~ partner_belief + partner_age + partner_ethnicity + partner_edu + imd, 
                   family = "binomial", data = data_partner)
summary(model.real2_partner)

model.syn2_partner <- glm.synds(partner_donate ~ partner_belief + partner_age + partner_ethnicity + partner_edu + imd, 
                        family = "binomial", data = data_partner_syn)
summary(model.syn2_partner)

# Again, get comparable pattern of results, this time for all of the additional coefficients in the model as well (again, store as PDF)
compare(model.syn2_partner, data_partner)

pdf("./Results/ComparingObservedVsSyntheticData/ComparingAdjustedModel_partners.pdf", height = 8, width = 12)
compare(model.syn2_partner, data_partner)
dev.off()


### Store the synthetic dataset for others to use
save(data_partner_syn_df, file = "./AnalysisCode_BloodDonation_B4030/ALSPACAnalysis/syntheticData_partner_B4030.RData")
write_csv(data_partner_syn_df, file = "./AnalysisCode_BloodDonation_B4030/ALSPACAnalysis/syntheticData_partner_B4030.csv")
write_dta(data_partner_syn_df, "./AnalysisCode_BloodDonation_B4030/ALSPACAnalysis/syntheticData_partner_B4030.dta")
