### Script for paper 'Exploring causality from observational data: An example assessing whether religiosity promotes cooperation' using ALSPAC data (ALSPAC B-number B4030)
### Script 3: Analysing mother's data
### Created 22/2/2023 by Dan Major-Smith
### R version 4.0.4

## Analysis plan for this paper has been accepted as a Registered Report for the journal Evolutionary Human Sciences, and the plan is available on the OSF: https://osf.io/z5gcm/


###########################################################################################
#### Clear workspace, install/load packages, and set working directory
rm(list = ls())

setwd("X:\\Studies\\RSBB Team\\Dan\\B4030 - RSBB and cooperation")

#install.packages("tidyverse")
library(tidyverse)

#install.packages("EValue")
library(EValue)

#install.packages("mice")
detach(package:mice, unload = TRUE)
library(mice)

#install.packages("ggplot2")
library(ggplot2)

#install.packages("marginaleffects")
library(marginaleffects)


## Note that the 'NARMICE' package for not-at-random multiple imputation is based on the 'mice' package, so has to be installed in a different location to avoid over-writing the original 'mice' package (as we use the original 'mice' package first, we will load this NARMICE package later on).
#library(devtools)
#install_github("moreno-betancur/mice",
#               lib = "C:/Temp/mice_test")


###########################################################################################
#### Read in the mother's processed data

## Also move exposures and outcome to end of dataset, to match synthesised data
load("data_mum_processed_B4030.RData")

data_mum <- data_mum %>%
  relocate(mum_belief, mum_identity, mum_attend, mum_donate, .after = mum_smkPreg)

## If using synthesised data here, read this dataset in instead - NOTE: The sample size is slightly smaller in the synthetic dataset, as unique replicates with identical data in both the observed and synthetic data have been removed for confidentiality reasons. Also, the statistics in the script below refer to the observed data, and will differ for the synthetic data
#load("./AnalysisCode_BloodDonation_B4030/ALSPACAnalysis/syntheticData_mum_B4030.RData")
#data_mum <- data_mum_syn_df
#data_mum <- data_mum %>%
#  select(-FALSE_DATA) ## Drop the 'FALSE_DATA' column


### Some descriptive stats of exposures and outcomes

# Religious belief exposure
table(data_mum$mum_belief, useNA = "ifany")
round((table(data_mum$mum_belief) / sum(table(data_mum$mum_belief))) * 100, 2)
sum(table(data_mum$mum_belief))
round((sum(is.na(data_mum$mum_belief)) / nrow(data_mum)) * 100, 2)

# Religious identity exposure
table(data_mum$mum_identity, useNA = "ifany")
round((table(data_mum$mum_identity) / sum(table(data_mum$mum_identity))) * 100, 2)
sum(table(data_mum$mum_identity))
round((sum(is.na(data_mum$mum_identity)) / nrow(data_mum)) * 100, 2)

# Religious attendance exposure
table(data_mum$mum_attend, useNA = "ifany")
round((table(data_mum$mum_attend) / sum(table(data_mum$mum_attend))) * 100, 2)
sum(table(data_mum$mum_attend))
round((sum(is.na(data_mum$mum_attend)) / nrow(data_mum)) * 100, 2)

# Blood donation outcome
table(data_mum$mum_donate, useNA = "ifany")
round((table(data_mum$mum_donate) / sum(table(data_mum$mum_donate))) * 100, 2)
sum(table(data_mum$mum_donate))
round((sum(is.na(data_mum$mum_donate)) / nrow(data_mum)) * 100, 2)


## Two-way tables of each exposure by outcome

# Religious belief exposure
addmargins(table(mum_belief = data_mum$mum_belief, mum_donate = data_mum$mum_donate))
round(prop.table(table(mum_belief = data_mum$mum_belief, mum_donate = data_mum$mum_donate), margin = 1) * 100, 2)

# Religious identity exposure
addmargins(table(mum_identity = data_mum$mum_identity, mum_donate = data_mum$mum_donate))
round(prop.table(table(mum_identity = data_mum$mum_identity, mum_donate = data_mum$mum_donate), margin = 1) * 100, 2)

# Religious attendance exposure
addmargins(table(mum_attend = data_mum$mum_attend, mum_donate = data_mum$mum_donate))
round(prop.table(table(mum_attend = data_mum$mum_attend, mum_donate = data_mum$mum_donate), margin = 1) * 100, 2)


### Descriptive statistics of each covariate and auxiliary variable

# Age at birth
summary(data_mum$mum_age); sd(data_mum$mum_age, na.rm = TRUE)
sum(is.na(data_mum$mum_age)); round((sum(is.na(data_mum$mum_age)) / nrow(data_mum)) * 100, 2)

# Ethnicity
table(data_mum$mum_ethnicity, useNA = "ifany")
round((table(data_mum$mum_ethnicity) / sum(table(data_mum$mum_ethnicity))) * 100, 2)
sum(table(data_mum$mum_ethnicity))
round((sum(is.na(data_mum$mum_ethnicity)) / nrow(data_mum)) * 100, 2)

# Education
table(data_mum$mum_edu, useNA = "ifany")
round((table(data_mum$mum_edu) / sum(table(data_mum$mum_edu))) * 100, 2)
sum(table(data_mum$mum_edu))
round((sum(is.na(data_mum$mum_edu)) / nrow(data_mum)) * 100, 2)

# Home ownership status
table(data_mum$home, useNA = "ifany")
round((table(data_mum$home) / sum(table(data_mum$home))) * 100, 2)
sum(table(data_mum$home))
round((sum(is.na(data_mum$home)) / nrow(data_mum)) * 100, 2)

# IMD
table(data_mum$imd, useNA = "ifany")
round((table(data_mum$imd) / sum(table(data_mum$imd))) * 100, 2)
sum(table(data_mum$imd))
round((sum(is.na(data_mum$imd)) / nrow(data_mum)) * 100, 2)

# Urban/rural status
table(data_mum$rural, useNA = "ifany")
round((table(data_mum$rural) / sum(table(data_mum$rural))) * 100, 2)
sum(table(data_mum$rural))
round((sum(is.na(data_mum$rural)) / nrow(data_mum)) * 100, 2)

# Employment status
table(data_mum$mum_employed, useNA = "ifany")
round((table(data_mum$mum_employed) / sum(table(data_mum$mum_employed))) * 100, 2)
sum(table(data_mum$mum_employed))
round((sum(is.na(data_mum$mum_employed)) / nrow(data_mum)) * 100, 2)

# Marital status
table(data_mum$mum_marital, useNA = "ifany")
round((table(data_mum$mum_marital) / sum(table(data_mum$mum_marital))) * 100, 2)
sum(table(data_mum$mum_marital))
round((sum(is.na(data_mum$mum_marital)) / nrow(data_mum)) * 100, 2)

# Maternal parity
table(data_mum$parity, useNA = "ifany")
round((table(data_mum$parity) / sum(table(data_mum$parity))) * 100, 2)
sum(table(data_mum$parity))
round((sum(is.na(data_mum$parity)) / nrow(data_mum)) * 100, 2)

# Locus of control
summary(data_mum$mum_locus); sd(data_mum$mum_locus, na.rm = TRUE)
sum(is.na(data_mum$mum_locus)); round((sum(is.na(data_mum$mum_locus)) / nrow(data_mum)) * 100, 2)

# Financial difficulties
table(data_mum$mum_finDiffs, useNA = "ifany")
round((table(data_mum$mum_finDiffs) / sum(table(data_mum$mum_finDiffs))) * 100, 2)
sum(table(data_mum$mum_finDiffs))
round((sum(is.na(data_mum$mum_finDiffs)) / nrow(data_mum)) * 100, 2)

# Health status
table(data_mum$mum_health, useNA = "ifany")
round((table(data_mum$mum_health) / sum(table(data_mum$mum_health))) * 100, 2)
sum(table(data_mum$mum_health))
round((sum(is.na(data_mum$mum_health)) / nrow(data_mum)) * 100, 2)

# Month of questionnaire completion
table(data_mum$mum_compMonth, useNA = "ifany")
round((table(data_mum$mum_compMonth) / sum(table(data_mum$mum_compMonth))) * 100, 2)
sum(table(data_mum$mum_compMonth))
round((sum(is.na(data_mum$mum_compMonth)) / nrow(data_mum)) * 100, 2)

# Questionnaire completed after pregnancy
table(data_mum$comp_postPreg, useNA = "ifany")
round((table(data_mum$comp_postPreg) / sum(table(data_mum$comp_postPreg))) * 100, 2)
sum(table(data_mum$comp_postPreg))
round((sum(is.na(data_mum$comp_postPreg)) / nrow(data_mum)) * 100, 2)

# Depression (Edinburgh post-natal depression score)
summary(data_mum$mum_dep); sd(data_mum$mum_dep, na.rm = TRUE)
sum(is.na(data_mum$mum_dep)); round((sum(is.na(data_mum$mum_dep)) / nrow(data_mum)) * 100, 2)

# Ever smoked
table(data_mum$mum_everSmk, useNA = "ifany")
round((table(data_mum$mum_everSmk) / sum(table(data_mum$mum_everSmk))) * 100, 2)
sum(table(data_mum$mum_everSmk))
round((sum(is.na(data_mum$mum_everSmk)) / nrow(data_mum)) * 100, 2)

# Smoked during pregnancy
table(data_mum$mum_smkPreg, useNA = "ifany")
round((table(data_mum$mum_smkPreg) / sum(table(data_mum$mum_smkPreg))) * 100, 2)
sum(table(data_mum$mum_smkPreg))
round((sum(is.na(data_mum$mum_smkPreg)) / nrow(data_mum)) * 100, 2)

# Alcohol intake pre-pregnancy
table(data_mum$mum_alcPrePreg, useNA = "ifany")
round((table(data_mum$mum_alcPrePreg) / sum(table(data_mum$mum_alcPrePreg))) * 100, 2)
sum(table(data_mum$mum_alcPrePreg))
round((sum(is.na(data_mum$mum_alcPrePreg)) / nrow(data_mum)) * 100, 2)

# Alcohol intake during pregnancy
table(data_mum$mum_alcPreg, useNA = "ifany")
round((table(data_mum$mum_alcPreg) / sum(table(data_mum$mum_alcPreg))) * 100, 2)
sum(table(data_mum$mum_alcPreg))
round((sum(is.na(data_mum$mum_alcPreg)) / nrow(data_mum)) * 100, 2)

# Occupational social class
table(data_mum$mum_occSocClass, useNA = "ifany")
round((table(data_mum$mum_occSocClass) / sum(table(data_mum$mum_occSocClass))) * 100, 2)
sum(table(data_mum$mum_occSocClass))
round((sum(is.na(data_mum$mum_occSocClass)) / nrow(data_mum)) * 100, 2)

# Household access to car
table(data_mum$carAccess, useNA = "ifany")
round((table(data_mum$carAccess) / sum(table(data_mum$carAccess))) * 100, 2)
sum(table(data_mum$carAccess))
round((sum(is.na(data_mum$carAccess)) / nrow(data_mum)) * 100, 2)



### Quick check whether religious variables differ by whether the questionnaire containing religion questions was completed during or shortly after pregnancy
table(data_mum$comp_postPreg, useNA = "ifany")

# Religious belief - No difference
addmargins(table(mum_belief = data_mum$mum_belief, comp_postPreg = data_mum$comp_postPreg))
round(prop.table(table(mum_belief = data_mum$mum_belief, comp_postPreg = data_mum$comp_postPreg), 
                 margin = 1) * 100, 2)
chisq.test(table(mum_belief = data_mum$mum_belief, comp_postPreg = data_mum$comp_postPreg), correct = FALSE)

# Religious identity - No difference
addmargins(table(mum_identity = data_mum$mum_identity, comp_postPreg = data_mum$comp_postPreg))
round(prop.table(table(mum_identity = data_mum$mum_identity, comp_postPreg = data_mum$comp_postPreg), 
                 margin = 1) * 100, 2)
chisq.test(table(mum_identity = data_mum$mum_identity, comp_postPreg = data_mum$comp_postPreg), correct = FALSE)

# Religious attendance - Is a slight difference here, with those completing the questionnaire after pregnancy more likely to attend regularly
addmargins(table(mum_attend = data_mum$mum_attend, comp_postPreg = data_mum$comp_postPreg))
round(prop.table(table(mum_attend = data_mum$mum_attend, comp_postPreg = data_mum$comp_postPreg), 
                 margin = 1) * 100, 2)
chisq.test(table(mum_attend = data_mum$mum_attend, comp_postPreg = data_mum$comp_postPreg), correct = FALSE)

# Association seems to be quite stable when adjusting for various potential sociodemographic confounders as well
summary(glm(mum_attend ~ comp_postPreg + mum_age + mum_ethnicity + mum_edu + imd + rural + home +
              mum_employed + mum_finDiffs + mum_compMonth, family = "binomial", data = data_mum))
exp(coef(summary(glm(mum_attend ~ comp_postPreg + mum_age + mum_ethnicity + mum_edu + imd + rural + home +
                       mum_employed + mum_finDiffs + mum_compMonth, 
                     family = "binomial", data = data_mum)))["comp_postPregYes", "Estimate"])
exp(confint(glm(mum_attend ~ comp_postPreg + mum_age + mum_ethnicity + mum_edu + imd + rural + home +
                  mum_employed + mum_finDiffs + mum_compMonth, 
                family = "binomial", data = data_mum))["comp_postPregYes", ])


# However, this is unlikely to result in bias unless the outcome responses (blood donation) are also associated with whether the questionnaire was completed during or after pregnancy
addmargins(table(mum_donate = data_mum$mum_donate, comp_postPreg = data_mum$comp_postPreg))
round(prop.table(table(mum_donate = data_mum$mum_donate, comp_postPreg = data_mum$comp_postPreg), 
                 margin = 1) * 100, 2)
chisq.test(table(mum_donate = data_mum$mum_donate, comp_postPreg = data_mum$comp_postPreg), correct = FALSE)

# There is weak evidence that those who completed the questionnaire after pregnancy were slightly less likely to report having donated blood (which was also asked in the same questionnaire as the religion questions).
summary(glm(mum_donate ~ comp_postPreg + mum_age + mum_ethnicity + mum_edu + imd + rural + home +
              mum_employed + mum_finDiffs + mum_compMonth, family = "binomial", data = data_mum))
exp(coef(summary(glm(mum_donate ~ comp_postPreg + mum_age + mum_ethnicity + mum_edu + imd + rural + home +
                       mum_employed + mum_finDiffs + mum_compMonth, 
                     family = "binomial", data = data_mum)))["comp_postPregYes", "Estimate"])
exp(confint(glm(mum_donate ~ comp_postPreg + mum_age + mum_ethnicity + mum_edu + imd + rural + home +
                  mum_employed + mum_finDiffs + mum_compMonth, 
                family = "binomial", data = data_mum))["comp_postPregYes", ])

## As time of questionnaire completion (during vs after pregnancy) seems to be associated with outcome and religious attendance (although not religious belief or identity), will adjust for this in all models as well as could be a potential source of confounding



###########################################################################################
#### Analysis 1) Complete case analyses, with different confounding assumptions

### First, make a complete-case marker to say whether data are observed for all variables in these models (so that all models use the same data)
data_mum <- data_mum %>%
  mutate(cca_marker = ifelse(complete.cases(mum_belief, mum_identity, mum_attend, mum_donate, mum_age, mum_ethnicity, 
                                            mum_edu, home, imd, rural, mum_marital, parity, mum_locus, mum_finDiffs,
                                            mum_health, mum_employed, mum_compMonth, comp_postPreg), 1, 0))
table(data_mum$cca_marker, useNA = "ifany")


## Make a nice a nice data frame to store these results in
mum_results <- as.data.frame(cbind(model = rep(c("CCA (n = 7,341)", "MI (n = 13,477)"), each = 9),
                                   exposure = rep(c("Belief", "Belief", "Belief",
                                                    "Identity", "Identity", "Identity",
                                                    "Attend", "Attend", "Attend"), 2),
                                   adjustment = rep(c("Unadjusted", "Confounders only", 
                                                      "Confounders and/or mediators"), 6),
                                   OR = rep(NA, 18),
                                   lower_CI = rep(NA, 18),
                                   upper_CI = rep(NA, 18),
                                   p = rep(NA, 18)
                                   ))
mum_results

## Also make a data frame to store the differences in probabilities of donating blood
mum_results_prob <- as.data.frame(cbind(model = rep(c("CCA (n = 7,341)", "MI (n = 13,477)"), each = 9),
                                   exposure = rep(c("Belief", "Belief", "Belief",
                                                    "Identity", "Identity", "Identity",
                                                    "Attend", "Attend", "Attend"), 2),
                                   adjustment = rep(c("Unadjusted", "Confounders only", 
                                                      "Confounders and/or mediators"), 6),
                                   diff = rep(NA, 18),
                                   lower_CI = rep(NA, 18),
                                   upper_CI = rep(NA, 18)
                                   ))
mum_results_prob


### First exposure: Religious belief

## Unadjusted model
mod.belief_unadj <- glm(mum_donate ~ mum_belief, family = "binomial", data = data_mum, subset = cca_marker == 1)
summary(mod.belief_unadj)

# Store results in table
mum_results$OR[mum_results$model == "CCA (n = 7,341)" & mum_results$exposure == "Belief" & 
                 mum_results$adjustment == "Unadjusted"] <- 
  round(exp(coef(summary(mod.belief_unadj))["mum_beliefYes", "Estimate"]), 3)

mum_results$lower_CI[mum_results$model == "CCA (n = 7,341)" & mum_results$exposure == "Belief" & 
                 mum_results$adjustment == "Unadjusted"] <- 
  round(exp(confint(mod.belief_unadj)["mum_beliefYes", "2.5 %"]), 3)

mum_results$upper_CI[mum_results$model == "CCA (n = 7,341)" & mum_results$exposure == "Belief" & 
                       mum_results$adjustment == "Unadjusted"] <- 
  round(exp(confint(mod.belief_unadj)["mum_beliefYes", "97.5 %"]), 3)

mum_results$p[mum_results$model == "CCA (n = 7,341)" & mum_results$exposure == "Belief" & 
                 mum_results$adjustment == "Unadjusted"] <- 
  round(coef(summary(mod.belief_unadj))["mum_beliefYes", "Pr(>|z|)"], 4)

# Probability of difference of donating blood
avg_comparisons(mod.belief_unadj)

mum_results_prob$diff[mum_results_prob$model == "CCA (n = 7,341)" & mum_results_prob$exposure == "Belief" & 
                        mum_results_prob$adjustment == "Unadjusted"] <- 
  round(avg_comparisons(mod.belief_unadj)$estimate * 100, 2)

mum_results_prob$lower_CI[mum_results_prob$model == "CCA (n = 7,341)" & mum_results_prob$exposure == "Belief" & 
                            mum_results_prob$adjustment == "Unadjusted"] <- 
  round(avg_comparisons(mod.belief_unadj)$conf.low * 100, 2)

mum_results_prob$upper_CI[mum_results_prob$model == "CCA (n = 7,341)" & mum_results_prob$exposure == "Belief" & 
                            mum_results_prob$adjustment == "Unadjusted"] <- 
  round(avg_comparisons(mod.belief_unadj)$conf.high * 100, 2)


## Adjusted for assumed confounders (age, ethnicity, socio-economic position, urban/rural status, recent financial difficulties, employment status, month of questionnaire completion, and whether questionnaire completed during pregnancy or not)
mod.belief_adjCon <- glm(mum_donate ~ mum_belief + mum_age + mum_ethnicity + mum_edu + home + imd + rural + 
                           mum_finDiffs + mum_employed + mum_compMonth + comp_postPreg, 
                         family = "binomial", data = data_mum, subset = cca_marker == 1)
summary(mod.belief_adjCon)

# Store results in table
mum_results$OR[mum_results$model == "CCA (n = 7,341)" & mum_results$exposure == "Belief" & 
                 mum_results$adjustment == "Confounders only"] <- 
  round(exp(coef(summary(mod.belief_adjCon))["mum_beliefYes", "Estimate"]), 3)

mum_results$lower_CI[mum_results$model == "CCA (n = 7,341)" & mum_results$exposure == "Belief" & 
                       mum_results$adjustment == "Confounders only"] <- 
  round(exp(confint(mod.belief_adjCon)["mum_beliefYes", "2.5 %"]), 3)

mum_results$upper_CI[mum_results$model == "CCA (n = 7,341)" & mum_results$exposure == "Belief" & 
                       mum_results$adjustment == "Confounders only"] <- 
  round(exp(confint(mod.belief_adjCon)["mum_beliefYes", "97.5 %"]), 3)

mum_results$p[mum_results$model == "CCA (n = 7,341)" & mum_results$exposure == "Belief" & 
                mum_results$adjustment == "Confounders only"] <- 
  round(coef(summary(mod.belief_adjCon))["mum_beliefYes", "Pr(>|z|)"], 4)

# Probability of difference of donating blood
avg_comparisons(mod.belief_adjCon, variable = "mum_belief")

mum_results_prob$diff[mum_results_prob$model == "CCA (n = 7,341)" & mum_results_prob$exposure == "Belief" & 
                        mum_results_prob$adjustment == "Confounders only"] <- 
  round(avg_comparisons(mod.belief_adjCon, variable = "mum_belief")$estimate * 100, 2)

mum_results_prob$lower_CI[mum_results_prob$model == "CCA (n = 7,341)" & mum_results_prob$exposure == "Belief" & 
                            mum_results_prob$adjustment == "Confounders only"] <- 
  round(avg_comparisons(mod.belief_adjCon, variable = "mum_belief")$conf.low * 100, 2)

mum_results_prob$upper_CI[mum_results_prob$model == "CCA (n = 7,341)" & mum_results_prob$exposure == "Belief" & 
                            mum_results_prob$adjustment == "Confounders only"] <- 
  round(avg_comparisons(mod.belief_adjCon, variable = "mum_belief")$conf.high * 100, 2)


## Adjusted for assumed confounders (as above) and potential confounders and/or mediators (marital status, parity, locus of control and health status)
mod.belief_adjConMed <- glm(mum_donate ~ mum_belief + mum_age + mum_ethnicity + mum_edu + home + imd + rural +
                              mum_finDiffs + mum_employed + mum_compMonth + comp_postPreg + mum_marital + 
                              parity + mum_locus + mum_health, 
                            family = "binomial", data = data_mum, subset = cca_marker == 1)
summary(mod.belief_adjConMed)

# Store results in table
mum_results$OR[mum_results$model == "CCA (n = 7,341)" & mum_results$exposure == "Belief" & 
                 mum_results$adjustment == "Confounders and/or mediators"] <- 
  round(exp(coef(summary(mod.belief_adjConMed))["mum_beliefYes", "Estimate"]), 3)

mum_results$lower_CI[mum_results$model == "CCA (n = 7,341)" & mum_results$exposure == "Belief" & 
                       mum_results$adjustment == "Confounders and/or mediators"] <- 
  round(exp(confint(mod.belief_adjConMed)["mum_beliefYes", "2.5 %"]), 3)

mum_results$upper_CI[mum_results$model == "CCA (n = 7,341)" & mum_results$exposure == "Belief" & 
                       mum_results$adjustment == "Confounders and/or mediators"] <- 
  round(exp(confint(mod.belief_adjConMed)["mum_beliefYes", "97.5 %"]), 3)

mum_results$p[mum_results$model == "CCA (n = 7,341)" & mum_results$exposure == "Belief" & 
                mum_results$adjustment == "Confounders and/or mediators"] <- 
  round(coef(summary(mod.belief_adjConMed))["mum_beliefYes", "Pr(>|z|)"], 4)

# Probability of difference of donating blood
avg_comparisons(mod.belief_adjConMed, variable = "mum_belief")

mum_results_prob$diff[mum_results_prob$model == "CCA (n = 7,341)" & mum_results_prob$exposure == "Belief" & 
                        mum_results_prob$adjustment == "Confounders and/or mediators"] <- 
  round(avg_comparisons(mod.belief_adjConMed, variable = "mum_belief")$estimate * 100, 2)

mum_results_prob$lower_CI[mum_results_prob$model == "CCA (n = 7,341)" & mum_results_prob$exposure == "Belief" & 
                            mum_results_prob$adjustment == "Confounders and/or mediators"] <- 
  round(avg_comparisons(mod.belief_adjConMed, variable = "mum_belief")$conf.low * 100, 2)

mum_results_prob$upper_CI[mum_results_prob$model == "CCA (n = 7,341)" & mum_results_prob$exposure == "Belief" & 
                            mum_results_prob$adjustment == "Confounders and/or mediators"] <- 
  round(avg_comparisons(mod.belief_adjConMed, variable = "mum_belief")$conf.high * 100, 2)



### Second exposure: Religious identity

## Unadjusted model
mod.identity_unadj <- glm(mum_donate ~ mum_identity, family = "binomial", data = data_mum, subset = cca_marker == 1)
summary(mod.identity_unadj)

# Store results in table
mum_results$OR[mum_results$model == "CCA (n = 7,341)" & mum_results$exposure == "Identity" & 
                 mum_results$adjustment == "Unadjusted"] <- 
  round(exp(coef(summary(mod.identity_unadj))["mum_identityReligious", "Estimate"]), 3)

mum_results$lower_CI[mum_results$model == "CCA (n = 7,341)" & mum_results$exposure == "Identity" & 
                       mum_results$adjustment == "Unadjusted"] <- 
  round(exp(confint(mod.identity_unadj)["mum_identityReligious", "2.5 %"]), 3)

mum_results$upper_CI[mum_results$model == "CCA (n = 7,341)" & mum_results$exposure == "Identity" & 
                       mum_results$adjustment == "Unadjusted"] <- 
  round(exp(confint(mod.identity_unadj)["mum_identityReligious", "97.5 %"]), 3)

mum_results$p[mum_results$model == "CCA (n = 7,341)" & mum_results$exposure == "Identity" & 
                mum_results$adjustment == "Unadjusted"] <- 
  round(coef(summary(mod.identity_unadj))["mum_identityReligious", "Pr(>|z|)"], 4)

# Probability of difference of donating blood
avg_comparisons(mod.identity_unadj)

mum_results_prob$diff[mum_results_prob$model == "CCA (n = 7,341)" & mum_results_prob$exposure == "Identity" & 
                        mum_results_prob$adjustment == "Unadjusted"] <- 
  round(avg_comparisons(mod.identity_unadj)$estimate * 100, 2)

mum_results_prob$lower_CI[mum_results_prob$model == "CCA (n = 7,341)" & mum_results_prob$exposure == "Identity" & 
                            mum_results_prob$adjustment == "Unadjusted"] <- 
  round(avg_comparisons(mod.identity_unadj)$conf.low * 100, 2)

mum_results_prob$upper_CI[mum_results_prob$model == "CCA (n = 7,341)" & mum_results_prob$exposure == "Identity" & 
                            mum_results_prob$adjustment == "Unadjusted"] <- 
  round(avg_comparisons(mod.identity_unadj)$conf.high * 100, 2)


## Adjusted for assumed confounders (age, ethnicity, socio-economic position, urban/rural status, recent financial difficulties, employment status, month of questionnaire completion, and whether questionnaire completed during pregnancy or not)
mod.identity_adjCon <- glm(mum_donate ~ mum_identity + mum_age + mum_ethnicity + mum_edu + home + imd + rural + 
                           mum_finDiffs + mum_employed + mum_compMonth + comp_postPreg, 
                         family = "binomial", data = data_mum, subset = cca_marker == 1)
summary(mod.identity_adjCon)

# Store results in table
mum_results$OR[mum_results$model == "CCA (n = 7,341)" & mum_results$exposure == "Identity" & 
                 mum_results$adjustment == "Confounders only"] <- 
  round(exp(coef(summary(mod.identity_adjCon))["mum_identityReligious", "Estimate"]), 3)

mum_results$lower_CI[mum_results$model == "CCA (n = 7,341)" & mum_results$exposure == "Identity" & 
                       mum_results$adjustment == "Confounders only"] <- 
  round(exp(confint(mod.identity_adjCon)["mum_identityReligious", "2.5 %"]), 3)

mum_results$upper_CI[mum_results$model == "CCA (n = 7,341)" & mum_results$exposure == "Identity" & 
                       mum_results$adjustment == "Confounders only"] <- 
  round(exp(confint(mod.identity_adjCon)["mum_identityReligious", "97.5 %"]), 3)

mum_results$p[mum_results$model == "CCA (n = 7,341)" & mum_results$exposure == "Identity" & 
                mum_results$adjustment == "Confounders only"] <- 
  round(coef(summary(mod.identity_adjCon))["mum_identityReligious", "Pr(>|z|)"], 4)

# Probability of difference of donating blood
avg_comparisons(mod.identity_adjCon, variable = "mum_identity")

mum_results_prob$diff[mum_results_prob$model == "CCA (n = 7,341)" & mum_results_prob$exposure == "Identity" & 
                        mum_results_prob$adjustment == "Confounders only"] <- 
  round(avg_comparisons(mod.identity_adjCon, variable = "mum_identity")$estimate * 100, 2)

mum_results_prob$lower_CI[mum_results_prob$model == "CCA (n = 7,341)" & mum_results_prob$exposure == "Identity" & 
                            mum_results_prob$adjustment == "Confounders only"] <- 
  round(avg_comparisons(mod.identity_adjCon, variable = "mum_identity")$conf.low * 100, 2)

mum_results_prob$upper_CI[mum_results_prob$model == "CCA (n = 7,341)" & mum_results_prob$exposure == "Identity" & 
                            mum_results_prob$adjustment == "Confounders only"] <- 
  round(avg_comparisons(mod.identity_adjCon, variable = "mum_identity")$conf.high * 100, 2)


## Adjusted for assumed confounders (as above) and potential confounders and/or mediators (marital status, parity, locus of control and health status)
mod.identity_adjConMed <- glm(mum_donate ~ mum_identity + mum_age + mum_ethnicity + mum_edu + home + imd + rural +
                              mum_finDiffs + mum_employed + mum_compMonth + comp_postPreg + mum_marital + 
                              parity + mum_locus + mum_health, 
                            family = "binomial", data = data_mum, subset = cca_marker == 1)
summary(mod.identity_adjConMed)

# Store results in table
mum_results$OR[mum_results$model == "CCA (n = 7,341)" & mum_results$exposure == "Identity" & 
                 mum_results$adjustment == "Confounders and/or mediators"] <- 
  round(exp(coef(summary(mod.identity_adjConMed))["mum_identityReligious", "Estimate"]), 3)

mum_results$lower_CI[mum_results$model == "CCA (n = 7,341)" & mum_results$exposure == "Identity" & 
                       mum_results$adjustment == "Confounders and/or mediators"] <- 
  round(exp(confint(mod.identity_adjConMed)["mum_identityReligious", "2.5 %"]), 3)

mum_results$upper_CI[mum_results$model == "CCA (n = 7,341)" & mum_results$exposure == "Identity" & 
                       mum_results$adjustment == "Confounders and/or mediators"] <- 
  round(exp(confint(mod.identity_adjConMed)["mum_identityReligious", "97.5 %"]), 3)

mum_results$p[mum_results$model == "CCA (n = 7,341)" & mum_results$exposure == "Identity" & 
                mum_results$adjustment == "Confounders and/or mediators"] <- 
  round(coef(summary(mod.identity_adjConMed))["mum_identityReligious", "Pr(>|z|)"], 4)

# Probability of difference of donating blood
avg_comparisons(mod.identity_adjConMed, variable = "mum_identity")

mum_results_prob$diff[mum_results_prob$model == "CCA (n = 7,341)" & mum_results_prob$exposure == "Identity" & 
                        mum_results_prob$adjustment == "Confounders and/or mediators"] <- 
  round(avg_comparisons(mod.identity_adjConMed, variable = "mum_identity")$estimate * 100, 2)

mum_results_prob$lower_CI[mum_results_prob$model == "CCA (n = 7,341)" & mum_results_prob$exposure == "Identity" & 
                            mum_results_prob$adjustment == "Confounders and/or mediators"] <- 
  round(avg_comparisons(mod.identity_adjConMed, variable = "mum_identity")$conf.low * 100, 2)

mum_results_prob$upper_CI[mum_results_prob$model == "CCA (n = 7,341)" & mum_results_prob$exposure == "Identity" & 
                            mum_results_prob$adjustment == "Confounders and/or mediators"] <- 
  round(avg_comparisons(mod.identity_adjConMed, variable = "mum_identity")$conf.high * 100, 2)


### Third exposure: Religious attendance

## Unadjusted model
mod.attend_unadj <- glm(mum_donate ~ mum_attend, family = "binomial", data = data_mum, subset = cca_marker == 1)
summary(mod.attend_unadj)

# Store results in table
mum_results$OR[mum_results$model == "CCA (n = 7,341)" & mum_results$exposure == "Attend" & 
                 mum_results$adjustment == "Unadjusted"] <- 
  round(exp(coef(summary(mod.attend_unadj))["mum_attendRegular", "Estimate"]), 3)

mum_results$lower_CI[mum_results$model == "CCA (n = 7,341)" & mum_results$exposure == "Attend" & 
                       mum_results$adjustment == "Unadjusted"] <- 
  round(exp(confint(mod.attend_unadj)["mum_attendRegular", "2.5 %"]), 3)

mum_results$upper_CI[mum_results$model == "CCA (n = 7,341)" & mum_results$exposure == "Attend" & 
                       mum_results$adjustment == "Unadjusted"] <- 
  round(exp(confint(mod.attend_unadj)["mum_attendRegular", "97.5 %"]), 3)

mum_results$p[mum_results$model == "CCA (n = 7,341)" & mum_results$exposure == "Attend" & 
                mum_results$adjustment == "Unadjusted"] <- 
  round(coef(summary(mod.attend_unadj))["mum_attendRegular", "Pr(>|z|)"], 4)

# Probability of difference of donating blood
avg_comparisons(mod.attend_unadj)

mum_results_prob$diff[mum_results_prob$model == "CCA (n = 7,341)" & mum_results_prob$exposure == "Attend" & 
                        mum_results_prob$adjustment == "Unadjusted"] <- 
  round(avg_comparisons(mod.attend_unadj)$estimate * 100, 2)

mum_results_prob$lower_CI[mum_results_prob$model == "CCA (n = 7,341)" & mum_results_prob$exposure == "Attend" & 
                            mum_results_prob$adjustment == "Unadjusted"] <- 
  round(avg_comparisons(mod.attend_unadj)$conf.low * 100, 2)

mum_results_prob$upper_CI[mum_results_prob$model == "CCA (n = 7,341)" & mum_results_prob$exposure == "Attend" & 
                            mum_results_prob$adjustment == "Unadjusted"] <- 
  round(avg_comparisons(mod.attend_unadj)$conf.high * 100, 2)


## Adjusted for assumed confounders (age, ethnicity, socio-economic position, urban/rural status, recent financial difficulties, employment status, month of questionnaire completion, and whether questionnaire completed during pregnancy or not)
mod.attend_adjCon <- glm(mum_donate ~ mum_attend + mum_age + mum_ethnicity + mum_edu + home + imd + rural + 
                             mum_finDiffs + mum_employed + mum_compMonth + comp_postPreg, 
                           family = "binomial", data = data_mum, subset = cca_marker == 1)
summary(mod.attend_adjCon)

# Store results in table
mum_results$OR[mum_results$model == "CCA (n = 7,341)" & mum_results$exposure == "Attend" & 
                 mum_results$adjustment == "Confounders only"] <- 
  round(exp(coef(summary(mod.attend_adjCon))["mum_attendRegular", "Estimate"]), 3)

mum_results$lower_CI[mum_results$model == "CCA (n = 7,341)" & mum_results$exposure == "Attend" & 
                       mum_results$adjustment == "Confounders only"] <- 
  round(exp(confint(mod.attend_adjCon)["mum_attendRegular", "2.5 %"]), 3)

mum_results$upper_CI[mum_results$model == "CCA (n = 7,341)" & mum_results$exposure == "Attend" & 
                       mum_results$adjustment == "Confounders only"] <- 
  round(exp(confint(mod.attend_adjCon)["mum_attendRegular", "97.5 %"]), 3)

mum_results$p[mum_results$model == "CCA (n = 7,341)" & mum_results$exposure == "Attend" & 
                mum_results$adjustment == "Confounders only"] <- 
  round(coef(summary(mod.attend_adjCon))["mum_attendRegular", "Pr(>|z|)"], 4)

# Probability of difference of donating blood
avg_comparisons(mod.attend_adjCon, variable = "mum_attend")

mum_results_prob$diff[mum_results_prob$model == "CCA (n = 7,341)" & mum_results_prob$exposure == "Attend" & 
                        mum_results_prob$adjustment == "Confounders only"] <- 
  round(avg_comparisons(mod.attend_adjCon, variable = "mum_attend")$estimate * 100, 2)

mum_results_prob$lower_CI[mum_results_prob$model == "CCA (n = 7,341)" & mum_results_prob$exposure == "Attend" & 
                            mum_results_prob$adjustment == "Confounders only"] <- 
  round(avg_comparisons(mod.attend_adjCon, variable = "mum_attend")$conf.low * 100, 2)

mum_results_prob$upper_CI[mum_results_prob$model == "CCA (n = 7,341)" & mum_results_prob$exposure == "Attend" & 
                            mum_results_prob$adjustment == "Confounders only"] <- 
  round(avg_comparisons(mod.attend_adjCon, variable = "mum_attend")$conf.high * 100, 2)


## Adjusted for assumed confounders (as above) and potential confounders and/or mediators (marital status, parity, locus of control and health status)
mod.attend_adjConMed <- glm(mum_donate ~ mum_attend + mum_age + mum_ethnicity + mum_edu + home + imd + rural +
                                mum_finDiffs + mum_employed + mum_compMonth + comp_postPreg + mum_marital + 
                                parity + mum_locus + mum_health, 
                              family = "binomial", data = data_mum, subset = cca_marker == 1)
summary(mod.attend_adjConMed)

# Store results in table
mum_results$OR[mum_results$model == "CCA (n = 7,341)" & mum_results$exposure == "Attend" & 
                 mum_results$adjustment == "Confounders and/or mediators"] <- 
  round(exp(coef(summary(mod.attend_adjConMed))["mum_attendRegular", "Estimate"]), 3)

mum_results$lower_CI[mum_results$model == "CCA (n = 7,341)" & mum_results$exposure == "Attend" & 
                       mum_results$adjustment == "Confounders and/or mediators"] <- 
  round(exp(confint(mod.attend_adjConMed)["mum_attendRegular", "2.5 %"]), 3)

mum_results$upper_CI[mum_results$model == "CCA (n = 7,341)" & mum_results$exposure == "Attend" & 
                       mum_results$adjustment == "Confounders and/or mediators"] <- 
  round(exp(confint(mod.attend_adjConMed)["mum_attendRegular", "97.5 %"]), 3)

mum_results$p[mum_results$model == "CCA (n = 7,341)" & mum_results$exposure == "Attend" & 
                mum_results$adjustment == "Confounders and/or mediators"] <- 
  round(coef(summary(mod.attend_adjConMed))["mum_attendRegular", "Pr(>|z|)"], 4)

# Probability of difference of donating blood
avg_comparisons(mod.attend_adjConMed, variable = "mum_attend")

mum_results_prob$diff[mum_results_prob$model == "CCA (n = 7,341)" & mum_results_prob$exposure == "Attend" & 
                        mum_results_prob$adjustment == "Confounders and/or mediators"] <- 
  round(avg_comparisons(mod.attend_adjConMed, variable = "mum_attend")$estimate * 100, 2)

mum_results_prob$lower_CI[mum_results_prob$model == "CCA (n = 7,341)" & mum_results_prob$exposure == "Attend" & 
                            mum_results_prob$adjustment == "Confounders and/or mediators"] <- 
  round(avg_comparisons(mod.attend_adjConMed, variable = "mum_attend")$conf.low * 100, 2)

mum_results_prob$upper_CI[mum_results_prob$model == "CCA (n = 7,341)" & mum_results_prob$exposure == "Attend" & 
                            mum_results_prob$adjustment == "Confounders and/or mediators"] <- 
  round(avg_comparisons(mod.attend_adjConMed, variable = "mum_attend")$conf.high * 100, 2)



###########################################################################################
#### Analysis 2) E-value to assess potential unmeasured confounding

## From the results table, there doesn't appear to be any association between religious belief or identity and blood donation in either of the adjusted models, so there is no point in performing sensitivity analyses to explore potential unmeasured confounding. As a demonstration, however, will perform an E-value sensitivity analysis on the unadjusted belief results. Will also perform an E-value analysis for religious attendance, adjusted for potential confounders, as does appear to be some association - albeit rather weak - between religious attendance and an increased odds of having donated blood
mum_results[mum_results$model == "CCA (n = 7,341)", ]


### First, a demonstration of applying the E-value approach for religious belief exposure with no additional adjustment. We just plug in the odds ratio and 95% confidence intervals here, as well as specifying that the outcome is not rare (defined as approx. <10% prevalence), and that we want to know the amount of residual confounding necessary to reduce the association to null (true = 1)
evalues.OR(est = 1.207, lo = 1.094, hi = 1.332, rare = FALSE, true = 1)

## The output here gives a simple table (even if the interpretation is somewhat less straight-forward). The first row ('RR') is the conversion of the odds ratios from our model to risk ratios (as the E-value analysis works on the risk ratio scale). The second row gives the E-value necessary for an unmeasured confounder to reduce the observed effect to null (the 'point' column), or so that it is no longer 'statistically significant' at a given alpha threshold (based on the confidence intervals provided; the 'lower' column). This is on the risk ratio scale. In this example, this output is saying that to make the point estimate null, the association between the unmeasured confounder and both the exposure and outcome must be 1.43; that is, the unmeasured confounder must increase the risk of both the exposure and the outcome by approximately 40% to make this effect null. In the 'lower' column this E-value is 1.26, which is the risk ratio effect size needed for the unmeasured confounder on both the exposure and the outcome to make this effect no longer 'statistically significant' at the 95% level.

# These E-values do not seem that large, especially given that no other variables have been controlled for in this model. Plus, we know that, when including plausible confounders, this association becomes effectively null.

## We can also make a bias plot showing not just the single E-value (i.e., the strength of unmeasured confounding in both the exposure and outcome to turn a causal effect to the null), but the full range of values necessary if one association is stronger than the other. RR_UD is the association between the unmeasured confounder and the outcome, while RR_EU is the association between the exposure and the unmeasured confounder, both on the risk ratio scale. Note that we have to specify the conversion factor - sqrt(1.205) - when working on the odds ratio scale with common outcomes to get the correct output.
bias_plot(sqrt(1.207), xmax = 5)

# Or plot the amount of residual confounding necessary to reach the 95% CI
bias_plot(sqrt(1.094), xmax = 5)


### Next, we can explore whether the amount of residual confounding necessary to make the adjusted religious attendance result null (note: given that the 95% CIs cross the null it will only be possible to assess this relative to the null effect, rather than the 95% level; also, given that the 95% CIs cross the null - albeit only just - we can already make an informed guess that not much residual confounding will be necessary to alter this result to null)
evalues.OR(est = 1.154, lo = 0.996, hi = 1.336, rare = FALSE, true = 1)

# To alter the observed result to null, the level of residual confounding with both the exposure and outcome is an RR of 1.36, approximately a 35% increase in risk of both. This does not seem especially high, given that this is lower than for the unadjusted religious belief association above. However, as we have adjusted for a range of potential confounders, so one would hope that the risk of unmeasured confounding is reduced. Still, given that unmeasured confounding may be a possibility - say, from personality characteristics - I would be hesitant to say that it is unlikely that this result is unbiased by residual confounding.

# Make a bias plot of these results
bias_plot(sqrt(1.154), xmax = 5)

## For comparison, the RR needed to alter the observed odds ratio for religious attendance in the unadjusted model to that in the adjusted model is 1.55, which is lower than the above value for moving the adjusted model to the null.
evalues.OR(est = 1.506, lo = 1.311, hi = 1.729, rare = FALSE, true = 1.154)


### As the results of the 'confounders and/or mediators' model are so similar, results of the E-value sensitivity analyses are practically identical
evalues.OR(est = 1.148, lo = 0.988, hi = 1.332, rare = FALSE, true = 1)


### For comparable sensitivity analyses to residual confounding using the 'Generalised Sensitivity Analysis' approach in Stata, please see the associated "Script3b_MothersData_GSA.do" code.



###########################################################################################
#### Analysis 3) Multiple imputation to explore, and potentially overcome, potential selection bias due to missing data

### Will start with standard multiple imputation (MI) using the 'mice' package

## Drop the complete case marker
data_mum <- data_mum %>%
  select(-cca_marker)

## Check that all categorical variables are factors, as need this for MICE to work
glimpse(data_mum)


### Now for the MI analysis

# First, set-up the imputation methods for each variable. All looks sensible - Will impute continuous variables using predictive mean matching (PMM) to maintain format of original data (i.e., integer values with specified lower and upper bounds)
meth <- make.method(data_mum)
meth

# Second, set-up the prediction matrix, which says which variables to use to impute other variables (here, we want to use all variables to impute all others)
pred <- make.predictorMatrix(data_mum)
pred

# Run a test imputation to make sure it looks okay, and check the amount of missing data in each variable
test <- mice(data_mum, m = 5, method = meth, predictorMatrix = pred, print = TRUE, maxit = 0)
test
table(test$nmis)


## Before running the full imputation model, will run imputation with just 10 imputed datasets but a burn-in period of 20, to ensure that chains are fully-converged and have reached a steady-state (as there is quite a lot of data and many variables to impute, this will take a fair amount of time - Approx. 2.5 hours on my standard-issue laptop)
imp_chains <- mice(data_mum, m = 10, method = meth, predictorMatrix = pred, print = TRUE, maxit = 20, seed = 849011)

## Save these imputations, to avoid having to run the imputations again
save(imp_chains, file = "data_mum_MIChainTest.RData")
#load("data_mum_MIChainTest.RData")

plot(imp_chains)

# Save this plot
pdf("./Results/MotherResults/mother_imputation_convergencePlots.pdf", width = 12, height = 8)
plot(imp_chains)
dev.off()

## Most chains appear to have stabilised by 5 iterations, although some variables appear to only settle down by 10 - To be on the safe side, will run all models below using 10 iterations.


## Now run the proper imputation model. Will create 50 imputed datasets here, with a burn-in of 10 (as all chains are converged and steady by this point) - This takes about 6 hours to run (probably best to let this run over-night, or in the background while doing other kick-ass science)
imp <- mice(data_mum, m = 50, method = meth, predictorMatrix = pred, print = TRUE, maxit = 10, seed = 17871)


## Save these imputations, to avoid having to run the imputations again
save(imp, file = "data_mum_MI.RData")
#load("data_mum_MI.RData")


### Now want to run the same analyses as above, but this time on the imputed data

## As a sanity check that the imputations are sensible, will check the prevalence of the exposures and outcomes - All look sensible, and no obvious problems jumping out

# Religious belief
round((table(data_mum$mum_belief) / sum(table(data_mum$mum_belief))) * 100, 2)
round(summary(pool(with(imp, glm(mum_belief ~ 1, family = binomial(link = "identity")))))[1, "estimate"] * 100, 2)

# Religious identity
round((table(data_mum$mum_identity) / sum(table(data_mum$mum_identity))) * 100, 2)
round(summary(pool(with(imp, glm(mum_identity ~ 1, family = binomial(link = "identity")))))[1, "estimate"] * 100, 2)

# Religious attendance
round((table(data_mum$mum_attend) / sum(table(data_mum$mum_attend))) * 100, 2)
round(summary(pool(with(imp, glm(mum_attend ~ 1, family = binomial(link = "identity")))))[1, "estimate"] * 100, 2)

# Blood donation
round((table(data_mum$mum_donate) / sum(table(data_mum$mum_donate))) * 100, 2)
round(summary(pool(with(imp, glm(mum_donate ~ 1, family = binomial(link = "identity")))))[1, "estimate"] * 100, 2)


### First exposure: Religious belief

## Unadjusted model
mod.belief_unadj_mi <- pool(with(imp, glm(mum_donate ~ mum_belief, family = "binomial")))
(res.belief_unadj_mi <- summary(mod.belief_unadj_mi, conf.int = TRUE))

# Store results in table
mum_results$OR[mum_results$model == "MI (n = 13,477)" & mum_results$exposure == "Belief" & 
                 mum_results$adjustment == "Unadjusted"] <- 
  round(exp(res.belief_unadj_mi$estimate[res.belief_unadj_mi$term == "mum_beliefYes"]), 3)

mum_results$lower_CI[mum_results$model == "MI (n = 13,477)" & mum_results$exposure == "Belief" & 
                       mum_results$adjustment == "Unadjusted"] <- 
  round(exp(res.belief_unadj_mi$'2.5 %'[res.belief_unadj_mi$term == "mum_beliefYes"]), 3)

mum_results$upper_CI[mum_results$model == "MI (n = 13,477)" & mum_results$exposure == "Belief" & 
                       mum_results$adjustment == "Unadjusted"] <- 
  round(exp(res.belief_unadj_mi$'97.5 %'[res.belief_unadj_mi$term == "mum_beliefYes"]), 3)

mum_results$p[mum_results$model == "MI (n = 13,477)" & mum_results$exposure == "Belief" & 
                mum_results$adjustment == "Unadjusted"] <- 
  round(res.belief_unadj_mi$p.value[res.belief_unadj_mi$term == "mum_beliefYes"], 4)


## And now for difference in probability by religion - Have to run this manually on each imputed dataset, then combine together using Rubin's rules

# Set up a matrix to store results in
temp <- matrix(data = NA, nrow = 50, ncol = 2)

# Loop over each dataset, storing the difference in probabilities and associated SE in the above matrix
for (i in 1:50) {
  print(paste0("On imputed dataset: ", i))
  df_temp <- complete(imp, i)
  mod_temp <- glm(mum_donate ~ mum_belief, family = "binomial", data = df_temp)
  temp[i, 1] <- avg_comparisons(mod_temp)$estimate
  temp[i, 2] <- avg_comparisons(mod_temp)$std.error
}
temp

# Generate mean value and SE using Rubin's rules
(mean_RR <- mean(temp[, 1]))
(var_within <- mean(temp[, 2] ^ 2))
(var_between <- ((1 / (50 - 1)) * sum((temp[, 1] - mean_RR) ^ 2)))
(var_total <- var_within + ((1 + (1 / 50)) * var_between))
(se_total <- sqrt(var_total))

# Add these to probability table
mum_results_prob$diff[mum_results_prob$model == "MI (n = 13,477)" & mum_results_prob$exposure == "Belief" & 
                      mum_results_prob$adjustment == "Unadjusted"] <- round(mean_RR * 100, 2)

mum_results_prob$lower_CI[mum_results_prob$model == "MI (n = 13,477)" & mum_results_prob$exposure == "Belief" & 
                            mum_results_prob$adjustment == "Unadjusted"] <- 
  round((mean_RR - (1.96 * se_total)) * 100, 2)

mum_results_prob$upper_CI[mum_results_prob$model == "MI (n = 13,477)" & mum_results_prob$exposure == "Belief" & 
                            mum_results_prob$adjustment == "Unadjusted"] <- 
  round((mean_RR + (1.96 * se_total)) * 100, 2)


## Adjusted for assumed confounders (age, ethnicity, socio-economic position, urban/rural status, recent financial difficulties, employment status, month of questionnaire completion, and whether questionnaire completed during pregnancy or not)
mod.belief_adjCon_mi <- pool(with(imp, glm(mum_donate ~ mum_belief + mum_age + mum_ethnicity + mum_edu + 
                                             home + imd + rural + mum_finDiffs + mum_employed + 
                                             mum_compMonth + comp_postPreg, 
                                           family = "binomial")))
(res.belief_adjCon_mi <- summary(mod.belief_adjCon_mi, conf.int = TRUE))

# Store results in table
mum_results$OR[mum_results$model == "MI (n = 13,477)" & mum_results$exposure == "Belief" & 
                 mum_results$adjustment == "Confounders only"] <- 
  round(exp(res.belief_adjCon_mi$estimate[res.belief_adjCon_mi$term == "mum_beliefYes"]), 3)

mum_results$lower_CI[mum_results$model == "MI (n = 13,477)" & mum_results$exposure == "Belief" & 
                       mum_results$adjustment == "Confounders only"] <- 
  round(exp(res.belief_adjCon_mi$'2.5 %'[res.belief_adjCon_mi$term == "mum_beliefYes"]), 3)

mum_results$upper_CI[mum_results$model == "MI (n = 13,477)" & mum_results$exposure == "Belief" & 
                       mum_results$adjustment == "Confounders only"] <- 
  round(exp(res.belief_adjCon_mi$'97.5 %'[res.belief_adjCon_mi$term == "mum_beliefYes"]), 3)

mum_results$p[mum_results$model == "MI (n = 13,477)" & mum_results$exposure == "Belief" & 
                mum_results$adjustment == "Confounders only"] <- 
  round(res.belief_adjCon_mi$p.value[res.belief_adjCon_mi$term == "mum_beliefYes"], 4)


## And now for difference in probability by religion - Have to run this manually on each imputed dataset, then combine together using Rubin's rules

# Set up a matrix to store results in
temp <- matrix(data = NA, nrow = 50, ncol = 2)

# Loop over each dataset, storing the difference in probabilities and associated SE in the above matrix
for (i in 1:50) {
  print(paste0("On imputed dataset: ", i))
  df_temp <- complete(imp, i)
  mod_temp <- glm(mum_donate ~ mum_belief + mum_age + mum_ethnicity + mum_edu + home + imd + rural + 
                    mum_finDiffs + mum_employed + mum_compMonth + comp_postPreg, 
                  family = "binomial", data = df_temp)
  temp[i, 1] <- avg_comparisons(mod_temp, variable = "mum_belief")$estimate
  temp[i, 2] <- avg_comparisons(mod_temp, variable = "mum_belief")$std.error
}
temp

# Generate mean value and SE using Rubin's rules
(mean_RR <- mean(temp[, 1]))
(var_within <- mean(temp[, 2] ^ 2))
(var_between <- ((1 / (50 - 1)) * sum((temp[, 1] - mean_RR) ^ 2)))
(var_total <- var_within + ((1 + (1 / 50)) * var_between))
(se_total <- sqrt(var_total))

# Add these to probability table
mum_results_prob$diff[mum_results_prob$model == "MI (n = 13,477)" & mum_results_prob$exposure == "Belief" & 
                        mum_results_prob$adjustment == "Confounders only"] <- round(mean_RR * 100, 2)

mum_results_prob$lower_CI[mum_results_prob$model == "MI (n = 13,477)" & mum_results_prob$exposure == "Belief" & 
                            mum_results_prob$adjustment == "Confounders only"] <- 
  round((mean_RR - (1.96 * se_total)) * 100, 2)

mum_results_prob$upper_CI[mum_results_prob$model == "MI (n = 13,477)" & mum_results_prob$exposure == "Belief" & 
                            mum_results_prob$adjustment == "Confounders only"] <- 
  round((mean_RR + (1.96 * se_total)) * 100, 2)


## Adjusted for assumed confounders (as above) and potential confounders and/or mediators (marital status, parity, locus of control and health status)
mod.belief_adjConMed_mi <- pool(with(imp, glm(mum_donate ~ mum_belief + mum_age + mum_ethnicity + mum_edu + 
                                             home + imd + rural + mum_finDiffs + mum_employed + 
                                             mum_compMonth + comp_postPreg + mum_marital + parity + 
                                             mum_locus + mum_health, 
                                           family = "binomial")))
(res.belief_adjConMed_mi <- summary(mod.belief_adjConMed_mi, conf.int = TRUE))

# Store results in table
mum_results$OR[mum_results$model == "MI (n = 13,477)" & mum_results$exposure == "Belief" & 
                 mum_results$adjustment == "Confounders and/or mediators"] <- 
  round(exp(res.belief_adjConMed_mi$estimate[res.belief_adjConMed_mi$term == "mum_beliefYes"]), 3)

mum_results$lower_CI[mum_results$model == "MI (n = 13,477)" & mum_results$exposure == "Belief" & 
                       mum_results$adjustment == "Confounders and/or mediators"] <- 
  round(exp(res.belief_adjConMed_mi$'2.5 %'[res.belief_adjConMed_mi$term == "mum_beliefYes"]), 3)

mum_results$upper_CI[mum_results$model == "MI (n = 13,477)" & mum_results$exposure == "Belief" & 
                       mum_results$adjustment == "Confounders and/or mediators"] <- 
  round(exp(res.belief_adjConMed_mi$'97.5 %'[res.belief_adjConMed_mi$term == "mum_beliefYes"]), 3)

mum_results$p[mum_results$model == "MI (n = 13,477)" & mum_results$exposure == "Belief" & 
                mum_results$adjustment == "Confounders and/or mediators"] <- 
  round(res.belief_adjConMed_mi$p.value[res.belief_adjConMed_mi$term == "mum_beliefYes"], 4)


## And now for difference in probability by religion - Have to run this manually on each imputed dataset, then combine together using Rubin's rules

# Set up a matrix to store results in
temp <- matrix(data = NA, nrow = 50, ncol = 2)

# Loop over each dataset, storing the difference in probabilities and associated SE in the above matrix
for (i in 1:50) {
  print(paste0("On imputed dataset: ", i))
  df_temp <- complete(imp, i)
  mod_temp <- glm(mum_donate ~ mum_belief + mum_age + mum_ethnicity + mum_edu + home + imd + rural + 
                    mum_finDiffs + mum_employed + mum_compMonth + comp_postPreg + mum_marital + 
                    parity + mum_locus + mum_health, 
                  family = "binomial", data = df_temp)
  temp[i, 1] <- avg_comparisons(mod_temp, variable = "mum_belief")$estimate
  temp[i, 2] <- avg_comparisons(mod_temp, variable = "mum_belief")$std.error
}
temp

# Generate mean value and SE using Rubin's rules
(mean_RR <- mean(temp[, 1]))
(var_within <- mean(temp[, 2] ^ 2))
(var_between <- ((1 / (50 - 1)) * sum((temp[, 1] - mean_RR) ^ 2)))
(var_total <- var_within + ((1 + (1 / 50)) * var_between))
(se_total <- sqrt(var_total))

# Add these to probability table
mum_results_prob$diff[mum_results_prob$model == "MI (n = 13,477)" & mum_results_prob$exposure == "Belief" & 
                        mum_results_prob$adjustment == "Confounders and/or mediators"] <- round(mean_RR * 100, 2)

mum_results_prob$lower_CI[mum_results_prob$model == "MI (n = 13,477)" & mum_results_prob$exposure == "Belief" & 
                            mum_results_prob$adjustment == "Confounders and/or mediators"] <- 
  round((mean_RR - (1.96 * se_total)) * 100, 2)

mum_results_prob$upper_CI[mum_results_prob$model == "MI (n = 13,477)" & mum_results_prob$exposure == "Belief" & 
                            mum_results_prob$adjustment == "Confounders and/or mediators"] <- 
  round((mean_RR + (1.96 * se_total)) * 100, 2)


### Second exposure: Religious identity

## Unadjusted model
mod.identity_unadj_mi <- pool(with(imp, glm(mum_donate ~ mum_identity, family = "binomial")))
(res.identity_unadj_mi <- summary(mod.identity_unadj_mi, conf.int = TRUE))

# Store results in table
mum_results$OR[mum_results$model == "MI (n = 13,477)" & mum_results$exposure == "Identity" & 
                 mum_results$adjustment == "Unadjusted"] <- 
  round(exp(res.identity_unadj_mi$estimate[res.identity_unadj_mi$term == "mum_identityReligious"]), 3)

mum_results$lower_CI[mum_results$model == "MI (n = 13,477)" & mum_results$exposure == "Identity" & 
                       mum_results$adjustment == "Unadjusted"] <- 
  round(exp(res.identity_unadj_mi$'2.5 %'[res.identity_unadj_mi$term == "mum_identityReligious"]), 3)

mum_results$upper_CI[mum_results$model == "MI (n = 13,477)" & mum_results$exposure == "Identity" & 
                       mum_results$adjustment == "Unadjusted"] <- 
  round(exp(res.identity_unadj_mi$'97.5 %'[res.identity_unadj_mi$term == "mum_identityReligious"]), 3)

mum_results$p[mum_results$model == "MI (n = 13,477)" & mum_results$exposure == "Identity" & 
                mum_results$adjustment == "Unadjusted"] <- 
  round(res.identity_unadj_mi$p.value[res.identity_unadj_mi$term == "mum_identityReligious"], 4)


## And now for difference in probability by religion - Have to run this manually on each imputed dataset, then combine together using Rubin's rules

# Set up a matrix to store results in
temp <- matrix(data = NA, nrow = 50, ncol = 2)

# Loop over each dataset, storing the difference in probabilities and associated SE in the above matrix
for (i in 1:50) {
  print(paste0("On imputed dataset: ", i))
  df_temp <- complete(imp, i)
  mod_temp <- glm(mum_donate ~ mum_identity, family = "binomial", data = df_temp)
  temp[i, 1] <- avg_comparisons(mod_temp)$estimate
  temp[i, 2] <- avg_comparisons(mod_temp)$std.error
}
temp

# Generate mean value and SE using Rubin's rules
(mean_RR <- mean(temp[, 1]))
(var_within <- mean(temp[, 2] ^ 2))
(var_between <- ((1 / (50 - 1)) * sum((temp[, 1] - mean_RR) ^ 2)))
(var_total <- var_within + ((1 + (1 / 50)) * var_between))
(se_total <- sqrt(var_total))

# Add these to probability table
mum_results_prob$diff[mum_results_prob$model == "MI (n = 13,477)" & mum_results_prob$exposure == "Identity" & 
                        mum_results_prob$adjustment == "Unadjusted"] <- round(mean_RR * 100, 2)

mum_results_prob$lower_CI[mum_results_prob$model == "MI (n = 13,477)" & mum_results_prob$exposure == "Identity" & 
                            mum_results_prob$adjustment == "Unadjusted"] <- 
  round((mean_RR - (1.96 * se_total)) * 100, 2)

mum_results_prob$upper_CI[mum_results_prob$model == "MI (n = 13,477)" & mum_results_prob$exposure == "Identity" & 
                            mum_results_prob$adjustment == "Unadjusted"] <- 
  round((mean_RR + (1.96 * se_total)) * 100, 2)


## Adjusted for assumed confounders (age, ethnicity, socio-economic position, urban/rural status, recent financial difficulties, employment status, month of questionnaire completion, and whether questionnaire completed during pregnancy or not)
mod.identity_adjCon_mi <- pool(with(imp, glm(mum_donate ~ mum_identity + mum_age + mum_ethnicity + mum_edu + 
                                               home + imd + rural + mum_finDiffs + mum_employed + 
                                               mum_compMonth + comp_postPreg, 
                                             family = "binomial")))
(res.identity_adjCon_mi <- summary(mod.identity_adjCon_mi, conf.int = TRUE))

# Store results in table
mum_results$OR[mum_results$model == "MI (n = 13,477)" & mum_results$exposure == "Identity" & 
                 mum_results$adjustment == "Confounders only"] <- 
  round(exp(res.identity_adjCon_mi$estimate[res.identity_adjCon_mi$term == "mum_identityReligious"]), 3)

mum_results$lower_CI[mum_results$model == "MI (n = 13,477)" & mum_results$exposure == "Identity" & 
                       mum_results$adjustment == "Confounders only"] <- 
  round(exp(res.identity_adjCon_mi$'2.5 %'[res.identity_adjCon_mi$term == "mum_identityReligious"]), 3)

mum_results$upper_CI[mum_results$model == "MI (n = 13,477)" & mum_results$exposure == "Identity" & 
                       mum_results$adjustment == "Confounders only"] <- 
  round(exp(res.identity_adjCon_mi$'97.5 %'[res.identity_adjCon_mi$term == "mum_identityReligious"]), 3)

mum_results$p[mum_results$model == "MI (n = 13,477)" & mum_results$exposure == "Identity" & 
                mum_results$adjustment == "Confounders only"] <- 
  round(res.identity_adjCon_mi$p.value[res.identity_adjCon_mi$term == "mum_identityReligious"], 4)


## And now for difference in probability by religion - Have to run this manually on each imputed dataset, then combine together using Rubin's rules

# Set up a matrix to store results in
temp <- matrix(data = NA, nrow = 50, ncol = 2)

# Loop over each dataset, storing the difference in probabilities and associated SE in the above matrix
for (i in 1:50) {
  print(paste0("On imputed dataset: ", i))
  df_temp <- complete(imp, i)
  mod_temp <- glm(mum_donate ~ mum_identity + mum_age + mum_ethnicity + mum_edu + home + imd + rural + 
                    mum_finDiffs + mum_employed + mum_compMonth + comp_postPreg, 
                  family = "binomial", data = df_temp)
  temp[i, 1] <- avg_comparisons(mod_temp, variable = "mum_identity")$estimate
  temp[i, 2] <- avg_comparisons(mod_temp, variable = "mum_identity")$std.error
}
temp

# Generate mean value and SE using Rubin's rules
(mean_RR <- mean(temp[, 1]))
(var_within <- mean(temp[, 2] ^ 2))
(var_between <- ((1 / (50 - 1)) * sum((temp[, 1] - mean_RR) ^ 2)))
(var_total <- var_within + ((1 + (1 / 50)) * var_between))
(se_total <- sqrt(var_total))

# Add these to probability table
mum_results_prob$diff[mum_results_prob$model == "MI (n = 13,477)" & mum_results_prob$exposure == "Identity" & 
                        mum_results_prob$adjustment == "Confounders only"] <- round(mean_RR * 100, 2)

mum_results_prob$lower_CI[mum_results_prob$model == "MI (n = 13,477)" & mum_results_prob$exposure == "Identity" & 
                            mum_results_prob$adjustment == "Confounders only"] <- 
  round((mean_RR - (1.96 * se_total)) * 100, 2)

mum_results_prob$upper_CI[mum_results_prob$model == "MI (n = 13,477)" & mum_results_prob$exposure == "Identity" & 
                            mum_results_prob$adjustment == "Confounders only"] <- 
  round((mean_RR + (1.96 * se_total)) * 100, 2)


## Adjusted for assumed confounders (as above) and potential confounders and/or mediators (marital status, parity, locus of control and health status)
mod.identity_adjConMed_mi <- pool(with(imp, glm(mum_donate ~ mum_identity + mum_age + mum_ethnicity + mum_edu + 
                                               home + imd + rural + mum_finDiffs + mum_employed + 
                                               mum_compMonth + comp_postPreg + mum_marital + parity + 
                                               mum_locus + mum_health, 
                                             family = "binomial")))
(res.identity_adjConMed_mi <- summary(mod.identity_adjConMed_mi, conf.int = TRUE))

# Store results in table
mum_results$OR[mum_results$model == "MI (n = 13,477)" & mum_results$exposure == "Identity" & 
                 mum_results$adjustment == "Confounders and/or mediators"] <- 
  round(exp(res.identity_adjConMed_mi$estimate[res.identity_adjConMed_mi$term == "mum_identityReligious"]), 3)

mum_results$lower_CI[mum_results$model == "MI (n = 13,477)" & mum_results$exposure == "Identity" & 
                       mum_results$adjustment == "Confounders and/or mediators"] <- 
  round(exp(res.identity_adjConMed_mi$'2.5 %'[res.identity_adjConMed_mi$term == "mum_identityReligious"]), 3)

mum_results$upper_CI[mum_results$model == "MI (n = 13,477)" & mum_results$exposure == "Identity" & 
                       mum_results$adjustment == "Confounders and/or mediators"] <- 
  round(exp(res.identity_adjConMed_mi$'97.5 %'[res.identity_adjConMed_mi$term == "mum_identityReligious"]), 3)

mum_results$p[mum_results$model == "MI (n = 13,477)" & mum_results$exposure == "Identity" & 
                mum_results$adjustment == "Confounders and/or mediators"] <- 
  round(res.identity_adjConMed_mi$p.value[res.identity_adjConMed_mi$term == "mum_identityReligious"], 4)


## And now for difference in probability by religion - Have to run this manually on each imputed dataset, then combine together using Rubin's rules

# Set up a matrix to store results in
temp <- matrix(data = NA, nrow = 50, ncol = 2)

# Loop over each dataset, storing the difference in probabilities and associated SE in the above matrix
for (i in 1:50) {
  print(paste0("On imputed dataset: ", i))
  df_temp <- complete(imp, i)
  mod_temp <- glm(mum_donate ~ mum_identity + mum_age + mum_ethnicity + mum_edu + home + imd + rural + 
                    mum_finDiffs + mum_employed + mum_compMonth + comp_postPreg + mum_marital + 
                    parity + mum_locus + mum_health, 
                  family = "binomial", data = df_temp)
  temp[i, 1] <- avg_comparisons(mod_temp, variable = "mum_identity")$estimate
  temp[i, 2] <- avg_comparisons(mod_temp, variable = "mum_identity")$std.error
}
temp

# Generate mean value and SE using Rubin's rules
(mean_RR <- mean(temp[, 1]))
(var_within <- mean(temp[, 2] ^ 2))
(var_between <- ((1 / (50 - 1)) * sum((temp[, 1] - mean_RR) ^ 2)))
(var_total <- var_within + ((1 + (1 / 50)) * var_between))
(se_total <- sqrt(var_total))

# Add these to probability table
mum_results_prob$diff[mum_results_prob$model == "MI (n = 13,477)" & mum_results_prob$exposure == "Identity" & 
                        mum_results_prob$adjustment == "Confounders and/or mediators"] <- round(mean_RR * 100, 2)

mum_results_prob$lower_CI[mum_results_prob$model == "MI (n = 13,477)" & mum_results_prob$exposure == "Identity" & 
                            mum_results_prob$adjustment == "Confounders and/or mediators"] <- 
  round((mean_RR - (1.96 * se_total)) * 100, 2)

mum_results_prob$upper_CI[mum_results_prob$model == "MI (n = 13,477)" & mum_results_prob$exposure == "Identity" & 
                            mum_results_prob$adjustment == "Confounders and/or mediators"] <- 
  round((mean_RR + (1.96 * se_total)) * 100, 2)



### Third exposure: Religious attendance

## Unadjusted model
mod.attend_unadj_mi <- pool(with(imp, glm(mum_donate ~ mum_attend, family = "binomial")))
(res.attend_unadj_mi <- summary(mod.attend_unadj_mi, conf.int = TRUE))

# Store results in table
mum_results$OR[mum_results$model == "MI (n = 13,477)" & mum_results$exposure == "Attend" & 
                 mum_results$adjustment == "Unadjusted"] <- 
  round(exp(res.attend_unadj_mi$estimate[res.attend_unadj_mi$term == "mum_attendRegular"]), 3)

mum_results$lower_CI[mum_results$model == "MI (n = 13,477)" & mum_results$exposure == "Attend" & 
                       mum_results$adjustment == "Unadjusted"] <- 
  round(exp(res.attend_unadj_mi$'2.5 %'[res.attend_unadj_mi$term == "mum_attendRegular"]), 3)

mum_results$upper_CI[mum_results$model == "MI (n = 13,477)" & mum_results$exposure == "Attend" & 
                       mum_results$adjustment == "Unadjusted"] <- 
  round(exp(res.attend_unadj_mi$'97.5 %'[res.attend_unadj_mi$term == "mum_attendRegular"]), 3)

mum_results$p[mum_results$model == "MI (n = 13,477)" & mum_results$exposure == "Attend" & 
                mum_results$adjustment == "Unadjusted"] <- 
  round(res.attend_unadj_mi$p.value[res.attend_unadj_mi$term == "mum_attendRegular"], 4)


## And now for difference in probability by religion - Have to run this manually on each imputed dataset, then combine together using Rubin's rules

# Set up a matrix to store results in
temp <- matrix(data = NA, nrow = 50, ncol = 2)

# Loop over each dataset, storing the difference in probabilities and associated SE in the above matrix
for (i in 1:50) {
  print(paste0("On imputed dataset: ", i))
  df_temp <- complete(imp, i)
  mod_temp <- glm(mum_donate ~ mum_attend, family = "binomial", data = df_temp)
  temp[i, 1] <- avg_comparisons(mod_temp)$estimate
  temp[i, 2] <- avg_comparisons(mod_temp)$std.error
}
temp

# Generate mean value and SE using Rubin's rules
(mean_RR <- mean(temp[, 1]))
(var_within <- mean(temp[, 2] ^ 2))
(var_between <- ((1 / (50 - 1)) * sum((temp[, 1] - mean_RR) ^ 2)))
(var_total <- var_within + ((1 + (1 / 50)) * var_between))
(se_total <- sqrt(var_total))

# Add these to probability table
mum_results_prob$diff[mum_results_prob$model == "MI (n = 13,477)" & mum_results_prob$exposure == "Attend" & 
                        mum_results_prob$adjustment == "Unadjusted"] <- round(mean_RR * 100, 2)

mum_results_prob$lower_CI[mum_results_prob$model == "MI (n = 13,477)" & mum_results_prob$exposure == "Attend" & 
                            mum_results_prob$adjustment == "Unadjusted"] <- 
  round((mean_RR - (1.96 * se_total)) * 100, 2)

mum_results_prob$upper_CI[mum_results_prob$model == "MI (n = 13,477)" & mum_results_prob$exposure == "Attend" & 
                            mum_results_prob$adjustment == "Unadjusted"] <- 
  round((mean_RR + (1.96 * se_total)) * 100, 2)


## Adjusted for assumed confounders (age, ethnicity, socio-economic position, urban/rural status, recent financial difficulties, employment status, month of questionnaire completion, and whether questionnaire completed during pregnancy or not)
mod.attend_adjCon_mi <- pool(with(imp, glm(mum_donate ~ mum_attend + mum_age + mum_ethnicity + mum_edu + home + 
                                             imd + rural + mum_finDiffs + mum_employed + mum_compMonth + 
                                             comp_postPreg, 
                                           family = "binomial")))
(res.attend_adjCon_mi <- summary(mod.attend_adjCon_mi, conf.int = TRUE))

# Store results in table
mum_results$OR[mum_results$model == "MI (n = 13,477)" & mum_results$exposure == "Attend" & 
                 mum_results$adjustment == "Confounders only"] <- 
  round(exp(res.attend_adjCon_mi$estimate[res.attend_adjCon_mi$term == "mum_attendRegular"]), 3)

mum_results$lower_CI[mum_results$model == "MI (n = 13,477)" & mum_results$exposure == "Attend" & 
                       mum_results$adjustment == "Confounders only"] <- 
  round(exp(res.attend_adjCon_mi$'2.5 %'[res.attend_adjCon_mi$term == "mum_attendRegular"]), 3)

mum_results$upper_CI[mum_results$model == "MI (n = 13,477)" & mum_results$exposure == "Attend" & 
                       mum_results$adjustment == "Confounders only"] <- 
  round(exp(res.attend_adjCon_mi$'97.5 %'[res.attend_adjCon_mi$term == "mum_attendRegular"]), 3)

mum_results$p[mum_results$model == "MI (n = 13,477)" & mum_results$exposure == "Attend" & 
                mum_results$adjustment == "Confounders only"] <- 
  round(res.attend_adjCon_mi$p.value[res.attend_adjCon_mi$term == "mum_attendRegular"], 4)


## And now for difference in probability by religion - Have to run this manually on each imputed dataset, then combine together using Rubin's rules

# Set up a matrix to store results in
temp <- matrix(data = NA, nrow = 50, ncol = 2)

# Loop over each dataset, storing the difference in probabilities and associated SE in the above matrix
for (i in 1:50) {
  print(paste0("On imputed dataset: ", i))
  df_temp <- complete(imp, i)
  mod_temp <- glm(mum_donate ~ mum_attend + mum_age + mum_ethnicity + mum_edu + home + imd + rural + 
                    mum_finDiffs + mum_employed + mum_compMonth + comp_postPreg, 
                  family = "binomial", data = df_temp)
  temp[i, 1] <- avg_comparisons(mod_temp, variable = "mum_attend")$estimate
  temp[i, 2] <- avg_comparisons(mod_temp, variable = "mum_attend")$std.error
}
temp

# Generate mean value and SE using Rubin's rules
(mean_RR <- mean(temp[, 1]))
(var_within <- mean(temp[, 2] ^ 2))
(var_between <- ((1 / (50 - 1)) * sum((temp[, 1] - mean_RR) ^ 2)))
(var_total <- var_within + ((1 + (1 / 50)) * var_between))
(se_total <- sqrt(var_total))

# Add these to probability table
mum_results_prob$diff[mum_results_prob$model == "MI (n = 13,477)" & mum_results_prob$exposure == "Attend" & 
                        mum_results_prob$adjustment == "Confounders only"] <- round(mean_RR * 100, 2)

mum_results_prob$lower_CI[mum_results_prob$model == "MI (n = 13,477)" & mum_results_prob$exposure == "Attend" & 
                            mum_results_prob$adjustment == "Confounders only"] <- 
  round((mean_RR - (1.96 * se_total)) * 100, 2)

mum_results_prob$upper_CI[mum_results_prob$model == "MI (n = 13,477)" & mum_results_prob$exposure == "Attend" & 
                            mum_results_prob$adjustment == "Confounders only"] <- 
  round((mean_RR + (1.96 * se_total)) * 100, 2)


## Adjusted for assumed confounders (as above) and potential confounders and/or mediators (marital status, parity, locus of control and health status)
mod.attend_adjConMed_mi <- pool(with(imp, glm(mum_donate ~ mum_attend + mum_age + mum_ethnicity + mum_edu + home + 
                                             imd + rural + mum_finDiffs + mum_employed + mum_compMonth + 
                                             comp_postPreg + mum_marital + parity + mum_locus + mum_health, 
                                           family = "binomial")))
(res.attend_adjConMed_mi <- summary(mod.attend_adjConMed_mi, conf.int = TRUE))

# Store results in table
mum_results$OR[mum_results$model == "MI (n = 13,477)" & mum_results$exposure == "Attend" & 
                 mum_results$adjustment == "Confounders and/or mediators"] <- 
  round(exp(res.attend_adjConMed_mi$estimate[res.attend_adjConMed_mi$term == "mum_attendRegular"]), 3)

mum_results$lower_CI[mum_results$model == "MI (n = 13,477)" & mum_results$exposure == "Attend" & 
                       mum_results$adjustment == "Confounders and/or mediators"] <- 
  round(exp(res.attend_adjConMed_mi$'2.5 %'[res.attend_adjConMed_mi$term == "mum_attendRegular"]), 3)

mum_results$upper_CI[mum_results$model == "MI (n = 13,477)" & mum_results$exposure == "Attend" & 
                       mum_results$adjustment == "Confounders and/or mediators"] <- 
  round(exp(res.attend_adjConMed_mi$'97.5 %'[res.attend_adjConMed_mi$term == "mum_attendRegular"]), 3)

mum_results$p[mum_results$model == "MI (n = 13,477)" & mum_results$exposure == "Attend" & 
                mum_results$adjustment == "Confounders and/or mediators"] <- 
  round(res.attend_adjConMed_mi$p.value[res.attend_adjConMed_mi$term == "mum_attendRegular"], 4)


## And now for difference in probability by religion - Have to run this manually on each imputed dataset, then combine together using Rubin's rules

# Set up a matrix to store results in
temp <- matrix(data = NA, nrow = 50, ncol = 2)

# Loop over each dataset, storing the difference in probabilities and associated SE in the above matrix
for (i in 1:50) {
  print(paste0("On imputed dataset: ", i))
  df_temp <- complete(imp, i)
  mod_temp <- glm(mum_donate ~ mum_attend + mum_age + mum_ethnicity + mum_edu + home + imd + rural + 
                    mum_finDiffs + mum_employed + mum_compMonth + comp_postPreg + mum_marital + 
                    parity + mum_locus + mum_health, 
                  family = "binomial", data = df_temp)
  temp[i, 1] <- avg_comparisons(mod_temp, variable = "mum_attend")$estimate
  temp[i, 2] <- avg_comparisons(mod_temp, variable = "mum_attend")$std.error
}
temp

# Generate mean value and SE using Rubin's rules
(mean_RR <- mean(temp[, 1]))
(var_within <- mean(temp[, 2] ^ 2))
(var_between <- ((1 / (50 - 1)) * sum((temp[, 1] - mean_RR) ^ 2)))
(var_total <- var_within + ((1 + (1 / 50)) * var_between))
(se_total <- sqrt(var_total))

# Add these to probability table
mum_results_prob$diff[mum_results_prob$model == "MI (n = 13,477)" & mum_results_prob$exposure == "Attend" & 
                        mum_results_prob$adjustment == "Confounders and/or mediators"] <- round(mean_RR * 100, 2)

mum_results_prob$lower_CI[mum_results_prob$model == "MI (n = 13,477)" & mum_results_prob$exposure == "Attend" & 
                            mum_results_prob$adjustment == "Confounders and/or mediators"] <- 
  round((mean_RR - (1.96 * se_total)) * 100, 2)

mum_results_prob$upper_CI[mum_results_prob$model == "MI (n = 13,477)" & mum_results_prob$exposure == "Attend" & 
                            mum_results_prob$adjustment == "Confounders and/or mediators"] <- 
  round((mean_RR + (1.96 * se_total)) * 100, 2)



########################################################################################################
#### Compare results of the complete-case analysis to those from multiple imputation

## Point estimates are practically identical in both CCA and MI, potentially suggesting that the CCA was largely unbiased by selection (i.e., the CCA met the MAR assumption), or that the MI did not sufficiently account for selection (i.e., the MI [and the CCA] did not meet the MAR assumption, and data may be MNAR) - From these results, it is impossible to tell between these two scenarios. The only real difference between the CCA and MI results is that the standard errors/confidence intervals are narrower for MI, as it is more efficient because it makes use of all the available data and the sample size is practically double that of the CCA.
mum_results

# Convert numeric results to numeric
mum_results <- mum_results %>%
  mutate(OR = as.numeric(OR)) %>%
  mutate(lower_CI = as.numeric(lower_CI)) %>%
  mutate(upper_CI = as.numeric(upper_CI)) %>%
  mutate(p = as.numeric(p))

mum_results
glimpse(mum_results)

# Save these results
write_csv(mum_results, file = "./Results/MotherResults/mum_results_CCAvsMI.csv")


## Make a nice forest plot comparing these results

# Set 'exposure' as a factor first
mum_results_plot <- mum_results %>%
  mutate(exposure = factor(exposure, levels = c("Belief", "Identity", "Attend")))
mum_results_plot

# Make one plot with all data combined, then use 'facet_wrap to split apart afterwards, maintaining a shared y-axis and legend
(p_combined <- ggplot(mum_results_plot, aes(x = adjustment, y = OR, ymin = lower_CI, ymax = upper_CI, 
                                            col = fct_rev(model), fill = fct_rev(model))) + 
    geom_hline(yintercept = 1, lty = 2) +
    geom_linerange(size = 0.5, position = position_dodge(width = 0.75), show.legend = FALSE) +
    geom_point(size = 2, position = position_dodge(width = 0.75)) +
    scale_fill_manual(values = c("red", "black"), guide = guide_legend(reverse = TRUE, byrow = TRUE), name = "Model",
                      labels = c("Multiple\nimputation\n(n = 13,477)", "Complete-case\nanalysis\n(n = 7,341)")) +
    scale_color_manual(values = c("red", "black"), guide = guide_legend(reverse = TRUE, byrow = TRUE), name = "Model",
                       labels = c("Multiple\nimputation\n(n = 13,477)", "Complete-case\nanalysis\n(n = 7,341)")) +
    scale_y_continuous(breaks = c(0.75, 1, 1.25, 1.5, 1.75), limits = c(0.7, 1.8)) +
    labs(x = "", y = "Odds ratio for blood donation",) +
    coord_flip() +
    theme_bw() + 
    facet_wrap(exposure ~ ., ncol = 1, labeller = as_labeller(c(Belief = "Religious Belief", 
                                                                Identity = "Religious Affiliation",
                                                                Attend = "Religious Attendance"))) +
    theme(panel.grid.minor = element_blank(), axis.text = element_text(size = 12),
          legend.text = element_text(size = 10), legend.title = element_text(size = 12),
          axis.title.x = element_text(size = 14), strip.background = element_blank(),
          strip.text = element_text(size = 14), legend.spacing.y = unit(0.25, 'cm')))

## Save this plot
pdf("./Results/MotherResults/mum_results_CCAvsMI_plot.pdf", width = 8, height = 8)
p_combined
dev.off()


## Plot with just CCA results
mum_results_plot_CCA <- mum_results_plot %>%
  filter(model == "CCA (n = 7,341)")
mum_results_plot_CCA

(p_combined_CCA <- ggplot(mum_results_plot_CCA, aes(x = adjustment, y = OR, ymin = lower_CI, ymax = upper_CI)) + 
    geom_hline(yintercept = 1, lty = 2) +
    geom_linerange(size = 0.5) +
    geom_point(size = 2) +
    scale_y_continuous(breaks = c(0.75, 1, 1.25, 1.5, 1.75), limits = c(0.7, 1.8)) +
    labs(x = "", y = "Odds ratio for blood donation",) +
    coord_flip() +
    theme_bw() + 
    facet_wrap(exposure ~ ., ncol = 1, labeller = as_labeller(c(Belief = "Religious Belief", 
                                                                Identity = "Religious Affiliation",
                                                                Attend = "Religious Attendance"))) +
    theme(panel.grid.minor = element_blank(), axis.text = element_text(size = 12),
          legend.text = element_text(size = 10), legend.title = element_text(size = 12),
          axis.title.x = element_text(size = 14), strip.background = element_blank(),
          strip.text = element_text(size = 14)))

## Save this plot
pdf("./Results/MotherResults/mum_results_CCA_plot.pdf", width = 8, height = 8)
p_combined_CCA
dev.off()


### And repeat for differences in probability results
mum_results_prob

# Convert numeric results to numeric
mum_results_prob <- mum_results_prob %>%
  mutate(diff = as.numeric(diff)) %>%
  mutate(lower_CI = as.numeric(lower_CI)) %>%
  mutate(upper_CI = as.numeric(upper_CI))

mum_results_prob
glimpse(mum_results_prob)

# Save these results
write_csv(mum_results_prob, file = "./Results/MotherResults/mum_results_CCAvsMI_probs.csv")


## Make a nice forest plot comparing these results

# Set 'exposure' as a factor first
mum_results_prob_plot <- mum_results_prob %>%
  mutate(exposure = factor(exposure, levels = c("Belief", "Identity", "Attend")))
mum_results_prob_plot

# Make one plot with all data combined, then use 'facet_wrap to split apart afterwards, maintaining a shared y-axis and legend
(p_combined_prob <- ggplot(mum_results_prob_plot, aes(x = adjustment, y = diff, ymin = lower_CI, ymax = upper_CI, 
                                            col = fct_rev(model), fill = fct_rev(model))) + 
    geom_hline(yintercept = 0, lty = 2) +
    geom_linerange(size = 0.5, position = position_dodge(width = 0.75), show.legend = FALSE) +
    geom_point(size = 2, position = position_dodge(width = 0.75)) +
    scale_fill_manual(values = c("red", "black"), guide = guide_legend(reverse = TRUE, byrow = TRUE), name = "Model",
                      labels = c("Multiple\nimputation\n(n = 13,477)", "Complete-case\nanalysis\n(n = 7,341)")) +
    scale_color_manual(values = c("red", "black"), guide = guide_legend(reverse = TRUE, byrow = TRUE), name = "Model",
                       labels = c("Multiple\nimputation\n(n = 13,477)", "Complete-case\nanalysis\n(n = 7,341)")) +
    scale_y_continuous(breaks = c(-2, 0, 2, 4, 6, 8, 10, 12), limits = c(-3.5, 13)) +
    labs(x = "", y = "Predicted difference in blood donation (%)",) +
    coord_flip() +
    theme_bw() + 
    facet_wrap(exposure ~ ., ncol = 1, labeller = as_labeller(c(Belief = "Religious Belief", 
                                                                Identity = "Religious Affiliation",
                                                                Attend = "Religious Attendance"))) +
    theme(panel.grid.minor = element_blank(), axis.text = element_text(size = 12),
          legend.text = element_text(size = 10), legend.title = element_text(size = 12),
          axis.title.x = element_text(size = 14), strip.background = element_blank(),
          strip.text = element_text(size = 14), legend.spacing.y = unit(0.25, 'cm')))

## Save this plot
pdf("./Results/MotherResults/mum_results_CCAvsMI_prob_plot.pdf", width = 8, height = 8)
p_combined_prob
dev.off()


## Plot with just CCA results
mum_results_prob_plot_CCA <- mum_results_prob_plot %>%
  filter(model == "CCA (n = 7,341)")
mum_results_prob_plot_CCA

(p_combined_prob_CCA <- ggplot(mum_results_prob_plot_CCA, aes(x = adjustment, y = diff, ymin = lower_CI, 
                                                              ymax = upper_CI)) + 
    geom_hline(yintercept = 0, lty = 2) +
    geom_linerange(size = 0.5) +
    geom_point(size = 2) +
    scale_y_continuous(breaks = c(-2, 0, 2, 4, 6, 8, 10, 12), limits = c(-3.5, 13)) +
    labs(x = "", y = "Predicted difference in blood donation (%)",) +
    coord_flip() +
    theme_bw() + 
    facet_wrap(exposure ~ ., ncol = 1, labeller = as_labeller(c(Belief = "Religious Belief", 
                                                                Identity = "Religious Affiliation",
                                                                Attend = "Religious Attendance"))) +
    theme(panel.grid.minor = element_blank(), axis.text = element_text(size = 12),
          legend.text = element_text(size = 10), legend.title = element_text(size = 12),
          axis.title.x = element_text(size = 14), strip.background = element_blank(),
          strip.text = element_text(size = 14)))

## Save this plot
pdf("./Results/MotherResults/mum_results_CCA_prob_plot.pdf", width = 8, height = 8)
p_combined_prob_CCA
dev.off()



######################################################################################################
#### As discussed in the main text, while MICE ought to return unbiased estimates assuming that the 'missing at random' assumption is met, it is impossible to definitively verify this assumption. To relax this assumption, and explore how different missing-not-at-random mechanisms impact our results, we can use NARMICE (not-at-random multiple imputation by chained equations). This is similar to standard MICE above, but we allow the missing values to deviate from the missing-at-random predictions; here, we will explore when the outcome (blood donation), the exposure (religious attendance), and both the exposure and outcome together, are MNAR, under a range of values. We focus on the religious attendance exposure here as religious belief and identity have little association with continued ALSPAC participation when adjusting for various sociodemographic confounders, while religious attendance still positively predicts increased participation (https://wellcomeopenresearch.org/articles/7-186).

## Note: As this NARMICE approach requires imputing over a range of sensitivity parameters, it can take a long time to run. I will set-up and test the script here, but run the full analysis on the University of Bristol's High Performance Computing suite (https://www.bristol.ac.uk/acrc/high-performance-computing/). If you do not have access to such computing facilities, a few solutions are possible if run-time is really prohibitive: 1) Reduce the number of imputations (say, from 50 to 25), although this may result in less accurate results; 2) Reduce the burn-in iteration period (say, from 10 to 5), although make sure that imputations have reached a steady state, else results may be biased; or 3) Reduce the number of sensitivity parameters to search through (e.g., rather than searching a range of 12 sensitivity parameters, you could select three values which span a plausible range of values, and compare results using this reduced set).


### The process for NARMICE is very similar to standard MI, except that: 1) you need to specify the sensitivity parameters for the variables you want to vary the MNAR-ness for; and 2) you need to include a missingness indicator for each variable with missing data in the imputation model

### NOTE: The recommendation to include missingness indicators for for variables with missing data is made by Tompsett et al. in their NARMICE paper (https://onlinelibrary.wiley.com/doi/pdf/10.1002/sim.7643). However, in the partners's data the inclusion of these missingness markers resulted in some implausible imputations, especially of the religious attendance exposure (those with missing data on this variable were more likely to be imputed as 'regular attendees', which is unlikely [as those with missing data would be expected to be *less* likely to attend], and is at odds with with standard MI analysis, which did not produce such results). This is probably because, in this ALSPAC data, many of the missingness indicators are highly-collinear, potentially producing such spurious results. The mother's NARMICE results with the missingness markers seem broadly sensible, but given this potential issue in the final full analysis on the high-performance computing clusters will compare the mother's results with vs without the missingness markers, to see if/how results differ.


### The first step of this analysis is to unload the standard mice package (if loaded), and load the user-written NARMICE extension to this mice package (details on this installation are at the top of the script)
detach(package:mice, unload = TRUE)
library(mice, lib.loc = "C:/Temp/mice_test")

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



###### NARMICE analysis 1: Exposure (religious attendance) MNAR

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

# Set up vector describing manner of imputation for each variable - As we want to run vary the association between religious attendance and it's missingness, we specify this as 'logregSens', which is a logistic sensitivity analysis - All others are the same as for the standard MI above
meth <- ini$method
meth

meth["mum_attend"] <- "logregSens"
meth

# Choose number of imputations and burn-in period - For testing purposes will just run 5 imputations with a burn-in period of 3, but in actual analyses will up this to 50 imputations with a burn-in period of 10
narmice_numimps <- 5
narmice_numiter <- 3

# To collect the parameters of interest
tipping_exposureMNAR <- as.data.frame(array(dim = c(dim = length(seq.int(-2, 0, by = 1)), 19))) # Number of sensitivity values we're going to try (varying the CSP from -2 to 0, in steps of 1 [will decrease to steps of 0.25 in final analyses]), plus the number of parameters we're going to store (here, is 19 [see row below])
colnames(tipping_exposureMNAR) <- c("csp", "msp", "imor", "sampprev", "est_unadj", "se_unadj", "lci_unadj",
                                   "uci_unadj", "p_unadj", "est_con", "se_con", "lci_con", "uci_con", "p_con",
                                   "est_conMed", "se_conMed", "lci_conMed", "uci_conMed", "p_conMed")
tipping_exposureMNAR

# Looping over delta/CSP values (i) - A CSP of -2 means that individuals with missing religious attendance data have -2 lower log-odds of attending regularly, compared to those with data (conditional on all other covariates); the converse applies to positive CSPs (i.e., those with missing data having greater likelihood of attending regularly), while a CSP of 0 should approximately correspond to a standard MI model, as there is no adjustment for data potentially being missing-not-at-random.
set.seed(941959)
k <- 0
for (i in seq.int(-2, 0, by = 1)) {
  k <- k+1 
  print(paste0("CSP = ", i))
  
  # specify a delta/CSP value for the prediction equation for missing data
  pSens[["mum_attend"]]<-list(c(i))
  
  # NARMICE imputation
  imp_NARMICE <- mice(data_mum_narmice, m = narmice_numimps, method = meth, predictorMatrix = pred,
                      predictorSens=predSens, parmSens=pSens, print = TRUE, maxit = narmice_numiter)
  
  # Derive the MSP for the given CSP value
  msp <- round(summary(pool(with(imp_NARMICE, 
                                 glm(mum_attend ~ M_attend, family = "binomial")))), 3)
  
  # Derive the prevalence of religious attendance in sample
  wholesampprev <- round(summary(pool(with(imp_NARMICE, 
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
  tipping_exposureMNAR[k,"csp"] <- i
  tipping_exposureMNAR[k,"msp"] <- msp["M_attend2", "est"]
  tipping_exposureMNAR[k,"imor"] <- exp(msp["M_attend2", "est"])
  tipping_exposureMNAR[k,"sampprev"] <- wholesampprev["(Intercept)", "est"]  
  tipping_exposureMNAR[k,"est_unadj"] <- res_unadj["mum_attend2", "est"]
  tipping_exposureMNAR[k,"se_unadj"] <- res_unadj["mum_attend2", "se"]
  tipping_exposureMNAR[k,"lci_unadj"] <- res_unadj["mum_attend2", "lo 95"]
  tipping_exposureMNAR[k,"uci_unadj"] <- res_unadj["mum_attend2", "hi 95"]
  tipping_exposureMNAR[k,"p_unadj"] <- res_unadj["mum_attend2", "Pr(>|t|)"]
  tipping_exposureMNAR[k,"est_con"] <- res_con["mum_attend2", "est"]
  tipping_exposureMNAR[k,"se_con"] <- res_con["mum_attend2", "se"]
  tipping_exposureMNAR[k,"lci_con"] <- res_con["mum_attend2", "lo 95"]
  tipping_exposureMNAR[k,"uci_con"] <- res_con["mum_attend2", "hi 95"]
  tipping_exposureMNAR[k,"p_con"] <- res_con["mum_attend2", "Pr(>|t|)"]
  tipping_exposureMNAR[k,"est_conMed"] <- res_conMed["mum_attend2", "est"]
  tipping_exposureMNAR[k,"se_conMed"] <- res_conMed["mum_attend2", "se"]
  tipping_exposureMNAR[k,"lci_conMed"] <- res_conMed["mum_attend2", "lo 95"]
  tipping_exposureMNAR[k,"uci_conMed"] <- res_conMed["mum_attend2", "hi 95"]
  tipping_exposureMNAR[k,"p_conMed"] <- res_conMed["mum_attend2", "Pr(>|t|)"]
  
  print(tipping_exposureMNAR[k,])
}

# Look at the 'tipping' output which contains all the values/estimates
tipping_exposureMNAR

# Convert the log odds to ORs
tipping_exposureMNAR$or_unadj <- exp(tipping_exposureMNAR$est_unadj)
tipping_exposureMNAR$lci_or_unadj <- exp(tipping_exposureMNAR$lci_unadj)
tipping_exposureMNAR$uci_or_unadj <- exp(tipping_exposureMNAR$uci_unadj)
tipping_exposureMNAR$or_con <- exp(tipping_exposureMNAR$est_con)
tipping_exposureMNAR$lci_or_con <- exp(tipping_exposureMNAR$lci_con)
tipping_exposureMNAR$uci_or_con <- exp(tipping_exposureMNAR$uci_con)
tipping_exposureMNAR$or_conMed <- exp(tipping_exposureMNAR$est_conMed)
tipping_exposureMNAR$lci_or_conMed <- exp(tipping_exposureMNAR$lci_conMed)
tipping_exposureMNAR$uci_or_conMed <- exp(tipping_exposureMNAR$uci_conMed)
tipping_exposureMNAR



## Plot the delta/CSP and IMOR (ignorable missingness odds ratio) and associated estimated difference in religious attendance odds ratio. Will just use the 'confounder only' adjustment scenario, as inclusion of potential confounders/mediators makes little difference to results.
delta_csp_plot_exposureMNAR <- ggplot(data = tipping_exposureMNAR, aes(x = csp)) +
  geom_point(aes(y = or_con), size = 2, colour = "blue") +
  geom_line(aes(y = or_con), color = "blue") +
  geom_line(aes(y = lci_or_con), linetype = "dashed", color = "green") +
  geom_line(aes(y = uci_or_con), linetype = "dashed", color = "red") +
  geom_vline(xintercept = 0, color = "red", size = 1) +
  geom_hline(yintercept = 1, color = "red", size = 1) +
  xlab("Delta/CSP value for religious attendance") + ylab("Estimated religious attendance OR") +
  theme_bw()
plot(delta_csp_plot_exposureMNAR)


# MSP
delta_msp_plot_exposureMNAR <- ggplot(data = tipping_exposureMNAR, aes(x = imor)) +
  geom_point(aes(y = or_con), size = 2, colour = "blue") +
  geom_line(aes(y = or_con), color = "blue") +
  geom_line(aes(y = lci_or_con), linetype = "dashed", color = "green") +
  geom_line(aes(y = uci_or_con), linetype = "dashed", color = "red") +
  geom_vline(xintercept = 1, color = "red", size = 1) +
  geom_hline(yintercept = 1, color = "red", size = 1) +
  xlab("IMOR value for religious attendance") + ylab("Estimated religious attendance OR") +
  theme_bw() +
  scale_x_continuous(trans = "log", breaks = c(0.05, 0.1, 0.25, 0.5, 1, 2))
plot(delta_msp_plot_exposureMNAR)


## Also plot the estimated prevalence of religious attendance in the sample, as alternative X-axis - Have put reference line at the observed prevalence value of ~16%
delta_prev_plot_exposureMNAR <- ggplot(data = tipping_exposureMNAR, aes(x = sampprev)) +
  geom_point(aes(y = or_con), size = 2, colour = "blue") +
  geom_line(aes(y = or_con), color = "blue") +
  geom_line(aes(y = lci_or_con), linetype = "dashed", color = "green") +
  geom_line(aes(y = uci_or_con), linetype = "dashed", color = "red") +
  geom_vline(xintercept = 0.16, color = "red", size = 1) +
  geom_hline(yintercept = 1, color = "red", size = 1) +
  xlab("Religious attendance prevalence") + ylab("Estimated religious attendance OR") +
  theme_bw()
plot(delta_prev_plot_exposureMNAR)



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

# Choose number of imputations and burn-in period - For testing purposes will just run 5 imputations with a burn-in period of 3, but in actual analyses will up this to 50 imputations with a burn-in period of 10
narmice_numimps <- 5
narmice_numiter <- 3

# To collect the parameters of interest
tipping_outcomeMNAR <- as.data.frame(array(dim = c(dim = length(seq.int(-2, 0, by = 1)), 19))) # Number of sensitivity values we're going to try (varying the CSP from -2 to 0, in steps of 1 [will decrease to steps of 0.25 in final analyses]), plus the number of parameters we're going to store (here, is 19 [see row below])
colnames(tipping_outcomeMNAR) <- c("csp", "msp", "imor", "sampprev", "est_unadj", "se_unadj", "lci_unadj",
                                   "uci_unadj", "p_unadj", "est_con", "se_con", "lci_con", "uci_con", "p_con",
                                   "est_conMed", "se_conMed", "lci_conMed", "uci_conMed", "p_conMed")
tipping_outcomeMNAR

# Looping over delta/CSP values (i) - A CSP of -2 means that individuals with missing blood donation data have -2 lower log-odds of having given blood, compared to those with data (conditional on all other covariates); the converse applies to positive CSPs (i.e., those with missing data having greater likelihood of having given blood), while a CSP of 0 should approximately correspond to a standard MI model, as there is no adjustment for data potentially being missing-not-at-random.
set.seed(720455)
k <- 0
for (i in seq.int(-2, 0, by = 1)) {
  k <- k+1 
  print(paste0("CSP = ", i))
  
  # specify a delta/CSP value for the prediction equation for missing data
  pSens[["mum_donate"]]<-list(c(i))
  
  # NARMICE imputation
  imp_NARMICE <- mice(data_mum_narmice, m = narmice_numimps, method = meth, predictorMatrix = pred,
                      predictorSens=predSens, parmSens=pSens, print = TRUE, maxit = narmice_numiter)
  
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



## Plot the delta/CSP and IMOR (ignorable missingness odds ratio) and associated estimated difference in religious attendance odds ratio. Will just use the 'confounder only' adjustment scenario, as inclusion of potential confounders/mediators makes little difference to results.
delta_csp_plot_outcomeMNAR <- ggplot(data = tipping_outcomeMNAR, aes(x = csp)) +
  geom_point(aes(y = or_con), size = 2, colour = "blue") +
  geom_line(aes(y = or_con), color = "blue") +
  geom_line(aes(y = lci_or_con), linetype = "dashed", color = "green") +
  geom_line(aes(y = uci_or_con), linetype = "dashed", color = "red") +
  geom_vline(xintercept = 0, color = "red", size = 1) +
  geom_hline(yintercept = 1, color = "red", size = 1) +
  xlab("Delta/CSP value for blood donation") + ylab("Estimated religious attendance OR") +
  theme_bw()
plot(delta_csp_plot_outcomeMNAR)


# MSP
delta_msp_plot_outcomeMNAR <- ggplot(data = tipping_outcomeMNAR, aes(x = imor)) +
  geom_point(aes(y = or_con), size = 2, colour = "blue") +
  geom_line(aes(y = or_con), color = "blue") +
  geom_line(aes(y = lci_or_con), linetype = "dashed", color = "green") +
  geom_line(aes(y = uci_or_con), linetype = "dashed", color = "red") +
  geom_vline(xintercept = 1, color = "red", size = 1) +
  geom_hline(yintercept = 1, color = "red", size = 1) +
  xlab("IMOR value for blood donation") + ylab("Estimated religious attendance OR") +
  theme_bw() +
  scale_x_continuous(trans = "log", breaks = c(0.05, 0.1, 0.25, 0.5, 1, 2))
plot(delta_msp_plot_outcomeMNAR)


## Also plot the estimated prevalence of blood donation in the sample, as alternative X-axis - Have put reference line at the observed prevalence value of ~30%
delta_prev_plot_outcomeMNAR <- ggplot(data = tipping_outcomeMNAR, aes(x = sampprev)) +
  geom_point(aes(y = or_con), size = 2, colour = "blue") +
  geom_line(aes(y = or_con), color = "blue") +
  geom_line(aes(y = lci_or_con), linetype = "dashed", color = "green") +
  geom_line(aes(y = uci_or_con), linetype = "dashed", color = "red") +
  geom_vline(xintercept = 0.30, color = "red", size = 1) +
  geom_hline(yintercept = 1, color = "red", size = 1) +
  xlab("Blood donation prevalence") + ylab("Estimated religious attendance OR") +
  theme_bw()
plot(delta_prev_plot_outcomeMNAR)



###### NARMICE analysis 3: Exposure (religious attendance) and outcome (blood donation) MNAR

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

# Set up vector describing manner of imputation for each variable - As we want to run vary the association between both the outcome blood donation and it's misisngness, as well as the exposure religious attendance and it's missingness, we specify these as 'logregSens', which is a logistic sensitivity analysis - All others are the same as for the standard MI above
meth <- ini$method
meth

meth["mum_donate"] <- "logregSens"
meth["mum_attend"] <- "logregSens"
meth

# Choose number of imputations and burn-in period - For testing purposes will just run 5 imputations with a burn-in period of 3, but in actual analyses will up this to 50 imputations with a burn-in period of 10
narmice_numimps <- 5
narmice_numiter <- 3

# To collect the parameters of interest
tipping_outcomeExposureMNAR <- as.data.frame(array(dim = c(dim = length(seq.int(-2, 0, by = 1)) *
                                                             length(seq.int(-2, 0, by = 1)), 23))) # Number of sensitivity values we're going to try (varying the CSP from -2 to 0, in steps of 1 [will decrease to steps of 0.25 in final analyses; to reduce processing time]), plus the number of parameters we're going to store (here, is 23 [see row below])
colnames(tipping_outcomeExposureMNAR) <- c("csp_outcome", "msp_outcome", "imor_outcome", "sampprev_outcome", 
                                    "csp_exposure", "msp_exposure", "imor_exposure", "sampprev_exposure",
                                    "est_unadj", "se_unadj", "lci_unadj", "uci_unadj", "p_unadj", 
                                    "est_con", "se_con", "lci_con", "uci_con", "p_con",
                                    "est_conMed", "se_conMed", "lci_conMed", "uci_conMed", "p_conMed")
tipping_outcomeExposureMNAR

# Looping over delta/CSP values, for both outcome (i) and exposure (j)
set.seed(640292)
k <- 0
for (i in seq.int(-2, 0, by = 1)) {
  for (j in seq.int(-2, 0, by = 1)) {
    k <- k+1 
    print(paste0("CSP for outcome = ", i, ": CSP for exposure = ", j))
    
    # specify a delta/CSP value for the prediction equation for missing data
    pSens[["mum_donate"]]<-list(c(i))
    pSens[["mum_attend"]]<-list(c(j))
    
    # NARMICE imputation
    imp_NARMICE <- mice(data_mum_narmice, m = narmice_numimps, method = meth, predictorMatrix = pred,
                        predictorSens=predSens, parmSens=pSens, print = TRUE, maxit = narmice_numiter)
    
    ## Derive the MSPs for the given CSP value, for both outcome and exposure
    msp_outcome <- round(summary(pool(with(imp_NARMICE, 
                                            glm(mum_donate ~ M_donate, family = "binomial")))), 3)
    
    msp_exposure <- round(summary(pool(with(imp_NARMICE, 
                                   glm(mum_attend ~ M_attend, family = "binomial")))), 3)
    
    
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
    tipping_outcomeExposureMNAR[k,"msp_outcome"] <- msp_outcome["M_donate2", "est"]
    tipping_outcomeExposureMNAR[k,"imor_outcome"] <- exp(msp_outcome["M_donate2", "est"])
    tipping_outcomeExposureMNAR[k,"sampprev_outcome"] <- wholesampprev_outcome["(Intercept)", "est"]
    tipping_outcomeExposureMNAR[k,"csp_exposure"] <- j
    tipping_outcomeExposureMNAR[k,"msp_exposure"] <- msp_exposure["M_attend2", "est"]
    tipping_outcomeExposureMNAR[k,"imor_exposure"] <- exp(msp_exposure["M_attend2", "est"])
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



### Plot the delta/CSP and IMOR (ignorable missingness odds ratio) and associated estimated difference in religious attendance odds ratio. Will just use the 'confounder only' adjustment scenario, as inclusion of potential confounders/mediators makes little difference to results.

### As lots of results here, will create two kinds of plots - First a heat-map of all the results, and second a facet plot with the outcome MNAR results split by different levels of exposure MNAR

## First, a heatmap with the odds ratio as the colour fill
p_heatmap_or <- ggplot(data = tipping_outcomeExposureMNAR, aes(x = csp_exposure, y = csp_outcome, fill = or_con)) +
  geom_tile() +
  geom_text(aes(label = round(or_con, 2))) +
  scale_fill_gradient2(low="blue", mid="white", high="red", midpoint = 1, name = "Odds ratio") +
  xlab("Delta/CSP value for religious attendance") + ylab("Delta/CSP value for blood donation") +
  scale_y_continuous(breaks = c(-2, -1, 0), limits = c(-3, 1)) +
  scale_x_continuous(breaks = c(-2, -1, 0), limits = c(-3, 1)) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), axis.text = element_text(size = 12),
        legend.text = element_text(size = 10), legend.title = element_text(size = 12),
        axis.title = element_text(size = 14))
plot(p_heatmap_or)


## Next, a heatmap with the lower CIs of the odds ratio as the colour fill (to see when results cross the null)
p_heatmap_lci <- ggplot(data = tipping_outcomeExposureMNAR, aes(x = csp_exposure, y = csp_outcome, 
                                                                fill = lci_or_con)) +
  geom_tile() +
  geom_text(aes(label = round(lci_or_con, 2))) +
  scale_fill_gradient2(low="blue", mid="white", high="red", midpoint = 1, name = "Lower CI (OR)") +
  xlab("Delta/CSP value for religious attendance") + ylab("Delta/CSP value for blood donation") +
  scale_y_continuous(breaks = c(-2, -1, 0), limits = c(-3, 1)) +
  scale_x_continuous(breaks = c(-2, -1, 0), limits = c(-3, 1)) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), axis.text = element_text(size = 12),
        legend.text = element_text(size = 10), legend.title = element_text(size = 12),
        axis.title = element_text(size = 14))
plot(p_heatmap_lci)


### For next set of plots, will compare full range of blood donation results at different religious attendance CSPs

## Focus just on religious attendance CSPs of -3, -1 and 1
tipping_outcomeExposureMNAR_plot <- tipping_outcomeExposureMNAR %>%
  filter(csp_exposure == -2 | csp_exposure == -1 | csp_exposure == 0) %>%
  mutate(csp_exposure = as.factor(csp_exposure))
tipping_outcomeExposureMNAR_plot

## Start with using CSP
delta_csp_plot_outcomeExposureMNAR <- ggplot(data = tipping_outcomeExposureMNAR_plot, aes(x = csp_outcome)) +
  geom_point(aes(y = or_con), size = 2, colour = "blue") +
  geom_line(aes(y = or_con), color = "blue") +
  geom_line(aes(y = lci_or_con), linetype = "dashed", color = "green") +
  geom_line(aes(y = uci_or_con), linetype = "dashed", color = "red") +
  geom_vline(xintercept = 0, color = "red", size = 1) +
  geom_hline(yintercept = 1, color = "red", size = 1) +
  scale_x_continuous(breaks = c(-2, -1, 0)) +
  xlab("Delta/CSP value for blood donation") + ylab("Estimated religious attendance OR") +
  theme_bw() +
  facet_wrap(csp_exposure ~ ., ncol = 1, labeller = as_labeller(c("-2" = "Religious Attendance CSP = -2",
                                                                  "-1" = "Religious Attendance CSP = -1",
                                                                  "0" = "Religious Attendance CSP = 0"))) +
  theme(panel.grid.minor = element_blank(), axis.text = element_text(size = 12),
        axis.title = element_text(size = 14), strip.background = element_blank(),
        strip.text = element_text(size = 14))
plot(delta_csp_plot_outcomeExposureMNAR)


# MSP
delta_msp_plot_outcomeExposureMNAR <- ggplot(data = tipping_outcomeExposureMNAR_plot, aes(x = imor_outcome)) +
  geom_point(aes(y = or_con), size = 2, colour = "blue") +
  geom_line(aes(y = or_con), color = "blue") +
  geom_line(aes(y = lci_or_con), linetype = "dashed", color = "green") +
  geom_line(aes(y = uci_or_con), linetype = "dashed", color = "red") +
  geom_vline(xintercept = 1, color = "red", size = 1) +
  geom_hline(yintercept = 1, color = "red", size = 1) +
  xlab("IMOR value for blood donation") + ylab("Estimated religious attendance OR") +
  theme_bw() +
  scale_x_continuous(trans = "log", breaks = c(0.05, 0.1, 0.25, 0.5, 1, 2)) +
  facet_wrap(csp_exposure ~ ., ncol = 1, labeller = as_labeller(c("-2" = "Religious Attendance CSP = -2",
                                                                  "-1" = "Religious Attendance CSP = -1",
                                                                  "0" = "Religious Attendance CSP = 0"))) +
  theme(panel.grid.minor = element_blank(), axis.text = element_text(size = 12),
        axis.title = element_text(size = 14), strip.background = element_blank(),
        strip.text = element_text(size = 14))
plot(delta_msp_plot_outcomeExposureMNAR)


## Also plot the estimated prevalence of blood donation in the sample, as alternative X-axis - Have put reference line at the observed prevalence value of ~30%
delta_prev_plot_outcomeExposureMNAR <- ggplot(data = tipping_outcomeExposureMNAR_plot, aes(x = sampprev_outcome)) +
  geom_point(aes(y = or_con), size = 2, colour = "blue") +
  geom_line(aes(y = or_con), color = "blue") +
  geom_line(aes(y = lci_or_con), linetype = "dashed", color = "green") +
  geom_line(aes(y = uci_or_con), linetype = "dashed", color = "red") +
  geom_vline(xintercept = 0.3, color = "red", size = 1) +
  geom_hline(yintercept = 1, color = "red", size = 1) +
  xlab("Blood donation prevalence") + ylab("Estimated religious attendance OR") +
  theme_bw() +
  facet_wrap(csp_exposure ~ ., ncol = 1, labeller = as_labeller(c("-2" = "Religious Attendance CSP = -2",
                                                                  "-1" = "Religious Attendance CSP = -1",
                                                                  "0" = "Religious Attendance CSP = 0"))) +
  theme(panel.grid.minor = element_blank(), axis.text = element_text(size = 12),
        axis.title = element_text(size = 14), strip.background = element_blank(),
        strip.text = element_text(size = 14))
plot(delta_prev_plot_outcomeExposureMNAR)



##### As mentioned above, given that NARMICE is computationally intensive, I will run these analyses on the University of Bristol High Performance Computing cluster (see scripts in the 'forBluePebbleHPC' folder). I will read the results in below and create the summary plots. Will run the results without the missingness markers first, as when including the missingness markers get the implausible result that those with missing data are *more* likely to regularly attend religious services (see the scripts ending "_noM" in the 'forBluePebbleHPC' folder for more details).

### NARMICE analysis 1: Only religious attendance exposure MNAR
tipping_exposureMNAR <- read_csv("./Results/Mother_NARMICE/Results_mum_NARMICE_exposureMNAR_noM.csv")

tipping_exposureMNAR

## Plot the delta/CSP and IMOR (ignorable missingness odds ratio) and associated estimated difference in religious attendance odds ratio. Will just use the 'confounder only' adjustment scenario, as inclusion of potential confounders/mediators makes little difference to results.
delta_csp_plot_exposureMNAR <- ggplot(data = tipping_exposureMNAR, aes(x = csp)) +
  geom_point(aes(y = or_con), size = 2, colour = "blue") +
  geom_line(aes(y = or_con), color = "blue") +
  geom_line(aes(y = lci_or_con), linetype = "dashed", color = "green") +
  geom_line(aes(y = uci_or_con), linetype = "dashed", color = "red") +
  geom_vline(xintercept = 0, color = "red", size = 1) +
  geom_hline(yintercept = 1, color = "red", size = 1) +
  xlab("Conditional Sensitivity Parameter for Religious Attendance") + ylab("Religious Attendance Odds Ratio") +
  theme_bw()
plot(delta_csp_plot_exposureMNAR)

## Save this plot
pdf("./Results/MotherResults/mum_results_csp_exposureMNAR_plot.pdf", width = 8, height = 6)
delta_csp_plot_exposureMNAR
dev.off()


# MSP
delta_msp_plot_exposureMNAR <- ggplot(data = tipping_exposureMNAR, aes(x = imor)) +
  geom_point(aes(y = or_con), size = 2, colour = "blue") +
  geom_line(aes(y = or_con), color = "blue") +
  geom_line(aes(y = lci_or_con), linetype = "dashed", color = "green") +
  geom_line(aes(y = uci_or_con), linetype = "dashed", color = "red") +
  geom_vline(xintercept = 1, color = "red", size = 1) +
  geom_hline(yintercept = 1, color = "red", size = 1) +
  xlab("Ignorable Missingness Odds Ratio for Religious Attendance") + ylab("Religious Attendance Odds Ratio") +
  theme_bw() +
  scale_x_continuous(trans = "log", breaks = c(0.05, 0.1, 0.25, 0.5, 1, 2))
plot(delta_msp_plot_exposureMNAR)

## Save this plot
pdf("./Results/MotherResults/mum_results_msp_exposureMNAR_plot.pdf", width = 8, height = 6)
delta_msp_plot_exposureMNAR
dev.off()


## Also plot the estimated prevalence of religious attendance in the sample, as alternative X-axis - Have put reference line at the observed prevalence value of ~14%
delta_prev_plot_exposureMNAR <- ggplot(data = tipping_exposureMNAR, aes(x = sampprev)) +
  geom_point(aes(y = or_con), size = 2, colour = "blue") +
  geom_line(aes(y = or_con), color = "blue") +
  geom_line(aes(y = lci_or_con), linetype = "dashed", color = "green") +
  geom_line(aes(y = uci_or_con), linetype = "dashed", color = "red") +
  geom_vline(xintercept = 0.14, color = "red", size = 1) +
  geom_hline(yintercept = 1, color = "red", size = 1) +
  xlab("Religious Attendance Prevalence") + ylab("Religious Attendance Odds Ratio") +
  theme_bw()
plot(delta_prev_plot_exposureMNAR)

## Save this plot
pdf("./Results/MotherResults/mum_results_prev_exposureMNAR_plot.pdf", width = 8, height = 6)
delta_prev_plot_exposureMNAR
dev.off()



### NARMICE analysis 2: Only blood donation outcome MNAR
tipping_outcomeMNAR <- read_csv("./Results/Mother_NARMICE/Results_mum_NARMICE_outcomeMNAR_noM.csv")

tipping_outcomeMNAR

## Plot the delta/CSP and IMOR (ignorable missingness odds ratio) and associated estimated difference in religious attendance odds ratio. Will just use the 'confounder only' adjustment scenario, as inclusion of potential confounders/mediators makes little difference to results.
delta_csp_plot_outcomeMNAR <- ggplot(data = tipping_outcomeMNAR, aes(x = csp)) +
  geom_point(aes(y = or_con), size = 2, colour = "blue") +
  geom_line(aes(y = or_con), color = "blue") +
  geom_line(aes(y = lci_or_con), linetype = "dashed", color = "green") +
  geom_line(aes(y = uci_or_con), linetype = "dashed", color = "red") +
  geom_vline(xintercept = 0, color = "red", size = 1) +
  geom_hline(yintercept = 1, color = "red", size = 1) +
  xlab("Conditional Sensitivity Parameter for Blood Donation") + ylab("Religious Attendance Odds Ratio") +
  theme_bw()
plot(delta_csp_plot_outcomeMNAR)

## Save this plot
pdf("./Results/MotherResults/mum_results_csp_outcomeMNAR_plot.pdf", width = 8, height = 6)
delta_csp_plot_outcomeMNAR
dev.off()


# MSP
delta_msp_plot_outcomeMNAR <- ggplot(data = tipping_outcomeMNAR, aes(x = imor)) +
  geom_point(aes(y = or_con), size = 2, colour = "blue") +
  geom_line(aes(y = or_con), color = "blue") +
  geom_line(aes(y = lci_or_con), linetype = "dashed", color = "green") +
  geom_line(aes(y = uci_or_con), linetype = "dashed", color = "red") +
  geom_vline(xintercept = 1, color = "red", size = 1) +
  geom_hline(yintercept = 1, color = "red", size = 1) +
  xlab("Ignorable Missingness Odds Ratio for Blood Donation") + ylab("Religious Attendance Odds Ratio") +
  theme_bw() +
  scale_x_continuous(trans = "log", breaks = c(0.05, 0.1, 0.25, 0.5, 1, 2))
plot(delta_msp_plot_outcomeMNAR)

## Save this plot
pdf("./Results/MotherResults/mum_results_msp_outcomeMNAR_plot.pdf", width = 8, height = 6)
delta_msp_plot_outcomeMNAR
dev.off()


## Also plot the estimated prevalence of blood donation in the sample, as alternative X-axis - Have put reference line at the observed prevalence value of ~30%
delta_prev_plot_outcomeMNAR <- ggplot(data = tipping_outcomeMNAR, aes(x = sampprev)) +
  geom_point(aes(y = or_con), size = 2, colour = "blue") +
  geom_line(aes(y = or_con), color = "blue") +
  geom_line(aes(y = lci_or_con), linetype = "dashed", color = "green") +
  geom_line(aes(y = uci_or_con), linetype = "dashed", color = "red") +
  geom_vline(xintercept = 0.30, color = "red", size = 1) +
  geom_hline(yintercept = 1, color = "red", size = 1) +
  xlab("Blood Donation Prevalence") + ylab("Religious Attendance Odds Ratio") +
  theme_bw()
plot(delta_prev_plot_outcomeMNAR)

## Save this plot
pdf("./Results/MotherResults/mum_results_prev_outcomeMNAR_plot.pdf", width = 8, height = 6)
delta_prev_plot_outcomeMNAR
dev.off()



### NARMICE analysis 3: Both blood donation outcome and religious attendance exposure MNAR

## Need to combine the various output files together
tipping_outcomeExposureMNAR1 <- read_csv("./Results/Mother_NARMICE/Results_mum_NARMICE_outcomeExposureMNAR_noM_1.csv")
tipping_outcomeExposureMNAR2 <- read_csv("./Results/Mother_NARMICE/Results_mum_NARMICE_outcomeExposureMNAR_noM_2.csv")
tipping_outcomeExposureMNAR3 <- read_csv("./Results/Mother_NARMICE/Results_mum_NARMICE_outcomeExposureMNAR_noM_3.csv")
tipping_outcomeExposureMNAR4 <- read_csv("./Results/Mother_NARMICE/Results_mum_NARMICE_outcomeExposureMNAR_noM_4.csv")
tipping_outcomeExposureMNAR5 <- read_csv("./Results/Mother_NARMICE/Results_mum_NARMICE_outcomeExposureMNAR_noM_5.csv")
tipping_outcomeExposureMNAR6 <- read_csv("./Results/Mother_NARMICE/Results_mum_NARMICE_outcomeExposureMNAR_noM_6.csv")
tipping_outcomeExposureMNAR7 <- read_csv("./Results/Mother_NARMICE/Results_mum_NARMICE_outcomeExposureMNAR_noM_7.csv")
tipping_outcomeExposureMNAR8 <- read_csv("./Results/Mother_NARMICE/Results_mum_NARMICE_outcomeExposureMNAR_noM_8.csv")
tipping_outcomeExposureMNAR9 <- read_csv("./Results/Mother_NARMICE/Results_mum_NARMICE_outcomeExposureMNAR_noM_9.csv")

tipping_outcomeExposureMNAR <- bind_rows(tipping_outcomeExposureMNAR1, tipping_outcomeExposureMNAR2,
                                         tipping_outcomeExposureMNAR3, tipping_outcomeExposureMNAR4,
                                         tipping_outcomeExposureMNAR5, tipping_outcomeExposureMNAR6,
                                         tipping_outcomeExposureMNAR7, tipping_outcomeExposureMNAR8,
                                         tipping_outcomeExposureMNAR9)
tipping_outcomeExposureMNAR

write_csv(tipping_outcomeExposureMNAR, file = "./Results/Mother_NARMICE/Results_mum_NARMICE_outcomeExposureMNAR_noM_combined.csv")


### Plot the delta/CSP and IMOR (ignorable missingness odds ratio) and associated estimated difference in religious attendance odds ratio. Will just use the 'confounder only' adjustment scenario, as inclusion of potential confounders/mediators makes little difference to results.

### As lots of results here, will create two kinds of plots - First a heat-map of all the results, and second a facet plot with the outcome MNAR results split by different levels of exposure MNAR

## First, a heatmap with the odds ratio as the colour fill
p_heatmap_or <- ggplot(data = tipping_outcomeExposureMNAR, aes(x = csp_exposure, y = csp_outcome, fill = or_con)) +
  geom_tile() +
  geom_text(aes(label = round(or_con, 2))) +
  scale_fill_gradient2(low="blue", mid="white", high="red", midpoint = 1, name = "Odds ratio") +
  xlab("Conditional Sensitivity Parameter for Religious Attendance") + 
  ylab("Conditional Sensitivity Parameter for Blood Donation") +
  scale_y_continuous(breaks = c(-2, -1.5, -1, -0.5, 0), limits = c(-2.2, 0.2)) +
  scale_x_continuous(breaks = c(-2, -1.5, -1, -0.5, 0), limits = c(-2.2, 0.2)) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), axis.text = element_text(size = 12),
        legend.text = element_text(size = 10), legend.title = element_text(size = 12),
        axis.title = element_text(size = 14))
plot(p_heatmap_or)

## Save this plot
pdf("./Results/MotherResults/mum_results_outcomeExposureMNAR_ORHeatmap.pdf", width = 8, height = 6)
p_heatmap_or
dev.off()


## Next, a heatmap with the lower CIs of the odds ratio as the colour fill (to see when results cross the null)
p_heatmap_lci <- ggplot(data = tipping_outcomeExposureMNAR, aes(x = csp_exposure, y = csp_outcome, 
                                                                fill = lci_or_con)) +
  geom_tile() +
  geom_text(aes(label = round(lci_or_con, 2))) +
  scale_fill_gradient2(low="blue", mid="white", high="red", midpoint = 1, name = "Lower CI (OR)") +
  xlab("Conditional Sensitivity Parameter for Religious Attendance") + 
  ylab("Conditional Sensitivity Parameter for Blood Donation") +
  scale_y_continuous(breaks = c(-2, -1.5, -1, -0.5, 0), limits = c(-2.2, 0.2)) +
  scale_x_continuous(breaks = c(-2, -1.5, -1, -0.5, 0), limits = c(-2.2, 0.2)) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), axis.text = element_text(size = 12),
        legend.text = element_text(size = 10), legend.title = element_text(size = 12),
        axis.title = element_text(size = 14))
plot(p_heatmap_lci)

## Save this plot
pdf("./Results/MotherResults/mum_results_outcomeExposureMNAR_LCIORHeatmap.pdf", width = 8, height = 6)
p_heatmap_lci
dev.off()


### For next set of plots, will compare full range of blood donation results at different religious attendance CSPs

## Focus just on religious attendance CSPs of -2, -1 and 0
tipping_outcomeExposureMNAR_plot <- tipping_outcomeExposureMNAR %>%
  filter(csp_exposure == -2 | csp_exposure == -1 | csp_exposure == 0) %>%
  mutate(csp_exposure = as.factor(csp_exposure))
tipping_outcomeExposureMNAR_plot

## Start with using CSP
delta_csp_plot_outcomeExposureMNAR <- ggplot(data = tipping_outcomeExposureMNAR_plot, aes(x = csp_outcome)) +
  geom_point(aes(y = or_con), size = 2, colour = "blue") +
  geom_line(aes(y = or_con), color = "blue") +
  geom_line(aes(y = lci_or_con), linetype = "dashed", color = "green") +
  geom_line(aes(y = uci_or_con), linetype = "dashed", color = "red") +
  geom_vline(xintercept = 0, color = "red", size = 1) +
  geom_hline(yintercept = 1, color = "red", size = 1) +
  scale_x_continuous(breaks = c(-2, -1.5, -1, -0.5, 0)) +
  xlab("Conditional Sensitivity Parameter for Blood Donation") + ylab("Religious Attendance Odds Ratio") +
  theme_bw() +
  facet_wrap(csp_exposure ~ ., ncol = 1, labeller = as_labeller(c("-2" = "Religious Attendance Conditional Sensitivity Parameter = -2",
                                                                  "-1" = "Religious Attendance Conditional Sensitivity Parameter = -1",
                                                                  "0" = "Religious Attendance Conditional Sensitivity Parameter = 0"))) +
  theme(panel.grid.minor = element_blank(), axis.text = element_text(size = 12),
        axis.title = element_text(size = 14), strip.background = element_blank(),
        strip.text = element_text(size = 14))
plot(delta_csp_plot_outcomeExposureMNAR)

## Save this plot
pdf("./Results/MotherResults/mum_results_csp_outcomeExposureMNAR.pdf", width = 8, height = 8)
delta_csp_plot_outcomeExposureMNAR
dev.off()


# MSP
delta_msp_plot_outcomeExposureMNAR <- ggplot(data = tipping_outcomeExposureMNAR_plot, aes(x = imor_outcome)) +
  geom_point(aes(y = or_con), size = 2, colour = "blue") +
  geom_line(aes(y = or_con), color = "blue") +
  geom_line(aes(y = lci_or_con), linetype = "dashed", color = "green") +
  geom_line(aes(y = uci_or_con), linetype = "dashed", color = "red") +
  geom_vline(xintercept = 1, color = "red", size = 1) +
  geom_hline(yintercept = 1, color = "red", size = 1) +
  xlab("Ignorable Missingness Odds Ratio for Blood Donation") + ylab("Religious Attendance Odds Ratio") +
  theme_bw() +
  scale_x_continuous(trans = "log", breaks = c(0.05, 0.1, 0.25, 0.5, 1, 2)) +
  facet_wrap(csp_exposure ~ ., ncol = 1, labeller = as_labeller(c("-2" = "Religious Attendance Conditional Sensitivity Parameter = -2",
                                                                  "-1" = "Religious Attendance Conditional Sensitivity Parameter = -1",
                                                                  "0" = "Religious Attendance Conditional Sensitivity Parameter = 0"))) +
  theme(panel.grid.minor = element_blank(), axis.text = element_text(size = 12),
        axis.title = element_text(size = 14), strip.background = element_blank(),
        strip.text = element_text(size = 14))
plot(delta_msp_plot_outcomeExposureMNAR)

## Save this plot
pdf("./Results/MotherResults/mum_results_msp_outcomeExposureMNAR.pdf", width = 8, height = 8)
delta_msp_plot_outcomeExposureMNAR
dev.off()


## Also plot the estimated prevalence of blood donation in the sample, as alternative X-axis - Have put reference line at the observed prevalence value of ~30%
delta_prev_plot_outcomeExposureMNAR <- ggplot(data = tipping_outcomeExposureMNAR_plot, aes(x = sampprev_outcome)) +
  geom_point(aes(y = or_con), size = 2, colour = "blue") +
  geom_line(aes(y = or_con), color = "blue") +
  geom_line(aes(y = lci_or_con), linetype = "dashed", color = "green") +
  geom_line(aes(y = uci_or_con), linetype = "dashed", color = "red") +
  geom_vline(xintercept = 0.3, color = "red", size = 1) +
  geom_hline(yintercept = 1, color = "red", size = 1) +
  xlab("Blood Donation Prevalence") + ylab("Religious Attendance Odds Ratio") +
  theme_bw() +
  facet_wrap(csp_exposure ~ ., ncol = 1, labeller = as_labeller(c("-2" = "Religious Attendance Conditional Sensitivity Parameter = -2",
                                                                  "-1" = "Religious Attendance Conditional Sensitivity Parameter = -1",
                                                                  "0" = "Religious Attendance Conditional Sensitivity Parameter = 0"))) +
  theme(panel.grid.minor = element_blank(), axis.text = element_text(size = 12),
        axis.title = element_text(size = 14), strip.background = element_blank(),
        strip.text = element_text(size = 14))
plot(delta_prev_plot_outcomeExposureMNAR)

## Save this plot
pdf("./Results/MotherResults/mum_results_prev_outcomeExposureMNAR.pdf", width = 8, height = 8)
delta_prev_plot_outcomeExposureMNAR
dev.off()



########################################################################################################
##### During testing, noticed that in this NARMICE analysis when no sensitivity parameter, the MSP/IMOR for religious attendance was positive, indicating that the imputation model 'thought' that those with missing religious attendance data were *more* likely to be religious. This is unlikely, and differs from the standard MI analysis, where those with missing data were slightly less likely to have attended church regularly. This was also found in the partner's NARMICE analysis, although to a less extent in the mothers compared to partners. This strange pattern of results seems to be due to the missingness indicators being included in the imputation model, as in standard MI - and in NARMICE when not including these missingness indicators - the imputed values are in the expected direction. To assess how these different assumptions regarding the imputation model affect results, I have also run these NARMICE models including these missingness indicators (see the scripts which do not end "_noM" in the 'forBluePebbleHPC' folder for more details) - The results will be plotted below.


### NARMICE analysis 1: Only religious attendance exposure MNAR
tipping_exposureMNAR_M <- read_csv("./Results/Mother_NARMICE/Results_mum_NARMICE_exposureMNAR.csv")

tipping_exposureMNAR_M


## Plot the delta/CSP and IMOR (ignorable missingness odds ratio) and associated estimated difference in religious attendance odds ratio. Will just use the 'confounder only' adjustment scenario, as inclusion of potential confounders/mediators makes little difference to results.
delta_csp_plot_exposureMNAR_M <- ggplot(data = tipping_exposureMNAR_M, aes(x = csp)) +
  geom_point(aes(y = or_con), size = 2, colour = "blue") +
  geom_line(aes(y = or_con), color = "blue") +
  geom_line(aes(y = lci_or_con), linetype = "dashed", color = "green") +
  geom_line(aes(y = uci_or_con), linetype = "dashed", color = "red") +
  geom_vline(xintercept = 0, color = "red", size = 1) +
  geom_hline(yintercept = 1, color = "red", size = 1) +
  xlab("Conditional Sensitivity Parameter for Religious Attendance") + ylab("Religious Attendance Odds Ratio") +
  theme_bw()
plot(delta_csp_plot_exposureMNAR_M)

## Save this plot
pdf("./Results/MotherResults/mum_results_csp_exposureMNAR_plot_M.pdf", width = 8, height = 6)
delta_csp_plot_exposureMNAR_M
dev.off()


# MSP
delta_msp_plot_exposureMNAR_M <- ggplot(data = tipping_exposureMNAR_M, aes(x = imor)) +
  geom_point(aes(y = or_con), size = 2, colour = "blue") +
  geom_line(aes(y = or_con), color = "blue") +
  geom_line(aes(y = lci_or_con), linetype = "dashed", color = "green") +
  geom_line(aes(y = uci_or_con), linetype = "dashed", color = "red") +
  geom_vline(xintercept = 1, color = "red", size = 1) +
  geom_hline(yintercept = 1, color = "red", size = 1) +
  xlab("Ignorable Missingness Odds Ratio for Religious Attendance") + ylab("Religious Attendance Odds Ratio") +
  theme_bw() +
  scale_x_continuous(trans = "log", breaks = c(0.05, 0.1, 0.25, 0.5, 1, 2))
plot(delta_msp_plot_exposureMNAR_M)

## Save this plot
pdf("./Results/MotherResults/mum_results_msp_exposureMNAR_plot_M.pdf", width = 8, height = 6)
delta_msp_plot_exposureMNAR_M
dev.off()


## Also plot the estimated prevalence of religious attendance in the sample, as alternative X-axis - Have put reference line at the observed prevalence value of ~14%
delta_prev_plot_exposureMNAR_M <- ggplot(data = tipping_exposureMNAR_M, aes(x = sampprev)) +
  geom_point(aes(y = or_con), size = 2, colour = "blue") +
  geom_line(aes(y = or_con), color = "blue") +
  geom_line(aes(y = lci_or_con), linetype = "dashed", color = "green") +
  geom_line(aes(y = uci_or_con), linetype = "dashed", color = "red") +
  geom_vline(xintercept = 0.14, color = "red", size = 1) +
  geom_hline(yintercept = 1, color = "red", size = 1) +
  xlab("Religious Attendance Prevalence") + ylab("Religious Attendance Odds Ratio") +
  theme_bw()
plot(delta_prev_plot_exposureMNAR_M)

## Save this plot
pdf("./Results/MotherResults/mum_results_prev_exposureMNAR_plot_M.pdf", width = 8, height = 6)
delta_prev_plot_exposureMNAR_M
dev.off()



### NARMICE analysis 2: Only blood donation outcome MNAR
tipping_outcomeMNAR_M <- read_csv("./Results/Mother_NARMICE/Results_mum_NARMICE_outcomeMNAR.csv")

tipping_outcomeMNAR_M


## Plot the delta/CSP and IMOR (ignorable missingness odds ratio) and associated estimated difference in religious attendance odds ratio. Will just use the 'confounder only' adjustment scenario, as inclusion of potential confounders/mediators makes little difference to results.
delta_csp_plot_outcomeMNAR_M <- ggplot(data = tipping_outcomeMNAR_M, aes(x = csp)) +
  geom_point(aes(y = or_con), size = 2, colour = "blue") +
  geom_line(aes(y = or_con), color = "blue") +
  geom_line(aes(y = lci_or_con), linetype = "dashed", color = "green") +
  geom_line(aes(y = uci_or_con), linetype = "dashed", color = "red") +
  geom_vline(xintercept = 0, color = "red", size = 1) +
  geom_hline(yintercept = 1, color = "red", size = 1) +
  xlab("Conditional Sensitivity Parameter for Blood Donation") + ylab("Religious Attendance Odds Ratio") +
  theme_bw()
plot(delta_csp_plot_outcomeMNAR_M)

## Save this plot
pdf("./Results/MotherResults/mum_results_csp_outcomeMNAR_plot_M.pdf", width = 8, height = 6)
delta_csp_plot_outcomeMNAR_M
dev.off()


# MSP
delta_msp_plot_outcomeMNAR_M <- ggplot(data = tipping_outcomeMNAR_M, aes(x = imor)) +
  geom_point(aes(y = or_con), size = 2, colour = "blue") +
  geom_line(aes(y = or_con), color = "blue") +
  geom_line(aes(y = lci_or_con), linetype = "dashed", color = "green") +
  geom_line(aes(y = uci_or_con), linetype = "dashed", color = "red") +
  geom_vline(xintercept = 1, color = "red", size = 1) +
  geom_hline(yintercept = 1, color = "red", size = 1) +
  xlab("Ignorable Missingness Odds Ratio for Blood Donation") + ylab("Religious Attendance Odds Ratio") +
  theme_bw() +
  scale_x_continuous(trans = "log", breaks = c(0.05, 0.1, 0.25, 0.5, 1, 2))
plot(delta_msp_plot_outcomeMNAR_M)

## Save this plot
pdf("./Results/MotherResults/mum_results_msp_outcomeMNAR_plot_M.pdf", width = 8, height = 6)
delta_msp_plot_outcomeMNAR_M
dev.off()


## Also plot the estimated prevalence of blood donation in the sample, as alternative X-axis - Have put reference line at the observed prevalence value of ~30%
delta_prev_plot_outcomeMNAR_M <- ggplot(data = tipping_outcomeMNAR_M, aes(x = sampprev)) +
  geom_point(aes(y = or_con), size = 2, colour = "blue") +
  geom_line(aes(y = or_con), color = "blue") +
  geom_line(aes(y = lci_or_con), linetype = "dashed", color = "green") +
  geom_line(aes(y = uci_or_con), linetype = "dashed", color = "red") +
  geom_vline(xintercept = 0.30, color = "red", size = 1) +
  geom_hline(yintercept = 1, color = "red", size = 1) +
  xlab("Blood Donation Prevalence") + ylab("Religious Attendance Odds Ratio") +
  theme_bw()
plot(delta_prev_plot_outcomeMNAR_M)

## Save this plot
pdf("./Results/MotherResults/mum_results_prev_outcomeMNAR_plot_M.pdf", width = 8, height = 6)
delta_prev_plot_outcomeMNAR_M
dev.off()



### NARMICE analysis 3: Both blood donation outcome and religious attendance exposure MNAR

## Need to combine the various output files together
tipping_outcomeExposureMNAR_M_1 <- read_csv("./Results/Mother_NARMICE/Results_mum_NARMICE_outcomeExposureMNAR_1.csv")
tipping_outcomeExposureMNAR_M_2 <- read_csv("./Results/Mother_NARMICE/Results_mum_NARMICE_outcomeExposureMNAR_2.csv")
tipping_outcomeExposureMNAR_M_3 <- read_csv("./Results/Mother_NARMICE/Results_mum_NARMICE_outcomeExposureMNAR_3.csv")
tipping_outcomeExposureMNAR_M_4 <- read_csv("./Results/Mother_NARMICE/Results_mum_NARMICE_outcomeExposureMNAR_4.csv")
tipping_outcomeExposureMNAR_M_5 <- read_csv("./Results/Mother_NARMICE/Results_mum_NARMICE_outcomeExposureMNAR_5.csv")
tipping_outcomeExposureMNAR_M_6 <- read_csv("./Results/Mother_NARMICE/Results_mum_NARMICE_outcomeExposureMNAR_6.csv")
tipping_outcomeExposureMNAR_M_7 <- read_csv("./Results/Mother_NARMICE/Results_mum_NARMICE_outcomeExposureMNAR_7.csv")
tipping_outcomeExposureMNAR_M_8 <- read_csv("./Results/Mother_NARMICE/Results_mum_NARMICE_outcomeExposureMNAR_8.csv")
tipping_outcomeExposureMNAR_M_9 <- read_csv("./Results/Mother_NARMICE/Results_mum_NARMICE_outcomeExposureMNAR_9.csv")

tipping_outcomeExposureMNAR_M <- bind_rows(tipping_outcomeExposureMNAR_M_1, tipping_outcomeExposureMNAR_M_2,
                                         tipping_outcomeExposureMNAR_M_3, tipping_outcomeExposureMNAR_M_4,
                                         tipping_outcomeExposureMNAR_M_5, tipping_outcomeExposureMNAR_M_6,
                                         tipping_outcomeExposureMNAR_M_7, tipping_outcomeExposureMNAR_M_8,
                                         tipping_outcomeExposureMNAR_M_9)
tipping_outcomeExposureMNAR

write_csv(tipping_outcomeExposureMNAR_M, file = "./Results/Mother_NARMICE/Results_mum_NARMICE_outcomeExposureMNAR_M_combined.csv")


### Plot the delta/CSP and IMOR (ignorable missingness odds ratio) and associated estimated difference in religious attendance odds ratio. Will just use the 'confounder only' adjustment scenario, as inclusion of potential confounders/mediators makes little difference to results.

### As lots of results here, will create two kinds of plots - First a heat-map of all the results, and second a facet plot with the outcome MNAR results split by different levels of exposure MNAR

## First, a heatmap with the odds ratio as the colour fill
p_heatmap_or_M <- ggplot(data = tipping_outcomeExposureMNAR_M, aes(x = csp_exposure, y = csp_outcome, 
                                                                       fill = or_con)) +
  geom_tile() +
  geom_text(aes(label = round(or_con, 2))) +
  scale_fill_gradient2(low="blue", mid="white", high="red", midpoint = 1, name = "Odds ratio") +
  xlab("Conditional Sensitivity Parameter for Religious Attendance") + 
  ylab("Conditional Sensitivity Parameter for Blood Donation") +
  scale_y_continuous(breaks = c(-2, -1.5, -1, -0.5, 0), limits = c(-2.2, 0.2)) +
  scale_x_continuous(breaks = c(-2, -1.5, -1, -0.5, 0), limits = c(-2.2, 0.2)) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), axis.text = element_text(size = 12),
        legend.text = element_text(size = 10), legend.title = element_text(size = 12),
        axis.title = element_text(size = 14))
plot(p_heatmap_or_M)

## Save this plot
pdf("./Results/MotherResults/mum_results_outcomeExposureMNAR_ORHeatmap_M.pdf", width = 8, height = 6)
p_heatmap_or_M
dev.off()


## Next, a heatmap with the lower CIs of the odds ratio as the colour fill (to see when results cross the null)
p_heatmap_lci_M <- ggplot(data = tipping_outcomeExposureMNAR_M, aes(x = csp_exposure, y = csp_outcome, 
                                                                fill = lci_or_con)) +
  geom_tile() +
  geom_text(aes(label = round(lci_or_con, 2))) +
  scale_fill_gradient2(low="blue", mid="white", high="red", midpoint = 1, name = "Lower CI (OR)") +
  xlab("Conditional Sensitivity Parameter for Religious Attendance") + 
  ylab("Conditional Sensitivity Parameter for Blood Donation") +
  scale_y_continuous(breaks = c(-2, -1.5, -1, -0.5, 0), limits = c(-2.2, 0.2)) +
  scale_x_continuous(breaks = c(-2, -1.5, -1, -0.5, 0), limits = c(-2.2, 0.2)) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), axis.text = element_text(size = 12),
        legend.text = element_text(size = 10), legend.title = element_text(size = 12),
        axis.title = element_text(size = 14))
plot(p_heatmap_lci_M)

## Save this plot
pdf("./Results/MotherResults/mum_results_outcomeExposureMNAR_LCIORHeatmap_M.pdf", width = 8, height = 6)
p_heatmap_lci_M
dev.off()


### For next set of plots, will compare full range of blood donation results at different religious attendance CSPs

## Focus just on religious attendance CSPs of -2, -1 and 0
tipping_outcomeExposureMNAR_plot_M <- tipping_outcomeExposureMNAR_M %>%
  filter(csp_exposure == -2 | csp_exposure == -1 | csp_exposure == 0) %>%
  mutate(csp_exposure = as.factor(csp_exposure))
tipping_outcomeExposureMNAR_plot_M

## Start with using CSP
delta_csp_plot_outcomeExposureMNAR_M <- ggplot(data = tipping_outcomeExposureMNAR_plot_M, aes(x = csp_outcome)) +
  geom_point(aes(y = or_con), size = 2, colour = "blue") +
  geom_line(aes(y = or_con), color = "blue") +
  geom_line(aes(y = lci_or_con), linetype = "dashed", color = "green") +
  geom_line(aes(y = uci_or_con), linetype = "dashed", color = "red") +
  geom_vline(xintercept = 0, color = "red", size = 1) +
  geom_hline(yintercept = 1, color = "red", size = 1) +
  scale_x_continuous(breaks = c(-2, -1.5, -1, -0.5, 0)) +
  xlab("Conditional Sensitivity Parameter for Blood Donation") + ylab("Religious Attendance Odds Ratio") +
  theme_bw() +
  facet_wrap(csp_exposure ~ ., ncol = 1, labeller = as_labeller(c("-2" = "Religious Attendance Conditional Sensitivity Parameter = -2",
                                                                  "-1" = "Religious Attendance Conditional Sensitivity Parameter = -1",
                                                                  "0" = "Religious Attendance Conditional Sensitivity Parameter = 0"))) +
  theme(panel.grid.minor = element_blank(), axis.text = element_text(size = 12),
        axis.title = element_text(size = 14), strip.background = element_blank(),
        strip.text = element_text(size = 14))
plot(delta_csp_plot_outcomeExposureMNAR_M)

## Save this plot
pdf("./Results/MotherResults/mum_results_csp_outcomeExposureMNAR_M.pdf", width = 8, height = 8)
delta_csp_plot_outcomeExposureMNAR_M
dev.off()


# MSP
delta_msp_plot_outcomeExposureMNAR_M <- ggplot(data = tipping_outcomeExposureMNAR_plot_M, aes(x = imor_outcome)) +
  geom_point(aes(y = or_con), size = 2, colour = "blue") +
  geom_line(aes(y = or_con), color = "blue") +
  geom_line(aes(y = lci_or_con), linetype = "dashed", color = "green") +
  geom_line(aes(y = uci_or_con), linetype = "dashed", color = "red") +
  geom_vline(xintercept = 1, color = "red", size = 1) +
  geom_hline(yintercept = 1, color = "red", size = 1) +
  xlab("Ignorable Missingness Odds Ratio value for Blood Donation") + ylab("Religious Attendance Odds Ratio") +
  theme_bw() +
  scale_x_continuous(trans = "log", breaks = c(0.05, 0.1, 0.25, 0.5, 1, 2)) +
  facet_wrap(csp_exposure ~ ., ncol = 1, labeller = as_labeller(c("-2" = "Religious Attendance Conditional Sensitivity Parameter = -2",
                                                                  "-1" = "Religious Attendance Conditional Sensitivity Parameter = -1",
                                                                  "0" = "Religious Attendance Conditional Sensitivity Parameter = 0"))) +
  theme(panel.grid.minor = element_blank(), axis.text = element_text(size = 12),
        axis.title = element_text(size = 14), strip.background = element_blank(),
        strip.text = element_text(size = 14))
plot(delta_msp_plot_outcomeExposureMNAR_M)

## Save this plot
pdf("./Results/MotherResults/mum_results_msp_outcomeExposureMNAR_M.pdf", width = 8, height = 8)
delta_msp_plot_outcomeExposureMNAR_M
dev.off()



## Also plot the estimated prevalence of blood donation in the sample, as alternative X-axis - Have put reference line at the observed prevalence value of ~30%
delta_prev_plot_outcomeExposureMNAR_M <- ggplot(data = tipping_outcomeExposureMNAR_plot_M, 
                                                  aes(x = sampprev_outcome)) +
  geom_point(aes(y = or_con), size = 2, colour = "blue") +
  geom_line(aes(y = or_con), color = "blue") +
  geom_line(aes(y = lci_or_con), linetype = "dashed", color = "green") +
  geom_line(aes(y = uci_or_con), linetype = "dashed", color = "red") +
  geom_vline(xintercept = 0.3, color = "red", size = 1) +
  geom_hline(yintercept = 1, color = "red", size = 1) +
  xlab("Blood Donation Prevalence") + ylab("Religious Attendance Odds Ratio") +
  theme_bw() +
  facet_wrap(csp_exposure ~ ., ncol = 1, labeller = as_labeller(c("-2" = "Religious Attendance Conditional Sensitivity Parameter = -2",
                                                                  "-1" = "Religious Attendance Conditional Sensitivity Parameter = -1",
                                                                  "0" = "Religious Attendance Conditional Sensitivity Parameter = 0"))) +
  theme(panel.grid.minor = element_blank(), axis.text = element_text(size = 12),
        axis.title = element_text(size = 14), strip.background = element_blank(),
        strip.text = element_text(size = 14))
plot(delta_prev_plot_outcomeExposureMNAR_M)

## Save this plot
pdf("./Results/MotherResults/mum_results_prev_outcomeExposureMNAR_M.pdf", width = 8, height = 8)
delta_prev_plot_outcomeExposureMNAR_M
dev.off()


