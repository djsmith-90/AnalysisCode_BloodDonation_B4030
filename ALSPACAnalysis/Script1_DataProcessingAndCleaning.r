### Script for paper 'Exploring causality from observational data: An example assessing whether religiosity promotes cooperation' using ALSPAC data (ALSPAC B-number B4030)
### Script 1: Data preparation/cleaning
### Created 16/2/2023 by Dan Major-Smith
### R version 4.0.4

## Analysis plan for this paper has been accepted as a Registered Report for the journal Evolutionary Human Sciences, and the plan is available on the OSF: https://osf.io/z5gcm/


###########################################################################################
#### Clear workspace, install/load packages, and set working directory
rm(list = ls())

setwd("X:\\Studies\\RSBB Team\\Dan\\B4030 - RSBB and cooperation")

#install.packages("tidyverse")
library(tidyverse)

#install.packages("haven")
library(haven)


###########################################################################################
#### Read in the raw data, and start processing/cleaning the data

data_raw <- read_csv("RSBB_BloodDonation_B4030.csv")

## Quick check of data
head(data_raw)
glimpse(data_raw)



#### Start by processing the mother's data
data_mum <- data_raw


### Removing some observations 

## Want to drop data from one mother if linked to two pregnancies (else data possibly repeated)
table(data_mum$mz005l, useNA = "ifany")

data_mum <- data_mum %>%
  filter(mz005l != "Yes, drop these mult mums")

## Drop data if mother withdrew consent for data to be used
table(data_mum$d810, useNA = "ifany")

data_mum <- data_mum %>%
  filter(d810 != ".a" | is.na(d810))

## Drop if pregnancy not result in a live birth
table(data_mum$mz012, useNA = "ifany")

data_mum <- data_mum %>%
  filter(mz012 == "All survived")

## Also drop data if not enrolled in ALSPAC during pregnancy
table(data_mum$mz028b, useNA = "ifany")

data_mum <- data_mum %>%
  filter(mz028b != "Not in core sample")

## Also drop mother data if younger than 17 when becoming pregnant, as cannot give blood before this age (n = 146)
table(data_mum$mz028a, useNA = "ifany")

data_mum <- data_mum %>%
  filter(mz028a != "< 16" & mz028a != "16")

## Finally, remove mothers with Rastafarian or Jehovah's Witness religious affiliation, as may have proscriptions against blood donation (numbers very small; n = 59)
table(data_mum$d813, useNA = "ifany")

data_mum <- data_mum %>%
  filter((d813 != "Jehovah Witness" & d813 != "Rastafarian") | is.na(d813))


### Keep just the variables of interest and re-order
data_mum <- data_mum %>%
  relocate(aln, d810, d813, d816, d290, mz028b, c800, c645a, a006, jan1993imd2010q5_M, jan1993ur01ind_M, a525, b032,
          d842, b594, c710, c711, c712, c713, b040, d992, b371, d990, b650, b665, b667, b720, b722, c755, a053) %>%
  select(aln:a053)

colnames(data_mum)



#### Now process the data and prep variables for analysis

### Start with exposures (religious belief, affiliation and attendance)

## Religious belief (Yes vs No/Not sure)
table(data_mum$d810, useNA = "ifany")

# If missing, code as NA, combine 'no' and 'not sure' together, then convert to factor and order levels
data_mum <- data_mum %>%
  mutate(d810 = na_if(d810, "-1")) %>%
  mutate(d810 = ifelse(d810 == "Not sure", "No", d810)) %>%
  mutate(d810 = factor(d810, levels = c("No", "Yes"))) %>%
  rename(mum_belief = d810)

table(data_mum$mum_belief, useNA = "ifany")
round(prop.table(table(data_mum$mum_belief)) * 100, 1)
sum(table(data_mum$mum_belief))


## Religious affiliation/identity (Yes vs None)
table(data_mum$d813, useNA = "ifany")

# If missing, code as NA, combine all religious affiliations together, then convert to factor and order levels
data_mum <- data_mum %>%
  mutate(d813 = na_if(d813, "-1")) %>%
  mutate(d813 = ifelse(is.na(d813), NA,
                       ifelse(d813 == "None", "None", "Religious"))) %>%
  mutate(d813 = factor(d813, levels = c("None", "Religious"))) %>%
  rename(mum_identity = d813)

table(data_mum$mum_identity, useNA = "ifany")
round(prop.table(table(data_mum$mum_identity)) * 100, 1)
sum(table(data_mum$mum_identity))


## Religious attendance (regular attendance vs Occasional/non-attendance)
table(data_mum$d816, useNA = "ifany")

# If missing, code as NA, then convert to factor and order levels
data_mum <- data_mum %>%
  mutate(d816 = na_if(d816, "-1")) %>%
  mutate(d816 = recode(d816, "MIN 1 a MTH" = "Regular", "MIN 1 a WK" = "Regular", 
                           "MIN 1 a YR" = "Occasional/None", "Not at all" = "Occasional/None")) %>%
  mutate(d816 = factor(d816, levels = c("Occasional/None", "Regular"))) %>%
  rename(mum_attend = d816)

table(data_mum$mum_attend, useNA = "ifany")
round(prop.table(table(data_mum$mum_attend)) * 100, 1)
sum(table(data_mum$mum_attend))


### Now to outcome (blood donation; Yes vs No)
table(data_mum$d290, useNA = "ifany")

# If missing, code as NA, then convert to factor and order levels
data_mum <- data_mum %>%
  mutate(d290 = na_if(d290, "-1")) %>%
  mutate(d290 = factor(d290, levels = c("No", "Yes"))) %>%
  rename(mum_donate = d290)

table(data_mum$mum_donate, useNA = "ifany")
round(prop.table(table(data_mum$mum_donate)) * 100, 1)
sum(table(data_mum$mum_donate))


### Next to potential confounders

## Maternal age (at birth; years)
table(data_mum$mz028b, useNA = "ifany")

# If missing, code as NA, then convert to numeric
data_mum <- data_mum %>%
  mutate(mz028b = na_if(mz028b, "Miscarried")) %>%
  mutate(mz028b = na_if(mz028b, "Outcome NK")) %>%
  mutate(mz028b = na_if(mz028b, "Triplet / quadruplet")) %>%
  mutate(mz028b = recode(mz028b, "< 16" = "16", ">43" = "43")) %>%
  mutate(mz028b = as.numeric(mz028b)) %>%
  rename(mum_age = mz028b)

table(data_mum$mum_age, useNA = "ifany")
summary(data_mum$mum_age)


# Mother's ethnicity (White vs other than White)
table(data_mum$c800, useNA = "ifany")

data_mum <- data_mum %>%
  mutate(c800 = na_if(c800, "Missing")) %>%
  mutate(c800 = recode(c800, "Bangladeshi" = "Other than White", "Black African" = "Other than White", 
                       "Black Caribbean" = "Other than White", "Chinese" = "Other than White", 
                       "Indian" = "Other than White", "Other" = "Other than White", 
                       "Other black" = "Other than White", "Pakistani" = "Other than White")) %>%
  mutate(c800 = factor(c800, levels = c("White", "Other than White"))) %>%
  rename(mum_ethnicity = c800)

table(data_mum$mum_ethnicity, useNA = "ifany")


# Mother's highest education qualification (CSE/None vs Vocational vs O-level vs A-level vs Degree)
table(data_mum$c645a, useNA = "ifany")

data_mum <- data_mum %>%
  mutate(c645a = na_if(c645a, "Missing")) %>%
  mutate(c645a = recode(c645a, "CSE" = "CSE/None")) %>%
  mutate(c645a = factor(c645a, levels = c("CSE/None", "Vocational", "O level", "A level", "Degree"))) %>%
  rename(mum_edu = c645a)

table(data_mum$mum_edu, useNA = "ifany")


# Home ownership status (owned/mortgaged vs rented vs Council/housing association vs other)
table(data_mum$a006, useNA = "ifany")

data_mum <- data_mum %>%
  mutate(a006 = na_if(a006, "Missing")) %>%
  mutate(a006 = na_if(a006, "YE short")) %>%
  mutate(a006 = recode(a006, "Council rented" = "Council/HA", "HA rented" = "Council/HA",
                       "Mortgaged" = "Owned/Mortgaged", "Owned" = "Owned/Mortgaged",
                       "RENT PRIV FURN" = "Rented", "RENT PRIV UNFURN" = "Rented")) %>%
  mutate(a006 = factor(a006, levels = c("Owned/Mortgaged", "Rented", "Council/HA", "Other"))) %>%
  rename(home = a006)

table(data_mum$home, useNA = "ifany")


# Area-level index of multiple deprivation (IMD; quintiles)
table(data_mum$jan1993imd2010q5_M, useNA = "ifany")

data_mum <- data_mum %>%
  mutate(jan1993imd2010q5_M = na_if(jan1993imd2010q5_M, "Missing")) %>%
  mutate(jan1993imd2010q5_M = na_if(jan1993imd2010q5_M, "Triplets/Quadruplets")) %>%
  mutate(jan1993imd2010q5_M = recode(jan1993imd2010q5_M, "Least deprived" = "Quin. 1/Least deprived", 
                                     "2" = "Quintile 2", "3" = "Quintile 3", "4" = "Quintile 4", 
                                     "Most deprived" = "Quin. 5/Most deprived")) %>%
  mutate(jan1993imd2010q5_M = factor(jan1993imd2010q5_M, levels = c("Quin. 1/Least deprived", 
                                                                    "Quintile 2", "Quintile 3", 
                                                                    "Quintile 4", "Quin. 5/Most deprived"))) %>%
  rename(imd = jan1993imd2010q5_M)

table(data_mum$imd, useNA = "ifany")


# Urban/rural status (Urban vs Rural)
table(data_mum$jan1993ur01ind_M, useNA = "ifany")

data_mum <- data_mum %>%
  mutate(jan1993ur01ind_M = na_if(jan1993ur01ind_M, "Missing")) %>%
  mutate(jan1993ur01ind_M = na_if(jan1993ur01ind_M, "Triplets/Quadruplets")) %>%
  mutate(jan1993ur01ind_M = recode(jan1993ur01ind_M, "Hamlet and Isolated Dwelling" = "Rural", 
                                     "Town and Fringe" = "Rural", "Village" = "Rural", 
                                     "Urban (pop. >= 10k)" = "Urban")) %>%
  mutate(jan1993ur01ind_M = factor(jan1993ur01ind_M, levels = c("Urban", "Rural"))) %>%
  rename(rural = jan1993ur01ind_M)

table(data_mum$rural, useNA = "ifany")


# Marital status (Married vs Never married vs widowed/divorced/separated)
table(data_mum$a525, useNA = "ifany")

data_mum <- data_mum %>%
  mutate(a525 = na_if(a525, "Missing")) %>%
  mutate(a525 = recode(a525, "1st marriage" = "Married", "Divorced" = "Sep/Div/Widow", "Marriage 2 or 3" = "Married",
                       "Separated" = "Sep/Div/Widow", "Widowed" = "Sep/Div/Widow")) %>%
  mutate(a525 = factor(a525, levels = c("Married", "Never married", "Sep/Div/Widow"))) %>%
  rename(mum_marital = a525)

table(data_mum$mum_marital, useNA = "ifany")


# Maternal parity (0 vs 1 vs 2 or more)
table(data_mum$b032, useNA = "ifany")

data_mum <- data_mum %>%
  mutate(b032 = na_if(b032, "Missing")) %>%
  mutate(b032 = na_if(b032, "HaB short")) %>%
  mutate(b032 = na_if(b032, "Inconsistent data")) %>%
  mutate(b032 = as.numeric(b032)) %>%
  mutate(b032 = ifelse(b032 > 2 & !is.na(b032), 2, b032)) %>%
  mutate(b032 = as.factor(b032)) %>%
  mutate(b032 = recode(b032, "0" = "0", "1" = "1", "2" = "2 or more")) %>%
  mutate(b032 = factor(b032, levels = c("0", "1", "2 or more"))) %>%
  rename(parity = b032)

table(data_mum$parity, useNA = "ifany")


# Locus of control (score from 0 to 12, with higher scores indicating greater external locus of control)
table(data_mum$d842, useNA = "ifany")

data_mum <- data_mum %>%
  mutate(d842 = na_if(d842, "Missing")) %>%
  mutate(d842 = as.numeric(d842)) %>%
  rename(mum_locus = d842)

table(data_mum$mum_locus, useNA = "ifany")
summary(data_mum$mum_locus)


# Recent financial difficulties (Code as binary yes vs no)
table(data_mum$b594, useNA = "ifany")

data_mum <- data_mum %>%
  mutate(b594 = na_if(b594, "Missing")) %>%
  mutate(b594 = na_if(b594, "HaB short / YHL")) %>%
  mutate(b594 = recode(b594, "affected a lot" = "Yes", "didnt happen" = "No", "fairly affected" = "Yes",
                       "mildly affected" = "Yes", "N effect at all" = "Yes")) %>%
  mutate(b594 = factor(b594, levels = c("No", "Yes"))) %>%
  rename(mum_finDiffs = b594)

table(data_mum$mum_finDiffs, useNA = "ifany")


# Currently employed (code to binary variable, based on answers to 4 questions about different types of employment - Then drop this individual variables)
table(data_mum$c710, useNA = "ifany")
table(data_mum$c711, useNA = "ifany")
table(data_mum$c712, useNA = "ifany")
table(data_mum$c713, useNA = "ifany")

data_mum <- data_mum %>%
  mutate(mum_employed = ifelse(is.na(c710), NA,
                               ifelse(c710 == "Yes" | c711 == "Yes" | c712 == "Yes" | c713 == "Yes", "Yes", "No"))) %>%
  mutate(mum_employed = factor(mum_employed, levels = c("No", "Yes")))

table(data_mum$mum_employed, useNA = "ifany")

data_mum <- data_mum %>%
  select(-c(c710, c711, c712, c713))


# Mother's self-reported health status (recode to always well vs usually well vs often/sometimes/always unwell)
table(data_mum$b040, useNA = "ifany")

data_mum <- data_mum %>%
  mutate(b040 = na_if(b040, "Missing")) %>%
  mutate(b040 = na_if(b040, "YHL")) %>%
  mutate(b040 = recode(b040, "always unwell" = "often unwell", "SMTS unwell" = "often unwell")) %>%
  mutate(b040 = factor(b040, levels = c("always well", "usually well", "often unwell"))) %>%
  rename(mum_health = b040)

table(data_mum$mum_health, useNA = "ifany")


# Month of questionnaire completion
table(data_mum$d992, useNA = "ifany")

data_mum <- data_mum %>%
  mutate(d992 = na_if(d992, "Missing")) %>%
  mutate(d992 = factor(d992, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"))) %>%
  rename(mum_compMonth = d992)

table(data_mum$mum_compMonth, useNA = "ifany")


### And now to process the auxiliary variables

# Mother's depression in pregnancy (using Edinburgh Postnatal Depression Scale; from 0 to 30)
table(data_mum$b371, useNA = "ifany")

data_mum <- data_mum %>%
  mutate(b371 = na_if(b371, "Missing")) %>%
  mutate(b371 = na_if(b371, "YHL")) %>%
  mutate(b371 = recode(b371, "not depressed" = "0", "very depressed" = "30")) %>%
  mutate(b371 = as.numeric(b371)) %>%
  rename(mum_dep = b371)

table(data_mum$mum_dep, useNA = "ifany")
summary(data_mum$mum_dep)


# Mother's smoking status (Look at both ever smoked, and smoked in pregnancy)
table(data_mum$b650, useNA = "ifany")

data_mum <- data_mum %>%
  mutate(b650 = na_if(b650, "Missing")) %>%
  mutate(b650 = recode(b650, "Y" = "Yes", "N" = "No")) %>%
  mutate(b650 = factor(b650, levels = c("No", "Yes"))) %>%
  rename(mum_everSmk = b650)

table(data_mum$mum_everSmk, useNA = "ifany")


table(data_mum$b665, useNA = "ifany")
table(data_mum$b667, useNA = "ifany")

data_mum <- data_mum %>%
  mutate(mum_smkPreg = ifelse(is.na(b665), NA,
                              ifelse(b665 == "Y cigars" | b665 == "Y CIGS" | b665 == "Y other" |
                                       b667 == "Y cigars" | b667 == "Y CIGS" | b667 == "Y other", "Yes", "No"))) %>%
  mutate(mum_smkPreg = factor(mum_smkPreg, levels = c("No", "Yes")))

table(data_mum$mum_smkPreg, useNA = "ifany")

data_mum <- data_mum %>%
  select(-c(b665, b667))

# Quick check for consistency between these smoking variables - If said never smoke but smoked in pregnancy, code as 'ever smoked' (only 3 cases); If said smoked in pregnancy but missing ever smokes, code as 'ever smoked' (again, only 3 cases
table(data_mum$mum_everSmk, data_mum$mum_smkPreg, useNA = "ifany")

data_mum <- data_mum %>%
  mutate(mum_everSmk = as.character(mum_everSmk)) %>%
  mutate(mum_smkPreg = as.character(mum_smkPreg)) %>%
  mutate(mum_everSmk = ifelse(mum_everSmk == "No" & mum_smkPreg == "Yes", "Yes", mum_everSmk)) %>%
  mutate(mum_everSmk = ifelse(is.na(mum_everSmk) & mum_smkPreg == "Yes", "Yes", mum_everSmk)) %>%
  mutate(mum_everSmk = factor(mum_everSmk, levels = c("No", "Yes"))) %>%
  mutate(mum_smkPreg = factor(mum_smkPreg, levels = c("No", "Yes")))

table(data_mum$mum_everSmk, data_mum$mum_smkPreg, useNA = "ifany")


# Mothers alcohol intake, both pre-pregnancy and in pregnancy (based on around time baby first moved; approx. 16-24 weeks)
table(data_mum$b720, useNA = "ifany")

data_mum <- data_mum %>%
  mutate(b720 = na_if(b720, "Missing")) %>%
  mutate(b720 = recode(b720, "never" = "Never", "<1 glass PWK" = "<1 p/wk", "1+ glasses PWK" = "1+ p/wk",
                       "1-2 glasses PDAY" = "1+ p/day", "3-9 glasses PDAY" = "1+ p/day",
                       "10+ glasses PDAY" = "1+ p/day")) %>%
  mutate(b720 = factor(b720, levels = c("Never", "<1 p/wk", "1+ p/wk", "1+ p/day"))) %>%
  rename(mum_alcPrePreg = b720)

table(data_mum$mum_alcPrePreg, useNA = "ifany")


table(data_mum$b722, useNA = "ifany")

data_mum <- data_mum %>%
  mutate(b722 = na_if(b722, "Missing")) %>%
  mutate(b722 = recode(b722, "never" = "Never", "<1 glass PWK" = "<1 p/wk", "1+ glasses PWK" = "1+ p/wk",
                       "1-2 glasses PDAY" = "1+ p/day", "3-9 glasses PDAY" = "1+ p/day",
                       "10+ glasses PDAY" = "1+ p/day")) %>%
  mutate(b722 = factor(b722, levels = c("Never", "<1 p/wk", "1+ p/wk", "1+ p/day"))) %>%
  rename(mum_alcPreg = b722)

table(data_mum$mum_alcPreg, useNA = "ifany")


# Mother's occupational social class (combine lowest social classes - partially unskilled [IV] and unskilled [V] togethr, due to small sample sizes)
table(data_mum$c755, useNA = "ifany")

data_mum <- data_mum %>%
  mutate(c755 = na_if(c755, "Missing")) %>%
  mutate(c755 = na_if(c755, "Armed forces")) %>%
  mutate(c755 = recode(c755, "IV" = "IV/V", "V" = "IV/V")) %>%
  mutate(c755 = factor(c755, levels = c("I", "II", "III (non-manual)", "III (manual)", "IV/V"))) %>%
  rename(mum_occSocClass = c755)

table(data_mum$mum_occSocClass, useNA = "ifany")


# Household access to a car
table(data_mum$a053, useNA = "ifany")

data_mum <- data_mum %>%
  mutate(a053 = na_if(a053, "Missing")) %>%
  mutate(a053 = na_if(a053, "YE short")) %>%
  mutate(a053 = factor(a053, levels = c("No", "Yes"))) %>%
  rename(carAccess = a053)

table(data_mum$carAccess, useNA = "ifany")


## And also process the variable saying whether the questionnaire with religion variables was completed during pregnancy (most cases) or shortly afterwards
table(data_mum$d990, useNA = "ifany")

data_mum <- data_mum %>%
  mutate(d990 = na_if(d990, "-1")) %>%
  mutate(d990 = ifelse(is.na(d990), NA,
                       ifelse(d990 == "Baby already born", "Yes", "No"))) %>%
  mutate(d990 = factor(d990, levels = c("No", "Yes"))) %>%
  rename(comp_postPreg = d990)

table(data_mum$comp_postPreg, useNA = "ifany")


### Save mother's data (in R, CSV and Stata format; the R and Stata formats will keep all the factor formatting, while the CSV file will lose this) - Drop the ALN identifier first, as not needed
data_mum <- data_mum %>%
  select(-aln)

save(data_mum, file = "data_mum_processed_B4030.RData")
write_csv(data_mum, file = "data_mum_processed_B4030.csv")
write_dta(data_mum, "data_mum_processed_B4030.dta")



############################################################################################################
#### Now process the partners data

data_partner <- data_raw


### Removing some observations 

## First repeat the steps to drop repeated mums, if mother withdrew consent, and non-core pregnancies

## Want to drop data from one mother if linked to two pregnancies (else data possibly repeated)
table(data_partner$mz005l, useNA = "ifany")

data_partner <- data_partner %>%
  filter(mz005l != "Yes, drop these mult mums")

## Drop data if mother withdrew consent for data to be used
table(data_partner$d810, useNA = "ifany")

data_partner <- data_partner %>%
  filter(d810 != ".a" | is.na(d810))

## Drop if pregnancy not result in a live birth
table(data_partner$mz012, useNA = "ifany")

data_partner <- data_partner %>%
  filter(mz012 == "All survived")

## Also drop data if not enrolled in ALSPAC during pregnancy
table(data_partner$mz028b, useNA = "ifany")

data_partner <- data_partner %>%
  filter(mz028b != "Not in core sample")

## Also drop mother data if younger than 17 when becoming pregnant, as cannot give blood before this age (n = 146)
table(data_partner$mz028a, useNA = "ifany")

data_partner <- data_partner %>%
  filter(mz028a != "< 16" & mz028a != "16")

## Remove mothers with Rastafarian or Jehovah's Witness religious affiliation, as may have proscriptions against blood donation (numbers very small; n = 59)
table(data_partner$d813, useNA = "ifany")

data_partner <- data_partner %>%
  filter((d813 != "Jehovah Witness" & d813 != "Rastafarian") | is.na(d813))


## See if any additional repeated partners linked to two pregnancies (are no extra partners)
table(data_partner$pz_mult, useNA = "ifany")

## See if any additional partners have withdrawn consent (are no extra partners)
table(data_partner$pb150, useNA = "ifany")

## Also remove any partners who changed identity between the time-periods explored here (as all data comes from pregnancy, this is a very short time-span and so few partners changed identity; n = 27)
table(data_partner$partner_changed_when, useNA = "ifany")

data_partner <- data_partner %>%
  filter(partner_changed_when != "PA" | is.na(partner_changed_when))

## Also drop partner data if younger than 17 in pregnancy, as cannot give blood before this age (n = 15)
table(data_partner$partner_age, useNA = "ifany")

data_partner <- data_partner %>%
  filter((partner_age != "11" & partner_age != "13" & partner_age != "14" & partner_age != "15" & 
           partner_age != "16") | is.na(partner_age))

## Finally, remove partners with Rastafarian or Jehovah's Witness religious affiliation, as may have proscriptions against blood donation (numbers very small; n = 11)
table(data_partner$pb153, useNA = "ifany")

data_partner <- data_partner %>%
  filter((pb153 != "Jehovah witness" & pb153 != "Rastafarian") | is.na(pb153))


### Keep just the variables of interest and re-order
data_partner <- data_partner %>%
  relocate(aln, pb150, pb153, pb155, pa300, partner_age, pb440, pb325a, pa065, pa782, pb184, pb380, pb381, pb382, 
           pb383, a524, pa902, pb260, pb071, pb077, pb100, c765, c666a, c801, c730, c731, c732, c733,
           d810, d813, d816, d290, mz028b, c800, c645a, a006, jan1993imd2010q5_M, jan1993ur01ind_M, a525, b032,
           d842, b594, c710, c711, c712, c713, b040, pb900, a053) %>%
  select(aln:a053)

colnames(data_partner)



#### Now process the data and prep variables for analysis

### Start with exposures (religious belief, affiliation and attendance)

## Religious belief (Yes vs No/Not sure)
table(data_partner$pb150, useNA = "ifany")

# If missing, code as NA, combine 'no' and 'not sure together, then convert to factor and order levels
data_partner <- data_partner %>%
  mutate(pb150 = na_if(pb150, "-1")) %>%
  mutate(pb150 = recode(pb150, "N" = "No", "Not sure" = "No", "Y" = "Yes")) %>%
  mutate(pb150 = factor(pb150, levels = c("No", "Yes"))) %>%
  rename(partner_belief = pb150)

table(data_partner$partner_belief, useNA = "ifany")
round(prop.table(table(data_partner$partner_belief)) * 100, 1)
sum(table(data_partner$partner_belief))


## Religious affiliation/identity (Yes vs None)
table(data_partner$pb153, useNA = "ifany")

# If missing, code as NA, combine all religious affiliations together, then convert to factor and order levels
data_partner <- data_partner %>%
  mutate(pb153 = na_if(pb153, "-1")) %>%
  mutate(pb153 = ifelse(is.na(pb153), NA,
                       ifelse(pb153 == "None", "None", "Religious"))) %>%
  mutate(pb153 = factor(pb153, levels = c("None", "Religious"))) %>%
  rename(partner_identity = pb153)

table(data_partner$partner_identity, useNA = "ifany")
round(prop.table(table(data_partner$partner_identity)) * 100, 1)
sum(table(data_partner$partner_identity))


## Religious attendance (Regular vs Occasional/non-attendance)
table(data_partner$pb155, useNA = "ifany")

# If missing, code as NA, then convert to factor and order levels
data_partner <- data_partner %>%
  mutate(pb155 = na_if(pb155, "-1")) %>%
  mutate(pb155 = recode(pb155, "MIN 1 PMTH" = "Regular", "MIN 1 PWK" = "Regular", 
                            "MIN 1 PYR" = "Occasional/None", "Not at all" = "Occasional/None")) %>%
  mutate(pb155 = factor(pb155, levels = c("Occasional/None", "Regular"))) %>%
  rename(partner_attend = pb155)

table(data_partner$partner_attend, useNA = "ifany")
round(prop.table(table(data_partner$partner_attend)) * 100, 1)
sum(table(data_partner$partner_attend))


### Now to outcome (blood donation; Yes vs No)
table(data_partner$pa300, useNA = "ifany")

# If missing, code as NA, then convert to factor and order levels
data_partner <- data_partner %>%
  mutate(pa300 = na_if(pa300, "-1")) %>%
  mutate(pa300 = factor(pa300, levels = c("No", "Yes"))) %>%
  rename(partner_donate = pa300)

table(data_partner$partner_donate, useNA = "ifany")
round(prop.table(table(data_partner$partner_donate)) * 100, 1)
sum(table(data_partner$partner_donate))


### Next to potential partner-based confounders

## Partner age (at delivery; years)
table(data_partner$partner_age, useNA = "ifany")

# If missing, code as NA, then convert to numeric - Also group some higher ages together, so not disclosive
data_partner <- data_partner %>%
  mutate(partner_age = na_if(partner_age, "Insufficient DOB information")) %>%
  mutate(partner_age = as.numeric(partner_age)) %>%
  mutate(partner_age = ifelse(is.na(partner_age), partner_age,
                              ifelse(partner_age > 55, 55, partner_age)))

table(data_partner$partner_age, useNA = "ifany")
summary(data_partner$partner_age)


# Partner's ethnicity (White vs other than White)
table(data_partner$pb440, useNA = "ifany")

data_partner <- data_partner %>%
  mutate(pb440 = na_if(pb440, "-1")) %>%
  mutate(pb440 = recode(pb440, "Bangladeshi" = "Other than White", "black African" = "Other than White", 
                       "black Caribbean" = "Other than White", "Chinese" = "Other than White", 
                       "Indian" = "Other than White", "other ethnic" = "Other than White", 
                       "black other" = "Other than White", "Pakistani" = "Other than White",
                       "white" = "White")) %>%
  mutate(pb440 = factor(pb440, levels = c("White", "Other than White"))) %>%
  rename(partner_ethnicity = pb440)

table(data_partner$partner_ethnicity, useNA = "ifany")


# Partner's highest education qualification (CSE/None vs Vocational vs O-level vs A-level vs Degree)
table(data_partner$pb325a, useNA = "ifany")

data_partner <- data_partner %>%
  mutate(pb325a = na_if(pb325a, "-1")) %>%
  mutate(pb325a = recode(pb325a, "CSE" = "CSE/None")) %>%
  mutate(pb325a = factor(pb325a, levels = c("CSE/None", "Vocational", "O level", "A level", "Degree"))) %>%
  rename(partner_edu = pb325a)

table(data_partner$partner_edu, useNA = "ifany")


# Marital status (Married vs Never married vs widowed/divorced/separated)
table(data_partner$pa065, useNA = "ifany")

data_partner <- data_partner %>%
  mutate(pa065 = na_if(pa065, "-1")) %>%
  mutate(pa065 = recode(pa065, "1st marriage" = "Married", "Divorced" = "Sep/Div/Widow", "Marriage 2 or 3" = "Married",
                       "Separated" = "Sep/Div/Widow", "Widowed" = "Sep/Div/Widow")) %>%
  mutate(pa065 = factor(pa065, levels = c("Married", "Never married", "Sep/Div/Widow"))) %>%
  rename(partner_marital = pa065)

table(data_partner$partner_marital, useNA = "ifany")


# Locus of control (score from 0 to 12, with higher scores indicating greater external locus of control)
table(data_partner$pa782, useNA = "ifany")

data_partner <- data_partner %>%
  mutate(pa782 = na_if(pa782, "-1")) %>%
  mutate(pa782 = as.numeric(pa782)) %>%
  rename(partner_locus = pa782)

table(data_partner$partner_locus, useNA = "ifany")
summary(data_partner$partner_locus)


# Recent financial difficulties (Code as binary yes vs no)
table(data_partner$pb184, useNA = "ifany")

data_partner <- data_partner %>%
  mutate(pb184 = na_if(pb184, "-1")) %>%
  mutate(pb184 = recode(pb184, "affected a lot" = "Yes", "didnt happen" = "No", "fairly affected" = "Yes",
                       "mildly affected" = "Yes", "N effect at all" = "Yes")) %>%
  mutate(pb184 = factor(pb184, levels = c("No", "Yes"))) %>%
  rename(partner_finDiffs = pb184)

table(data_partner$partner_finDiffs, useNA = "ifany")


# Currently employed (code to binary variable, based on answers to 4 questions about different types of employment - Then drop this individual variables)
table(data_partner$pb380, useNA = "ifany")
table(data_partner$pb381, useNA = "ifany")
table(data_partner$pb382, useNA = "ifany")
table(data_partner$pb383, useNA = "ifany")

data_partner <- data_partner %>%
  mutate(partner_employed = ifelse(is.na(pb380), NA,
                               ifelse(pb380 == "Y" | pb381 == "Y" | pb382 == "Y" | pb383 == "Y", "Yes", "No"))) %>%
  mutate(partner_employed = factor(partner_employed, levels = c("No", "Yes")))

table(data_partner$partner_employed, useNA = "ifany")

data_partner <- data_partner %>%
  select(-c(pb380, pb381, pb382, pb383))


# Partner's health status, as reported by mother (recode to always well vs usually well vs often/sometimes/always unwell)
table(data_partner$a524, useNA = "ifany")

data_partner <- data_partner %>%
  mutate(a524 = na_if(a524, "Missing")) %>%
  mutate(a524 = na_if(a524, "No partner")) %>%
  mutate(a524 = recode(a524, "Always unwell" = "Often unwell", "Sometimes unwell" = "Often unwell")) %>%
  mutate(a524 = factor(a524, levels = c("Always well", "Usually well", "Often unwell"))) %>%
  rename(partner_health = a524)

table(data_partner$partner_health, useNA = "ifany")


# Month of questionnaire completion
table(data_partner$pa902, useNA = "ifany")

data_partner <- data_partner %>%
  mutate(pa902 = na_if(pa902, "Missing")) %>%
  mutate(pa902 = factor(pa902, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"))) %>%
  rename(partner_compMonth = pa902)

table(data_partner$partner_compMonth, useNA = "ifany")


### And now to process the partner-based auxiliary variables

# Partner's depression in pregnancy (using Edinburgh Postnatal Depression Scale; from 0 to 30)
table(data_partner$pb260, useNA = "ifany")

data_partner <- data_partner %>%
  mutate(pb260 = na_if(pb260, "-1")) %>%
  mutate(pb260 = as.numeric(pb260)) %>%
  rename(partner_dep = pb260)

table(data_partner$partner_dep, useNA = "ifany")
summary(data_partner$partner_dep)


# Partner's smoking status (ever smoked vs former smoker vs current smoker)
table(data_partner$pb071, useNA = "ifany")
table(data_partner$pb077, useNA = "ifany")

data_partner <- data_partner %>%
  mutate(pb071 = na_if(pb071, "-1")) %>%
  mutate(pb071 = recode(pb071, "Y" = "Yes", "N" = "No")) %>%
  mutate(pb071 = factor(pb071, levels = c("No", "Yes"))) %>%
  mutate(pb077 = na_if(pb077, "-1")) %>%
  mutate(pb077 = recode(pb077, "Y cigars" = "Yes", "Y cigs" = "Yes", "Y pipe" = "Yes", "Y other" = "Yes", 
                        "N" = "No")) %>%
  mutate(pb077 = factor(pb077, levels = c("No", "Yes")))

table(data_partner$pb071, useNA = "ifany")
table(data_partner$pb077, useNA = "ifany")
table(data_partner$pb071, data_partner$pb077, useNA = "ifany")

data_partner <- data_partner %>%
  mutate(partner_smk = ifelse(is.na(pb071) | is.na(pb077), NA,
                              ifelse(pb071 == "No" & pb077 == "No", "Never",
                                     ifelse(pb071 == "Yes" & pb077 == "No", "Former", "Current")))) %>%
  mutate(partner_smk = factor(partner_smk, levels = c("Never", "Former", "Current")))

table(data_partner$partner_smk, useNA = "ifany")

data_partner <- data_partner %>%
  select(-c(pb071, pb077))


# Partners alcohol intake in past 3 months
table(data_partner$pb100, useNA = "ifany")

data_partner <- data_partner %>%
  mutate(pb100 = na_if(pb100, "Missing")) %>%
  mutate(pb100 = recode(pb100, "<1 glass PWK" = "<1 p/wk", "1+ glasses PWK" = "1+ p/wk",
                       "1-2 glasses PDAY" = "1+ p/day", "3-9 glasses PDAY" = "1+ p/day",
                       "10+ glasses PDAY" = "1+ p/day")) %>%
  mutate(pb100 = factor(pb100, levels = c("Never", "<1 p/wk", "1+ p/wk", "1+ p/day"))) %>%
  rename(partner_alc = pb100)

table(data_partner$partner_alc, useNA = "ifany")


# Partner's occupational social class (combine lowest social classes - partially unskilled [IV] and unskilled [V] togethr, due to small sample sizes)
table(data_partner$c765, useNA = "ifany")

data_partner <- data_partner %>%
  mutate(c765 = na_if(c765, "Missing")) %>%
  mutate(c765 = na_if(c765, "Armed forces")) %>%
  mutate(c765 = recode(c765, "IV" = "IV/V", "V" = "IV/V")) %>%
  mutate(c765 = factor(c765, levels = c("I", "II", "III (non-manual)", "III (manual)", "IV/V"))) %>%
  rename(partner_occSocClass = c765)

table(data_partner$partner_occSocClass, useNA = "ifany")


# Partner's highest education qualification, as reported by the mother (CSE/None vs Vocational vs O-level vs A-level vs Degree)
table(data_partner$c666a, useNA = "ifany")

data_partner <- data_partner %>%
  mutate(c666a = na_if(c666a, "Missing")) %>%
  mutate(c666a = recode(c666a, "CSE" = "CSE/None")) %>%
  mutate(c666a = factor(c666a, levels = c("CSE/None", "Vocational", "O level", "A level", "Degree"))) %>%
  rename(partner_edu_byMum = c666a)

table(data_partner$partner_edu_byMum, useNA = "ifany")


# Partner's ethnicity, as reported by the mother (White vs other than White)
table(data_partner$c801, useNA = "ifany")

data_partner <- data_partner %>%
  mutate(c801 = na_if(c801, "-1")) %>%
  mutate(c801 = recode(c801, "Bangladeshi" = "Other than White", "Black African" = "Other than White", 
                        "Black Caribbean" = "Other than White", "Chinese" = "Other than White", 
                        "Indian" = "Other than White", "Other" = "Other than White", 
                        "Other black" = "Other than White", "Pakistani" = "Other than White")) %>%
  mutate(c801 = factor(c801, levels = c("White", "Other than White"))) %>%
  rename(partner_ethnicity_byMum = c801)

table(data_partner$partner_ethnicity_byMum, useNA = "ifany")


# Partner's employment, as reported by mother (code to binary variable, based on answers to 4 questions about different types of employment - Then drop this individual variables)
table(data_partner$c730, useNA = "ifany")
table(data_partner$c731, useNA = "ifany")
table(data_partner$c732, useNA = "ifany")
table(data_partner$c733, useNA = "ifany")

data_partner <- data_partner %>%
  mutate(partner_employed_byMum = ifelse(is.na(c730), NA,
                                   ifelse(c730 == "Yes" | c731 == "Yes" | c732 == "Yes" | c733 == "Yes", 
                                          "Yes", "No"))) %>%
  mutate(partner_employed_byMum = factor(partner_employed_byMum, levels = c("No", "Yes")))

table(data_partner$partner_employed_byMum, useNA = "ifany")

data_partner <- data_partner %>%
  select(-c(c730, c731, c732, c733))


### And copying the mother-based exposures, outcome, confounders and auxiliary variables from above

## Mother's religious belief (Yes vs No/Not sure)
table(data_partner$d810, useNA = "ifany")

# If missing, code as NA, combine 'no' and 'not sure' together, then convert to factor and order levels
data_partner <- data_partner %>%
  mutate(d810 = na_if(d810, "-1")) %>%
  mutate(d810 = ifelse(d810 == "Not sure", "No", d810)) %>%
  mutate(d810 = factor(d810, levels = c("No", "Yes"))) %>%
  rename(mum_belief = d810)

table(data_partner$mum_belief, useNA = "ifany")
round(prop.table(table(data_partner$mum_belief)) * 100, 1)


## Mother's religious affiliation/identity (Yes vs None)
table(data_partner$d813, useNA = "ifany")

# If missing, code as NA, combine all religious affiliations together, then convert to factor and order levels
data_partner <- data_partner %>%
  mutate(d813 = na_if(d813, "-1")) %>%
  mutate(d813 = ifelse(is.na(d813), NA,
                       ifelse(d813 == "None", "None", "Religious"))) %>%
  mutate(d813 = factor(d813, levels = c("None", "Religious"))) %>%
  rename(mum_identity = d813)

table(data_partner$mum_identity, useNA = "ifany")
round(prop.table(table(data_partner$mum_identity)) * 100, 1)


## Religious attendance (regular attendance vs Occasional/non-attendance)
table(data_partner$d816, useNA = "ifany")

# If missing, code as NA, then convert to factor and order levels
data_partner <- data_partner %>%
  mutate(d816 = na_if(d816, "-1")) %>%
  mutate(d816 = recode(d816, "MIN 1 a MTH" = "Regular", "MIN 1 a WK" = "Regular", 
                       "MIN 1 a YR" = "Occasional/None", "Not at all" = "Occasional/None")) %>%
  mutate(d816 = factor(d816, levels = c("Occasional/None", "Regular"))) %>%
  rename(mum_attend = d816)

table(data_partner$mum_attend, useNA = "ifany")
round(prop.table(table(data_partner$mum_attend)) * 100, 1)


### Now to outcome (blood donation; Yes vs No)
table(data_partner$d290, useNA = "ifany")

# If missing, code as NA, then convert to factor and order levels
data_partner <- data_partner %>%
  mutate(d290 = na_if(d290, "-1")) %>%
  mutate(d290 = factor(d290, levels = c("No", "Yes"))) %>%
  rename(mum_donate = d290)

table(data_partner$mum_donate, useNA = "ifany")
round(prop.table(table(data_partner$mum_donate)) * 100, 1)


## Maternal age (at birth; years)
table(data_partner$mz028b, useNA = "ifany")

# If missing, code as NA, then convert to numeric
data_partner <- data_partner %>%
  mutate(mz028b = na_if(mz028b, "Miscarried")) %>%
  mutate(mz028b = na_if(mz028b, "Outcome NK")) %>%
  mutate(mz028b = na_if(mz028b, "Triplet / quadruplet")) %>%
  mutate(mz028b = recode(mz028b, "< 16" = "16", ">43" = "43")) %>%
  mutate(mz028b = as.numeric(mz028b)) %>%
  rename(mum_age = mz028b)

table(data_partner$mum_age, useNA = "ifany")
summary(data_partner$mum_age)


# Mother's ethnicity (White vs other than White)
table(data_partner$c800, useNA = "ifany")

data_partner <- data_partner %>%
  mutate(c800 = na_if(c800, "Missing")) %>%
  mutate(c800 = recode(c800, "Bangladeshi" = "Other than White", "Black African" = "Other than White", 
                       "Black Caribbean" = "Other than White", "Chinese" = "Other than White", 
                       "Indian" = "Other than White", "Other" = "Other than White", 
                       "Other black" = "Other than White", "Pakistani" = "Other than White")) %>%
  mutate(c800 = factor(c800, levels = c("White", "Other than White"))) %>%
  rename(mum_ethnicity = c800)

table(data_partner$mum_ethnicity, useNA = "ifany")


# Mother's highest education qualification (CSE/None vs Vocational vs O-level vs A-level vs Degree)
table(data_partner$c645a, useNA = "ifany")

data_partner <- data_partner %>%
  mutate(c645a = na_if(c645a, "Missing")) %>%
  mutate(c645a = recode(c645a, "CSE" = "CSE/None")) %>%
  mutate(c645a = factor(c645a, levels = c("CSE/None", "Vocational", "O level", "A level", "Degree"))) %>%
  rename(mum_edu = c645a)

table(data_partner$mum_edu, useNA = "ifany")


# Home ownership status (owned/mortgaged vs rented vs Council/housing association vs other)
table(data_partner$a006, useNA = "ifany")

data_partner <- data_partner %>%
  mutate(a006 = na_if(a006, "Missing")) %>%
  mutate(a006 = na_if(a006, "YE short")) %>%
  mutate(a006 = recode(a006, "Council rented" = "Council/HA", "HA rented" = "Council/HA",
                       "Mortgaged" = "Owned/Mortgaged", "Owned" = "Owned/Mortgaged",
                       "RENT PRIV FURN" = "Rented", "RENT PRIV UNFURN" = "Rented")) %>%
  mutate(a006 = factor(a006, levels = c("Owned/Mortgaged", "Rented", "Council/HA", "Other"))) %>%
  rename(home = a006)

table(data_partner$home, useNA = "ifany")


# Area-level index of multiple deprivation (IMD; quintiles)
table(data_partner$jan1993imd2010q5_M, useNA = "ifany")

data_partner <- data_partner %>%
  mutate(jan1993imd2010q5_M = na_if(jan1993imd2010q5_M, "Missing")) %>%
  mutate(jan1993imd2010q5_M = na_if(jan1993imd2010q5_M, "Triplets/Quadruplets")) %>%
  mutate(jan1993imd2010q5_M = recode(jan1993imd2010q5_M, "Least deprived" = "Quin. 1/Least deprived", 
                                     "2" = "Quintile 2", "3" = "Quintile 3", "4" = "Quintile 4", 
                                     "Most deprived" = "Quin. 5/Most deprived")) %>%
  mutate(jan1993imd2010q5_M = factor(jan1993imd2010q5_M, levels = c("Quin. 1/Least deprived", 
                                                                    "Quintile 2", "Quintile 3", 
                                                                    "Quintile 4", "Quin. 5/Most deprived"))) %>%
  rename(imd = jan1993imd2010q5_M)

table(data_partner$imd, useNA = "ifany")


# Urban/rural status (Urban vs Rural)
table(data_partner$jan1993ur01ind_M, useNA = "ifany")

data_partner <- data_partner %>%
  mutate(jan1993ur01ind_M = na_if(jan1993ur01ind_M, "Missing")) %>%
  mutate(jan1993ur01ind_M = na_if(jan1993ur01ind_M, "Triplets/Quadruplets")) %>%
  mutate(jan1993ur01ind_M = recode(jan1993ur01ind_M, "Hamlet and Isolated Dwelling" = "Rural", 
                                   "Town and Fringe" = "Rural", "Village" = "Rural", 
                                   "Urban (pop. >= 10k)" = "Urban")) %>%
  mutate(jan1993ur01ind_M = factor(jan1993ur01ind_M, levels = c("Urban", "Rural"))) %>%
  rename(rural = jan1993ur01ind_M)

table(data_partner$rural, useNA = "ifany")


# Marital status (Married vs Never married vs widowed/divorced/separated)
table(data_partner$a525, useNA = "ifany")

data_partner <- data_partner %>%
  mutate(a525 = na_if(a525, "Missing")) %>%
  mutate(a525 = recode(a525, "1st marriage" = "Married", "Divorced" = "Sep/Div/Widow", "Marriage 2 or 3" = "Married",
                       "Separated" = "Sep/Div/Widow", "Widowed" = "Sep/Div/Widow")) %>%
  mutate(a525 = factor(a525, levels = c("Married", "Never married", "Sep/Div/Widow"))) %>%
  rename(mum_marital = a525)

table(data_partner$mum_marital, useNA = "ifany")


# Maternal parity (0 vs 1 vs 2 or more)
table(data_partner$b032, useNA = "ifany")

data_partner <- data_partner %>%
  mutate(b032 = na_if(b032, "Missing")) %>%
  mutate(b032 = na_if(b032, "HaB short")) %>%
  mutate(b032 = na_if(b032, "Inconsistent data")) %>%
  mutate(b032 = as.numeric(b032)) %>%
  mutate(b032 = ifelse(b032 > 2 & !is.na(b032), 2, b032)) %>%
  mutate(b032 = as.factor(b032)) %>%
  mutate(b032 = recode(b032, "0" = "0", "1" = "1", "2" = "2 or more")) %>%
  mutate(b032 = factor(b032, levels = c("0", "1", "2 or more"))) %>%
  rename(parity = b032)

table(data_partner$parity, useNA = "ifany")


# Mother's locus of control (score from 0 to 12, with higher scores indicating greater external locus of control)
table(data_partner$d842, useNA = "ifany")

data_partner <- data_partner %>%
  mutate(d842 = na_if(d842, "Missing")) %>%
  mutate(d842 = as.numeric(d842)) %>%
  rename(mum_locus = d842)

table(data_partner$mum_locus, useNA = "ifany")
summary(data_partner$mum_locus)


# Recent financial difficulties (Code as binary yes vs no)
table(data_partner$b594, useNA = "ifany")

data_partner <- data_partner %>%
  mutate(b594 = na_if(b594, "Missing")) %>%
  mutate(b594 = na_if(b594, "HaB short / YHL")) %>%
  mutate(b594 = recode(b594, "affected a lot" = "Yes", "didnt happen" = "No", "fairly affected" = "Yes",
                       "mildly affected" = "Yes", "N effect at all" = "Yes")) %>%
  mutate(b594 = factor(b594, levels = c("No", "Yes"))) %>%
  rename(mum_finDiffs = b594)

table(data_partner$mum_finDiffs, useNA = "ifany")


# Mother currently employed (code to binary variable, based on answers to 4 questions about different types of employment - Then drop this individual variables)
table(data_partner$c710, useNA = "ifany")
table(data_partner$c711, useNA = "ifany")
table(data_partner$c712, useNA = "ifany")
table(data_partner$c713, useNA = "ifany")

data_partner <- data_partner %>%
  mutate(mum_employed = ifelse(is.na(c710), NA,
                               ifelse(c710 == "Yes" | c711 == "Yes" | c712 == "Yes" | c713 == "Yes", "Yes", "No"))) %>%
  mutate(mum_employed = factor(mum_employed, levels = c("No", "Yes")))

table(data_partner$mum_employed, useNA = "ifany")

data_partner <- data_partner %>%
  select(-c(c710, c711, c712, c713))


# Mother's self-reported health status (recode to always well vs usually well vs often/sometimes/always unwell)
table(data_partner$b040, useNA = "ifany")

data_partner <- data_partner %>%
  mutate(b040 = na_if(b040, "Missing")) %>%
  mutate(b040 = na_if(b040, "YHL")) %>%
  mutate(b040 = recode(b040, "always unwell" = "often unwell", "SMTS unwell" = "often unwell")) %>%
  mutate(b040 = factor(b040, levels = c("always well", "usually well", "often unwell"))) %>%
  rename(mum_health = b040)

table(data_partner$mum_health, useNA = "ifany")


# Household access to a car
table(data_partner$a053, useNA = "ifany")

data_partner <- data_partner %>%
  mutate(a053 = na_if(a053, "Missing")) %>%
  mutate(a053 = na_if(a053, "YE short")) %>%
  mutate(a053 = factor(a053, levels = c("No", "Yes"))) %>%
  rename(carAccess = a053)

table(data_partner$carAccess, useNA = "ifany")


## And also process the variable saying whether the questionnaire with religion variables was completed during pregnancy (most cases) or shortly afterwards
table(data_partner$pb900, useNA = "ifany")

data_partner <- data_partner %>%
  mutate(pb900 = na_if(pb900, "-1")) %>%
  mutate(pb900 = ifelse(is.na(pb900), NA,
                       ifelse(pb900 == "After delivery", "Yes", "No"))) %>%
  mutate(pb900 = factor(pb900, levels = c("No", "Yes"))) %>%
  rename(comp_postPreg = pb900)

table(data_partner$comp_postPreg, useNA = "ifany")


### Save partner's data - Drop the ALN identifier first, as not needed
data_partner <- data_partner %>%
  select(-aln)

save(data_partner, file = "data_partner_processed_B4030.RData")
write_csv(data_partner, file = "data_partner_processed_B4030.csv")
write_dta(data_partner, "data_partner_processed_B4030.dta")


