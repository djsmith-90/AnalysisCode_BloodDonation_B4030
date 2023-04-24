*** Script for paper 'Exploring causality from observational data: An example assessing whether religiosity promotes cooperation' using ALSPAC data (ALSPAC B-number B4030)
*** Script 4b: Generalised Sensitivity Analysis to explore residual confounding in partner's data
*** Created 27/2/2023 by Dan Major-Smith
*** Stata version 17

** Analysis plan for this paper has been accepted as a Registered Report for the journal Evolutionary Human Sciences, and the plan is available on the OSF: https://osf.io/z5gcm/


**** NOTE: The main analyses for this data have been conducted in R - Please see the associated "Script4_AnalysingPartnersData.r" script ****


*** Install the 'gsa' package, if needed
*ssc install gsa, replace


************************************************************************************
*** Set working directory and read in the dataset
cd "X:\Studies\RSBB Team\Dan\B4030 - RSBB and cooperation"

use "data_partner_processed_B4030.dta", clear

** Also move exposures and outcome to end of dataset, to match synthesised data
order mum_belief-mum_donate partner_age-partner_ethnicity_byMum ///
	partner_employed comp_postPreg partner_smk partner_belief-partner_donate, last

** If using synthesised data here, read this dataset in instead - NOTE: The sample size is slightly smaller in the synthetic dataset, as unique replicates with identical data in both the observed and synthetic data have been removed for confidentiality reasons. Also, the statistics in the script below refer to the observed data, and will differ for the synthetic data
*use ".\AnalysisCode_BloodDonation_B4030\ALSPACAnalysis\syntheticData_partner_B4030.dta", clear


* Also drop the variable saying whether questionnaire completed during or after pregnancy (as not associated with exposures or outcomes in partner)
drop comp_postPreg


** Quick check of the data
describe
codebook


** Will need to recode the exposures and outcome to 0 and 1, in order for logistic model and GSA to work
recode partner_donate (1 = 0) (2 = 1)
label define partner_donate 0 "No" 1 "Yes", modify
tab partner_donate, m

recode partner_belief (1 = 0) (2 = 1)
label define partner_belief 0 "No" 1 "Yes", modify
tab partner_belief, m

recode partner_identity (1 = 0) (2 = 1)
label define partner_identity 0 "None" 1 "Religious", modify
tab partner_identity, m

recode partner_attend (1 = 0) (2 = 1)
label define partner_attend 0 "Occasional/None" 1 "Regular", modify
tab partner_attend, m


** Also make a complete case marker, and drop any observations with missing data
egen cca_marker = rowmiss(partner_belief partner_identity partner_attend ///
	partner_donate partner_age partner_ethnicity partner_edu home imd rural ///
	partner_marital parity partner_locus partner_finDiffs partner_health ///
	partner_employed partner_compMonth)
	
tab cca_marker, m

drop if cca_marker != 0
drop cca_marker


*** From the main analysis in the R script, we know that there is little association between religious belief or identity and blood donation, in adjusted analyses; for religious attendance there appears to be a stronger association. Here, we will perform a Generalised Sensitivity Analysis (GSA) to explore the amount of unmeasured confounding necessary to alter the observed result, when adjusting for assumed confounders only.

* Here's the model again
logit partner_donate i.partner_attend partner_age i.partner_ethnicity i.partner_edu ///
	i.home i.imd i.rural i.partner_finDiffs i.partner_employed i.partner_compMonth

	
** Before running GSA, need to convert categorical variables to dummy variables (as GSA doesn't accept the "i." notation)

* Mother's education
tab partner_edu
xi i.partner_edu
rename _Ipartner_e_2 Vocational
rename _Ipartner_e_3 Olevel
rename _Ipartner_e_4 Alevel
rename _Ipartner_e_5 Degree

* Home ownership status
tab home
xi i.home
rename _Ihome_2 home_rent
rename _Ihome_3 home_HA
rename _Ihome_4 home_other

* Index of multiple deprivation
tab imd
xi i.imd
rename _Iimd_2 imd_q2
rename _Iimd_3 imd_q3
rename _Iimd_4 imd_q4
rename _Iimd_5 imd_q5

* Month of questionnaire completion
tab partner_compMonth
xi i.partner_compMonth
rename _Ipartner_c_2 comp_feb
rename _Ipartner_c_3 comp_mar
rename _Ipartner_c_4 comp_apr
rename _Ipartner_c_5 comp_may
rename _Ipartner_c_6 comp_jun
rename _Ipartner_c_7 comp_jul
rename _Ipartner_c_8 comp_aug
rename _Ipartner_c_9 comp_sep
rename _Ipartner_c_10 comp_oct
rename _Ipartner_c_11 comp_nov
rename _Ipartner_c_12 comp_dec


* Rename some other vars, to make the plot nicer when they are displayed
rename partner_age Age
rename partner_ethnicity Ethnicity
rename partner_employed Employed


** Now, run a GSA to see the amount of residual confounding necessary to make this association (essentially) null. NOTE: Due to the way the GSA algorithm works, it is not possible to select a 'tau' or 'tstat' of 0 (i.e., a null effect). This is because GSA requires positive values and will accept a simulated value if it falls within a given precision threshold of the target value (e.g., within 5%). If 'tau' or 'tstat' are set to 0, then GSA will only accept exact values of '0', which is improbable and the algorithm cannot proceed. As a slight fudge to get around this, you could set 'tau' or 'tstat' to near 0, and then increase the percision option so that the GSA algoritm accepts values within a t-statistic of 0 and 0.2 (say). E.g., if 'tstat' was set to 0.1, and 'precision' set to 100, all t-tstatistics within the range 0 and 0.2 would be considered (this is what I have done below).

* This takes about 10/15 minutes to run
gsa partner_donate partner_attend Age Ethnicity Vocational Olevel Alevel Degree ///
	Employed rural partner_finDiffs home_rent home_HA home_other imd_q2 imd_q3 ///
	imd_q4 imd_q5 comp_feb comp_mar comp_apr comp_may comp_jun comp_jul comp_aug ///
	comp_sep comp_oct comp_nov comp_dec, /// This is the regression model
	tstat(0.1) /// This is the 'target' t-statistic we're aiming for
	precision(100) /// This is the precision of statistic we're aiming for (see above)
	ylogit /// This is the model for the outcome variable (here, logistic)
	logit /// This is the model for the exposure variable (here, logistic)
	binu /// This specifies to model a binary unmeasured confounder
	scatter /// Print scatter plot of results on the figure afterwards
	corr /// Give partial correlations, rather than partial R2s, on plot
	nplots(8) /// Number of covariates plotted on figure (max = 10)
	seed(597680) // Set seed so reproducible

* Have made some manual tweaks to the plot before saving, including: Making points darker/more visible, editing axis text, removing some overlapping points, changing line colour to red, and making text darker/more visible.
graph export ".\Results\PartnerResults\partner_GSA.pdf", replace
graph save ".\Results\PartnerResults\partner_GSA.gph", replace


** This GSA says that the partial correlation between an unmeasured confounder and the exposure and outcome needs to be approximately 0.25 (for the outcome) and 0.2 (for the exposure) in order to explain away the association between religious attendance and blood donation. While difficult to judge out of context, these correlations are much stronger than those in the mother's GSA (of around 0.1-0.15). Plus, these partial correlations are much stronger than that observed between the closest measured confounder of degree-level education (as shown in the plot). While not impossible, an unmeasured confounder would need to have quite a strong association if the true exposure-outcome association was null.


** Sanity check that modelling a continuous unmeasured confounder doesn't impact results - Nope, results are practically identical
gsa partner_donate partner_attend Age Ethnicity Vocational Olevel Alevel Degree ///
	Employed rural partner_finDiffs home_rent home_HA home_other imd_q2 imd_q3 ///
	imd_q4 imd_q5 comp_feb comp_mar comp_apr comp_may comp_jun comp_jul comp_aug ///
	comp_sep comp_oct comp_nov comp_dec, /// This is the regression model
	tstat(0.1) /// This is the 'target' t-statistic we're aiming for
	precision(100) /// This is the precision of statistic we're aiming for (see above)
	ylogit /// This is the model for the outcome variable (here, logistic)
	logit /// This is the model for the exposure variable (here, logistic)
	scatter /// Print scatter plot of results on the figure afterwards
	corr /// Give partial correlations, rather than partial R2s, on plot
	nplots(8) /// Number of covariates plotted on figure (max = 10)
	seed(865675) // Set seed so reproducible
	
* Have made some manual tweaks to the plot before saving, including: Making points darker/more visible, editing axis text, removing some overlapping points, changing line colour to red, and making text darker/more visible.
graph export ".\Results\PartnerResults\partner_GSA_contU.pdf", replace
graph save ".\Results\PartnerResults\partner_GSA_contU.gph", replace



** Can also run another GSA, this time exploring the amount of unmeasured confounding necessary produce an association not longer 'statistically significant' (rather than the strong null, as modelled above)
gsa partner_donate partner_attend Age Ethnicity Vocational Olevel Alevel Degree ///
	Employed rural partner_finDiffs home_rent home_HA home_other imd_q2 imd_q3 ///
	imd_q4 imd_q5 comp_feb comp_mar comp_apr comp_may comp_jun comp_jul comp_aug ///
	comp_sep comp_oct comp_nov comp_dec, /// This is the regression model
	tstat(1.96) /// This is the 'target' t/z-statistic we're aiming for
	ylogit /// This is the model for the outcome variable (here, logistic)
	logit /// This is the model for the exposure variable (here, logistic)
	binu /// This specifies to model a binary unmeasured confounder
	scatter /// Print scatter plot of results on the figure afterwards
	corr /// Give partial correlations, rather than partial R2s, on plot
	nplots(8) /// Number of covariates plotted on figure (max = 10)
	seed(241319) // Set seed so reproducible
	
* Can see here that the level of unmeasured confounding to reduce the effect size to a z-statistic of 1.96 is approx. a 0.15 partial correlation with both the exposure and outcome - This is not a massive association, and is only slightly larger than the observed associations with degree-level education. This provides some assurance that a relatively sizeable level of residual confounding is necessary to alter the conclusions, but is not implausibly large to rule out. 

* Have made some manual tweaks to the plot before saving, including: Making points darker/more visible, editing axis text, removing some overlapping points, changing line colour to red, and making text darker/more visible.
graph export ".\Results\PartnerResults\partner_GSA_tstatNonSig.pdf", replace
graph save ".\Results\PartnerResults\partner_GSA_tstatNonSig.gph", replace


*** As the results of the 'confounders and/or mediators' model are so similar, I have not shown the GSA results here, as they will be practically identical


graph close _all
clear

