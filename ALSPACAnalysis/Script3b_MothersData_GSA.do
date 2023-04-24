*** Script for paper 'Exploring causality from observational data: An example assessing whether religiosity promotes cooperation' using ALSPAC data (ALSPAC B-number B4030)
*** Script 3b: Generalised Sensitivity Analysis to explore residual confounding in mother's data
*** Created 22/2/2023 by Dan Major-Smith
*** Stata version 17

** Analysis plan for this paper has been accepted as a Registered Report for the journal Evolutionary Human Sciences, and the plan is available on the OSF: https://osf.io/z5gcm/


**** NOTE: The main analyses for this data have been conducted in R - Please see the associated "Script3_AnalysingMothersData.r" script ****


*** Install the 'gsa' package, if needed
*ssc install gsa, replace


************************************************************************************
*** Set working directory and read in the dataset
cd "X:\Studies\RSBB Team\Dan\B4030 - RSBB and cooperation"

use "data_mum_processed_B4030.dta", clear

** Also move exposures and outcome to end of dataset, to match synthesised data
order mum_belief-mum_donate, last

** If using synthesised data here, read this dataset in instead - NOTE: The sample size is slightly smaller in the synthetic dataset, as unique replicates with identical data in both the observed and synthetic data have been removed for confidentiality reasons. Also, the statistics in the script below refer to the observed data, and will differ for the synthetic data
*use ".\AnalysisCode_BloodDonation_B4030\ALSPACAnalysis\syntheticData_mum_B4030.dta", clear


** Quick check of the data
describe
codebook


** Will need to recode the exposures and outcome to 0 and 1, in order for logistic model and GSA to work
recode mum_donate (1 = 0) (2 = 1)
label define mum_donate 0 "No" 1 "Yes", modify
tab mum_donate, m

recode mum_belief (1 = 0) (2 = 1)
label define mum_belief 0 "No" 1 "Yes", modify
tab mum_belief, m

recode mum_identity (1 = 0) (2 = 1)
label define mum_identity 0 "None" 1 "Religious", modify
tab mum_identity, m

recode mum_attend (1 = 0) (2 = 1)
label define mum_attend 0 "Occasional/None" 1 "Regular", modify
tab mum_attend, m


** Also make a complete case marker, and drop any observations with missing data
egen cca_marker = rowmiss(mum_belief mum_identity mum_attend mum_donate ///
	mum_age mum_ethnicity mum_edu home imd rural mum_marital parity ///
	mum_locus mum_finDiffs mum_health mum_employed mum_compMonth comp_postPreg)
	
tab cca_marker, m

drop if cca_marker != 0
drop cca_marker


*** From the main analysis in the R script, we know that there is little association between religious belief or identity and blood donation, in adjusted analyses; for religious attendance there appears to be a weak association, at best. These are not the best circumstances to perform a Generalised Sensitivity Analysis (GSA), as there's very little association to test! Nonetheless, to demonstrate these methods I will apply this GSA method to the model with attendance as the exposure, adjusting for assumed confounders only.

* Here's the model again
logit mum_donate i.mum_attend mum_age i.mum_ethnicity i.mum_edu i.home i.imd ///
	i.rural i.mum_finDiffs i.mum_employed i.mum_compMonth i.comp_postPreg

	
** Before running GSA, need to convert categorical variables to dummy variables (as GSA doesn't accept the "i." notation)

* Mother's education
tab mum_edu
xi i.mum_edu
rename _Imum_edu_2 Vocational
rename _Imum_edu_3 Olevel
rename _Imum_edu_4 Alevel
rename _Imum_edu_5 Degree

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
tab mum_compMonth
xi i.mum_compMonth
rename _Imum_compM_2 comp_feb
rename _Imum_compM_3 comp_mar
rename _Imum_compM_4 comp_apr
rename _Imum_compM_5 comp_may
rename _Imum_compM_6 comp_jun
rename _Imum_compM_7 comp_jul
rename _Imum_compM_8 comp_aug
rename _Imum_compM_9 comp_sep
rename _Imum_compM_10 comp_oct
rename _Imum_compM_11 comp_nov
rename _Imum_compM_12 comp_dec


* Rename some other vars, to make the plot nicer when they are displayed
rename mum_age Age
rename mum_ethnicity Ethnicity
rename mum_employed Employed


** Now, run a GSA to see the amount of residual confounding necessary to make this association (essentially) null. NOTE: Due to the way the GSA algorithm works, it is not possible to select a 'tau' or 'tstat' of 0 (i.e., a null effect). This is because GSA requires positive values and will accept a simulated value if it falls within a given precision threshold of the target value (e.g., within 5%). If 'tau' or 'tstat' are set to 0, then GSA will only accept exact values of '0', which is improbable and the algorithm cannot proceed. As a slight fudge to get around this, you could set 'tau' or 'tstat' to near 0, and then increase the percision option so that the GSA algoritm accepts values within a t-statistic of 0 and 0.2 (say). E.g., if 'tstat' was set to 0.1, and 'precision' set to 100, all t-tstatistics within the range 0 and 0.2 would be considered (this is what I have done below).

* This takes about 15/20 minutes to run
gsa mum_donate mum_attend Age Ethnicity Vocational Olevel Alevel Degree ///
	Employed rural mum_finDiffs home_rent home_HA home_other imd_q2 imd_q3 ///
	imd_q4 imd_q5 comp_feb comp_mar comp_apr comp_may comp_jun comp_jul comp_aug ///
	comp_sep comp_oct comp_nov comp_dec comp_postPreg, /// This is the regression model
	tstat(0.1) /// This is the 'target' t-statistic we're aiming for
	precision(100) /// This is the precision of statistic we're aiming for (see above)
	ylogit /// This is the model for the outcome variable (here, logistic)
	logit /// This is the model for the exposure variable (here, logistic)
	binu /// This specifies to model a binary unmeasured confounder
	scatter /// Print scatter plot of results on the figure afterwards
	corr /// Give partial correlations, rather than partial R2s, on plot
	nplots(7) /// Number of covariates plotted on figure (max = 10)
	seed(377727) // Set seed so reproducible

* Have made some manual tweaks to the plot before saving, including: Making points darker/more visible, editing axis text, changing line colour to red, and making text darker/more visible.
graph export ".\Results\MotherResults\mother_GSA.pdf", replace
graph save ".\Results\MotherResults\mother_GSA.gph", replace


** This GSA says that the partial correlation between an unmeasured confounder and the exposure and outcome needs to be approximately 0.1 (for the outcome) and 0.15 (for the exposure) in order to explain away the association between religious attendance and blood donation. While difficult to judge out of context, correlations of around 0.1-0.15 do not seem *that* strong. In fact, these are approximately the partial correlations between degree-level education and the exposure and outcome (as shown in the plot), so an unmeasured association of this magnitude is well within the realms of possibility.


** Sanity check that modelling a continuous unmeasured confounder doesn't impact results - Nope, results are practically identical
gsa mum_donate mum_attend Age Ethnicity Vocational Olevel Alevel Degree ///
	Employed rural mum_finDiffs home_rent home_HA home_other imd_q2 imd_q3 ///
	imd_q4 imd_q5 comp_feb comp_mar comp_apr comp_may comp_jun comp_jul comp_aug ///
	comp_sep comp_oct comp_nov comp_dec comp_postPreg, /// This is the regression model
	tstat(0.1) /// This is the 'target' t-statistic we're aiming for
	precision(100) /// This is the precision of statistic we're aiming for (see above)
	ylogit /// This is the model for the outcome variable (here, logistic)
	logit /// This is the model for the exposure variable (here, logistic)
	scatter /// Print scatter plot of results on the figure afterwards
	corr /// Give partial correlations, rather than partial R2s, on plot
	nplots(7) /// Number of covariates plotted on figure (max = 10)
	seed(729359) // Set seed so reproducible
	
* Have made some manual tweaks to the plot before saving, including: Making points darker/more visible, editing axis text, changing line colour to red, and making text darker/more visible.
graph export ".\Results\MotherResults\mother_GSA_contU.pdf", replace
graph save ".\Results\MotherResults\mother_GSA_contU.gph", replace



** Can also run another GSA, this time exploring the amount of unmeasured confounding necessary to half the observed effect estimate (from a log-odds of 0.14 to a log-odds of 0.07) - This uses the 'tau' option, rather than 'tstat'
gsa mum_donate mum_attend Age Ethnicity Vocational Olevel Alevel Degree ///
	Employed rural mum_finDiffs home_rent home_HA home_other imd_q2 imd_q3 ///
	imd_q4 imd_q5 comp_feb comp_mar comp_apr comp_may comp_jun comp_jul comp_aug ///
	comp_sep comp_oct comp_nov comp_dec comp_postPreg, /// This is the regression model
	tau(0.07) /// This is the 'target' effect size we're aiming for
	ylogit /// This is the model for the outcome variable (here, logistic)
	logit /// This is the model for the exposure variable (here, logistic)
	binu /// This specifies to model a binary unmeasured confounder
	scatter /// Print scatter plot of results on the figure afterwards
	corr /// Give partial correlations, rather than partial R2s, on plot
	nplots(7) /// Number of covariates plotted on figure (max = 10)
	seed(494856) // Set seed so reproducible
	
* Can see here that the level of unmeasured confounding to reduce the effect size to a log-odds of 0.07 is approx. a 0.1 partial correlation with both the exposure and outcome - This is a rather low threshold, is similar to the association with A-level education, and smaller than the association with degree-level education. In sum, not much unmeasured confounding is necessary to alter the effect estimate by this much.	

* Have made some manual tweaks to the plot before saving, including: Making points darker/more visible, editing axis text, changing line colour to red, and making text darker/more visible.
graph export ".\Results\MotherResults\mother_GSA_tauHalf.pdf", replace
graph save ".\Results\MotherResults\mother_GSA_tauHalf.gph", replace


*** As the results of the 'confounders and/or mediators' model are so similar, I have not shown the GSA results here, as they will be practically identical


graph close _all
clear

