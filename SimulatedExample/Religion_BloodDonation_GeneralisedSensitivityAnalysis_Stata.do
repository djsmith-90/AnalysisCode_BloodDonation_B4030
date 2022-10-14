*** 'Generalised Sensitivity Analysis' for unmeasured confounding.
*** Created 10/10/2022 by Dan Major-Smith
*** Stata version 17


*** This 'generalised sensitivity analysis' (GSA) is conceptually similar to an E-value, in that it estimates the association between an unmeasured confounder and both the exposure and outcome necessary to alter a study's conclusions.

** Unlike E-values, GSA works with individual level data, so is more flexible than E-values (although requiring more processing time). GSA can also work with either continuous or binary unmeasured confounders. Also, rather than working on the risk ratio scale (as with E-values), GSA assesses the associations via partial correlations or partial R^2 values. Despite these minor differences, overall the aim and application of both E-values and GSA is similar: to detect the amount of unmeasured confounding necessary to alter a study's conclusions.


*** Install the 'gsa' package, if needed
*ssc install gsa, replace


*** Start by simulating some data. Will use the same example as with E-values in the associated R script. That is, the exposure is a binary religiosity variable, the outcome is a binary cooperation/blood donation variable, socioeconomic position (SEP) is a binary confounder which causes both the exposure and the outcome, while 'U' is an unmeasured binary confounder that also causes the exposure and outcome.


** Clear data, set the number of observations, and set a seed so reproducible
clear
set obs 15000
set seed 54321

* Simulate SEP - As not caused by anything, just do 50/50 split
gen sep = rbinomial(1, 0.5)
tab sep

* Simulate U - As not caused by anything, just do a 50/50 split
gen u = rbinomial(1, 0.5)
tab u

* Simulate religiosity - Caused by SEP and U, with greater SEP and greater U = greater religiosity. Approx. 50% religious
gen relig_p = invlogit(log(0.35) + (log(2) * sep) + (log(3) * u))
gen relig = rbinomial(1, relig_p)
tab relig

* Simulate blood donation - Caused by SEP, U and religiosity, with all increasing blood donation (approx. 30% donated)
gen donate_p = invlogit(log(0.1) + (log(2) * sep) + (log(3) * u) + (log(2) * relig))
gen donate = rbinomial(1, donate_p)
tab donate


** Logistic regression model for the true model (with both SEP and U as confounders)
logistic donate relig sep u

* Now run model without U (as in a real analysis it would be unknown, and therefore excluded) - Returns an biased estimate of the religion-blood donation association (OR = 2.45, rather than the true value of 1.92)
logistic donate relig sep



**** If we suspect that unmeasured confounding may be an issue, we can use quantitative bias analyses like Generalised Sensitivity Analyses (or E-values) to examine the strength of unmeasured confounding necessary to alter an observed effect.

** For the 'gsa' command, you first specify the regression equation of interest. The 'gsa' command then has several options:
* - tau: specifies the target size of the coefficient of the treatment variable. E.g., if the observed treatment effect is 2, then setting tau to 1 will display the results (the contour in the resulting figure) which halves the observed effect size.
* - tstat: Specifies the target size of the t-statistic of the treatment variable. i.e., if select 1.96 then the contour plot shows the bias necessary to make the result 'non-significant' at the 95% level
* - correlation: Specifies showing the partial corrleations on the contour plot (rather than the default partial R2 parameters)
* - binu: Generates binary unmeasured confounders, instead of the default continuous
* - You also have to specify the outcome equations (e.g., 'ycontinuous' for continuous outcomes, 'ylogit' for binary outcomes estimated with logit, etc.). The same also applies to the exposure/treatment variables (e.g., 'continuous' if the exmposure is continuous, 'logit' for binary estimated with logit, etc.).


*** NOTE 1: Due to the way the GSA algorithm works, it is not possible to select a 'tau' or 'tstat' of 0 (i.e., a null effect). This is because GSA requires positive values and will accept a simulated value if it falls within a given precision threshold of the target value (e.g., within 5%). If 'tau' or 'tstat' are set to 0, then GSA will only accept exact values of '0', which is improbable and the algorithm cannot proceed. As a slight fudge to get around this, you could set 'tau' or 'tstat' to near 0, and then increase the percision option so that the GSA algoritm accepts values within a t-statistic of 0 and 0.2 (say). E.g., if 'tstat' was set to 0.1, and 'precision' set to 100, all t-tstatistics within the range 0 and 0.2 would be considered.

*** NOTE 2: This GSA approach also differs from the E-value method in that GSA is based on generating plausible unobserved variables from the observed data (while E-value just uses summary data). This means that it may not be possible to simulate a given unobserved confounder if the target value is impossible given the data available. For instance, in the simulated example above, as the true log-odds association between religiosity and blood donation when adjusting for the true 'U' is approx. log(2) = 0.69, setting a 'tau' value much below this may lead to an error message as there is simply not enough variation in the dataset to simulate an unobserved confounder associated with both the exposure and outcome to reduce the effect size this much.

*** NOTE 3: While testing this method on the simple simulated example, it seems that there are quite a few estimation problems when both the exposure and outcome are binary and specified using 'logit' and 'ylogit' options. In note 2 above, the true log-odds estimate for the religiosity-blood donation association is 0.69, yet even if we set the 'tau' value to be above this (0.75), the analysis still does not run correctly. In the GSA documentation it does say that using a linear probability model ('lpm' and 'ylmp' options) may help estimation if the exposure and/or outcome are binary. This does appear to help, although at the expense of changing the intepretation of the coefficients from log-odds estimates (with logistic models) to the proportional increase in the outcome associated with the exposure (with linear probability models; e.g., blood donation being 17% higher among religious individuals compared to non-religious individuals). This GSA method does seem much more robust when the exposure and/or the outcome are continuous. However, at the end of the script I test this method with binary exposures and outcomes using a real-world dataset, and the approach works fine - Perhaps there is not enough variation in the simulated example, which causes the method to fall over. 

*** NOTE 4: If you get the warning 'Error: c1 and/or c2 are too small', you can increase these using the 'maxc1()' and 'maxc2()' options, which may help estimation. c1 and c2 are parameters that determine the association between the simulated unmeasured confounder and the exposure and outcome, respectively. However, even increasing these maximum values does not always fix the problem, and more often than not this error message means that it is not possible to estimate the target value given the observed data and model.


*** Example GSA

** First, look at the effect size of interest (on the log-odds scale here)
logit donate relig sep

* Observed effect size is a log-odds coefficient of 0.9. Will first specify a tau value of 0.45 to see the amount of confounding necessary to halve this effect size.
gsa donate relig sep, tau(0.45) logit ylogit binu scatter seed(1234) // Not work, as get 'c1 and/or c2 are too small' warning
gsa donate relig sep, tau(0.45) logit ylogit binu scatter maxc1(10) maxc2(5) seed(1234) // Still get same warning, so this target value seems to be a non-starter

* Can try increasing the tau value to 0.75, as we know this is possible to simulate from the observed data (as the log-odds coefficient when including the unmeasured confounder 'u' is 0.65, so there ought to be enough variation in the data to simulate this). Nonetheless, for reasons I'm not 100% sure on this method still does not work.
logit donate relig sep u

gsa donate relig sep, tau(0.75) logit ylogit binu scatter maxc1(10) maxc2(5) seed(1234) // Not work

* Method also doesn't work if specify 'tstat', rather than 'tau', as target value
gsa donate relig sep, tstat(1.96) logit ylogit binu scatter maxc1(10) maxc2(5) seed(1234) // Not work

gsa donate relig sep, tstat(10) logit ylogit binu scatter maxc1(10) maxc2(5) seed(1234) // Not work


** Instead, can use a linear probability model, rather than a logit model, if exposure and/or outcome are binary. This does appear to help estimation, but at the expense of the results being on a different scale to the original analyses (as no longer in log-odds)
regress donate relig sep

** Explore the level of unmeasured confounding necessary to halve the observed association (from 0.17 to 0.085)
gsa donate relig sep, tau(0.085) lpm ylpm binu scatter seed(1234) // Does work

* To make plot from results above using partial correlation coefficients
gsagraph donate relig sep, tau(0.085) scatter cor
gsagraph donate relig sep, tau(0.085) scatter

** This analysis shows us the association between the unmeasured confounder and the exposure and outcome necessary to result in a halving of the exposure-outcome association. This plot informs us that any association on or beyond the blue line is sufficient to reduce the exposure-outcome assocation by half. So, if the partial correlation between U and the exposure is 0.7 (a partial R2 of 0.5) and the partial correlation between U and the outcome is 0.1 (partial R2 of 0.01), this would be sufficient to alter the study's result by said amount - The same could be said if the partial correlation between U and the exposure is 0.45 (a partial R2 of 0.2) and the partial correlation between U and the outcome is 0.2 (partial R2 of 0.04). This suggests that even though the association between U and the outcome could be relatively small, the association between U and the exposure would need to be quite substantial in order to change the results by this amount.

* This plot also 'benchmarks' these results against the observed confounders (here, 'SEP'). We can see that the partial correlations between SEP and the exposure - although not the outcome - are much weaker than required to change the result by said amount. If we think that SEP is likely to be a strong confounder of the exposure-outcome association, then from these results may may conclude that the strength of unmeasured confounding required to change our results is implausible.


** Can also base target value on t-statistic, rather than effect estimate. Will start with t = 1.96 (no longer 'statistically significant')
gsa donate relig sep, tstat(1.96) lpm ylpm binu scatter seed(1234) // Doesn't work, as c1 and/or c2 too small
gsa donate relig sep, tstat(1.96) lpm ylpm binu maxc1(10) maxc2(5) scatter seed(1234) // This does just about work if increase the max c1 and c2 values, but the plot is not very neat or informative, as points not fall on a consecutive line - Probably because t-statistic is too small to accurately estimate

* Increasing t-statistic to 10. This does work, but it's difficult to interpret a t-statistic of 10 - So if cannot use a convential t-statistic (such as 1.96), probably better to specify a tau/effect size instead.
gsa donate relig sep, tstat(10) lpm ylpm binu scatter seed(1234) // Does work, as increased the t-statistic to aim for

* To make plot from results above using partial correlation coefficients
gsagraph donate relig sep, tstat(10) scatter cor
gsagraph donate relig sep, tstat(10) scatter



****************************************************************************************
**** Example taken from the GSA documentation (with continuous outcome and binary exposure) - Just to demonstrate that this method does work!

** Read in the data
sysuse nlsw88, clear

* Create dummary variables of race and industry
tab race

xi i.race
rename _Irace_2 black
rename _Irace_3 other

tab industry

xi i.industry
rename _Iindustry_5 mnfctr

* Run a standard regression model with wage as outcome and union membership as exposure
sum wage
tab union

regress wage union age black other grade married south c_city mnfctr

* Perform GSA, with the target value 'tau' taken to be half the observed estimate
gsa wage union age black other grade married south c_city mnfctr, tau(.314) logit ycontinuous binu scatter nplots(8) // Does work


**** Testing method with both binary exposure and outcome, to see if works with 'real' data - Will convert 'wage' to binary using median split
sum wage, d
local median = r(p50)

gen wage_bin = .
replace wage_bin = 0 if wage < `median'
replace wage_bin = 1 if wage >= `median' & wage < .
tab wage_bin, m

logistic wage_bin union age black other grade married south c_city mnfctr

logit wage_bin union age black other grade married south c_city mnfctr

* Get to t-statistic of 1.96 (currently 5.64)
gsa wage_bin union age black other grade married south c_city mnfctr, tstat(1.96) ylogit logit binu scatter nplots(8) seed(1234)


*** This works fine, so there must not be enough variation in the simulated data to allow the GSA to work correctly - Hopefully with the real-world ALSPAC data this approach will work fine (with linear probability models as a back-up)


