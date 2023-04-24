### ALSPAC Religion and Cooperation paper - Example simulation script
### Created 7/10/2022 by Dan Major-Smith
### R 4.0.4


#### Using simple simulated examples, this script aims to introduce the methods described in the paper to explore, and potentially overcome, some of the possible biases when trying to estimate causal effects from observational research. The first two sections cover different aspects of confounding bias (confounder mis-specification of observed covariates and residual confounding of unmeasured covariates), while the third concerns selection bias due to missing data.


###################################################################################################################
#### Clear workspace and install/load packages

rm(list = ls())

#install.packages("dagitty")
#install.packages("EValue")
#install.packages("mice")
#install.packages("ggplot2")

library(dagitty)
library(EValue)
library(mice)
library(ggplot2)

## Note that the 'NARMICE' package for not-at-random multiple imputation is based on the 'mice' package, so has to be installed in a different location to avoid over-writing the original 'mice' package (as we use the original 'mice' package first, we will load this NARMICE package later on).
#library(devtools)
#install_github("moreno-betancur/mice",
#               lib = "C:/Temp/mice_test")




####################################################################################################################
#### 1) Confounder mis-specification of observed covariates

### We will first describe how uncertainty over the true confounding model can result in biased causal estimates. This may be because it is not clear whether some variables are confounders (cause both the exposure and outcome), or mediators (caused by the exposure, and in turn cause the outcome), or both (reciprocal causation). The simple directed acyclic graphs (DAGs) below illustrate these different causal structures. In all examples we will use 'religiosity' as our exposure and 'blood donation' as our outcome.


## Confounding: As described above, a confounder is a variable which causes both the exposure and the outcome. Here, we may expect socioeconomic position (SEP) to cause both religion (in the ALSPAC parents sample higher SEP is associated with greater religiosity; https://wellcomeopenresearch.org/articles/7-159) and greater levels of blood donation. The canonical confounding DAG looks like this:
dag_confounding <- dagitty('dag {
                Religiosity [pos = "0,1"]
                SEP [pos = "1,0"]
                BloodDonation [pos = "2,1"]
                
                Religiosity -> BloodDonation
                SEP -> Religiosity
                SEP -> BloodDonation
                }')
plot(dag_confounding)

## When attempting causal inference, not adjusting for a confounder will result in a biased exposure-outcome association. We can demonstrate this by simulating data based on this DAG (for simplicity, will assume all variables are binary and n = 15,000 [the approx. size of the ALSPAC dataset])
n <- 15000

# Set a seed, so is reproducible
set.seed(883751)

# Simulate SEP - As not caused by anything, just do 50/50 split
sep <- rbinom(n = n, size = 1, prob = 0.5)
table(sep)

# Simulate religiosity - Caused by SEP, with greater SEP = greater religiosity. Approx. 50% religious
relig_p <- plogis(log(0.6) + (log(3) * sep))
relig <- rbinom(n = n, size = 1, prob = relig_p)
table(relig)

# Simulate blood donation - Caused by SEP and religiosity, with both increasing blood donation (approx. 30% donated)
donate_p <- plogis(log(0.2) + (log(3) * sep) + (log(2) * relig))
donate <- rbinom(n = n, size = 1, prob = donate_p)
table(donate)


## Logistic regression model which does not adjust for SEP - Returns a biased estimate of the religion-blood donation association (should be OR = 2, but here OR = 2.49)
mod1 <- glm(donate ~ relig, family = "binomial")
exp(cbind(OR = coef(summary(mod1))[, 1], confint(mod1)))

## Now run adjusting for SEP - Returns an unbiased estimate of the religion-blood donation association (OR = approx. 2)
mod2 <- glm(donate ~ relig + sep, family = "binomial")
exp(cbind(OR = coef(summary(mod2))[, 1], confint(mod2)))



### In other instances, however, we may not know that true confounding structure of the data. Let's use the example in figure 2 of the paper, and say that there is reciprocal causation between marital status and religiosity (with marital status causing religiosity, and religiosity causing marital status), meaning that marital status is both a confounder and and mediator of the religiosity-blood donation association. Let's DAG this up (we'll ignore other sources of confounding, such as SEP, in this example):
dag_recip <- dagitty('dag {
                Religiosity [pos = "1,1"]
                MaritalStatus_t1 [pos = "0,0"]
                MaritalStatus_t2 [pos = "2,0"]
                BloodDonation [pos = "3,1"]
                
                Religiosity -> BloodDonation
                MaritalStatus_t1 -> Religiosity
                Religiosity -> MaritalStatus_t2
                MaritalStatus_t1 -> MaritalStatus_t2
                MaritalStatus_t2 -> BloodDonation
                }')
plot(dag_recip)


## If marital status was measured at only time 2, then we may not be able to accurately estimate the true causal effect of religiosity on blood donation; this is because to estimate an unbiased causal effect between religiosity and blood donation we would need to adjust for marital status at time 1 (as it confounds both religiosity and blood donation) but *not* adjust for marital status at time 2 (as it is a mediator between religiosity and blood donation). Let's simulate this example to show this:

# Set a seed, so is reproducible
set.seed(883751)

# Marital status at time 1 - Not caused by anything, so say 50% married
marital_t1 <- rbinom(n = n, size = 1, prob = 0.5)
table(marital_t1)

# Religiosity - Caused by marital status, with marriage increasing religiosity - Approx 50% religious
relig_p <- plogis(log(0.7) + (log(2) * marital_t1))
relig <- rbinom(n = n, size = 1, prob = relig_p)
table(relig)

# Marital status at time 2 - Caused by marital status at time 1 (people married at time 1 also married at time 2) and religiosity (religious individuals more likely to get or remain married) - Again, approx 50% married
marital_t2_p <- plogis(log(0.25) + (log(10) * marital_t1) + (log(2) * relig))
marital_t2 <- rbinom(n = n, size = 1, prob = marital_t2_p)
table(marital_t2)

# Blood donation - Caused by marital status at time 2 and religiosity (both increasing donations) - Approx. 30% donated
donate_p <- plogis(log(0.2) + (log(2) * marital_t2) + (log(2) * relig))
donate <- rbinom(n = n, size = 1, prob = donate_p)
table(donate)


## Logistic regression model to obtain true/unbiased causal effect - requires adjustment for marital status at time 1, not time 2 (note that this OR is higher than 2, as it combines both the direct effect of religiosity on blood donation - which does have an OR of 2 - and the indirect effect of religiosity on blood donation via marital status at time 2).
mod1 <- glm(donate ~ relig + marital_t1, family = "binomial")
exp(cbind(OR = coef(summary(mod1))[, 1], confint(mod1)))


## In many cases, however, we may not have repeated covariate data, so instead only marital status at time 2 is observed, but not at time 1. We also may not know whether a certain variable is a confounder, a mediator, or both (or neither). In situations like this estimating an unbiased causal effect may not be possible. However, as described in the paper, we can bracket these plausible minimum and maximum estimates by repeating the analyses both with and without the potential confounder(s)/mediator(s). By removing assumed confounders/mediators, the model is likely to 'under-control' for confounding; in contrast, the model with the assumed confounders/mediators is likely to 'over-control' for confounding. The true causal effect is likely to fall between these two extremes. We will demonstrate this below, using the data simulated above and imagining that marital status at time 2 is observed, while marital status at time 1 is unobserved.

# Model not adjusting for marital status at time 2 (under-controlling for confounding = over-estimate of true causal effect)
mod2 <- glm(donate ~ relig, family = "binomial")
exp(cbind(OR = coef(summary(mod2))[, 1], confint(mod2)))

# Model adjusting for marital status at time 2 (over-controlling for confounding = under-estimate of true causal effect)
mod3 <- glm(donate ~ relig + marital_t2, family = "binomial")
exp(cbind(OR = coef(summary(mod3))[, 1], confint(mod3)))

## In this example, the over-estimate when not controlling for marital status at time 2 (mod2) was an OR of 2.37, and the under-estimate when adjusting for marital status at time 2 (mod3) was an OR of 2.05. By comparing these different models - and assuming no other sources of bias - we can state that the true causal effect is likely to fall between these two estimates (which it does, as the true effect (mod1) was an odds ratio of 2.24).



### This logic is flipped if the direction of confounding goes in the opposite direction, however. For instance, say that being married increases the probability of being religious but decreases the probability of blood donation. Now not adjusting for marital status at time 2 (under-controlling for confounding) results in an under-estimate of the true causal effect (OR = 1.64 vs true OR of 1.77), while adjusting for marital status at time 2 (over-controlling for confounding) results in an over-estimate of the true causal effect (OR = 1.93). Regardless, the true causal effect is still bracketed by these models.
donate_p <- plogis(log(0.3) + (log(0.5) * marital_t2) + (log(2) * relig))
donate <- rbinom(n = n, size = 1, prob = donate_p)
table(donate)

## Logistic regression model to obtain true/unbiased causal effect 
mod1 <- glm(donate ~ relig + marital_t1, family = "binomial")
exp(cbind(OR = coef(summary(mod1))[, 1], confint(mod1)))

# Model not adjusting for marital status at time 2 (under-controlling for confounding)
mod2 <- glm(donate ~ relig, family = "binomial")
exp(cbind(OR = coef(summary(mod2))[, 1], confint(mod2)))

# Model adjusting for marital status at time 2 (over-controlling for confounding)
mod3 <- glm(donate ~ relig + marital_t2, family = "binomial")
exp(cbind(OR = coef(summary(mod3))[, 1], confint(mod3)))



####################################################################################################################
#### 2) Residual confounding due to unmeasured covariates

### In this section, we will demonstrate how residual confounding due to unmeasured covariates can bias causal estimates, and introduce quantitative bias analyses to explore the extent of residual confounding required to change the study's conclusions. In this script will will show how to apply the 'E-value' approach (using the 'EValue' package), while the Stata script demonstrates how to apply the conceptually similar 'Generalised Sensitivity Analysis' method (which is not available in R).

## For this example, we will use the same SEP-example as above, but with an unmeasured confounder 'U' (this may be something like social desirability bias, or a measure of personality, which causes both religiosity and blood donation). The updated DAG looks like:
dag_residConfounding <- dagitty('dag {
                Religiosity [pos = "0,2"]
                SEP [pos = "1,1"]
                U [pos = "1,0"]
                BloodDonation [pos = "2,2"]
                
                Religiosity -> BloodDonation
                SEP -> Religiosity
                SEP -> BloodDonation
                U -> Religiosity
                U -> BloodDonation
                }')
plot(dag_residConfounding)


## Let's simulate this scenario

# Set a seed, so is reproducible
set.seed(883751)
n <- 15000

# Simulate SEP - As not caused by anything, just do 50/50 split
sep <- rbinom(n = n, size = 1, prob = 0.5)
table(sep)

# Simulate U - As not caused by anything, just do a 50/50 split
u <- rbinom(n = n, size = 1, prob = 0.5)
table(u)

# Simulate religiosity - Caused by SEP and U, with greater SEP and greater U = greater religiosity. Approx. 50% religious
relig_p <- plogis(log(0.35) + (log(3) * sep) + (log(3) * u))
relig <- rbinom(n = n, size = 1, prob = relig_p)
table(relig)

# Simulate blood donation - Caused by SEP, U and religiosity, with all increasing blood donation (approx. 30% donated)
donate_p <- plogis(log(0.1) + (log(3) * sep) + (log(3) * u) + (log(2) * relig))
donate <- rbinom(n = n, size = 1, prob = donate_p)
table(donate)


## Logistic regression model for the true model (with both SEP and U as confounders)
mod1 <- glm(donate ~ relig + sep + u, family = "binomial")
exp(cbind(OR = coef(summary(mod1))[, 1], confint(mod1)))

## Now run model without U (as in a real analysis it would be unknown, and therefore excluded) - Returns an biased estimate of the religion-blood donation association (OR = 2.37, rather than the true value of 1.90)
mod2 <- glm(donate ~ relig + sep, family = "binomial")
exp(cbind(OR = coef(summary(mod2))[, 1], confint(mod2)))


### If we suspect that unmeasured confounding may be an issue, we can use quantitative bias analyses like E-values (or Generalised Sensitivity Analyses) to examine the strength of unmeasured confounding necessary to overturn an observed effect. Note that although the E-value reported from these methods is based on a single unmeasured confounder, this strength of confounding could represent composite bias from multiple confounders.

## The syntax for E-values is very simple. As our analysis is a logistic regression, we will use the odds ratio effect measure, using the command 'evalues.OR()'. The 'est' argument is the odds ratio point estimate from our model (here, 2.37), the 'lo' argument is the lower limit of the confidence interval, while the 'hi' argument is the upper limit of the confidence interval. The 'rare' argument is a logical argument saying whether the outcome is rare (<15%) or not; if the outcome is rare, then the E-value for the odds ratio is calculated using a risk ratio (as for rare outcomes the odds and risk ratios are broadly equivalent); if the event is not rare, then the E-value for the odds ratio is calculated using an approximate conversion from odds ratios to risk ratios by square-rooting the odds ratio (see  Table 2 of VanderWeele TJ, Ding P. Sensitivity Analysis in Observational Research: Introducing the E-Value. Annals of Internal Medicine. 2017; 167(4): 268-75). As the outcome here is not rare, we will use this conversion factor. The 'true' argument is the value to shift the observed point estimate against (for odds ratios, this is usually '1' to indicate a null effect, but could be altered if desired).
evalues.OR(est = 2.37, lo = 2.20, hi = 2.55, rare = FALSE, true = 1)

# The output here gives a simple table (even if the interpretation is somewhat less straight-forward). The first row ('RR') is the conversion of the odds ratios from our model to risk ratios. The second row gives the E-value necessary for an unmeasured confounder to reduce the observed effect to null (the 'point' column), or so that it is no longer 'statistically significant' at a given alpha threshold (based on the confidence intervals provided; the 'lower' column). This is on the risk ratio scale. In this example, this output is saying that to make the point estimate null, the association between the unmeasured confounder and both the exposure and outcome must be 2.45; that is, the unmeasured confounder must more than double the risk of both the exposure and the outcome to make this effect null. In the 'lower' column this E-value is 2.33, which is the risk ratio effect size needed for the unmeasured confounder on both the exposure and the outcome to make this effect no longer 'statistically significant' at the 95% level.

# Whether this effect size is plausible needs to be discussed and justified, but a larger E-value means that the strength of unmeasured confounding needs to be larger to overturn a result. A more than doubling of the risk to both the exposure and outcome does seem like quite a large effect size, so we may conclude that an unmeasured confounder of this size is unlikely to exist, and therefore that a causal interpretation of this observed effect may be warranted (this will of course vary from case-to-case).


## We can also make a bias plot showing not just the single E-value (i.e., the strength of unmeasured confounding in both the exposure and outcome to turn a causal effect to the null), but the full range of values necessary if one association is stronger than the other. RR_UD is the association between the unmeasured confounder and the outcome, while RR_EU is the association between the exposure and the unmeasured confounder, both on the risk ratio scale. Note that we have to specify the conversion factor - sqrt(2.37) - when working on the odds ratio scale with common outcomes to get the correct output.
bias_plot(sqrt(2.37), xmax = 10)

# And we can plot this using the lower CI as well, if we wanted to
bias_plot(sqrt(2.20), xmax = 10)


## In addition to moving an effect to the null, or be no longer 'statistically significant' at a given confidence interval, using an E-value approach is it also possible to specify a specific effect size one may be interested in. For instance, say we wanted to know the extent of residual confounding necessary to reduce the point estimate to '2' or lower - To do this, we can edit the 'true' argument.
evalues.OR(est = 2.37, lo = 2.20, hi = 2.55, rare = FALSE, true = 2)

# Now, to reduce the observed OR of 2.37 to 2 or lower, the strength of residual confounding would have to be a 40% increase in the risk of both the exposure and outcome caused by the unmeasured confounder (which is much lower than the 2.45 risk ratio necessary to overturn the result to the null). We might therefore conclude that residual confounding necessary to reduce the observed odds ratio to 2 is plausible, but not to reduce the observed effect to null.


## We can get some intuition of the size of this effect by running a series of poisson regressions to estimate the risk ratio association between U and the exposure and outcome, conditional on all other covariates (although ordinarily this would not be possible, of course!), and then looking at the true risk ratio associations.
mod_u_outcome <- glm(donate ~ u + relig + sep, family = "poisson")
exp(cbind(RR = coef(summary(mod_u_outcome))[, 1], confint(mod_u_outcome)))

mod_u_exposure <- glm(u ~ relig + donate + sep, family = "poisson")
exp(cbind(RR = coef(summary(mod_u_exposure))[, 1], confint(mod_u_exposure)))

## Here, the risk ratio between U and blood donation is 1.92 (the unmeasured confounder nearly doubles the risk of blood donation), while the risk ratio between religiosity U is 1.5 (religiosity increase the risk of the unmeasured confounder by 50%). These risk ratios are similar in magnitude to the known confounder SEP, and are lower than the E-value of 2.45 necessary to overturn this result. 

# When interpreting these results, we can use the effect sizes of observed confounders to help 'benchmark' these findings. For instance, if we think that SEP is likely to be the strongest confounder of the exposure-outcome association, then knowing that the E-value is greater than this effect means we may have more confidence that residual confounding is unlikely to overturn our result. In contrast, if we think that SEP is unlikely to be a particularly strong confounder, then we may conclude that the risk of residual confounding altering our conclusions may be much higher. These are qualitative judgments, and need to be considered on a case-by-case basis.



## For a similar quantitative bias analysis approach to unmeasured confounding, see the associated 'Generalised Sensitivity Analysis' Stata script ("Religion_BloodDonation_GeneralisedSensitivityAnalysis_Stata.do").



####################################################################################################################
#### 3) Selection bias

### As described in the main manuscript, selection bias can occur at either selection into the study sample (e.g., study recruitment) or at selection into the analytic sample (e.g., missing data due to loss to follow-up), or both. Here we will focus on the latter case, and assume that there is no/little selection bias due to recruitment into the study (in ALSPAC, this assumption is likely to be broadly plausible). 

### Selection bias occurs when both the exposure and the outcome are associated with selection into the study. This occurs because 'selection' acts as a collider variable, and is implicitly conditioned upon in any analysis using the observed data. We can demonstrate this in the simplest example of an exposure (religiosity) and an outcome (blood donation) which both cause missing data. Here's a canonical selection/collider bias DAG:
dag_selection1 <- dagitty('dag {
                Religiosity [pos = "0,0"]
                BloodDonation [pos = "2,0"]
                Selection [pos = "1,1"]
                
                Religiosity -> BloodDonation
                Religiosity -> Selection
                BloodDonation -> Selection
                }')
plot(dag_selection1)


## If we condition on the collider (here, selection), this will induce a biased association between the exposure and the outcome

## Let's simulate this scenario

# Set a seed, so is reproducible
set.seed(883751)
n <- 15000

# Simulate religiosity - Not caused by anything. Approx. 50% religious
relig <- rbinom(n = n, size = 1, prob = 0.5)
table(relig)

# Simulate blood donation - Caused by religiosity, with it increasing blood donation (approx. 30% donated)
donate_p <- plogis(log(0.3) + (log(2) * relig))
donate <- rbinom(n = n, size = 1, prob = donate_p)
table(donate)

# Simulate selection - Caused by religiosity and blood donation, with both increasing the risk of continued participation in the study (i.e., non-missing data) - Approx. 50% with observed data
selection_p <- plogis(log(0.4) + (log(3) * relig) + (log(3) * donate))
selection <- rbinom(n = n, size = 1, prob = selection_p)
table(selection)


## First, let's look at the true exposure-outcome association (i.e., no selection)
mod1 <- glm(donate ~ relig, family = "binomial")
exp(cbind(OR = coef(summary(mod1))[, 1], confint(mod1)))

## Now run model conditioning on those selected - That is, excluding those with missing data. The OR is now 1.57, rather than the true value of 2.12. 
mod2 <- glm(donate ~ relig, family = "binomial", subset = selection == 1)
exp(cbind(OR = coef(summary(mod2))[, 1], confint(mod2)))

## Conditioning on a collider results in bias because, in the selected sample, knowing the value of one variable provides information about the other. In this example, conditioning on selection provides information about both religiosity (as selected individuals are more religious) and blood donation (as selected individuals are more likely to donate); within the selected sample if we know someone is religious that means they are less likely to have donated blood (and vice versa), hence why the bias is towards the null. In this example conditioning on a collider results in bias towards the null because both the exposure and outcome are positively associated with selection, but if one variable was positively associated with selection and the other negatively associated, the direction of the bias would reverse. 


### Having illustrated the basic concept of selection and how it can result in bias, we will explore a more realistic scenario, and introduce methods to potentially overcome selection bias (and when these methods will and wont work).

## Take the example below as our DAG. Again, there is our exposure (religiosity) and our outcome (blood donation), but this time there is a confounder which causes both religiosity and blood donation (SEP) and a mediator (well-being) which is caused by religiosity (religion having a positive impact on well-being) and in turn causes blood donation (those with greater well-being being more likely to donate blood).
dag_selection2 <- dagitty('dag {
                Religiosity [pos = "0,2"]
                BloodDonation [pos = "2,2"]
                SEP [pos = "1,0"]
                Wellbeing [pos = "1,1"]
                
                Religiosity -> BloodDonation
                SEP -> Religiosity
                SEP -> BloodDonation
                Religiosity -> Wellbeing
                Wellbeing -> BloodDonation
                }')
plot(dag_selection2)


## Let's simulate this scenario

# Set a seed, so is reproducible
set.seed(883751)

# Simulate SEP - Not caused by anything, so just do 50/50 split
sep <- rbinom(n = n, size = 1, prob = 0.5)
table(sep)

# Simulate religiosity - Caused by SEP, with higher SEP = greater religiosity. Approx. 50% religious
relig_p <- plogis(log(0.6) + (log(3) * sep))
relig <- rbinom(n = n, size = 1, prob = relig_p)
table(relig)

# Simulate well-being - Caused by religiosity, with religiosity increasing well-being. Approx 70% high well-being (assuming binary variable for simplicity)
wellbeing_p <- plogis(log(1) + (log(3) * relig))
wellbeing <- rbinom(n = n, size = 1, prob = wellbeing_p)
table(wellbeing)

# Simulate blood donation - Caused by religiosity and well-being, with both increasing blood donation (approx. 30% donated)
donate_p <- plogis(log(0.15) + (log(2) * relig) +(log(3) * wellbeing))
donate <- rbinom(n = n, size = 1, prob = donate_p)
table(donate)


## First, let's look at the true exposure-outcome association on the whole dataset (i.e., no selection) - This involves adjusting for SEP (the confounder) but not adjusting for well-being (the mediator)
mod1 <- glm(donate ~ relig + sep, family = "binomial")
exp(cbind(OR = coef(summary(mod1))[, 1], confint(mod1)))


### Next, we'll introduce some selection into this model. To keep things simple, we'll imagine that all variables are fully-observed, other than our outcome blood donation - So it's clear that only this variable has missing data, we'll call missingness in this variable 'M_donation', which in turn causes 'selection' into the complete-case analysis. To show when standard multiple imputation will and wont recover the true parameter estimate, we'll consider two selection scenarios: i) Selection just caused by confounder, mediator and exposure; and ii) selection caused by all variables.

# The first selection scenario looks like this when DAG-ged up:
dag_selection3_i <- dagitty('dag {
                Religiosity [pos = "0,2"]
                BloodDonation [pos = "2,2"]
                SEP [pos = "1,0"]
                Wellbeing [pos = "1.33,1.33"]
                M_donation [pos = "1,3"]
                Selection [pos = "1,4"]
                
                
                Religiosity -> BloodDonation
                SEP -> Religiosity
                SEP -> BloodDonation
                Religiosity -> Wellbeing
                Wellbeing -> BloodDonation
                Religiosity -> M_donation
                SEP -> M_donation
                Wellbeing -> M_donation
                M_donation -> Selection
                }')
plot(dag_selection3_i)

# While the second selection scenario is the same, it has the outcome causing missingness in the blood donation outcome as well
dag_selection3_ii <- dagitty('dag {
                Religiosity [pos = "0,2"]
                BloodDonation [pos = "2,2"]
                SEP [pos = "1,0"]
                Wellbeing [pos = "1.33,1.33"]
                M_donation [pos = "1,3"]
                Selection [pos = "1,4"]
                
                
                Religiosity -> BloodDonation
                SEP -> Religiosity
                SEP -> BloodDonation
                Religiosity -> Wellbeing
                Wellbeing -> BloodDonation
                SEP -> M_donation
                Wellbeing -> M_donation
                Religiosity -> M_donation
                BloodDonation -> M_donation
                M_donation -> Selection
                }')
plot(dag_selection3_ii)


## Let's code these selection scenarios into different variables - Approx. 75% missing data in both (to have a strong selection effect). Note that the well-being effect is rather large, just to show how these methods can result in bias.

# First selection scenario
selection_i_p <- plogis(log(0.02) + (log(3) * sep) + (log(5) * wellbeing) + (log(3) * relig))
selection_i <- rbinom(n = n, size = 1, prob = selection_i_p)
table(selection_i)

# Second selection scenario
selection_ii_p <- plogis(log(0.013) + (log(3) * sep) + (log(5) * wellbeing) + (log(3) * relig) + (log(3) * donate))
selection_ii <- rbinom(n = n, size = 1, prob = selection_ii_p)
table(selection_ii)


## In both of these scenarios will we get biased estimates when performing complete case analyses. As a reminder, the true effect is 2.53:
exp(cbind(OR = coef(summary(mod1))[, 1], confint(mod1)))

# First selection scenario. In this scenario, where selection is only caused by the confounder (SEP), the mediator (well-being) and the exposure (religiosity), there is bias in the point estimate - from 2.53 to 2.20. Overall, the bias is not massive and the 95% confidence intervals of the model overlap with the true estimate. This bias occurs because one of the variables which causes selection - the mediator, well-being - is a cause of both blood donation and selection, and there is an open path between blood donation and selection; As well-being is not included in the analysis model - which it can't be, because it is a mediator, so would cause bias if adjusted for - this results in bias.
mod2_i <- glm(donate ~ relig + sep, family = "binomial", subset = selection_i == 1)
exp(cbind(OR = coef(summary(mod2_i))[, 1], confint(mod2_i)))

# Second selection scenario. In this scenario, selection is also caused directly by the outcome (blood donation), meaning that the strength of selection is much greater; from a true value of OR = 2.53 to 1.69 here.
mod2_ii <- glm(donate ~ relig + sep, family = "binomial", subset = selection_ii == 1)
exp(cbind(OR = coef(summary(mod2_ii))[, 1], confint(mod2_ii)))


#### These scenarios above show how selection can bias causal estimates. Next, we will demonstrate how to apply multiple imputation (MI) to try and recover the unbiased estimates.

## Briefly, MI works by imputing m datasets without missing data using a specified imputation model based on the observed data, performs the substantive analysis in each imputed dataset, and then combines the results together using Rubin's Rules. Like all methods, MI rests upon various assumptions, but if these assumptions are met then MI will return unbiased estimates. 

# A key assumption is that the imputation model to generate the imputed data is correctly specified, and meets the 'missing-at-random' assumption; that is, differences between the missing and observed values can be explained by the observed data. In the first scenario above, if we include well-being in the imputation model (in addition to SEP and religiosity), then this meets the missing-at-random assumption because missing outcome/blood donation values can be explained by observed well-being values. Thus, we can use 'well-being' as an auxiliary variable in our imputation model to make the missing-at-random assumption more plausible, even though we can't include it in the substantive analysis model (because it's a mediator).

# In the second scenario, however, the variable blood donation causes its own missingness (i.e., those who donated blood are less likely to have missing data). This means that the missing-at-random assumption is not met, and that standard MI will still return biased estimates. We would say that this scenario is 'missing-not-at-random', as differences between the missing and observed values *cannot* be explained by the observed data.


### To demonstrate this, we will start by performing standard multiple imputation using the 'mice' package which assumes that data are missing-at-random. Later, we will detail further MI methods which can relax this missing-at-random assumption, and explore how we can assess cases where we think data may be missing-not-at-random.

## Selection scenario 1 - Data known to be missing-at-random

# Put the relevant data into a data frame
df_i <- as.data.frame(cbind(sep, wellbeing, relig, donate, selection_i))
head(df_i)

# Code outcome as missing if 'selection_i' = 0, then drop this selection variable
df_i$donate[df_i$selection_i == 0] <- NA
df_i$selection_i <- NULL
head(df_i)

# Convert binary/categorical variables to factors (need this for MICE to work)
df_i$sep <- as.factor(df_i$sep)
df_i$wellbeing <- as.factor(df_i$wellbeing)
df_i$relig <- as.factor(df_i$relig)
df_i$donate <- as.factor(df_i$donate)

# Quick check of the data
summary(df_i)
str(df_i)


### Now for the MI analysis

# First, set-up the imputation methods for each variable. As the only variable with missing data is 'donate', and this is binary, we will impute using a logistic model (which is the default here)
meth_i <- make.method(df_i)
meth_i

# Second, set-up the prediction matrix, which says which variables to use to impute other variables (here, we want to use all variables when imputing donate)
pred_i <- make.predictorMatrix(df_i)
pred_i

# Run a test imputation to make sure it looks okay, and check the amount of missing data in each variable
test <- mice(df_i, m = 5, method = meth_i, predictorMatrix = pred_i, print = TRUE, maxit = 0)
test
table(test$nmis)

# Now run the proper imputation model. Will create 50 imputed datasets here (note that as there is only one variable with missing values to impute, we only need to use a burn-in period of 1 iteration - if there are >1 variables with missing data, this needs to be increased [say, a burn-in period of 10 iterations] so that the imputed values of one variable can influence the imputed values of another variable)
imp_i <- mice(df_i, m = 50, method = meth_i, predictorMatrix = pred_i, print = TRUE, maxit = 1, seed = 98765)

# Now run substantive analysis model in each imputed dataset and combine using Rubin's rules
model_i <- with(imp_i, glm(donate ~ relig + sep, family = "binomial"))
est_i <- pool(model_i)
(results_i <- summary(est_i, conf.int = TRUE))

# Exponentiate to gets results on odds ratio scale, and compare against both the 'true' values and the results from the complete-case model
cbind(OR = exp(results_i$estimate[results_i$term == "relig1"]), 
      lower_CI = exp(results_i$`2.5 %`[results_i$term == "relig1"]),
      upper_CI = exp(results_i$`97.5 %`[results_i$term == "relig1"]))

# The odds ratio from the imputed data is now practically identical to the true model (OR = 2.5), showing how we can recover unbiased causal estimates using multiple imputation (assuming all assumptions are met)
exp(cbind(OR = coef(summary(mod2_i))[, 1], confint(mod2_i)))
exp(cbind(OR = coef(summary(mod1))[, 1], confint(mod1)))



### Next, we will repeat the MI procedure above, but this time using the second selection scenario where data is missing-not-at-random (because the outcome causes its own missingness)

## Selection scenario 2 - Data known to be missing-not-at-random

# Put the relevant data into a data frame
df_ii <- as.data.frame(cbind(sep, wellbeing, relig, donate, selection_ii))
head(df_ii)

# Code outcome as missing if 'selection_ii' = 0, then drop this selection variable
df_ii$donate[df_ii$selection_ii == 0] <- NA
df_ii$selection_ii <- NULL
head(df_ii)

# Convert binary/categorical variables to factors (need this for MICE to work)
df_ii$sep <- as.factor(df_ii$sep)
df_ii$wellbeing <- as.factor(df_ii$wellbeing)
df_ii$relig <- as.factor(df_ii$relig)
df_ii$donate <- as.factor(df_ii$donate)

# Quick check of the data
summary(df_ii)
str(df_ii)


### Now for the MI analysis

# First, set-up the imputation methods for each variable. As the only variable with missing data is 'donate', and this is binary, we will impute using a logistic model (which is the default here)
meth_ii <- make.method(df_ii)
meth_ii

# Second, set-up the prediction matrix, which says which variables to use to impute other variables (here, we want to use all variables when imputing donate)
pred_ii <- make.predictorMatrix(df_ii)
pred_ii

# Run a test imputation to make sure it looks okay, and check the amount of missing data in each variable
test <- mice(df_ii, m = 5, method = meth_ii, predictorMatrix = pred_ii, print = TRUE, maxit = 0)
test
table(test$nmis)

# Now run the proper imputation model. Will create 50 imputed datasets here, again with a burn-in period of just 1 iteration
imp_ii <- mice(df_ii, m = 50, method = meth_ii, predictorMatrix = pred_ii, print = TRUE, maxit = 1, seed = 56789)

# Now run substantive analysis model in each imputed dataset and combine using Rubin's rules
model_ii <- with(imp_ii, glm(donate ~ relig + sep, family = "binomial"))
est_ii <- pool(model_ii)
(results_ii <- summary(est_ii, conf.int = TRUE))

# Exponentiate to gets results on odds ratio scale, and compare against both the 'true' values and the results from the complete-case model
cbind(OR = exp(results_ii$estimate[results_ii$term == "relig1"]), 
      lower_CI = exp(results_ii$`2.5 %`[results_ii$term == "relig1"]),
      upper_CI = exp(results_ii$`97.5 %`[results_ii$term == "relig1"]))

# Here, the odds ratio from the imputed data (OR = 2.00) is *not* identical to the true model (OR = 2.5). The imputed results are closer to the truth than the complete-case analysis results (OR = 1.69), showing that some of the bias has been addressed (this is the bias due to the mediator causing selection), but not all of this. This example shows how multiple imputation does not recover true parameter estimates, if the missing-at-random assumption is not met.
exp(cbind(OR = coef(summary(mod2_ii))[, 1], confint(mod2_ii)))
exp(cbind(OR = coef(summary(mod1))[, 1], confint(mod1)))


### To overcome, or at least explore, the impact of data being missing-not-at-random on our results, we can run an additional MI sensitivity analysis known as 'not-at-random multiple imputation by chained equations' (or, more simply, NARMICE). This method allows researchers to vary the missing-not-at-random assumption to examine how violations of this assumption can alter conclusions.

## For instance, say we had good reason to think that people who donated blood were more likely to have complete data. If we had a good idea of the strength of this bias, we could perform a probabilistic NARMICE analysis to see how this level of bias altered our results. However, often we do not know the strength of selection, so instead running a deterministic NARMICE analysis at various strengths of selection can give us an idea as to how stable - or not - our results may be to selection.

## Key to this NARMICE method is the 'conditional sensitivity parameter' (CSP), which is the difference between those with and without observed data on a given variable, conditional on all other variables in the imputation model. For instance, here the CSP will be applied to the blood donation variable, and specifies the difference in having blood donation data in those with missing blood donation data, relative to those without blood donation data, conditional on all other covariates in the imputation model. This is a bit awkward to think about or estimate, so to aid interpretation we can convert this CSP to either a 'marginal sensitivity parameter' (MSP), which is simply the difference in having blood donation data between the observed vs unobserved (*not* conditional on all other covariates) or a prevalence estimate of the variable in the population; both of these are more easy to estimate, and can aid interpretation. For this deterministic NARMICE analysis, we will explore a range of CSP values, and examine how these impact our conclusions.


### The first step of this analysis is to unload the standard mice package (if loaded), and load the user-written NARMICE extension to this mice package (details on this installation are at the top of the script)
detach(package:mice, unload = TRUE)
library(mice, lib.loc = "C:/Temp/mice_test")

# Put the relevant data into a data frame
df_narmice <- as.data.frame(cbind(sep, wellbeing, relig, donate, selection_ii))
head(df_narmice)

# Code outcome as missing if 'selection_ii' = 0. Additionally, for NARMICE, recommendation is to include missingness indicators for all variables with missing data, so will do that here - Will swap the 'selection_ii' variable so that missingness = 1, and rename this to 'M_donate', to make clearer.
df_narmice$donate[df_narmice$selection_ii == 0] <- NA
df_narmice$M_donate <- 1 - df_narmice$selection_ii
df_narmice$selection_ii <- NULL
head(df_narmice)

# Convert binary/categorical variables to factors (need this for MICE to work)
df_narmice$sep <- as.factor(df_narmice$sep)
df_narmice$wellbeing <- as.factor(df_narmice$wellbeing)
df_narmice$relig <- as.factor(df_narmice$relig)
df_narmice$donate <- as.factor(df_narmice$donate)
df_narmice$M_donate <- as.factor(df_narmice$M_donate)

# Quick check of the data
summary(df_narmice)
str(df_narmice)


## Set up prediction matrix for the imputation
ini <- mice(df_narmice, maxit = 0, print = TRUE)

# Specify the prediction matrix for the observable data - And edit so that the missingness markers don't predict the variables they represent
pred <- ini$predictorMatrix
pred

pred["donate", "M_donate"] <- 0
pred

# Set-up the prediction matrix for the unidentifiable part of the model (i.e., the missing-not-at-random element)
# In this case the whole matrix should be zeroes because the unidentifiable part of the imputation model contains a single constant (CSP*M) rather than additional contributions from the other variables in the dataset
predSens <- ini$predictorMatrix
predSens

predSens[predSens == 1] <- 0
predSens

# Set up list with sensitivity parameter values (currently all blank - to be filled in below)
pSens <- rep(list(list("")), ncol(df_narmice))
names(pSens) <- names(df_narmice)
pSens

# Set up vector describing manner of imputation for each variable - As we want to run vary the association between donation and it's missingness, we specify this as 'logregSens', which is a logistic sensitivity analysis.
meth <- ini$method
meth

meth["donate"] <- "logregSens"
meth

# Choose number of imputations and burn-in period - As only 1 variable to impute, the computational burden burden is quite low, so will run 50 imputations per CSP, with a burn-in period of 1 (note that with more variables to impute and a greater number of burn-in iterations, the time taken for this NARMICE method to run can quickly spiral upwards).
narmice_numimps <- 50
narmice_numiter <- 1

# To collect the parameters of interest
tipping <- as.data.frame(array(dim = c(dim = length(seq.int(-3, 1, by = 0.25)), 8))) # Number of sensitivity values we're going to try (varying the CSP from -3 to 1, in steps of 0.25), plus the number of parameters we're going to store (here, is 8 [see row below])
colnames(tipping) <- c("csp", "msp", "imor", "sampprev", "beta", "se_beta", "lower_ci", "upper_ci")
tipping

# Looping over delta/CSP values (i) - A CSP of -3 means that individuals with missing blood donation data have -3 lower log-odds of having given blood, compared to those with data (conditional on all other covariates); the converse applies to positive CSPs (i.e., those with missing data having greater likelihood of having given blood), while a CSP of 0 should approximately correspond to a standard MI model, as there is no adjustment for data potentially being missing-not-at-random. As we simulated this data, we know that the true CSP should be approximately log(0.33), which is -1.11 (we know this, because the true causal effect of donation on selection - i.e., having observed blood donation data - is log(3), which we reverse to get the odds ratio of having missing data); ordinarily, of course, we would not know this, hence why we are exploring a range of parameter values.
set.seed(918273)
k <- 0
for (i in seq.int(-3, 1, by = 0.25)) {
  k <- k+1 
  print(paste0("CSP = ", i))
  
  # specify a delta/CSP value for the prediction equation for missing data
  pSens[["donate"]]<-list(c(i))
  
  # NARMICE imputation
  imp_NARMICE <- mice(df_narmice, m = narmice_numimps, method = meth, predictorMatrix = pred,
                      predictorSens=predSens, parmSens=pSens, print = TRUE, maxit = narmice_numiter)
  
  # Derive the MSP for the given CSP value
  msp <- round(summary(pool(with(imp_NARMICE, 
                                 glm(donate ~ M_donate, family = "binomial")))), 3)
  
  # Derive the prevalence of blood donation in sample
  wholesampprev <- round(summary(pool(with(imp_NARMICE, 
                                           glm(donate ~ 1, family = binomial(link = "identity"))))), 3)
  
  # Calculate the adjusted odds ratio in these imputed datasets
  newest <- round(summary(pool(with(imp_NARMICE, 
                                    glm(donate ~ relig + sep, family = "binomial")))), 3)
  
  # Store these estimates in the 'tipping' dataframe
  tipping[k,1] <- i
  tipping[k,2] <- msp["M_donate1", "est"]
  tipping[k,3] <- exp(msp["M_donate1", "est"])
  tipping[k,4] <- wholesampprev["(Intercept)", "est"]  
  tipping[k,5] <- newest["relig2", "est"]
  tipping[k,6] <- newest["relig2", "se"]
  tipping[k,7] <- newest["relig2", "lo 95"]
  tipping[k,8] <- newest["relig2", "hi 95"]
  
  print(tipping[k,])
}

# Look at the 'tipping' output which contains all the values/estimates
tipping

# Convert the log odds to ORs
tipping$or <- exp(tipping$beta)
tipping$lowerCI_or <- exp(tipping$lower_ci)
tipping$upperCI_or <- exp(tipping$upper_ci)
tipping

## As mentioned above, as we simulated this data we know that the true CSP should be around -1.11. Looking at this table, we can see that the religiosity OR for CSP values near -1.11 are near the true value of 2.5. This is much closer to the truth than our previous standard MI results, which gave an OR = 2.00 (copied below for reference). So this NARMICE method appeared to do a good job of removing bias due to data being missing-not-at-random.
cbind(OR = exp(results_ii$estimate[results_ii$term == "relig1"]), 
      lower_CI = exp(results_ii$`2.5 %`[results_ii$term == "relig1"]),
      upper_CI = exp(results_ii$`97.5 %`[results_ii$term == "relig1"]))

## However, ordinarily we would not know the true CSP value, hence why we performed a tipping-point style NARMICE analysis to see how various CSP values impacted results. As can be seen in the table - and more clearly in the plots below - across all CSP values explored here, the association between religiosity and blood donation is still positive (from ~3.8 at the lowest CSP value, to ~1.6 at the highest CSP value). This suggests that the direction of this effect is robust even to severe selection effects; using the prevalence estimates and the MSP, we could also try to narrow this range further still to provide more accurate estimates.


## Plot the delta/CSP and IMOR (ignorable missingness odds ratio) and associated estimated difference in religiosity odds ratio. The blue dashed line gives the true CSP.
delta_csp_plot <- ggplot(data = tipping, aes(x = csp)) +
  geom_point(aes(y = or), size = 2, colour = "blue") +
  geom_line(aes(y = or), color = "blue") +
  geom_line(aes(y = lowerCI_or), linetype = "dashed", color = "green") +
  geom_line(aes(y = upperCI_or), linetype = "dashed", color = "red") +
  geom_vline(xintercept = 0, color = "red", size = 1) +
  geom_vline(xintercept = log(0.33), color = "blue", size = 1, linetype = "dotted") +
  geom_hline(yintercept = 1, color = "red", size = 1) +
  xlab("Delta/CSP value for blood donation") + ylab("Estimated religiosity OR") +
  theme_bw()
plot(delta_csp_plot)


# MSP
delta_msp_plot <- ggplot(data = tipping, aes(x = imor)) +
  geom_point(aes(y = or), size = 2, colour = "blue") +
  geom_line(aes(y = or), color = "blue") +
  geom_line(aes(y = lowerCI_or), linetype = "dashed", color = "green") +
  geom_line(aes(y = upperCI_or), linetype = "dashed", color = "red") +
  geom_vline(xintercept = 1, color = "red", size = 1) +
  geom_hline(yintercept = 1, color = "red", size = 1) +
  xlab("IMOR value for blood donation") + ylab("Estimated religiosity OR") +
  theme_bw() +
  scale_x_continuous(trans = "log", breaks = c(0.05, 0.1, 0.25, 0.5, 1, 2))
plot(delta_msp_plot)


## Also plot the estimated prevalence of blood donation in the sample, as alternative X-axis - Have put reference line at 30%, as a rough idea of 'true' prevalence
delta_prev_plot <- ggplot(data = tipping, aes(x = sampprev)) +
  geom_point(aes(y = or), size = 2, colour = "blue") +
  geom_line(aes(y = or), color = "blue") +
  geom_line(aes(y = lowerCI_or), linetype = "dashed", color = "green") +
  geom_line(aes(y = upperCI_or), linetype = "dashed", color = "red") +
  geom_vline(xintercept = 0.30, color = "red", size = 1) +
  geom_hline(yintercept = 1, color = "red", size = 1) +
  xlab("Blood donation prevalence") + ylab("Estimated religiosity OR") +
  theme_bw()
plot(delta_prev_plot)




### Simple simulation example to replicate NARMICE results in the paper, showing that if the exposure does not cause selection/missingness in the outcome, then all outcome MNAR scenarios will produce broadly similar results, regardless of CSP value

## DAG this up, then simulate some data
dag_selection3_iii <- dagitty('dag {
                Religiosity [pos = "0,2"]
                BloodDonation [pos = "2,2"]
                M_donation [pos = "1,3"]
                Selection [pos = "1,4"]
                
                
                Religiosity -> BloodDonation
                BloodDonation -> M_donation
                M_donation -> Selection
                }')
plot(dag_selection3_iii)

# Set a seed, so is reproducible
set.seed(883751)
n <- 15000

# Simulate religiosity - Caused by nothing. Approx. 50% religious
relig <- rbinom(n = n, size = 1, prob = 0.5)
table(relig)

# Simulate blood donation - Caused by religiosity, which increases blood donation (approx. 30% donated)
donate_p <- plogis(log(0.3) + (log(2) * relig))
donate <- rbinom(n = n, size = 1, prob = donate_p)
table(donate)


## First, let's look at the true exposure-outcome association on the whole dataset (i.e., no selection)
mod1 <- glm(donate ~ relig, family = "binomial")
exp(cbind(OR = coef(summary(mod1))[, 1], confint(mod1)))


### Next, we'll introduce some selection into this model. To keep things simple, we'll imagine that all variables are fully-observed, other than our outcome blood donation - So it's clear that only this variable has missing data, we'll call missingness in this variable 'M_donation', which in turn causes 'selection' into the complete-case analysis. Here, only the outcome 'blood donation' will cause missing data (so blood donation is MNAR) - Approx. 75% missing data in both (to have a strong selection effect)
selection_iii_p <- plogis(log(3) + (log(3) * donate))
selection_iii <- rbinom(n = n, size = 1, prob = selection_iii_p)
table(selection_iii)


## Note that, in this scenario, as just outcome is MNAR will not get bias (for binary outcomes with logistic model, only observe selection bias if both exposure *and* outcome relate to selection; this is different from continuous outcomes and linear models, where outcome-only MNAR will result in bias towards the null). As a reminder here, the true effect is an odds ratio of 2.11:
exp(cbind(OR = coef(summary(mod1))[, 1], confint(mod1)))

# Now to include selection - Result is very similar, an OR of 2.05
mod2_iii <- glm(donate ~ relig, family = "binomial", subset = selection_iii == 1)
exp(cbind(OR = coef(summary(mod2_iii))[, 1], confint(mod2_iii)))


### Now perform a NARMICE analysis, varying the outcome MNAR assumption

# Put the relevant data into a data frame
df_narmice <- as.data.frame(cbind(relig, donate, selection_iii))
head(df_narmice)

# Code outcome as missing if 'selection_iii' = 0. Additionally, for NARMICE, recommendation is to include missingness indicators for all variables with missing data, so will do that here - Will swap the 'selection_iii' variable so that missingness = 1, and rename this to 'M_donate', to make clearer.
df_narmice$donate[df_narmice$selection_iii == 0] <- NA
df_narmice$M_donate <- 1 - df_narmice$selection_iii
df_narmice$selection_iii <- NULL
head(df_narmice)

# Convert binary/categorical variables to factors (need this for MICE to work)
df_narmice$relig <- as.factor(df_narmice$relig)
df_narmice$donate <- as.factor(df_narmice$donate)
df_narmice$M_donate <- as.factor(df_narmice$M_donate)

# Quick check of the data
summary(df_narmice)
str(df_narmice)


## Set up prediction matrix for the imputation
ini <- mice(df_narmice, maxit = 0, print = TRUE)

# Specify the prediction matrix for the observable data - And edit so that the missingness markers don't predict the variables they represent
pred <- ini$predictorMatrix
pred

pred["donate", "M_donate"] <- 0
pred

# Set-up the prediction matrix for the unidentifiable part of the model (i.e., the missing-not-at-random element)
# In this case the whole matrix should be zeroes because the unidentifiable part of the imputation model contains a single constant (CSP*M) rather than additional contributions from the other variables in the dataset
predSens <- ini$predictorMatrix
predSens

predSens[predSens == 1] <- 0
predSens

# Set up list with sensitivity parameter values (currently all blank - to be filled in below)
pSens <- rep(list(list("")), ncol(df_narmice))
names(pSens) <- names(df_narmice)
pSens

# Set up vector describing manner of imputation for each variable - As we want to run vary the association between donation and it's missingness, we specify this as 'logregSens', which is a logistic sensitivity analysis.
meth <- ini$method
meth

meth["donate"] <- "logregSens"
meth

# Choose number of imputations and burn-in period - As only 1 variable to impute, the computational burden burden is quite low, so will run 50 imputations per CSP, with a burn-in period of 1 (note that with more variables to impute and a greater number of burn-in iterations, the time taken for this NARMICE method to run can quickly spiral upwards).
narmice_numimps <- 50
narmice_numiter <- 1

# To collect the parameters of interest
tipping <- as.data.frame(array(dim = c(dim = length(seq.int(-3, 1, by = 0.5)), 8))) # Number of sensitivity values we're going to try (varying the CSP from -3 to 1, in steps of 0.5), plus the number of parameters we're going to store (here, is 8 [see row below])
colnames(tipping) <- c("csp", "msp", "imor", "sampprev", "beta", "se_beta", "lower_ci", "upper_ci")
tipping

# Looping over delta/CSP values (i) - A CSP of -3 means that individuals with missing blood donation data have -3 lower log-odds of having given blood, compared to those with data (conditional on all other covariates); the converse applies to positive CSPs (i.e., those with missing data having greater likelihood of having given blood), while a CSP of 0 should approximately correspond to a standard MI model, as there is no adjustment for data potentially being missing-not-at-random. As we simulated this data, we know that the true CSP should be approximately log(0.33), which is -1.11 (we know this, because the true causal effect of donation on selection - i.e., having observed blood donation data - is log(3), which we reverse to get the odds ratio of having missing data); ordinarily, of course, we would not know this, hence why we are exploring a range of parameter values.
set.seed(771396)
k <- 0
for (i in seq.int(-3, 1, by = 0.5)) {
  k <- k+1 
  print(paste0("CSP = ", i))
  
  # specify a delta/CSP value for the prediction equation for missing data
  pSens[["donate"]]<-list(c(i))
  
  # NARMICE imputation
  imp_NARMICE <- mice(df_narmice, m = narmice_numimps, method = meth, predictorMatrix = pred,
                      predictorSens=predSens, parmSens=pSens, print = TRUE, maxit = narmice_numiter)
  
  # Derive the MSP for the given CSP value
  msp <- round(summary(pool(with(imp_NARMICE, 
                                 glm(donate ~ M_donate, family = "binomial")))), 3)
  
  # Derive the prevalence of blood donation in sample
  wholesampprev <- round(summary(pool(with(imp_NARMICE, 
                                           glm(donate ~ 1, family = binomial(link = "identity"))))), 3)
  
  # Calculate the adjusted odds ratio in these imputed datasets
  newest <- round(summary(pool(with(imp_NARMICE, 
                                    glm(donate ~ relig, family = "binomial")))), 3)
  
  # Store these estimates in the 'tipping' dataframe
  tipping[k,1] <- i
  tipping[k,2] <- msp["M_donate1", "est"]
  tipping[k,3] <- exp(msp["M_donate1", "est"])
  tipping[k,4] <- wholesampprev["(Intercept)", "est"]  
  tipping[k,5] <- newest["relig2", "est"]
  tipping[k,6] <- newest["relig2", "se"]
  tipping[k,7] <- newest["relig2", "lo 95"]
  tipping[k,8] <- newest["relig2", "hi 95"]
  
  print(tipping[k,])
}

# Look at the 'tipping' output which contains all the values/estimates
tipping

# Convert the log odds to ORs
tipping$or <- exp(tipping$beta)
tipping$lowerCI_or <- exp(tipping$lower_ci)
tipping$upperCI_or <- exp(tipping$upper_ci)
tipping


### Can see here that there is little difference in the odds ratios for this range of CSP values, as religiosity does not cause misingness in the outcome. If we repeat the code above, this time including religiosity as a cause of missingness in the outcome, then we will observe much more variation in the odds ratios by different CSP values - Let's code this below.

# Here's the DAG
dag_selection3_iv <- dagitty('dag {
                Religiosity [pos = "0,2"]
                BloodDonation [pos = "2,2"]
                M_donation [pos = "1,3"]
                Selection [pos = "1,4"]
                
                
                Religiosity -> BloodDonation
                Religiosity -> M_donation
                BloodDonation -> M_donation
                M_donation -> Selection
                }')
plot(dag_selection3_iv)

# And here's how to code this up
selection_iv_p <- plogis(log(1.5) + (log(3) * relig) + (log(3) * donate))
selection_iv <- rbinom(n = n, size = 1, prob = selection_iv_p)
table(selection_iv)


## In this scenario, as the exposure and outcome both cause missingness, there will be selection bias. As a reminder here, the true effect is an odds ratio of 2.11:
exp(cbind(OR = coef(summary(mod1))[, 1], confint(mod1)))

# Now to include selection - Now there's much more bias, as the OR = 1.75
mod2_iv <- glm(donate ~ relig, family = "binomial", subset = selection_iv == 1)
exp(cbind(OR = coef(summary(mod2_iv))[, 1], confint(mod2_iv)))


### Now perform a NARMICE analysis, varying the outcome MNAR assumption

# Put the relevant data into a data frame
df_narmice <- as.data.frame(cbind(relig, donate, selection_iv))
head(df_narmice)

# Code outcome as missing if 'selection_iv' = 0. Additionally, for NARMICE, recommendation is to include missingness indicators for all variables with missing data, so will do that here - Will swap the 'selection_iv' variable so that missingness = 1, and rename this to 'M_donate', to make clearer.
df_narmice$donate[df_narmice$selection_iv == 0] <- NA
df_narmice$M_donate <- 1 - df_narmice$selection_iv
df_narmice$selection_iv <- NULL
head(df_narmice)

# Convert binary/categorical variables to factors (need this for MICE to work)
df_narmice$relig <- as.factor(df_narmice$relig)
df_narmice$donate <- as.factor(df_narmice$donate)
df_narmice$M_donate <- as.factor(df_narmice$M_donate)

# Quick check of the data
summary(df_narmice)
str(df_narmice)


## Set up prediction matrix for the imputation
ini <- mice(df_narmice, maxit = 0, print = TRUE)

# Specify the prediction matrix for the observable data - And edit so that the missingness markers don't predict the variables they represent
pred <- ini$predictorMatrix
pred

pred["donate", "M_donate"] <- 0
pred

# Set-up the prediction matrix for the unidentifiable part of the model (i.e., the missing-not-at-random element)
# In this case the whole matrix should be zeroes because the unidentifiable part of the imputation model contains a single constant (CSP*M) rather than additional contributions from the other variables in the dataset
predSens <- ini$predictorMatrix
predSens

predSens[predSens == 1] <- 0
predSens

# Set up list with sensitivity parameter values (currently all blank - to be filled in below)
pSens <- rep(list(list("")), ncol(df_narmice))
names(pSens) <- names(df_narmice)
pSens

# Set up vector describing manner of imputation for each variable - As we want to run vary the association between donation and it's missingness, we specify this as 'logregSens', which is a logistic sensitivity analysis.
meth <- ini$method
meth

meth["donate"] <- "logregSens"
meth

# Choose number of imputations and burn-in period - As only 1 variable to impute, the computational burden burden is quite low, so will run 50 imputations per CSP, with a burn-in period of 1 (note that with more variables to impute and a greater number of burn-in iterations, the time taken for this NARMICE method to run can quickly spiral upwards).
narmice_numimps <- 50
narmice_numiter <- 1

# To collect the parameters of interest
tipping_iv <- as.data.frame(array(dim = c(dim = length(seq.int(-3, 1, by = 0.5)), 8))) # Number of sensitivity values we're going to try (varying the CSP from -3 to 1, in steps of 0.5), plus the number of parameters we're going to store (here, is 8 [see row below])
colnames(tipping_iv) <- c("csp", "msp", "imor", "sampprev", "beta", "se_beta", "lower_ci", "upper_ci")
tipping_iv

# Looping over delta/CSP values (i) - A CSP of -3 means that individuals with missing blood donation data have -3 lower log-odds of having given blood, compared to those with data (conditional on all other covariates); the converse applies to positive CSPs (i.e., those with missing data having greater likelihood of having given blood), while a CSP of 0 should approximately correspond to a standard MI model, as there is no adjustment for data potentially being missing-not-at-random. As we simulated this data, we know that the true CSP should be approximately log(0.33), which is -1.11 (we know this, because the true causal effect of donation on selection - i.e., having observed blood donation data - is log(3), which we reverse to get the odds ratio of having missing data); ordinarily, of course, we would not know this, hence why we are exploring a range of parameter values.
set.seed(196704)
k <- 0
for (i in seq.int(-3, 1, by = 0.5)) {
  k <- k+1 
  print(paste0("CSP = ", i))
  
  # specify a delta/CSP value for the prediction equation for missing data
  pSens[["donate"]]<-list(c(i))
  
  # NARMICE imputation
  imp_NARMICE <- mice(df_narmice, m = narmice_numimps, method = meth, predictorMatrix = pred,
                      predictorSens=predSens, parmSens=pSens, print = TRUE, maxit = narmice_numiter)
  
  # Derive the MSP for the given CSP value
  msp <- round(summary(pool(with(imp_NARMICE, 
                                 glm(donate ~ M_donate, family = "binomial")))), 3)
  
  # Derive the prevalence of blood donation in sample
  wholesampprev <- round(summary(pool(with(imp_NARMICE, 
                                           glm(donate ~ 1, family = binomial(link = "identity"))))), 3)
  
  # Calculate the adjusted odds ratio in these imputed datasets
  newest <- round(summary(pool(with(imp_NARMICE, 
                                    glm(donate ~ relig, family = "binomial")))), 3)
  
  # Store these estimates in the 'tipping_iv' dataframe
  tipping_iv[k,1] <- i
  tipping_iv[k,2] <- msp["M_donate1", "est"]
  tipping_iv[k,3] <- exp(msp["M_donate1", "est"])
  tipping_iv[k,4] <- wholesampprev["(Intercept)", "est"]  
  tipping_iv[k,5] <- newest["relig2", "est"]
  tipping_iv[k,6] <- newest["relig2", "se"]
  tipping_iv[k,7] <- newest["relig2", "lo 95"]
  tipping_iv[k,8] <- newest["relig2", "hi 95"]
  
  print(tipping_iv[k,])
}

# Look at the 'tipping_iv' output which contains all the values/estimates
tipping_iv

# Convert the log odds to ORs
tipping_iv$or <- exp(tipping_iv$beta)
tipping_iv$lowerCI_or <- exp(tipping_iv$lower_ci)
tipping_iv$upperCI_or <- exp(tipping_iv$upper_ci)
tipping_iv


### Now that the exposure causes selection/missingness, there is much more variation in the exposure-outcome association by different CSP values.


