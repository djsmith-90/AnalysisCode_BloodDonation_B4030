## Git repository for ALSPAC 'Religion and Blood Donation' analysis code (B4030)

This repository contains two folders: 'SimulatedExample' and 'ALSPACAnalysis'. 

The 'SimulatedExample' folder contains two script files which demonstrate the different assumptions necessary
to make causal inferences from observational data, how violating these assumpations can lead to bias, and methods
to explore/overcome these biases. These assumptions focus on confounding bias (e.g., mis-specified confounding
model of observed covariates and residual confounding due to unmeasured covariates) and selection bias due to 
missing data. Simple simulations are used to demonastrate these concepts and methods. There are two script files
in this folder, one in R (Religion_BloodDonation_SimulatedExample.r) which contains the majority of the sensitivity
analyses, plus another in Stata (Religion_BloodDonation_GeneralisedSensitivityAnalysis_Stata.do) which contains the
'Generalised Sensitivity Analysis' for residual confounding. 

The 'ALSPACAnalysis' folder contains scripts of the actual ALSPAC analyses.

Note that ALSPAC data access is through a system of managed open access. Information
about access to ALSPAC data is given on the ALSPAC website 
(http://www.bristol.ac.uk/alspac/researchers/access/). These datasets used in these
scripts are linked to ALSPAC project number B4030; if you are interested in accessing
these datasets, please quote this number during your application.
