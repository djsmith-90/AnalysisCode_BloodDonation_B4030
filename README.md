## Git repository for ALSPAC 'Religion and Blood Donation' analysis code (B4030)

This repository contains two folders: 'SimulatedExample' and 'ALSPACAnalysis'. 

The 'SimulatedExample' folder contains two script files which demonstrate the different assumptions necessary
to make causal inferences from observational data, how violating these assumpations can lead to bias, and methods
to explore/overcome these biases. These assumptions focus on confounding bias (e.g., mis-specified confounding
model of observed covariates and residual confounding due to unmeasured covariates) and selection bias due to 
missing data. Simple simulations are used to demonstrate these concepts and methods. There are two script files
in this folder, one in R (Religion_BloodDonation_SimulatedExample.r) which contains the majority of the sensitivity
analyses, plus another in Stata (Religion_BloodDonation_GeneralisedSensitivityAnalysis_Stata.do) which contains the
'Generalised Sensitivity Analysis' method for residual confounding. 


The 'ALSPACAnalysis' folder contains scripts of the actual ALSPAC analyses, in addition to synthetic ALSPAC datasets
(more on these below). The script files are as follows:
 - Script1_DataProcessingAndCleaning.r - R script to clean and process the raw data
 - Script2_CreatingSimulatedDatasets.r - R script to create synthetic datasets using the 'synthpop' package
 - Script3_AnalysingMothersData.r - R script to analyse the mother's data
 - Script3b_MothersData_GSA.do - Stata script to run the 'Generalised Sensitivity Analysis' on the mother's data
 - Script3_AnalysingPartnersData.r - R script to analyse the partner's data
 - Script3b_PartnersData_GSA.do - Stata script to run the 'Generalised Sensitivity Analysis' on the partner's data

The 'forBluePebbleHPC' folder contains both R and shell scripts to perform the 'Not-At-Random Multiple Imputation' 
sensitivity analyses using the University of Bristol's High Performance Computing suite 
(https://www.bristol.ac.uk/acrc/high-performance-computing/).

This 'ALSPACAnalysis' folder also contains synthetic versions of the mother's and partner's ALSPAC datasets, created
using Script 2 above. As raw ALSPAC data cannot be released, these synthesised datasets are modelled on the original 
ALSPAC data, thus maintaining variable distributions and relations among variables (albeit not pefectly), while 
at the same time preserving participant anonymity and confidentiality. These mother's synthetic data have the file
name 'syntheticData_mum_B4030', while for partners they are 'syntheticData_partner_B4030', and are available in 
R ('.RData'), CSV ('.csv') and Stata ('.dta') formats.


Note that ALSPAC data access is through a system of managed open access. Information
about access to ALSPAC data is given on the ALSPAC website 
(http://www.bristol.ac.uk/alspac/researchers/access/). These datasets used in these
scripts are linked to ALSPAC project number B4030; if you are interested in accessing
these datasets, please quote this number during your application.
