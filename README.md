# ReadMe File


## General Information:

This repository provides the replication material for the analysis presented in the maters's thesis "Who Votes for Which Populists?
A Multiclass Classification of Populist Voters in Europe".


## Contents:

**1. Code**

data_cleaning: data cleaning from the original ESS data  
data_imputation: imputation of income variable  
RF1 - RF6: Models inlcuding the presented outcomes and interpretation techniques  
CP_profiles_RF1, CP_profiles_RF2: additional results for RF1 and RF 2  

**2. Data**

pop_class_parties: classification of parties  
county_level_data: country level variables  
data: complete cleaned data  
data_pop_1: cleaned data with first operationalization of populist party  
data_pop_2: cleaned data with second operationalization of populist party  
data_pop_1_imp: cleaned data with first operationalization of populist party and imputed values for income variable   
data_pop_2_imp: cleaned data with second operationalization of populist party and imputed values for income variable 

## Notes:

- Download of the original ESS 9 data used in the R script data_cleaning is available via https://ess-search.nsd.no/en/study/bdc7c350-1029-4cb3-9d5e-53f668b8fa74  
- All libraries used in the code are specified at the beginning of each R script  
- All the paths in the R scripts need to be adjusted to the correct directories  
