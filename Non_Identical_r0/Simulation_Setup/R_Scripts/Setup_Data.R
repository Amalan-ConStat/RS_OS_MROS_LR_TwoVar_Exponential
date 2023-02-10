library(here)

#Model_Path<-"Model_1"
#Model_Path<-"Model_2"
#Model_Path<-"Model_3"
Model_Path<-"Model_4"

# Random Sampling Data ----
# Base_Path<-"Random_Sampling"
# 
# ## No Correlated Covariate Data -----
# load(here("Non_Identical_r0","Generate_Big_Data",Model_Path,"No_Correlated_Covariate.RData"))
# save(list = c("Simulated_Data","Replicates","Subsample_Size","Model_Path"),
#      file = here("Non_Identical_r0","Simulation_Setup","Analysis",
#                  Base_Path,"Init.RData"))        
# 
# # Rare Event Random Sampling Data ----
# Base_Path<-"RE_Random_Sampling"
# 
# ## No Correlated Covariate Data -----
# load(here("Non_Identical_r0","Generate_Big_Data",Model_Path,"No_Correlated_Covariate.RData"))
# save(list = c("Simulated_Data","Replicates","Subsample_Size","Model_Path"),
#      file = here("Non_Identical_r0","Simulation_Setup","Analysis",
#                  Base_Path,"Init.RData"))        

# OSMAC Data ----
Base_Path<-"OSMAC"

## No Correlated Covariate Data -----
load(here("Non_Identical_r0","Generate_Big_Data",Model_Path,"No_Correlated_Covariate.RData"))
save(list = c("Simulated_Data","Replicates","Subsample_Size","Model_Path","r0"),
     file = here("Non_Identical_r0","Simulation_Setup","Analysis",
                 Base_Path,"Init.RData"))        

remove(Base_Path)

# OSMAC Model Free Data ----
Base_Path<-"OSMAC_Model_Free"

## No Correlated Covariate Data -----
load(here("Non_Identical_r0","Generate_Big_Data",Model_Path,"No_Correlated_Covariate.RData"))
save(list = c("Simulated_Data","Replicates","Subsample_Size","Model_Path","r0"),
     file = here("Non_Identical_r0","Simulation_Setup","Analysis",
                 Base_Path,"Init.RData"))

remove(Base_Path)

rm(list = ls())
