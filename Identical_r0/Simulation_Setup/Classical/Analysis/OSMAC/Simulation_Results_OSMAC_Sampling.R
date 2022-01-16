# Run for OSMAC Sampling----
library(compiler)
library(here)
library(LaplacesDemon)

enableJIT(1)

# Load the No Correlated Data----
load(here("Identical_r0","Simulation_Setup","Classical","Analysis","OSMAC","Init.RData"))
#load(here("Init.RData"))

# Load the OSMAC Sample----
load(here("Identical_r0","Simulation_Setup","Classical","Analysis","OSMAC","Run_OSMAC.RData"))
#load(here("Run_OSMAC.RData"))

All_Covariates<-c("X0","X1","X2","X1^2","X2^2")

# Model 1----
# Generate for Random sample of 1000 different times ---
## Pi=MMSE and Pi=MVC Proportional probabilities ---
if(Model_Path=="Model_1")
{
  combs<-list(All_Covariates[-5],
              All_Covariates[-4],
              All_Covariates) 
  
  Final_Parameter<-Run_OSMAC(Replicates = Replicates, r1 = r0, r2 = c(100*(2:15)),
                             Y = as.matrix(Simulated_Data[[2]]$Assumed_Model_3[,1]),
                             X = as.matrix(Simulated_Data[[2]]$Assumed_Model_3[,-1]),
                             Real_Data = as.matrix(Simulated_Data[[2]]$Real_Model),
                             N = Simulated_Data$Basic$N,
                             alpha = rep(1/4,length(combs)+1),
                             combs = combs,
                             All_Covariates = All_Covariates)
}

# Model 2----
# Generate for Random sample of 1000 different times ---
## Pi=MMSE and Pi=MVC Proportional probabilities ---
if(Model_Path=="Model_2")
{
  combs<-list(All_Covariates[-c(4,5)],
              All_Covariates[-4],
              All_Covariates) 
  
  Final_Parameter<-Run_OSMAC(Replicates = Replicates, r1 = r0, r2 = c(100*(2:15)),
                             Y = as.matrix(Simulated_Data[[2]]$Assumed_Model_3[,1]),
                             X = as.matrix(Simulated_Data[[2]]$Assumed_Model_3[,-1]),
                             Real_Data = as.matrix(Simulated_Data[[2]]$Real_Model),
                             N = Simulated_Data$Basic$N,
                             alpha = rep(1/4,length(combs)+1),
                             combs = combs,
                             All_Covariates = All_Covariates)
}

# Model 3----
# Generate for Random sample of 1000 different times ---
## Pi=MMSE and Pi=MVC Proportional probabilities ---
if(Model_Path=="Model_3")
{
  combs<-list(All_Covariates[-c(4,5)],
              All_Covariates[-5],
              All_Covariates) 
  
  Final_Parameter<-Run_OSMAC(Replicates = Replicates, r1 = r0, r2 = c(100*(2:15)),
                             Y = as.matrix(Simulated_Data[[2]]$Assumed_Model_3[,1]),
                             X = as.matrix(Simulated_Data[[2]]$Assumed_Model_3[,-1]),
                             Real_Data = as.matrix(Simulated_Data[[2]]$Real_Model),
                             N = Simulated_Data$Basic$N,
                             alpha = rep(1/4,length(combs)+1),
                             combs = combs,
                             All_Covariates = All_Covariates)
}

# Model 4----
# Generate for Random sample of 1000 different times ---
## Pi=MMSE and Pi=MVC Proportional probabilities ---
if(Model_Path=="Model_4")
{
  combs<-list(All_Covariates[-c(4,5)],
              All_Covariates[-4],
              All_Covariates[-5]) 
  
  Final_Parameter<-Run_OSMAC(Replicates = Replicates, r1 = r0, r2 = c(100*(2:15)),
                             Y = as.matrix(Simulated_Data[[2]]$Real_Model[,1]),
                             X = as.matrix(Simulated_Data[[2]]$Real_Model[,-1]),
                             Real_Data = as.matrix(Simulated_Data[[2]]$Real_Model),
                             N = Simulated_Data$Basic$N,
                             alpha = rep(1/4,length(combs)+1),
                             combs = combs,
                             All_Covariates = All_Covariates)
}
