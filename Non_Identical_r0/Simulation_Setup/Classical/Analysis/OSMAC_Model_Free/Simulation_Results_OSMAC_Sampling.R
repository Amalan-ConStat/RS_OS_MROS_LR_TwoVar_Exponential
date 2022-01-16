# Run for OSMAC Sampling----
library(compiler)
library(here)
library(LaplacesDemon)

enableJIT(1)

# Load the No Correlated Data----
load(here("Non_Identical_r0","Simulation_Setup","Classical","Analysis","OSMAC_Model_Free","Init.RData"))

# Load the OSMAC Sample----
load(here("Non_Identical_r0","Simulation_Setup","Classical","Analysis","OSMAC_Model_Free","Run_OSMAC.RData"))

All_Covariates<-c("X0","X1","X2","X1^2","X2^2")

# Model 1 ----

if(Model_Path=="Model_1")
{
  combs<-list(All_Covariates[-c(4,5)],
              All_Covariates[-5],
              All_Covariates[-4],
              All_Covariates)
  
  # Generate for Random sample of 1000 different times ----
  ## Pi=MMSE and Pi=MVC Proportional probabilities ---
  Final_Parameter<-Run_OSMAC_MF(Replicates=Replicates,r1=r0,r2=c(100*(2:15)),
                                Y=as.matrix(Simulated_Data[[2]]$Assumed_Model_3[,1]),
                                X=as.matrix(Simulated_Data[[2]]$Assumed_Model_3[,-1]),
                                alpha = rep(1/4,length(combs)), N=Simulated_Data$Basic$N,
                                All_Covariates=All_Covariates,
                                combs=combs)
}

# Model 2 ----

if(Model_Path=="Model_2")
{
  combs<-list(All_Covariates[-5],
              All_Covariates[-c(4,5)],
              All_Covariates[-4],
              All_Covariates)
  
  # Generate for Random sample of 1000 different times ----
  ## Pi=MMSE and Pi=MVC Proportional probabilities ---
  Final_Parameter<-Run_OSMAC_MF(Replicates=Replicates,r1=r0,r2=c(100*(2:15)),
                                Y=as.matrix(Simulated_Data[[2]]$Assumed_Model_3[,1]),
                                X=as.matrix(Simulated_Data[[2]]$Assumed_Model_3[,-1]),
                                alpha = rep(1/4,length(combs)), N=Simulated_Data$Basic$N,
                                All_Covariates=All_Covariates,
                                combs=combs)
}

# Model 3 ----
if(Model_Path=="Model_3")
{
  combs<-list(All_Covariates[-4],
              All_Covariates[-c(4,5)],
              All_Covariates[-5],
              All_Covariates)
  
  # Generate for Random sample of 1000 different times ----
  ## Pi=MMSE and Pi=MVC Proportional probabilities ---
  Final_Parameter<-Run_OSMAC_MF(Replicates=Replicates,r1=r0,r2=c(100*(2:15)),
                                Y=as.matrix(Simulated_Data[[2]]$Assumed_Model_3[,1]),
                                X=as.matrix(Simulated_Data[[2]]$Assumed_Model_3[,-1]),
                                alpha = rep(1/4,length(combs)), N=Simulated_Data$Basic$N,
                                All_Covariates=All_Covariates,
                                combs=combs)
}

# Model 4 ----
if(Model_Path=="Model_4")
{
  combs<-list(All_Covariates,
              All_Covariates[-c(4,5)],
              All_Covariates[-5],
              All_Covariates[-4])
  
  # Generate for Random sample of 1000 different times ----
  ## Pi=MMSE and Pi=MVC Proportional probabilities ---
  Final_Parameter<-Run_OSMAC_MF(Replicates=Replicates,r1=r0,r2=c(100*(2:15)),
                                Y=as.matrix(Simulated_Data[[2]]$Real_Model[,1]),
                                X=as.matrix(Simulated_Data[[2]]$Real_Model[,-1]),
                                alpha = rep(1/4,length(combs)), N=Simulated_Data$Basic$N,
                                All_Covariates=All_Covariates,
                                combs=combs)
}
