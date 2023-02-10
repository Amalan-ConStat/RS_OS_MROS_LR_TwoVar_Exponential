# Run for OSMAC Sampling----
library(compiler)
library(here)
library(LaplacesDemon)

enableJIT(1)

# Load the No Correlated Data----
load(here("Non_Identical_r0","Simulation_Setup","Analysis","OSMAC","Init.RData"))

# Load the OSMAC Sample----
load(here("Non_Identical_r0","Simulation_Setup","Analysis","OSMAC","r1_Run_OSMAC.RData"))

# Real Model ----
# Generate for Random sample of 1000 different times ---
## Pi=MMSE and Pi=MVC Proportional probabilities ---
Final_Parameter<-Run_OSMAC(Replicates=Replicates,r1=100*(1:9),r2=1000,
                           Y=as.matrix(Simulated_Data[[2]]$Real_Model[,1]),
                           X=as.matrix(Simulated_Data[[2]]$Real_Model[,-1]),
                           Real_Data=as.matrix(Simulated_Data[[2]]$Real_Model),
                           N=Simulated_Data$Basic$N,Model="Real_Model")

# Assumed Model 1 ----
# Generate for Random sample of 1000 different times ---
## Pi=MMSE and Pi=MVC Proportional probabilities ---
Final_Parameter<-Run_OSMAC(Replicates=Replicates,r1=100*(1:9),r2=1000,
                           Y=as.matrix(Simulated_Data[[2]]$Assumed_Model_1[,1]),
                           X=as.matrix(Simulated_Data[[2]]$Assumed_Model_1[,-1]),
                           Real_Data = as.matrix(Simulated_Data[[2]]$Real_Model),
                           N=Simulated_Data$Basic$N, Model="Assumed_Model_1")

# Assumed Model 2 ----
# Generate for Random sample of 1000 different times ---
## Pi=MMSE and Pi=MVC Proportional probabilities ---
Final_Parameter<-Run_OSMAC(Replicates=Replicates,r1=100*(1:9),r2=1000,
                           Y=as.matrix(Simulated_Data[[2]]$Assumed_Model_2[,1]),
                           X=as.matrix(Simulated_Data[[2]]$Assumed_Model_2[,-1]),
                           Real_Data = as.matrix(Simulated_Data[[2]]$Real_Model),
                           N=Simulated_Data$Basic$N, Model="Assumed_Model_2")

# Assumed Model 3 ----
# Generate for Random sample of 1000 different times ---
## Pi=MMSE and Pi=MVC Proportional probabilities ---
Final_Parameter<-Run_OSMAC(Replicates=Replicates,r1=100*(1:9),r2=1000,
                           Y=as.matrix(Simulated_Data[[2]]$Assumed_Model_3[,1]),
                           X=as.matrix(Simulated_Data[[2]]$Assumed_Model_3[,-1]),
                           Real_Data = as.matrix(Simulated_Data[[2]]$Real_Model),
                           N=Simulated_Data$Basic$N,Model="Assumed_Model_3")
