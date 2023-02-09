# Run for Random Sampling----
library(compiler)
library(here)
library(LaplacesDemon)

enableJIT(1)

# Load the No Correlated Data----
load(here("Non_Identical_r0","Simulation_Setup","Classical","Analysis","Random_Sampling","Init.RData"))

# Load the Random Sample----
load(here("Non_Identical_r0","Simulation_Setup","Classical","Analysis","Random_Sampling","Run_RandomSample.RData"))

# Real Model ----
# Generate for Random sample of 1000 different times
Final_Parameter<-Run_RandomSample(Replicates = Replicates, 
                                  FullData = Simulated_Data[[2]]$Real_Model,
                                  N=Simulated_Data$Basic$N,
                                  Subsample_Size=Subsample_Size,Choices=c(100*(2:15)),
                                  No_of_Models = 4)

Est_Param_RandomSample<-Final_Parameter$EstPar[[1]]
Utility_RandomSample<-Final_Parameter$Utility[[1]]
#SelectedData_RandomSample<-Final_Parameter$Subsampled_Data[[1]]
Bias_RandomSample<-Final_Parameter$Bias[[1]]

# Save the Results ----
save(Est_Param_RandomSample,Utility_RandomSample,Bias_RandomSample,#SelectedData_RandomSample,
     file = here("Non_Identical_r0","Simulation_Setup","Classical","Analysis","Random_Sampling",
                 "Results","Real_Model","Random_Sample_output.RData"))

save(Est_Param_RandomSample,Utility_RandomSample,Bias_RandomSample,#SelectedData_RandomSample,
     file = here("Non_Identical_r0","Outputs","Classical",Model_Path,"Random_Sampling",
                 "Real_Model","Random_Sample_output.RData"))

# Assumed Model 1----
# Generate for Random sample of 1000 different times
Est_Param_RandomSample<-Final_Parameter$EstPar[[2]]
Utility_RandomSample<-Final_Parameter$Utility[[2]]
#SelectedData_RandomSample<-Final_Parameter$Subsampled_Data[[2]]
Bias_RandomSample<-Final_Parameter$Bias[[2]]

# Save the Results ---
save(Est_Param_RandomSample,Utility_RandomSample,Bias_RandomSample,#SelectedData_RandomSample,
     file = here("Non_Identical_r0","Simulation_Setup","Classical","Analysis","Random_Sampling",
                 "Results","Assumed_Model_1","Random_Sample_output.RData"))

save(Est_Param_RandomSample,Utility_RandomSample,Bias_RandomSample,#SelectedData_RandomSample,
     file = here("Non_Identical_r0","Outputs","Classical",Model_Path,"Random_Sampling",
                 "Assumed_Model_1","Random_Sample_output.RData"))

# Assumed Model 2----
# Generate for Random sample of 1000 different times
Est_Param_RandomSample<-Final_Parameter$EstPar[[3]]
Utility_RandomSample<-Final_Parameter$Utility[[3]]
#SelectedData_RandomSample<-Final_Parameter$Subsampled_Data[[3]]
Bias_RandomSample<-Final_Parameter$Bias[[3]]

# Save the Results ---
save(Est_Param_RandomSample,Utility_RandomSample,Bias_RandomSample,#SelectedData_RandomSample,
     file = here("Non_Identical_r0","Simulation_Setup","Classical","Analysis","Random_Sampling",
                 "Results","Assumed_Model_2","Random_Sample_output.RData"))

save(Est_Param_RandomSample,Utility_RandomSample,Bias_RandomSample,#SelectedData_RandomSample,
     file = here("Non_Identical_r0","Outputs","Classical",Model_Path,"Random_Sampling",
                 "Assumed_Model_2","Random_Sample_output.RData"))

# Assumed Model 3----
# Generate for Random sample of 1000 different times
Est_Param_RandomSample<-Final_Parameter$EstPar[[4]]
Utility_RandomSample<-Final_Parameter$Utility[[4]]
#SelectedData_RandomSample<-Final_Parameter$Subsampled_Data
Bias_RandomSample<-Final_Parameter$Bias[[4]]

# Save the Results ---
save(Est_Param_RandomSample,Utility_RandomSample,Bias_RandomSample,#SelectedData_RandomSample,
     file = here("Non_Identical_r0","Simulation_Setup","Classical","Analysis","Random_Sampling",
                 "Results","Assumed_Model_3","Random_Sample_output.RData"))

save(Est_Param_RandomSample,Utility_RandomSample,Bias_RandomSample,#SelectedData_RandomSample,
     file = here("Non_Identical_r0","Outputs","Classical",Model_Path,"Random_Sampling",
                 "Assumed_Model_3","Random_Sample_output.RData"))
