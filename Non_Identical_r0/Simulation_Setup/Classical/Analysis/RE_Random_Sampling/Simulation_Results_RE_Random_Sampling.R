# Run for Rare Event Random Sampling----
library(compiler)
library(here)
library(LaplacesDemon)

enableJIT(1)

# Load the No Correlated Data----
load(here("Non_Identical_r0","Simulation_Setup","Classical","Analysis","RE_Random_Sampling","Init.RData"))

# Load the Rare Event Random Sample----
load(here("Non_Identical_r0","Simulation_Setup","Classical","Analysis","RE_Random_Sampling","RE_RandomSample.RData"))

# Real Model ----
# Generate for Rare Evnet Random sample of 1000 different times
Final_Parameter<-Run_RE_RandomSample(Replicates = Replicates, 
                                     FullData = Simulated_Data[[2]]$Real_Model,
                                     N=Simulated_Data$Basic$N,
                                     Subsample_Size=Subsample_Size,Choices=c(100*(2:15)),
                                     No_of_models = 4)

Est_Param_RE_RandomSample<-Final_Parameter$EstPar[[1]]
Utility_RE_RandomSample<-Final_Parameter$Utility[[1]]
Bias_RE_RandomSample<-Final_Parameter$Bias[[1]]
#SelectedData_RE_RandomSample<-Final_Parameter$Subsampled_Data[[1]]

# Save the Results ---
save(Est_Param_RE_RandomSample,Utility_RE_RandomSample,Bias_RE_RandomSample,#SelectedData_RE_RandomSample,
     file = here("Non_Identical_r0","Simulation_Setup","Classical","Analysis","RE_Random_Sampling",
                 "Results","Real_Model","RE_Random_Sample_output.RData"))

save(Est_Param_RE_RandomSample,Utility_RE_RandomSample,Bias_RE_RandomSample,#SelectedData_RE_RandomSample,
     file = here("Non_Identical_r0","Outputs","Classical",Model_Path,"RE_Random_Sampling",
                 "Real_Model","RE_Random_Sample_output.RData"))

# Assumed Model 1----
# Generate for Rare Evnet Random sample of 1000 different times
Est_Param_RE_RandomSample<-Final_Parameter$EstPar[[2]]
Utility_RE_RandomSample<-Final_Parameter$Utility[[2]]
Bias_RE_RandomSample<-Final_Parameter$Bias[[2]]
#SelectedData_RE_RandomSample<-Final_Parameter$Subsampled_Data

# Save the Results ---
save(Est_Param_RE_RandomSample,Utility_RE_RandomSample,Bias_RE_RandomSample,#SelectedData_RE_RandomSample,
     file = here("Non_Identical_r0","Simulation_Setup","Classical","Analysis","RE_Random_Sampling",
                 "Results","Assumed_Model_1","RE_Random_Sample_output.RData"))

save(Est_Param_RE_RandomSample,Utility_RE_RandomSample,Bias_RE_RandomSample,#SelectedData_RE_RandomSample,
     file = here("Non_Identical_r0","Outputs","Classical",Model_Path,"RE_Random_Sampling",
                 "Assumed_Model_1","RE_Random_Sample_output.RData"))

# Assumed Model 2----
# Generate for Rare Evnet Random sample of 1000 different times
Est_Param_RE_RandomSample<-Final_Parameter$EstPar[[3]]
Utility_RE_RandomSample<-Final_Parameter$Utility[[3]]
Bias_RE_RandomSample<-Final_Parameter$Bias[[3]]
#SelectedData_RE_RandomSample<-Final_Parameter$Subsampled_Data

# Save the Results ---
save(Est_Param_RE_RandomSample,Utility_RE_RandomSample,Bias_RE_RandomSample,#SelectedData_RE_RandomSample,
     file = here("Non_Identical_r0","Simulation_Setup","Classical","Analysis","RE_Random_Sampling",
                 "Results","Assumed_Model_2","RE_Random_Sample_output.RData"))

save(Est_Param_RE_RandomSample,Utility_RE_RandomSample,Bias_RE_RandomSample,#SelectedData_RE_RandomSample,
     file = here("Non_Identical_r0","Outputs","Classical",Model_Path,"RE_Random_Sampling",
                 "Assumed_Model_2","RE_Random_Sample_output.RData"))

# Assumed Model 3----
# Generate for Rare Evnet Random sample of 1000 different times
Est_Param_RE_RandomSample<-Final_Parameter$EstPar[[4]]
Utility_RE_RandomSample<-Final_Parameter$Utility[[4]]
Bias_RE_RandomSample<-Final_Parameter$Bias[[4]]
#SelectedData_RE_RandomSample<-Final_Parameter$Subsampled_Data

# Save the Results ---
save(Est_Param_RE_RandomSample,Utility_RE_RandomSample,Bias_RE_RandomSample,#SelectedData_RE_RandomSample,
     file = here("Non_Identical_r0","Simulation_Setup","Classical","Analysis","RE_Random_Sampling",
                 "Results","Assumed_Model_3","RE_Random_Sample_output.RData"))

save(Est_Param_RE_RandomSample,Utility_RE_RandomSample,Bias_RE_RandomSample,#SelectedData_RE_RandomSample,
     file = here("Non_Identical_r0","Outputs","Classical",Model_Path,"RE_Random_Sampling",
                 "Assumed_Model_3","RE_Random_Sample_output.RData"))
