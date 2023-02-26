library(here)
library(compiler)
library(LaplacesDemon)
library(matrixStats)

# Load the OSMAC algorithm
source(here("Non_Identical_r0","Simulation_Setup","R_Scripts","OSMAC_Model_Free",
            "r1_OSMAC_Algorithm_Model_Free.R"))

OSMAC_MF<-cmpfun(OSMAC_MF)
getMLE<-cmpfun(getMLE)
Cordeiro<-cmpfun(Cordeiro)

# Using OSMAC to Sub-sample from Big Data----
Run_OSMAC_MF<-function(Replicates,r1,r2,Y,X,alpha,combs,All_Covariates,N,Theta)
{
  # From r0 and r conduct OSMAC subsampling for Logistic regression-----
  Parameter_mMSE<-replicate(length(combs),list()) ;
  Parameter_mVc<-replicate(length(combs),list()) ;

  Bias_mMSE<-replicate(length(combs),list()) ; 
  Bias_mVc<-replicate(length(combs),list()) ; 

  Utility_mMSE<-replicate(length(combs),list()) ; 
  Utility_mVc<-replicate(length(combs),list()) ;

  # Sample_mMSE<-replicate(length(combs),list()) ;  
  # Sample_mVc<-replicate(length(combs),list()) ; 
  
  i=1
  while(i <= Replicates)
  {
    tryCatch({
      Results<-OSMAC_MF(r1,r2,Y,X,n=N,alpha,combs,All_Covariates,Theta)
      
      for (j in 1:length(combs)) 
      {
        Parameter_mMSE[[j]][[i]]<-Results$opt[[j]]$Est_Theta_mMSE
        Parameter_mVc[[j]][[i]]<-Results$opt[[j]]$Est_Theta_mVc
        
        Bias_mMSE[[j]][[i]]<-Results$opt[[j]]$Bias_mMSE
        Bias_mVc[[j]][[i]]<-Results$opt[[j]]$Bias_mVc
        
        Utility_mMSE[[j]][[i]]<-Results$opt[[j]]$Utility_mMSE
        Utility_mVc[[j]][[i]]<-Results$opt[[j]]$Utility_mVc
        
        # Sample_mMSE[[j]][[i]]<-cbind(i,Results$Sample_mMSE[,colnames(Results$Sample_mMSE) %in% c("r2","Y","SP",combs[[j]]) ])
        # Sample_mVc[[j]][[i]]<-cbind(i,Results$Sample_mVc[,colnames(Results$Sample_mVc) %in% c("r2","Y","SP",combs[[j]]) ])
        
        colnames(Parameter_mMSE[[j]][[i]])<-colnames(Parameter_mVc[[j]][[i]])<-
        colnames(Bias_mMSE[[j]][[i]])<-colnames(Bias_mVc[[j]][[i]])<-
          c("Subsample_Size",paste0("Theta",0:(length(combs[[j]])-1)))
          
        colnames(Utility_mMSE[[j]][[i]])<-colnames(Utility_mVc[[j]][[i]])<-
          c("Subsample_Size",paste0(c("A","D"),"-optimality"))
          
        # colnames(Sample_mMSE[[j]][[i]])<-colnames(Sample_mVc[[j]][[i]])<-
        #   c("Simulation","Subsample_Size","Y","SP",combs[[j]])
      }
      cat(1:j,";",i,"\n")
      i<-i+1
    },error=function(e){ i=i })
  }
  
  Final_param_mMSE<-Final_param_mVc<-list()
  Final_bias_mMSE<-Final_bias_mVc<-list()
  Final_utility_mMSE<-Final_utility_mVc<-list()
  #Final_Sample_mMSE<-Final_Sample_mVc<-list()
  
  for (j in 1:length(combs)) 
  {
    Final_param_mMSE[[j]]<-do.call(rbind,Parameter_mMSE[[j]])
    Final_param_mVc[[j]]<-do.call(rbind,Parameter_mVc[[j]])
    
    Final_bias_mMSE[[j]]<-do.call(rbind,Bias_mMSE[[j]])
    Final_bias_mVc[[j]]<-do.call(rbind,Bias_mVc[[j]])
    
    Final_utility_mMSE[[j]]<-do.call(rbind,Utility_mMSE[[j]])
    Final_utility_mVc[[j]]<-do.call(rbind,Utility_mVc[[j]])
    
    # Final_Sample_mMSE[[j]]<-do.call(rbind,Sample_mMSE[[j]])
    # Final_Sample_mVc[[j]]<-do.call(rbind,Sample_mVc[[j]])
  }
  
  for (j in 1:length(combs)) 
  {
    assign(paste0("Results_OSMAC_",j),
           list("mMSE_Output"=cbind.data.frame(Type="mMSE",Final_param_mMSE[[j]]),
                "mVc_Output"=cbind.data.frame(Type="mVc",Final_param_mVc[[j]]))) 
    
    assign(paste0("Bias_OSMAC_",j),
           list("mMSE_Output"=cbind.data.frame(Type="mMSE",Final_bias_mMSE[[j]]),
                "mVc_Output"=cbind.data.frame(Type="mVc",Final_bias_mVc[[j]])))
    
    assign(paste0("Utility_OSMAC_",j),
           list("mMSE_Output"=cbind.data.frame(Type="mMSE",Final_utility_mMSE[[j]]),
                "mVc_Output"=cbind.data.frame(Type="mVc",Final_utility_mVc[[j]])))
    
    # assign(paste0("Sample_OSMAC_",j),
    #        list("mMSE_Output"=cbind.data.frame(Type="mMSE",Final_Sample_mMSE[[j]]),
    #             "mVc_Output"=cbind.data.frame(Type="mVc",Final_Sample_mVc[[j]])))
  } 
  
  save(list= c(paste0("Results_OSMAC_",1:length(combs)),paste0("Bias_OSMAC_",1:length(combs)),
               paste0("Utility_OSMAC_",1:length(combs))),
       file = here("Non_Identical_r0","Simulation_Setup","Analysis","OSMAC_Model_Free",
                   "Results","OSMAC_MF_output_r1.RData"))

  save(list = c(paste0("Results_OSMAC_",1:length(combs)),paste0("Bias_OSMAC_",1:length(combs)),
                paste0("Utility_OSMAC_",1:length(combs))),
       file = here("Non_Identical_r0","Outputs",Model_Path,"OSMAC_Model_Free",
                   "OSMAC_MF_output_r1.RData"))
  
  # for (i in 1:length(combs)) 
  # {
  #   save(list = c(paste0("Sample_OSMAC_",i)),
  #        file = here("Non_Identical_r0","Simulation_Setup","Analysis","OSMAC_Model_Free",
  #                    "Results",paste0("OSMAC_MF_Samples_output_",i,".RData")))
  #   
  #   save(list = c(paste0("Sample_OSMAC_",i)),
  #        file = here("Non_Identical_r0","Outputs",Model_Path,"OSMAC_Model_Free",
  #                    paste0("OSMAC_MF_Samples_output_",i,".RData")))
  # }
}

Run_OSMAC_MF<-cmpfun(Run_OSMAC_MF)

#Save the OSMAC Sample function----
save(list =c("Run_OSMAC_MF","OSMAC_MF","getMLE","Cordeiro"),
     file=here("Non_Identical_r0","Simulation_Setup","Analysis",
               "OSMAC_Model_Free","r1_Run_OSMAC.RData"))

rm(list = ls())

# Run the OSMAC sampling method ----
source(here("Non_Identical_r0","Simulation_Setup","Analysis","OSMAC_Model_Free",
            "r1_Simulation_Results_OSMAC_Sampling.R"))
