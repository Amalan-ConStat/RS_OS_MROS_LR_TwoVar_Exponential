library(here)
library(compiler)
library(LaplacesDemon)

# Load the OSMAC algorithm
source(here("Identical_r0","Simulation_Setup","R_Scripts","OSMAC_Algorithm.R"))

AlgTwoStp<-cmpfun(AlgTwoStp)
getMLE<-cmpfun(getMLE)
Cordeiro<-cmpfun(Cordeiro)

# Using OSMAC to Sub-sample from Big Data ----
Run_OSMAC<-function(Replicates,r1,r2,Y,X,Real_Data,N,alpha,combs,All_Covariates)
{
  X_Real<-Real_Data[,-1]
  # From r0 and r conduct OSMAC subsampling for Logistic regression-----
  Parameter_Real_mMSE<-list(); Parameter_Real_mVc<-list()
  Parameter_ModelFree_mMSE<-list(); Parameter_ModelFree_mVc<-list()
  
  for (j in 1:length(combs)) 
  {
    assign(paste0("Parameter_Assumed_Old_mMSE_",j),list())
    assign(paste0("Parameter_Assumed_Old_mVc_",j),list())
    assign(paste0("Parameter_Assumed_New_mMSE_",j),list())
    assign(paste0("Parameter_Assumed_New_mVc_",j),list())  
  }
  
  Bias_Real_mMSE<-list(); Bias_Real_mVc<-list()
  Bias_ModelFree_mMSE<-list(); Bias_ModelFree_mVc<-list()
  
  for(j in 1:length(combs))
  {
    assign(paste0("Bias_Assumed_Old_mMSE_",j),list())
    assign(paste0("Bias_Assumed_Old_mVc_",j),list())
    assign(paste0("Bias_Assumed_New_mMSE_",j),list())
    assign(paste0("Bias_Assumed_New_mVc_",j),list())
  }
  
  Utility_Real_mMSE<-list(); Utility_Real_mVc<-list()
  Utility_ModelFree_mMSE<-list(); Utility_ModelFree_mVc<-list()
  
  for(j in 1:length(combs))
  {
    assign(paste0("Utility_Assumed_Old_mMSE_",j),list())
    assign(paste0("Utility_Assumed_Old_mVc_",j),list())
    assign(paste0("Utility_Assumed_New_mMSE_",j),list())
    assign(paste0("Utility_Assumed_New_mVc_",j),list())
  }
  
  # Sample_Real_mMSE<-list(); Sample_Real_mVc<-list()
  # Sample_Assumed_Old_mMSE<-list(); Sample_Assumed_Old_mVc<-list()
  # Sample_Assumed_New_mMSE<-list(); Sample_Assumed_New_mVc<-list()
  # Sample_ModelFree_mMSE<-list(); Sample_ModelFree_mVc<-list()
  
  # Full_SP_Real<-list()
  # Full_SP_Assumed_Old<-list()
  # Full_SP_Assumed_New<-list()
  # Full_SP_ModelFree<-list()
  
  i=1
  while(i <= Replicates)
  {
    tryCatch({
      Results<-AlgTwoStp(r1,r2,Y,X,n=N,Real_Data,alpha,combs,All_Covariates)
      
      Parameter_Real_mMSE[[i]]<-Results$opt$opt_Real$Est_Theta_mMSE
      Parameter_Real_mVc[[i]]<-Results$opt$opt_Real$Est_Theta_mVc
      Parameter_ModelFree_mMSE[[i]]<-Results$opt$opt_join$Est_Theta_mMSE
      Parameter_ModelFree_mVc[[i]]<-Results$opt$opt_join$Est_Theta_mVc
      
      Parameter_Assumed_Old_mMSE_1[[i]] <-Results$opt$opt_Old$opt_Old_1$Est_Theta_mMSE
      Parameter_Assumed_Old_mVc_1[[i]]<-Results$opt$opt_Old$opt_Old_1$Est_Theta_mVc
      Parameter_Assumed_New_mMSE_1[[i]]<-Results$opt$opt_New$opt_New_1$Est_Theta_mMSE
      Parameter_Assumed_New_mVc_1[[i]]<-Results$opt$opt_New$opt_New_1$Est_Theta_mVc
      
      Parameter_Assumed_Old_mMSE_2[[i]] <-Results$opt$opt_Old$opt_Old_2$Est_Theta_mMSE
      Parameter_Assumed_Old_mVc_2[[i]]<-Results$opt$opt_Old$opt_Old_2$Est_Theta_mVc
      Parameter_Assumed_New_mMSE_2[[i]]<-Results$opt$opt_New$opt_New_2$Est_Theta_mMSE
      Parameter_Assumed_New_mVc_2[[i]]<-Results$opt$opt_New$opt_New_2$Est_Theta_mVc
      
      Parameter_Assumed_Old_mMSE_3[[i]] <-Results$opt$opt_Old$opt_Old_3$Est_Theta_mMSE
      Parameter_Assumed_Old_mVc_3[[i]]<-Results$opt$opt_Old$opt_Old_3$Est_Theta_mVc
      Parameter_Assumed_New_mMSE_3[[i]]<-Results$opt$opt_New$opt_New_3$Est_Theta_mMSE
      Parameter_Assumed_New_mVc_3[[i]]<-Results$opt$opt_New$opt_New_3$Est_Theta_mVc
      
      Bias_Real_mMSE[[i]]<-Results$opt$opt_Real$Bias_mMSE
      Bias_Real_mVc[[i]]<-Results$opt$opt_Real$Bias_mVc
      Bias_ModelFree_mMSE[[i]]<-Results$opt$opt_join$Bias_mMSE
      Bias_ModelFree_mVc[[i]]<-Results$opt$opt_join$Bias_mVc
      
      Bias_Assumed_Old_mMSE_1[[i]]<-Results$opt$opt_Old$opt_Old_1$Bias_mMSE
      Bias_Assumed_Old_mVc_1[[i]]<-Results$opt$opt_Old$opt_Old_1$Bias_mVc
      Bias_Assumed_New_mMSE_1[[i]]<-Results$opt$opt_New$opt_New_1$Bias_mMSE
      Bias_Assumed_New_mVc_1[[i]]<-Results$opt$opt_New$opt_New_1$Bias_mVc
      
      Bias_Assumed_Old_mMSE_2[[i]]<-Results$opt$opt_Old$opt_Old_2$Bias_mMSE
      Bias_Assumed_Old_mVc_2[[i]]<-Results$opt$opt_Old$opt_Old_2$Bias_mVc
      Bias_Assumed_New_mMSE_2[[i]]<-Results$opt$opt_New$opt_New_2$Bias_mMSE
      Bias_Assumed_New_mVc_2[[i]]<-Results$opt$opt_New$opt_New_2$Bias_mVc
      
      Bias_Assumed_Old_mMSE_3[[i]]<-Results$opt$opt_Old$opt_Old_3$Bias_mMSE
      Bias_Assumed_Old_mVc_3[[i]]<-Results$opt$opt_Old$opt_Old_3$Bias_mVc
      Bias_Assumed_New_mMSE_3[[i]]<-Results$opt$opt_New$opt_New_3$Bias_mMSE
      Bias_Assumed_New_mVc_3[[i]]<-Results$opt$opt_New$opt_New_3$Bias_mVc
     
      Utility_Real_mMSE[[i]]<-Results$opt$opt_Real$Utility_mMSE
      Utility_Real_mVc[[i]]<-Results$opt$opt_Real$Utility_mVc
      Utility_ModelFree_mMSE[[i]]<-Results$opt$opt_join$Utility_mMSE
      Utility_ModelFree_mVc[[i]]<-Results$opt$opt_join$Utility_mVc
      
      Utility_Assumed_Old_mMSE_1[[i]]<-Results$opt$opt_Old$opt_Old_1$Utility_mMSE
      Utility_Assumed_Old_mVc_1[[i]]<-Results$opt$opt_Old$opt_Old_1$Utility_mVc
      Utility_Assumed_New_mMSE_1[[i]]<-Results$opt$opt_New$opt_New_1$Utility_mMSE
      Utility_Assumed_New_mVc_1[[i]]<-Results$opt$opt_New$opt_New_1$Utility_mVc
      
      Utility_Assumed_Old_mMSE_2[[i]]<-Results$opt$opt_Old$opt_Old_2$Utility_mMSE
      Utility_Assumed_Old_mVc_2[[i]]<-Results$opt$opt_Old$opt_Old_2$Utility_mVc
      Utility_Assumed_New_mMSE_2[[i]]<-Results$opt$opt_New$opt_New_2$Utility_mMSE
      Utility_Assumed_New_mVc_2[[i]]<-Results$opt$opt_New$opt_New_2$Utility_mVc
      
      Utility_Assumed_Old_mMSE_3[[i]]<-Results$opt$opt_Old$opt_Old_3$Utility_mMSE
      Utility_Assumed_Old_mVc_3[[i]]<-Results$opt$opt_Old$opt_Old_3$Utility_mVc
      Utility_Assumed_New_mMSE_3[[i]]<-Results$opt$opt_New$opt_New_3$Utility_mMSE
      Utility_Assumed_New_mVc_3[[i]]<-Results$opt$opt_New$opt_New_3$Utility_mVc
      
      # Sample_Real_mMSE[[i]]<-cbind(i,Results$opt[[1]]$Sample_mMSE)
      # Sample_Real_mVc[[i]]<-cbind(i,Results$opt[[1]]$Sample_mVc)
      # Sample_Assumed_Old_mMSE[[i]]<-cbind(i,Results$opt[[2]]$Sample_mMSE)
      # Sample_Assumed_Old_mVc[[i]]<-cbind(i,Results$opt[[2]]$Sample_mVc)
      # Sample_Assumed_New_mMSE[[i]]<-cbind(i,Results$opt[[3]]$Sample_mMSE)
      # Sample_Assumed_New_mVc[[i]]<-cbind(i,Results$opt[[3]]$Sample_mVc)
      # Sample_ModelFree_mMSE[[i]]<-cbind(i,Results$opt[[4]]$Sample_mMSE)
      # Sample_ModelFree_mVc[[i]]<-cbind(i,Results$opt[[4]]$Sample_mVc)
      
      # Full_SP_Real[[i]]<-cbind(i,Results$opt[[1]]$Full_SP)
      # Full_SP_Assumed_Old[[i]]<-cbind(i,Results$opt[[2]]$Full_SP)
      # Full_SP_Assumed_New[[i]]<-cbind(i,Results$opt[[3]]$Full_SP)
      # Full_SP_ModelFree[[i]]<-cbind(i,Results$opt[[4]]$Full_SP)
      
      print(i)
      i<-i+1
    },error=function(e){ i=i; print("Failed")})
    
  }

  # Real Model
  Final_param_mMSE<-do.call(rbind,Parameter_Real_mMSE)
  Final_param_mVc<-do.call(rbind,Parameter_Real_mVc)
  
  Final_bias_mMSE<-do.call(rbind,Bias_Real_mMSE)
  Final_bias_mVc<-do.call(rbind,Bias_Real_mVc)
  
  Final_utility_mMSE<-do.call(rbind,Utility_Real_mMSE)
  Final_utility_mVc<-do.call(rbind,Utility_Real_mVc)
  
  # Final_Sample_mMSE<-do.call(rbind,Sample_Real_mMSE)
  # Final_Sample_mVc<-do.call(rbind,Sample_Real_mVc)
  # Final_SP<-do.call(rbind,Full_SP_Real)
  
  Results_OSMAC<-list("mMSE_Output"=cbind.data.frame("mMSE",Final_param_mMSE),
                      "mVc_Output"=cbind.data.frame("mVc",Final_param_mVc))
  colnames(Results_OSMAC$mMSE_Output)<-colnames(Results_OSMAC$mVc_Output)<-c("Type","Subsample_Size",
                                                                             paste0("Theta",0:(ncol(X_Real)-1)))
  
  Bias_OSMAC<-list("mMSE_Output"=cbind.data.frame("mMSE",Final_bias_mMSE),
                   "mVc_Output"=cbind.data.frame("mVc",Final_bias_mVc))
  colnames(Bias_OSMAC$mMSE_Output)<-colnames(Bias_OSMAC$mVc_Output)<-c("Type","Subsample_Size",
                                                                       paste0("Theta",0:(ncol(X_Real)-1)))
  
  Utility_OSMAC<-list("mMSE_Output"=cbind.data.frame("mMSE",Final_utility_mMSE),
                      "mVc_Output"=cbind.data.frame("mVc",Final_utility_mVc))
  colnames(Utility_OSMAC$mMSE_Output)<-colnames(Utility_OSMAC$mVc_Output)<-c("Type","Subsample_Size",
                                                                             "A_optimality","D_optimality")
  
  # Sample_OSMAC<-list("mMSE_Output"=cbind.data.frame("mMSE",Final_Sample_mMSE),
  #                    "mVc_Output"=cbind.data.frame("mVc",Final_Sample_mVc))
  # colnames(Sample_OSMAC$mMSE_Output)<-colnames(Sample_OSMAC$mVc_Output)<-c("Type","Simulation",
  #                                                                          "Subsample_Size","Y",
  #                                                                          paste0("X",0:(ncol(X_Real)-1)),"SP")
  
  # Probability<-as.data.frame(Final_SP)
  # colnames(Probability)<-c("Simulation","Y",paste0("X",0:(ncol(X_Real)-1)),"mMSE","mVc")
  
  save(list= c("Results_OSMAC","Bias_OSMAC","Utility_OSMAC"#,"Sample_OSMAC","Probability"
               ),
       file = here("Identical_r0","Simulation_Setup","Analysis","OSMAC",
                   "Results","Real_Model","OSMAC_output.RData"))
  
  save(list = c("Results_OSMAC","Bias_OSMAC","Utility_OSMAC"#,"Sample_OSMAC","Probability"
                ),
       file = here("Identical_r0","Outputs",Model_Path,"OSMAC","Real_Model","OSMAC_output.RData"))
  
  # Assumed Model Old
  for (j in 1:length(combs)) 
  {
    Final_param_mMSE<-do.call(rbind,get(paste0("Parameter_Assumed_Old_mMSE_",j)))
    Final_param_mVc<-do.call(rbind,get(paste0("Parameter_Assumed_Old_mVc_",j)))
    
    Final_bias_mMSE<-do.call(rbind,get(paste0("Bias_Assumed_Old_mMSE_",j)))
    Final_bias_mVc<-do.call(rbind,get(paste0("Bias_Assumed_Old_mVc_",j)))
    
    Final_utility_mMSE<-do.call(rbind,get(paste0("Utility_Assumed_Old_mMSE_",j)))
    Final_utility_mVc<-do.call(rbind,get(paste0("Utility_Assumed_Old_mVc_",j)))
    
    # Final_Sample_mMSE<-do.call(rbind,Sample_Assumed_Old_mMSE)
    # Final_Sample_mVc<-do.call(rbind,Sample_Assumed_Old_mVc)
    # 
    # Final_SP<-do.call(rbind,Full_SP_Assumed_Old)
    
    Results_OSMAC<-list("mMSE_Output"=cbind.data.frame("mMSE",Final_param_mMSE),
                        "mVc_Output"=cbind.data.frame("mVc",Final_param_mVc))
    colnames(Results_OSMAC$mMSE_Output)<-colnames(Results_OSMAC$mVc_Output)<-c("Type","Subsample_Size",
                                                                               paste0("Theta",0:(ncol(X_Real)-1)))
    
    Bias_OSMAC<-list("mMSE_Output"=cbind.data.frame("mMSE",Final_bias_mMSE),
                     "mVc_Output"=cbind.data.frame("mVc",Final_bias_mVc))
    colnames(Bias_OSMAC$mMSE_Output)<-colnames(Bias_OSMAC$mVc_Output)<-c("Type","Subsample_Size",
                                                                         paste0("Theta",0:(ncol(X_Real)-1)))
    
    Utility_OSMAC<-list("mMSE_Output"=cbind.data.frame("mMSE",Final_utility_mMSE),
                        "mVc_Output"=cbind.data.frame("mVc",Final_utility_mVc))
    colnames(Utility_OSMAC$mMSE_Output)<-colnames(Utility_OSMAC$mVc_Output)<-c("Type","Subsample_Size",
                                                                               "A_optimality","D_optimality")
    
    # Sample_OSMAC<-list("mMSE_Output"=cbind.data.frame("mMSE",Final_Sample_mMSE),
    #                    "mVc_Output"=cbind.data.frame("mVc",Final_Sample_mVc))
    # colnames(Sample_OSMAC$mMSE_Output)<-colnames(Sample_OSMAC$mVc_Output)<-c("Type","Simulation",
    #                                                                          "Subsample_Size","Y",
    #                                                                          paste0("X",0:(ncol(X_Real)-1)),"SP")
    # 
    # Probability<-as.data.frame(Final_SP)
    # colnames(Probability)<-c("Simulation","Y",paste0("X",0:(ncol(X_Real)-1)),"mMSE","mVc")
    
    save(list= c("Results_OSMAC","Bias_OSMAC","Utility_OSMAC"#,"Sample_OSMAC","Probability"
                 ),
         file = here("Identical_r0","Simulation_Setup","Analysis","OSMAC",
                     "Results","Assumed_Model",paste0("OSMAC_output_Old_",j,".RData")))
    
    save(list = c("Results_OSMAC","Bias_OSMAC","Utility_OSMAC"#,"Sample_OSMAC","Probability"
                  ),
         file = here("Identical_r0","Outputs",Model_Path,"OSMAC",
                     "Assumed_Model",paste0("OSMAC_output_Old_",j,".RData")))
    
    # Assumed Model New
    Final_param_mMSE<-do.call(rbind,get(paste0("Parameter_Assumed_New_mMSE_",j)))
    Final_param_mVc<-do.call(rbind,get(paste0("Parameter_Assumed_New_mVc_",j)))
    
    Final_bias_mMSE<-do.call(rbind,get(paste0("Bias_Assumed_New_mMSE_",j)))
    Final_bias_mVc<-do.call(rbind,get(paste0("Bias_Assumed_New_mVc_",j)))
    
    Final_utility_mMSE<-do.call(rbind,get(paste0("Utility_Assumed_New_mMSE_",j)))
    Final_utility_mVc<-do.call(rbind,get(paste0("Utility_Assumed_New_mVc_",j)))
    
    # Final_Sample_mMSE<-do.call(rbind,Sample_Assumed_New_mMSE)
    # Final_Sample_mVc<-do.call(rbind,Sample_Assumed_New_mVc)
    # 
    # Final_SP<-do.call(rbind,Full_SP_Assumed_New)
    
    Results_OSMAC<-list("mMSE_Output"=cbind.data.frame("mMSE",Final_param_mMSE),
                        "mVc_Output"=cbind.data.frame("mVc",Final_param_mVc))
    colnames(Results_OSMAC$mMSE_Output)<-colnames(Results_OSMAC$mVc_Output)<-c("Type","Subsample_Size",
                                                                               paste0("Theta",0:(ncol(X_Real)-1)))
    
    Bias_OSMAC<-list("mMSE_Output"=cbind.data.frame("mMSE",Final_bias_mMSE),
                     "mVc_Output"=cbind.data.frame("mVc",Final_bias_mVc))
    colnames(Bias_OSMAC$mMSE_Output)<-colnames(Bias_OSMAC$mVc_Output)<-c("Type","Subsample_Size",
                                                                         paste0("Theta",0:(ncol(X_Real)-1)))
    
    Utility_OSMAC<-list("mMSE_Output"=cbind.data.frame("mMSE",Final_utility_mMSE),
                        "mVc_Output"=cbind.data.frame("mVc",Final_utility_mVc))
    colnames(Utility_OSMAC$mMSE_Output)<-colnames(Utility_OSMAC$mVc_Output)<-c("Type","Subsample_Size",
                                                                               "A_optimality","D_optimality")
    
    # Sample_OSMAC<-list("mMSE_Output"=cbind.data.frame("mMSE",Final_Sample_mMSE),
    #                    "mVc_Output"=cbind.data.frame("mVc",Final_Sample_mVc))
    # colnames(Sample_OSMAC$mMSE_Output)<-colnames(Sample_OSMAC$mVc_Output)<-c("Type","Simulation",
    #                                                                          "Subsample_Size","Y",
    #                                                                          paste0("X",0:(ncol(X_Real)-1)),"SP")
    # 
    # Probability<-as.data.frame(Final_SP)
    # colnames(Probability)<-c("Simulation","Y",paste0("X",0:(ncol(X_Real)-1)),"mMSE","mVc")
    
    save(list= c("Results_OSMAC","Bias_OSMAC","Utility_OSMAC"#,"Sample_OSMAC","Probability"
                ),
    file = here("Identical_r0","Simulation_Setup","Analysis","OSMAC","Results",
                "Assumed_Model",paste0("OSMAC_output_New_",j,".RData")))
    
    save(list = c("Results_OSMAC","Bias_OSMAC","Utility_OSMAC"#,"Sample_OSMAC","Probability"
                  ),
    file = here("Identical_r0","Outputs",Model_Path,"OSMAC",
                "Assumed_Model",paste0("OSMAC_output_New_",j,".RData")))
  }
  
  # Model Free
  Final_param_mMSE<-do.call(rbind,Parameter_ModelFree_mMSE)
  Final_param_mVc<-do.call(rbind,Parameter_ModelFree_mVc)
  
  Final_bias_mMSE<-do.call(rbind,Bias_ModelFree_mMSE)
  Final_bias_mVc<-do.call(rbind,Bias_ModelFree_mVc)
  
  Final_utility_mMSE<-do.call(rbind,Utility_ModelFree_mMSE)
  Final_utility_mVc<-do.call(rbind,Utility_ModelFree_mVc)
  
  # Final_Sample_mMSE<-do.call(rbind,Sample_ModelFree_mMSE)
  # Final_Sample_mVc<-do.call(rbind,Sample_ModelFree_mVc)
  # 
  # Final_SP<-do.call(rbind,Full_SP_ModelFree)
  
  Results_OSMAC<-list("mMSE_Output"=cbind.data.frame("mMSE",Final_param_mMSE),
                      "mVc_Output"=cbind.data.frame("mVc",Final_param_mVc))
  colnames(Results_OSMAC$mMSE_Output)<-colnames(Results_OSMAC$mVc_Output)<-c("Type","Subsample_Size",
                                                                             paste0("Theta",0:(ncol(X_Real)-1)))
  
  Bias_OSMAC<-list("mMSE_Output"=cbind.data.frame("mMSE",Final_bias_mMSE),
                   "mVc_Output"=cbind.data.frame("mVc",Final_bias_mVc))
  colnames(Bias_OSMAC$mMSE_Output)<-colnames(Bias_OSMAC$mVc_Output)<-c("Type","Subsample_Size",
                                                                       paste0("Theta",0:(ncol(X_Real)-1)))
  
  Utility_OSMAC<-list("mMSE_Output"=cbind.data.frame("mMSE",Final_utility_mMSE),
                      "mVc_Output"=cbind.data.frame("mVc",Final_utility_mVc))
  colnames(Utility_OSMAC$mMSE_Output)<-colnames(Utility_OSMAC$mVc_Output)<-c("Type","Subsample_Size",
                                                                             "A_optimality","D_optimality")
  
  # Sample_OSMAC<-list("mMSE_Output"=cbind.data.frame("mMSE",Final_Sample_mMSE),
  #                    "mVc_Output"=cbind.data.frame("mVc",Final_Sample_mVc))
  # colnames(Sample_OSMAC$mMSE_Output)<-colnames(Sample_OSMAC$mVc_Output)<-c("Type","Simulation",
  #                                                                          "Subsample_Size","Y",
  #                                                                          paste0("X",0:(ncol(X_Real)-1)),"SP")
  # 
  # Probability<-as.data.frame(Final_SP)
  # colnames(Probability)<-c("Simulation","Y",paste0("X",0:(ncol(X_Real)-1)),"mMSE","mVc")
  
  save(list= c("Results_OSMAC","Bias_OSMAC","Utility_OSMAC"#,"Sample_OSMAC","Probability"
               ),
       file = here("Identical_r0","Simulation_Setup","Analysis","OSMAC","Results",
                   "ModelFree","OSMAC_output.RData"))
  
  save(list = c("Results_OSMAC","Bias_OSMAC","Utility_OSMAC"#,"Sample_OSMAC","Probability"
                ),
       file = here("Identical_r0","Outputs",Model_Path,"OSMAC","ModelFree","OSMAC_output.RData"))
}

Run_OSMAC<-cmpfun(Run_OSMAC)

#Save the OSMAC Sample function----
save(list =c("Run_OSMAC","AlgTwoStp","getMLE","Cordeiro"),
     file=here("Identical_r0","Simulation_Setup","Analysis","OSMAC","Run_OSMAC.RData"))

rm(list = ls())

# Run the OSMAC sampling method ----
source(here("Identical_r0","Simulation_Setup","Analysis","OSMAC",
            "Simulation_Results_OSMAC_Sampling.R"))
