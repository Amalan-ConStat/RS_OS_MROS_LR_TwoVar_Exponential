# Model 1 ----

# Real Model :$$ log\bigg(\frac{p}{1-p}\bigg) = \beta_0 + \beta_1 X_1 + \beta_2 X_2$$
# Assumed Model 1:$$ log\bigg(\frac{p}{1-p}\bigg) = \beta_0 + \beta_1 X_1 + \beta_2 X_2 + \beta_3 X_1^2$$
# Assumed Model 2:$$ log\bigg(\frac{p}{1-p}\bigg) = \beta_0 + \beta_1 X_1 + \beta_2 X_2 + \beta_3 X_2^2$$
# Assumed Model 3:$$ log\bigg(\frac{p}{1-p}\bigg) = \beta_0 + \beta_1 X_1 + \beta_2 X_2 + \beta_3 X_1^2 + \beta_4 X_2^2$$

A_optimality<-function(x,Theta,Model)
{
  x1<-x[1]; x2<-x[2]
  if(Model=="Real_Model") {X<-cbind(1,x1,x2)}
         
  if(Model=="Assumed_Model_1") {X<-cbind(1,x1,x2,x1^2)}
  
  if(Model=="Assumed_Model_2") {X<-cbind(1,x1,x2,x2^2)}
  
  if(Model=="Assumed_Model_3") {X<-cbind(1,x1,x2,x1^2,x2^2)}
  
  lp <- X%*%Theta
  p <- as.vector(exp(lp)/(1+exp(lp)))
  W <- diag(p*(1-p),1)
  sum(diag(solve(t(X)%*%W%*%X)))
}
  
All_Models<-list(Real_Model=c("X0","X1","X2"),
                 Assumed_Model_1=c("X0","X1","X2","X1^2"),
                 Assumed_Model_2=c("X0","X1","X2","X2^2"),
                 Assumed_Model_3=c("X0","X1","X2","X1^2","X2^2"))

Generate_Data<-function(Rate,Theta,N,All_Models)
{
  X<-replicate(2,rexp(n = N, rate = Rate))
  Complete_Data<-cbind(1,X,X^2)
  colnames(Complete_Data)<-c(paste0("X",0:ncol(X)),
                             paste0("X",1:ncol(X),"^2"))
  
  Pi_Data <- invlogit(Complete_Data[,colnames(Complete_Data) %in% All_Models$Real_Model]%*%Theta)
  Y_Data <- rbern(N,Pi_Data)
  
  All_Data<-list()
  for (i in 1:length(All_Models)) 
  {
    All_Data[[i]]<-cbind(Y=Y_Data,Complete_Data[,colnames(Complete_Data) %in% All_Models[[i]] ])
  }
  
  names(All_Data)<-names(All_Models)
  
  Simulated_Data<-list("Basic"=list("N"=N,
                                    "Theta"=Theta,
                                    "Rate"=Rate,
                                    "Pi"=Pi_Data),
                       "All_Data"=All_Data)
  
  return(list(Simulated_Data,Y_Data,X,Pi_Data))
}
  
# Set parameters
N <- c(100,200,1000,5000,10000)

Simulated_Data<-list()
Testing<-NULL
Testing_2<-NULL
Plot_Collection_1<-list()
Plot_Collection_2<-list()
All_Plots_1<-list()
All_Plots_2<-list()

Rate<-3

Theta<-as.matrix(cbind(-1,0.4,seq(0.1,0.9,0.2)))
#Theta<-as.matrix(cbind(-1,seq(0.1,0.9,0.2),0.5))
#Theta<-as.matrix(cbind(-1,seq(0.1,0.9,0.2),seq(0.1,0.9,0.2)))

for (j in 1:nrow(Theta))
{
  Temp_Theta<-Theta[j,]
  
  for (i in 1:length(N))
    {
    Temp_N<-N[i]
    All_Data<-Generate_Data(Rate = Rate,Theta = Temp_Theta,N=Temp_N,All_Models =All_Models)
    
    Results_Temp<-list()
    Results_Tempsy<-list()
    
    for (k in 1:length(All_Models)) 
    {
      glm(Y~.-1,data = as.data.frame(All_Data[[1]]$All_Data[[k]]),
          family = binomial("logit"))->Results_Temp[[k]]
      
      summary(Results_Temp[[k]])->Results_Tempsy[[k]]
      print(names(All_Models[k]))
      print(Results_Tempsy[[k]]$coefficients[,4])
    }
    
    Plotting_Data<-cbind.data.frame(Y_Data=All_Data[[2]],
                                    X1=All_Data[[3]][,1],
                                    X2=All_Data[[3]][,2],
                                    Actual=log(All_Data[[4]]/(1-All_Data[[4]])),
                                    Real_E=log(Results_Temp[[1]]$fitted.values/(1-Results_Temp[[1]]$fitted.values)),
                                    Assumed_1_E=log(Results_Temp[[2]]$fitted.values/(1-Results_Temp[[2]]$fitted.values)), 
                                    Assumed_2_E=log(Results_Temp[[3]]$fitted.values/(1-Results_Temp[[3]]$fitted.values)), 
                                    Assumed_3_E=log(Results_Temp[[4]]$fitted.values/(1-Results_Temp[[4]]$fitted.values)))
    
    if(N[length(N)]==Temp_N)
    {
      sum(round(Plotting_Data$Actual,4)==round(Plotting_Data$Real_E,4))+
        sum(round(Plotting_Data$Actual,4)==round(Plotting_Data$Assumed_1_E,4))+
        sum(round(Plotting_Data$Real_E,4)==round(Plotting_Data$Assumed_1_E,4))+ 
        sum(round(Plotting_Data$Actual,4)==round(Plotting_Data$Assumed_2_E,4))+
        sum(round(Plotting_Data$Real_E,4)==round(Plotting_Data$Assumed_2_E,4))+
        sum(round(Plotting_Data$Actual,4)==round(Plotting_Data$Assumed_3_E,4))+
        sum(round(Plotting_Data$Real_E,4)==round(Plotting_Data$Assumed_3_E,4))+
        sum(round(Plotting_Data$Assumed_1_E,4)==round(Plotting_Data$Assumed_2_E,4))+
        sum(round(Plotting_Data$Assumed_1_E,4)==round(Plotting_Data$Assumed_3_E,4))+
        sum(round(Plotting_Data$Assumed_2_E,4)==round(Plotting_Data$Assumed_3_E,4))->Testing[j]
      
      sum(abs(round(Plotting_Data$Actual,4)-round(Plotting_Data$Real_E,4)))+
        sum(abs(round(Plotting_Data$Actual,4)-round(Plotting_Data$Assumed_1_E,4)))+
        sum(abs(round(Plotting_Data$Real_E,4)-round(Plotting_Data$Assumed_1_E,4)))+ 
        sum(abs(round(Plotting_Data$Actual,4)-round(Plotting_Data$Assumed_2_E,4)))+
        sum(abs(round(Plotting_Data$Real_E,4)-round(Plotting_Data$Assumed_2_E,4)))+
        sum(abs(round(Plotting_Data$Actual,4)-round(Plotting_Data$Assumed_3_E,4)))+
        sum(abs(round(Plotting_Data$Real_E,4)-round(Plotting_Data$Assumed_3_E,4)))+
        sum(abs(round(Plotting_Data$Assumed_1_E,4)-round(Plotting_Data$Assumed_2_E,4)))+
        sum(abs(round(Plotting_Data$Assumed_1_E,4)-round(Plotting_Data$Assumed_3_E,4)))+
        sum(abs(round(Plotting_Data$Assumed_2_E,4)-round(Plotting_Data$Assumed_3_E,4)))->Testing_2[j]
      
      Simulated_Data[[j]] <- All_Data[[1]]
    }
    
    Plotting_Data %>%
      pivot_longer(cols = Actual:Assumed_3_E,names_to = "Model",values_to = "Values") %>%
      ggplot(.,aes(x=X1,y=Values,color=factor(Model)))+
      geom_point(alpha=0.5)+
      #geom_line(alpha=0.5)+
      xlab("X1")+ylab("log(p/(1-p))")+theme_bw()+
      scale_color_viridis_d(guide = guide_legend(override.aes = list(alpha = 1) ) )+
      ggtitle("Comparing Real vs Assumed models",
              subtitle = paste("N =",Temp_N,
                               ", \U03F4_0 =",Theta[j,1],",\U03F4_1 =",Theta[j,2],",\U03F4_2 =",Theta[j,3]))+
      #geom_vline(xintercept=Opt_1,color="Blue",size=1.5)+
      #geom_vline(xintercept=Opt_2,color="Red",size=1.5)+
      theme(plot.title = element_text(size = 8),
            plot.subtitle = element_text(size = 7),
            legend.position = "none") ->Plot_Collection_1[[i]]
    
    Plotting_Data %>%
      pivot_longer(cols = Actual:Assumed_3_E,names_to = "Model",values_to = "Values") %>%
    ggplot(.,aes(x=X2,y=Values,color=factor(Model)))+
      geom_point(alpha=0.5)+
      #geom_line(alpha=0.5)+
      xlab("X2")+ylab("log(p/(1-p))")+theme_bw()+
      scale_color_viridis_d(guide = guide_legend(override.aes = list(alpha = 1) ) )+
      ggtitle("Comparing Real vs Assumed models",
              subtitle = paste("N =",Temp_N,
                               ", \U03F4_0 =",Theta[j,1],",\U03F4_1 =",Theta[j,2],",\U03F4_2 =",Theta[j,3]))+
      #geom_vline(xintercept=Opt_1,color="Blue",size=1.5)+
      #geom_vline(xintercept=Opt_2,color="Red",size=1.5)+
      theme(plot.title = element_text(size = 8),
            plot.subtitle = element_text(size = 7),
            legend.position = "none") ->Plot_Collection_2[[i]]
  }
  All_Plots_1[[j]]<-Plot_Collection_1
  All_Plots_2[[j]]<-Plot_Collection_2
}

ggarrange(ggarrange(plotlist =All_Plots_1[[1]],nrow = 1),
          ggarrange(plotlist =All_Plots_1[[2]],nrow = 1),
          ggarrange(plotlist =All_Plots_1[[3]],nrow = 1),
          ggarrange(plotlist =All_Plots_1[[4]],nrow = 1),
          ggarrange(plotlist =All_Plots_1[[5]],nrow = 1,
                    common.legend = TRUE,legend = "bottom"),
          nrow = nrow(Theta),ncol = 1) %>%
ggexport(filename = "Model_1_X1.jpeg",nrow = nrow(Theta),ncol = length(N),
         width = 1024,height = 1024)

ggarrange(ggarrange(plotlist =All_Plots_2[[1]],nrow = 1),
          ggarrange(plotlist =All_Plots_2[[2]],nrow = 1),
          ggarrange(plotlist =All_Plots_2[[3]],nrow = 1),
          ggarrange(plotlist =All_Plots_2[[4]],nrow = 1),
          ggarrange(plotlist =All_Plots_2[[5]],nrow = 1,
                    common.legend = TRUE,legend = "bottom"),
          nrow = nrow(Theta),ncol = 1) %>%
  ggexport(filename = "Model_1_X2.jpeg",nrow = nrow(Theta),ncol = length(N),
           width = 1024,height = 1024)

cbind(Testing,Testing_2)

Model_Path<-"Model_1"; Subsample_Size<-1500; r0<-Nc_size<-100; Replicates <- 1000
Simulated_Data<-Simulated_Data[[1]]

save(list = c("Simulated_Data","Subsample_Size","Replicates","Model_Path","Nc_size","r0"),
     file=here("Identical_r0","Generate_Big_Data",Model_Path,"No_Correlated_Covariate.RData"))

save(list = c("Simulated_Data","Subsample_Size","Replicates","Model_Path","Nc_size","r0"),
     file=here("Non_Identical_r0","Generate_Big_Data",Model_Path,"No_Correlated_Covariate.RData"))

rm(list = ls())

# Model 2 ----

# Real Model :$$ log\bigg(\frac{p}{1-p}\bigg) = \beta_0 + \beta_1 X_1 + \beta_2 X_2 + \beta_3 X_1^2$$
# Assumed Model 1:$$ log\bigg(\frac{p}{1-p}\bigg) = \beta_0 + \beta_1 X_1 + \beta_2 X_2$$
# Assumed Model 2:$$ log\bigg(\frac{p}{1-p}\bigg) = \beta_0 + \beta_1 X_1 + \beta_2 X_2 + \beta_3 X_2^2$$
# Assumed Model 3:$$ log\bigg(\frac{p}{1-p}\bigg) = \beta_0 + \beta_1 X_1 + \beta_2 X_2 + \beta_3 X_1^2 + \beta_4 X_2^2$$

A_optimality<-function(x,Theta,Model)
{
  x1<-x[1]; x2<-x[2]
  if(Model=="Real_Model") {X<-cbind(1,x1,x2,x1^2)}
  
  if(Model=="Assumed_Model_1") {X<-cbind(1,x1,x2)}
  
  if(Model=="Assumed_Model_2") {X<-cbind(1,x1,x2,x2^2)}
  
  if(Model=="Assumed_Model_3") {X<-cbind(1,x1,x2,x1^2,x2^2)}
  
  lp <- X%*%Theta
  p <- as.vector(exp(lp)/(1+exp(lp)))
  W <- diag(p*(1-p),1)
  sum(diag(solve(t(X)%*%W%*%X)))
}

All_Models<-list(Real_Model=c("X0","X1","X2","X1^2"),
                 Assumed_Model_1=c("X0","X1","X2"),
                 Assumed_Model_2=c("X0","X1","X2","X2^2"),
                 Assumed_Model_3=c("X0","X1","X2","X1^2","X2^2"))

Generate_Data<-function(Rate,Theta,N,All_Models)
{
  X<-replicate(2,rexp(n = N, rate = Rate))
  Complete_Data<-cbind(1,X,X^2)
  colnames(Complete_Data)<-c(paste0("X",0:ncol(X)),
                             paste0("X",1:ncol(X),"^2"))
  
  Pi_Data <- invlogit(Complete_Data[,colnames(Complete_Data) %in% All_Models$Real_Model]%*%Theta)
  Y_Data <- rbern(N,Pi_Data)
  
  All_Data<-list()
  for (i in 1:length(All_Models)) 
  {
    All_Data[[i]]<-cbind(Y=Y_Data,Complete_Data[,colnames(Complete_Data) %in% All_Models[[i]] ])
  }
  
  names(All_Data)<-names(All_Models)
  
  Simulated_Data<-list("Basic"=list("N"=N,
                                    "Theta"=Theta,
                                    "Rate"=Rate,
                                    "Pi"=Pi_Data),
                       "All_Data"=All_Data)
  
  return(list(Simulated_Data,Y_Data,X,Pi_Data))
}

# Set parameters
N <- c(100,200,1000,5000,10000)

Simulated_Data<-list()
Testing<-NULL
Testing_2<-NULL
Plot_Collection_1<-list()
Plot_Collection_2<-list()
All_Plots_1<-list()
All_Plots_2<-list()

Rate<-sqrt(3)
Theta<-as.matrix(cbind(-2,1.7,-1.2,seq(0.2,1.0,0.2)))

for (j in 1:nrow(Theta))
{
  Temp_Theta<-Theta[j,]
  
  for (i in 1:length(N))
  {
    Temp_N<-N[i]
    All_Data<-Generate_Data(Rate=Rate,Theta = Temp_Theta,N=Temp_N,All_Models =All_Models)
    
    Results_Temp<-list()
    Results_Tempsy<-list()
    
    for (k in 1:length(All_Models)) 
    {
      glm(Y~.-1,data = as.data.frame(All_Data[[1]]$All_Data[[k]]),
          family = binomial("logit"))->Results_Temp[[k]]
      
      summary(Results_Temp[[k]])->Results_Tempsy[[k]]
      print(names(All_Models[k]))
      print(Results_Tempsy[[k]]$coefficients[,4])
    }
    
    Plotting_Data<-cbind.data.frame(Y_Data=All_Data[[2]],
                                    X1=All_Data[[3]][,1],
                                    X2=All_Data[[3]][,2],
                                    Actual=log(All_Data[[4]]/(1-All_Data[[4]])),
                                    Real_E=log(Results_Temp[[1]]$fitted.values/(1-Results_Temp[[1]]$fitted.values)),
                                    Assumed_1_E=log(Results_Temp[[2]]$fitted.values/(1-Results_Temp[[2]]$fitted.values)), 
                                    Assumed_2_E=log(Results_Temp[[3]]$fitted.values/(1-Results_Temp[[3]]$fitted.values)), 
                                    Assumed_3_E=log(Results_Temp[[4]]$fitted.values/(1-Results_Temp[[4]]$fitted.values)))
    
    if(N[length(N)]==Temp_N)
    {
      sum(round(Plotting_Data$Actual,4)==round(Plotting_Data$Real_E,4))+
        sum(round(Plotting_Data$Actual,4)==round(Plotting_Data$Assumed_1_E,4))+
        sum(round(Plotting_Data$Real_E,4)==round(Plotting_Data$Assumed_1_E,4))+ 
        sum(round(Plotting_Data$Actual,4)==round(Plotting_Data$Assumed_2_E,4))+
        sum(round(Plotting_Data$Real_E,4)==round(Plotting_Data$Assumed_2_E,4))+
        sum(round(Plotting_Data$Actual,4)==round(Plotting_Data$Assumed_3_E,4))+
        sum(round(Plotting_Data$Real_E,4)==round(Plotting_Data$Assumed_3_E,4))+
        sum(round(Plotting_Data$Assumed_1_E,4)==round(Plotting_Data$Assumed_2_E,4))+
        sum(round(Plotting_Data$Assumed_1_E,4)==round(Plotting_Data$Assumed_3_E,4))+
        sum(round(Plotting_Data$Assumed_2_E,4)==round(Plotting_Data$Assumed_3_E,4))->Testing[j]
      
      sum(abs(round(Plotting_Data$Actual,4)-round(Plotting_Data$Real_E,4)))+
        sum(abs(round(Plotting_Data$Actual,4)-round(Plotting_Data$Assumed_1_E,4)))+
        sum(abs(round(Plotting_Data$Real_E,4)-round(Plotting_Data$Assumed_1_E,4)))+ 
        sum(abs(round(Plotting_Data$Actual,4)-round(Plotting_Data$Assumed_2_E,4)))+
        sum(abs(round(Plotting_Data$Real_E,4)-round(Plotting_Data$Assumed_2_E,4)))+
        sum(abs(round(Plotting_Data$Actual,4)-round(Plotting_Data$Assumed_3_E,4)))+
        sum(abs(round(Plotting_Data$Real_E,4)-round(Plotting_Data$Assumed_3_E,4)))+
        sum(abs(round(Plotting_Data$Assumed_1_E,4)-round(Plotting_Data$Assumed_2_E,4)))+
        sum(abs(round(Plotting_Data$Assumed_1_E,4)-round(Plotting_Data$Assumed_3_E,4)))+
        sum(abs(round(Plotting_Data$Assumed_2_E,4)-round(Plotting_Data$Assumed_3_E,4)))->Testing_2[j]
      
      Simulated_Data[[j]] <- All_Data[[1]]
    }
    
    Plotting_Data %>%
      pivot_longer(cols = Actual:Assumed_3_E,names_to = "Model",values_to = "Values") %>%
      ggplot(.,aes(x=X1,y=Values,color=factor(Model)))+
      geom_point(alpha=0.5)+
      #geom_line(alpha=0.5)+
      xlab("X1")+ylab("log(p/(1-p))")+theme_bw()+
      scale_color_viridis_d(guide = guide_legend(override.aes = list(alpha = 1) ) )+
      ggtitle("Comparing Real vs Assumed models",
              subtitle = paste("N =",Temp_N,
                               ", \U03F4_0 =",Theta[j,1],",\U03F4_1 =",Theta[j,2],
                               ", \U03F4_2 =",Theta[j,3],",\U03F4_3 =",Theta[j,4]))+
      #geom_vline(xintercept=Opt_1,color="Blue",size=1.5)+
      #geom_vline(xintercept=Opt_2,color="Red",size=1.5)+
      theme(plot.title = element_text(size = 8),
            plot.subtitle = element_text(size = 7),
            legend.position = "none") ->Plot_Collection_1[[i]]
    
    Plotting_Data %>%
      pivot_longer(cols = Actual:Assumed_3_E,names_to = "Model",values_to = "Values") %>%
      ggplot(.,aes(x=X2,y=Values,color=factor(Model)))+
      geom_point(alpha=0.5)+
      #geom_line(alpha=0.5)+
      xlab("X2")+ylab("log(p/(1-p))")+theme_bw()+
      scale_color_viridis_d(guide = guide_legend(override.aes = list(alpha = 1) ) )+
      ggtitle("Comparing Real vs Assumed models",
              subtitle = paste("N =",Temp_N,
                               ", \U03F4_0 =",Theta[j,1],",\U03F4_1 =",Theta[j,2],
                               ", \U03F4_2 =",Theta[j,3],",\U03F4_3 =",Theta[j,4]))+
      #geom_vline(xintercept=Opt_1,color="Blue",size=1.5)+
      #geom_vline(xintercept=Opt_2,color="Red",size=1.5)+
      theme(plot.title = element_text(size = 8),
            plot.subtitle = element_text(size = 7),
            legend.position = "none") ->Plot_Collection_2[[i]]
  }
  All_Plots_1[[j]]<-Plot_Collection_1
  All_Plots_2[[j]]<-Plot_Collection_2
}

ggarrange(ggarrange(plotlist =All_Plots_1[[1]],nrow = 1),
          ggarrange(plotlist =All_Plots_1[[2]],nrow = 1),
          ggarrange(plotlist =All_Plots_1[[3]],nrow = 1),
          ggarrange(plotlist =All_Plots_1[[4]],nrow = 1),
          ggarrange(plotlist =All_Plots_1[[5]],nrow = 1,
                    common.legend = TRUE,legend = "bottom"),
          nrow = nrow(Theta),ncol = 1) %>%
  ggexport(filename = "Model_2_X1.jpeg",nrow = nrow(Theta),ncol = length(N),
           width = 1024,height = 1024)

ggarrange(ggarrange(plotlist =All_Plots_2[[1]],nrow = 1),
          ggarrange(plotlist =All_Plots_2[[2]],nrow = 1),
          ggarrange(plotlist =All_Plots_2[[3]],nrow = 1),
          ggarrange(plotlist =All_Plots_2[[4]],nrow = 1),
          ggarrange(plotlist =All_Plots_2[[5]],nrow = 1,
                    common.legend=TRUE,legend = "bottom"),
          nrow = nrow(Theta),ncol = 1) %>%
  ggexport(filename = "Model_2_X2.jpeg",nrow = nrow(Theta),ncol = length(N),
           width = 1024,height = 1024)

cbind(Testing,Testing_2)

Model_Path<-"Model_2"; Subsample_Size<-1500; r0<-Nc_size<-100; Replicates <- 1000
Simulated_Data<-Simulated_Data[[1]]

save(list = c("Simulated_Data","Subsample_Size","Replicates","Model_Path","Nc_size","r0"),
     file=here("Identical_r0","Generate_Big_Data",Model_Path,"No_Correlated_Covariate.RData"))

save(list = c("Simulated_Data","Subsample_Size","Replicates","Model_Path","Nc_size","r0"),
     file=here("Non_Identical_r0","Generate_Big_Data",Model_Path,"No_Correlated_Covariate.RData"))

rm(list = ls())

# Model 3 ----

# Real Model :$$ log\bigg(\frac{p}{1-p}\bigg) = \beta_0 + \beta_1 X_1 + \beta_2 X_2 + \beta_3 X_2^2$$
# Assumed Model 1:$$ log\bigg(\frac{p}{1-p}\bigg) = \beta_0 + \beta_1 X_1 + \beta_2 X_2$$
# Assumed Model 2:$$ log\bigg(\frac{p}{1-p}\bigg) = \beta_0 + \beta_1 X_1 + \beta_2 X_2 + \beta_3 X_1^2$$
# Assumed Model 3:$$ log\bigg(\frac{p}{1-p}\bigg) = \beta_0 + \beta_1 X_1 + \beta_2 X_2 + \beta_3 X_1^2 + \beta_4 X_2^2$$

A_optimality<-function(x,Theta,Model)
{
  x1<-x[1]; x2<-x[2]
  if(Model=="Real_Model") {X<-cbind(1,x1,x2,x2^2)}
  
  if(Model=="Assumed_Model_1") {X<-cbind(1,x1,x2)}
  
  if(Model=="Assumed_Model_2") {X<-cbind(1,x1,x2,x1^2)}
  
  if(Model=="Assumed_Model_3") {X<-cbind(1,x1,x2,x1^2,x2^2)}
  
  lp <- X%*%Theta
  p <- as.vector(exp(lp)/(1+exp(lp)))
  W <- diag(p*(1-p),1)
  sum(diag(solve(t(X)%*%W%*%X)))
}

All_Models<-list(Real_Model=c("X0","X1","X2","X2^2"),
                 Assumed_Model_1=c("X0","X1","X2"),
                 Assumed_Model_2=c("X0","X1","X2","X1^2"),
                 Assumed_Model_3=c("X0","X1","X2","X1^2","X2^2"))

Generate_Data<-function(Rate,Theta,N,All_Models)
{
  X<-replicate(2,rexp(n = N, rate = Rate))
  Complete_Data<-cbind(1,X,X^2)
  colnames(Complete_Data)<-c(paste0("X",0:ncol(X)),
                             paste0("X",1:ncol(X),"^2"))
  
  Pi_Data <- invlogit(Complete_Data[,colnames(Complete_Data) %in% All_Models$Real_Model]%*%Theta)
  Y_Data <- rbern(N,Pi_Data)
  
  All_Data<-list()
  for (i in 1:length(All_Models)) 
  {
    All_Data[[i]]<-cbind(Y=Y_Data,Complete_Data[,colnames(Complete_Data) %in% All_Models[[i]] ])
  }
  
  names(All_Data)<-names(All_Models)
  
  Simulated_Data<-list("Basic"=list("N"=N,
                                    "Theta"=Theta,
                                    "Rate"=Rate,
                                    "Pi"=Pi_Data),
                       "All_Data"=All_Data)
  
  return(list(Simulated_Data,Y_Data,X,Pi_Data))
}

# Set parameters
N <- c(100,200,1000,5000,10000)

Simulated_Data<-list()
Testing<-NULL
Testing_2<-NULL
Plot_Collection_1<-list()
Plot_Collection_2<-list()
All_Plots_1<-list()
All_Plots_2<-list()

Rate<-sqrt(3)
Theta<-as.matrix(cbind(-2,-1.3,1.9,seq(0.1,0.9,0.2)))

for (j in 1:nrow(Theta))
{
  Temp_Theta<-Theta[j,]
  
  for (i in 1:length(N))
  {
    Temp_N<-N[i]
    All_Data<-Generate_Data(Rate=Rate,Theta = Temp_Theta,N=Temp_N,All_Models =All_Models)
    
    Results_Temp<-list()
    Results_Tempsy<-list()
    
    for (k in 1:length(All_Models)) 
    {
      glm(Y~.-1,data = as.data.frame(All_Data[[1]]$All_Data[[k]]),
          family = binomial("logit"))->Results_Temp[[k]]
      
      summary(Results_Temp[[k]])->Results_Tempsy[[k]]
      print(names(All_Models[k]))
      print(Results_Tempsy[[k]]$coefficients[,4])
    }
    
    Plotting_Data<-cbind.data.frame(Y_Data=All_Data[[2]],
                                    X1=All_Data[[3]][,1],
                                    X2=All_Data[[3]][,2],
                                    Actual=log(All_Data[[4]]/(1-All_Data[[4]])),
                                    Real_E=log(Results_Temp[[1]]$fitted.values/(1-Results_Temp[[1]]$fitted.values)),
                                    Assumed_1_E=log(Results_Temp[[2]]$fitted.values/(1-Results_Temp[[2]]$fitted.values)), 
                                    Assumed_2_E=log(Results_Temp[[3]]$fitted.values/(1-Results_Temp[[3]]$fitted.values)), 
                                    Assumed_3_E=log(Results_Temp[[4]]$fitted.values/(1-Results_Temp[[4]]$fitted.values)))
    
    if(N[length(N)]==Temp_N)
    {
      sum(round(Plotting_Data$Actual,4)==round(Plotting_Data$Real_E,4))+
        sum(round(Plotting_Data$Actual,4)==round(Plotting_Data$Assumed_1_E,4))+
        sum(round(Plotting_Data$Real_E,4)==round(Plotting_Data$Assumed_1_E,4))+ 
        sum(round(Plotting_Data$Actual,4)==round(Plotting_Data$Assumed_2_E,4))+
        sum(round(Plotting_Data$Real_E,4)==round(Plotting_Data$Assumed_2_E,4))+
        sum(round(Plotting_Data$Actual,4)==round(Plotting_Data$Assumed_3_E,4))+
        sum(round(Plotting_Data$Real_E,4)==round(Plotting_Data$Assumed_3_E,4))+
        sum(round(Plotting_Data$Assumed_1_E,4)==round(Plotting_Data$Assumed_2_E,4))+
        sum(round(Plotting_Data$Assumed_1_E,4)==round(Plotting_Data$Assumed_3_E,4))+
        sum(round(Plotting_Data$Assumed_2_E,4)==round(Plotting_Data$Assumed_3_E,4))->Testing[j]
      
      sum(abs(round(Plotting_Data$Actual,4)-round(Plotting_Data$Real_E,4)))+
        sum(abs(round(Plotting_Data$Actual,4)-round(Plotting_Data$Assumed_1_E,4)))+
        sum(abs(round(Plotting_Data$Real_E,4)-round(Plotting_Data$Assumed_1_E,4)))+ 
        sum(abs(round(Plotting_Data$Actual,4)-round(Plotting_Data$Assumed_2_E,4)))+
        sum(abs(round(Plotting_Data$Real_E,4)-round(Plotting_Data$Assumed_2_E,4)))+
        sum(abs(round(Plotting_Data$Actual,4)-round(Plotting_Data$Assumed_3_E,4)))+
        sum(abs(round(Plotting_Data$Real_E,4)-round(Plotting_Data$Assumed_3_E,4)))+
        sum(abs(round(Plotting_Data$Assumed_1_E,4)-round(Plotting_Data$Assumed_2_E,4)))+
        sum(abs(round(Plotting_Data$Assumed_1_E,4)-round(Plotting_Data$Assumed_3_E,4)))+
        sum(abs(round(Plotting_Data$Assumed_2_E,4)-round(Plotting_Data$Assumed_3_E,4)))->Testing_2[j]
      
      Simulated_Data[[j]] <- All_Data[[1]]
    }
    
    Plotting_Data %>%
      pivot_longer(cols = Actual:Assumed_3_E,names_to = "Model",values_to = "Values") %>%
      ggplot(.,aes(x=X1,y=Values,color=factor(Model)))+
      geom_point(alpha=0.5)+
      #geom_line(alpha=0.5)+
      xlab("X1")+ylab("log(p/(1-p))")+theme_bw()+
      scale_color_viridis_d(guide = guide_legend(override.aes = list(alpha = 1) ) )+
      ggtitle("Comparing Real vs Assumed models",
              subtitle = paste("N =",Temp_N,
                               ", \U03F4_0 =",Theta[j,1],",\U03F4_1 =",Theta[j,2],
                               ", \U03F4_2 =",Theta[j,3],",\U03F4_3 =",Theta[j,4]))+
      #geom_vline(xintercept=Opt_1,color="Blue",size=1.5)+
      #geom_vline(xintercept=Opt_2,color="Red",size=1.5)+
      theme(plot.title = element_text(size = 8),
            plot.subtitle = element_text(size = 7),
            legend.position = "none") ->Plot_Collection_1[[i]]
    
    Plotting_Data %>%
      pivot_longer(cols = Actual:Assumed_3_E,names_to = "Model",values_to = "Values") %>%
      ggplot(.,aes(x=X2,y=Values,color=factor(Model)))+
      geom_point(alpha=0.5)+
      #geom_line(alpha=0.5)+
      xlab("X2")+ylab("log(p/(1-p))")+theme_bw()+
      scale_color_viridis_d(guide = guide_legend(override.aes = list(alpha = 1) ) )+
      ggtitle("Comparing Real vs Assumed models",
              subtitle = paste("N =",Temp_N,
                               ", \U03F4_0 =",Theta[j,1],",\U03F4_1 =",Theta[j,2],
                               ", \U03F4_2 =",Theta[j,3],",\U03F4_3 =",Theta[j,4]))+
      #geom_vline(xintercept=Opt_1,color="Blue",size=1.5)+
      #geom_vline(xintercept=Opt_2,color="Red",size=1.5)+
      theme(plot.title = element_text(size = 8),
            plot.subtitle = element_text(size = 7),
            legend.position = "none") ->Plot_Collection_2[[i]]
  }
  All_Plots_1[[j]]<-Plot_Collection_1
  All_Plots_2[[j]]<-Plot_Collection_2
}

ggarrange(ggarrange(plotlist =All_Plots_1[[1]],nrow = 1),
          ggarrange(plotlist =All_Plots_1[[2]],nrow = 1),
          ggarrange(plotlist =All_Plots_1[[3]],nrow = 1),
          ggarrange(plotlist =All_Plots_1[[4]],nrow = 1),
          ggarrange(plotlist =All_Plots_1[[5]],nrow = 1,
                    common.legend = TRUE,legend = "bottom"),
          nrow = nrow(Theta),ncol = 1) %>%
  ggexport(filename = "Model_3_X1.jpeg",nrow = nrow(Theta),ncol = length(N),
           width = 1024,height = 1024)

ggarrange(ggarrange(plotlist =All_Plots_2[[1]],nrow = 1),
          ggarrange(plotlist =All_Plots_2[[2]],nrow = 1),
          ggarrange(plotlist =All_Plots_2[[3]],nrow = 1),
          ggarrange(plotlist =All_Plots_2[[4]],nrow = 1),
          ggarrange(plotlist =All_Plots_2[[5]],nrow = 1,
                    common.legend=TRUE,legend = "bottom"),
          nrow = nrow(Theta),ncol = 1) %>%
  ggexport(filename = "Model_3_X2.jpeg",nrow = nrow(Theta),ncol = length(N),
           width = 1024,height = 1024)

cbind(Testing,Testing_2)

Model_Path<-"Model_3"; Subsample_Size<-1500; r0<-Nc_size<-100; Replicates <- 1000
Simulated_Data<-Simulated_Data[[5]]

save(list = c("Simulated_Data","Subsample_Size","Replicates","Model_Path","Nc_size","r0"),
     file=here("Identical_r0","Generate_Big_Data",Model_Path,"No_Correlated_Covariate.RData"))

save(list = c("Simulated_Data","Subsample_Size","Replicates","Model_Path","Nc_size","r0"),
     file=here("Non_Identical_r0","Generate_Big_Data",Model_Path,"No_Correlated_Covariate.RData"))

rm(list = ls())

# Model 4 ----

# Real Model :$$ log\bigg(\frac{p}{1-p}\bigg) = \beta_0 + \beta_1 X_1 + \beta_2 X_2 + \beta_3 X_1^2 + \beta_4 X_2^2$$
# Assumed Model 1:$$ log\bigg(\frac{p}{1-p}\bigg) = \beta_0 + \beta_1 X_1 + \beta_2 X_2$$
# Assumed Model 2:$$ log\bigg(\frac{p}{1-p}\bigg) = \beta_0 + \beta_1 X_1 + \beta_2 X_2 + \beta_3 X_1^2$$
# Assumed Model 3:$$ log\bigg(\frac{p}{1-p}\bigg) = \beta_0 + \beta_1 X_1 + \beta_2 X_2 + \beta_3 X_2^2$$

A_optimality<-function(x,Theta,Model)
{
  x1<-x[1]; x2<-x[2]
  if(Model=="Real_Model") {X<-cbind(1,x1,x2,x1^2,x2^2)}
  
  if(Model=="Assumed_Model_1") {X<-cbind(1,x1,x2)}
  
  if(Model=="Assumed_Model_2") {X<-cbind(1,x1,x2,x1^2)}
  
  if(Model=="Assumed_Model_3") {X<-cbind(1,x1,x2,x2^2)}
  
  lp <- X%*%Theta
  p <- as.vector(exp(lp)/(1+exp(lp)))
  W <- diag(p*(1-p),1)
  sum(diag(solve(t(X)%*%W%*%X)))
}

All_Models<-list(Real_Model=c("X0","X1","X2","X1^2","X2^2"),
                 Assumed_Model_1=c("X0","X1","X2"),
                 Assumed_Model_2=c("X0","X1","X2","X1^2"),
                 Assumed_Model_3=c("X0","X1","X2","X2^2"))

Generate_Data<-function(Rate,Theta,N,All_Models)
{
  X<-replicate(2,rexp(n = N, rate = Rate))
  Complete_Data<-cbind(1,X,X^2)
  colnames(Complete_Data)<-c(paste0("X",0:ncol(X)),
                             paste0("X",1:ncol(X),"^2"))
  
  Pi_Data <- invlogit(Complete_Data[,colnames(Complete_Data) %in% All_Models$Real_Model]%*%Theta)
  Y_Data <- rbern(N,Pi_Data)
  
  All_Data<-list()
  for (i in 1:length(All_Models)) 
  {
    All_Data[[i]]<-cbind(Y=Y_Data,Complete_Data[,colnames(Complete_Data) %in% All_Models[[i]] ])
  }
  
  names(All_Data)<-names(All_Models)
  
  Simulated_Data<-list("Basic"=list("N"=N,
                                    "Theta"=Theta,
                                    "Rate"=Rate,
                                    "Pi"=Pi_Data),
                       "All_Data"=All_Data)
  
  return(list(Simulated_Data,Y_Data,X,Pi_Data))
}

# Set parameters
N <- c(100,200,1000,5000,10000)

Simulated_Data<-list()
Testing<-NULL
Testing_2<-NULL
Plot_Collection_1<-list()
Plot_Collection_2<-list()
All_Plots_1<-list()
All_Plots_2<-list()

Rate<-sqrt(3)
Theta<-as.matrix(cbind(-2,1.9,1.9,0.9,seq(0.1,0.9,0.2)))

for (j in 1:nrow(Theta))
{
  Temp_Theta<-Theta[j,]
  
  for (i in 1:length(N))
  {
    Temp_N<-N[i]
    All_Data<-Generate_Data(Rate=Rate,Theta = Temp_Theta,N=Temp_N,All_Models =All_Models)
    
    Results_Temp<-list()
    Results_Tempsy<-list()
    
    for (k in 1:length(All_Models)) 
    {
      glm(Y~.-1,data = as.data.frame(All_Data[[1]]$All_Data[[k]]),
          family = binomial("logit"))->Results_Temp[[k]]
      
      summary(Results_Temp[[k]])->Results_Tempsy[[k]]
      print(names(All_Models[k]))
      print(Results_Tempsy[[k]]$coefficients[,4])
    }
    
    Plotting_Data<-cbind.data.frame(Y_Data=All_Data[[2]],
                                    X1=All_Data[[3]][,1],
                                    X2=All_Data[[3]][,2],
                                    Actual=log(All_Data[[4]]/(1-All_Data[[4]])),
                                    Real_E=log(Results_Temp[[1]]$fitted.values/(1-Results_Temp[[1]]$fitted.values)),
                                    Assumed_1_E=log(Results_Temp[[2]]$fitted.values/(1-Results_Temp[[2]]$fitted.values)), 
                                    Assumed_2_E=log(Results_Temp[[3]]$fitted.values/(1-Results_Temp[[3]]$fitted.values)), 
                                    Assumed_3_E=log(Results_Temp[[4]]$fitted.values/(1-Results_Temp[[4]]$fitted.values)))
    
    if(N[length(N)]==Temp_N)
    {
      sum(round(Plotting_Data$Actual,4)==round(Plotting_Data$Real_E,4))+
        sum(round(Plotting_Data$Actual,4)==round(Plotting_Data$Assumed_1_E,4))+
        sum(round(Plotting_Data$Real_E,4)==round(Plotting_Data$Assumed_1_E,4))+ 
        sum(round(Plotting_Data$Actual,4)==round(Plotting_Data$Assumed_2_E,4))+
        sum(round(Plotting_Data$Real_E,4)==round(Plotting_Data$Assumed_2_E,4))+
        sum(round(Plotting_Data$Actual,4)==round(Plotting_Data$Assumed_3_E,4))+
        sum(round(Plotting_Data$Real_E,4)==round(Plotting_Data$Assumed_3_E,4))+
        sum(round(Plotting_Data$Assumed_1_E,4)==round(Plotting_Data$Assumed_2_E,4))+
        sum(round(Plotting_Data$Assumed_1_E,4)==round(Plotting_Data$Assumed_3_E,4))+
        sum(round(Plotting_Data$Assumed_2_E,4)==round(Plotting_Data$Assumed_3_E,4))->Testing[j]
      
      sum(abs(round(Plotting_Data$Actual,4)-round(Plotting_Data$Real_E,4)))+
        sum(abs(round(Plotting_Data$Actual,4)-round(Plotting_Data$Assumed_1_E,4)))+
        sum(abs(round(Plotting_Data$Real_E,4)-round(Plotting_Data$Assumed_1_E,4)))+ 
        sum(abs(round(Plotting_Data$Actual,4)-round(Plotting_Data$Assumed_2_E,4)))+
        sum(abs(round(Plotting_Data$Real_E,4)-round(Plotting_Data$Assumed_2_E,4)))+
        sum(abs(round(Plotting_Data$Actual,4)-round(Plotting_Data$Assumed_3_E,4)))+
        sum(abs(round(Plotting_Data$Real_E,4)-round(Plotting_Data$Assumed_3_E,4)))+
        sum(abs(round(Plotting_Data$Assumed_1_E,4)-round(Plotting_Data$Assumed_2_E,4)))+
        sum(abs(round(Plotting_Data$Assumed_1_E,4)-round(Plotting_Data$Assumed_3_E,4)))+
        sum(abs(round(Plotting_Data$Assumed_2_E,4)-round(Plotting_Data$Assumed_3_E,4)))->Testing_2[j]
      
      Simulated_Data[[j]] <- All_Data[[1]]
    }
    
    Plotting_Data %>%
      pivot_longer(cols = Actual:Assumed_3_E,names_to = "Model",values_to = "Values") %>%
      ggplot(.,aes(x=X1,y=Values,color=factor(Model)))+
      geom_point(alpha=0.5)+
      #geom_line(alpha=0.5)+
      xlab("X1")+ylab("log(p/(1-p))")+theme_bw()+
      scale_color_viridis_d(guide = guide_legend(override.aes = list(alpha = 1) ) )+
      ggtitle("Comparing Real vs Assumed models",
              subtitle = paste("N =",Temp_N,
                               ", \U03F4_0 =",Theta[j,1],",\U03F4_1 =",Theta[j,2],
                               ", \U03F4_2 =",Theta[j,3],",\U03F4_3 =",Theta[j,4]))+
      #geom_vline(xintercept=Opt_1,color="Blue",size=1.5)+
      #geom_vline(xintercept=Opt_2,color="Red",size=1.5)+
      theme(plot.title = element_text(size = 8),
            plot.subtitle = element_text(size = 7),
            legend.position = "none") ->Plot_Collection_1[[i]]
    
    Plotting_Data %>%
      pivot_longer(cols = Actual:Assumed_3_E,names_to = "Model",values_to = "Values") %>%
      ggplot(.,aes(x=X2,y=Values,color=factor(Model)))+
      geom_point(alpha=0.5)+
      #geom_line(alpha=0.5)+
      xlab("X2")+ylab("log(p/(1-p))")+theme_bw()+
      scale_color_viridis_d(guide = guide_legend(override.aes = list(alpha = 1) ) )+
      ggtitle("Comparing Real vs Assumed models",
              subtitle = paste("N =",Temp_N,
                               ", \U03F4_0 =",Theta[j,1],",\U03F4_1 =",Theta[j,2],
                               ", \U03F4_2 =",Theta[j,3],",\U03F4_3 =",Theta[j,4]))+
      #geom_vline(xintercept=Opt_1,color="Blue",size=1.5)+
      #geom_vline(xintercept=Opt_2,color="Red",size=1.5)+
      theme(plot.title = element_text(size = 8),
            plot.subtitle = element_text(size = 7),
            legend.position = "none") ->Plot_Collection_2[[i]]
  }
  All_Plots_1[[j]]<-Plot_Collection_1
  All_Plots_2[[j]]<-Plot_Collection_2
}

ggarrange(ggarrange(plotlist =All_Plots_1[[1]],nrow = 1),
          ggarrange(plotlist =All_Plots_1[[2]],nrow = 1),
          ggarrange(plotlist =All_Plots_1[[3]],nrow = 1),
          ggarrange(plotlist =All_Plots_1[[4]],nrow = 1),
          ggarrange(plotlist =All_Plots_1[[5]],nrow = 1,
                    common.legend = TRUE,legend = "bottom"),
          nrow = nrow(Theta),ncol = 1) %>%
  ggexport(filename = "Model_4_X1.jpeg",nrow = nrow(Theta),ncol = length(N),
           width = 1024,height = 1024)

ggarrange(ggarrange(plotlist =All_Plots_2[[1]],nrow = 1),
          ggarrange(plotlist =All_Plots_2[[2]],nrow = 1),
          ggarrange(plotlist =All_Plots_2[[3]],nrow = 1),
          ggarrange(plotlist =All_Plots_2[[4]],nrow = 1),
          ggarrange(plotlist =All_Plots_2[[5]],nrow = 1,
                    common.legend=TRUE,legend = "bottom"),
          nrow = nrow(Theta),ncol = 1) %>%
  ggexport(filename = "Model_4_X2.jpeg",nrow = nrow(Theta),ncol = length(N),
           width = 1024,height = 1024)

cbind(Testing,Testing_2)

Model_Path<-"Model_4"; Subsample_Size<-1500; r0<-Nc_size<-100; Replicates <- 1000
Simulated_Data<-Simulated_Data[[4]]

save(list = c("Simulated_Data","Subsample_Size","Replicates","Model_Path","Nc_size","r0"),
     file=here("Identical_r0","Generate_Big_Data",Model_Path,"No_Correlated_Covariate.RData"))

save(list = c("Simulated_Data","Subsample_Size","Replicates","Model_Path","Nc_size","r0"),
     file=here("Non_Identical_r0","Generate_Big_Data",Model_Path,"No_Correlated_Covariate.RData"))

rm(list = ls())
