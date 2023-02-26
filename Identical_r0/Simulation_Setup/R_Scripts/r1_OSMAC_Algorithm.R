# Cordeiro Bias Estimation ----
Cordeiro<-function(XData,With_bias)
{
  p <- as.vector(invlogit(XData%*%as.vector(With_bias)))
  W <- diag(p*(1-p))
  inverse_term <- solve(t(XData)%*%W%*%XData)
  
  Term1 <- inverse_term%*%t(XData)%*%W
  Term2 <- diag(diag(XData%*%(inverse_term)%*%t(XData))) %*% as.matrix(p-0.5)
  
  bias <- as.vector(Term1%*%Term2)
  return(bias)
} 

# Newton Rhapson Algorithm for MLE ----
getMLE <- function(x, y, w) {
  beta <- rep(0, ncol(x))
  loop  <- 1
  Loop  <- 200
  msg <- "NA"
  while (loop <= Loop) {
    pr <- c(1 - 1 / (1 + exp(x %*% beta)))
    H <- t(x) %*% (pr * (1 - pr) * w * x)
    S <- colSums((y - pr) * w * x)
    tryCatch(
      {shs <- NA
      shs <- solve(H, S) }, 
      error=function(e){
        cat("\n ERROR :", loop, conditionMessage(e), "\n")})
    if (is.na(shs[1])) {
      msg <- "Not converge"
      beta <- loop <- NA
      break
    }
    beta.new <- beta + shs
    tlr  <- sum((beta.new - beta)^2)
    beta  <- beta.new
    if(tlr < 0.000001) {
      msg <- "Successful convergence"
      break
    }
    if (loop == Loop)
      warning("Maximum iteration reached")
    loop  <- loop + 1
  }
  list(par=beta, message=msg, iter=loop)
}

# Two step OSMAC ----
AlgTwoStp <- function(r1=r1, r2=r2,Y,X,n,Real_Data,alpha,combs,All_Covariates,Theta){
    Y_Real<-Real_Data[,1] #  Real Data
    X_Real<-Real_Data[,-1] # Real Data
    
    n1 <- sum(Y)
    n0 <- n - n1
    PI.prop <- rep(1/(2*n0), n)
    PI.prop[Y==1] <- 1/(2*n1)
    
    # For the Real Model 
    beta.mVc_Real<-matrix(nrow = length(r1),ncol = ncol(X_Real) )
    Utility_mVc_Real<-matrix(nrow = length(r1),ncol = 3 )
    Bias_mVc_Real<-matrix(nrow = length(r1),ncol = ncol(X_Real) )
    
    beta.mMSE_Real<-matrix(nrow = length(r1),ncol = ncol(X_Real) )
    Utility_mMSE_Real<-matrix(nrow = length(r1),ncol = 3 )
    Bias_mMSE_Real<-matrix(nrow = length(r1),ncol = ncol(X_Real) )
    
    #Sample.mMSE_Real<-list()
    #Sample.mVc_Real<-list()
    
    beta.mVc_Old<-Utility_mVc_Old<-Bias_mVc_Old<-list()
    beta.mMSE_Old<-Utility_mMSE_Old<-Bias_mMSE_Old<-list()
    #Sample.mMSE_Assumed_Old<-list()
    #Sample.mVc_Assumed_Old<-list()
    
    # For the Assumed Model with Already Available Sub-sampling probabilities
    for (a in 1:length(combs)) 
    {
      beta.mVc_Old[[a]]<-matrix(nrow = length(r1),ncol = ncol(X_Real) )
      Utility_mVc_Old[[a]]<-matrix(nrow = length(r1),ncol = 3 )
      Bias_mVc_Old[[a]]<-matrix(nrow = length(r1),ncol = ncol(X_Real) )
      
      beta.mMSE_Old[[a]]<-matrix(nrow = length(r1),ncol = ncol(X_Real) )
      Utility_mMSE_Old[[a]]<-matrix(nrow = length(r1),ncol = 3 )
      Bias_mMSE_Old[[a]]<-matrix(nrow = length(r1),ncol = ncol(X_Real) )
      
      #Sample.mMSE_Assumed_Old[[a]]<-list()
      #Sample.mVc_Assumed_Old[[a]]<-list()
    }
    
    beta.mVc_New<-Utility_mVc_New<-Bias_mVc_New<-list()
    beta.mMSE_New<-Utility_mMSE_New<-Bias_mMSE_New<-list()
    
    #Sample.mMSE_Assumed_New<-list()
    #Sample.mVc_Assumed_New<-list()
    
    # For the Assumed Model with Already New Sub-sampling probabilities
    for (a in 1:length(combs)) 
    {
      beta.mVc_New[[a]]<-matrix(nrow = length(r1),ncol = ncol(X_Real) )
      Utility_mVc_New[[a]]<-matrix(nrow = length(r1),ncol = 3 )
      Bias_mVc_New[[a]]<-matrix(nrow = length(r1),ncol = ncol(X_Real) )
      
      beta.mMSE_New[[a]]<-matrix(nrow = length(r1),ncol = ncol(X_Real) )
      Utility_mMSE_New[[a]]<-matrix(nrow = length(r1),ncol = 3 )
      Bias_mMSE_New[[a]]<-matrix(nrow = length(r1),ncol = ncol(X_Real) )
      
      #Sample.mMSE_Assumed_New[[a]]<-list()
      #Sample.mVc_Assumed_New[[a]]<-list()
    }
    
    # For the Real Model with joined Sub-sampling probabilities
    beta.mVc_join<-matrix(nrow = length(r1),ncol = ncol(X_Real) )
    Utility_mVc_join<-matrix(nrow = length(r1),ncol = 3 )
    Bias_mVc_join<-matrix(nrow = length(r1),ncol = ncol(X_Real) )
    
    beta.mMSE_join<-matrix(nrow = length(r1),ncol = ncol(X_Real) )
    Utility_mMSE_join<-matrix(nrow = length(r1),ncol = 3 )
    Bias_mMSE_join<-matrix(nrow = length(r1),ncol = ncol(X_Real) )
    
    #Sample.mMSE_join<-list()
    #Sample.mVc_join<-list()
    
    for (i in 1:length(r1)) 
    {
      idx.prop <- sample(1:n, r1[i], T, PI.prop)
      
      x.prop<-lapply(1:length(combs),function(j){
        X[idx.prop,All_Covariates %in% combs[[j]]] # Assumed Data 
      })
      y.prop <- Y[idx.prop,]  # Assumed Data 
      
      x_Real.prop<-X_Real[idx.prop,] # Real Data
      y_Real.prop<-Y_Real[idx.prop] # Real Data
      
      pinv.prop <- n
      pinv.prop <- 1/PI.prop[idx.prop]
      
      fit.prop <- lapply(1:length(combs), function(j){
        getMLE(x=x.prop[[j]], y=y.prop, w=pinv.prop) # Assumed Data
      })
      fit_Real.prop <- getMLE(x=x_Real.prop, y=y_Real.prop, w=pinv.prop) # Real Data
      
      beta.prop<-list()
      for (j in 1:length(combs)) 
      {
        beta.prop[[j]] <- fit.prop[[j]]$par # Assumed Data
        if(anyNA(beta.prop[[j]]))
        {
          return(list(opt=NA, msg="first stage not converge"))
        }
      }
      beta_Real.prop <- fit_Real.prop$par # Real Data
      
      if(anyNA(beta_Real.prop[1]))
        return(list(opt=NA, msg="first stage not converge"))
      
      P.prop  <- lapply(1:length(combs),function(j){
        1 - 1 / (1 + exp(X[,All_Covariates %in% combs[[j]] ] %*% beta.prop[[j]])) # Assumed Data
      })
      P_Real.prop  <- 1 - 1 / (1 + exp(X_Real %*% beta_Real.prop)) # Real Data
      
      ## mVC
      PI_Real.mVc <- sqrt((Y - P_Real.prop)^2 * rowSums(X_Real^2)) # Real Data
      PI_Real.mVc <- PI_Real.mVc / sum(PI_Real.mVc) # Real Data
      
      PI_Assumed_Old.mVc <- lapply(1:length(combs), function(j){
        sqrt((Y - P.prop[[j]])^2 * rowSums(X[,All_Covariates %in% combs[[j]] ]^2)) # Assumed Data Old probabilities
      } )
        
      PI_Assumed_Old.mVc <- lapply(1:length(combs), function(j){
        PI_Assumed_Old.mVc[[j]] / sum(PI_Assumed_Old.mVc[[j]]) # Assumed Data Old probabilities
      })
        
      PI_join.mVc<-rowSums2(cbind(alpha[1]*PI_Real.mVc,
                                  do.call(cbind,PI_Assumed_Old.mVc)%*%diag(alpha[-1]) ) )
        
      
      idx_Real.mVc <- sample(1:n, r2-r1[i], T, PI_Real.mVc) # Real Data 
      idx_Assumed.mVc <- lapply(1:length(combs), function(j){
        sample(1:n, r2-r1[i], T, PI_Assumed_Old.mVc[[j]]) # Assumed Data Old probabilities
      })
      idx_join.mVc <- sample(1:n, r2-r1[i], T, PI_join.mVc) # Joined Real Data
      
      x_Real.mVc <- X_Real[c(idx_Real.mVc, idx.prop),] # Real Data
      y_Real.mVc <- Y_Real[c(idx_Real.mVc, idx.prop)] # Real Data
      
      x_Assumed.mVc <-lapply(1:length(combs),function(j){
        X_Real[c(idx_Assumed.mVc[[j]], idx.prop),] # Assumed Data
      }) 
        
      y_Assumed.mVc <- lapply(1:length(combs),function(j){
        Y_Real[c(idx_Assumed.mVc[[j]], idx.prop)] # Assumed Data
      })
        
      x_join.mVc <- X_Real[c(idx_join.mVc, idx.prop),] # Joined Data
      y_join.mVc <- Y_Real[c(idx_join.mVc, idx.prop)] # Joined Data
      
      fit_Real.mVc <- getMLE(x=x_Real.mVc, y=y_Real.mVc, 
                             w=c(1 / PI_Real.mVc[idx_Real.mVc], pinv.prop)) # Real Data
      
      fit_Assumed_Old.mVc <-lapply(1:length(combs), function(j){
        getMLE(x=x_Assumed.mVc[[j]], y=y_Assumed.mVc[[j]], 
               w=c(1 / PI_Assumed_Old.mVc[[j]][idx_Assumed.mVc[[j]]], pinv.prop)) # Assumed Data Old probabilities
      }) 
      
      PI_Assumed_New.mVc<-lapply(1:length(combs), function(j){
        sqrt((Y_Real[c(idx_Assumed.mVc[[j]])] - P_Real.prop[c(idx_Assumed.mVc[[j]])])^2 * 
               rowSums(X_Real[c(idx_Assumed.mVc[[j]]),]^2)) 
      }) 
      Full_PI_Assumed_New.mVc<-sqrt((Y_Real - P_Real.prop)^2 * rowSums(X_Real^2)) 
      PI_Assumed_New.mVc<-lapply(1:length(combs), function(j){
        PI_Assumed_New.mVc[[j]]/sum(Full_PI_Assumed_New.mVc) # Assumed Data New Probabilities
      })
        
      fit_Assumed_New.mVc <-lapply(1:length(combs), function(j){
        getMLE(x=x_Assumed.mVc[[j]], y=y_Assumed.mVc[[j]], 
               w=c(1 / PI_Assumed_New.mVc[[j]], pinv.prop)) # Assumed Data New probabilities
      }) 
      fit_join.mVc <- getMLE(x=x_join.mVc, y=y_join.mVc, 
                             w=c(1 / PI_join.mVc[idx_join.mVc], pinv.prop)) # Joined Data 
      
      # Sample.mVc_Real[[i]]<-cbind(r2[i],y_Real.mVc,x_Real.mVc,
      #                             c(PI_Real.mVc[idx_Real.mVc], 1 / pinv.prop)) # Real Data
      
      # for (j in 1:length(combs)) 
      # {
      #   Sample.mVc_Assumed_Old[[j]][[i]]<-cbind(r2[i],y_Assumed.mVc[[j]],x_Assumed.mVc[[j]],
      #                                           c(PI_Assumed_Old.mVc[[j]][idx_Assumed.mVc[[j]] ], 1 / pinv.prop)) 
      # Assumed Data Old probabilities
      #   
      #   Sample.mVc_Assumed_New[[j]][[i]]<-cbind(r2[i],y_Assumed.mVc[[j]],x_Assumed.mVc[[j]],
      #                                      c(PI_Assumed_New.mVc[[j]], 1 / pinv.prop)) # Assumed Data New probabilities
      # }
      # 
      # Sample.mVc_join[[i]]<-cbind(r2[i],y_join.mVc,x_join.mVc,
      #                             c(PI_join.mVc[idx_join.mVc], 1 / pinv.prop)) # Joined Data
      
      if(anyNA(fit_Real.mVc$par) || anyNA(fit_Assumed_Old.mVc$par) || anyNA(fit_Assumed_New.mVc$par) || 
         anyNA(fit_join.mVc$par))
      {
        stop("There are NA or NaN values")
      }
        
      beta.mVc_Real[i,] <- fit_Real.mVc$par # Real Data
      
      for (j in 1:length(combs)) 
      {
        beta.mVc_Old[[j]][i,] <- fit_Assumed_Old.mVc[[j]]$par # Assumed Data Old probabilities 
        beta.mVc_New[[j]][i,] <- fit_Assumed_New.mVc[[j]]$par # Assumed Data New probabilities 
      }
      
      beta.mVc_join[i,] <- fit_join.mVc$par # Joined Data
      
      # Real Data
      pi<- invlogit(x_Real.mVc %*% beta.mVc_Real[i,])
      W<-diag(as.vector(pi*(1-pi)*c(1 / PI_Real.mVc[idx_Real.mVc], pinv.prop)))
      Mx<-(t(x_Real.mVc) %*% W %*% x_Real.mVc)
      Mx<-solve(Mx)
      Middle<-diag(((as.vector(y_Real.mVc)-as.vector(pi))*
                      as.vector(c(1 / PI_Real.mVc[idx_Real.mVc], pinv.prop)))^2)
      V_Temp<-(t(x_Real.mVc)%*%Middle%*%x_Real.mVc)
      V_Final<-Mx %*% V_Temp %*% Mx
      
      pi_1<- invlogit(x_Real.mVc %*% Theta)
      W_1<-diag(as.vector(pi_1*(1-pi_1)))
      Mx_1<-(t(x_Real.mVc) %*% W_1 %*% x_Real.mVc)
      
      Utility_mVc_Real[i,]<-cbind(r1[i],tr(V_Final),det(Mx_1))
      Bias_mVc_Real[i,]<-Cordeiro(XData=x_Real.mVc,With_bias = beta.mVc_Real[i,])
      
      # Assumed Data Old probabilities
      pi<- lapply(1:length(combs), function(j){
        invlogit(x_Assumed.mVc[[j]] %*% beta.mVc_Old[[j]][i,])
      })
        
      W<-lapply(1:length(combs),function(j){
        diag(as.vector(pi[[j]]*(1-pi[[j]])*c(1 / PI_Assumed_Old.mVc[[j]][idx_Assumed.mVc[[j]]], pinv.prop)))
      })
        
      Mx<-lapply(1:length(combs),function(j){
        solve((t(x_Assumed.mVc[[j]]) %*% W[[j]] %*% x_Assumed.mVc[[j]]))
      }) 
      Middle<-lapply(1:length(combs),function(j){
        diag(((as.vector(y_Assumed.mVc[[j]])-as.vector(pi[[j]]))*
                as.vector(c(1 / PI_Assumed_Old.mVc[[j]][idx_Assumed.mVc[[j]]], pinv.prop)))^2)
      }) 
      V_Temp<-lapply(1:length(combs), function(j){
        (t(x_Assumed.mVc[[j]])%*%Middle[[j]]%*%x_Assumed.mVc[[j]])
      })
        
      V_Final<-lapply(1:length(combs),function(j){
        Mx[[j]] %*% V_Temp[[j]] %*% Mx[[j]]
      }) 
      
      pi_1<- lapply(1:length(combs), function(j){
        invlogit(x_Assumed.mVc[[j]] %*% Theta)
      })
      
      W_1<-lapply(1:length(combs),function(j){
        diag(as.vector(pi_1[[j]]*(1-pi_1[[j]])))
      })
      
      Mx_1<-lapply(1:length(combs),function(j){
        (t(x_Assumed.mVc[[j]]) %*% W_1[[j]] %*% x_Assumed.mVc[[j]])
      })
      
      for (j in 1:length(combs)) 
      {
        Utility_mVc_Old[[j]][i,]<-cbind(r1[i],tr(V_Final[[j]]),det(Mx_1[[j]]))
        Bias_mVc_Old[[j]][i,]<-Cordeiro(XData=x_Assumed.mVc[[j]],With_bias = beta.mVc_Old[[j]][i,])    
      }
      
      # Assumed Data New probabilities
      pi<- lapply(1:length(combs),function(j){
        invlogit(x_Assumed.mVc[[j]] %*% beta.mVc_New[[j]][i,])
      })
      W<-lapply(1:length(combs),function(j){
        diag(as.vector(pi[[j]]*(1-pi[[j]])*c(1 / PI_Assumed_New.mVc[[j]], pinv.prop)))
      }) 
      Mx<-lapply(1:length(combs),function(j){
        solve((t(x_Assumed.mVc[[j]]) %*% W[[j]] %*% x_Assumed.mVc[[j]]))
      })
      Middle<-lapply(1:length(combs),function(j){
        diag(((as.vector(y_Assumed.mVc[[j]])-as.vector(pi[[j]]))*
                as.vector(c(1 / PI_Assumed_New.mVc[[j]], pinv.prop)))^2)
      })
      V_Temp<-lapply(1:length(combs),function(j){
        (t(x_Assumed.mVc[[j]])%*%Middle[[j]]%*%x_Assumed.mVc[[j]])
      })
      V_Final<-lapply(1:length(combs),function(j){
        Mx[[j]] %*% V_Temp[[j]] %*% Mx[[j]]
        }) 
      
      pi_1<- lapply(1:length(combs),function(j){
        invlogit(x_Assumed.mVc[[j]] %*% Theta)
      })
      W_1<-lapply(1:length(combs),function(j){
        diag(as.vector(pi_1[[j]]*(1-pi_1[[j]])))
      }) 
      Mx_1<-lapply(1:length(combs),function(j){
        (t(x_Assumed.mVc[[j]]) %*% W_1[[j]] %*% x_Assumed.mVc[[j]])
      })
      
      for (j in 1:length(combs)) 
      {
        Utility_mVc_New[[j]][i,]<-cbind(r1[i],tr(V_Final[[j]]),det(Mx_1[[j]]))
        Bias_mVc_New[[j]][i,]<-Cordeiro(XData=x_Assumed.mVc[[j]],With_bias = beta.mVc_New[[j]][i,])    
      }
      
      # Assumed Data Joined Probabilities
      pi<- invlogit(x_join.mVc %*% beta.mVc_join[i,])
      W<-diag(as.vector(pi*(1-pi)*c(1 / PI_join.mVc[idx_join.mVc], pinv.prop)))
      Mx<-(t(x_join.mVc) %*% W %*% x_join.mVc)
      Mx<-solve(Mx)
      Middle<-diag(((as.vector(y_join.mVc)-as.vector(pi))*
                      as.vector(c(1 / PI_join.mVc[idx_join.mVc], pinv.prop)))^2)
      V_Temp<-(t(x_join.mVc)%*%Middle%*%x_join.mVc)
      V_Final<-Mx %*% V_Temp %*% Mx
      
      pi_1<- invlogit(x_join.mVc %*% Theta)
      W_1<-diag(as.vector(pi_1*(1-pi_1)))
      Mx_1<-(t(x_join.mVc) %*% W_1 %*% x_join.mVc)
      
      Utility_mVc_join[i,]<-cbind(r1[i],tr(V_Final),det(Mx_1))
      Bias_mVc_join[i,]<-Cordeiro(XData=x_join.mVc,With_bias = beta.mVc_join[i,])    
      
      ## mMSE
      p_Real.prop <- P_Real.prop[idx.prop] # Real data
      w_Real.prop <- p_Real.prop * (1 - p_Real.prop) # Real data
      W_Real.prop <- solve(t(x_Real.prop) %*% (x_Real.prop * w_Real.prop * pinv.prop)) # Real data
      
      p_Assumed.prop <- lapply(1:length(combs),function(j){
        P.prop[[j]][idx.prop] # Assumed data
      })
      w_Assumed.prop <- lapply(1:length(combs),function(j){
        p_Assumed.prop[[j]] * (1 - p_Assumed.prop[[j]]) # Assumed data
      })
      W_Assumed.prop <- lapply(1:length(combs),function(j){
        solve(t(x.prop[[j]]) %*% (x.prop[[j]] * w_Assumed.prop[[j]] * pinv.prop)) # Assumed data
      })
      
      PI_Real.mMSE <- sqrt((Y_Real - P_Real.prop)^2 * rowSums((X_Real%*%W_Real.prop)^2)) # Real data
      PI_Real.mMSE <- PI_Real.mMSE / sum(PI_Real.mMSE) # Real data
      
      PI_Assumed_Old.mMSE <- lapply(1:length(combs),function(j){
        sqrt((Y - P.prop[[j]])^2 * rowSums((X[,All_Covariates %in% combs[[j]] ]%*%W_Assumed.prop[[j]])^2)) # Assumed data
      })
        
      PI_Assumed_Old.mMSE <- lapply(1:length(combs),function(j){
        PI_Assumed_Old.mMSE[[j]] / sum(PI_Assumed_Old.mMSE[[j]]) # Assumed data
      })
        
      PI_join.mMSE<-rowSums2(cbind(alpha[1]*PI_Real.mMSE,(do.call(cbind,PI_Assumed_Old.mMSE)%*%diag(alpha[-1]))))
      # Joined data
      
      idx_Real.mMSE <- sample(1:n, r2-r1[i], T, PI_Real.mMSE) # Real data
      idx_Assumed.mMSE <- lapply(1:length(combs),function(j){
        sample(1:n, r2-r1[i], T, PI_Assumed_Old.mMSE[[j]]) # Assumed data
      }) 
      idx_join.mMSE <- sample(1:n, r2-r1[i], T, PI_join.mMSE) # Joined Data
      
      x_Real.mMSE <- X_Real[c(idx_Real.mMSE, idx.prop),] # Real Data
      y_Real.mMSE <- Y_Real[c(idx_Real.mMSE, idx.prop)] # Real Data
      
      x_Assumed.mMSE <- lapply(1:length(combs),function(j){
        X_Real[c(idx_Assumed.mMSE[[j]], idx.prop),] # Assumed Data
      })
      y_Assumed.mMSE <- lapply(1:length(combs),function(j){
        Y_Real[c(idx_Assumed.mMSE[[j]], idx.prop)] # Assumed Data
      })
      
      x_join.mMSE <- X_Real[c(idx_join.mMSE, idx.prop),] # Joined Data
      y_join.mMSE <- Y_Real[c(idx_join.mMSE, idx.prop)] # Joined Data
      
      fit_Real.mMSE <- getMLE(x=x_Real.mMSE, y=y_Real.mMSE, 
                             w=c(1 / PI_Real.mMSE[idx_Real.mMSE], pinv.prop)) # Real Data
      fit_Assumed_Old.mMSE <- lapply(1:length(combs),function(j){
        getMLE(x=x_Assumed.mMSE[[j]], y=y_Assumed.mMSE[[j]], 
               w=c(1 / PI_Assumed_Old.mMSE[[j]][idx_Assumed.mMSE[[j]]], pinv.prop)) # Assumed Data Old probabilities
      })
        
      PI_Assumed_New.mMSE<-lapply(1:length(combs),function(j){
        sqrt((Y_Real[c(idx_Assumed.mMSE[[j]])] - P_Real.prop[c(idx_Assumed.mMSE[[j]])])^2 * 
               rowSums((X_Real[c(idx_Assumed.mMSE[[j]]),]%*%W_Real.prop)^2)) 
      }) 
        
      Full_PI_Assumed_New.mMSE<-sqrt((Y_Real - P_Real.prop)^2 * rowSums((X_Real%*%W_Real.prop)^2)) 
      PI_Assumed_New.mMSE<-lapply(1:length(combs), function(j){
        PI_Assumed_New.mMSE[[j]]/sum(Full_PI_Assumed_New.mMSE) # Assumed Data New Probabilities
      })
        
      fit_Assumed_New.mMSE <- lapply(1:length(combs),function(j){
        getMLE(x=x_Assumed.mMSE[[j]], y=y_Assumed.mMSE[[j]], 
               w=c(1 /PI_Assumed_New.mMSE[[j]], pinv.prop)) # Assumed Data New probabilities
      })
        
      fit_join.mMSE <- getMLE(x=x_join.mMSE, y=y_join.mMSE, 
                             w=c(1 / PI_join.mMSE[idx_join.mMSE], pinv.prop)) # Joined Data 
      
      # Sample.mMSE_Real[[i]]<-cbind(r2[i],y_Real.mMSE,x_Real.mMSE,
      #                             c(PI_Real.mMSE[idx_Real.mMSE], 1 / pinv.prop)) # Real Data
      
      # for (j in 1:length(combs))
      # {
      #   Sample.mMSE_Assumed_Old[[j]][[i]]<-cbind(r2[i],y_Assumed.mMSE[[j]],x_Assumed.mMSE[[j]],
      #                                            c(PI_Assumed_Old.mMSE[[j]][idx_Assumed.mMSE[[j]]], 1 / pinv.prop)) 
      # Assumed Data Old probabilities
      #   
      #   Sample.mMSE_Assumed_New[[j]][[i]]<-cbind(r2[i],y_Assumed.mMSE[[j]],x_Assumed.mMSE[[j]],
      #                                            c(PI_Assumed_New.mMSE[[j]], 1 / pinv.prop)) 
      # Assumed Data New probabilities
      # }
      # 
      # Sample.mMSE_join[[i]]<-cbind(r2[i],y_join.mMSE,x_join.mMSE,
      #                             c(PI_join.mMSE[idx_join.mMSE], 1 / pinv.prop)) # Joined Data
      
      if(anyNA(fit_Real.mMSE$par) || anyNA(fit_Assumed_Old.mMSE$par) || anyNA(fit_Assumed_New.mMSE$par) || 
         anyNA(fit_join.mMSE$par))
      {
        stop("There are NA or NaN values")
      }
      
      beta.mMSE_Real[i,] <- fit_Real.mMSE$par # Real Data
      for (j in 1:length(combs)) 
      {
        beta.mMSE_Old[[j]][i,] <- fit_Assumed_Old.mMSE[[j]]$par # Assumed Data Old probabilities 
        beta.mMSE_New[[j]][i,] <- fit_Assumed_New.mMSE[[j]]$par # Assumed Data New probabilities 
      }
      beta.mMSE_join[i,] <- fit_join.mMSE$par # Joined Data
      
      # Real Data
      pi<- invlogit(x_Real.mMSE %*% beta.mMSE_Real[i,])
      W<-diag(as.vector(pi*(1-pi)*c(1 / PI_Real.mMSE[idx_Real.mMSE], pinv.prop)))
      Mx<-(t(x_Real.mMSE) %*% W %*% x_Real.mMSE)
      Mx<-solve(Mx)
      Middle<-diag(((as.vector(y_Real.mMSE)-as.vector(pi))*
                      as.vector(c(1 / PI_Real.mMSE[idx_Real.mMSE], pinv.prop)))^2)
      V_Temp<-(t(x_Real.mMSE)%*%Middle%*%x_Real.mMSE)
      V_Final<-Mx %*% V_Temp %*% Mx
      
      pi_1<- invlogit(x_Real.mMSE %*% Theta)
      W_1<-diag(as.vector(pi_1*(1-pi_1)))
      Mx_1<-(t(x_Real.mMSE) %*% W_1 %*% x_Real.mMSE)
      
      Utility_mMSE_Real[i,]<-cbind(r1[i],tr(V_Final),det(Mx_1))
      Bias_mMSE_Real[i,]<-Cordeiro(XData=x_Real.mMSE,With_bias = beta.mMSE_Real[i,])
      
      # Assumed Data Old probabilities
      pi<-lapply(1:length(combs),function(j){
        invlogit(x_Assumed.mMSE[[j]] %*% beta.mMSE_Old[[j]][i,])
      }) 
      W<-lapply(1:length(combs),function(j){
        diag(as.vector(pi[[j]]*(1-pi[[j]])*c(1 / PI_Assumed_Old.mMSE[[j]][idx_Assumed.mMSE[[j]]], pinv.prop)))
      }) 
      Mx<-lapply(1:length(combs),function(j){
        solve((t(x_Assumed.mMSE[[j]]) %*% W[[j]] %*% x_Assumed.mMSE[[j]]))
      }) 
      Middle<-lapply(1:length(combs),function(j){
        diag(((as.vector(y_Assumed.mMSE[[j]])-as.vector(pi[[j]]))*
                as.vector(c(1 / PI_Assumed_Old.mMSE[[j]][idx_Assumed.mMSE[[j]]], pinv.prop)))^2)
      })
      V_Temp<-lapply(1:length(combs),function(j){
        (t(x_Assumed.mMSE[[j]])%*%Middle[[j]]%*%x_Assumed.mMSE[[j]])
        }) 
      V_Final<-lapply(1:length(combs),function(j){
        Mx[[j]] %*% V_Temp[[j]] %*% Mx[[j]]
      }) 
      
      pi_1<-lapply(1:length(combs),function(j){
        invlogit(x_Assumed.mMSE[[j]] %*% Theta)
      }) 
      W_1<-lapply(1:length(combs),function(j){
        diag(as.vector(pi_1[[j]]*(1-pi_1[[j]])))
      }) 
      Mx_1<-lapply(1:length(combs),function(j){
        (t(x_Assumed.mMSE[[j]]) %*% W_1[[j]] %*% x_Assumed.mMSE[[j]])
      })
      
      for (j in 1:length(combs)) 
      {
        Utility_mMSE_Old[[j]][i,]<-cbind(r1[i],tr(V_Final[[j]]),det(Mx_1[[j]]))
        Bias_mMSE_Old[[j]][i,]<-Cordeiro(XData=x_Assumed.mMSE[[j]],With_bias = beta.mMSE_Old[[j]][i,])    
      }
      
      # Assumed Data New probabilities
      pi<- lapply(1:length(combs),function(j){
        invlogit(x_Assumed.mMSE[[j]] %*% beta.mMSE_New[[j]][i,])
      }) 
      W<-lapply(1:length(combs),function(j){
        diag(as.vector(pi[[j]]*(1-pi[[j]])*c(1 / PI_Assumed_New.mMSE[[j]], pinv.prop)))
      }) 
      Mx<-lapply(1:length(combs),function(j){
        solve((t(x_Assumed.mMSE[[j]]) %*% W[[j]] %*% x_Assumed.mMSE[[j]]))
      })
      Middle<-lapply(1:length(combs),function(j){
        diag(((as.vector(y_Assumed.mMSE[[j]])-as.vector(pi[[j]]))*
                as.vector(c(1 / PI_Assumed_New.mMSE[[j]], pinv.prop)))^2)
      })
      V_Temp<-lapply(1:length(combs),function(j){
        (t(x_Assumed.mMSE[[j]])%*%Middle[[j]]%*%x_Assumed.mMSE[[j]])
      }) 
      V_Final<-lapply(1:length(combs),function(j){
        Mx[[j]] %*% V_Temp[[j]] %*% Mx[[j]]
      })
      
      pi_1<- lapply(1:length(combs),function(j){
        invlogit(x_Assumed.mMSE[[j]] %*% Theta)
      }) 
      W_1<-lapply(1:length(combs),function(j){
        diag(as.vector(pi_1[[j]]*(1-pi_1[[j]])))
      }) 
      Mx_1<-lapply(1:length(combs),function(j){
        (t(x_Assumed.mMSE[[j]]) %*% W_1[[j]] %*% x_Assumed.mMSE[[j]])
      })
      
      for(j in 1:length(combs))
      {
        Utility_mMSE_New[[j]][i,]<-cbind(r1[i],tr(V_Final[[j]]),det(Mx_1[[j]]))
        Bias_mMSE_New[[j]][i,]<-Cordeiro(XData=x_Assumed.mMSE[[j]],With_bias = beta.mMSE_New[[j]][i,])
      }
          
      # Assumed Data Joined Probabilities
      pi<- invlogit(x_join.mMSE %*% beta.mMSE_join[i,])
      W<-diag(as.vector(pi*(1-pi)*c(1 / PI_join.mMSE[idx_join.mMSE], pinv.prop)))
      Mx<-(t(x_join.mMSE) %*% W %*% x_join.mMSE)
      Mx<-solve(Mx)
      Middle<-diag(((as.vector(y_join.mMSE)-as.vector(pi))*
                      as.vector(c(1 / PI_join.mMSE[idx_join.mMSE], pinv.prop)))^2)
      V_Temp<-(t(x_join.mMSE)%*%Middle%*%x_join.mMSE)
      V_Final<-Mx %*% V_Temp %*% Mx
      
      pi_1<- invlogit(x_join.mMSE %*% Theta)
      W_1<-diag(as.vector(pi_1*(1-pi_1)))
      Mx_1<-(t(x_join.mMSE) %*% W_1 %*% x_join.mMSE)
      
      Utility_mMSE_join[i,]<-cbind(r1[i],tr(V_Final),det(Mx_1))
      Bias_mMSE_join[i,]<-Cordeiro(XData=x_join.mMSE,With_bias = beta.mMSE_join[i,])    
    }
    
    # Full_SP_Real<-cbind(Real_Data,PI_Real.mMSE,PI_Real.mVc)
    # Full_SP_Old<-cbind(Real_Data,do.call(cbind,PI_Assumed_Old.mMSE),do.call(cbind,PI_Assumed_Old.mVc))
    # Full_SP_New<-cbind(Real_Data,Full_PI_Assumed_New.mMSE/sum(Full_PI_Assumed_New.mMSE),
    #                    Full_PI_Assumed_New.mVc/sum(Full_PI_Assumed_New.mVc))
    # Full_SP_join<-cbind(Real_Data,PI_join.mMSE,PI_join.mVc)
    # 
    # Sample_mVc_Real<-do.call(rbind,Sample.mVc_Real)
    # for (j in 1:length(combs)) 
    # {
    #   assign(paste0("Sample_mVc_Old_",j),
    #          do.call(rbind,Sample.mVc_Assumed_Old[[j]]))
    #   assign(paste0("Sample_mVc_New_",j),
    #          do.call(rbind,Sample.mVc_Assumed_New[[j]]))
    # }
    # Sample_mVc_join<-do.call(rbind,Sample.mVc_join)
    # 
    # Sample_mMSE_Real<-do.call(rbind,Sample.mMSE_Real)
    # for (j in 1:length(combs)) 
    # {
    #   assign(paste0("Sample_mMSE_Old_",j),
    #          do.call(rbind,Sample.mMSE_Assumed_Old[[j]]))
    #   assign(paste0("Sample_mMSE_New_",j),
    #          do.call(rbind,Sample.mMSE_Assumed_New[[j]]))
    # }
    # Sample_mMSE_join<-do.call(rbind,Sample.mMSE_join)
    
    opt_Real<-list("Est_Theta_mMSE"=cbind(r1,beta.mMSE_Real),"Utility_mMSE"=Utility_mMSE_Real,
                   "Bias_mMSE"=cbind(r1,Bias_mMSE_Real),
                   "Est_Theta_mVc"=cbind(r1,beta.mVc_Real),"Utility_mVc"=Utility_mVc_Real,
                   "Bias_mVc"=cbind(r1,Bias_mVc_Real))
    
    for(j in 1:length(combs))
    {
      assign(paste0("opt_Old_",j),
             list("Est_Theta_mMSE"=cbind(r1,beta.mMSE_Old[[j]]),"Utility_mMSE"=Utility_mMSE_Old[[j]],
                  "Bias_mMSE"=cbind(r1,Bias_mMSE_Old[[j]]),
                  "Est_Theta_mVc"=cbind(r1,beta.mVc_Old[[j]]),"Utility_mVc"=Utility_mVc_Old[[j]],
                  "Bias_mVc"=cbind(r1,Bias_mVc_Old[[j]]))
             )
      assign(paste0("opt_New_",j),
             list("Est_Theta_mMSE"=cbind(r1,beta.mMSE_New[[j]]),"Utility_mMSE"=Utility_mMSE_New[[j]],
                  "Bias_mMSE"=cbind(r1,Bias_mMSE_New[[j]]),
                  "Est_Theta_mVc"=cbind(r1,beta.mVc_New[[j]]),"Utility_mVc"=Utility_mVc_New[[j]],
                  "Bias_mVc"=cbind(r1,Bias_mVc_New[[j]]))
             )
    }
    
    opt_join<-list("Est_Theta_mMSE"=cbind(r1,beta.mMSE_join),"Utility_mMSE"=Utility_mMSE_join,
                   "Bias_mMSE"=cbind(r1,Bias_mMSE_join),
                   "Est_Theta_mVc"=cbind(r1,beta.mVc_join),"Utility_mVc"=Utility_mVc_join,
                   "Bias_mVc"=cbind(r1,Bias_mVc_join))
    
    # msg <- c(fit.prop$message, fit_Real.prop$message, 
    #          fit_Real.mVc$message,fit_Assumed_Old.mVc$message,fit_Assumed_New.mVc$message,fit_join.mVc$message,
    #          fit_Real.mMSE$message,fit_Assumed_Old.mMSE$message,fit_Assumed_New.mMSE$message,fit_join.mMSE$message)
    msg <- NULL
    return(list(opt=list("opt_Real"=opt_Real,
                         "opt_Old"=mget(paste0("opt_Old_",1:length(combs))),
                         "opt_New"=mget(paste0("opt_New_",1:length(combs))),
                         "opt_join"=opt_join), msg=msg)) 
}
