library(here)
library(ezknitr)
library(rmarkdown)

Model<-c("Model_1","Model_2",
         "Model_3","Model_4") ; j=1

for (j in 1:length(Model)) 
{
        # Random Sampling ----
        ezknit(file=here("Non_Identical_r0","Rmarkdown","Classical","Random_Sampling.Rmd"),
               out_dir=here("Non_Identical_r0","htmloutputs","Classical",Model[j],"Random_Sampling"),
               fig_dir = c("Plots"),
               params=list("Model_Path"=Model[j]),
               verbose = TRUE,keep_md = FALSE)
        #open_output_dir()

        # Rare Event Random Sampling ----
        ezknit(file=here("Non_Identical_r0","Rmarkdown","Classical","RE_Random_Sampling.Rmd"),
               out_dir=here("Non_Identical_r0","htmloutputs","Classical",Model[j],"RE_Random_Sampling"),
               fig_dir = c("Plots"),
               params=list("Model_Path"=Model[j]),
               verbose = TRUE,keep_md = FALSE)
        #open_output_dir()

        # OSMAC Method ----
        ezknit(file=here("Non_Identical_r0","Rmarkdown","Classical","OSMAC_Method.Rmd"),
               out_dir=here("Non_Identical_r0","htmloutputs","Classical",Model[j],"OSMAC"),
               fig_dir = c("Plots"),
               params=list("Model_Path"=Model[j]),
               verbose = TRUE,keep_md = FALSE)
        #open_output_dir()
        
        # OSMAC Model Free Method ----
        ezknit(file=here("Non_Identical_r0","Rmarkdown","Classical","OSMAC_Model_Free_Method.Rmd"),
               out_dir=here("Non_Identical_r0","htmloutputs","Classical",Model[j],"OSMAC_Model_Free"),
               fig_dir = c("Plots"),
               params=list("Model_Path"=Model[j]),
               verbose = TRUE,keep_md = FALSE)
        #open_output_dir()
}

