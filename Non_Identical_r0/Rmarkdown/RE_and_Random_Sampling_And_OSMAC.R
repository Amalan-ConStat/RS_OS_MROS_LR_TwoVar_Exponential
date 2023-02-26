library(here)
library(rmarkdown)

Model<-c("Model_1","Model_2",
         "Model_3","Model_4") ; jj=1

for (jj in 1:length(Model)) 
{
  # Random Sampling ----
  render(input = here("Non_Identical_r0","Rmarkdown","Random_Sampling.Rmd"),
         output_format = "html_document",
         output_file = "Random_Sampling",
         output_dir = here("Non_Identical_r0","htmloutputs",Model[jj],"Random_Sampling"),
         params = list("Model_Path"=Model[jj]))

  # Rare Event Random Sampling ----
  render(input = here("Non_Identical_r0","Rmarkdown","RE_Random_Sampling.Rmd"),
         output_format = "html_document",
         output_file = "RE_Random_Sampling",
         output_dir = here("Non_Identical_r0","htmloutputs",Model[jj],"RE_Random_Sampling"),
         params = list("Model_Path"=Model[jj]))

  # OSMAC Method ----
  render(input = here("Non_Identical_r0","Rmarkdown","OSMAC_Method.Rmd"),
         output_format = "html_document",
         output_file = "OSMAC_Method",
         output_dir = here("Non_Identical_r0","htmloutputs",Model[jj],"OSMAC"),
         params = list("Model_Path"=Model[jj]))

  render(input = here("Non_Identical_r0","Rmarkdown","r1_OSMAC_Method.Rmd"),
         output_format = "html_document",
         output_file = "r1_OSMAC_Method",
         output_dir = here("Non_Identical_r0","htmloutputs",Model[jj],"OSMAC"),
         params = list("Model_Path"=Model[jj]))
  
  # OSMAC Model Free Method ----
  render(input = here("Non_Identical_r0","Rmarkdown","OSMAC_Model_Free_Method.Rmd"),
         output_format = "html_document",
         output_file = "OSMAC_Model_Free_Method",
         output_dir = here("Non_Identical_r0","htmloutputs",Model[jj],"OSMAC_Model_Free"),
         params = list("Model_Path"=Model[jj]))
  
  render(input = here("Non_Identical_r0","Rmarkdown","r1_OSMAC_Model_Free_Method.Rmd"),
         output_format = "html_document",
         output_file = "r1_OSMAC_Model_Free_Method",
         output_dir = here("Non_Identical_r0","htmloutputs",Model[jj],"OSMAC_Model_Free"),
         params = list("Model_Path"=Model[jj]))
}

