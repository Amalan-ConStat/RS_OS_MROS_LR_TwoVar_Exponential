library(here)
library(rmarkdown)

Model<-c("Model_1","Model_2",
         "Model_3","Model_4") ; jj=1

for (jj in 1:length(Model)) 
{
  # OSMAC Method ----
  render(input=here("Identical_r0","Rmarkdown","OSMAC_Method.Rmd"),
         output_format = "html_document",
         output_file = "OSMAC_Method",
         output_dir=here("Identical_r0","htmloutputs",Model[jj],"OSMAC"),
         params=list("Model_Path"=Model[jj]))
  
  render(input=here("Identical_r0","Rmarkdown","r1_OSMAC_Method.Rmd"),
         output_format = "html_document",
         output_file = "r1_OSMAC_Method",
         output_dir=here("Identical_r0","htmloutputs",Model[jj],"OSMAC"),
         params=list("Model_Path"=Model[jj]))
}

