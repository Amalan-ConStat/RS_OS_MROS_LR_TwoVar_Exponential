library(here)
library(rmarkdown)

Model<-c("Model_1","Model_2",
         "Model_3","Model_4") ; j=1

for (j in 1:length(Model)) 
{
  # OSMAC Method ----
  render(input=here("Identical_r0","Rmarkdown","Classical","OSMAC_Method.Rmd"),
         output_format = "html_document",
         output_file = "OSMAC_Method",
         output_dir=here("Identical_r0","htmloutputs","Classical",Model[j],"OSMAC"),
         params=list("Model_Path"=Model[j]))
  
  render(input=here("Identical_r0","Rmarkdown","Classical","r1_OSMAC_Method.Rmd"),
         output_format = "html_document",
         output_file = "r1_OSMAC_Method",
         output_dir=here("Identical_r0","htmloutputs","Classical",Model[j],"OSMAC"),
         params=list("Model_Path"=Model[j]))
}

