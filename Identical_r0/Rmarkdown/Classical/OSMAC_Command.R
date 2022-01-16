library(here)
library(ezknitr)
library(rmarkdown)

Model<-c("Model_1","Model_2",
         "Model_3","Model_4") ; j=4

for (j in 1:length(Model)) 
{
  # OSMAC Method ----
  ezknit(file=here("Identical_r0","Rmarkdown","Classical","OSMAC_Method.Rmd"),
         out_dir=here("Identical_r0","htmloutputs","Classical",Model[j],"OSMAC"),
         fig_dir = c("Plots"),
         params=list("Model_Path"=Model[j]),
         verbose = TRUE,keep_md = FALSE)
  #open_output_dir()
}

