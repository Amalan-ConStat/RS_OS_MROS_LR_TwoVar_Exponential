library(here)
library(rmarkdown)

Model<-c("Model_1","Model_2",
         "Model_3","Model_4"); i=1

# Best_Subsampling_Method ----
for (i in 1:length(Model))
{
  render(input=here("Identical_r0","Rmarkdown","Classical","Summary","Best_Subsampling_Method.Rmd"),
         output_format = "html_document",
         output_file = "Best_Subsampling_Method",
         output_dir=here("Identical_r0","Summary","Classical",Model[i],"Best_Subsampling"),
         params=list("Model_Path"=Model[i]))
  
  render(input=here("Identical_r0","Rmarkdown","Classical","Summary","r1_Best_Subsampling_Method.Rmd"),
         output_format = "html_document",
         output_file = "r1_Best_Subsampling_Method",
         output_dir=here("Identical_r0","Summary","Classical",Model[i],"Best_Subsampling"),
         params=list("Model_Path"=Model[i]))
}

