library(here)
library(rmarkdown)

Model<-c("Model_1","Model_2",
         "Model_3","Model_4"); i=1

for(ii in 1:length(Model))
{
  # Publication Ready ----
  render(input = here("Publication_Ready.Rmd"),
         output_format = "github_document",
         output_file = "Publication_Ready",
         output_dir = here("Publication_Ready",Model[ii]),
         params=list("Model_Path"=Model[ii]))
  
  render(input = here("r1_Publication_Ready.Rmd"),
         output_format = "github_document",
         output_file = "r1_Publication_Ready",
         output_dir = here("Publication_Ready",Model[i]),
         params=list("Model_Path"=Model[ii]))
}
