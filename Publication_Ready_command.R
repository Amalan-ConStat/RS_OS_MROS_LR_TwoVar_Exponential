library(here)
library(ezknitr)
library(rmarkdown)

Model<-c("Model_1","Model_2",
         "Model_3","Model_4"); i=1

for(i in 1:length(Model))
{
  # Publication Ready ----
  ezknit(file=here("Publication_Ready.Rmd"),
         out_dir=here("Publication_Ready",Model[i]),
         fig_dir = c("Plots"),
         params=list("Model_Path"=Model[i]),
         verbose = TRUE,keep_md = TRUE,keep_html = FALSE)
  open_output_dir()
}
