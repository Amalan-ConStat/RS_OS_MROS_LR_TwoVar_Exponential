library(here)
library(ezknitr)
library(rmarkdown)

Model<-c("Model_1","Model_2",
         "Model_3","Model_4"); i=1

# Best_Subsampling_Method ----
for (i in 1:length(Model))
{
ezknit(file=here("Identical_r0","Rmarkdown","Classical","Summary","Best_Subsampling_Method.Rmd"),
        out_dir=here("Identical_r0","Summary","Classical",Model[i],"Best_Subsampling"),
        fig_dir = c("Plots"),
        params=list("Model_Path"=Model[i]),
        verbose = TRUE,keep_md = FALSE)
#open_output_dir()
}

