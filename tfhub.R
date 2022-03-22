# install.packages("tfhub")
#devtools::install_github("rstudio/tfhub")
library(tfhub)

tfhub::install_tfhub()

module <- hub_load("https://tfhub.dev/google/LaBSE/2")
