#install.packages(c("devtools", "roxygen2", "testthat", "knitr"))
#install.packages(c("RSQLite","Rcpp"))
#install.packages(c("plyr","gdistance","data.table","latticeExtra","rgeos"))

install.packages("SpaDES.tools")
#devtools::install_github("PredictiveEcology/SpaDES.tools")
#usethis::create_package(getwd())
devtools::document();devtools::load_all()
#devtools::install_github("LandSciTech/roads",ref="dev")
citation("roads")

install.packages("fs")

devtools::document();devtools::load_all()
