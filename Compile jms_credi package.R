#remove.packages("credi")
remove.packages("jmscredi")
.rs.restartR()
devtools::install_github("jmseiden/jms_credi")
rm(list = ls())

gitkraken_wd = "G:/github-gitkraken"


setwd(paste0(gitkraken_wd,"/jms_credi"))
library("roxygen2")
library("devtools")
library("desc")

#library("RcppArmadillo")

#RcppArmadillo.package.skeleton(name = "credi_cpp")


#devtools::create("credi", rstudio = TRUE)    #First time only


#### DESCRIPTION file ####
desc <- description$new("!new")
desc$set(Package = "jmscredi",
         Title = "JMS-CREDI Scoring",
         Description = "Scoring functions for CREDI response data.",
         Maintainer = "Jonathan Seiden <jseiden@g.harvard.edu>",
         Version = "0.0.1",
         License = "MIT",
         BugReports = "Beta version.")
desc$add_urls(urls = "https://sites.sph.harvard.edu/credi/")
desc$add_author(given = "Jonathan",family = "Seiden",role = c("cre"),email = "jseiden@g.harvard.edu")
desc$add_author(given = "Marcus", family = "Waldman", role = c("aut"), email = "marcus.waldman@unmc.edu")

# Imports:
desc$set_dep(package = "tidyverse", version = ">= 1.3.0")
desc$set_dep(package = "svDialogs", version = ">= 1.0.0")
desc$set_dep(package = "stats", version = ">= 3.5.0")
desc$set_dep(package = "magrittr")
desc$set_dep(package = "readr")
desc$set_dep(package = "dplyr")

# Write out the DESCRIPTION folder
desc$normalize()
desc$write(file = "DESCRIPTION")


#Add descriptions
load(paste0(gitkraken_wd,"/jms_credi/R/sysdata.rda"))
setwd(paste0(gitkraken_wd,"/jms_credi"))
usethis::use_data(B, invS, mest_df, beta, gamma, K, P, normcoef_mean, normcoef_sd,
         internal = TRUE, overwrite = TRUE, compress = "gzip")



#devtools::use_rcpp(pkg = "credi")

devtools::load_all(reset = TRUE)

setwd("..")
install("jms_credi")
