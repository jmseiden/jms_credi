remove.packages("credi")
.rs.restartR()

rm(list = ls())

# Acer Laptop
  #dropbox_wd = "C:/Users/marcu/Dropbox/"
# Gaby 
  dropbox_wd = "Z:/Dropbox/"
# Queen Mary's Revenge
  #dropbox_wd = "D:/Dropbox/"

setwd(paste(dropbox_wd, "/Marcus CREDI/MIRT Scoring", sep = ""))
library("roxygen2")
library("devtools")
library("desc")

#library("RcppArmadillo")

#RcppArmadillo.package.skeleton(name = "credi_cpp")


#devtools::create("credi", rstudio = TRUE)    #First time only


setwd("credi")
#### DESCRIPTION file ####
desc <- description$new()
desc$get_deps()
desc$set(Title = "CREDI Scoring",
         Description = "This function scores CREDI response data.",
         Author = "Marcus Waldman <marcus_waldman@gse.harvard.edu>",
         Maintainer = "Marcus Waldman <marcus_waldman@gse.harvard.edu>",
         Version = "1.0.5",
         License = "MIT")
desc$add_urls(urls = "https://sites.sph.harvard.edu/credi/")

# Imports:
desc$set_dep(package = "svDialogs", version = ">= 1.0.0")
desc$set_dep(package = "stats", version = ">= 3.5.0")

# Write out the DESCRIPTION folder
desc$normalize()
desc$write(file = "DESCRIPTION")


#Add descriptions

setwd("..")
setwd("source code")
load("preliminary.RData")
setwd("..")

setwd("credi")
usethis::use_data(B, invS, mest_df, beta, gamma, K, P, normcoef_mean, normcoef_sd,
         internal = TRUE, overwrite = TRUE, compress = "gzip")



#devtools::use_rcpp(pkg = "credi")

devtools::load_all(reset = TRUE)

setwd("..")
install("credi")
