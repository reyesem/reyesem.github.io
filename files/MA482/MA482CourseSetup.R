################################################################################
# File: MA482CourseSetup
# Class: MA482/BE482 - BioStatistics
# Description: Automate the package installation process.
#
# Author: Eric Reyes
# Modified:
#
# Notes:

# ---- Update Old Packages ----
update.packages(ask = FALSE, 
                checkBuilt = TRUE,
                quiet = TRUE)


# ---- Load Additional Packages First ----
# ---- Load Additional Packages First ----
install.packages(c("devtools",
                   "curl",
                   "tidyverse",
                   "broom",
                   "broom.mixed",
                   "car",
                   "geepack",
                   "import",
                   "knitr",
                   "lme4",
                   "rms",
                   "rmarkdown",
                   "skimr",
                   "splines"),
                 dependencies = TRUE,
                 quiet = TRUE)


# ---- Install Biostat Functionality ----
install.packages("https://github.com/reyesem/reyesem.github.io/raw/master/files/MA482/reyes482_0.1.0.tar.gz",
                 repos = NULL,
                 quiet = TRUE)


if (testr <- require("reyes482")) {
  message("Set-up was successful!")
} else {
  warning("Set-up unsuccessful; reach out to instructor.")
}