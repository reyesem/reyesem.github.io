################################################################################
# File: InstallPkgs
# Class: MA482/BE482 - Bioengineering Statistics
# Description: Automate the package installation process.
#
# Author: Eric Reyes
# Modified:
#
# Notes:


# ---- Load Additional Packages First ----
install.packages(c("car",
                   "skimr",
                   "splines",
                   "geepack",
                   "rms",
                   "lme4",
                   "tidyverse",
                   "broom",
                   "broom.mixed",
                   "knitr"), 
                 repos = "https://cloud.r-project.org",
                 dependencies = TRUE,
                 quiet = TRUE)


message("Set-up was successful!")
