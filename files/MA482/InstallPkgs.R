################################################################################
# File: InstallPkgs
# Class: MA482/BE482 - Bioengineering Statistics
# Description: Automate the package installation process.
#
# Author: Eric Reyes
# Date: Spring 2018-2019
# Modified:
#
# Notes:


# Load Additional Packages First
install.packages(c("car",
                   "skimr",
                   "splines",
                   "geepack",
                   "rms",
                   "lme4",
                   "tidyverse",
                   "broom",
                   "knitr"), dependencies = TRUE)

