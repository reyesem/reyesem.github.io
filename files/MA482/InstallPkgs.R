################################################################################
# File: InstallPkgs
# Class: MA482/BE482 - Bioengineering Statistics
# Description: Automate the package installation process.
#
# Author: Eric Reyes
# Date: Spring 2016-2017
# Modified:
#
# Notes:

# Specify Library
.globallib <- paste(Sys.getenv("R_HOME"), "/library", sep="")

# Load Additional Packages First
install.packages("tidyverse", lib = .globallib, dependencies = TRUE)
install.packages("broom", lib = .globallib, dependencies = TRUE)
install.packages("splines", lib = .globallib, dependencies = TRUE)
install.packages("geepack", lib = .globallib, dependencies = TRUE)
install.packages("rms", lib = .globallib, dependencies = TRUE)
install.packages("lme4", lib = .globallib, dependencies = TRUE)
