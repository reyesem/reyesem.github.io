################################################################################
# File: InstallRtools
# Class: MA482/BE482 - Bioengineering Statistics
# Description: Automate the package installation process.
#
# Author: Eric Reyes
# Date: Fall 2017-2018
# Modified:
#
# Notes:

# Specify Library
.globallib <- paste(Sys.getenv("R_HOME"), "/library", sep="")

# Load Rtools
#  Need to install 'installr' package.
#  Use install.Rtools() to perform the installation.
#  Check that installation successful.
install.packages("installr", lib = .globallib)

installr::install.Rtools(check = FALSE)
