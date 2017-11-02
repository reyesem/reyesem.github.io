################################################################################
# File: InstallPkgs
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


# Check Installation of Rtools
cat("Did you include Rtools in PATH?", "\n",
    ifelse(grepl("Rtools", Sys.getenv("PATH")), "Yes", 
           "No...you need to reinstall Rtools and be sure to check the box during installation."),
    "\n\n")

system("g++ -v")
system("where make")


# Install Rstan
install.packages("rstan", lib = .globallib, dependencies = TRUE)

# Check Rstan Installation
fx <- inline::cxxfunction( signature(x = "integer", y = "numeric" ) , '
	return ScalarReal( INTEGER(x)[0] * REAL(y)[0] ) ;
                           ' )

cat("Did rstan install correctly?", "\n",
    ifelse(fx(2L, 5)==10, "Yes", 
           "No...you should see the instructor."),
    "\n\n")


# Additional Packages
install.packages("tidyverse", lib = .globallib, dependencies = TRUE)
