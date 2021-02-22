################################################################################
# File: MA482CourseSetup
# Class: MA482/BE482 - BioStatistics
# Description: Automate the package installation process.
#
# Author: Eric Reyes
# Modified:
#
# Notes:


# ---- Load Additional Packages First ----
install.packages("devtools",
                 "curl",
                 dependencies = TRUE,
                 quiet = TRUE)


# ---- Install Biostat Functionality ----
devtools::install_github("reyesem/reyes482", 
                         dependencies = TRUE, 
                         quite = TRUE)

message("Set-up was successful!")
