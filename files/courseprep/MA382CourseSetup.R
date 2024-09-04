################################################################################
# Program: MA382CourseSetup
# Class: MA382 Introductory Statistics
# Description: Automate the directory set up package installation for the class.
#
# Author: Eric Reyes
# Date: Fall 2425
#
# Notes:

# ---- Package Installation ----
install.packages(
  "https://github.com/reyesem/reyesem.github.io/raw/master/files/courseprep/IntroAnalysis_0.1.0.tar.gz",
  repos = NULL,
  quiet = TRUE)

install.packages(
  'quantreg',
  quiet = TRUE
)

# ---- Create Directory ----
dir.create("Individual-Assignments")
dir.create("Group-Work")
dir.create("In-Class-Demos")

if (testr <- require("IntroAnalysis")) {
  message("Set-up was successful!")
} else {
  warning("Set-up unsuccessful; reach out to instructor.")
}
