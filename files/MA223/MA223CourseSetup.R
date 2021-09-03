################################################################################
# Program: MA223CourseSetup
# Class: MA223 Engineering Statistics I
# Description: Automate the directory set up package installation for the class.
#
# Author: Eric Reyes
# Date: Winter AY2021
#
# Notes:

# ---- Package Installation ----
install.packages("googlesheets", quiet = TRUE)
install.packages("learnr", quiet = TRUE)
install.packages("https://reyesem.github.io/files/MA223/IntroAnalysis_0.1.0.tar.gz", 
                 repos = NULL, quiet = TRUE)


# ---- Create Directory ----
dir.create("Individual-Assignments")
dir.create("Group-Work")
dir.create("In-Class-Demos")

if (testr <- require("IntroAnalysis")) {
  message("Set-up was successful!")
} else {
  warning("Set-up unsuccessful; reach out to instructor.")
}
