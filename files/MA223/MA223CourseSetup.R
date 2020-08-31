################################################################################
# Program: MA223CourseSetup
# Class: MA223 Engineering Statistics I
# Description: Automate the directory set up package installation for the class.
#
# Author: Eric Reyes
# Date: Fall 2021
#
# Notes:

# ---- Package Installation ----
install.packages("googlesheets", quiet = TRUE)

install.packages("https://reyesem.github.io/files/MA223/IntroAnalysis_0.1.0.tar.gz", 
                 repos = NULL)


# ---- Create Directory ----
dir.create("Individual-Assignments")
dir.create("Group-Work")
dir.create("In-Class-Demos")

message("Set-up was successful!")
