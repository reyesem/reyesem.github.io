################################################################################
# File: MA386CourseSetup
# Course: MA386 Statistical Programming
# Description: Completes installation of necessary packages.
#
# Author: Eric Reyes
# Date: Fall 2020

# ---- Install CRAN Packages ----
options(install.packages.check.source = "no")

update.packages(ask = FALSE, 
                checkBuilt = TRUE,
                quiet = TRUE)

install.packages(c("tidyverse",
                   "rvest",
                   "jsonlite",
                   "ggmap",
                   "tidygeocoder",
                   "plotly",
                   "learnr",
                   "knitr"), 
                 dependencies = TRUE,
                 quiet = TRUE)



# ---- Install Guided Notes ----
install.packages("https://reyesem.github.io/files/courseprep/reyescourses_0.1.0.tar.gz",
                 repos = NULL,
                 quiet = TRUE)



if (testr <- require("reyes386")) {
  message("Set-up was successful!")
} else {
  warning("Set-up unsuccessful; reach out to instructor.")
}