################################################################################
# File: MA386CourseSetup
# Course: MA386 Statistical Programming
# Description: Completes installation of necessary packages.
#
# Author: Eric Reyes
# Date: Fall 2020

# ---- Install CRAN Packages ----
options(install.packages.check.source = "no")

install.packages(c("tidyverse",
                   "rvest",
                   "jsonlite",
                   "ggmap",
                   "tidygeocoder",
                   "plotly",
                   "learnr",
                   "knitr"), 
                 dependencies = TRUE)



# ---- Install Guided Notes ----
install.packages("https://reyesem.github.io/files/MA386/reyes386_0.1.0.tar.gz",
                 repos = NULL)



message("\n\nSetup Complete!\n")