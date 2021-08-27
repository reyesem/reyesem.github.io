################################################################################
# File: MA482CourseSetup
# Class: MA482/BE482 - BioStatistics
# Description: Automate the package installation process.
#
# Author: Eric Reyes
# Modified:
#
# Notes:

# ---- Update Old Packages ----
update.packages(ask = FALSE, 
                checkBuilt = TRUE,
                quiet = TRUE)


# ---- Load Additional Packages First ----
install.packages(c("devtools",
                   "curl",
                   "tidyverse",
                   "broom",
                   "gganimate",
                   "car",
                   "import",
                   "knitr",
                   "lmboot",
                   "plotly",
                   "rms",
                   "rmarkdown",
                   "shiny",
                   "skimr",
                   "splines",
                   "Stat2Data"),
                 dependencies = TRUE,
                 quiet = TRUE)


# ---- Install Biostat Functionality ----
install.packages("https://github.com/reyesem/reyesem.github.io/raw/master/files/MA382/reyes382_0.1.0.tar.gz",
                 repos = NULL,
                 quiet = TRUE)

message("Set-up was successful!")
