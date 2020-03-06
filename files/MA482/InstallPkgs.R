################################################################################
# File: InstallPkgs
# Class: MA482/BE482 - Bioengineering Statistics
# Description: Automate the package installation process.
#
# Author: Eric Reyes
# Modified:
#
# Notes:


# ---- Load Additional Packages First ----
install.packages(c("car",
                   "skimr",
                   "splines",
                   "geepack",
                   "rms",
                   "lme4",
                   "tidyverse",
                   "broom",
                   "broom.mixed",
                   "knitr",
                   "rstudioapi"), 
                 repos = "https://cloud.r-project.org",
                 dependencies = TRUE,
                 quiet = TRUE)

# ---- Create Directory ----
ma482dir <- rstudioapi::selectDirectory(
  caption = "Select directory for course materials.")

download.file("https://github.com/reyesem/reyesem.github.io/raw/master/files/MA482/ReyesStyleDoc.docx",
              paste0(ma482dir, "/ReyesStyles.docx"))

message("Set-up was successful!")
