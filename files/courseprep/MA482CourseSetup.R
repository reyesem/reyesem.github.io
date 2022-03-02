################################################################################
# File: MA482CourseSetup
# Class: MA482 - Biostatistics
# Description: Automate the package installation process.
#
# Author: Eric Reyes
# Date: Spring 2021-2022
# Modified:
#
# Notes:


# ---- Update Old Packages ----
update.packages(ask = FALSE,
                checkBuilt = TRUE,
                quiet = TRUE)


# ---- Load Required Packages First ----
install.packages(
  c('tidyverse',
    'rlang',
    'rmarkdown',
    'knitr',
    'withr',
    'boot',
    'car',
    'rms',
    'skimr',
    'geepack',
    'lme4',
    'broom',
    'broom.mixed'),
  repos = 'https://cloud.r-project.org',
  dependencies = TRUE,
  quiet = TRUE
)


# ---- Install Additional Functionality ----
install.packages(
  "https://github.com/reyesem/reyesem.github.io/raw/master/files/courseprep/reyescourses_0.1.0.tar.gz",
  repos = NULL,
  quiet = TRUE)


if (testr <- require("reyescourses")) {
  message("Set-up was successful!")
} else {
  warning("Set-up unsuccessful; reach out to instructor.")
}
