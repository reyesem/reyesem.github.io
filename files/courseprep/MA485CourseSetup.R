################################################################################
# File: MA485CourseSetup
# Class: MA485 - Applied Linear Regression
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


# ---- Load Required Packages First ----
# These are packages that are required for all courses.
install.packages(
  c('tidyverse',
    'rlang',
    'rmarkdown',
    'knitr',
    'withr'),
  repos = 'https://cloud.r-project.org',
  dependencies = TRUE,
  quiet = TRUE
)



# ---- Load Regression Packages ----
# These packages are specific to regression.
install.packages(
  c('broom',
    'glmnet',
    'boot',
    'car',
    'lmboot'),
  repos = 'https://cloud.r-project.org',
  dependencies = TRUE,
  quiet = TRUE
)


message('Set-up Complete')