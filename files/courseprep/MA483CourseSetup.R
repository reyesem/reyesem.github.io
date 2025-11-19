################################################################################
# File: MA483CourseSetup
# Class: MA483 - Bayesian Data Analysis
# Description: Automate the package installation process.
#
# Author: Eric Reyes
# Date: Winter 2021-2022
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



# ---- Load Development Packages ----
# These packages help with the installation and configuration of rstan.
# In particular, Rtools needs to be installed.
install.packages(
  c('devtools',
    'installr'),
  repos = 'https://cloud.r-project.org',
  dependencies = TRUE,
  quiet = TRUE
)

if (!devtools::find_rtools()) {
  warning("RTools not installed. See Instructor for help.")
}


# ---- Load rstan ----
# rstan seems to be unstable with respect to how I have run things before. I 
# am going to try switching to cmdstanr

install.packages("rstan",
                 repos = c('https://stan-dev.r-universe.dev',
                           getOption("repos")),
                 dependencies = TRUE,
                 quiet = TRUE)

install.packages("cmdstanr",
                 repos = c('https://stan-dev.r-universe.dev',
                           getOption("repos")),
                 dependencies = TRUE,
                 quiet = TRUE)

if (!cmdstanr::check_cmdstan_toolchain()) {
  warning("Something went wrong with RTools. See Instructor for help.")
}

cmdstanr::install_cmdstan(cores = 2, quiet = TRUE)




# ---- Load Bayesian Packages ----
# These packages are specific to the Bayesian framework.
install.packages(
  c('posterior',
    'HDInterval',
    'bayesplot',
    'rstanarm',
    'bridgesampling',
    'broom.mixed',
    'tidybayes',
    'bayesrules',
    'modelr',
    'e1071'),
  repos = 'https://cloud.r-project.org',
  dependencies = TRUE,
  quiet = TRUE
)


message('Set-up Complete')