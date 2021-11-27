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

if (!devtools::find_rtools()) installr::install.Rtools(check = FALSE)

# Add RTools to PATH
write('PATH="${RTOOLS40_HOME}\\usr\\bin;${PATH}"',
      file = "~/.Renviron", append = TRUE)


# ---- Load rstan ----
# Following steps on github.com/stan-dev/rstan/wiki/RStan-Getting-Started
# on loading rstan and configuring the C++ toolchain.
dotR <- file.path(Sys.getenv('HOME'), '.R')
if (!file.exists(dotR)) dir.create(dotR)

M <- file.path(dotR, 'Makevars.win')
if (!file.exists(M)) file.create(M)

cat('\n CXX14FLAGS += -mtune=native -O3 -mmmx -msse -msse2 -msse3 -mssse3 -msse4.1 -msse4.2',
    file = M,
    sep = '\n',
    append = FALSE)


install.packages(
  'rstan',
  repos = 'https://cloud.r-project.org/',
  dependencies = TRUE)



# ---- Load Bayesian Packages ----
# These packages are specific to the Bayesian framework.
install.packages(
  c('HDInterval',
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
