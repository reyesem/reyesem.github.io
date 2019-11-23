################################################################################
# File: MA483Install
# Class: MA483 - Bayesian Data Analysis
# Description: Automate the package installation process.
#
# Author: Eric Reyes
# Date: Winter 2019-2020
# Modified:
#
# Notes:

# ---- Load Additional Packages First ----
# These are packages that are general and not specific to the Bayesian
# framework.
install.packages(c("skimr",
                   "tidyverse",
                   "broom",
                   "knitr"), 
                 repos = "https://cloud.r-project.org",
                 dependencies = TRUE,
                 quiet = TRUE)


# ---- Load Development Packages ----
# These packages help with the installation and configuration of rstan.
# In particular, Rtools needs to be installed.
install.packages(c("devtools",
                   "installr"),
                 repos = "https://cloud.r-project.org",
                 dependencies = TRUE,
                 quiet = TRUE)

if (!devtools::find_rtools()) installr::install.Rtools(check = FALSE)


# ---- Load rstan ----
# Following steps on github.com/stan-dev/rstan/wiki/RStan-Getting-Started
# on loading rstan and configuring the C++ toolchain.
install.packages("rstan",
                 repos = "https://cloud.r-project.org",
                 dependencies = TRUE,
                 quiet = TRUE)

pkgbuild::has_build_tools(debug = TRUE)



# ---- Load Bayesian Packages ----
# These packages are specific to the Bayesian framework.
install.packages(c("HDInterval",
                   "bayesplot",
                   "rstanarm",
                   "bridgesampling",
                   "broom.mixed"),
                 repos = "https://cloud.r-project.org",
                 dependencies = TRUE,
                 quiet = TRUE)


# ---- Print Completion ----
cat("\n\n\nInstallation complete!\n")
