################################################################################
# File: MA490CourseSetup
# Class: MA490 - Social Justice and Statistics
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
install.packages(c("tidyverse",
                   "broom",
                   "knitr",
                   "rmarkdown",
                   "survey",
                   "srvyr"),
                 dependencies = TRUE,
                 quiet = TRUE)


if (testr <- require("tidyverse")) {
  message("Set-up was successful!")
} else {
  warning("Set-up unsuccessful; reach out to instructor.")
}