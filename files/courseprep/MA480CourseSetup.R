################################################################################
# File: MA480CourseSetup
# Class: MA480 - Social Justice and Statistics
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
                   "import",
                   'rsample',
                   "knitr",
                   "rmarkdown",
                   'skimr',
                   "survey",
                   "srvyr"),
                 dependencies = TRUE,
                 quiet = TRUE)

install.packages(
  "https://github.com/reyesem/reyesem.github.io/raw/master/files/courseprep/reyescourses_0.1.0.tar.gz",
  repos = NULL,
  quiet = TRUE)


if (testr <- require("tidyverse")) {
  message("Set-up was successful!")
} else {
  warning("Set-up unsuccessful; reach out to instructor.")
}
