################################################################################
# Program: Directory Setup
# Class: MA223 Engineering Statistics I
# Description: Automate the directory set up package installation for the class.
#
# Author: Eric Reyes
# Date: Fall 1920
#
# Notes:

# ---- Update Packages ----
update.packages(lib.loc = .libPaths()[2],
                checkBuilt = TRUE,
                ask = FALSE,
                instlib = .libPaths()[1],
                quiet = TRUE)

# ---- Package Installation ----
install.packages("knitr", lib = .libPaths()[1], quiet = TRUE)
install.packages("caTools", lib = .libPaths()[1], quiet = TRUE)
install.packages("rprojroot", lib = .libPaths()[1], quiet = TRUE)
install.packages("googlesheets", lib = .libPaths()[1], quiet = TRUE)
install.packages("https://reyesem.github.io/files/MA223/IntroAnalysis_0.1.0.tar.gz",
                 lib = .libPaths()[1], repos = NULL)
install.packages("rlang", lib = .libPaths()[1], quiet = TRUE)
install.packages('tinytex', lib = .libPaths()[1], quiet = TRUE)

# ---- Install Tex ----
tinytex::install_tinytex()


# ---- Create Directory ----
dir.create("Analysis-Tasks")
dir.create("Homework")
dir.create("Labs")
dir.create("In-Class-Demos")
dir.create("Final-Exam")


# ---- Add Files to Directory ----
# Note: this needs to be updated. add the tex style file. also, include other
# templates and documents for use???
download.file("https://raw.githubusercontent.com/reyesem/reyesem.github.io/master/files/MA223/Assignment-Template.Rmd",
              "Homework/Assignment-Template.Rmd")

download.file("https://raw.githubusercontent.com/reyesem/reyesem.github.io/master/files/MA223/Assignment-Template.Rmd",
              "Analysis-Tasks/Assignment-Template.Rmd")

download.file("https://raw.githubusercontent.com/reyesem/reyesem.github.io/master/files/MA223/Assignment-Template.Rmd",
              "In-Class-Demos/Assignment-Template.Rmd")
