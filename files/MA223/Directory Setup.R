################################################################################
# Program: Directory Setup
# Class: MA223 Engineering Statistics I
# Description: Automate the directory set up package installation for the class.
#
# Author: Eric Reyes
# Date: Fall 1819
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
install.packages("googlesheets", lib = .libPaths()[1], quiet = TRUE)
install.packages("https://reyesem.github.io/files/MA223/IntroAnalysis_0.1.0.tar.gz",
                 lib = .libPaths()[1], repos = NULL)


# ---- Create Directory ----
dir.create("Analysis-Tasks")
dir.create("Homework")
dir.create("Labs")
dir.create("In-Class-Demos")
dir.create("Final-Exam")

download.file("https://reyesem.github.io/files/MA223/MA223StyleDoc.docx",
              "MA223StyleDoc.docx")

