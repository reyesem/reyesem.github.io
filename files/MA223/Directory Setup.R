################################################################################
# Program: Directory Setup
# Class: MA223 Engineering Statistics I
# Description: Automate the directory set up package installation for the class.
#
# Author: Eric Reyes
# Date: Fall 1819
#
# Notes:

# ---- Package Installation ----
install.packages("stringi")
install.packages("knitr")
install.packages("googlesheets")
install.packages("https://reyesem.github.io/files/MA223/IntroAnalysis_0.1.0.tar.gz", repos = NULL)


# ---- Create Directory ----
dir.create("Homework")
dir.create("Labs")
dir.create("In-Class-Demos")
dir.create("Final-Exam")

download.file("https://reyesem.github.io/files/MA223/MA223StyleDoc.docx",
              "MA223StyleDoc.docx")

