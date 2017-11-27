################################################################################
# File: MA223Styles
# Course: MA223 Engineering Statistics I
# Description: Additional R functions for creating MA223 styles.
#
# Author: Eric Reyes
# Date: Fall 2017

## ---- Load Packages ----
pkgs <- c("car",
          "tidyverse",
          "broom",
          "stringr",
          "knitr")

for(pkg in pkgs) library(pkg, character.only = TRUE)


## ---- Include Bootstrap Functions ----
source("https://reyesem.github.io/files/MA223/BootstrapFunctions.R")


## ---- Redefine Functions ----
# In 'broom' package, augment() throws warning messages every time it is issued
# due to the use of outdated code. This updates that code to suppress these
# errors.
augment <- function(x, ...){
  suppressWarnings(broom::augment(x, ...))
}


## ---- Change Options ----
# Suppress status bar in dplyr.
options(dplyr.show_progress = FALSE)

# Change theme for plots
theme_set(theme_bw(12))
theme_update(legend.position = "bottom")

# Specify chunck options
knitr::opts_chunk$set(
  prompt = FALSE,
  comment = "",
  message = FALSE,
  warning = FALSE)


## ---- Create Special Blocks ----
eng_instructor <- function(options) {
  if (identical(options$echo, FALSE)) return()
  
  code = paste(options$code, collapse = '\n'); type = options$type
  if (is.null(type)) return(code)
  
  if(!is.null(type) && type=="solution"){
    code = paste("__SOLUTION__:", code)
  }
  
  if (is.null(opts_knit$get("rmarkdown.pandoc.to"))) stop('The engine "block2" is for R Markdown only')
  
  l1 = options$latex.options
  if (is.null(l1)) l1 = ''
  # protect environment options because Pandoc may escape the characters like
  # {}; when encoded in integers, they won't be escaped, but will need to
  # restore them later; see bookdown:::restore_block2
  if (l1 != '') l1 = paste(
    c('\\iffalse{', utf8ToInt(enc2utf8(l1)), '}\\fi{}'), collapse = '-'
  )
  h2 = ifelse(is.null(options$html.tag), 'div', options$html.tag)
  h3 = ifelse(is.null(options$html.before), '', options$html.before)
  h4 = ifelse(is.null(options$html.after), '', options$html.after)
  h5 = ifelse(is.null(options$html.before2), '', options$html.before2)
  h6 = ifelse(is.null(options$html.after2), '', options$html.after2)
  
  sprintf(
    '\\BeginKnitrBlock{%s}%s%s<%s class="%s" custom-style="%s">%s%s%s</%s>%s\\EndKnitrBlock{%s}',
    type, l1, h3, h2, type, type, h5, code, h6, h2, h4, type
  )
}


knit_engines$set(c(knit_engines$get(), 
                   "instructor" = eng_instructor))
