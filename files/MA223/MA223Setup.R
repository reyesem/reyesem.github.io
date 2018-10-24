################################################################################
# File: MA223Styles
# Course: MA223 Engineering Statistics I
# Description: Additional R functions for creating MA223 styles.
#
# Author: Eric Reyes
# Date: Fall 2018

## ---- Clean Slate ----
# Remove anything previously existing and start with clean slate
rm(list = ls(sorted=FALSE)[ls(sorted=FALSE)!="params"])
gc()

# temp adjustment until next year
obtain_diagnostics <- function(mean.model,
                               data = stats::model.frame(mean.model)){
  .out <- broom::augment(mean.model, data = data,
                         type.predict = "response",
                         type.residuals = "deviance")
  
  .out <- subset(.out, select =  which(!is.element(colnames(.out),
                                                   c(".hat",
                                                     ".sigma",
                                                     ".cooksd",
                                                     ".se.fit",
                                                     ".std.resid"))))
  
  # adjust column names to undo what broom does
  colnames(.out)[1:ncol(data)] <- colnames(data)
  
  .out
}

## ---- Load Packages ----
pkgs <- c("IntroAnalysis",
          "tidyverse",
          "broom",
          "stringr",
          "knitr")

for(pkg in pkgs) library(pkg, character.only = TRUE)


## ---- Change Options ----
# Suppress status bar in dplyr.
# Change handling of ordered factors
options(dplyr.show_progress = FALSE,
        contrasts = rep("contr.treatment", 2))

# Change theme for plots
theme_set(theme_bw(14))
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

  code = paste0(paste(options$code, collapse = '\n'), "\n"); type = options$type
  if (is.null(type)) return(code)

  if(!is.null(type) && type=="solution"){
    code = paste("__SOLUTION__:  ", code, sep="\n")
  }

  if (is.null(opts_knit$get("rmarkdown.pandoc.to"))) stop('The engine "instructor" is for R Markdown only')

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
