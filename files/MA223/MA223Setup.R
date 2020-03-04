################################################################################
# File: MA223Setup
# Course: MA223 Engineering Statistics I
# Description: Loads all proper packages.
#
# Author: Eric Reyes
# Date: Fall 2019

## ---- Clean Slate ----
# Remove anything previously existing and start with clean slate
rm(list = ls(sorted=FALSE)[ls(sorted=FALSE) != "params"])
gc()

## ---- Load Packages ----
pkgs <- c("IntroAnalysis",
          "rlang",
          "magrittr",
          "ggplot2",
          "dplyr",
          "tidyr",
          "readr",
          "purrr",
          "tibble",
          "stringr",
          "forcats",
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

# Specify chunk options
knitr::opts_chunk$set(
  prompt = FALSE,
  comment = "",
  message = FALSE,
  warning = FALSE,
  out.width = ifelse(is_latex_output(), "0.8\\textwidth", "80%"),
  fig.align = "center",
  linewidth = 80)


## ---- Ensure Source Code Wraps ----
.hook_source = knitr::knit_hooks$get("source")

knitr::knit_hooks$set(
  source = function(x, options){
    # this hook is used only when linewidth option is not NULL
    if (!is.null(n <- options$linewidth)){
      # # only split on real line breaks
      # breakinstr <- '(\"([^\"]*\n)+[^\"]+\")|(\'([^\']*\n)+[^\']+\')'
      # if (stringr::str_detect(x, breakinstr)){
      #   t1 <- unlist(stringr::str_extract_all(x, breakinstr))
      #   t2 <- stringr::str_replace_all(t1, "\n", " ")
      #   
      #   for (i in 1:length(t1)){
      #     x <- stringr::str_replace(x, t1[i], t2[i])
      #   }
      # }
      # 
      # x = knitr:::split_lines(x)
      # x.length <- nchar(x)
      # spaces <- stringr::str_extract(x, "^[[:space:]]+")
      # spaces[is.na(spaces)] <- ""
      # spaces.length <- nchar(spaces)
      # 
      # # if all lines fine, skip
      # if (any(x.length > n)){
      #   # wrap lines which are longer than n characters
      #   x <- mapply(function(u, v, w, y){
      #     if (v > n){
      #       u = paste(w, stringr::str_wrap(u, width = n - y))
      #     } else {
      #       u
      #     }}, x, x.length, spaces, spaces.length)
      #   
      #   x <- paste(x, collapse = "\n")
      # }
      
      x = knitr:::split_lines(x)
      
      x = ifelse(nchar(x) > n, stringr::str_wrap(x, width = n, exdent = 2), x)
    }
    
    .hook_source(x, options)
  })


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
