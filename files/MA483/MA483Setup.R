################################################################################
# File: MA483Setup
# Class: MA483 - Bayesian Data Analysis
# Description: Additional code to be included with each assignment.
#
# Author: Eric Reyes
# Date: Winter 2019-2020
# Modified:
#
# Notes:

# ---- Load Packages ----
# These provide additional functionality that we will use in class.
pkgs <- c("tidyverse",
          "skimr",
          "broom",
          "broom.mixed",
          "HDInterval",
          "rstan",
          "rstanarm",
          "bayesplot",
          "bridgesampling")

for(pkg in pkgs) library(pkg, character.only = TRUE)



# ---- Change Options ----
# This changes some of the default options specified in R and RMarkdown.

# Suppress status bar in dplyr.
options(dplyr.show_progress = FALSE)

# Always use dummy contrasts.
options(contrasts = rep("contr.treatment", 2))

# Correct behavior of skimr
skim_with(numeric = list(hist = NULL), 
          ts = list(line_graph = NULL),
          character = get_skimmers()$factor)

# Use multiple cores for rstan.
options(mc.cores = (parallel::detectCores() - 2))

# Change theme for plots
theme_set(theme_bw(12))
theme_update(legend.position = "bottom",
             legend.box = "vertical")

rstan_ggtheme_options(legend.position = "bottom",
                      legend.box = "vertical",
                      panel.background = element_rect(fill = "white",
                                                      color = NA),
                      panel.border = element_rect(fill = NA,
                                                  color = "grey20"),
                      panel.grid = element_line(color = "grey92"),
                      panel.grid.minor = element_line(size = rel(0.5)),
                      strip.background = element_rect(fill = "grey85",
                                                      color = "grey20"),
                      legend.key = element_rect(fill = "white",
                                                color = NA),
                      complete = TRUE)


# Specify chunck options
knitr::opts_chunk$set(
  prompt = FALSE,
  comment = "")


# ---- Create Special Blocks ----
# This creates an environment for interpreting an instructor block that might
# appear in a handout. This will generally be ignored by students.
eng_instructor <- function(options) {
  if (identical(options$echo, FALSE)) return()
  
  # Steal some ideas from block definition
  to = knitr::opts_knit$get("rmarkdown.pandoc.to")
  is_pandoc = !is.null(to)
  if(!is_pandoc){
    to = knitr::opts_knit$get("out.format")
    if(!(to %in% c("latex", "html", "markdown"))) to = NULL
  }
  
  if(is.null(to)) return(code)
  if(to=="beamer") to = "latex"
  if(grepl("(markdown)|(epub)|(html)|(revealjs)|(s5)|(slideous)|(slidy)",
           to)) to = "html"
  
  
  code = paste(options$code, collapse = '\n'); type = options$type
  if (is.null(type)) return(code)
  
  if(!is.null(type) && type=="solution"){
    code = paste("__SOLUTION__:", code)
  }
  
  if (is.null(knitr::opts_knit$get("rmarkdown.pandoc.to"))){
    stop('The engine "block2" is for R Markdown only')
  } 
  
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
  
  if(to=="latex"){
    sprintf('\\BeginKnitrBlock{%s}%s\n%s%s%s\n\\EndKnitrBlock{%s}',
            type, l1, h5, code, h6, type)
  } else {
    sprintf(
      '\\BeginKnitrBlock{%s}%s%s<%s class="%s" custom-style="%s">%s%s%s</%s>%s\\EndKnitrBlock{%s}',
      type, l1, h3, h2, type, type, h5, code, h6, h2, h4, type
    )
  }
}

knitr::knit_engines$set(c(knitr::knit_engines$get(), 
                          "instructor" = eng_instructor))



# ---- Additional Functions ----
# These are special functions written specifically for the course.

# function: stan_to_df
# description: Convert the parameter arguments from stan to a tibble.
#
# parameters:
#  object      stanfit object (see stan::extract()).
#
# importFrom rlang .data
stan_to_df <- function(object, include_warmup = FALSE){
  params <- rstan::extract(object, 
                           permuted = FALSE,
                           inc_warmup = TRUE)
  
  col.names <- dimnames(params)$parameters
  
  params <-
    do.call(apply(params, 3, function(u){
      tibble::tibble(
        `_Value` = c(u),
        `_Chain` = rep(seq_along(dimnames(u)[[2]]),
                       each = nrow(u)),
        `_Iteration` = rep(seq(nrow(u)),
                           times = length(dimnames(u)[[2]])))}),
      what = "cbind")
  
  params <- 
    cbind(dplyr::select(params, tidyselect::contains("_Value")),
          paste0("chain:", params[, ncol(params) - 1]),
          params[, ncol(params)]) %>%
    tibble::as_tibble()
  
  colnames(params) <- c(col.names, ".chain", ".iteration")
  
  params <- params %>%
    dplyr::mutate(`.warmup` = .iteration <= object@sim$warmup)
  
  if (!include_warmup){
    params <- params %>%
      dplyr::filter(!(.data$`.warmup`)) %>%
      dplyr::select(-(.data$`.warmup`))
  }
  
  params
}
