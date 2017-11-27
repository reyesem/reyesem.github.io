################################################################################
# File: BootstrapFunctions
# Description: Additional functions that provide the ability to bootstrap a
#              linear model. Bootstrapping can be done to develop confidence
#              intervals or construct p-values. What is returned is complete
#              bootstrap distribution along with confidence interval or p-value.
#
# Author: Eric Reyes
# Date: Fall 2017
#
# Notes:
#  1. Not written for efficiency. Instead, the idea was to make the interface
#     as simple for students as possible and support conceptual learning of
#     bootstrapping.

require(tidyverse)
require(stringr)
require(broom)
require(modelr)

# function: rmammen
# description: Take a random sample from the distribution described by Mammen
#              for implementing the wild bootstrap.
#
# parameters:
#  n           Number of replicates to generate.
rmammen <- function(n){
  .vals <- c(-(sqrt(5)-1)/2, (sqrt(5)+1)/2)
  .probs <- rev(abs(.vals)/sqrt(5))
  
  sample(.vals, size = n, replace = TRUE, prob = .probs)
}


# function: print.reyesboot
# description: Method for printing an object of the class bootstrap.
print.bootstrap <- function(x, ...){
  .addattr <- attributes(x) %>%
    names() %>%
    str_subset("(CI)|(p-value)")
  
  if(str_detect(.addattr, "CI")){
    cat(.addattr, ": \n", sep = "")
    print(attr(x, .addattr))
  } else {
    cat("Test Statistic: ", attr(x, "test-stat"), "\n",
        "P-Value: ", attr(x, "p-value"), "\n", sep = "")
  }
}


# function: boot_add_ci
# description: Add CI to a bootstrap distribution.
boot_add_ci <- function(boot_out, conf.level){
  # Remove p-value
  boot_out <- boot_out %>%
    select(-p.value)
  
  # Compute CI
  .out.ci <- boot_out %>%
    group_by(term) %>%
    summarise(LCL = quantile(estimate, prob = (1-conf.level)/2),
              UCL = quantile(estimate, prob = (1+conf.level)/2))
  
  
  # Prepare Output
  attr(boot_out, str_c(100*conf.level, "% CI")) <- .out.ci
  class(boot_out) <- c("bootstrap", class(boot_out))
  
  boot_out
}


# function: boot_add_p
# description: Add p-value to a bootstrap distribution.
boot_add_p <- function(boot_out, orig.model, null.model){
  # Remove p-value
  boot_out <- boot_out %>%
    select(-p.value)
  
  # Remove extraneous lines from anova() output
  boot_out <- boot_out %>%
    filter(!is.na(statistic)) %>%
    as_data_frame()
  
  # Compute Observed Statistic
  .obs.F <- anova(null.model, orig.model)$F[2]
  
  
  # Compute p-value
  .out.p <- boot_out %>%
    summarise(`p-value` = mean(statistic >= .obs.F)) %>%
    as.numeric()
  
  
  # Prepare Output
  attr(boot_out, "p-value") <- .out.p
  attr(boot_out, "test-stat") <- .obs.F
  class(boot_out) <- c("bootstrap", class(boot_out))
  
  boot_out
}


# function: boot_pairwise_ci
# description: Perform a pairwise bootstrap of a model and return the full
#              summary of the model from each replicate as well as the CI
#              for each of the parameters in the model.
boot_pairwise_ci <- function(orig.model, n.boot.reps, conf.level, group.var){
  # Ungrouped Pairwise Bootstrap
  if(is.null(rlang::f_rhs(rlang::enquo(group.var)))){
    # obtain the original dataset from model fit
    .orig.df <- suppressWarnings(augment(orig.model))
    
    # correct names in original dataset
    .orig.names <- names(orig.model$model)
    names(.orig.df)[1:length(.orig.names)] <- .orig.names
    
    # generate bootstrap replicates
    #  these are stored efficiently through modelr package
    .boot.df <- .orig.df %>%
      modelr::bootstrap(n = n.boot.reps, id = "replicate")
    
    # fit models
    .out.df <- map(.boot.df$strap, ~ update(orig.model, data = .)) %>%
      map_df(., tidy, .id = "replicate") %>%
      as_data_frame()
    
  }else{
    
    # obtain original dataset, enquo only works if group_by not piped
    .orig.df <- suppressWarnings(augment(orig.model))
    .orig.df <- group_by(.orig.df, !!rlang::enquo(group.var))
    
    # correct names in original dataset
    .orig.names <- names(orig.model$model)
    names(.orig.df)[1:length(.orig.names)] <- .orig.names
    
    # bootstrap, handle groups using do()
    .boot.df <- .orig.df %>%
      do(bootstrap(., n = n.boot.reps)) %>%
      group_by(.id)
    
    # fit models, need to regroup split replicates requires do.call, requiring
    #  do() to run the fitting for each replicate
    .out.df <- .boot.df %>%
      do({
        do.call(rbind, map(.$strap, as_data_frame)) %>%
          update(orig.model, data = .) %>%
          tidy()
      }) %>%
      rename(replicate = .id) %>%
      as_data_frame()
  }
  
  
  # Compute CI
  boot_add_ci(.out.df, conf.level)
}


# function: boot_pairwise_p
# description: Perform a pairwise bootstrap of a model and return the full
#              summary of the comparison to the null model as well as the
#              p-value for the corresponding comparison.
boot_pairwise_p <- function(orig.model, null.model, n.boot.reps, group.var){
  # Ungrouped Pairwise Bootstrap
  if(is.null(rlang::f_rhs(rlang::enquo(group.var)))){
    # obtain the fitted values under the null
    .null.df <- suppressWarnings(augment(null.model))
    
    # obtain the original dataset but replace the fitted values with those
    #  under the null and create new "original responses"
    .orig.df <- suppressWarnings(augment(orig.model)) %>%
      mutate(.fitted = .null.df$.fitted,
             .newy = .fitted + .resid)
    
    # correct names in original dataset
    .orig.names <- names(orig.model$model)
    names(.orig.df)[1:length(.orig.names)] <- .orig.names
    
    # bootstrap
    .boot.df <- .orig.df %>%
      modelr::bootstrap(n = n.boot.reps, id = "replicate")
    
    # fit models
    #  both null and alternative models are fit with new data and
    #  anova() is used to compute the statistic testing the two models.
    .out.df <- map(.boot.df$strap, function(df){
      h1 <- update(orig.model, .newy ~ ., data = df)
      h0 <- update(null.model, .newy ~ ., data = df)
      
      anova(h0, h1)}) %>%
      map_df(., tidy, .id = "replicate")
    
  }else{
    
    # obtain original dataset, enquo only works if group_by not piped
    .null.df <- suppressWarnings(augment(null.model))
    .orig.df <- suppressWarnings(augment(orig.model)) %>%
      mutate(.fitted = .null.df$.fitted,
             .newy = .fitted + .resid)
    .orig.df <- group_by(.orig.df,
                         !!rlang::enquo(group.var))
    
    # correct names in original dataset
    .orig.names <- names(orig.model$model)
    names(.orig.df)[1:length(.orig.names)] <- .orig.names
    
    # bootstrap, handle groups using do()
    .boot.df <- .orig.df %>%
      do(modelr::bootstrap(., n = n.boot.reps)) %>%
      group_by(.id)
    
    # fit models, need to regroup split replicates requires do.call, requiring
    #  do() to run the fitting for each replicate
    .out.df <- .boot.df %>%
      do({
        df <- do.call(rbind, map(.$strap, as_data_frame))
        h1 <- update(orig.model, .newy ~ ., data = df)
        h0 <- update(null.model, .newy ~ ., data = df)
        
        anova(h0, h1) %>%
          tidy()
      }) %>%
      rename(replicate = .id)
  }
  
  
  # Add P-value
  boot_add_p(.out.df, orig.model, null.model)
}


# function: boot_residual_ci
# description: Perform a residual bootstrap of a model and return the full
#              summary of the model from each replicate as well as the CI
#              for each of the parameters in the model. A wild bootstrap is
#              implemented for the case of non-constant variance.
boot_residual_ci <- function(orig.model, n.boot.reps, conf.level, constant.var){
  # Obtain the original residuals
  .orig.df <- suppressWarnings(augment(orig.model))
  .n <- nrow(.orig.df)
  
  # correct names in original dataset
  .orig.names <- names(orig.model$model)
  names(.orig.df)[1:length(.orig.names)] <- .orig.names
  
  # Traditional Residual Bootstrap
  if(constant.var){
    # bootstrap residuals, only store residuals for efficiency
    .boot.list <- map(seq_len(n.boot.reps),
                      function(u){
                        sample(.orig.df$.resid, size = .n, replace = TRUE)
                      })
    
  }else{
    
    # bootstrap, wild bootstrap
    .boot.list <- map(seq_len(n.boot.reps),
                      function(u){
                        .orig.df$.resid*rmammen(.n)
                      })
  }
  
  # Fit models
  #  create new response and then update
  .out.df <- .boot.list %>%
    map_df(function(resids){
      mutate(.orig.df, .newy = .fitted + resids) %>%
        update(orig.model, .newy ~ ., data = .) %>%
        tidy()
    })
  
  
  # Compute CI
  boot_add_ci(.out.df, conf.level)
}


# function: boot_residual_p
# description: Perform a residual bootstrap of a model and return the full
#              summary of the comparison to the null model as well as the
#              p-value for the corresponding comparison. A wild bootstrap is
#              implemented for the case of non-constant variance.
boot_residual_p <- function(orig.model, null.model, n.boot.reps, constant.var){
  # Obtain the original residuals
  .orig.df <- suppressWarnings(augment(orig.model))
  .null.df <- suppressWarnings(augment(null.model))
  .n <- nrow(.orig.df)
  
  # correct names in original dataset
  .orig.names <- names(orig.model$model)
  names(.orig.df)[1:length(.orig.names)] <- .orig.names
  
  # Traditional Residual Bootstrap
  if(constant.var){
    # bootstrap residuals, only store residuals for efficiency
    #  bootstrap under null model
    .boot.list <- map(seq_len(n.boot.reps),
                      function(u){
                        sample(.null.df$.resid, size = .n, replace = TRUE)
                      })
    
  }else{
    
    # bootstrap, wild bootstrap under null model
    .boot.list <- map(seq_len(n.boot.reps),
                      function(u){
                        .null.df$.resid*rmammen(.n)
                      })
  }
  
  # Fit models
  #  generate new response under null model
  .out.df <- .boot.list %>%
    map_df(function(resids){
      df <- mutate(.orig.df, .newy = .null.df$.fitted + resids)
      h1 <- update(orig.model, .newy ~ ., data = df)
      h0 <- update(null.model, .newy ~ ., data = df)
      
      anova(h0, h1) %>% tidy()
    })
  
  
  # Add P-value
  boot_add_p(.out.df, orig.model, null.model)
}


# function: boot_model
# description: Primary function called by users to conduct the bootstrap.
#
# parameters:
#  orig.model    Model fit. The model of interest.
#  null.model    Model fit (optional). The model under the null hypothesis under
#                 which data is generated. If NULL, confidence intervals
#                 computed instead of a p-value.
#  n.boot.reps   Scalar. Number of bootstrap replicates to generate
#                 (default = 1000).
#  method        Character. If "residual" (default), a residual bootstrap is
#                 conducted. If "pairwise", a pairwise bootstrap is conducted.
#  constant.var  Boolean. If TRUE (default) then constant variance is assumed
#                 when doing the residual bootstrap. If FALSE, a wild bootstrap
#                 is implemented. Ignored if `residual = FALSE`
#  group.var     Variable in the orig.model. A pairwise bootstrap will be done
#                 within each level of this variable if specified. If NULL
#                 (default), bootstrapping is done across entire dataset.
#                 Ignored if `residual = TRUE`
#  conf.level    Scalar. The confidence level used to compute confidence
#                 intervals. This should be a value between 0 and 1. If between
#                 1 and 100, divided by 100 to form a value between 0 and 1. If
#                 greater than 100, error thrown (default = 0.95). Ignored if
#                 null.model specified.
boot_model <- function(orig.model,
                       null.model = NULL,
                       n.boot.reps = 1000,
                       method = c("residual", "pairwise"),
                       constant.var = TRUE,
                       group.var = NULL,
                       conf.level = 0.95){
  
  # Determine method
  method = match.arg(method)
  
  # Adjust Confidence Level
  if(method == "residual"){
    if(conf.level >= 1) conf.level <- conf.level/100
    if(conf.level <= 0 | conf.level > 1){
      stop("Confidence level must be between 0 and 1.")
    }
  }
  
  # Confidence Intervals vs. P-value
  if(is.null(null.model)){
    # If Residual Bootstrap
    if(method == "residual"){
      boot_residual_ci(orig.model = orig.model,
                       n.boot.reps = n.boot.reps,
                       conf.level = conf.level,
                       constant.var = constant.var)
    }else{
      boot_pairwise_ci(orig.model = orig.model,
                       n.boot.reps = n.boot.reps,
                       conf.level = conf.level,
                       group.var = !!rlang::enquo(group.var))
    }
  }else{
    # If Residual Bootstrap
    if(method == "residual"){
      boot_residual_p(orig.model = orig.model,
                      null.model = null.model,
                      n.boot.reps = n.boot.reps,
                      constant.var = constant.var)
    }else{
      boot_pairwise_p(orig.model = orig.model,
                      null.model = null.model,
                      n.boot.reps = n.boot.reps,
                      group.var = !!rlang::enquo(group.var))
    }
  }
}
