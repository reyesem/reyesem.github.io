################################################################################
# File: MA482Styles
# Course: MA223 Engineering Statistics I
# Description: Additional R functions for creating MA482 styles.
#
# Author: Eric Reyes
# Date: Fall 2017

## ---- Load Packages ----
pkgs <- c("tidyverse",
          "broom",
          "splines",
          "geepack",
          "rms",
          "lme4",
          "knitr")

for(pkg in pkgs) library(pkg, character.only = TRUE)


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
  
  # Steal some ideas from block definition
  to = opts_knit$get("rmarkdown.pandoc.to")
  is_pandoc = !is.null(to)
  if(!is_pandoc){
    to = opts_knit$get("out.format")
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


knit_engines$set(c(knit_engines$get(), 
                   "instructor" = eng_instructor))


# Special Functions

################################################################################
# function: corY
# description: Compute the empirical correlation of a response across a grouping
#              variable.
#
# parameters:
#   y          Vector. The value of the response.
#   id         Vector. The variable specifying the grouping structure.
#   time       Vector. The time points at which the values were taken. If
#               omitted, it is assumed the order of the data defines the time
#               so that all missing data occurs at the end.
#   data       Dataframe. Optional dataframe specifying where the variables
#               should be drawn from.
corY <- function(y, id, time, data){
  require(reshape2, quietly=TRUE)
  data <- data.frame(data)
  
  # Construct Data
  if(!missing(data)){
    dat <- data.frame(y=data[,as.character(substitute(y))],
                      id=data[,as.character(substitute(id))])
    
    if(!missing(time)){
      dat$time <- data[,as.character(substitute(time))]
    }
    
    if(missing(time)){
      dat$time <- c(unlist(sapply(table(dat$id), seq)))
    }
  }
  
  if(missing(data)){
    dat <- data.frame(y=y, id=id)
    if(!missing(time)) dat$time <- time
    if(missing(time)) dat$time <- c(unlist(sapply(table(dat$id), seq)))
  }
  
  # Reshape the data
  dat <- melt(dat, measure.vars="y")
  dat <- dcast(dat, id ~ time, drop=FALSE)
  
  
  # Compute correlation
  #  use all available data
  out <- cor(dat[,-1], use="pairwise.complete.obs")
  return(out)
}




################################################################################
# function: CoxSnellPlot
# description: Create a plot of the Cox-Snell residuals for examining the
#              goodness of fit for a cox PH model.
# 
# parameters
#   fit        coxph Model fit.  Object fit using coxph() in survival package.
CoxSnellPlot <- function(fit){
  # Update model, if necessary
  fit <- update(fit, x=TRUE, y=TRUE)
  
  # Obtain Cox-Snell residuals from fit
  resi <- fit$y[,2] - residuals(fit,type="martingale") 
  
  # Compute the cumulative hazard of these residuals
  Sfit <- survfit(Surv(time=resi, event=fit$y[,2])~1)
  CumHaz <- cumsum(Sfit$n.event/Sfit$n.risk)
  
  plot.dat <- data.frame(resi=Sfit$time,
                         cumhaz=CumHaz)
  
  # Construct cumulative hazard vs. residuals
  p1 <- ggplot(data=plot.dat,mapping=aes(y=cumhaz,x=resi)) +
    geom_abline(slope=1,intercept=0,colour="grey") +
    geom_point() +
    labs(x="Cox-Snell Residuals",y="Estimated Cumulative Hazard Rates") +
    theme_bw(16)
  
  return(p1)
}




################################################################################
# function: DataSummary
# description: Create a nice-ish printable summary of the contents of a
#              dataset stored in a dataframe. This is strictly for printing,
#              no information is stored. See describe() in the Hmisc package
#              for a function which summarizes the output and stores it in a
#              list for future uses.
#
# parameters:
#   df         Dataframe. The dataset to be summarized.
DataSummary <- function(df){
  # Separate categorical/numeric variables
  vars <- sapply(df,class)
  char.vars <- vars %in% c("logical","factor","character")
  num.vars <- vars %in% c("numeric","integer","double")
  
  # Summarize each categorical variable
  char.tab <- NULL
  if(sum(char.vars)>0){
    # get character variables
    char.tab <- df[,char.vars,drop=FALSE]
    
    # Obtain frequency (relative freq) for each level of each variable
    # and store results in a large table.
    char.tab <- lapply(char.tab, function(u) table(factor(u)))
    char.tab <- lapply(char.tab, function(u) round(100*u/sum(u),2))
    char.tab <- lapply(char.tab, function(u) paste(names(u),u,sep=": "))
    
    # Pad with blanks
    if(length(char.tab)>1){
      max.leng <- max(sapply(char.tab, function(u) length(u)))
      
      char.tab <- lapply(char.tab, function(u){
        c(u, rep("", max.leng - length(u)))
      })
    }
    
    char.tab <- data.frame(char.tab)
  }
  
  # Summarize each numeric variable
  #   compute 5 number summary, IQR, Mean, and Std. Dev.
  num.tab <- NULL
  if(sum(num.vars)>0){
    # get numerical variables
    num.tab <- df[,num.vars,drop=FALSE]
    
    num.tab <- sapply(num.tab, function(u){
      c(mean(u, na.rm=TRUE),
        sd(u, na.rm=TRUE),
        summary(u)[c(1,2,3,5,6)], diff(summary(u)[c(2,5)]))
    })
    
    num.tab <- data.frame(num.tab,
                          row.names=c("Mean","Std.Dev.",
                                      "Min","Q1","Median","Q3","Max",
                                      "IQR"))
    names(num.tab) <- names(df)[num.vars]
  }
  
  # Print out Summary
  cat("Number of Observations: ",nrow(df),"\n",
      "Number of variables: ",ncol(df),"\n",
      "  Number of categorical variables: ",sum(char.vars),"\n",
      "  Number of quantitative variables: ",sum(num.vars),"\n",
      "\n\n",
      "Relative frequency of categorical variables:\n",sep="")
  if(!is.null(char.tab)) print(char.tab,row.names=FALSE)
  cat("\n\n",
      "Summary statistics for quantitative variables:\n",sep="")
  if(!is.null(num.tab)) print(num.tab)
}




################################################################################
# glance.geeglm
# description: Method for glance for the geeglm class.
glance.geeglm <- function(x, ...){
  s <- summary(x)
  data.frame(n=sum(s$clusz),
             sigma=sqrt(s$geese$scale$estimate),
             df.residual=s$df[2],
             n.clusters=length(s$clusz),
             max.cluster.size=max(s$clusz))
}





################################################################################
# KMest
# description: Compute kaplan-meier estimates for survival data. This is a front
#              end to survfit in order to return a nicer set of data.
#
# parameters:
#   form       Formula for describing the survival data.
#   data       Optional dataset where variables appearing in form should be
#                located
#   conflevel  Scalar. Confidence level for confidence intervals (default=0.95).
KMest <- function(form, data, conflevel=0.95){
  # Use survfit to get the estimates
  if(missing(data)) fit <- survfit(formula=form, conf.int=conflevel)
  if(!missing(data)){
    fit <- survfit(formula=form, data=data, conf.int=conflevel)
  } 
  
  # Obtain name of time in dataset
  survobj <- strsplit(as.character(form[2]), ",")[[1]][1]
  survobj <- strsplit(survobj,"\\(")[[1]][2]
  survobj <- strsplit(survobj,"=")[[1]][1]
  
  # Construct data
  dat <- data.frame(Survival=fit$surv,
                    UCL=fit$upper,
                    LCL=fit$lower)
  
  dat[,survobj] <- fit$time
  dat <- dat[,c(4,1,2,3)]
  
  # Add grouping if available
  if(!is.null(fit$strata)){
    names(fit$strata) <- sapply(strsplit(names(fit$strata),"="),"[",2)
    dat[,as.character(form[3])] <- rep(names(fit$strata), times=fit$strata)
  }
  
  return(dat)
}




################################################################################
# function: LifeTable
# description: Construct life table estimates for survival data.
#
# parameters:
#   nrisk      Number at risk at start of interval.
#   nevent     Number of events occurring during interval.
#   ncensor    Number of subjects censored during interval.
#   data       Dataframe. Optional dataset containing the variables above.
#   conflevel  Scalar. Number between 0 and 1 defining the confidence level.
LifeTable <- function(nrisk, nevent, ncensor, data, conflevel=0.95){
  data <- data.frame(data)
  
  # Form Dataset
  if(!missing(data)){
    dat <- data.frame(nrisk=data[,as.character(substitute(nrisk))],
                      nevent=data[,as.character(substitute(nevent))],
                      ncensor=data[,as.character(substitute(ncensor))])
  }
  
  if(missing(data)){
    dat <- data.frame(nrisk=nrisk,
                      nevent=nevent,
                      ncensor=ncensor)
  }
  
  
  # Compute Quantities of Interest
  dat <- within(dat, {
    # Mortality Rate
    mortality <- nevent/(nrisk - 0.5*ncensor)
    
    # Survival
    survival <- cumprod(1-mortality)
    
    # SE
    se <- survival*sqrt(cumsum(mortality/(nevent*(1-mortality))))
    
    # Confidence Limits
    LCL <- pmax(0, survival - qnorm(0.5*(1+conflevel))*se)
    UCL <- pmin(1, survival + qnorm(0.5*(1+conflevel))*se)
  })
  
  dat <- dat[,c(1,2,3,8,7,6,5,4)]
  names(dat) <- c("Number at Risk","Number of Events","Number Censored",
                  "Mortality", "Survival", "Std. Error",
                  "Lower CL", "Upper CL")
  
  class(dat) <- c("LifeTable","data.frame")
  return(dat)
}




################################################################################
# function: LinearityPlots
# description: Construct residual plots to assess linearity of covariates.
#
# parameters:
#   fit        Fit Object. The full model fit which is to be assessed.
#   x          Vector. Character vector giving the names of the variables in the
#                dataset for which to assess linearity. Default is to include
#                linear variables in the  model.
#   labs       Vector. Character vector of labels for the covariates specified 
#                in x (default=Variable Names).
#   smooth     Boolean. If TRUE (default), a smoother is added to the plot to
#                aid in identifying trends. If FALSE, no smoother is added.
LinearityPlots <- function(fit, x, labs, smooth=TRUE){
  # Linear Model
  #   Create the usual 4-in-1 plot from Minitab which has a qq-plot of
  #   the residuals, a residual versus fitted values, a histogram of the
  #   residuals and a run chart.
  if(class(fit)[1]=="ols"){
    # Update fit to include x attribute, if necessary
    if(is.null(fit$x)) fit <- update(fit, x=TRUE)
    
    # Obtain predicted values and residuals and add quantiles
    #   Adjust for any weighting if necessary.
    resi <- fit$residuals
    if(!is.null(fit$weights)){
      resi <- fit$weights*resi
    }
    
    # Determine variables
    if(missing(x)) x <- fit$Design$name[fit$Design$assume.code==1]
    
    # Construct labels
    if(missing(labs)) labs <- x
    
    # Construct Plots
    for(i in 1:length(x)){
      # Create Data
      plot.dat <- data.frame(resi=resi, x=fit$x[,x[i]])
      
      # Make Plot
      plot <- ggplot(data=plot.dat, mapping=aes(y=resi, x=x)) +
        geom_hline(yintercept=0, colour="grey", linetype=2) +
        geom_point() +
        labs(x=labs[i], y="Residuals")
      
      if(smooth) plot <- plot + geom_smooth(method="loess")
      
      print(plot)
    }
  }
  
  # Cox Model
  #  Use the correct residuals in order to obtain the linearity
  if(class(fit)[1]=="cph"){
    # Update fit to include x and y attribute, if necessary
    fit <- update(fit, x=TRUE, y=TRUE)
    
    # Obtain residuals
    resi.m <- residuals(fit,type="martingale")
    resi.s <- residuals(fit,type="scaledsch")
    
    # Determine variables
    lin.vars <- fit$Design$name[fit$Design$assume.code==1]
    all.vars <- fit$Design$name[fit$Design$assume.code %in% c(1,5)]
    if(missing(x)) x <- all.vars
    
    # Construct labels
    if(missing(labs)) labs <- x
    
    for(i in 1:length(x)){
      # Continuous variables have both plots generated, categorical do not.
      if(x[i] %in% lin.vars){
        plot.dat1 <- data.frame(resi.m=resi.m, x=fit$x[,x[i]])
        plot.dat2 <- data.frame(resi.s=resi.s[,which(all.vars==x[i])],
                                time=fit$y[fit$y[,2]==1,1])
        
        p1 <- ggplot(data=plot.dat1, mapping=aes(y=resi.m, x=x)) +
          geom_point() +
          labs(x=labs[i], y="Martingale Residuals")
        
        p2 <- ggplot(data=plot.dat2, mapping=aes(y=resi.s, x=time)) +
          geom_point() +
          labs(x="Survival Time", 
               y=paste("Scaled Schoenfeld Residuals\n(",
                       labs[i],")",sep=""))
        
        if(smooth){
          p1 <- p1 + geom_smooth(method="loess")
          p2 <- p2 + geom_smooth(method="loess")
        }
        
        do.call(grid.arrange, list(p1,p2))
      }
      
      if(!(x[i] %in% lin.vars)){
        plot.dat2 <- data.frame(resi.s=resi.s[,which(all.vars==x[i])],
                                time=fit$y[fit$y[,2]==1,1])
        
        p2 <- ggplot(data=plot.dat2, mapping=aes(y=resi.s, x=time)) +
          geom_point() +
          labs(x="Survival Time", 
               y=paste("Scaled Schoenfeld Residuals\n(",
                       labs[i],")",sep=""))
        
        if(smooth) p2 <- p2 + geom_smooth(method="loess")
        
        print(p2)
      }
    }
  }
}




################################################################################
# function: PairedSample
# description: Conduct a paired test for two groups of data. Similar to the
#              TwoSample function.
#
# parameters:
#   formula    Formula. A symbolic representation that communicates the response
#                variable and the factor of interest, as well as the grouping
#                structure.
#   data       DataFrame. Optional dataframe, list or environment containing the
#                variables in the formula.
#   method     String. If "t" (default), a paired-t test will be conducted
#                assuming that normality is reasonable. If "Z", a large-sample 
#                approximation based on the CLT will be used. If "boot", then
#                a nonparametric bootstrap will be performed.
#   m          Scalar. If method="boot", then this is the number of
#                bootstrap replicates to consider (default=5000). Otherwise, 
#                it is ignored.
#   alt        String. If "ne" (default), a two-sided test is performed, if
#                "gt", then the alternative mu[1] > mu[2] is tested, if "lt" the
#                alternative mu[1] < mu[2] is tested.
#   conflevel  Scalar. A number between 0 and 1 (default=0.95) specifying the
#                confidence level for a 2-sided confidence interval.
#   plots      Boolean. If TRUE (default), plots corresponding to assessing
#                assumptions are constructed. If FALSE, no plots are
#                constructed. Note: run charts assume data is given in the order
#                it was observed.
PairedSample <- function(formula, data, method=c("t", "Z", "boot"), m=5000, 
                         alt=c("ne", "gt", "lt"), conflevel=0.95, plots=TRUE){
  
  require(dplyr, quietly=TRUE, warn.conflicts=FALSE)
  require(ggplot2, quietly=TRUE, warn.conflicts=FALSE)
  
  method <- match.arg(method)
  alt <- match.arg(alt)
  
  # Construct Data
  #  Place data into a dataframe for easy use.
  if(!missing(data)){    
    dat <- with(data, {
      data.frame(y=get(as.character(formula[[2]])),
                 x=factor(get(as.character(formula[[3]])[2])),
                 subj=get(as.character(formula[[3]])[3]))
    })
  }
  
  if(missing(data)){
    dat <- data.frame(y=get(as.character(formula[[2]])),
                      x=factor(get(as.character(formula[[3]]))),
                      subj=subject)
  }
  
  dat <- dat %>% group_by(subj) %>% 
    mutate(diff=y[x==unique(x)[2]] - y[x==unique(x)[1]]) %>%
    ungroup() %>%
    distinct(subj, .keep_all=TRUE)
  
  dat <- select(dat, diff)
  
  
  # Construct Estimates
  #  Need the mean and standard deviations of each group.
  stats <- summarise(dat, mean=mean(diff), vars=var(diff), n=n())
  
  
  # Construct Statistic
  t.stat <- with(stats,{
    mean/sqrt(vars/n)
  })
  
  
  # If t-Distribution Used
  if(method=="t"){
    # Compute P-value
    if(alt=="ne") pval <- 2*pt(abs(t.stat), df=stats$n-1, lower.tail=FALSE)
    if(alt=="gt") pval <- pt(t.stat, df=stats$n-1, lower.tail=FALSE)
    if(alt=="lt") pval <- pt(t.stat, df=stats$n-1, lower.tail=TRUE)
    
    # Compute CI
    ci <- with(stats,{
      mean + c(-1,1)*qt(0.5*(1+conflevel), df=n-1)*sqrt(vars/n)
    })
  }
  
  # If Normal Distribution Used
  if(method=="Z"){
    # Compute P-value
    if(alt=="ne") pval <- 2*pnorm(abs(t.stat), lower.tail=FALSE)
    if(alt=="gt") pval <- pnorm(t.stat, lower.tail=FALSE)
    if(alt=="lt") pval <- pnorm(t.stat, lower.tail=TRUE)
    
    # Compute CI
    ci <- with(stats, {
      mean + c(-1,1)*qnorm(0.5*(1+conflevel))*sqrt(vars/n)
    })
  }
  
  # If Bootstrapping
  if(method=="boot"){
    boot1 <- matrix(sample(dat$diff-mean(dat$diff), 
                           size=nrow(dat)*m, replace=TRUE),
                    nrow=m, ncol=nrow(dat))
    
    boot1 <- data.frame(xbar=apply(boot1, 1, mean),
                        n=nrow(dat),
                        s2=apply(boot1, 1, var))
    
    
    # Compute P-value
    boot.t <- boot1$xbar/sqrt(boot1$s2/boot1$n)
    if(alt=="ne") pval <- mean(abs(boot.t) >= abs(t.stat))
    if(alt=="gt") pval <- mean(t.stat >= boot.t)
    if(alt=="lt") pval <- mean(t.stat <= boot.t)
    
    # Compute CI
    ci <- quantile(boot1$xbar + stats$mean, 
                   probs=c(0.5*(1-conflevel), 0.5*(1+conflevel)))
    names(ci) <- NULL
  }
  
  
  # Construct Plots
  if(plots){
    dat$order <- seq(nrow(dat))
    avg <- mean(dat$diff)
    p1 <- ggplot(data=dat, mapping=aes(x=order, y=diff)) +
      geom_hline(yintercept=avg, colour="grey", linetype=2) +
      geom_line() +
      geom_point() +
      labs(x="Order of Observations", y="Paired Differences")
    print(p1)
    
    if(method=="t"){
      dat <- transform(dat, quants=qnorm(ppoints(diff))[order(order(diff))])
      std <- sd(dat$diff)
      p2 <- ggplot(data=dat, mapping=aes(x=quants, y=diff)) +
        geom_point() +
        geom_abline(slope=std, intercept=avg) +
        labs(x="Theoretical Quantiles", y="Sample Quantiles")
      print(p2)
    }
    
    if(method=="boot"){
      boot.t <- data.frame(x=boot.t)
      p3 <- ggplot(data=boot.t, mapping=aes(x=x)) +
        geom_density() +
        geom_vline(xintercept=t.stat, colour="red", size=1.5) +
        labs(x="Bootstrap Test Statistics", y="")
      print(p3)
    }
  } 
  
  
  # Print/Return Results
  out <- c("Mean Diff"=stats$mean, "Test Stat"=t.stat, "P-Value"=pval,
           "CI LCL"=ci[1], "CI UCL"=ci[2], "Conf Level"=conflevel)
  if(method=="t") out <- c(out, "df"=stats$n-1)
  if(method=="boot") out <- c(out, "m"=m)
  attr(out, "Alternative") <- alt
  
  class(out) <- "PairedSample"
  return(out)
}





################################################################################
# function: ParamTest
# description: Conducts Wald-type tests for hypotheses about the parameters of
#              a regression model such that the hypotheses can be placed into
#              the general linear hypothesis framework:
#                  H0: Kb = m
#
# parameters:
#   b          Vector. The coefficients (estimates of the parameters) from the
#                model fit.
#   Sigma      Matrix. The variance-covariance matrix of the parameter
#                estimates.
#   K          Matrix. An r-by-p matrix that defines the linear contrasts for
#                the null hypothesis (default=identity matrix).
#   m          Vector. The values of the linear contrasts under the null 
#                hypothesis (default=0 vector).
#   conflevel  Scalar. Value between 0 and 1 (default=0.95) specifing the 
#                confidence level for confidence intervals of individual
#                contrasts. Note: no adjustment is made for multiple testing.
#   transform  String. This character string is code specifying the form of an
#                algebraic function for constructing intervals for
#                transformations of the estimates (default="x" corresponding to
#                identity transformation). Note: transformations are only for
#                estimates and confidence limits, and do not correspond to the
#                hypothesis itself. That is, m is not transformed.
#   paramboot  Boolean. If TRUE, then transformed confidence intervals are
#                based on a parametric bootstrap which will provide more robust
#                estimates of the confidence limits. If FALSE (default), a
#                naive (though often valid) interval is constructed by
#                transforming the endpoints. Parametric bootstrap is based on
#                5000 resamples.
#   df         Scalar. If specified, then F-tests are performed using this 
#                value for the degrees of freedom for error. If NULL (default),
#                then large sample theory is used to construct the tests.
ParamTest <- function(b, Sigma, K=diag(length(b)), m=rep(0, nrow(K)), 
                      conflevel=0.95, transform="x", paramboot=FALSE, df=NULL){
  # Ensure b is numeric vector
  b <- drop(as.matrix(b))
  K <- as.matrix(K)
  
  # Construct the estimates and variance-covariance of Kb
  Kb <- K %*% b
  Sig <- as.matrix(K %*% tcrossprod(Sigma,K))
  
  
  # Compute test statistics for individual tests
  ind.test <- ((Kb-m)*(Kb-m))/diag(Sig)
  
  
  # If normality is assumed in a linear model, compute p-values using an
  # F-distribution instead of chi-squared. Otherwise, use large-sample theory.
  if(!is.null(df)){
    ind.pval <- pf(ind.test, df1=1, df2=df, lower.tail=FALSE)
  }
  
  else if(is.null(df)){
    ind.pval <- pchisq(ind.test, df=1, lower.tail=FALSE)
  }
  
  
  # If more than one contrast was specified, also perform the joint test
  joint.test <- joint.pval <- NULL
  if(nrow(K)>1){
    # Compute the inverse of the variance-covariance matrix
    Inv <- chol2inv(chol(Sig))
    
    # Compute test statistic for joint test
    joint.test <- crossprod((Kb-m), Inv %*% (Kb-m))
    
    # If normality is assumed in a linear model, compute p-values using an
    # F-distribution instead of chi-squared. Otherwise, use large-sample theory.
    if(!is.null(df)){
      joint.test <- joint.test/nrow(K)
      joint.pval <- pf(joint.test, df1=nrow(K), df2=df, lower.tail=FALSE)
    }
    
    else if(is.null(df)){
      joint.pval <- pchisq(joint.test, df=nrow(K), lower.tail=FALSE)
    }
  }
  
  
  # Construct confidence bounds for individual estimates.
  # Use different distribution depending on whether normality is assumed.
  if(!is.null(df)){
    lcl <- Kb - qt(0.5*(1+conflevel), df=df)*sqrt(diag(Sig))
    ucl <- Kb + qt(0.5*(1+conflevel), df=df)*sqrt(diag(Sig))
  }
  
  else if(is.null(df)){
    lcl <- Kb - qnorm(0.5*(1+conflevel))*sqrt(diag(Sig))
    ucl <- Kb + qnorm(0.5*(1+conflevel))*sqrt(diag(Sig))
  }
  
  
  # Perform transformations, if applicable
  if(transform!="x"){
    # construct function for performing transformation
    f <- function(x) eval(parse(text=transform))
    
    # construct estimates
    Kb <- f(Kb)
    
    # transform confidence bands
    if(paramboot){
      # Generate replicates based on parametric bootstrap of asymptotic
      # distribution.
      boot <- rmvnorm(n=5000, mu=Kb, Sigma=Sig)
      
      # Transform
      boot <- f(boot)
      
      # Construce lower/upper limits of CI
      lcl <- apply(boot, 1, quantile, probs=c(0.5*(1-conflevel)))
      ucl <- apply(boot, 1, quantile, probs=c(0.5*(1+conflevel)))
    }
    
    else if(!paramboot){
      lcl <- f(lcl)
      ucl <- f(ucl)
    }
  }
  
  # Adjust CI limits, if necessary
  if(any(lcl >= ucl)){
    ci <- cbind(pmin(lcl,ucl), pmax(lcl,ucl))
    lcl <- ci[,1]
    ucl <- ci[,2]
  }
  
  # Return all components necessary for constructing this test
  out <- list(K=K,m=m,est=Kb,lcl=lcl,ucl=ucl,ind.test=ind.test,
              ind.pval=ind.pval,joint.test=joint.test,
              joint.pval=joint.pval,
              conflevel=conflevel,transform=transform,
              paramboot=paramboot,df=df)
  class(out) <- "ParamTest"
  
  return(out)
}




################################################################################
# function: PredictResponse
# description: Allow the prediction of the response from a fitted object.
#
# parameters:
#   fit        Fit object. The result of a model fit.
#   newdat     Dataframe. The dataframe containing all covariates used in the
#                model fit. A prediction is created for each row of covariate
#                values specified in newdat.
#   conflevel  Scalar. A value between 0 and 1 (default=0.95) that specifies the
#                confidence level for confidence intervals about the mean
#                response.
PredictResponse <- function(fit, newdat, conflevel=0.95){
  # Linear Models
  #  Using the built-in prediction capability of rms package.
  if(class(fit)[1]=="ols"){
    # Update fit, if necessary
    if(is.null(fit$x)) fit <- update(fit, x=TRUE)
    
    # Make predictions
    pred <- predict(fit, newdat, conf.int=conflevel)
    
    # Add predictions to existing dataset
    dat.pred <- data.frame(PredictedValue=pred$linear.predictors,
                           PredValLCL=pred$lower,
                           PredValUCL=pred$upper)
    
    dat.pred <- cbind(newdat, dat.pred)
  }
  
  if(class(fit)[1]=="lm"){
    # Make predictions
    pred <- predict(fit, newdat, interval="confidence", level=conflevel)
    pred <- as.data.frame(pred)
    
    # Add predictions to existing dataset
    dat.pred <- data.frame(PredictedValue=pred$fit,
                           PredValLCL=pred$lwr,
                           PredValUCL=pred$upr)
    
    dat.pred <- cbind(newdat, dat.pred)
  }
  
  
  # Logistic Models
  #  Using the built-in prediction capability of the rms package.
  if(class(fit)[1]=="lrm"){
    # Update fit, if necessary
    if(is.null(fit$x)) fit <- update(fit, x=TRUE)
    
    # Make predictions
    pred <- predict(fit, newdat, type="lp", se.fit=TRUE)
    
    # Convert predictions
    dat.pred <- with(pred,{
      yhat <- plogis(linear.predictors)
      lcl <- plogis(linear.predictors - qnorm(0.5*(1+conflevel))*se.fit)
      ucl <- plogis(linear.predictors + qnorm(0.5*(1+conflevel))*se.fit)
      
      return(data.frame(PredictedValue=yhat,
                        PredValLCL=lcl,
                        PredValUCL=ucl))
    })
    
    dat.pred <- cbind(newdat, dat.pred)
  }
  
  if(class(fit)[1]=="glm"){
    # Make predictions
    pred <- predict(fit, newdat, type="link", se.fit=TRUE)
    pred <- as.data.frame(pred)
    
    # Convert predictions
    pred <- mutate(pred,
                   lwr=fit - qnorm(0.5*(1+conflevel))*se.fit,
                   upr=fit + qnorm(0.5*(1+conflevel))*se.fit)
    
    dat.pred <- data.frame(PredictedValue=fit$family$linkinv(pred$fit),
                           PredValLCL=fit$family$linkinv(pred$lwr),
                           PredValUCL=fit$family$linkinv(pred$upr))
    
    dat.pred <- cbind(newdat, dat.pred)
  }
  
  
  # Nonlinear Models
  #  The confidence limits have to be created from scratch since predict does
  #  not provide these by default.
  if(class(fit)[1] %in% c("gnls","nls")){
    # Obtain predictions
    pred <- predict(fit, newdat)
    
    # Obtain CI for predictions using nls to get gradient
    temp <- cbind(1,newdat)
    colnames(temp)[1] <- paste(formula(fit)[[2]])
    
    suppressWarnings(temp.fit <- nls(formula(fit), data=temp, start=coef(fit),
                                     control=list(maxiter=0, warnOnly=TRUE)))
    
    # Construct variance of response
    Sigma <- temp.fit$m$gradient() %*% vcov(fit) %*% t(temp.fit$m$gradient())
    if(nrow(Sigma)>1) Sigma <- diag(Sigma)
    
    # Construct CI
    dat.pred <- data.frame(PredictedValue=pred,
                           PredValLCL=pred-qnorm(0.5*(1+conflevel))*sqrt(Sigma),
                           PredValUCL=pred+qnorm(0.5*(1+conflevel))*sqrt(Sigma))
    
    dat.pred <- cbind(newdat, dat.pred)
  }
  
  
  # Repeated Measures Models
  #  The confidence limits have to be created from scratch since predict does
  #  not provide these by default.
  if(class(fit)[1]=="geeglm"){
    # Add fake response to new data
    temp.dat <- newdat
    temp.dat[,as.character(formula(fit)[[2]])] <- 1
    
    # Refit to obtain X-matrix
    suppressWarnings(temp.fit <- glm(formula(fit), data=temp.dat, x=TRUE,
                                     control=list(maxit=1L)))
    
    Xmat <- temp.fit$x
    
    # Create predictions
    lin.pred <- Xmat %*% coef(fit)
    pred <- fit$family$linkinv(lin.pred)
    
    # Obtain standard errors
    se <- sqrt(diag(Xmat %*% (fit$geese$vbeta) %*% t(Xmat)))
    
    # Construct CI
    ci <- cbind(fit$family$linkinv(lin.pred - qnorm(0.5*(1+conflevel))*se),
                fit$family$linkinv(lin.pred + qnorm(0.5*(1+conflevel))*se))
    
    lcl <- pmin(ci[,1], ci[,2])
    ucl <- pmax(ci[,1], ci[,2])
    
    dat.pred <- data.frame(PredictedValue=pred,
                           PredValLCL=lcl,
                           PredValUCL=ucl)
    
    dat.pred <- cbind(newdat, dat.pred)
  }
  
  # Return adjusted dataset
  return(dat.pred)
}



################################################################################
# function: print.LifeTable
# description: Method for printing results of a LifeTable object.  This will not
#              be used by students directly.
#
# parameters:
#  x           LifeTable object.  The result of a LifeTable call.
#  ...         Other parameters to be passed to function. Currently ignored.
print.LifeTable <- function(x, ...){
  x[,-c(1:3)] <- round(x[,-c(1:3)], 3)
  print.data.frame(x, row.names=FALSE)
}




################################################################################
# function: print.PairedSample
# description: Method for printing the results of a PairedSample object. This
#              will not be used by students directly.
#
# parameters:
#   x          PairedSample Object. The result of a paired-sample test.
#   ...        Other parameters to be passed to function. Currently ignored.
print.PairedSample <- function(x, ...){
  # Print Method
  method <- ifelse(!is.na(x["df"]), "t-Distribution", 
                   ifelse(!is.na(x["m"]), "Bootstrapping", 
                          "Normal-Distribution"))
  
  cat("Paired Sample Procedure using ", method, ":\n", sep="")
  
  # Summarize Output
  cat("  Mean Difference:", x["Mean Diff"], "\n")
  cat("  ", 100*x["Conf Level"], "% CI: (", round(x["CI LCL"],3), ", ",
      round(x["CI UCL"],3), ")\n", sep="")
  cat("  Alternative Hypothesis:", switch(attr(x,"Alternative"), 
                                          ne="mu1 Not Equal mu2",
                                          gt="mu1 Greater Than mu2",
                                          lt="mu1 Less Than mu2"), "\n")
  cat("  P-Value:", round(x["P-Value"], 4), "\n")
  
  if(method=="t-Distribution"){
    cat("  Degrees of Freedom:", x["df"], "\n")
  }
  
  else if(method=="Bootstrapping"){
    cat("  Number of Bootstrap Replicates:", x["m"], "\n")
  }
}




################################################################################
# function: print.ParamTests
# description: Method for printing the results of a ParamTest object. This will
#              not be used by students directly.
#
# parameters:
#   x          ParamTest Object. The result of a parameter test using a general
#                linear hypothesis framework via the ParamTest function.
#   ...        Other parameters to be passed to function. Currently ignored.
print.ParamTest <- function(x, ...){
  # Print key information regarding the tests
  cat("K Matrix:\n")
  print(x$K)
  
  cat("\n\nm Vector:\n")
  print(x$m)
  
  
  if(x$transform!="x"){
    cat("\n\nNote: the following transformation applied to estimates.\n")
    cat("f(x) =",x$transform,"\n")
    
    if(x$paramboot){
      cat("\nNote: the confidence limits are based on parametric",
          "bootstrapping.\n")
    }
  }
  
  if(!is.null(x$df)){
    cat("\nNote: distribution of error term assumed normal.\n")
  }
  
  # Create a table that summarizes the estimates and confidence intervals
  #   for the individual tests.
  cat("\nEstimates and Individual Tests for H0: Kb = m:\n\n")
  df <- data.frame(x$est, x$lcl, x$ucl, x$ind.test, x$ind.pval)
  names(df) <- c("Estimate",
                 paste(c("Lower ","Upper "),100*x$conflevel,
                       "% Limit",sep=""),
                 "Test Statistic",
                 "P-value")
  
  print(df,row.names=FALSE,digits=3)
  
  # Print out joint tests
  if(!is.null(x$joint.pval)){
    cat("\nTest statistic for Joint Test of H0: Kb = m is",
        round(x$joint.test,2),"with a p-value of",round(x$joint.pval,3))
  }
}




################################################################################
# function: print.TwoSample
# description: Method for printing the results of a TwoSample object. This will
#              not be used by students directly.
#
# parameters:
#   x          TwoSample Object. The result of a two-sample test.
#   ...        Other parameters to be passed to function. Currently ignored.
print.TwoSample <- function(x, ...){
  # Print Method
  method <- ifelse(!is.na(x["df"]), "t-Distribution", 
                   ifelse(!is.na(x["m"]), "Bootstrapping", 
                          "Normal-Distribution"))
  
  cat("Two Sample Procedure using ", method, ":\n", sep="")
  
  # Summarize Output
  cat("  Difference in Means:", x["Diff"], "\n")
  cat("  ", 100*x["Conf Level"], "% CI: (", round(x["CI LCL"],3), ", ",
      round(x["CI UCL"],3), ")\n", sep="")
  cat("  Alternative Hypothesis:", switch(attr(x,"Alternative"), 
                                          ne="mu1 Not Equal mu2",
                                          gt="mu1 Greater Than mu2",
                                          lt="mu1 Less Than mu2"), "\n")
  cat("  P-Value:", round(x["P-Value"], 4), "\n")
  
  if(method=="t-Distribution"){
    cat("  Degrees of Freedom:", x["df"], "\n")
  }
  
  else if(method=="Bootstrapping"){
    cat("  Number of Bootstrap Replicates:", x["m"], "\n")
  }
}




################################################################################
# function: ResidualPlots
# description: Create common residual plots for various types of models.
#
# parameters:
#   fit        Fit Object. The full model fit which is to be assessed.
#   grid       Boolean. If TRUE (default), the plots are placed in a compact
#                grid. If FALSE, each plot is returned individually.
#   smooth     Boolean. If TRUE, a loess smoother is placed on all scatterplots
#                for trend assessment. If FALSE (default), a smoother is not
#                included.
ResidualPlots <- function(fit, grid=TRUE, smooth=FALSE){
  # Linear Model
  #   Create the usual 4-in-1 plot from Minitab which has a qq-plot of
  #   the residuals, a residual versus fitted values, a histogram of the
  #   residuals and a run chart.
  if(class(fit)[1]=="ols"){
    # Obtain predicted values and residuals and add quantiles
    #   Adjust for any weighting if necessary.
    resi <- fit$residuals
    resi <- resi / sqrt(sum(resi*resi)/fit$df.residual)
    
    if(!is.null(fit$weights)){
      resi <- sqrt(fit$weights)*resi
    }
    
    plot.dat <- data.frame(yhat=fit$fitted.values,
                           resi=resi,
                           lag=c(resi[-1],NA),
                           quants=qnorm(ppoints(resi)),
                           order=seq(length(resi)))
    
    # Compute slope and intercept of theoretical line
    theory.int <- mean(plot.dat$resi)
    theory.slope <- sd(plot.dat$resi)
    
    # Construct QQ-plot
    p1 <- ggplot(data=plot.dat,mapping=aes(y=sort(resi),x=quants)) +
      geom_abline(slope=theory.slope,intercept=theory.int) + 
      geom_point() +
      labs(x="Theoretical Quantiles",y="Sample Quantiles")
    
    # Construct residuals vs. fits
    p2 <- ggplot(data=plot.dat,mapping=aes(y=resi,x=yhat)) +
      geom_hline(yintercept=0, linetype=2, colour="grey") +
      geom_point() +
      labs(x="Predicted Values",y="Standardized Residuals")
    
    # Construct a density plot
    p3 <- ggplot(data=plot.dat,mapping=aes(x=resi)) +
      geom_density(alpha=0.35) +
      labs(y="Density",x="Standardized Residuals")
    
    # Construct a run chart
    p4 <- ggplot(data=plot.dat,mapping=aes(y=resi,x=order)) +
      geom_hline(yintercept=0, linetype=2, colour="grey") +
      geom_point() +
      geom_line() +
      labs(x="Order of Observations",y="Standardized Residuals")
    
    # Lag plot of the residuals
    p5 <- ggplot(data=plot.dat,mapping=aes(y=lag,x=resi)) +
      geom_hline(yintercept=0, linetype=2, colour="grey") +
      geom_point() +
      labs(x="Lag-1 Std. Residuals",y="Standardized Residuals")
    
    # Adjust if smoothing requested
    if(smooth){
      p2 <- p2 + geom_smooth(method="loess")
      p5 <- p5 + geom_smooth(method="loess")
    }
    
    if(grid){
      return(suppressWarnings(do.call(grid.arrange,list(p1,p2,p3,p4,p5))))
    }
    
    if(!grid){
      for(plot.p in list(p1,p2,p3,p4,p5)) suppressWarnings(print(plot.p))
    }
  }
  
  
  # Nonlinear Models
  if(class(fit)[1] %in% c("nls","gnls")){
    # Obtain predicted values and residuals and add quantiles.
    #  Adjust for any weighting, if necessary.
    if(class(fit)[1]=="gnls"){
      resi <- fit$residuals
      resi <- resi / sqrt(sum(resi*resi)/(fit$dims$N - fit$dims$p))
      
      if(!is.null(fit$modelStruct$varStruct)){
        resi <- sqrt(attr(fit$modelStruct$varStruct, "weights"))*resi
      }
      
      fitted <- fit$fitted
    }
    
    if(class(fit)[1]=="nls"){
      resi <- fit$m$resid()
      n <- length(resi)
      p <- nrow(summary(fit)$coefficients)
      resi <- resi / sqrt(sum(resi*resi)/(n-p))
      
      if(!is.null(fit$weights)){
        resi <- sqrt(fit$weights)*resi
      }
      
      fitted <- fit$m$fitted()
    }
    
    
    plot.dat <- data.frame(yhat=fitted,
                           resi=resi,
                           lag=c(resi[-1],NA),
                           quants=qnorm(ppoints(resi)),
                           order=seq(length(resi)))
    
    # Compute slope and intercept of theoretical line
    theory.int <- mean(plot.dat$resi)
    theory.slope <- sd(plot.dat$resi)
    
    # Construct QQ-plot
    p1 <- ggplot(data=plot.dat,mapping=aes(y=sort(resi),x=quants)) +
      geom_abline(slope=theory.slope,intercept=theory.int) + 
      geom_point() +
      labs(x="Theoretical Quantiles",y="Sample Quantiles")
    
    # Construct residuals vs. fits
    p2 <- ggplot(data=plot.dat,mapping=aes(y=resi,x=yhat)) +
      geom_hline(yintercept=0, linetype=2, colour="grey") +
      geom_point() +
      labs(x="Predicted Values",y="Standardized Residuals")
    
    # Construct a density plot
    p3 <- ggplot(data=plot.dat,mapping=aes(x=resi)) +
      geom_density(alpha=0.35) +
      labs(y="Density",x="Standardized Residuals")
    
    # Construct a "double-density" plot
    p4 <- ggplot(data=plot.dat,mapping=aes(y=resi^2,x=yhat)) +
      geom_point() +
      labs(x="Predicted Values",y="Squared Std. Residuals")
    
    # Construct a run chart
    p5 <- ggplot(data=plot.dat,mapping=aes(y=resi,x=order)) +
      geom_hline(yintercept=0, linetype=2, colour="grey") +
      geom_point() +
      geom_line() +
      labs(x="Order of Observations",y="Standardized Residuals")
    
    # Lag plot of the residuals
    p6 <- ggplot(data=plot.dat,mapping=aes(y=lag,x=resi)) +
      geom_hline(yintercept=0, linetype=2, colour="grey") +
      geom_point() +
      labs(x="Lag-1 Std. Residuals",y="Standardized Residuals")
    
    # Adjust if smoothing requested
    if(smooth){
      p2 <- p2 + geom_smooth(method="loess")
      p4 <- p4 + geom_smooth(method="loess")
      p6 <- p6 + geom_smooth(method="loess")
    }
    
    if(grid){
      return(suppressWarnings(do.call(grid.arrange,list(p1,p2,p3,p4,p5,p6))))
    }
    
    if(!grid){
      for(plot.p in list(p1,p2,p3,p4,p5,p6)) suppressWarnings(print(plot.p))
    }
  }
  
  
  # Cox Model
  #   We want to use the Cox-Snell residuals to assess the goodness of fit.
  if(class(fit)[1]=="cph"){
    # Update model, if necessary
    fit <- update(fit, x=TRUE, y=TRUE)
    
    # Obtain Cox-Snell residuals from fit
    resi <- fit$y[,2] - residuals(fit,type="martingale") 
    
    # Compute the cumulative hazard of these residuals
    Sfit <- survfit(Surv(time=resi, event=fit$y[,2])~1)
    CumHaz <- cumsum(Sfit$n.event/Sfit$n.risk)
    
    plot.dat <- data.frame(resi=Sfit$time,
                           cumhaz=CumHaz)
    
    # Construct cumulative hazard vs. residuals
    p1 <- ggplot(data=plot.dat,mapping=aes(y=cumhaz,x=resi)) +
      geom_abline(slope=1,intercept=0,colour="grey") +
      geom_point() +
      labs(x="Cox-Snell Residuals",y="Estimated Cumulative Hazard Rates")
    
    return(p1)
  }
}




################################################################################
# function: rmvnorm
# description: Generate random variates from a multivariate normal density.
#                  X ~ N[p](mu, Sigma), mu is p-by-1 and Sigma is p-by-p
#
# parameters:
#   n          Scalar. The number of replicates to generate.
#   mu         Vector. The means for each component of the resulting variate.
#   Sigma      Matrix. The covariance matrix of the resulting variate
#               (default=diag(length(mu)), independence with variance 1).
#
# returns: 
#   Matrix of dim(n,length(mu)) representing the design matrix. This matrix has
#   one attribute: Sigma, the covariance matrix of one independent observation
#   (row of the design matrix) corresponding to the Sigma supplied in the 
#   parameter list.
rmvnorm <- function(n, mu, Sigma=diag(length(mu))){
  ### Error Checks
  # All parameters of correct form.
  if(!is.numeric(n) || !is.numeric(mu) || !is.numeric(Sigma)){
    stop("All parameters must be numeric.")
  }
  
  # Sigma and mu of conformable dimensions
  Sigma <- as.matrix(Sigma)
  if(nrow(Sigma)!=ncol(Sigma) || nrow(Sigma)!=length(mu)){
    stop("'mu' and 'Sigma' are non-conformable arguments.")
  }
  
  # n must be a scalar
  n <- n[1]
  
  
  ### Generate base variables
  X <- matrix(rnorm(length(mu)*n),nrow=length(mu),ncol=n)
  
  
  ### Correct base variables
  # Use variance-covariance matrix to create correct multiplier:
  #   OUT[i,] = AX[,i] + mu, where Sigma = AA'
  A <- t(chol(Sigma))
  
  # Create correct matrix
  X <- t(A%*%X + mu)
  attr(X,"Sigma") <- Sigma
  return(X)
}




################################################################################
# function: RobustVar
# description: Compute the robust (sandwich) variance-covariance matrix for a 
#              given model fit.
#
# parameters:
#   fit        Object fit. The model which to construct the variance-covariance
#                matrix for.
#   replace    Boolean. If TRUE, the variance-covariance matrix of the object
#                fit is replaced with the robust version. This could affect how
#                some other functions interact with a model fit. In this case,
#                the full model fit is returned. If FALSE (default) only the
#                updated matrix is returned.
RobustVar <- function(fit, replace=FALSE){
  # Linear/Logistic/CoxPH Models
  #  Use built-in rms capabilities.
  if(class(fit)[1] %in% c("ols","lrm","cph")){
    # Update fit, if necessary
    if(is.null(fit$x) || is.null(fit.y)){
      fit <- update(fit, x=TRUE, y=TRUE)
    }
    
    # If not replacing, return the right matrix
    if(!replace) return(sandwich(fit))
    
    # If replacing, return updated fit
    if(replace) return(robcov(fit))
  }
  
  if(class(fit)[1]=="lm"){
    # Update model, if necessary
    fit <- update(fit, x=TRUE)
    
    # Determine if weights
    wgts <- if(is.null(weights(fit))){
      rep(1, length(residuals(fit)))
    } else {
      weights(fit)
    }
    
    # Form outer layer of sandwich
    XUX.inv <- vcov(fit)/summary(fit)$sigma^2
    
    # Form the second layer of the sandwich estimator
    XU <- fit$x*sqrt(wgts)
    
    # Form sandwich estimator and give it same attributes as current
    # variance-covariance matrix.
    out <- structure(XUX.inv %*% crossprod(XU,(fit$residuals^2*XU)) %*%
                       XUX.inv, dimnames=attr(vcov(fit),"dimnames"))
    
    # If not replacing
    if(!replace) return(out)
    
    # If replacing
    if(replace){
      warning("Cannot currently replace for linear models fit with lm.")
      return(fit)
    }
  }
  
  
  # Repeated Measures
  #  Cannot currently address.
  if(class(fit)[1]=="gls"){
    stop("Error: Cannot obtain robust variance matrix for repeated measures.")
  }
  
  
  # Nonlinear Models
  #  Use nls() to re-fit models and obtain corrections. Will not perform if the
  #  model contains repeated measures.
  if(class(fit)[1]=="gnls"){
    # If repeated measures, cannot currently address.
    if(!is.null(fit$modelStruct$corStruct)){
      stop("Error: Cannot obtain robust variance matrix for repeated measures.")
    }
    
    # Determine the weights used from final iteration, if any.
    wgts <- rep(1, length(fit$fitted))
    if(!is.null(fit$modelStruct$varStruct)){
      wgts <- attr(fit$modelStruct$varStruct,"weights")
    }
    
    # Fit with nls to access gradient, which must be done via text since the
    # data is not stored with the fit.
    dat.text <- ifelse(is.null(fit$call["data"]), "", 
                       paste(",data=", paste(fit$call["data"])))
    sub.text <- ifelse(is.null(fit$call["subset"]), "",
                       paste(",subset=", paste(fit$call["subset"])))
    
    temp <- paste("nls(", paste(fit$call["model"]), dat.text, sub.text,
                  ",weights=wgts",
                  ",control=list(maxiter=0, warnOnly=TRUE))", sep="")
    suppressWarnings(temp.fit <- try(eval(parse(text=temp)), silent=TRUE))
    
    if(class(temp.fit)[1]!="nls"){
      stop("Error: Gradient could not be obtained.")
    }
    
    # Form outer layer of sandwich
    XUX.inv <- vcov(fit)/fit$sigma^2
    
    # Form the second layer of the sandwich estimator
    XU <- temp.fit$m$gradient()*sqrt(weights(temp.fit))
    
    # Form sandwich estimator and give it same attributes as current
    # variance-covariance matrix.
    out <- structure(XUX.inv %*% crossprod(XU,(fit$residuals^2*XU)) %*%
                       XUX.inv, dimnames=attr(fit$varBeta,"dimnames"))
    
    # If not replacing
    if(!replace) return(out)
    
    # If replacing
    if(replace){
      fit$varBeta <- out
      return(fit)
    }
  }
  
  if(class(fit)[1]=="nls"){
    # Determine if weights
    wgts <- if(is.null(weights(fit))){
      rep(1, length(residuals(fit)))
    } else {
      weights(fit)
    }
    
    # Form outer layer of sandwich
    XUX.inv <- vcov(fit)/summary(fit)$sigma^2
    
    # Form the second layer of the sandwich estimator
    XU <- fit$m$gradient()*sqrt(wgts)
    
    # Form sandwich estimator and give it same attributes as current
    # variance-covariance matrix.
    out <- structure(XUX.inv %*% crossprod(XU,(residuals(fit)^2*XU)) %*%
                       XUX.inv, dimnames=attr(vcov(fit),"dimnames"))
    
    # If not replacing
    if(!replace) return(out)
    
    # If replacing
    if(replace){
      warning("Cannot currently replace for nonlinear models fit with nls.")
      return(fit)
    }
  }
}




################################################################################
# fct: round.data.frame
# description: Dataframe method for round() function that ignores non-numeric
#              variables in the dataset.
#
# parameters:
#   df         DataFrame. The dataset to round.
#   digits     Scalar. The number of decimal places to which the variables are
#                rounded (default=0, see 'round' for more details).
round.data.frame <- function(df, digits=0){
  data.frame(lapply(df, function(y) if(is.numeric(y)) round(y, digits) else y))
}




################################################################################
# fct: stat_halfnorm
# description: Make a stat for ggplot2 that makes the halfnormal plot.
StatHalfnorm <- ggproto("StatHalfnorm", Stat,
                        compute_group = function(data, scales, na.rm=FALSE){
                          # sort data
                          sample <- sort(data$sample)
                          
                          # obtain length
                          n <- length(sample)
                          
                          # determine quanitles
                          halfnorm <- qnorm((n + seq(n))/(2*n + 1))
                          
                          data.frame(sample, halfnorm)
                        },
                        required_aes = "sample",
                        default_aes = aes(x=..halfnorm.., y=..sample..))

stat_halfnorm <- function(mapping=NULL, data=NULL, geom="point",
                          position="identity", na.rm=FALSE, show.legend=NA,
                          inherit.aes=TRUE, ...){
  ggplot2::layer(data=data, mapping=mapping, stat=StatHalfnorm, geom=geom,
                 position=position, show.legend=show.legend,
                 inherit.aes=inherit.aes, params=list(na.rm=na.rm, ...))
}



################################################################################
# fct: stat_qqline
# description: Make a stat for ggplot2 that adds a best-fit line to the 
#              quantile plot to better see the linearity.
StatQqline <- ggproto("StatQqline", Stat,
                      compute_group = function(data, scales, quantiles=NULL, 
                                               distribution=stats::qnorm, dparams=list(), 
                                               na.rm=FALSE){
                        # replicate that from StatQq
                        sample <- sort(data$sample)
                        n <- length(sample)
                        if(is.null(quantiles)){
                          quantiles <- stats::ppoints(n)
                        } else {
                          stopifnot(length(quantiles)==n)
                        }
                        
                        theoretical <- do.call(distribution, c(list(p=quote(quantiles)), dparams))
                        
                        # compute best fit
                        fit <- lm(sample ~ theoretical)
                        intercept <- coef(fit)[1]
                        slope <- coef(fit)[2]
                        
                        data.frame(intercept, slope)
                      },
                      required_aes = "sample")

stat_qqline <- function(mapping=NULL, data=NULL, geom="abline",
                        position="identity", distribution=stats::qnorm,
                        dparams=list(), na.rm=FALSE, show.legend=NA,
                        inherit.aes=TRUE, ...){
  ggplot2::layer(data=data, mapping=mapping, stat=StatQqline, geom=geom,
                 position=position, show.legend=show.legend, 
                 inherit.aes=inherit.aes, 
                 params=list(distribution=distribution, dparams=dparams, 
                             na.rm=na.rm, ...))
}




################################################################################
# function: TwoSample
# description: Compare two independent samples without assuming the variance of
#              the response is constant across the two groups.
#
# parameters:
#   formula    Formula. A symbolic representation that communicates the response
#                variable and the grouping variable.
#   data       DataFrame. Optional dataframe, list or environment containing the
#                variables in the formula.
#   method     String. If "t" (default), a two-sample-t test will be conducted
#                assuming that normality is reasonable. If "Z", a large-sample 
#                approximation based on the CLT will be used. If "boot", then
#                a nonparametric bootstrap will be performed.
#   m          Scalar. If method="boot", then this is the number of
#                bootstrap replicates to consider (default=5000). Otherwise, 
#                it is ignored.
#   alt        String. If "ne" (default), a two-sided test is performed, if
#                "gt", then the alternative mu[1] > mu[2] is tested, if "lt" the
#                alternative mu[1] < mu[2] is tested.
#   conflevel  Scalar. A number between 0 and 1 (default=0.95) specifying the
#                confidence level for a 2-sided confidence interval.
#   plots      Boolean. If TRUE (default), plots corresponding to assessing
#                assumptions are constructed. If FALSE, no plots are
#                constructed. Note: run charts assume data is given in the order
#                it was observed.
TwoSample <- function(formula, data, method=c("t", "Z", "boot"), m=5000,
                      alt=c("ne", "gt", "lt"), conflevel=0.95, plots=TRUE){
  
  require(dplyr, quietly=TRUE, warn.conflicts=FALSE)
  require(ggplot2, quietly=TRUE, warn.conflicts=FALSE)
  
  method <- match.arg(method)
  alt <- match.arg(alt)
  
  # Construct Data
  #  Place data into a dataframe for easy use.
  if(!missing(data)){
    dat <- with(data, {
      data.frame(y=get(as.character(formula[[2]])),
                 x=factor(get(as.character(formula[[3]]))))
    })
  }
  
  if(missing(data)){
    dat <- data.frame(y=get(as.character(formula[[2]])),
                      x=factor(get(as.character(formula[[3]]))))
  }
  
  
  # Construct Estimates
  #  Need the mean and standard deviations of each group.
  stats <- dat %>% group_by(x) %>% 
    summarise(mean=mean(y), vars=var(y), n=n())
  
  
  # Construct Statistic
  t.stat <- with(stats, {
    (mean[2]-mean[1])/sqrt((vars[2]/n[2]) + (vars[1]/n[1]))
  })
  
  
  # If t-Distribution Used
  if(method=="t"){
    df <- with(stats, {
      (sum(vars/n))^2 / 
        ((vars[1]/n[1])^2/(n[1]-1) + (vars[2]/n[2])^2/(n[2]-1))
    })
    
    # Compute P-value
    if(alt=="ne") pval <- 2*pt(abs(t.stat), df=df, lower.tail=FALSE)
    if(alt=="gt") pval <- pt(t.stat, df=df, lower.tail=FALSE)
    if(alt=="lt") pval <- pt(t.stat, df=df, lower.tail=TRUE)
    
    # Compute CI
    ci <- with(stats, {
      (mean[2]-mean[1]) + 
        c(-1,1)*qt(0.5*(1+conflevel), df=df)*
        sqrt((vars[2]/n[2]) + (vars[1]/n[1]))
    })
  }
  
  # If Normal Distribution Used
  if(method=="Z"){
    # Compute P-value
    if(alt=="ne") pval <- 2*pnorm(abs(t.stat), lower.tail=FALSE)
    if(alt=="gt") pval <- pnorm(t.stat, lower.tail=FALSE)
    if(alt=="lt") pval <- pnorm(t.stat, lower.tail=TRUE)
    
    # Compute CI
    ci <- with(stats, {
      (mean[2]-mean[1]) +
        c(-1,1)*qnorm(0.5*(1+conflevel))*
        sqrt((vars[2]/n[2]) + (vars[1]/n[1]))
    })
  }
  
  # If Bootstrapping
  if(method=="boot"){
    boot1 <- dat[dat$x==unique(dat$x)[1],"y"]
    boot2 <- dat[dat$x==unique(dat$x)[2],"y"]
    
    xbar1 <- mean(boot1)
    xbar2 <- mean(boot2)
    
    boot1 <- matrix(sample(boot1, size=length(boot1)*m, replace=TRUE),
                    nrow=m, ncol=length(boot1))
    boot2 <- matrix(sample(boot2, size=length(boot2)*m, replace=TRUE),
                    nrow=m, ncol=length(boot2))
    
    boot1 <- data.frame(xbar=apply(boot1, 1, mean),
                        n=ncol(boot1),
                        s2=apply(boot1, 1, var))
    boot2 <- data.frame(xbar=apply(boot2, 1, mean),
                        n=ncol(boot2),
                        s2=apply(boot2, 1, var))
    
    
    # Compute P-value
    boot.t <- ((boot2$xbar-xbar2)-(boot1$xbar-xbar1))/
      sqrt(boot2$s2/boot2$n + boot1$s2/boot1$n)
    if(alt=="ne") pval <- mean(abs(boot.t) >= abs(t.stat))
    if(alt=="gt") pval <- mean(t.stat >= boot.t)
    if(alt=="lt") pval <- mean(t.stat <= boot.t)
    
    # Compute CI
    boot.diff <- (boot2$xbar - boot1$xbar)
    ci <- quantile(boot.diff, probs=c(0.5*(1-conflevel), 0.5*(1+conflevel)))
    names(ci) <- NULL
  }
  
  
  # Construct Plots
  if(plots){
    dat$order <- seq(nrow(dat))
    avg <- mean(dat$y)
    p1 <- ggplot(data=dat, mapping=aes(x=order, y=y)) +
      geom_hline(yintercept=avg, colour="grey", linetype=2) +
      geom_line() +
      geom_point() +
      labs(x="Order of Observations", y="Response")
    print(p1)
    
    if(method=="t"){
      dat <- ddply(dat, .(x), function(df){
        return(data.frame(y=df$y, 
                          quants=qnorm(ppoints(df$y))[order(order(df$y))]))
      })
      dat2 <- ddply(dat, .(x), summarise, yint=mean(y), slope=sd(y))
      p2 <- ggplot(data=dat, mapping=aes(x=quants, y=y)) +
        geom_point() +
        geom_abline(data=dat2, aes(slope=slope,intercept=yint)) +
        labs(x="Theoretical Quantiles", y="Sample Quantiles") +
        facet_wrap(~x)
      print(p2)
    }
    
    if(method=="boot"){
      boot.t <- data.frame(x=boot.t)
      p3 <- ggplot(data=boot.t, mapping=aes(x=x)) +
        geom_density() +
        geom_vline(xintercept=t.stat, colour="red", size=1.5) +
        labs(x="Bootstrap Test Statistics", y="")
      print(p3)
    }
  } 
  
  
  # Print/Return Results
  diff <- with(stats,{
    mean[2] - mean[1]})
  
  out <- c("Diff"=diff, "Test Stat"=t.stat, "P-Value"=pval,
           "CI LCL"=ci[1], "CI UCL"=ci[2], "Conf Level"=conflevel)
  if(method=="t") out <- c(out, "df"=df)
  if(method=="boot") out <- c(out, "m"=m)
  attr(out, "Alternative") <- alt
  
  class(out) <- "TwoSample"
  return(out)
}



################################################################################
# function: vcov.geeglm
# description: Method for obtaining the variance covariance matrix from a
#              geeglm object.
#
# parameters:
#   x          geeglm Object. The result of a gee fit to a glm model using the
#               geepack algorithms.
#   ...
vcov.geeglm <- function(x,...){
  return(x$geese$vbeta)
}
