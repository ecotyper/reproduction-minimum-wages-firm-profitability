###STEP-BY-STEP STATISTICAL REPRODUCTION OF "MINIMUM WAGES AND FIRM PROFITABILITY" 
###BY M. DRACA, S. MACHIN and J. VAN REENEN FROM 2011 
###IN AMERICAN ECONOMIC JOURNAL: APPLIED ECONOMICS

##########STEP 1: INSTALL AND LOAD NECESSARY PACKAGES##########

install.packages("tidyverse")
install.packages("readstata13")
install.packages("broom")
install.packages("sandwich")
install.packages("miceadds")
install.packages("car")
install.packages("carData")
install.packages("survival")
install.packages("NCmisc")
install.packages("ggplot2")
install.packages("ggthemes")
install.packages("maxLik")
install.packages("statar")
library(tidyverse) # to manipulate data
library(readstata13) # to import stata files
library(broom) # to clean regression results
library(miceadds) # to use clustered standard errors in regression
library(sandwich) # to use clustered standard errors in regression
library(car) # to perform F-Tests
library(carData) # to use car package
library(survival) # to perform interval regression
library(maxLik) # to perform interval regression
library(NCmisc) # to create percentile variables
library(ggplot2) # to reproduce figures
library(ggthemes) # to reproduce figures
library(statar) # to mirror simple stata commands in R

##########STEP 2: IMPORT DATA##########

#Importing the main dataset of the study "Financial Analysis Made Easy" as fame_data
fame_data <- read.dta13("/Users/manueldiazgarcia/Library/Mobile Documents/com~apple~CloudDocs/Studium/Master - Universität Duisburg-Essen/Studium M.A. Politikmanagement, Public Policy und öffentl. Verwaltung/2. Semester/Orientierungswissen/S Which are the best public policies? Applied Quantitative Policy Evaluation/Assignment 3 - Statistical Reproduction/Assignment3_Reproduction_Diaz Garcia/Data/main_fame.dta")
#Importing data from the Labor Force Survey as Seq98
Seq98 <- read.dta13("/Users/manueldiazgarcia/Library/Mobile Documents/com~apple~CloudDocs/Studium/Master - Universität Duisburg-Essen/Studium M.A. Politikmanagement, Public Policy und öffentl. Verwaltung/2. Semester/Orientierungswissen/S Which are the best public policies? Applied Quantitative Policy Evaluation/Assignment 3 - Statistical Reproduction/Assignment3_Reproduction_Diaz Garcia/Data/Seq98.dta")

##########STEP 3: FIGURE 1: PROPORTION OF LOW WAGE WORKERS AND AVERAGE WAGE##########

#Manipulate the data to prepare for FIGURE 1. Only a subset of variables is 
#selected from the dataset Seq98. Variables for union membership (union) and 
#gender (male) were transformed into numeric variables.
Seq98_one <- Seq98 %>%
  select(serial, serno, a3, a4, a5, c1_, d1, d2, d5, d11, empwt_nr) %>%
  rename(hours = a3,
         overtime = a4,
         tu = c1_,
         male = d1,
         age = d2,
         educ = d5) %>%
  mutate(union = ifelse(tu == "yes", 1, 0),
         male = ifelse(male == "male", 1, 0))

#To perform an interval regression two variables to determine
#these intervals have to be created. One for the lower bound (w1) of
#the interval and one for the upper bound (w2). The intervals are created
#frome the variable how much a worker gets paid weekly which was measured
#in intervals. Then these variables get log transformed (lw1, lw2) for the regression.
#Also the independent variable hours gets log transformed (lh). In addition the
#variables for age and education get transformed into factors to include
#them as categorical variables in the regression.
Seq98_two <- Seq98_one %>%
  mutate(w1 = NA,
         w1 = replace(w1, d11 == "ú51-ú80 per week", 51),
         w1 = replace(w1, d11 == "ú81-ú140 per week", 81),
         w1 = replace(w1, d11 == "ú141-ú180 per week", 141),
         w1 = replace(w1, d11 == "ú181-ú220 per week", 181),
         w1 = replace(w1, d11 == "ú221-ú260 per week", 221),
         w1 = replace(w1, d11 == "ú261-ú310 per week", 261),
         w1 = replace(w1, d11 == "ú311-ú360 per week", 311),
         w1 = replace(w1, d11 == "ú361-ú430 per week", 361),
         w1 = replace(w1, d11 == "ú431-ú540 per week", 431),
         w1 = replace(w1, d11 == "ú541-ú680 per week", 541),
         w1 = replace(w1, d11 == "ú681 or more per week", 681),
         w2 = NA,
         w2 = replace(w2, d11 == "less than ú50 per week", 50),
         w2 = replace(w2, d11 == "ú51-ú80 per week", 80),
         w2 = replace(w2, d11 == "ú81-ú140 per week", 140),
         w2 = replace(w2, d11 == "ú141-ú180 per week", 180),
         w2 = replace(w2, d11 == "ú181-ú220 per week", 220),
         w2 = replace(w2, d11 == "ú221-ú260 per week", 260),
         w2 = replace(w2, d11 == "ú261-ú310 per week", 310),
         w2 = replace(w2, d11 == "ú311-ú360 per week", 360),
         w2 = replace(w2, d11 == "ú361-ú430 per week", 430),
         w2 = replace(w2, d11 == "ú431-ú540 per week", 540),
         w2 = replace(w2, d11 == "ú541-ú680 per week", 680),
         lw1 = log(w1),
         lw2 = log(w2),
         lh = log(hours),
         lh = replace(lh, lh == "NaN", NA),
         age.f = factor(age),
         educ = replace(educ, educ == "other", NA),
         educ.f = factor(educ))

#I failed to reproduce the interval regression in R because different ways of
#running an interval regression in R did not produce the same results as with
#the provided Stata code and as in the paper.
#Since the provided Stata code can produce the interval regression, this part of 
#the reproduction to arrive at FIGURE1 was done in Stata.
#Below the attempts to reproduce the interval regression in R.

#Performing the interval regression with the variables created before. The survreg() function
#from the survival-package is used. The results of the regression differ from the results with 
#provided Stata code.
interval <- with(Seq98_two, Surv(lw1, lw2, event = rep(3, nrow(Seq98_two)), type = 'interval'))
intreg <- survreg(interval ~ age.f + male + union + educ.f + lh, data = Seq98_two, dist = "gaussian")
summary(intreg)

#Another way to perform interval regressions in R is with the intReg() function from the intReg package.
#Since the intReg package is not available anymore, the intReg() function is created here without
#loading the package.
intReg <- function(formula, start, boundaries,
                   ...,
                   contrasts = NULL, Hess = FALSE,
                   model = TRUE,
                   method = c("probit", "logistic", "cloglog", "cauchit", "model.frame"),
                   print.level=0,
                   data, subset, weights, na.action,
                   iterlim=100)
{
  ## beta     parameters for the x-s (linear model)
  ## nBeta    number of x-s (including constant)
  logit <- function(p) log(p/(1 - p))
  ## log likelihood
  loglik <- function(theta) {
    beta <- theta[iBeta]
    zeta <- theta[iBoundaries]
    sd <- theta[iStd]
    eta <- offset
    if (nBeta > 0)
      eta <- eta + drop(x %*% beta)
    Pr <- pfun((zeta[boundaryInterval + 1] - eta)/sd) - pfun((zeta[boundaryInterval] - eta)/sd)
    if(any(Pr <= 0))
      return(NA)
    names(Pr) <- NULL
    # individual probability to be in interval (zeta[y+1], zeta[y]])
    ll <- wt * log(Pr)
    (ll)
  }
  ## gradient
  gradlik <- function(theta)
  {
    jacobian <- function(theta) { ## dzeta by dtheta matrix
      k <- length(theta)
      etheta <- exp(theta)
      mat <- matrix(0 , k, k)
      mat[, 1] <- rep(1, k)
      for (i in 2:k) mat[i:k, i] <- etheta[i]
      mat
    }
    beta <- theta[iBeta]
    zeta <- theta[iBoundaries]
    sigma <- theta[iStd]
    eta <- offset
    if(nBeta > 0)
      eta <- eta + drop(x %*% beta)
    normArg1 <- (zeta[boundaryInterval +1] - eta)/sigma
    normArg2 <- (zeta[boundaryInterval] - eta)/sigma
    Pr <- pfun(normArg1) - pfun(normArg2)
    p1 <- dfun(normArg1)
    p2 <- dfun(normArg2)
    g1 <-
      # d loglik / d beta
      if(nBeta > 0)
        x * (wt*(p2 - p1)/Pr/sigma)
    else
      matrix(0, 0, nBeta)
    xx <- .polrY1*p1 - .polrY2*p2
    dg.dzeta <- xx * (wt/Pr/sigma)
    ##         g2 <- - t(xx) %*% (wt/Pr/sigma)
    ##         g2 <- t(g2) %*% jacobian(zeta)
    ##         if(all(Pr > 0))
    ##             -c(g1, g2)
    ##         else
    ##             rep(NA, nBeta+nInterval)
    s1 <- p1*normArg1
    s1[is.infinite(normArg1)] <- 0
    # if a boundary is Inf, we get 0*inf type of NaN
    s2 <- p2*normArg2
    s2[is.infinite(normArg2)] <- 0
    dg.dsd <- -(s1 - s2)*(wt/Pr/sigma)
    grad <- cbind(g1, dg.dzeta, dg.dsd)
    grad
  }
  ## ---------- main function -------------
  cl <- match.call(expand.dots = FALSE)
  method <- match.arg(method)
  if(is.matrix(eval.parent(cl$data)))
    cl$data <- as.data.frame(data)
  cl$start <- cl$Hess <- cl$method <- cl$model <- cl$boundaries <- cl$... <- cl$print.level <- NULL
  cl[[1]] <- as.name("model.frame")
  mf <- eval.parent(cl)
  if (method == "model.frame")
    return(mf)
  mt <- attr(mf, "terms")
  ## Select the correct model
  pfun <- switch(method, logistic = plogis, probit = pnorm,
                 cloglog = pgumbel, cauchit = pcauchy)
  dfun <- switch(method, logistic = dlogis, probit = dnorm,
                 cloglog = dgumbel, cauchit = dcauchy)
  x <- model.matrix(mt, mf, contrasts)
  xint <- match("(Intercept)", colnames(x), nomatch=0)
  nObs <- nrow(x)
  nBeta <- ncol(x)
  cons <- attr(x, "contrasts") # will get dropped by subsetting
  ##     if(xint > 0) {
  ##        x <- x[, -xint, drop=FALSE]
  ##     }
  ##     else
  ##         warning("an intercept is needed and assumed")
  wt <- model.weights(mf)
  if(!length(wt)) wt <- rep(1, nObs)
  offset <- model.offset(mf)
  if(length(offset) <= 1) offset <- rep(0, nObs)
  y <- model.response(mf)
  if(is.matrix(y)) {
    ## Use the intervals, given for each observation.  We have interval regression and the interval boundaries are fixed.
    ## Save boundaries as a sequence of pairs of L,B boundaries
    ordered <- FALSE
    dimnames(y) <- NULL
    lowerBound <- y[,1]
    upperBound <- y[,2]
    ## in case of interval regression, we have to construct a set of intervals and pack them correctly to the
    ## parameter vector
    intervals <- sets::set()
    for(i in 1:length(lowerBound)) {
      intervals <- intervals | c(lowerBound[i], upperBound[i])
    }
    ## Now make the set to a list to have ordering
    intervals <- as.list(intervals)
    ## Now find which interval each observation falls into
    y <- boundaryInterval <- numeric(length(lowerBound))
    # which interval observation falls to 
    # y:  in terms of ordered intervals
    # boundaryInterval         in terms of ordered boundaries
    for(i in seq(along=intervals)) {
      j <- lowerBound == intervals[[i]][1] & upperBound == intervals[[i]][2]
      boundaryInterval[j] <- 1 + 2*(i - 1)
      y[j] <- i
    }
    boundaries <- unlist(intervals)
    # boundaries as a vector (note the joint boundaries are twice
    names(boundaries) <- paste(c("L", "U"), rep(seq(along=intervals), each=2))
    nInterval <- length(boundaries) - 1
  }
  else {
    ## response is given as an ordered factor, boundaries must be given separately.
    ## Save them as a vector of boundaries, all numbers (except the first, last) represent the upper boundary of
    ## the smaller interval and the lower boundary of the upper interval at the same time.
    lev <- levels(y)
    if(length(lev) <= 2)
      stop("response must have 3 or more levels")
    if(!is.factor(y))
      stop("response must be a factor or Nx2 matrix of boundaries")
    y <- unclass(y)
    nInterval <- length(lev)
    if(missing(boundaries))
      ordered <- TRUE
    else {
      ordered <- FALSE
      intervals <- vector("list", length(boundaries) - 1)
      for(i in seq(length=length(boundaries) - 1)) {
        intervals[[i]] <- c(boundaries[i], boundaries[i+1])
      }
      boundaryInterval <- y
      # y falls inbetween boundaries 'boundaryInterval' and 'boundaryInterval + 1'
    }
    if(is.null(names(boundaries)))
      names(boundaries) <- paste("Boundary", seq(along=boundaries))
  }
  Y <- matrix(0, nObs, nInterval + 1)
  .polrY1 <- col(Y) == boundaryInterval + 1
  .polrY2 <- col(Y) == boundaryInterval 
  # .polr are markers for which interval the boundaryInterval falls to
  ## starting values
  iBeta <- seq(length=ncol(x))
  # coefficients
  iBoundaries <- nBeta + seq(along=boundaries)
  # boundaries
  iStd <- max(iBoundaries) + 1
  # standard deviation
  if(missing(start)) {
    start <- numeric(max(iBeta, iBoundaries, iStd))
    # +1 for the error variance 'sigma'
    activePar <- logical(length(start))
    if(ordered) {
      ## try logistic/probit regression on 'middle' cut
      q1 <- nInterval %/% 2
      y1 <- (y > q1)
      fit <-
        switch(method,
               "logistic"= glm.fit(x, y1, wt, family = binomial(), offset = offset),
               "probit" = glm.fit(x, y1, wt, family = binomial("probit"), offset = offset),
               ## this is deliberate, a better starting point
               "cloglog" = glm.fit(x, y1, wt, family = binomial("probit"), offset = offset),
               "cauchit" = glm.fit(x, y1, wt, family = binomial("cauchit"), offset = offset))
      if(!fit$converged)
        warning("attempt to find suitable starting values did not converge")
      coefs <- fit$coefficients
      if(any(is.na(coefs))) {
        warning("design appears to be rank-deficient, so dropping some coefs")
        keep <- names(coefs)[!is.na(coefs)]
        coefs <- coefs[keep]
        #          x <- x[, keep[-1], drop = FALSE]
        ## note: we keep the intercept
        nBeta <- ncol(x)
      }
      spacing <- logit((1:nInterval)/(nInterval+1)) # just a guess
      if(method != "logit") spacing <- spacing/1.7
      zetas <- -coefs[2] + spacing - spacing[q1]
      coefs[1] <- 0
      activePar <- c(FALSE, rep(TRUE, length(coef) - 1 + length(zetas) + 1))
      # intercept is fixed to 0
      sigma <- 1
    }
    else {
      ## not ordered: estimate OLS on interval middle points
      means <- sapply(intervals, mean)
      # we have to put a reasonable value to infinite intervals.
      # Pick the average width of the interval and use it as the meanpoint
      widths <- sapply(intervals, function(x) x[2] - x[1])
      meanWidth <- mean(widths[!is.infinite(widths)])
      negInf <- is.infinite(means) & means < 0
      if(any(negInf)) {
        # if none is true, sapply returns 'list()' and transforms means to a list
        means[negInf] <- sapply(intervals[negInf], function(x) x[2] - meanWidth)
      }
      posInf <- is.infinite(means) & means > 0
      if(any(posInf)) {
        means[posInf] <- sapply(intervals[posInf], function(x) x[1] + meanWidth)
      }
      yMean <- means[y]
      fit <- lm(yMean ~ x - 1)
      xCoefs <- coef(fit)
      if(any(is.na(xCoefs))) {
        cat("Suggested initial values:\n")
        print(xCoefs)
        stop("NA in the initial values")
      }
      names(xCoefs) <- gsub("^x", "", names(xCoefs))
      sigma <- sqrt(var(fit$residuals))
    }
    start[iBeta] <- xCoefs
    names(start)[iBeta] <- names(xCoefs)
    start[iBoundaries] <- boundaries
    names(start)[iBoundaries] <- names(boundaries)
    start[iStd] <- sigma
    names(start)[iStd] <- "sigma"
  }
  else
    if(length(start) != iStd)
      stop("'start' is of wrong length:\n",
           "The current model includes ", nBeta,
           " explanatory variables plus\n",
           length(iBeta), " interval boundaries ",
           "plus 1 disturbance standard deviation\n",
           "(", iStd, " in total).\n",
           "However, 'start' is of length ",
           length(start))
  if(print.level > 0) {
    cat("Initial values:\n")
    print(start)
  }
  if(!ordered) {
    ## Not ordered model: fix the fixed parameters
    activePar <- logical(length(start))
    activePar[iBeta] <- TRUE
    activePar[iBoundaries] <- FALSE
    activePar[iStd] <- TRUE
  }
  ##     compareDerivatives(loglik, gradlik, t0=start)
  ##     stop()
  estimation <- maxLik(loglik, gradlik, start=start,
                       method="BHHH", activePar=activePar, iterlim=iterlim, ...)
  res <- c(estimation,
           param=list(list(ordered=ordered,
                           boundaries=boundaries,
                           index=list(beta=iBeta, boundary=iBoundaries, std=iStd),
                           df=nObs - sum(activePar),
                           nObs=nObs
           )),
           call=cl,
           terms=mt,
           method=method,
           na.action=list(attr(mf, "na.action"))
  )
  class(res) <- c("intReg", class(estimation))
  return(res)
}

#Then the interval regression is run with the intReg() function but does not create
#the desired interval regression.
intreg1 <- intReg(cbind(lw1,lw2) ~ age.f + male + union + educ.f + lh, data = Seq98_two)
summary(intreg1)

#After conducting the interval regression in Stata and predicting the new variable z
#the dataset "Seq98_z" was exported and is now newly imported into R to conduct further reproduction.
#"z" is the predicted value for the log weekly payment to their workers in each firm
#based on the interval regression.
Seq98_z <- read.dta13("/Users/manueldiazgarcia/Library/Mobile Documents/com~apple~CloudDocs/Studium/Master - Universität Duisburg-Essen/Studium M.A. Politikmanagement, Public Policy und öffentl. Verwaltung/2. Semester/Orientierungswissen/S Which are the best public policies? Applied Quantitative Policy Evaluation/Assignment 3 - Statistical Reproduction/Assignment3_Reproduction_Diaz Garcia/Data/Seq98_z.dta")
z <- Seq98_z["z"]
Seq98_z_new <- cbind(Seq98_two, z)

#Preparing the data in three steps to create the data frame Seq98_collapse_final which 
#contains the data to reproduce FIGURE 1. In the first step the proportion of minimum
#wage workers in a firm is calculated from the data (propmin). The second step replicates
#further data manipulation from the provided Stata code to assign firms to different categories
#based on their average wages. The third step calculates 20 quantiles of the average wage 
#distribution and assigns each firm to one of those quantiles (band5).
#After that FIGURE1 is reproduced with the variables propmin and band5.
Seq98_z_full <- Seq98_z_new %>%
  mutate(ez = exp(z),
         hourly = ez/hours,
         hh = ifelse(hourly <= 3.6, 1,0),
         annual_w = hourly*52*hours) %>%
  arrange(serno) %>%
  group_by(serno) %>%
  mutate(hrs_firm = sum(hours, na.rm = TRUE),
         wage_bill = sum(annual_w, na.rm = TRUE),
         k = 1,
         emp = sum(k),
         num_mw = sum(hh, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(avwage = wage_bill/emp,
         k12 = ifelse(avwage < 12001, 1, 0)) %>%
  group_by(k12) %>%
  mutate(num = sum(hh, na.rm = TRUE),
         propmin = num_mw/emp)

Seq98_collapse <- Seq98_z_full %>%
  group_by(serno) %>%
  summarize(hourly = mean(hourly, na.rm = TRUE),
            hh = mean(hh, na.rm = TRUE),
            propmin = mean(propmin, na.rm = TRUE),
            avwage = mean(avwage, na.rm = TRUE),
            emp = mean(emp, na.rm = TRUE),
            num_mw = mean(num_mw, na.rm = TRUE),
            k12 = mean(k12, na.rm = TRUE)) %>%
  mutate(top10 = quantile(propmin, probs = .9, na.rm = TRUE)) %>%
  group_by(serno) %>%
  mutate(band = NA,
         band = replace(band, avwage > 0 & avwage <= 1000, 1),
         band = replace(band, avwage > 1000 & avwage <= 2000, 2),
         band = replace(band, avwage > 2000 & avwage <= 3000, 3),
         band = replace(band, avwage > 3000 & avwage <= 4000, 4),
         band = replace(band, avwage > 4000 & avwage <= 5000, 5),
         band = replace(band, avwage > 5000 & avwage <= 6000, 6),
         band = replace(band, avwage > 6000 & avwage <= 7000, 7),
         band = replace(band, avwage > 7000 & avwage <= 8000, 8),
         band = replace(band, avwage > 8000 & avwage <= 9000, 9),
         band = replace(band, avwage > 9000 & avwage <= 10000, 10),
         band = replace(band, avwage > 10000 & avwage <= 11000, 11),
         band = replace(band, avwage > 11000 & avwage <= 12000, 12),
         band = replace(band, avwage > 12000 & avwage <= 13000, 13),
         band = replace(band, avwage > 13000 & avwage <= 14000, 14),
         band = replace(band, avwage > 14000 & avwage <= 15000, 15),
         band = replace(band, avwage > 15000 & avwage <= 16000, 16),
         band = replace(band, avwage > 16000 & avwage <= 17000, 17),
         band = replace(band, avwage > 17000 & avwage <= 18000, 18),
         band = replace(band, avwage > 18000 & avwage <= 19000, 19),
         band = replace(band, avwage > 19000 & avwage <= 20000, 20),
         band = replace(band, avwage > 20000 & avwage <= 21000, 21),
         band = replace(band, avwage > 21000 & avwage <= 22000, 22),
         band = replace(band, avwage > 22000 & avwage <= 23000, 23),
         band = replace(band, avwage > 23000 & avwage <= 24000, 24),
         band2 = NA,
         band2 = replace(band2, avwage > 0 & avwage < 2000, 1),
         band2 = replace(band2, avwage > 2000 & avwage < 4000, 2),
         band2 = replace(band2, avwage > 4000 & avwage < 6000, 3),
         band2 = replace(band2, avwage > 6000 & avwage < 8000, 4),
         band2 = replace(band2, avwage > 8000 & avwage < 10000, 5),
         band2 = replace(band2, avwage > 10000 & avwage < 12000, 6),
         band2 = replace(band2, avwage > 12000 & avwage < 14000, 7),
         band2 = replace(band2, avwage > 14000 & avwage < 16000, 8),
         band2 = replace(band2, avwage > 16000 & avwage < 18000, 9),
         band2 = replace(band2, avwage > 18000 & avwage < 20000, 10))
  
band5 <- xtile(Seq98_collapse$avwage, 20)
Seq98_collapse_final <- cbind(Seq98_collapse, band5)

#FIGURE 1 shows the average proportion of minimum wage workers and businesses average
#wages in 20 quantiles. The result is exactly the same as in the initial paper and shows
#that firms in the lower average wage quantiles have a higher proportion of low wage workers.
#The result supports the main assumption in the paper of firms with lower average wages being
#more impacted by the introduction of a national minimum wage than firms with higher average wages.
figure1 <- Seq98_collapse_final %>%
  rename(band5 = ...12) %>%
  group_by(band5) %>%
  summarize(meanp = mean(propmin)) %>%
  ggplot(aes(x = band5, y = meanp), color = band5) +
  geom_col(fill = "royalblue4", alpha = 0.90) +
  theme_tufte() +
  theme(axis.title.x = element_text(size = 12, color = "black"),
        axis.title.y = element_text(size = 12, color = "black"),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 12, color = "black"),
        axis.ticks = element_blank()) +
  scale_x_continuous("", breaks = seq(1, 20, by = 1)) +
  scale_y_continuous("Proportion (%) Minimum Wage Workers", limits = c(0,0.5)) +
  geom_vline(xintercept = 8.5, color = "red", linetype = "dashed") +
  annotate("text", x = 11, y = 0.45, label = "Average wage = \n £12,000", size = 3) +
  geom_vline(xintercept = 16.5, color = "red", linetype = "dashed") + 
  annotate("text", x = 19, y = 0.45, label = "Average wage = \n £20,000", size = 3)

##########STEP 4: FIGURE 2: CHANGE IN LOG AVERAGE WAGE BY PERCENTILE###########

#Building a new function percentile() to create variables that 
#contain percentiles of the distribution of log average wage 
#in each year from 1995 to 2002.
percentile <- function(x) {
  pctilex <- fame_data %>%
    filter(year == x & avwage >= 3)
  quantile(pctilex$ln_avwage, probs = seq(0.01, 0.99, by = 0.01), na.rm = TRUE)
}

#Create percentile variables (pcw*) for each financial year in the dataset.
#Also creating new variables for each year that just contain numbers
#form 1 to 99 to represent percent values (percent*). Then all variables
#are merged into one data frame.
pcw95 <- percentile(1995)
percent95 <- seq(1,99,1)
pcw96 <- percentile(1996)
percent96 <- seq(1,99,1)
pcw97 <- percentile(1997)
percent97 <- seq(1,99,1)
pcw98 <- percentile(1998)
percent98 <- seq(1,99,1)
pcw99 <- percentile(1999)
percent99 <- seq(1,99,1)
pcw00 <- percentile(2000)
percent00 <- seq(1,99,1)
pcw01 <- percentile(2001)
percent01 <- seq(1,99,1)
pcw02 <- percentile(2002)
percent02 <- seq(1,99,1)
pctile_data <- data.frame(cbind(pcw95, pcw96, pcw97, pcw98, pcw99, pcw00, pcw01, pcw02, percent95, percent96, percent97, percent98, percent99, percent00, percent01, percent02))

#To arrive at FIGURE2 the difference between percentiles of log average wage from one year
#to another are calculated and stored in new difference variables (diff*).
pctile_data_diff <- pctile_data %>%
  arrange(percent95) %>%
  mutate(diff96 = pcw96 - pcw95,
        diff97 = pcw97 - pcw96,
        diff98 = pcw98 - pcw97,
        diff99 = pcw99 - pcw98,
        diff00 = pcw00 - pcw99,
        diff01 = pcw01 - pcw00,
        diff02 = pcw02 - pcw01)

#Building FIGURE2 with Percentiles of initial wage distribution (Percentile 76 and higher) on the x axis
#and change in log average wage on the y-axis. Two line plots are added. One for
#the change in in log average wage between 1998 and 1999 (pre-policy). And one for the change
#in log average wage between 1999 and 2000 (post-policy). The results are exactly the same as
#in the paper.
figure2 <- pctile_data_diff %>% 
  filter(percent99 < 76) %>%
  ggplot(aes(x = percent99)) +
  geom_line(aes(y = diff99, color = "1998-1999"), linetype = "dashed") +
  geom_line(aes(y = diff00, color = "1999-2000")) +
  scale_x_continuous("Percentiles of initial wage distribution", breaks = c(0,13,25,50,75)) +
  scale_y_continuous("Change in log average wage", breaks = c(0, .05, .1, .15), limits = c(-0.01, 0.155)) +
  scale_color_manual("", breaks = c("1998-1999", "1999-2000"), values = c ("chocolate3", "navyblue")) +                    
  geom_vline(xintercept = 14, color = "grey", linetype = "dashed") +
  annotate("text", x = 21, y = 0.13, label = "£12,000", size = 5) +
  geom_vline(xintercept = 53, color = "grey", linetype = "dashed") + 
  annotate("text", x = 60, y = 0.11, label = "£20,000", size = 5) +
  theme_tufte() +
  theme(axis.title.x = element_text(size = 16, color = "black"),
        axis.title.y = element_text(size = 16, color = "black"),
        axis.text.x = element_text(size = 14, color = "black"),
        axis.text.y = element_text(size = 14, color = "black"),
        axis.ticks = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(face = "bold", size = 14))

##########STEP 5: TABLE 1: DIFFERENCE-IN-DIFFERENCE##########

#Building three functions to reproduce the tables as in the provided Stata code 
#which contain mean and standard deviation for average wage (avwage),
#logarithmic average wage (ln_avwage) and gross profits (net_pcm)
#grouped by policy period (NMW) and treatment or control group (ctreat1).
#The threshold to be part of the treatment group is an average wage of
#below £12,000.

tabs_avwage <- function(x,y){ fame_data %>%
    filter(pp == 1, NMW == x, ctreat1 == y) %>%
    summarize(mean = mean(avwage),
              sd = sd(avwage))
}

tabs_lnwage <- function(x,y){ fame_data %>%
    filter(pp == 1, NMW == x, ctreat1 == y) %>%
    summarize(mean = mean(ln_avwage),
              sd = sd(ln_avwage))
}

tabs_profits <- function(x,y){ fame_data %>%
    filter(pp == 1, NMW == x, ctreat1 == y) %>%
    summarize(mean = mean(net_pcm),
              sd = sd(net_pcm))
}

#Calculate mean and standard deviation 
#of average wage for all four groups
meansd_00_wage <- tabs_avwage(0,0)
meansd_01_wage <- tabs_avwage(0,1)
meansd_10_wage <- tabs_avwage(1,0)
meansd_11_wage <- tabs_avwage(1,1)

#Calculate mean and standard deviation 
#of logarithmic average wage for all 
#four groups
meansd_00_lnwage <- tabs_lnwage(0,0)
meansd_01_lnwage <- tabs_lnwage(0,1)
meansd_10_lnwage <- tabs_lnwage(1,0)
meansd_11_lnwage <- tabs_lnwage(1,1)

#Calculate mean and standard deviation 
#of profits for all four groups
meansd_00_profit <- tabs_profits(0,0)
meansd_01_profit <- tabs_profits(0,1)
meansd_10_profit <- tabs_profits(1,0)
meansd_11_profit <- tabs_profits(1,1)

#Claculate number of observations for 
#each of the four cells in the table
n_ctreatvsNMW <- fame_data %>%
  filter(pp == 1) %>%
  group_by(NMW, ctreat1) %>%
  tally () %>%
  spread(NMW, n) %>%
  select(-ctreat1)

#Putting all values together in three tables
#as in the provided Stata code under the point
#TABLE1: DIFF-IN-DIFF
tabfromstata_wage <- matrix(c(meansd_00_wage, n_ctreatvsNMW[1,1], meansd_01_wage,n_ctreatvsNMW[2,1], meansd_10_wage, n_ctreatvsNMW[1,2],meansd_11_wage, n_ctreatvsNMW[2,2]), ncol = 2)
tabfromstata_lnwage <- matrix(c(meansd_00_lnwage, n_ctreatvsNMW[1,1], meansd_01_lnwage,n_ctreatvsNMW[2,1], meansd_10_lnwage, n_ctreatvsNMW[1,2],meansd_11_lnwage, n_ctreatvsNMW[2,2]), ncol = 2)
tabfromstata_profits <- matrix(c(meansd_00_profit, n_ctreatvsNMW[1,1], meansd_01_profit,n_ctreatvsNMW[2,1], meansd_10_profit, n_ctreatvsNMW[1,2],meansd_11_profit, n_ctreatvsNMW[2,2]), ncol = 2)

#First Difference-in-Difference model as diff_in_diff1 without controls for logarithmic average wages
#with the independent variables for the indicator of the treatment 
#or control group membership (ctreat1), the interaction term between group membership
#and pre-post policy variable (treat1_NMW) and the pre-post policy variable (NMW).
#Results of the regression with clustered standard errors by firm (regno) are exactly
#the same as with the provided Stata code.
diff_in_diff1 <- fame_data %>%
  filter (pp == 1) %>%
  lm.cluster(ln_avwage ~ ctreat1 + treat1_NMW + NMW, data = ., cluster = 'regno')
summary(diff_in_diff1)

#To arrive at TABLE1 of the paper one has to calculate the 
#values from the regression coefficients of diff_in_diff1
#to get the upper half of TABLE1. After calculating the 
#values, they were added to the reproduced TABLE 1 in a word doc.

#Value for change in logarithmic average wages in firms of the treatment group
#before implementation of the National Minimum Wage. First row, first column.
#Values extracted from Regression: Intercept + Coefficient of ctreat1.
preNMW_firstrow1 <- matrix(summary(diff_in_diff1))[1,] + matrix(summary(diff_in_diff1))[2,]

#Value for change in logarithmic average wages in firms of the control group
#before implementation of the National Minimum Wage: Second row, first column
#Values extracted from regression: Intercept.
preNMW_secondrow1 <- matrix(summary(diff_in_diff1))[1,]

#Value for change in logarithmic average wages in firms of the treatment group
#after implementation of the National Minimum Wage: First row, second column
#Values extracted from Regression: Intercept + Coefficients of ctreat1, treat1_NMW and NMW.
postNMW_firstrow1 <- matrix(summary(diff_in_diff1))[1,] + matrix(summary(diff_in_diff1))[2,] + matrix(summary(diff_in_diff1))[3,] + matrix(summary(diff_in_diff1))[4,]

#Value for change in average wages in firms of the control group
#after implementation of the National Minimum Wage: Second row, second column
#Values extracted from Regression: Intercept + Coefficient of NMW.
postNMW_secondrow1 <- matrix(summary(diff_in_diff1))[1,] + matrix(summary(diff_in_diff1))[4,]

#Value for difference in change in logarithmic average wages for firms of the treatment group
#before and after the implementation of the National Minimum Wage: First row, third column
#Values extracted from Regression: Coefficients treat1_NMW and NMW.
difference_firstrow1 <- matrix(summary(diff_in_diff1))[3,] + matrix(summary(diff_in_diff1))[4,]

#Value for difference in change in logarithmic average wages for firms of the control group
#before and after the implementation of the National Minimum Wage: Second row, third column
#Values extracted from Regression: Coefficient of NMW.
difference_secondrow1 <- matrix(summary(diff_in_diff1))[4,]

#Difference between change in logarithmic average wages for firms of the treatment group
#before and after the implementation of the National Minimum Wage and the control group 
#before and after implementation: Third row, third column. And associated standard error.
#Values extracted from Regression: Coefficients treat1_NMW.
difference_thirdrow1 <- matrix(summary(diff_in_diff1))[3,]
stderrdiff1 <- matrix(summary(diff_in_diff1))[7,]

#First Difference-in-Difference model as diff_in_diff2 without controls for net profits
#with the independent variables for the indicator of the treatment 
#or control group membership (ctreat1), the interaction term between group membership
#and pre-post policy variable (treat1_NMW) and the pre-post policy variable (NMW).
#Results of the regression with clustered standard errors by firm (regno) are exactly
#the same as with the provided Stata code.
#The procedure is similar to the first difference-in-difference model. After calculating 
#the values they were added to the reproduced TABLE 1 in a word doc.
diff_in_diff2 <- fame_data %>%
  filter (pp ==1) %>%
  lm.cluster(net_pcm ~ ctreat1 + treat1_NMW + NMW, data = ., cluster = "regno")
summary(diff_in_diff2)

preNMW_firstrow2 <- matrix(summary(diff_in_diff2))[1,] + matrix(summary(diff_in_diff2))[2,]
preNMW_secondrow2 <- matrix(summary(diff_in_diff2))[1,]
postNMW_firstrow2 <- matrix(summary(diff_in_diff2))[1,] + matrix(summary(diff_in_diff2))[2,] + matrix(summary(diff_in_diff2))[3,] + matrix(summary(diff_in_diff2))[4,] #with interaction term treatmentgroup*post-policy
postNMW_secondrow2 <- matrix(summary(diff_in_diff2))[1,] + matrix(summary(diff_in_diff2))[4,]
difference_firstrow2 <- matrix(summary(diff_in_diff2))[3,] + matrix(summary(diff_in_diff2))[4,]
difference_secondrow2 <- matrix(summary(diff_in_diff2))[4,]
difference_thirdrow2 <- matrix(summary(diff_in_diff2))[3,]
stderrdiff2 <- matrix(summary(diff_in_diff2))[7,]

##########STEP 6: TABLE 2: REGRESSION WITH CONTROL VARIABLES#############

#Create factors from the numeric variables for 2-digit UK Industry Code
#(sic2), the year (year) and government office region (gorwk) to
#include them as categorical variables in following regressions.
fame_data <- fame_data %>%
  mutate(sic2.f = factor(sic2),
         year.f = factor(year),
         gorwk.f = factor(gorwk))

#Again regressions with logarithmic average wage (model_avwage_discrete) and net profits 
#(model_profit_discrete) as dependent variables and clustered standard errors are run.
#But this time control variables besides the independent variables (ctreat1, treat1_NMW, NMW)
#from before are included. Controls consist of graduate proportion in government region (grad2),
#industry proportion of union members (unionmem), industry proportion of part-time workers
#(ptwk), industry proportion of female workers (female) and the newly created variables from
#above: sic2.f, year.f, gorwk.f.
#To arrive at TABLE2 one has to extract the coefficients and standard errors for the independent
#variables treat_NMW and avwage99_NMW. avwage99_NMW is a continuous variable of the average wage
#which is included in further regressions (model_avwage_continuous, model_profit_continuous) 
#to determine the treatment's strength. The results appear to be exactly the same as in 
#the provided Stata code and the published paper.
#After calculating coefficients and standard errors they were added to reproduced TABLE2 in a word doc.

#Regressions with treatment variable as discrete.
model_avwage_discrete <- fame_data %>%
  filter(pp == 1) %>%
  lm.cluster(ln_avwage ~ ctreat1 + treat1_NMW + NMW + grad2 + unionmem + ptwk + female + sic2.f + year.f + gorwk.f, data = ., cluster = 'regno')

model_profit_discrete <- fame_data %>%
  filter(pp == 1) %>%
  lm.cluster(net_pcm ~ ctreat1 + treat1_NMW + NMW + grad2 + unionmem + ptwk + female + sic2.f + year.f + gorwk.f, data = ., cluster = 'regno')

#Regressions with treatment variable as continuous.
model_avwage_continuous <- fame_data %>%
  filter(pp == 1) %>%
  lm.cluster(ln_avwage ~ c_avwage99 + avwage99_NMW + NMW + grad2 + unionmem + ptwk + female + sic2.f + year.f + gorwk.f, data = ., cluster = 'regno')

model_profit_continuous <- fame_data %>%
  filter(pp == 1) %>%
  lm.cluster(net_pcm ~ c_avwage99 + avwage99_NMW + NMW + grad2 + unionmem + ptwk + female + sic2.f + year.f + gorwk.f, data = ., cluster = 'regno')

#Extract coefficients and standard errors for treat1_NMW in the average wage 
#and net profit model with the treatment variable as discrete variable.
table2_column1_row1 <- matrix(summary(model_avwage_discrete), ncol = 4)[3,c(1,2)]
table2_column2_row1 <- matrix(summary(model_profit_discrete), ncol = 4)[3,c(1,2)] 

#Extract coefficients and standard errors for avwage99_NMW in the average wage 
#and net profit model with the treatment variable as continuous variable.
#The coefficient is inverted because the continuous variable goes from low
#average wages to high average wages while the discrete goes from high average wage (0)
#to low average wages (1). In order to have coefficients which show the effect of
#the treatment in the same direction, the value is inverted here.
mathelp1 <- matrix(summary(model_avwage_continuous), ncol = 4)[3,c(1,2)]
table2_column1_row2 <- c(mathelp1[1]*(-1),mathelp1[2])

mathelp2 <- matrix(summary(model_profit_continuous), ncol = 4)[3,c(1,2)]
table2_column2_row2 <- c(mathelp2[1]*(-1),mathelp2[2])

#In TABLE2, besides the coeffcients and standard errors the authors report the results of 
#two F-tests to test their "no behavioral response hypothesis" which assumes no impact on
#labor demand by the introduction of a National Minimum Wage.
#In Stata these F-tests were performed with a multivariate linear regression followed by
#the "test" command to test the linear Hypothesis that the coeffecients of treat1_NMW in 
#both regressions are equal to 0 and both coeffcients of avwage99_NMW are equal to 0.
#I failed to reproduce the results of the F-Test correctly in R because there is no 
#equivalent function to perform the test exactly like in Stata. I tried two commonly
#used methods to conduct F-Tests in R with the functions "var.test" and "linearHypothesis" 
#from the car-Package. But could not get the same results as in the paper.
#Since the provided Stata code can produce the F-Tests, this part of the reproduction was done in Stata.
#Below the attempts to reproduce the F-Tests in R.

#The first attempt to reproduce the F-tests in the paper was with "var.test" function.
#The results for the treat1_NMW variable were extracted from both regressions and an
#F-Test was run. The resulting p-value was .25 which is lower than in the paper.
model_avwage_discrete_nocluster <- fame_data %>%
  filter(pp == 1) %>%
  lm(ln_avwage ~ ctreat1 + treat1_NMW + NMW + grad2 + unionmem + ptwk + female + sic2.f + year.f + gorwk.f, data =.)

model_profit_discrete_nocluster <- fame_data %>%
  filter(pp == 1) %>%
  lm(net_pcm ~ ctreat1 + treat1_NMW + NMW + grad2 + unionmem + ptwk + female + sic2.f + year.f + gorwk.f, data = .)

coefficient_treat_wagemodel <- coef(summary(model_avwage_discrete_nocluster))["treat1_NMW",]
coefficient_treat_profitmodel <- coef(summary(model_profit_discrete_nocluster))["treat1_NMW",]
var.test(coefficient_treat_profitmodel, coefficient_treat_wagemodel*.27)

#The second attempt to reproduce the F-tests of the paper was with the "linearHypothesis" 
#function from the car-Package. After a multivariate regression with two independent variables was
#run, I tried to perform the F-test on the results of the treat1_NMW variable. Multiple
#attempts only resulted in an error message.
mvregression <- lm(cbind(ln_avwage, net_pcm) ~ ctreat1 + treat1_NMW + NMW + grad2 + unionmem + ptwk + female + sic2.f + year.f + gorwk.f, data = fame_data)
linearHypothesis(mvregression, c("treat1_NMW = treat1_NMW"))

#First attempt with "var.test" for the continuous treatment variable (avwage99_NMW). 
#The resulting p-value was .86 which is higher than in the paper.
model_avwage_continuous_nocluster <- fame_data %>%
  filter(pp == 1) %>%
  lm(ln_avwage ~ c_avwage99 + avwage99_NMW + NMW + grad2 + unionmem + ptwk + female + sic2.f + year.f + gorwk.f, data =.)
summary(model_avwage_continuous_nocluster)

model_profit_continuous_nocluster <- fame_data %>%
  filter(pp == 1) %>%
  lm(net_pcm ~ c_avwage99 + avwage99_NMW + NMW + grad2 + unionmem + ptwk + female + sic2.f + year.f + gorwk.f, data = .)
summary(model_profit_continuous_nocluster)

coefficient_treat_wagemodel_con <- coef(summary(model_avwage_continuous_nocluster))["avwage99_NMW", ]
coefficient_treat_profitmodel_con <- coef(summary(model_profit_continuous_nocluster))["avwage99_NMW", ]
var.test(coefficient_treat_profitmodel_con, coefficient_treat_wagemodel_con*.27)

#Second attempt with "linearHypothesis" for the continuous treatment variable (avwage99_NMW).
#linearHypothesis resulted in an error.
mvregression_con <- lm(cbind(ln_avwage, net_pcm) ~ c_avwage99 + avwage99_NMW + NMW + grad2 + unionmem + ptwk + female + sic2.f + year.f + gorwk.f, data = fame_data)
linearHypothesis(mvregression, c("avwage99_NMW=avwage99_NMW"))
