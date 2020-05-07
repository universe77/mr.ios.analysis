library(mr.ios)
library(parallel)
library(tidyverse)

#------------------------------------------------------------------------
#Method: modified second order * IOS_mean
#------------------------------------------------------------------------
ios_dat <-ios(exp=exp_dat, bg=bg_dat)

a <- list()
invisible(capture.output(a <- lapply(1:100, function(x) {

  ios_dat$SNP <- sample(ios_dat$SNP)
  ios_radialMR <- mr.ios_type(dat=dat, ios = ios_dat, ios_type="ios1_sd", alpha = 0.05, 6, tol = 0.0001)

  temp <- ios_radialMR$coef[1, ]
  temp <- tibble::rownames_to_column(temp, "method")
  temp$exposure <- dat$exposure[1]
  temp$outcome <- dat$outcome[1]
  temp$nsnp <- length(dat$SNP)
  temp$Q <- ios_radialMR$qstatistic
  temp$Q_pval <- Total_Q_chi<-pchisq(ios_radialMR$qstatistic, ios_radialMR$df, lower.tail = FALSE)

  temp2 <- temp[, c(6, 7, 1, 8, 2, 3, 4, 5, 9, 10)]

  return(temp2)
}
))
)

a <- data.frame(t(sapply(a, c)))
a$permutation <- rownames(a)

temp <- orig.est(dat)
set1 <- bind.est(temp, a)

set1$method <- "Mod.2nd * ios1_mean"


#------------------------------------------------------------------------
#Method: modified second order * IOS1_sd
#------------------------------------------------------------------------

ios_dat <-ios(exp=exp_dat, bg=bg_dat)

b <- list()
invisible(capture.output(b <- lapply(1:100, function(x) {

  ios_dat$SNP <- sample(ios_dat$SNP)
  ios_radialMR <- mr.ios_type(dat=dat, ios = ios_dat, ios_type="ios1_sd", alpha = 0.05, 6, tol = 0.0001)
  temp <- ios_radialMR$coef[1, ]
  temp <- tibble::rownames_to_column(temp, "method")
  temp$exposure <- dat$exposure[1]
  temp$outcome <- dat$outcome[1]
  temp$nsnp <- length(dat$SNP)
  temp$Q <- ios_radialMR$qstatistic
  temp$Q_pval <- Total_Q_chi<-pchisq(ios_radialMR$qstatistic, ios_radialMR$df, lower.tail = FALSE)

  temp2 <- temp[, c(6, 7, 1, 8, 2, 3, 4, 5, 9, 10)]

  return(temp2)
}
))
)

b <- data.frame(t(sapply(b, c)))
b$permutation <- rownames(b)


temp <- orig.est(dat = dat, ios_type = "ios1_sd")
set2 <- bind.est(temp, b)

set2$method <- "Mod.2nd * ios1_sd"


#------------------------------------------------------------------------
#Method: modified second order * IOS1_iqr
#------------------------------------------------------------------------

ios_dat <-ios(exp=exp_dat, bg=bg_dat)

c <- list()
invisible(capture.output(c <- lapply(1:100, function(x) {

  ios_dat$SNP <- sample(ios_dat$SNP)
  ios_radialMR <- mr.ios_type(dat=dat, ios = ios_dat, ios_type="ios1_iqr", alpha = 0.05, 6, tol = 0.0001)
  temp <- ios_radialMR$coef[1, ]
  temp <- tibble::rownames_to_column(temp, "method")
  temp$exposure <- dat$exposure[1]
  temp$outcome <- dat$outcome[1]
  temp$nsnp <- length(dat$SNP)
  temp$Q <- ios_radialMR$qstatistic
  temp$Q_pval <- Total_Q_chi<-pchisq(ios_radialMR$qstatistic, ios_radialMR$df, lower.tail = FALSE)

  temp2 <- temp[, c(6, 7, 1, 8, 2, 3, 4, 5, 9, 10)]

  return(temp2)
}
))
)

c <- data.frame(t(sapply(c, c)))
c$permutation <- rownames(c)


temp <- orig.est(dat = dat, ios_type = "ios1_iqr")
set3 <- bind.est(temp, c)

set3$method <- "Mod.2nd * ios1_iqr"


#------------------------------------------------------------------------
#Method: modified second order * IOS1_median
#------------------------------------------------------------------------
ios_dat <-ios(exp=exp_dat, bg=bg_dat)

d <- list()
invisible(capture.output(d <- lapply(1:100, function(x) {

  ios_dat$SNP <- sample(ios_dat$SNP)
  ios_radialMR <- mr.ios_type(dat=dat, ios = ios_dat, ios_type="ios1_median", alpha = 0.05, 6, tol = 0.0001)
  temp <- ios_radialMR$coef[1, ]
  temp <- tibble::rownames_to_column(temp, "method")
  temp$exposure <- dat$exposure[1]
  temp$outcome <- dat$outcome[1]
  temp$nsnp <- length(dat$SNP)
  temp$Q <- ios_radialMR$qstatistic
  temp$Q_pval <- Total_Q_chi<-pchisq(ios_radialMR$qstatistic, ios_radialMR$df, lower.tail = FALSE)

  temp2 <- temp[, c(6, 7, 1, 8, 2, 3, 4, 5, 9, 10)]

  return(temp2)
}
))
)

d <- data.frame(t(sapply(d, c)))
d$permutation <- rownames(d)

temp <- orig.est(dat = dat, ios_type = "ios1_median")
set4 <- bind.est(temp, d)

set4$method <- "Mod.2nd * ios1_median"

#------------------------------------------------------------------------
#Method: modified second order * IOS1_95
#------------------------------------------------------------------------

ios_dat <-ios(exp=exp_dat, bg=bg_dat)

e <- list()
invisible(capture.output(e <- lapply(1:100, function(x) {

  ios_dat$SNP <- sample(ios_dat$SNP)
  ios_radialMR <- mr.ios_type(dat=dat, ios = ios_dat, ios_type="ios1_95", alpha = 0.05, 6, tol = 0.0001)
  temp <- ios_radialMR$coef[1, ]
  temp <- tibble::rownames_to_column(temp, "method")
  temp$exposure <- dat$exposure[1]
  temp$outcome <- dat$outcome[1]
  temp$nsnp <- length(dat$SNP)
  temp$Q <- ios_radialMR$qstatistic
  temp$Q_pval <- Total_Q_chi<-pchisq(ios_radialMR$qstatistic, ios_radialMR$df, lower.tail = FALSE)

  temp2 <- temp[, c(6, 7, 1, 8, 2, 3, 4, 5, 9, 10)]

  return(temp2)
}
))
)

e <- data.frame(t(sapply(e, c)))
e$permutation <- rownames(e)

temp <- orig.est(dat = dat, ios_type = "ios1_95")
set5 <- bind.est(temp, e)

set5$method <- "Mod.2nd * ios1_95"

#------------------------------------------------------------------------
#Method: modified second order * IOS_max
#------------------------------------------------------------------------

ios_dat <-ios(exp=exp_dat, bg=bg_dat)

f <- list()
invisible(capture.output(f <- lapply(1:100, function(x) {

  ios_dat$SNP <- sample(ios_dat$SNP)
  ios_radialMR <- mr.ios_type(dat=dat, ios = ios_dat, ios_type="ios1_max", alpha = 0.05, 6, tol = 0.0001)
  temp <- ios_radialMR$coef[1, ]
  temp <- tibble::rownames_to_column(temp, "method")
  temp$exposure <- dat$exposure[1]
  temp$outcome <- dat$outcome[1]
  temp$nsnp <- length(dat$SNP)
  temp$Q <- ios_radialMR$qstatistic
  temp$Q_pval <- Total_Q_chi<-pchisq(ios_radialMR$qstatistic, ios_radialMR$df, lower.tail = FALSE)

  temp2 <- temp[, c(6, 7, 1, 8, 2, 3, 4, 5, 9, 10)]

  return(temp2)
}
))
)

f <- data.frame(t(sapply(f, c)))
f$permutation <- rownames(f)

temp <- orig.est(dat = dat, ios_type = "ios1_max")
set6 <- bind.est(temp, f)

set6$method <- "Mod.2nd * ios1_max"


#------------------------------------------------------------------------
#Generate a dataframe of simulation results
#------------------------------------------------------------------------
ios1_sim <- rbind(set1, set2, set3, set4, set5, set6)
ios1_sim <- as.data.frame(lapply(ios1_sim, unlist))


#Define the class if it is not specified (null)
factor_cols <- c("exposure","outcome", "method")
ios1_sim[factor_cols] <- lapply(ios1_sim[factor_cols], as.factor)
numeric_cols <- c("nsnp", "Estimate", "Std.Error", "t.value", "P_val", "Q", "Q_pval", "permutation")
ios1_sim[numeric_cols] <- lapply(ios1_sim[numeric_cols], as.numeric)


#------------------------------------------------------------------------
#Plots
#------------------------------------------------------------------------

high <- ios1_sim %>%
  group_by(method) %>%
  arrange(Q) %>%
  mutate(id = row_number()) %>%
  filter(permutation < 1)

p1 <- ios1_sim %>%
  group_by(method) %>%
  arrange(Q) %>%
  mutate(id = row_number()) %>%
  mutate(highperm = permutation == 0) %>%
  #filter(grepl("mean", method)) %>%
  ggplot(aes(y=Q, x=id, group=as.factor(method)), colour=as.factor(method)) +
  geom_point(aes(colour=as.factor(method)), alpha = 0, shape = ".") +
  geom_line(aes(colour=as.factor(method))) +
  geom_point(data=high, aes(y=Q, x=id), colour='red', size=1) +
  labs(x="Permutations", y="Q statistics", colour="Method")
p1



#------------------------------------------------------------------------
#functions
#------------------------------------------------------------------------
orig.est <- function(dat = dat, ios_type = "ios1_mean", weights =6){
  ios_dat <-ios(exp=exp_dat, bg=bg_dat)
  mr <- mr.ios_type(dat=dat, ios = ios_dat, ios_type=ios_type, alpha = 0.05, weights, tol = 0.0001)
  temp <- mr$coef[1, ]
  temp <- tibble::rownames_to_column(temp, "method")
  temp$exposure <- dat$exposure[1]
  temp$outcome <- dat$outcome[1]
  temp$nsnp <- length(dat$SNP)
  temp$Q <- mr$qstatistic
  temp$Q_pval <- Total_Q_chi<-pchisq(mr$qstatistic, mr$df, lower.tail = FALSE)
  temp2 <- temp[, c(6, 7, 1, 8, 2, 3, 4, 5, 9, 10)]
  temp2$permutation <- 0
  return(temp2)
}


bind.est <- function(x, y){
  for (i in names(x)) {
    if (!(i %in% names(y))) {
      names = c(colnames(y))
      colnames(x) = names
      set <- rbind(x, y)
    }
    else if(i==tail(names(y),n=1)) {
      set <- rbind(x, y)
    }
  }
  return(set)
}


mr.ios_type <-function(dat=dat, ios = ios_dat, ios_type="ios1_mean", alpha = 0.05, weights, tol = 0.0001){
  dat_rmr <- RadialMR::format_radial(dat$beta.exposure, dat$beta.outcome, dat$se.exposure, dat$se.outcome, dat$SNP, ios[[ios_type]], ios$SNP)
  rares <- RadialMR::ivw_radial(dat_rmr, alpha, weights, tol, external_weight = TRUE)
  return(rares)
  }

