library(mr.ios)
library(parallel)
library(tidyverse)

#------------------------------------------------------------------------
#Method: modified second order * IOS_mean
#------------------------------------------------------------------------
ios_dat <-ios(exp=exp_dat, bg=bg_dat)

a2 <- list()
invisible(capture.output(a2 <- lapply(1:100, function(x) {

  ios_dat$SNP <- sample(ios_dat$SNP)
  ios_radialMR <- mr.ios_type(dat=dat, ios = ios_dat, ios_type="ios2_sd", alpha = 0.05, 6, tol = 0.0001)

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

a2 <- data.frame(t(sapply(a2, c)))
a2$permutation <- rownames(a2)

temp <- orig.est(dat)
set7 <- bind.est(temp, a2)

set7$method <- "Mod.2nd * ios2_mean"


#------------------------------------------------------------------------
#Method: modified second order * ios2_sd
#------------------------------------------------------------------------

ios_dat <-ios(exp=exp_dat, bg=bg_dat)

b2 <- list()
invisible(capture.output(b2 <- lapply(1:100, function(x) {

  ios_dat$SNP <- sample(ios_dat$SNP)
  ios_radialMR <- mr.ios_type(dat=dat, ios = ios_dat, ios_type="ios2_sd", alpha = 0.05, 6, tol = 0.0001)
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

b2 <- data.frame(t(sapply(b2, c)))
b2$permutation <- rownames(b2)


temp <- orig.est(dat = dat, ios_type = "ios2_sd")
set8 <- bind.est(temp, b)

set8$method <- "Mod.2nd * ios2_sd"


#------------------------------------------------------------------------
#Method: modified second order * ios2_iqr
#------------------------------------------------------------------------

ios_dat <-ios(exp=exp_dat, bg=bg_dat)

c2 <- list()
invisible(capture.output(c2 <- lapply(1:100, function(x) {

  ios_dat$SNP <- sample(ios_dat$SNP)
  ios_radialMR <- mr.ios_type(dat=dat, ios = ios_dat, ios_type="ios2_iqr", alpha = 0.05, 6, tol = 0.0001)
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

c2 <- data.frame(t(sapply(c2, c)))
c2$permutation <- rownames(c2)


temp <- orig.est(dat = dat, ios_type = "ios2_iqr")
set9 <- bind.est(temp, c2)

set9$method <- "Mod.2nd * ios2_iqr"


#------------------------------------------------------------------------
#Method: modified second order * ios2_median
#------------------------------------------------------------------------
ios_dat <-ios(exp=exp_dat, bg=bg_dat)

d2 <- list()
invisible(capture.output(d2 <- lapply(1:100, function(x) {

  ios_dat$SNP <- sample(ios_dat$SNP)
  ios_radialMR <- mr.ios_type(dat=dat, ios = ios_dat, ios_type="ios2_median", alpha = 0.05, 6, tol = 0.0001)
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

d2 <- data.frame(t(sapply(d2, c)))
d2$permutation <- rownames(d2)

temp <- orig.est(dat = dat, ios_type = "ios2_median")
set10 <- bind.est(temp, d2)

set10$method <- "Mod.2nd * ios2_median"


#Method: modified second order * ios2_95
ios_dat <-ios(exp=exp_dat, bg=bg_dat)

e2 <- list()
invisible(capture.output(e2 <- lapply(1:100, function(x) {

  ios_dat$SNP <- sample(ios_dat$SNP)
  ios_radialMR <- mr.ios_type(dat=dat, ios = ios_dat, ios_type="ios2_95", alpha = 0.05, 6, tol = 0.0001)
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

e2 <- data.frame(t(sapply(e2, c)))
e2$permutation <- rownames(e2)

temp <- orig.est(dat = dat, ios_type = "ios2_95")
set11 <- bind.est(temp, e2)

set11$method <- "Mod.2nd * ios2_95"


#Method: modified second order * IOS_max
ios_dat <-ios(exp=exp_dat, bg=bg_dat)

f2 <- list()
invisible(capture.output(f2 <- lapply(1:100, function(x) {

  ios_dat$SNP <- sample(ios_dat$SNP)
  ios_radialMR <- mr.ios_type(dat=dat, ios = ios_dat, ios_type="ios2_max", alpha = 0.05, 6, tol = 0.0001)
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

f2 <- data.frame(t(sapply(f2, c)))
f2$permutation <- rownames(f2)

temp <- orig.est(dat = dat, ios_type = "ios2_max")
set12 <- bind.est(temp, f2)

set12$method <- "Mod.2nd * ios2_max"


#------------------------------------------------------------------------
#Generate a dataframe of simulation results
#------------------------------------------------------------------------
ios2_sim <- rbind(set7, set8, set9, set10, set11, set12)
#ios2_sim <- as.data.frame(lapply(ios2_sim, unlist))


#Define the class if it is not specified (null)
factor_cols <- c("exposure","outcome", "method")
ios2_sim[factor_cols] <- lapply(ios2_sim[factor_cols], as.factor)
numeric_cols <- c("nsnp", "Estimate", "Std.Error", "t.value", "P_val", "Q", "Q_pval", "permutation")
ios2_sim[numeric_cols] <- lapply(ios2_sim[numeric_cols], as.numeric)


#------------------------------------------------------------------------
#Plots
#------------------------------------------------------------------------

high2 <- ios2_sim %>%
  group_by(method) %>%
  arrange(Q) %>%
  mutate(id = row_number()) %>%
  filter(permutation < 1)

p2 <- ios2_sim %>%
  group_by(method) %>%
  arrange(Q) %>%
  mutate(id = row_number()) %>%
  mutate(highperm = permutation == 0) %>%
  #filter(grepl("mean", method)) %>%
  ggplot(aes(y=Q, x=id, group=as.factor(method)), colour=as.factor(method)) +
  geom_point(aes(colour=as.factor(method)), alpha = 0, shape = ".") +
  geom_line(aes(colour=as.factor(method))) +
  geom_point(data=high2, aes(y=Q, x=id), colour='red', size=1) +
  geom_hline(yintercept=140.8, linetype="dashed", color = "grey") +
  labs(x="Permutations", y="Q statistics", colour="Method")
p2



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


