library(mr.ios)
library(parallel)
library(tidyverse)


load("/newhome/yc16575/mrios/results/ios_sim_chd_bmi_240619.RData")



#------------------------------------------------------------------------
#Method: modified second order * IOS_mean
#------------------------------------------------------------------------
ios_dat <-mr.ios::ios(exp=exp_dat, bg=bg_dat)

a <- list()
invisible(capture.output(a <- lapply(1:100, function(x) {

  ios_dat$SNP <- sample(ios_dat$SNP)
  ios_radialMR <- mr.ios(dat=dat, ios = ios_dat, ios_type="ios1_mean", alpha = 0.05, weight = 6, tol = 0.0001)

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

ios_dat <-mr.ios::ios(exp=exp_dat, bg=bg_dat)

b <- list()
invisible(capture.output(b <- lapply(1:100, function(x) {

  ios_dat$SNP <- sample(ios_dat$SNP)
  ios_radialMR <- mr.ios(dat=dat, ios = ios_dat, ios_type="ios1_sd", alpha = 0.05, 6, tol = 0.0001)
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

ios_dat <-mr.ios::ios(exp=exp_dat, bg=bg_dat)

c <- list()
invisible(capture.output(c <- lapply(1:100, function(x) {

  ios_dat$SNP <- sample(ios_dat$SNP)
  ios_radialMR <- mr.ios(dat=dat, ios = ios_dat, ios_type="ios1_iqr", alpha = 0.05, 6, tol = 0.0001)
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
ios_dat <-mr.ios::ios(exp=exp_dat, bg=bg_dat)

d <- list()
invisible(capture.output(d <- lapply(1:100, function(x) {

  ios_dat$SNP <- sample(ios_dat$SNP)
  ios_radialMR <- mr.ios(dat=dat, ios = ios_dat, ios_type="ios1_median", alpha = 0.05, 6, tol = 0.0001)
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

ios_dat <-mr.ios::ios(exp=exp_dat, bg=bg_dat)

e <- list()
invisible(capture.output(e <- lapply(1:100, function(x) {

  ios_dat$SNP <- sample(ios_dat$SNP)
  ios_radialMR <- mr.ios(dat=dat, ios = ios_dat, ios_type="ios1_95", alpha = 0.05, 6, tol = 0.0001)
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

ios_dat <-mr.ios::ios(exp=exp_dat, bg=bg_dat)

f <- list()
invisible(capture.output(f <- lapply(1:100, function(x) {

  ios_dat$SNP <- sample(ios_dat$SNP)
  ios_radialMR <- mr.ios(dat=dat, ios = ios_dat, ios_type="ios1_max", alpha = 0.05, 6, tol = 0.0001)
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
#ios1_sim <- as.data.frame(lapply(ios1_sim, unlist))
names(ios1_sim)[8] <- "P_val"

#Define the class if it is not specified (null)
factor_cols <- c("exposure","outcome", "method")
ios1_sim[factor_cols] <- lapply(ios1_sim[factor_cols], as.factor)
numeric_cols <- c("nsnp", "Estimate", "Std.Error", "t.value", "P_val", "Q", "Q_pval", "permutation")
ios1_sim[numeric_cols] <- lapply(ios1_sim[numeric_cols], as.numeric)


#------------------------------------------------------------------------
#Plots
#------------------------------------------------------------------------

#original IVW Radial MR
drmr <- RadialMR::format_radial(dat$beta.exposure, dat$beta.outcome, dat$se.exposure, dat$se.outcome, dat$SNP, ios_dat$ios1_mean, ios_dat$SNP)
ormr <- RadialMR::ivw_radial(drmr, alpha = 0.05, weights = 3, tol = 0.0001, external_weight = FALSE)



#plot - ios1 mean
high <- ios1_sim %>%
  group_by(method) %>%
  filter(grepl("mean", method)) %>%
  arrange(Q) %>%
  mutate(id = row_number()) %>%
  filter(permutation < 1)

p1 <- ios1_sim %>%
  group_by(method) %>%
  arrange(Q) %>%
  #mutate(id = row_number()) %>%
  mutate(highperm = permutation == 0) %>%
  filter(grepl("mean", method)) %>%
  ggplot(aes(y=Q, x=Estimate, group=as.factor(method)), colour=as.factor(method)) +
  geom_smooth(method='lm', formula = y ~ x, se=FALSE, size=0.5) +
  geom_point(aes(colour=as.factor(method)), colour = "coral1", alpha = 1) +
  #geom_line(aes(colour=as.factor(method))) +
  geom_point(data=high, aes(y=Q, x=Estimate), colour='black', size=1) +
  geom_hline(yintercept=876.6109, linetype="dashed", color = "grey") +
  labs(x="Estimate", y="Q statistics", colour="Method")

#plot - ios1 sd
high <- ios1_sim %>%
  group_by(method) %>%
  filter(grepl("sd", method)) %>%
  arrange(Q) %>%
  mutate(id = row_number()) %>%
  filter(permutation < 1)

p2 <- ios1_sim %>%
  group_by(method) %>%
  arrange(Q) %>%
  #mutate(id = row_number()) %>%
  mutate(highperm = permutation == 0) %>%
  filter(grepl("sd", method)) %>%
  ggplot(aes(y=Q, x=Estimate, group=as.factor(method)), colour=as.factor(method)) +
  geom_smooth(method='lm', formula = y ~ x, se=FALSE, size=0.5) +
  geom_point(aes(colour=as.factor(method)), colour = "deepskyblue",alpha = 1) +
  #geom_line(aes(colour=as.factor(method))) +
  geom_point(data=high, aes(y=Q, x=Estimate), colour='black', size=1) +
  geom_hline(yintercept=876.6109, linetype="dashed", color = "grey") +
  labs(x="Estimate", y="Q statistics", colour="Method")

#plot - ios1 median
high <- ios1_sim %>%
  group_by(method) %>%
  filter(grepl("median", method)) %>%
  arrange(Q) %>%
  mutate(id = row_number()) %>%
  filter(permutation < 1)

p3 <- ios1_sim %>%
  group_by(method) %>%
  arrange(Q) %>%
  #mutate(id = row_number()) %>%
  mutate(highperm = permutation == 0) %>%
  filter(grepl("median", method)) %>%
  ggplot(aes(y=Q, x=Estimate, group=as.factor(method)), colour=as.factor(method)) +
  geom_smooth(method='lm', formula = y ~ x, se=FALSE, size=0.5) +
  geom_point(aes(colour=as.factor(method)), colour = "darkolivegreen3", alpha = 1) +
  #geom_line(aes(colour=as.factor(method))) +
  geom_point(data=high, aes(y=Q, x=Estimate), colour='black', size=1) +
  geom_hline(yintercept=876.6109, linetype="dashed", color = "grey") +
  labs(x="Estimate", y="Q statistics", colour="Method")

#plot - ios1 max
high <- ios1_sim %>%
  group_by(method) %>%
  filter(grepl("max", method)) %>%
  arrange(Q) %>%
  mutate(id = row_number()) %>%
  filter(permutation < 1)

p4 <- ios1_sim %>%
  group_by(method) %>%
  arrange(Q) %>%
  #mutate(id = row_number()) %>%
  mutate(highperm = permutation == 0) %>%
  filter(grepl("max", method)) %>%
  ggplot(aes(y=Q, x=Estimate, group=as.factor(method)), colour=as.factor(method)) +
  geom_smooth(method='lm', formula = y ~ x, se=FALSE, size=0.5) +
  geom_point(aes(colour=as.factor(method)), colour ="darkgoldenrod2", alpha = 1) +
  #geom_line(aes(colour=as.factor(method))) +
  geom_point(data=high, aes(y=Q, x=Estimate), colour='black', size=1) +
  geom_hline(yintercept=876.6109, linetype="dashed", color = "grey") +
  labs(x="Estimate", y="Q statistics", colour="Method")

#plot - ios1 iqr
high <- ios1_sim %>%
  group_by(method) %>%
  filter(grepl("iqr", method)) %>%
  arrange(Q) %>%
  mutate(id = row_number()) %>%
  filter(permutation < 1)

p5 <- ios1_sim %>%
  group_by(method) %>%
  arrange(Q) %>%
  #mutate(id = row_number()) %>%
  mutate(highperm = permutation == 0) %>%
  filter(grepl("iqr", method)) %>%
  ggplot(aes(y=Q, x=Estimate, group=as.factor(method)), colour=as.factor(method)) +
  geom_smooth(method='lm', formula = y ~ x, se=FALSE, size=0.5) +
  geom_point(aes(colour=as.factor(method)), colour = "darkmagenta", alpha = 1) +
  #geom_line(aes(colour=as.factor(method))) +
  geom_point(data=high, aes(y=Q, x=Estimate), colour='black', size=1) +
  geom_hline(yintercept=876.6109, linetype="dashed", color = "grey") +
  labs(x="Estimate", y="Q statistics", colour="Method")

#plot - ios1 95
high <- ios1_sim %>%
  group_by(method) %>%
  filter(grepl("95", method)) %>%
  arrange(Q) %>%
  mutate(id = row_number()) %>%
  filter(permutation < 1)

p6 <- ios1_sim %>%
  group_by(method) %>%
  arrange(Q) %>%
  #mutate(id = row_number()) %>%
  mutate(highperm = permutation == 0) %>%
  filter(grepl("95", method)) %>%
  ggplot(aes(y=Q, x=Estimate, group=as.factor(method)), colour=as.factor(method)) +
  geom_smooth(method='lm', formula = y ~ x, se=FALSE, size=0.5) +
  geom_point(aes(colour=as.factor(method)), colour = "darkorange", alpha = 1) +
  #geom_line(aes(colour=as.factor(method))) +
  geom_point(data=high, aes(y=Q, x=Estimate), colour='black', size=1) +
  geom_hline(yintercept=876.6109, linetype="dashed", color = "grey") +
  labs(x="Estimate", y="Q statistics", colour="Method")


library(cowplot)
ios1_p <- plot_grid(p1, p2, p3, p4, p5, p6, labels = "AUTO")


