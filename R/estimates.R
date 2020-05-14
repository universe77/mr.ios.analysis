#------------------------------------------------------------------------
#functions
#------------------------------------------------------------------------
orig.est <- function(dat = dat, ios_type = "ios1_mean", weights =6){
  ios_dat <- ios(exp=exp_dat, bg=bg_dat)
  mr <- mr.ios(dat=dat, ios = ios_dat, ios_type=ios_type, alpha = 0.05, weights, tol = 0.0001)
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
