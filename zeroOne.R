## Performs MCMC P-value calculation for 0-1 tables
## from Darwin's Finch data



testStat = function(A){
  m = nrow(A)
  AAT = A %*% t(A)
  2 * sum( AAT[upper.tri(AAT)] ^ 2 ) / m / (m - 1)
}

bootstrap_pvalues = function(ts, value, niter){
  ps = c()
  for(it in 1:niter){
    ts_resample = sample(ts, length(ts), replace = TRUE)
    ps = c(ps, mean(ts_resample >= value))
  }
  list(pvalue = mean(ps), std = sqrt(var(ps)))
}

simulation = function(A, numsim = 5e4, 
                      burnin = 5e3, numbs = 1e3){
  test1 = c(1, 0, 0, 1); test2 = c(0, 1, 1, 0)
  rownum = nrow(A); colnum = ncol(A)
  ts = c(); ts_init = testStat(A)
  for(sim in 1:numsim){
    i = sample(rownum, 2); j = sample(colnum, 2)
    if (all(A[i, j] == test1)) {A[i, j] = test2}
    else if (all(A[i, j] == test2)) {A[i, j] = test1}
    # else nothing
    ts = c(ts, testStat(A))
  }
  ts = ts[burnin:numsim]
  half_size = floor(length(ts) / 4)
  res_half = bootstrap_pvalues(ts[1:half_size], ts_init, numbs)
  res_full = bootstrap_pvalues(ts, ts_init, numbs)
  
  list(p_half = res_half$pvalue, p_full = res_full$pvalue,
       ci_half = 1.96*res_half$std, ci_full = 1.96*res_full$std)
}

sample_table = function(l){
  theta = 10*rgamma(7,2,2); lambda = 10*rgamma(7,3,1)
  nullhypo = outer(theta, lambda, '-')
  zeta = nullhypo + l*matrix(rnorm(49), 7, 7)
  ptable = exp(zeta) / (1+exp(zeta))
  
  (matrix(runif(49),7,7) <= ptable)^2
}


