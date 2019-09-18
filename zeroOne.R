## Performs MCMC P-value calculation for 0-1 tables
## from Darwin's Finch data



testStat = function(A){
  m = nrow(A)
  AAT = A %*% t(A)
  2 * sum( AAT[upper.tri(AAT)] ^ 2 ) / m / (m - 1)
}

# bootstrap_pvalues = function(ts, value, niter){
#   ps = c()
#   for(it in 1:niter){
#     ts_resample = sample(ts, length(ts), replace = TRUE)
#     ps = c(ps, mean(ts_resample >= value))
#   }
#   list(pvalue = mean(ps), std = sqrt(var(ps)))
# }

simulation = function(A, numsim = 5e4, burnin = 5e3){
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
  p_half = mean(ts[1:half_size] >= ts_init)
  p_full = mean(ts >= ts_init)
  
  c(p_half, p_full)
}

sample_table = function(m=10, r1=0.4, r2=0.7){
  theta = 10*rgamma(m,2,1); lambda = 10*rgamma(m,4,2)
  zeta = outer(theta, lambda, '-')
  ptable = exp(zeta) / (1+exp(zeta))
  
  samp = (matrix(runif(m^2),m,m) <= ptable)
  I = sample(m, floor(m*r1)); J = sample(m, floor(m*r1))
  cols = sample(m, floor(m*r2)) 
  samp[I, cols] = 1 - samp[J, cols]
  
  samp
}

# p_half = c(); p_full = c()
# for(i in 1:10){
#   cat(i," ")
#   res = simulation(sample_table(n = 0.4, r = 0.8), numsim = 1e5)
#   # res = simulation(A, numsim = 4e5)
#   p_half = c(p_half, res[1])
#   p_full = c(p_full, res[2])
# }
# cat("\n")
# mean(p_half); 1.96*sqrt(var(p_half))
# mean(p_full); 1.96*sqrt(var(p_full))
