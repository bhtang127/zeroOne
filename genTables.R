##First try a bunch of tables of the same size
I = 10; J = 10
noTables = 1e4
noSim = 1e5

source("zeroOne.R")

file = "data.txt"

for (i in 1 : noTables){
  if(i %% 10 == 0) cat(i," ")
  # r2 = runif(1)^2; r1 = runif(1)^2
  # tbl = sample_table(I, r1, r2)
  prob = plogis(matrix(rnorm(I*J), I, J))
  tbl = (matrix(runif(I*J),I,J) <= prob)^2

  ## order by rows and columns
  tbl = tbl[order(rowSums(tbl), decreasing = TRUE), 
            order(colSums(tbl), decreasing = TRUE)]
  
  ## calculate the Pvalue
  pv = simulation(tbl, noSim)

  dataLine = paste(
      c(
          as.character(as.vector(tbl)),
          as.character(round(pv, 5))
       ),
      collapse = " ")

  write(dataLine, file = file, append = TRUE)
}

cat("\n")

dat = as.matrix(read.table("data.txt"))
dat = dat[dat[,102] > 0 & dat[,102] < 1,]
ind = sample(dim(dat)[1], 1000)
for(i in 1:1000){
  if(i %% 10 == 0) cat(i," ")
  ps = c()
  for(k in 1:20){
    pfull = simulation(matrix(dat[ind[i],1:100],10,10), 
                       (noSim-5000)/4+5000)[2]
    ps = c(ps, pfull)
  }
  dataLine = paste(
    c(
      as.character(ind[i]-1),
      as.character(dat[ind[i],1:100]),
      as.character(round(ps, 5))
    ),
    collapse = " ")
  
  write(dataLine, file = "data_var.txt", append = TRUE)
}
cat("\n")


zeroOne = function(A, nosim = 1000){
  ## Just useful for sampling 
  rows = 1 : nrow(A)
  cols = 1 : ncol(A)
  
  ## The observed test statsitic
  tsobs = testStat(A)
  
  ## The test condition
  test1 = c(1, 0, 0, 1)
  test2 = c(0, 1, 1, 0)
  
  tsCurrent = tsobs
  
  pvalue = 1 / (nosim + 1)
  
  halfP = NA
  for (sim in 1 : nosim){
    i = sample(rows, 2)
    j = sample(cols, 2)
    ## the tetrad subtable
    subTab = A[i, j]
    ## check if it's one of the two types
    if (all(subTab == test1)) {
      A[i, j] = test2
      tsCurrent = testStat(A)
    }
    else if (all(subTab == test2)) {
      A[i, j] = test1
      tsCurrent = testStat(A)
    }
    #else no change
    pvalue = pvalue + (tsCurrent >= tsobs) / (nosim + 1)
    if (sim == round(nosim / 2)) halfP = pvalue * (nosim + 1) / round(nosim / 2)
  }  
  
  c(halfP, pvalue)
}

