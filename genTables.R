##First try a bunch of tables of the same size
I = 5; J = 5
noTables = 10000
noSim = 100000

source("zeroOne.R")

file = "data.txt"



for (i in 1 : noTables){
  tbl = matrix(sample(0 : 1, I * J, replace = TRUE), I, J)

  ## order by rows and columns
  tbl = tbl[order(rowSums(tbl), decreasing = TRUE), 
            order(colSums(tbl), decreasing = TRUE)]
  
  ## calculate the Pvalue
  pv = zeroOne(tbl, 10000)

  dataLine = paste(
      c(
          as.character(as.vector(tbl)),
          as.character(round(pv, 3))
       ),
      collapse = " ")

  write(dataLine, file = file, append = TRUE)

}


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
  
  list(pvalue=pvalue, halfP=halfP)
}

