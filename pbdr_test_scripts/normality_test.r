## A script to test normality of data.
library(pbdDEMO, quiet = TRUE)



##Serial read..but we will fix this
init.grid()
if(comm.rank() == 0){
	data <- readLines("http://www.smart-earn.com/gj/5/16")
}else {
	data <- NULL
}

x <- as.ddmatrix(data)
print(x)

finalize()