## Not run:
# Save code in a file "demo.r" and run in 4 processors by
# > mpiexec -np 4 Rscript demo.r
### Setup environment.
library(pbdDEMO, quiet = TRUE)
### Generate an example data.
N.gbd <- 5 * (comm.rank() * 2)
X.gbd <- rnorm(N.gbd * 3)
dim(X.gbd) <- c(N.gbd, 3)
comm.cat("X.gbd[1:5,]\n", quiet = TRUE)
comm.print(X.gbd[1:5,], rank.print = 1, quiet = TRUE)
bal.info <- balance.info(X.gbd)
new.X.gbd <- load.balance(X.gbd, bal.info)
org.X.gbd <- unload.balance(new.X.gbd, bal.info)
comm.cat("org.X.gbd[1:5,]\n", quiet = TRUE)
comm.print(org.X.gbd[1:5,], rank.print = 1, quiet = TRUE)
if(any(org.X.gbd - X.gbd != 0)){
cat("Unbalance fails in the rank ", comm.rank(), "\n")
}
### Quit.
finalize()
## End(Not run)