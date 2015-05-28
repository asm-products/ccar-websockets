library ( pbdMPI , quiet = TRUE )
init ()
n.gbd <- sample (1:10 , size =1)
sm <- allreduce ( n.gbd ) # default op is ’sum ’
print ( sm )
gt <- allgather ( n.gbd )
print ( gt )
finalize ()
