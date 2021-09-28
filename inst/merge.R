rdatas <- c(
  "R:/PaCCT/02 Clients/NIH/0216822.000.001 R21 ICT/03 Simulation Studies/Study2/study2-RTI-102921.RData",
  "R:/PaCCT/02 Clients/NIH/0216822.000.001 R21 ICT/03 Simulation Studies/Study2/fullreplication/study2replication.RData",
  "R:/PaCCT/02 Clients/NIH/0216822.000.001 R21 ICT/03 Simulation Studies/Study2/fullreplication2/study2replication2.RData"
)

condgrids <- list()
for(i in seq_along(rdatas))
{
  load(rdatas[[i]])
  condgrids[[i]] <- t2$condGrid
  rm(t2)
}
condgrid <- do.call(rbind, condgrids)

