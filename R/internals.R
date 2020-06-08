# .assignNodes ####
#' .assignNodes - assign jobs to nodes
#'
#' @author Stephen Tueller \email{stueller@@rti.org}
#'
#' @keywords internal

.assignNodes <- function(condGrid, dirNames, nodeNames, seed)
{
  cores       <- lapply(nodeNames, function(x) as.numeric(x[2]))
  totalCores  <- sum(unlist(cores))
  jobsPerCore <- ceiling( nrow(condGrid)/totalCores )

  assignCores <- list()
  nameCores   <- list()
  for(i in seq_along(cores))
  {
    nm <- names(cores)[i]
    nn <- nodeNames[[i]][1]
    nc <- unlist(cores[i])
    jobsThisNode <- ceiling(jobsPerCore*nc)
    assignCores[[nm]] <- rep(nm, jobsThisNode)
    nameCores[[nn]]   <- rep(nn, jobsThisNode)
  }
  assignCores <- unlist(assignCores)[1:nrow(condGrid)]
  nameCores   <- unlist(nameCores)[1:nrow(condGrid)]

  seeds <- PersonAlyticsPower:::.makeSeeds(seed, nrow(condGrid))

  condGrid <- data.frame(jobName=names(assignCores),
                         node=assignCores,
                         nodeName=nameCores,
                         .makeDirs(condGrid, dirNames),
                         seeds=seeds)
  row.names(condGrid) <- NULL

  return(condGrid)
}

# .makeDirs ####
#' .makeDirs - create directories and return names
#'
#' @author Stephen Tueller \email{stueller@@rti.org}
#'
#' @keywords internal

# this is study1 centric, need to generalize and give user options
.makeDirs <- function(condGrid, dirNames)
{
  cdirNames <- unlist(dirNames)
  if( ! all(cdirNames %in% names(condGrid)) )
  {
    wdirNames <- cdirNames[which(! cdirNames %in% names(condGrid))]
    stop("\n`dinNames` includes the variables\n\n", paste(wdirNames, collapse=",\n"),
         "\n\nbut these variables are not in `conditions`.\n\n")
  }
  dir1 <- do.call("paste", c(condGrid[,dirNames[[1]]], sep="_"))
  dir2 <- do.call("paste", c(condGrid[,dirNames[[2]]], sep="_"))
  condGrid$dir <- paste(getwd(), dir1, dir2, sep='/')

  message("\nCreating directories in ", getwd(),
          ".\nThis may take several minutes.\n\n")
  invisible( lapply(as.list(unique(dir1)), dir.create) )
  invisible( lapply(as.list(condGrid$dir), dir.create) )

  return(condGrid)
}


# .scanNode ####
#' .scanNode - scan directories for this node and check how many are done
#'
#' @author Stephen Tueller \email{stueller@@rti.org}
#'
#' @keywords internal

.scanNode <- function(self, thisNode, theseJobs, ext="complete")
{
  # in each directory we need to find
  # 1. the *.RData or *.csv file produced by PersonAlytics
  # 2. the *.txt file produced by PersonAlyticsPower
  # 3. the ????? file produced by PersonAlyticsPower (TBD, need something machine readable)
  .scan <- function(x, ext)
  {
    theFiles <- dir(x, full.names = TRUE)
    if( length(theFiles) > 0 )
    {
      # this could be a user option instead of fixed to 'rdata'
      exts <- tools::file_ext(theFiles)
      if( any( tolower(exts) %in% ext ) ) return( 1 )
      else return( 0 )
    }
    else return( 0 )
  }
  isDone <- unlist( lapply(as.list(theseJobs), .scan, ext=ext) )
  message("\n", sum(isDone), " out of ", length(isDone),
          " conditions are complete for node ", thisNode,
          " (", round(100*sum(isDone)/length(isDone)), "%).\n")
  return(isDone)
}

