#' PersonAlyticsSim: Tools for large scale simulations using PersonAlyticsPower
#'
#' @details
#' PersonAlyticsSim has two basic functions: \code{\link{simDeploy}} and
#' \code{\link{simCollect}}. The first aids with setting up and running a
#' simulations, while the second is used to monitor progress and summarize and
#' plot the results.
#'
#' @docType package
#' @name PersonAlyticsSim
#' @author Stephen Tueller \email{stueller@@rti.org}
#'
NULL

# .active ####
#' .active - active bindings for Sim R6 class definitions
#'
#' @author Stephen Tueller \email{stueller@@rti.org}
#'
#' @keywords internal
#'
.active <- function()
{
  list(

    simName = function(value)
    {
      if( missing(value) ){ private$.simName }
      else
      {
        private$.simName <- value
        self
      }
    },

    seed = function(value)
    {
      if( missing(value) ){ private$.seed }
      else
      {
        stop("`seed` cannot be updated, use a new call to Sim$new().")
        self
      }
    },

    B = function(value)
    {
      if( missing(value) ){ private$.B }
      else
      {
        private$.B <- value
        self
      }
    },

    conditions = function(value)
    {
      if( missing(value) ){ private$.conditions }
      else
      {
        stop("`conditions` cannot be updated, use a new call to Sim$new().")
        self
      }
    },

    dirNames = function(value)
    {
      if( missing(value) ){ private$.dirNames }
      else
      {
        stop("`dirNames` cannot be updated, use a new call to Sim$new().")
        self
      }
    },

    type = function(value)
    {
      if( missing(value) ){ private$.type }
      else
      {
        private$.type <- value
        self
      }
    },

    nodeNames = function(value)
    {
      if( missing(value) ){ private$.nodeNames }
      else
      {
        stop("`nodeNames` is read-only.")
        self
      }
    },

    condNames = function(value)
    {
      if( missing(value) ){ private$.condNames }
      else
      {
        stop("`condNames` is read-only.")
        self
      }
    },

    condGrid = function(value)
    {
      if( missing(value) ){ private$.condGrid }
      else
      {
        stop("`condGrid` is read-only.")
      }
    },

    nConditions = function(value)
    {
      if( missing(value) ){ private$.nConditions }
      else
      {
        stop("`nConditions` is read-only.")
        self
      }
    },

    nAnalyses = function(value)
    {
      if( missing(value) ){ private$.nAnalyses }
      else
      {
        stop("`nAnalyses` is read-only.")
        self
      }
    },

    wd = function(value)
    {
      if( missing(value) ){ private$.wd }
      else
      {
        private$.wd <- value
        self
      }
    }

  )
}


# start of Sim class ####
#' \code{Sim} class generator
#'
#' @docType class
#' @author Stephen Tueller \email{stueller@@rti.org}
#'
#' @export
#' @import PersonAlyticsPower
#' @import ggplot2
#'
#' @details
#' Some text here
#'
#' @field simName Charecter. A name for your simulation study.
#'
#' @section Methods:
#' \describe{
#'   \item{print}
#' }
#'
#' @examples
#'
#' \dontrun{
#'
#' }
#'
Sim <- R6::R6Class("Sim",

                   # private ####
                   private = list(
                     .simName     = NULL,
                     .seed        = NULL,
                     .B           = NULL,
                     .conditions  = NULL,
                     .dirNames    = NULL,
                     .type        = NULL,
                     .nodeNames   = NULL,
                     .condNames   = NULL,
                     .condGrid    = NULL,
                     .nConditions = NULL,
                     .nAnalyses   = NULL,
                     .wd          = NULL

                   ), # end of private

                   # active ####
                   active = .active(),

                   # public ####
                   public = list(
                     # initialize ####
                     initialize = function
                     (
                       simName                             ,
                       seed                                ,
                       B                                   ,
                       conditions = list()                 ,
                       dirNames   = list()                 ,
                       type       = c('param', 'nonpar')   ,
                       nodeNames  = Sys.info()["nodename"]

                     )
                     {
                       # there is a temptation to write a function to validate inputs here,
                       # but that is already done elsewhere, we might as well expand the grid
                       # and run the checks already in place
                       condNames <- lapply(conditions, names)
                       condGrid  <- expand.grid(condNames)

                       # divide the conditions across the nodes
                       condGrid <- .assignNodes(condGrid, dirNames, nodeNames, seed)

                       # derive number of time points (ntp) from phases
                       ntp <- as.character(condGrid$phases)
                       ntp <- strsplit(ntp, "_")
                       ntp <- lapply(ntp, function(x) c(substring(x[1], 2, 100), x[2]))
                       ntp <- lapply(ntp, function(x) sum(as.numeric(x)))
                       condGrid$ntp <- unlist( ntp )

                       # calculate some statistics
                       nConditions <- nrow(condGrid)
                       nAnalyses   <- conditions$B * nConditions

                       # populate private
                       private$.simName     <- simName
                       private$.seed        <- seed
                       private$.B           <- B
                       private$.conditions  <- conditions
                       private$.type        <- type
                       private$.nodeNames   <- nodeNames
                       private$.condNames   <- condNames
                       private$.condGrid    <- condGrid
                       private$.nConditions <- nConditions
                       private$.nAnalyses   <- nAnalyses
                       private$.wd          <- getwd()

                       # this save is only partially useable, required saving outside by user
                       # save yourself
                       #save(self, file=paste(simName, "RData", sep='.'))
                       # the above may not work, consider
                       #save.image(file=paste(simName, "RData", sep='.'))
                     },

                     # print ####
                     print = function
                     (
                       ...
                     )
                     {
                       message("\nPersonAlyticPower simulation with ",
                               prettyNum(self$nConditions, big.mark = ','),
                               " conditions,",
                               "\neach with ", self$conditions$B,
                               " replications for a total of ",
                               prettyNum(self$nAnalyses, big.mark = ','),
                               "\nanalyses.")

                       message("\nThis simulation is set up to run on the following computers:\n",
                               paste(self$nodeNames, collapse='\n'))

                       return(self)
                     },

                     emptyDirs = function
                     (

                     )
                     {
                       eD <- function(x)
                       {
                         files <- dir(x, all.files = TRUE, full.names = TRUE, recursive = TRUE)
                         if(length(files)>0) file.remove(files)
                       }
                       lapply(self$condGrid$dir, eD)
                     },

                     # start ####
                     # -- the following does NOT work to add documentiation for methods --
                     #' @param ext
                     #' @param run1
                     #' @param cleanup
                     #' @param checkOnly
                     start = function
                     (
                       ext       = 'complete' ,
                       run1      = NULL       ,
                       cleanup   = TRUE       ,
                       checkOnly = FALSE      ,
                       debug     = FALSE      ,
                       ...
                     )
                     {
                       # check wd in case a prior run failure left us in a subdir
                       setwd(self$wd)
                       reRun <- FALSE

                       # check the current node
                       thisNode  <- Sys.info()["nodename"]
                       theseJobs <- self$condGrid$dir[which(self$condGrid$nodeName == thisNode)]
                       if(length(theseJobs)==0)
                       {
                         message("\nYou are currently on machine ", thisNode, " but the machine",
                              "\nnames include\n\n",
                              paste(1:length(self$nodeNames), ": ",
                                    lapply(self$nodeNames, function(x) x[[1]]), collapse = '\n'),
                              "\n\n"
                         )
                        subNode <- readline(prompt=paste("Which machine's jobs do you want ",
                                   thisNode, "to work on (enter an integer)?\n\n", sep='') )

                        if(suppressWarnings(is.na(as.numeric(subNode))))
                        {
                          stop("\nThe value you gave was not a number.\n\n")
                        }
                        if(!round(as.numeric(subNode))==as.numeric(subNode))
                        {
                          stop("\nThe value you gave was not an integer.\n\n")
                        }
                        subNode <- self$nodeNames[[as.numeric(subNode)]][[1]]
                        theseJobs <- self$condGrid$dir[which(self$condGrid$nodeName == subNode)]
                       }

                       # determine which jobs to run
                       if( is.null(run1) )
                       {
                         # identify the current computer and scan to see what is done
                         isDone    <- .scanNode(self, thisNode, theseJobs, ext)
                         unDone    <- which(self$condGrid$dir %in% theseJobs[!isDone])

                         # if all assigned jobs are done, take on one quarter of the remaining jobs
                         if( length(unDone) == 0 )
                         {
                           pleaseRun <- self$progress()
                           lpr       <- length(pleaseRun)
                           unDone    <- which(self$condGrid$dir %in% pleaseRun[ceiling(.25*lpr):lpr])
                         }
                       }

                       # if only one condition is requested
                       if( !is.null(run1) )
                       {
                         unDone <- which(self$condGrid$dir == run1)
                         if(length(unDone)==0)
                         {
                           stop("\nThe directory you requested is not in `condGrid$dir`. You gave:",
                                run1)
                         }
                         cleanup <- FALSE
                         reRun   <- TRUE
                       }

                       # set up progress bar - no good b/c other things printing to screen, including
                       # other progress bars
                       #pb <- utils::txtProgressBar(min = 0, max = length(unDone), style = 3)

                       # start running conditions that aren't done
                       for( i in unDone )
                       {
                         # progress bar
                         #utils::setTxtProgressBar(pb, i)

                         # get the directory name to use in labeling
                         dirfName <- self$condGrid$dir[i]
                         setwd(dirfName)
                         dirName  <- basename(self$condGrid$dir[i])

                         # only run if the folder is not complete, but allow for run1 reRuns
                         nowDone <- paste(dirName, "complete", sep='.')
                         if(reRun & file.exists(nowDone)) file.remove(nowDone)
                         if( !file.exists(nowDone) )
                         {
                           # create ICT object - note this will be missing many options, need generalization
                           groups <- self$conditions$groups[[self$condGrid$groups[i]]]
                           phases <- makePhase( self$conditions$phases[[self$condGrid$phases[i]]] )
                           propErrVar <- self$conditions$propErrVar[[self$condGrid$propErrVar[i]]]
                           if( length(self$conditions$errors)>0 )
                           {
                             error <- self$conditions$errors[[self$condGrid$errors[i]]]
                           }
                           else error <- armaErr$new(model = list(ar=c(.5), ma=c(.5)))
                           thisICT <- polyICT$new(
                             groups     = groups     ,
                             phases     = phases     ,
                             propErrVar = propErrVar ,
                             error      = error
                           )

                           # update the Input Matrix, this is hard coded to Sim 1, generalize
                           thisICT$inputMat[,c('Mean0', 'Mean1')] <- self$conditions$d[[
                             self$condGrid$d[i]
                             ]]

                           if( self$type == "nonpar" )
                           {
                             # create data
                             Data <- thisICT$makeData(seed = self$condGrid$seeds[i])
                             save(Data, file = "Data.RData")

                             # screen check
                             message("\nN=", nrow(Data), " participants were simulated.\n\n")

                             # create a palatyic object for plotting
                             pa <- Palytic$new(Data, 'id', 'y', 'Time', 'phase')

                             # extract correlation
                             ar          <- length(thisICT$error$model$ar[thisICT$error$model$ar!=0])
                             ma          <- length(thisICT$error$model$ma[thisICT$error$model$ma!=0])
                             correlation <- paste('corARMA(p=', ar, ',q=', ma, ')', sep='')

                             # save the sample graphic and the designCheck graphic
                             pdf('designCheck.pdf', onefile = F,
                                 width = 11, height=8.5)
                             print( suppressMessages(thisICT$designCheck(title =
                                      "Large Sample (Theoretical)")) )
                             dev.off()

                             pdf('Population.pdf', onefile = F,
                                 width = 11, height=8.5)
                             print( suppressMessages( pa$plot(title=
                                      "Finite Sample (Actual)")  ) )
                             dev.off()

                             # run sim
                             if(!checkOnly)
                             {
                               if( debug )
                               {
                                 print(
                                 list(outFile     = c(dirName, "RData")           ,
                                      B           = self$B                        ,
                                      dataFile    = "Data.RData"                  ,
                                      sampleSizes = self$conditions$sampSizes[[
                                        self$condGrid$sampSizes[i]]]              ,
                                      prompt      = FALSE                         ,
                                      alignPhase  = self$condGrid$alignPhase[[i]] ,
                                      fpc         = self$conditions$groups[[
                                        self$condGrid$groups[[i]]]]               ,
                                      correlation = correlation                   )
                                 )
                               }
                               ICTpower(outFile     = c(dirName, "RData")           ,
                                        B           = self$B                        ,
                                        dataFile    = "Data.RData"                  ,
                                        sampleSizes = self$conditions$sampSizes[[
                                          self$condGrid$sampSizes[i]]]              ,
                                        prompt      = FALSE                         ,
                                        alignPhase  = self$condGrid$alignPhase[[i]] ,
                                        fpc         = self$conditions$groups[[
                                        self$condGrid$groups[[i]]]]                 ,
                                        correlation = correlation                   ,
                                        debugforeach= debug                         )
                             }
                           }

                           if( self$type == "param" )
                           {
                             # run sim
                           }

                           # leave a file indicating completion
                           cat(1, file = nowDone)

                           if( cleanup )
                           {
                             rd <- dir(dirfName, glob2rx("*.RData"), full.names = TRUE)
                             rt <- dir(dirfName, glob2rx("*-RTI-*"), full.names = TRUE)
                             file.remove("REMLlme.txt", rd, rt)
                           }
                         } # end of if( !file.exists(nowDone) )
                       } # end of for( i in unDone )

                       setwd(self$wd)
                       self

                     },

                     # cleandirs ####
                     cleandirs = function()
                     {
                       .cleanup <- function(x)
                       {
                         rd <- dir(x, glob2rx("*.RData"), full.names = TRUE)
                         rt <- dir(x, glob2rx("*-RTI-*"), full.names = TRUE)
                         txt <- paste(x, "REMLlme.txt", sep='/')
                         capture.output( file.remove(txt, rd, rt), file = 'NUL' )
                       }
                       lapply(self$condGrid$dir, .cleanup)
                     },

                     # progress ####
                     progress = function
                     (
                       ext = 'complete',
                       ...
                     )
                     {
                       nodes     <- unique(  self$condGrid$nodeName )
                       pleaseRun <- list()
                       for(i in seq_along(nodes))
                       {
                         theseJobs <- self$condGrid$dir[which(self$condGrid$nodeName == nodes[i])]
                         done      <- .scanNode(self, nodes[i], theseJobs, ext)
                         pleaseRun[[nodes[i]]] <- theseJobs[!done]
                       }
                       invisible( unlist(pleaseRun) )
                     },

                     # summary ####
                     summary = function
                     (
                       ...
                     )
                     {
                       lapply(self$condGrid[, names(self$conditions)], table)
                     },

                     # summarize ####
                     summarize = function
                     (
                       fpc = TRUE  ,
                       raw = FALSE ,
                       ...
                     )
                     {
                       # prevent a slow rerun
                       fnm  <- paste(self$simName, "SimSummary.RData", sep="_")
                       if( file.exists(fnm) )
                       {
                         message('\n`$summarize()` has already been run, the file',
                                 '\n`SimSummary.RData` already exists, and `$summarize()`',
                                 '\ncan take several minutes to run.')
                         ans <- readline(prompt =
                                           'Are you sure you want to rerun `$summarize()`? y/n: ')
                         if(tolower(ans)=='n') stop('`$summarize()` cancelled.')
                       }

                       fnmr  <- paste(self$simName, "SimSummaryRaw.RData", sep="_")
                       if( file.exists(fnmr) )
                       {
                         message('\n`$summarize(raw=TRUE)` has already been run, the file',
                                 '\n`SimSummary.RData` already exists, and `$summarize(raw=TRUE)`',
                                 '\ncan take several minutes to run.')
                         ans <- readline(prompt =
                                           'Are you sure you want to rerun `$summarize(raw=TRUE)`? y/n: ')
                         if(tolower(ans)=='n') stop('`$summarize(raw=TRUE)` cancelled.')
                       }

                       # get the file names
                       panames <- function(x)
                       {
                         dir(x, glob2rx("*PersonAlytic*.csv"), full.names = TRUE)
                       }
                       panames <- lapply( as.list(self$condGrid$dir), panames)

                       # horizontal line function
                       .hl <- function()
                       {
                         paste(paste(rep("\u2500", 80), collapse=''), '\n')
                       }

                       if(!raw)
                       {
                         powerReports <- function(x, alpha=.05, file=NULL, saveReport=F, fpc=fpc)
                         {
                           if(length(x)==1)
                           {
                             paout <- read.csv(x)
                             paout <- PersonAlyticsPower:::powerReport(paout, alpha, file, saveReport,
                                                                       fpc, FALSE)
                             nms   <- expand.grid(row.names(paout), names(paout))
                             nms   <- data.frame(lapply(nms, as.character), stringsAsFactors = FALSE)
                             nms   <- paste(nms[,1], nms[,2], sep = '_')
                             paout <- data.frame(t(unlist(c(paout))))
                             names(paout) <- nms

                             return( paout )
                           }
                           if(length(x)==0)
                           {
                             return( NULL )
                           }
                         }
                         powerReportsRaw <- lapply(panames, powerReports, fpc=FALSE)
                         if(fpc) powerReportsFPC <- lapply(panames, powerReports, fpc=fpc)
                         for(i in seq_along(powerReportsRaw))
                         {
                           powerReportsRaw[[i]]$name <- basename(self$condGrid$dir[i])
                           powerReportsFPC[[i]]$name <- basename(self$condGrid$dir[i])
                           if(!is.data.frame(powerReportsRaw[[i]]))
                           {
                             powerReportsRaw[[i]] <- as.data.frame(powerReportsRaw[[i]])
                           }
                           if(!is.data.frame(powerReportsFPC[[i]]))
                           {
                             powerReportsFPC[[i]] <- as.data.frame(powerReportsFPC[[i]])
                           }
                         }
                         simSumm <- cbind(self$condGrid, plyr::rbind.fill(powerReportsRaw))
                         if(fpc) simSum <- cbind(simSumm, plyr::rbind.fill(powerReportsFPC))

                         # save the data
                         save(simSumm, file = fnm)
                       }

                       if( raw)
                       {
                         getPout <- function(x)
                         {
                           # extract paout and add condGrid columns
                           paout <- read.csv(x)
                           nms   <- names(self$condGrid)
                           w     <- which(self$condGrid$dir == dirname(x))
                           for(i in seq_along(nms))
                           {
                             paout[[nms[i]]] <- self$condGrid[w,i]
                           }

                           # drop extraneous columns to yield a smaller object with short
                           # read/write times
                           keepNms <- c(names(self$condGrid),
                                        names(paout)[grepl("*value*", names(paout), TRUE)],
                                        names(paout)[grepl("*std.error*", names(paout), TRUE)])

                           return(paout[,keepNms])
                         }
                         paout <- lapply(panames, getPout)
                         paout <- plyr::rbind.fill(paout)

                         save(paout, file = fnmr)
                       }


                       # return a discription of missingness, etc.,
                     },

                     glm = function
                     (
                       y                      ,
                       interactionOrder = 1   ,
                       alpha            = .05 ,
                       subset           = NULL,
                       subsetVarName    = NULL,
                       recode           = NULL,
                       ...
                     )
                     {
                       # if you get users, this should me left to the user to manage
                       memory.limit(9999e9)

                       fnmr  <- paste(self$simName, "SimSummaryRaw.RData", sep="_")
                       if(!file.exists(fnmr))
                       {
                         stop("\nThe file ", fnmr, " does not exist in the current directory.",
                              "\nFirst run $summarize(raw=TRUE), which can take several hours.")
                       }

                       if( file.exists(fnmr) )
                       {
                         if(!exists("paout"))
                         {
                           message("\nLoading raw data, this can take several minutes...\n")
                           load(fnmr, .GlobalEnv)
                         }
                         if(!is.null(recode))
                         {
                           if(!is.function(recode))
                           {
                             stop("\n`recode` must be a function that takes the file created by",
                                  "\n$summarize() as an argument.")
                           }
                           paout <- recode(paout)
                           rhs   <- paout$nms
                           paout <- paout$x
                         }

                         if( missing(y) )
                         {
                           stop("\n`y` is missing and must be  provided. Available p-value",
                                "\ncolumns are:\n\n",
                                paste(names(paout)[ grepl("*p.value*", names(paout)) ],
                                      collapse='\n')
                           )
                         }

                         paout[[y]] <- as.numeric( paout[[y]] <= alpha )

                         if(is.null(recode)) rhs <- names(self$conditions)

                         if(!is.null(subset)) rhs <- rhs[! rhs %in% subsetVarName]
                         rhs <- paste(rhs, collapse="+")

                         frm <- as.formula( paste(y, '~ (', rhs,
                                                  ifelse(interactionOrder>1,
                                                         paste(')^', interactionOrder),
                                                         paste(')'))
                         )
                         )

                         if(is.null(subset)) subset <- rep(TRUE, nrow(paout))
                         message("\nStarting model fitting. This can take several hours to ",
                                 "complete.")
                         mod0 <- glm(frm, family = 'binomial', data = paout, subset = subset)

                         # also consider
                         # https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4842399/

                         return(mod0)
                       }

                     },

                     # plot ####
                     plot = function
                     (
                       x             ,
                       y             ,
                       group         ,
                       facet  = NULL ,
                       subset = NULL ,
                       title  = ""   ,
                       data   = NULL ,
                       ...
                     )
                     {

                       setwd(self$wd)
                       summDone <- file.exists(paste(self$simName, "SimSummary.RData", sep="_"))
                       if(!summDone )
                       {
                         message("\nRun $summarize() before using $plot().\n\n")
                       }
                       ssnm <- paste(self$simName, "SimSummary.RData", sep="_")
                       if(is.null(data))
                       {
                         load(ssnm)
                       }
                       if(!is.null(data))
                       {
                         simSumm <- data; rm(data)
                       }

                       if(  is.null(subset) ) subset <- rep(TRUE, nrow(simSumm))
                       if( !is.null(subset) )
                       {
                         simSumm <- subset(simSumm, subset)
                       }

                       if( missing(y) | missing(x) | missing(group) )
                       {
                         message("Error:",
                   "\n\nThe user must provide `x``, `y``, and `group` where `x` is the horizontal",
                   "\naxis, y is the verticle axis and should be a variable with `power` in the name",
                   "\nand group is a grouping variable for separate lines. A `facet` can also be",
                   "\nspecified for creating a grid of plots, and `subset` can be specified as a",
                   "\nlogical vector indicating which rows should be included. Below are the available",
                   "\ncolumn names for plotting. If you want to explore the data directly use",
                   "\n`load('", ssnm, "')` or `$conditions()` to see the levels in each condition.\n\n"
                         )
                         pnames <- c(names(simSumm)[grepl("*power*", names(simSumm))],
                                     names(self$conditions))
                         cat(paste(pnames, collapse = '\n'))
                         stopQuietly()
                       }

                       pd <- position_dodge(0.25)
                       g <-
                         ggplot(simSumm, aes_string(x=x, y=y, group=group, col=group)) +
                         stat_summary(fun.y = mean, geom = "line", position=pd, size=2) +
                         stat_summary(fun.data = mean_se, geom = "errorbar", position=pd, size=2) +
                         ggtitle(title)
                       if(! is.null(facet))
                       {
                         g <- g + facet_wrap(as.formula(paste(". ~ ", facet)))
                       }

                       return(g)

                     }

                   ) # end of public
)

#' stopQuietly
#' @export
stopQuietly <- function(...) {
  blankMsg <- sprintf("\r%s\r", paste(rep(" ", getOption("width")-1L), collapse=" "));
  stop(simpleError(blankMsg));
}