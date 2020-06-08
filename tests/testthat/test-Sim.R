context("test-Sim")
library(PersonAlyticsSim)

# 2019 R21 ICT simulation study #1 ####
test_that("input checking works",
{
  groups <- list(p3500 = c(group1=3500),
                 p1750 = c(group1=1750),
                  p200 = c(group1=200) )

  sampSizes <- c( seq(10,40,by=10), seq(50,100,by=20))[-1]
  names(sampSizes) <- paste('n', sampSizes, sep='')

  nObs   <- c(20,40,60,80,100)
  propBL <- c(5,10,20)/100
  # check, only set up with a minumum of 5 bl observations
  lapply(as.list(nObs), function(x) x*propBL)

  phases <- list(
    # nObs = 20
    p5_15  = c(5, 15),
    # nObs = 40
    p5_35  = c(5, 35),
    p8_32  = c(8, 32),
    # nObs = 60
    p5_55  = c(5, 55),
    p6_54  = c(6, 54),
    p12_48 = c(12, 48),
    # nObs = 80
    p5_75  = c(5, 75),
    p8_72  = c(8, 72),
    p16_64 = c(16, 64),
    # nObs = 100
    p5_95  = c(5, 95),
    p10_90 = c(10, 90),
    p20_80 = c(20, 80)
  )
  unlist(lapply(phases, sum))

  propErrVar <- list(
    pev10 = c(randFx=.90, res=.05 , mserr=.05),
    pev25 = c(randFx=.75, res=.125, mserr=.125),
    pev50 = c(randFx=.50, res=.25 , mserr=.25)
  )
  unlist(lapply(propErrVar, sum))

  d <- list(
    # jump
    smlJump = matrix(c(0,.2,0,0), 2, 2),
    medJump = matrix(c(0,.5,0,0), 2, 2),
    lrgJump = matrix(c(0,.8,0,0), 2, 2),

    # slope - multiply by 2 so that the average difference is the effect size
    smlSlope = matrix(c(0,0,0,.2*2), 2, 2),
    medSlope = matrix(c(0,0,0,.5*2), 2, 2),
    lrgSlope = matrix(c(0,0,0,.8*2), 2, 2)
  )

  alignPhase  <- c(mmta = 'none', piecewise = 'piecewise')

  nodeNames <- list(sdesk   = c(name="RTI-102807", cores=8),
                    sgreen  = c(name="RTI-104040", cores=8),
                    sorange = c(name="RTI-102921", cores=8)
  )

  dirNames <- list(c("d", "alignPhase"), c("groups", "sampSizes", "phases", "propErrVar"))

  # fast test
  conditions <- list(
    groups     = groups[1]      ,
    sampSizes  = sampSizes[1:2] ,
    phases     = phases[1:2]    ,
    propErrVar = propErrVar[1]  ,
    d          = d[1]           ,
    alignPhase = alignPhase
  )
  t1 <- Sim$new("t1", 1, 3, conditions, dirNames, 'nonpar', nodeNames)
  t1
  t1$condGrid
  t1$start()

  # no actual test?

  if(1==2){
  # long test
  conditions <- list(
    groups     = groups     ,
    sampSizes  = sampSizes  ,
    phases     = phases     ,
    propErrVar = propErrVar ,
    d          = d          ,
    alignPhase = alignPhase
  )
  }

}
)
