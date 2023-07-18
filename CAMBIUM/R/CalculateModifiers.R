#' Calculate Modifiers
#'
#' Calculates various modifiers based on specific conditions and data.
#'
#' @details This function calculates modifiers such as fWaterStress, fCytokinin, fTemp,
#'          FEnvironment, StemActivityModifier, LeafGrowthModifier, LeafModifierAuxin,
#'          FoliageAllocationMean, and StemAllocationMean.
#'
#' @return None
#'
#' @author David Drew
#' @details Code translated from Pascal to R by: ChatGPT, Annemarie Eckes-Shephard
#' @keywords xylem growth modifiers
#' @export
#' @example
CalculateModifiers <- function() {



  #	//****************************************************************************
  # calculate water stress modifier:
  # move away from iterating over days as in .pas , to calculating the whole vector of values immediately.
  # for (day in 1:(totaldays - 1)) {
    if (WaterStressShape == TRUE) {
      # The effect of water stress is an exponential effect:
      fWaterStress <- (WPMax / PreDawnWP)
    } else {
      fWaterStress <- (((WPMax - PreDawnWP) + (WPMin * 2 - PreDawnWP)) / (WPMin * 2 - WPMax))
    }

    if (sum(fWaterStress < 0)) fWaterStress[which(fWaterStress < 0)] <- 0

    #[TODO] not checked the paper or anything for the logic behind this - but just sanity-checking: if auxin sensing is FALSE,
    # why should cytokinin then be flexible?
    if (AuxSens == FALSE) {
      fCytokinin <<- 1 - (etacr / maxetacr)
    } else {
      fCytokinin <<- rep(1,length(etacr))
    }


    #	//****************************************************************************
    # Calculating temperature modifier
    # for (x in 0:(Form_CumulativeTemperature$TrackBar_CumTempDuration$Position - 1)) {
    # [TODO] seems to be something that can be modified through the user- interface ("Trackbar?")?
    # seems to be the a period of flexible length defined by the user on how long a cumulative effect should
    # act upon minimum and maximum mean temperature.
    # turn this into an instruction at the beginning of the model- run.
    # does this have anything to do with acclimation?
    # this has to be treated in a running-mean fashion:

    #must add length of values that cannot be evaluated for this back into the vector:
    #[TODO] should this be 0 or should this be e.g. MinTemp or MaxTemp?
    MinTempMean <<- c(rep(0,CumTempDuration-1),zoo::rollmean(MinTemp,k=CumTempDuration))
    MaxTempMean <<- c(rep(0,CumTempDuration-1),zoo::rollmean(MaxTemp,k=CumTempDuration))

    # calculate temperature modifier, but also set fTemp to 0, if growing temperatures are not suitable

    fTemp <<- TemperatureFunction(MinTempMean, MaxTempMean, MinTempCambialGrowth, OpTempCambialGrowth, MaxTempCambialGrowth)

    # now test whether cases wehre growing conditions are not suitable exist,
    # then identify which days those are
    # and then replace those days with "0" within fTemp
      if (sum( (((MinTempMean + MaxTempMean) / 2) < MinTempCambialGrowth) >0) ||
          sum( (((MinTempMean + MaxTempMean) / 2) > MaxTempCambialGrowth) >0) ) {
        idx_min = which(  (((MinTempMean + MaxTempMean) / 2) < MinTempCambialGrowth) )
        idx_max = which(  (((MinTempMean + MaxTempMean) / 2) > MaxTempCambialGrowth) )
        idx <- c(idx_max,idx_min)
        fTemp[idx] <- 0
      }

    # this is built into the "environmental modifier", based on the principle of the
    # most limiting factor, in a fashion similar to the VS-model:
    FEnvironment <<- pmin((fWaterStress ^ 0.25), fTemp)

  #	//****************************************************************************
  #  //Now that fEnv has been calculated, we modify FWaterStress according to the sens. of cell growth rate to water deficit...
    fWaterStress <<- (fWaterStress^WaterDeficitSensitivityGrowth)

 # [TODO] could be re-instigated in the code when e.g. wnating to do data assimilation, but leaving this out for now.
 # if (dendrodate == logdate[logday + 1]) {
 #   //i.e. if dendrometer data has been read in and data exists for this day
 #   for (x in 1:5) {
 #     StemActivityModifier[logday] <- IncrementDuration_dendrodata[logday - x] + StemActivityModifier[logday]
 #   }
 #
 #   StemActivityModifier[logday] <- (StemActivityModifier[logday] / 5) / 24
 # }


  # //****************************************************************************
  # //Leaf growth modifiers for auxin distribution

  LeafGrowthModifier <<- (etaf * NPP) / MaxDailyFoliageAllocation

  if ( sum(etaf == 0) >0 ) {
    idx <- which(etaf==0)
    LeafGrowthModifier[idx] <- MinLeafAllocation / MaxLeafAllocation
  }


  # replacing running mean foliage calculations pascal code with "one-liner":
  # this leaves in the current setup 90 days of FoliageAllocationMean at value 0.
  # which is not sensible. How did the pascal code deal with this? is this why it was rerun?
  # but then, it would simply use some values from simulationperiod-90 days to calculate the running means
  # for the start. maybe the best way for now, but only valid, if the dataset stops at a time suitable for
  # calculation of this behaviour that it leads to equivalent dynamics at the right time of the year  , but maybe a different way possible?
  # 0 not a good option:
  # plot( c(rep(0,RunningMeanDurationFoliage-1),zoo::rollmean(LeafGrowthModifier,k=RunningMeanDurationFoliage)))
  #

  #FoliageAllocationMean_tmp <- c(rep(0,5),zoo::rollmean(LeafGrowthModifier,k=5))
  #LeafGrowthModifier[1:RunningMeanDurationFoliage] <- FoliageAllocationMean_tmp[1:RunningMeanDurationFoliage]
  #FoliageAllocationMean <- c(FoliageAllocationMean_tmp[1:RunningMeanDurationFoliage],zoo::rollmean(LeafGrowthModifier,k=RunningMeanDurationFoliage))
  #FoliageAllocationMean <- c(rep(mean(LeafGrowthModifier),RunningMeanDurationFoliage-1),zoo::rollmean(LeafGrowthModifier,k=RunningMeanDurationFoliage))

  #[TODO] this looks better, but must double-check with David, maybe this stand is just growing up or something..:
  #front <- LeafGrowthModifier[(length(LeafGrowthModifier)-(RunningMeanDurationFoliage-1)):length(LeafGrowthModifier)]
  # adding "front" to the front of the evaluations. it is the last 90 days of the dataset
  #FoliageAllocationMean <- zoo::rollmean(c(front,LeafGrowthModifier),k=RunningMeanDurationFoliage)
  # actually.. it is: tree height is 0.15 to begin with. so starting with v.low values is valid,
  #so doing this for now:
  FoliageAllocationMean_tmp <- c(rep(0,5),zoo::rollmean(LeafGrowthModifier,k=5))
  front <- FoliageAllocationMean_tmp[(length(LeafGrowthModifier)-(RunningMeanDurationFoliage-2)):length(LeafGrowthModifier)]
  FoliageAllocationMean <<- zoo::rollmean(c(front,LeafGrowthModifier),k=RunningMeanDurationFoliage)

  # tree-height dependence? makes sense- Was this in the publication?
  #calculate auxin - no lag
    LeafModifierAuxin <- FoliageAllocationMean ^ AuxinSenstoLeafGrowth
  #then replace values, assume lag, and also dependent on treeheight:
    day <- 1:length(treeheight)
    if (sum(day > (1 / AuxinLag) * treeheight)>0){
      idx <- which(day > (1 / AuxinLag) * treeheight)
        LeafModifierAuxin[idx] <<- (FoliageAllocationMean[round(day[idx] - ((1 / AuxinLag) * treeheight[idx]))]) ^ AuxinSenstoLeafGrowth
    }

  if (sum(LeafModifierAuxin <= 0.001)<0) {
    idx <- LeafModifierAuxin <= 0.001
    LeafModifierAuxin[idx] <- 0.001
  }

  # //****************************************************************************
  # //stem allocation modifiers
  # replacing running mean stemallocation calculations pascal code with one-liner:

  front <- (etas * NPPTree)[1:RunningMeanDurationStem]
  StemAllocationMean <<- zoo::rollmean(c(front,etas * NPPTree),k=RunningMeanDurationStem)


}
