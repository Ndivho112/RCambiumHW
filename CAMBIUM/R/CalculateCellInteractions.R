#' Calculate Cell Interactions
#'
#' This function calculates the interaction between cells in two dimensions.
#'
#' @details Calculate the interaction between cells in two dimensions
#'
#' @return None
#'
#' @author David Drew
#' @details
#' code translated from Pascal to R by: ChatGPT, Annemarie Eckes-Shephard
#' @keywords "cell interactions", "two dimensions"
#' @export
#' @example
CalculateCellInteractions <- function(day) {
  k <- 0
  PressureModifier <- 0

  #******************************************************************************
  # Calculate the interaction between cells in two dimensions
  #******************************************************************************

  MeanFibRadGrowthRate[day] <<- 0
  MeanFibRadGrowthRateCounter[day] <<- 0

  for (LoopNumber in 1:TotalLoopSize) {
    n <- CellKey[LoopNumber]


    if(TESTING==TRUE){
      MaxRadNonCrushableCellDistanceLower[n] <- -5
      MaxTanNonCrushableCellDistanceLower[n] <- 0
      MaxTanNonCrushableCellDistanceUpper[n] <- 10000
    }else{
      MaxRadNonCrushableCellDistanceLower[n] <<- -5
      MaxTanNonCrushableCellDistanceLower[n] <<- 0
      MaxTanNonCrushableCellDistanceUpper[n] <<- 10000
    }

    for (positioncellcounter in StartNumberCells:TotalCells) {
      # We do not use the "TotalLoopSize" loop here because dead
      # cells must be taken into consideration for crushable
      # distance calculations.

      if (positioncellcounter != n) {
        CalculateRadCrushableDistance(n,positioncellcounter)
        CalculateTanCrushableDistance(n,positioncellcounter)
      }
    }

    if (DaysSinceCZExit[n] < CalculateMaxDaysBeforeThickening(CellType[n])) {
      if (RadPosition[n] <= MaxInitialPosition[day]) {
        if ((RadPosition[n] <= MinXMCPosition[day]) && (MaxDeadCellPosition < MinXMCPosition[day])) {
          PressureModifier <<- (RadPosition[n]^2 - MaxDeadCellPosition^2) / (MinXMCPosition[day]^2 - MaxDeadCellPosition^2)
        } else {
          PressureModifier <<- 1
        }

        if(TESTING==TRUE){
          LowerRadGrowthRate[n] <- LowerRadGrowthRate[n] * PressureModifier
          UpperRadGrowthRate[n] <- UpperRadGrowthRate[n] * PressureModifier
          LowerTanGrowthRate[n] <- LowerTanGrowthRate[n] * PressureModifier
          UpperTanGrowthRate[n] <- UpperTanGrowthRate[n] * PressureModifier
        }else{
        LowerRadGrowthRate[n] <<- LowerRadGrowthRate[n] * PressureModifier
        UpperRadGrowthRate[n] <<- UpperRadGrowthRate[n] * PressureModifier
        LowerTanGrowthRate[n] <<- LowerTanGrowthRate[n] * PressureModifier
        UpperTanGrowthRate[n] <<- UpperTanGrowthRate[n] * PressureModifier
        }
      }
    }

    if (LowerTanGrowthRate[n] < 0) {
      LowerTanGrowthRate[n] <<- 0
    }
    if (UpperTanGrowthRate[n] < 0) {
      UpperTanGrowthRate[n] <<- 0
    }
    if (LowerRadGrowthRate[n] < 0) {
      LowerRadGrowthRate[n] <<- 0
    }
    if (UpperRadGrowthRate[n] < 0) {
      UpperTanGrowthRate[n] <<- 0
    }

    ActualGrowth_CrushableDistanceConstraint(n)

    if (LowerTanGrowthRate[n] < 0) {
      LowerTanGrowthRate[n] <<- 0
    }
    if (UpperTanGrowthRate[n] < 0) {
      UpperTanGrowthRate[n] <<- 0
    }
    if (LowerRadGrowthRate[n] < 0) {
      LowerRadGrowthRate[n] <<- 0
    }
    if (UpperRadGrowthRate[n] < 0) {
      UpperRadGrowthRate[n] <<- 0
    }

    #**************************************************************************
    if (FinalGrowthSpurtFlag[n] == TRUE) {
      UpperRadGrowthRate[n] <<- MinRDSecThick - cellRD[n]
      # If cells are waiting to thicken but are not large enough, they are given a
      # little "nudge"
    }
    #**************************************************************************

  } # for loop

  CalculateRadialShift() # loops through cellnumbers internally

  for (LoopNumber in 1:TotalLoopSize) {
    n <- CellKey[LoopNumber]

    if (CellType[n] == "FIBRE" || CellType[n] == "XYLEMMOTHERCELL") {
      MeanFibRadGrowthRate[day] <<- MeanFibRadGrowthRate[day] + UpperRadGrowthRate[n]
      MeanFibRadGrowthRateCounter[day] <<- MeanFibRadGrowthRateCounter[day] + 1
    }

    CalculateActualSize(positioncellcounter)  #[TODO] This function calls CrushCells(), and within that, we need positioncellcounter. but that would not update here.
    # double-check this constant value of positioncellcounter is correct! This is how it is done in Main.pas , too!

    if (cellRD[n] > MaxCellRD) {
      MaxCellRD <<- cellRD[n]
    }

    RadPosition[n] <<- RadPosition[n] + NewRadialShift[n]
    # The radial position is calculated from the total accumulated growth for today of cells
    # pithward
  }

  if (MeanFibRadGrowthRateCounter[day] > 0) {
    MeanFibRadGrowthRate[day] <<- MeanFibRadGrowthRate[day] / MeanFibRadGrowthRateCounter[day]
  }

  for (LoopNumber in 1:TotalLoopSize) {
    n <- CellKey[LoopNumber]

    for (k in 1:TotalLoopSize) {
      PositionCellCounter <- CellKey[k]

      if (PositionCellCounter != n) {
        #print("im here")
        RemoveCrushedCells(PositionCellCounter,n)
      }
    }
  }

  #release to globalenv
  PressureModifier <<- PressureModifier
  MeanFibRadGrowthRate <<- MeanFibRadGrowthRate
  MeanFibRadGrowthRateCounter <<- MeanFibRadGrowthRateCounter
  MaxRadNonCrushableCellDistanceLower <<- MaxRadNonCrushableCellDistanceLower
  MaxTanNonCrushableCellDistanceLower <<- MaxTanNonCrushableCellDistanceLower
  MaxTanNonCrushableCellDistanceUpper <<- MaxTanNonCrushableCellDistanceUpper
  LowerRadGrowthRate <<- LowerRadGrowthRate
  UpperRadGrowthRate <<- UpperRadGrowthRate
  LowerTanGrowthRate <<- LowerTanGrowthRate
  UpperTanGrowthRate <<- UpperTanGrowthRate
  MaxCellRD <<- MaxCellRD
  RadPosition <<- RadPosition

}
