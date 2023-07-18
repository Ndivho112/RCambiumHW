#' SumCountLoop
#'
#' This function has two main functions:
#' 1. To make the program as fast as possible, looping is only done
#'    on living cells. A separate looping variable is created for this purpose,
#'    since even once cells die they still retain their unique identifier "cell number".
#' 2. This function refreshes the count and summed variables of cells in different developmental stages.
#'
#' @details This function updates various variables and calculates averages and counts
#'          related to cell properties and positions.
#'
#' @param day simulation day
#' @return None
#'SegmentCountSumFibres
#' @author David Drew
#' @details
#' Code translated from Pascal to R by: ChatGPT, Annemarie Eckes-Shephard
#' @keywords  Cell Analysis, Developmental Stages, Averages, Counts
#' @export
#' @example
SumCountLoop <- function(day) {
  DENSITYWIDTH <- 20
print(day)
  #Pascal starts with index 0, so initialisation is easy- it is not actually a "logged simulation day"
  # in R, we are using the first entry of the vector for initialisation, and have to start
  # the simulation at day =2.
  MaxCellPosition[day] <- MaxCellPosition[day - 1]
  MinInitialPosition[day] <- MinInitialPosition[day - 1]
  MaxInitialPosition[day] <- MaxInitialPosition[day - 1]
  MinXMCPosition[day] <- MinXMCPosition[day - 1]
  MaxXMCPosition[day] <- MaxXMCPosition[day - 1]

  DBH_CAMBIUM[day] <- (DBH_CABALAStart + ((MaxCellPosition[day]/10000)*2)* (100/RadialGrowthContribution))

  for (loopNumber in 1:TotalLoopSize) {
    CellKey[loopNumber] <- 0
  }

  #form_CellLevelGraphs$Chart_FibGrowthRate_OneDay$Series[0]$clear()
  #form_CellLevelGraphs$Chart_PotFibGrowthRate_OneDay$Series[0]$clear()
  #form_CellLevelGraphs$Chart_WallThickRate_OneDay$Series[0]$clear()
  #form_CellLevelGraphs$Chart_potWallThickRate_OneDay$Series[0]$clear()
  #form_CellLevelGraphs$CHart_AuxinConc_OneDay$Series[0]$clear()
  #form_CellLevelGraphs$CHart_AuxinConc_OneDay$Series[1]$clear()

  StartNumberCells <<- 1 # release to globalenv, because used later in CalculateTanCrushableDistance() ( Aux_CalculateCellInteractions.R)

  LoopNumber <- 0
  TotalLoopSize <- 0

  # hand over number of dead cells from previous day, to then start adding to that on this day
  DeadCells[day] <- DeadCells[day-1]

  for (CellNumber in StartNumberCells:TotalCells) {

    #AHES removed routine drawing of image code here.

    if (CellDead[CellNumber] == TRUE) {
      DaysSinceDeath[CellNumber] <<- DaysSinceDeath[CellNumber] + 1
      CalculateConductiveCells(CellNumber,day)

      if ((CellType[CellNumber] != "PHLOEM") && (CellType[CellNumber] != "RAY")) {
        if (DaysSinceDeath[CellNumber] == 1) {
          if (CellCrushed[CellNumber] != TRUE)
            DeadCells[day] <<- DeadCells[day] + 1
        }
      }

      if (JustDiedFlag[CellNumber] == TRUE) {
        JustDeadFibresCount[day] <<- JustDeadFibresCount[day] + 1
        JustDiedFlag[CellNumber] <<- FALSE
      }
    }

    #//We reset all of the variables which are not carried over between days
    RadialShift[CellNumber] <<- 0
    NewRadialShift[CellNumber] <<- 0
    AdjustedFlag[CellNumber] <<- FALSE
    AdjustmentCompleteFlag[CellNumber] <<- FALSE
    CumulativeAuxinScore[CellNumber] <<- 0
    LowerRadGrowthRate[CellNumber] <<- 0
    UpperRadGrowthRate[CellNumber] <<- 0
    UpperTanGrowthRate[CellNumber] <<- 0
    LowerTanGrowthRate[CellNumber] <<- 0
    PotCellRadGrowthRate[CellNumber] <<- 0
    PotCellTanGrowthRate[CellNumber] <<- 0
    #[TODO] 11.05.2023 could not find where this is initialised.
    #for now just move on, and se if things run anyways. Must double-check later
    #WallThickRate0[CellNumber] <<- PotentialAreaWallThickRate[CellNumber]
    WallThickRate[CellNumber]  <<- 0

    #for (CellNumber in StartNumberCells:TotalCells) { #for testing
    if(TESTING ==TRUE){# just for testing within function. somehow
      #the breakpoints don't work..
      RadialShift[CellNumber] <- 0
      NewRadialShift[CellNumber] <- 0
      AdjustedFlag[CellNumber] <- FALSE
      AdjustmentCompleteFlag[CellNumber] <- FALSE
      CumulativeAuxinScore[CellNumber] <- 0
      LowerRadGrowthRate[CellNumber] <- 0
      UpperRadGrowthRate[CellNumber] <- 0
      UpperTanGrowthRate[CellNumber] <- 0
      LowerTanGrowthRate[CellNumber] <- 0
      PotCellRadGrowthRate[CellNumber] <- 0
      PotCellTanGrowthRate[CellNumber] <- 0
      WallThickRate[CellNumber]  <- 0
    }


    if (CellType[CellNumber] == "FIBRE" || CellType[CellNumber] == "XYLEMMOTHERCELL") {
      SegmentCountSumFibres(CellNumber,FSegmentNumber) #AHES [TODO] test whether these work as input.
      #this would make the tracing of the indeces easier.
      #This function works on one index only, so having it as input should work fine.
    }

    if (CellType[CellNumber] == "VESSEL" || CellType[CellNumber] == "XYLEMMOTHERCELL") {
      SegmentCountSumVessels(CellNumber,VSegmentNumber)#AHES s.a. [TODO] test whether these work as input.
    }

    if (RadPosition[CellNumber] > MaxCellPosition[day]) {
      MaxCellPosition[day] <- RadPosition[CellNumber]
    }

    if (CellDead[CellNumber]) {
      if ((CellType[CellNumber] == "PHLOEM" && DaysSinceDeath[CellNumber] >= 20) ||
          (CellType[CellNumber] == "RAY" && RadPosition[CellNumber] > MaxInitialPosition[day])) {
        # To try to speed up the model we remove phloem cells that have died from the
        # "current living cells" loop
        CellCrushed[CellNumber] <- TRUE
        cellRD[CellNumber] <- 0
        cellTD[CellNumber] <- 0
      }
    }

    if (CellCrushed[CellNumber] != TRUE) {
      if ((CellDead[CellNumber] == TRUE && DaysSinceDeath[CellNumber] <= 75) ||
          CellDead[CellNumber] != TRUE) {
        # We define the loop size here...
        # Only if cells are alive or dead for 20 days (and not crushed)
        # are they included in the mini-loop
        TotalLoopSize <- TotalLoopSize + 1
        LoopNumber <- LoopNumber + 1
        CellKey[LoopNumber] <- CellNumber

        # We sum here: the actual average is calculated outside the loop...
        # cell alive:
        if (CellDead[CellNumber] != TRUE && CellCrushed[CellNumber] != TRUE) {
          # if ((Menu_Options_UseFibreCellsForTotalCellCalc.Checked = TRUE) and
          if (CellType[CellNumber] == "FIBRE" || CellType[CellNumber] == "XYLEMMOTHERCELL" ||
              CellType[CellNumber] == "CAMBIALINITIAL") {
            # (Menu_Options_UseFibreCellsForTotalCellCalc.Checked != TRUE) then begin
            LivingCellCount[day] <- LivingCellCount[day] + 1
            AverageCellVolume[day] <- CellVolume[CellNumber] + AverageCellVolume[day]
            AverageLumenVolume[day] <- LumenVolume[CellNumber] + AverageLumenVolume[day]
          }
        }

        if (CellType[CellNumber] == "CAMBIALINITIAL" || CellType[CellNumber] == "RAYINITIALC") {
          # Count the initials and assess the
          # min and max initial positions
          if (CellType[CellNumber] == "CAMBIALINITIAL") {
            CambialInitials[day] <- CambialInitials[day] + 1
          }

          if (RadPosition[CellNumber] < MinInitialPosition[day]) {
            MinInitialPosition[day] <- RadPosition[CellNumber]
          } else if (RadPosition[CellNumber] > MaxInitialPosition[day]) {
            MaxInitialPosition[day] <- RadPosition[CellNumber]
          } # count phloem and xylem mother cells
        } else if (CellType[CellNumber] == "PHLOEMMOTHERCELL") {
          PhloemMotherCells[day] <- PhloemMotherCells[day] + 1
        } else if (CellType[CellNumber] == "XYLEMMOTHERCELL") {
          XylemMotherCells[day] <- XylemMotherCells[day] + 1
        }

        if (CellType[CellNumber] == "XYLEMMOTHERCELL" || CellType[CellNumber] == "RAYINITIALX") {
          if (RadPosition[CellNumber] < MinXMCPosition[day]) {
            MinXMCPosition[day] <- RadPosition[CellNumber]
          } else if (RadPosition[CellNumber] > MaxXMCPosition[day]) {
            MaxXMCPosition[day] <- RadPosition[CellNumber]
          }
        }

        if (DifferentiationStatus[CellNumber] == "GROWING") {
          if (CellType[CellNumber] == "FIBRE" || CellType[CellNumber] == "VESSEL") {
            GrowingCells[day] <- GrowingCells[day] + 1
          }
        } else if (DifferentiationStatus[CellNumber] == "SECONDARYTHICKENING") {
          if (CellType[CellNumber] == "FIBRE" || CellType[CellNumber] == "VESSEL") {
            ThickeningCells[day] <- ThickeningCells[day] + 1
          }
        }
      }
    }
  }

  AverageCellVolume[day] <- AverageCellVolume[day] / LivingCellCount[day]
  AverageLumenVolume[day] <- AverageLumenVolume[day] / LivingCellCount[day]

  #//Estimate the width (in number of cells) of the whole cambium: XMC's and PMC's.
  #//The number of cambial initials give an estimate of the number of radial files

  CambiumWidth[day] <- round((XylemMotherCells[day] + PhloemMotherCells[day] + CambialInitials[day]) / CambialInitials[day])

  if (day > 5) {
    CambiumWidth_averaged[day] <- sum(CambiumWidth[(day-4):day]) / 5
  } else {
    CambiumWidth_averaged[day] <- CambiumWidth[day]
  }

  if (CambiumWidth[day] > NczMax) {
    print(paste("WARNING: The predicted width of the cambial zone on", logdate[day], "(", CambiumWidth[day], ")
                exceeds the maximum specified for this species (", NczMax, ")"))
  }

  FibreSegmentMeans(day,FSegmentNumber)
  VesselSegmentMeans(day,VSegmentNumber)

  if(debug_print){
  print(day)
  print(paste("VSegmentNumber",VSegmentNumber))
  print(paste("FSegmentNumber",FSegmentNumber))
  }

  # now that all the loops have finished, release these updated vectors into the global environment.
  GrowingCells <<- GrowingCells
  ThickeningCells <<- ThickeningCells
  MaxXMCPosition <<- MaxXMCPosition
  MaxCellPosition <<- MaxCellPosition
  CellCrushed <<- CellCrushed
  CambialInitials <<- CambialInitials
  AverageLumenVolume <<- AverageLumenVolume
  AverageCellVolume <<- AverageCellVolume
  LivingCellCount <<- LivingCellCount
  CellKey <<- CellKey
  CambiumWidth <<- CambiumWidth
  CambiumWidth_averaged <<- CambiumWidth
  DBH_CAMBIUM <<- DBH_CAMBIUM
  TotalLoopSize <<- TotalLoopSize
 print(paste0 ("Totalloopsize ", TotalLoopSize))
}







