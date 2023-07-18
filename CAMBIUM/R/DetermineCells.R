#' Determine Cells
#'
#' This function determines the cell identity based on the calculated auxin gradient once each day.
#'
#' @param CellIdentityDeterminationMethod string with "Barlow" or "Canalisation"; hypothesis for cell identity
#' @param TotalLoopSize alive cells
#' @details The function iterates through each cell and determines its identity based on various criteria.
#'
#' @author David Drew
#' @details
#' code translated from Pascal to R by: ChatGPT, Annemarie Eckes-Shephard
#' @keywords
#' @export
DetermineCells <- function(CellIdentityDeterminationMethod,TotalLoopSize,day) {
  FibGrowthDurationCount <- 0 # just for internal funciton purposes
  VessGrowthDurationCount <- 0

  # Determine cell identity using the calculated auxin gradient once each day
  for (LoopNumber in 1:TotalLoopSize) {
    CellNumber <- CellKey[LoopNumber]

    if (((AuxinConc[CellNumber] >= (MinAuxConcDivision * fCytokinin[day])) &
        (MeristematicStatus[CellNumber] == "MERISTEMATIC")) |
        (CellType[CellNumber] == "CAMBIALINITIAL") |
        (CellType[CellNumber] == "RAYINITIALC")) {
      # The cell is still meristematic if all these criteria are met
      MeristematicStatus[CellNumber] <- "MERISTEMATIC"
      TimeSinceMitosis[CellNumber] <- TimeSinceMitosis[CellNumber] + 1
      DaysSinceCZExit[CellNumber] <- 0
      CellDetermined[CellNumber] <- FALSE
    } else if ((!GrowthPhaseMinSize) |
               ((GrowthPhaseMinSize) &
                (cellRD[CellNumber] >= MinDDivision))) {
      # A control to prevent cells too small from exiting the meristematic phase if the user chooses...

      # The cell is differentiating

      # The cell will automatically be "GROWING"; this differentiation status is also
      # assigned to meristematic cells...

      MeristematicStatus[CellNumber] <- "DIFFERENTIATING"
      DaysSinceCZExit[CellNumber] <- DaysSinceCZExit[CellNumber] + 1

      if (DaysSinceCZExit[CellNumber] > CalculateMaxDaysBeforeThickening(CellType[CellNumber],day)/4) {
        if ((DaysSinceCZExit[CellNumber] > CalculateMaxDaysBeforeThickening(CellType[CellNumber],day)) ||
            (AuxinConc[CellNumber] < MinAuxConcGrowth)) {
          if ((MinSizeSecThickSet == TRUE && (cellRD[CellNumber] >= MinRDSecThick)) ||
            (MinSizeSecThickSet != TRUE) ) {
              if (CellDead[CellNumber] != TRUE) {
                DaysSinceSecThickening[CellNumber] <- DaysSinceSecThickening[CellNumber] + 1

                if (DifferentiationStatus[CellNumber] != "SECONDARYTHICKENING") {
                  GrowthStopDay[CellNumber] <- day
                  GrowingDays[CellNumber] <- DaysSinceCZExit[CellNumber]

                  if (CellType[CellNumber] == "VESSEL") {
                    MeanVessGrowthDuration[day] <- (GrowingDays[CellNumber] - 1) + MeanVessGrowthDuration[day]
                    VessGrowthDurationCount <- VessGrowthDurationCount + 1
                  } else {
                    MeanFibGrowthDuration[day] <- (GrowingDays[CellNumber] - 1) + MeanFibGrowthDuration[day]
                    FibGrowthDurationCount <- FibGrowthDurationCount + 1
                  }
                }

                DifferentiationStatus[CellNumber] <- "SECONDARYTHICKENING"
                RelativeRadWallExtensibility[CellNumber] <- 0
                RelativeTanWallExtensibility[CellNumber] <- 0
                PrimaryWallThickRate[CellNumber] <- 0

                if ((CritThickeningTimeThreshold[CellNumber] == 0 && SecThickDur == TRUE) ||
                    SecThickDur != TRUE ) {
                    CritThickeningTimeThreshold[CellNumber] <- ThickeningDuration[day]
                  }

                Apoptosis(CellNumber,day)
              }
            } else {
              if (MinSizeSecThickSet == TRUE && (cellRD[CellNumber] < MinRDSecThick)) {
                FinalGrowthSpurtFlag[CellNumber] <- TRUE
              }
            }
        }
      }


      if (CellDetermined[CellNumber] != TRUE) {
        CellDetermined[CellNumber] <- TRUE

        if (CellType[CellNumber] == "PHLOEMMOTHERCELL") {
          CellType[CellNumber] <- "PHLOEM"
        } else if ((CellType[CellNumber] == "RAYINITIALP") |
                   (CellType[CellNumber] == "RAYINITIALX") |
                   (CellType[CellNumber] == "RAYINITIAL") |
                   (CellType[CellNumber] == "RAYINITIALC")) {
          CellType[CellNumber] <- "RAY"
        } else if (CellType[CellNumber] == "XYLEMMOTHERCELL") {
          #AHES determine whether a xylem mother turns into something else based on two different hypoetheses:
          # switch can be used to test these hypotheses:
            if(CellIdentityDeterminationMethod == "Barlow"){
              XylemDetermine_Barlow(TotalLoopSize,CellNumber,day)
            }else if(CellIdentityDeterminationMethod == "Canalisation") {
              XylemDetermine_Canalisation(TotalLoopSize,CellNumber,day)
            }else{
                stop("No xylem Mother cell determination method was chosen. Choose either Barlow or Canalisation")
            }
        }
      }
    }
  }

  if (VessGrowthDurationCount > 0) {
  MeanVessGrowthDuration[day] <- MeanVessGrowthDuration[day] / VessGrowthDurationCount
  } else {
  MeanVessGrowthDuration[day] <- 0
  }

  if (FibGrowthDurationCount > 0) {
  MeanFibGrowthDuration[day] <- MeanFibGrowthDuration[day] / FibGrowthDurationCount
  } else {
  MeanFibGrowthDuration[day] <- 0
  }

  # Release vectors to the global environment
  assign("MeanVessGrowthDuration", MeanVessGrowthDuration, envir = globalenv())
  assign("MeanFibGrowthDuration", MeanFibGrowthDuration, envir = globalenv())
  assign("DifferentiationStatus", DifferentiationStatus, envir = globalenv())
  assign("RelativeRadWallExtensibility", RelativeRadWallExtensibility, envir = globalenv())
  assign("RelativeTanWallExtensibility", RelativeTanWallExtensibility, envir = globalenv())
  assign("PrimaryWallThickRate", PrimaryWallThickRate, envir = globalenv())
  assign("CellType", CellType, envir = globalenv())
  assign("GrowingDays", GrowingDays, envir = globalenv())
  assign("FinalGrowthSpurtFlag", FinalGrowthSpurtFlag, envir = globalenv())
  assign("TimeSinceMitosis", TimeSinceMitosis, envir = globalenv())
  assign("DaysSinceCZExit", DaysSinceCZExit, envir = globalenv())
  assign("CellDetermined", CellDetermined, envir = globalenv())
  assign("GrowthStopDay", GrowthStopDay, envir = globalenv())
  assign("DaysSinceSecThickening", DaysSinceSecThickening, envir = globalenv())

}
