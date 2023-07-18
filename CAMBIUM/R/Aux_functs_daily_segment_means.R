#auxiliary functions related to daily segments

#//******************************************************************************
#//Calculations of daily and segment means

#' SegmentCountSumFibres
#'
#'
#' @export
SegmentCountSumFibres <- function(CellNumber,FSegmentNumber) {
  segmentLength <- as.numeric(SegmentLengthFibres)
  segmentStart <- segmentLength * FSegmentNumber - segmentLength
  segmentEnd <- segmentLength * FSegmentNumber

  if (RadPosition[CellNumber] >= segmentStart && RadPosition[CellNumber] < segmentEnd) {
    if (CellDead[CellNumber] != TRUE) {
      FSegmentActive[FSegmentNumber] <<- TRUE
    }

    if (CellCountedFlag[CellNumber] != TRUE &&
        CellCrushed[CellNumber] != TRUE &&
        CellDead[CellNumber] == TRUE) {
      CellCountedFlag[CellNumber] <<- TRUE

      if (CellType[CellNumber] == "FIBRE") {
        FibreSegmentCount[FSegmentNumber] <<- FibreSegmentCount[FSegmentNumber] + 1
        FibreSegmentRDSum[FSegmentNumber] <<- FibreSegmentRDSum[FSegmentNumber] + CellRD[CellNumber]
        FibreSegmentWTSum[FSegmentNumber] <<- FibreSegmentWTSum[FSegmentNumber] + CellWallThickness[CellNumber]
        FibreSegmentWASum[FSegmentNumber] <<- FibreSegmentWASum[FSegmentNumber] + CellWallCSArea[CellNumber]
      }
    }
  }
}


#' SegmentCountSumVessels
#'
#'
#' @export
SegmentCountSumVessels <- function(CellNumber,VSegmentNumber) {
  segmentLength <- as.numeric(SegmentLengthVessels)
  segmentStart <- segmentLength * VSegmentNumber - segmentLength
  segmentEnd <- segmentLength * VSegmentNumber

  if (RadPosition[CellNumber] >= segmentStart && RadPosition[CellNumber] <= segmentEnd) {
    if (CellDead[CellNumber] != TRUE) {
      VSegmentActive[VSegmentNumber] <<- TRUE
    }

    if (CellCountedFlag[CellNumber] != TRUE &&
        CellCrushed[CellNumber] != TRUE &&
        CellDead[CellNumber] == TRUE) {
      CellCountedFlag[CellNumber] <<- TRUE

      if (CellType[CellNumber] == "VESSEL") {
        VesselSegmentCount[VSegmentNumber] <<- VesselSegmentCount[VSegmentNumber] + 1
        VesselSegmentRDSum[VSegmentNumber] <<- VesselSegmentRDSum[VSegmentNumber] + CellRD[CellNumber]
      }
    }
  }
}




#' VesselSegmentMeans
#'
#'@param day simulation day
#'@param VSegmentNumber Vessel segment number
#'
#' @export
VesselSegmentMeans <- function(day,VSegmentNumber) {
  j <- 0

  # This procedure calculates the average values of the fibre/vessel properties
  # in the segment whose sum and count was previously calculated, if all cells
  # have died

  if (VSegmentActive[VSegmentNumber] == FALSE &&
      MinXMCPosition[day] > as.numeric(SegmentLengthVessels) * VSegmentNumber) {

    # If there are no vessels still living in the current segment
    if (VesselSegmentCount[VSegmentNumber] > 0) {
      VF[VSegmentNumber] <<- VesselSegmentCount[VSegmentNumber] / ((((as.numeric(SegmentLengthVessels))/1000)) *
                                                                     (((form_RadialFiles$TrackBar_RadialCellFiles$Position * MaxTDcz) + (RayWidth * 2))/1000))
      MeanVesselRD[VSegmentNumber] <<- VesselSegmentRDSum[VSegmentNumber] / VesselSegmentCount[VSegmentNumber]
    } else {
      VF[VSegmentNumber] <<- 0
      MeanVesselRD[VSegmentNumber] <<- 0
    }

    VSegmentStartDay[VSegmentNumber] <<- VSegmentEndDay[VSegmentNumber-1] + 1

    j <<- VSegmentStartDay[VSegmentNumber]

    if (MinXMCPosition[j] < as.numeric(SegmentLengthVessels) * VSegmentNumber) {
      while (MinXMCPosition[j] <= as.numeric(SegmentLengthVessels) * VSegmentNumber &&
             j < day) {
        j <- j + 1
      }
    }

    VSegmentEndDay[VSegmentNumber] <<- j

    for (VSegmentDay in VSegmentStartDay[VSegmentNumber]:VSegmentEndDay[VSegmentNumber]) {
      DrawVesselPropertyGraphs()

      SegmentType <<- "VESSELSEGMENT"

      MyLogDate <<- LogDate[VSegmentDay]

      MeanCellRD <<- MeanVesselRD[VSegmentDay]

      WriteSegmentCalculatedData()
    }

    VSegmentNumber <<- VSegmentNumber + 1
  }

  VSegmentActive[VSegmentNumber] <<- FALSE
}

#' Calculate Conductive Cells
#'
#' This function calculates the conductive cells based on the number of days since their death.
#' Cells become conductive units after a certain period following their death.
#'
#' @param n The cell number index
#' @param day The log day
#'
#' @return None
#' @export
CalculateConductiveCells <- function(n,day) {
  if (DaysSinceDeath[n] > DaysBeforeConductive) {
    # We assume that cells become conductive units after a certain period following their death
    if (!VesselForm) {
      if (CellType[n] == "VESSEL") {
        ConductingXylemPosition[day] <<- RadPosition[n]
        ConductiveStatus[n] <<- "CONDUCTING"
      }
    } else {
      ConductingXylemPosition[day] <<- RadPosition[n]
      ConductiveStatus[n] <<- "NONCONDUCTING"
    }
  }

  # Calculate vulnerability to cavitation here...
}


#' FibreSegmentMeans
#'
#' This function calculates the average values of the fibre/vessel properties
#' in the segment whose sum and count was previously calculated if all cells
#' have died.
#'
#' @details This procedure calculates the average values of the fibre/vessel properties
#' in the segment whose sum and count was previously calculated if all cells
#' have died.
#' @param day simulation day
#' @param FSegmentNumber Fibre Segment Number
#'
#' @return None
#'
#' @author David Drew
#' @details
#' Code translated from Pascal to R by: ChatGPT, Annemarie Eckes-Shephard
#' @keywords FibreSegmentMeans
#' @export
#' @example
FibreSegmentMeans <- function(day,FSegmentNumber) {
print("here in FibreSegmentMeans")

   if (!FSegmentActive[FSegmentNumber] && MinXMCPosition[day] > as.numeric(SegmentLengthFibres) * FSegmentNumber) {
    if (FibreSegmentCount[FSegmentNumber] > 0) {
      MeanFibreWallThickness[FSegmentNumber] <- FibreSegmentWTSum[FSegmentNumber] / FibreSegmentCount[FSegmentNumber]
      MeanFibreRD[FSegmentNumber] <- FibreSegmentRDSum[FSegmentNumber] / FibreSegmentCount[FSegmentNumber]
      MeanFibreWallArea[FSegmentNumber] <- FibreSegmentWASum[FSegmentNumber] / FibreSegmentCount[FSegmentNumber]
    } else {
      MeanFibreWallThickness[FSegmentNumber] <- 0
      MeanFibreRD[FSegmentNumber] <- 0
      MeanFibreWallArea[FSegmentNumber] <- 0
    }
    CalculateDensity()

    #DrawAnatDistanceGraphs()

    FSegmentStartDay[FSegmentNumber] <- FSegmentEndDay[FSegmentNumber - 1] + 1

    i <- FSegmentStartDay[FSegmentNumber]

    if (MinXMCPosition[i] < as.numeric(SegmentLengthFibres) * FSegmentNumber) {
      while (MinXMCPosition[i] <= as.numeric(SegmentLengthFibres) * FSegmentNumber && i < day) {
        i <- i + 1
      }
    }

    FSegmentEndDay[FSegmentNumber] <- i

    for (FSegmentDay in FSegmentStartDay[FSegmentNumber]:FSegmentEndDay[FSegmentNumber]) {
     # DrawFibrePropertyGraphs()

     # DrawDensityGraph()

      SegmentType <- "FIBRESEGMENT"

     # MyLogDate <- Logdate[FSegmentDay][TODO] is this important? seems to be for the old outputs.
     # commented out for now.

      MeanCellRD <- MeanFibreRD[FSegmentDay]

     # WriteSegmentCalculatedData()
    }

    FSegmentNumber <- FSegmentNumber + 1

    SapwoodSegments <- round(SAPWOODWIDTH / as.numeric(SegmentLengthFibres))

    MeanSapwoodDensity <<- 0

    if (FSegmentNumber > SapwoodSegments) {
      for (x in 0:(SapwoodSegments - 1)) {
        MeanSapwoodDensity <- MeanSapwoodDensity + ExtractedWoodDensity[FSegmentNumber - x]
      }

      MeanSapwoodDensity <- MeanSapwoodDensity / SapwoodSegments
    } else {
      MeanSapwoodDensity <- b0Density / 2
    }

    # release the updated vectors into globalenv:
    MeanFibreWallThickness <<- MeanFibreWallThickness
    MeanFibreRD <<- MeanFibreRD
    MeanFibreWallArea <<- MeanFibreWallArea
    FSegmentStartDay <<- FSegmentStartDay
    FSegmentEndDay <<- FSegmentEndDay
    FSegmentNumber <<- FSegmentNumber


    if(!exists("MeanSapwoodDensity")){ # [TODO] CANNOT GET MEANSAPWOOD DENSITY OTHERWISE:, needed
      #[TODO] making this global here, because it seems
      # to be required for some evaluations downstream in Function
      # CalculateCellSpecificAllocation
      MeanSapwoodDensity <<- b0Density / 2#MeanSapwoodDensity
    }
  }
  FSegmentActive[FSegmentNumber] <- FALSE
  FSegmentActive <<- FSegmentActive

  if(!exists("MeanSapwoodDensity")){ # [TODO] CANNOT GET MEANSAPWOOD DENSITY OTHERWISE:, needed
    #[TODO] making this global here, because it seems
    # to be required for some evaluations downstream in Function
    # CalculateCellSpecificAllocation
    MeanSapwoodDensity <<- b0Density / 2#MeanSapwoodDensity
  }

}

