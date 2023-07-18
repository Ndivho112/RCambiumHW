# collection of smaller auxiliary functions here:
# can later be moved into their own file and documented more, if needed:

#'TemperatureFunction
#'
#' @export
TemperatureFunction <- function(TMinD, TMaxD, TMin, Topt, TMax) {
  TMeanD <- (TMinD + TMaxD) / 2

  Result <- ((TMeanD - TMin) / (Topt - TMin)) * ((TMax - TMeanD) / (TMax - Topt))^((TMax - Topt) / (Topt - TMin))

  return(Result)
}


#//******************************************************************************
#  //Length, volume and surface area functions

#'CylinderVolume
#'
#' @export
CylinderVolume <- function(RD, TD, Length) {
  return(pi * ((RD + TD)/4)^2 * Length)
}

#'CylinderCSArea
#'
#' @export
CylinderCSArea <- function(Volume, Length) {
  return(Volume / Length)
}

#'CylinderArea
#'
#' @export
CylinderArea <- function(RD, TD, Length) {
  return((2 * pi * (RD + TD)/4 * Length) + (2 * pi * ((RD + TD)/4)^2))
}

#'ConeVolume
#'
#' @export
ConeVolume <- function(RD, TD, Length) {
  return(1/3 * pi * ((RD + TD)/4)^2 * Length)
}

#'ConeCSArea
#'
#' @export
ConeCSArea <- function(Volume, Length) {
  return(3 * (Volume / Length))
}

#'ConeArea
#'
#' @export
ConeArea <- function(RD, TD, Length) {
  return(pi * ((RD + TD)/4)^2 + pi * sqrt(((RD + TD)/4)^2 * (Length/2)^2))
}


#'RectangularPrismVolume
#'
#' @export
RectangularPrismVolume <- function(RD, TD) {
  return(TD * RD^2)
}

#'RectangularPrismCSArea
#'
#' @export
RectangularPrismCSArea <- function(RD, TD) {
  return(RD * TD)
}

#'RectangularPrismArea
#'
#' @export
RectangularPrismArea <- function(RD, TD) {
  return(2 * (RD * TD + RD^2 + RD * TD))
}


#'BodyVolume
#'
#' @export
BodyVolume <- function(TypeofCell, RD, TD, Length) {
  if (TypeofCell == "PHLOEM" || TypeofCell == "VESSEL") {
    return(CylinderVolume(RD, TD, Length))
  } else if (TypeofCell == "FIBRE" || TypeofCell == "XYLEMMOTHERCELL" ||
             TypeofCell == "PHLOEMMOTHERCELL" || TypeofCell == "CAMBIALINITIAL" ||
             TypeofCell == "FUSINITIAL") {
    return(2 * ConeVolume(RD, TD, Length/2))
  } else if (TypeofCell == "RAY" || TypeofCell == "RAYINITIALC" ||
             TypeofCell == "RAYINITIALP" || TypeofCell == "RAYINITIALX" ||
             TypeofCell == "RAYINITIAL") {
    return(RectangularPrismVolume(RD, TD))
  }
}


#AHES: had to add "RD and TD as additional inputs for the function, this deviates from the
#.pas code.
#[TODO] check this makes sense
#'BodyCSArea
#'
#' @export
BodyCSArea <- function(TypeofCell, Volume, Length, RD, TD) {
  if (TypeofCell == "PHLOEM" || TypeofCell == "VESSEL") {
    return(CylinderCSArea(Volume, Length))
  } else if (TypeofCell == "FIBRE" || TypeofCell == "XYLEMMOTHERCELL" ||
             TypeofCell == "PHLOEMMOTHERCELL" || TypeofCell == "CAMBIALINITIAL" ||
             TypeofCell == "FUSINITIAL") {
    return(ConeCSArea(Volume, Length))
  } else if (TypeofCell == "RAY" || TypeofCell == "RAYINITIALC" ||
             TypeofCell == "RAYINITIALP" || TypeofCell == "RAYINITIALX" ||
             TypeofCell == "RAYINITIAL") {
    return(RD * TD) # This used to be Result := CellRD[cellnumber] * CellTD[cellnumber];,
    # but that is never an input into the pascal function.
    # maybe not a problem for .pas, but a problem for R with this setup.
  }
}

#'BodySurfaceArea
#'
#' @export
BodySurfaceArea <- function(TypeofCell, RD, TD, Length) {
  if (TypeofCell == "PHLOEM" || TypeofCell == "VESSEL") {
    return(CylinderArea(RD, TD, Length))
  } else if (TypeofCell == "FIBRE" || TypeofCell == "XYLEMMOTHERCELL" ||
             TypeofCell == "PHLOEMMOTHERCELL" || TypeofCell == "CAMBIALINITIAL" ||
             TypeofCell == "FUSINITIAL") {
    return(2 * ConeArea(RD, TD, Length))
  } else if (TypeofCell == "RAY" || TypeofCell == "RAYINITIALC" ||
             TypeofCell == "RAYINITIALP" || TypeofCell == "RAYINITIALX" ||
             TypeofCell == "RAYINITIAL") {
    return(RectangularPrismArea(RD, TD))
  }
}

#'CalculateCellLength
#'
#' @export
CalculateCellLength <- function(TypeofCell, RD) {
  if (TypeofCell == "PHLOEM" || TypeofCell == "FIBRE" ||
      TypeofCell == "XYLEMMOTHERCELL" || TypeofCell == "PHLOEMMOTHERCELL" ||
      TypeofCell == "CAMBIALINITIAL" || TypeofCell == "FUSINITIAL") {
    if (RD * RatioLengthRadFib < MAXFIBRELENGTH) {
      return(RD * RatioLengthRadFib)
    } else {
      return(MAXFIBRELENGTH)
    }
  } else if (TypeofCell == "RAY" || TypeofCell == "RAYINITIALC" ||
             TypeofCell == "RAYINITIALP" || TypeofCell == "RAYINITIALX" ||
             TypeofCell == "RAYINITIAL") {
    return(RD)
  }
}


#//******************************************************************************


#' CalculateDensity
#'
#' This function calculates the wood density based on the mean fibre radial density
#' and mean fibre wall thickness of the segment. If the mean values are greater than
#' 0, the wood density is calculated using the specified formulas. Otherwise, the
#' previous wood density value is used.
#'
#' @details This function calculates the wood density based on the mean fibre radial density
#' and mean fibre wall thickness of the segment. If the mean values are greater than
#' 0, the wood density is calculated using the specified formulas. Otherwise, the
#' previous wood density value is used.
#'
#' @return None
#'
#' @author David Drew
#' @details
#' Code translated from Pascal to R by: ChatGPT, Annemarie Eckes-Shephard
#' @keywords  CalculateDensity
#' @export
#' @example
CalculateDensity <- function() {
  if (MeanFibreRD[FSegmentNumber] > 0 && MeanFibreWallThickness[FSegmentNumber] > 0) {
    ExtractedWoodDensity[FSegmentNumber] <- b0Density + (b1Density * MeanFibreRD[FSegmentNumber]) +
      (b2Density * MeanFibreWallThickness[FSegmentNumber])
  } else {
    ExtractedWoodDensity[FSegmentNumber] <- ExtractedWoodDensity[FSegmentNumber - 1]
  }

  if (MeanWoodDensity > 0) {
    MeanWoodDensity <<- (MeanWoodDensity + ExtractedWoodDensity[FSegmentNumber]) / 2
  } else {
    MeanWoodDensity <<- b0Density / 2
  }

  if (ExtractedWoodDensity[FSegmentNumber] < 200) {
    print(paste("Predicted density for segment", FSegmentNumber, "less than 200 kg/m^3"))
  }
}


#' AuxinField
#'
#' Calculates the auxin field for each cell based on specific parameters.
#'
#' @details This function calculates the auxin concentration for each cell using
#'          the given parameters and formulas. It considers factors such as leaf
#'          modifiers, environmental conditions, height, stem activity, and the
#'          distance from the initial cell. The resulting auxin concentration is
#'          stored in the AuxinConc vector for each cell.
#'
#' @author David Drew
#' @details
#' Code translated from Pascal to R by: ChatGPT, Annemarie Eckes-Shephard
#' @keywords AuxinField, auxin concentration, cell, leaf modifier, environmental condition, height, stem activity, distance, initial cell
#' @export
#' @example
#'
AuxinField <- function(day,n) {
  #used to work with CellNumber in .pas code. but we can use internal "n" for cell number.
  PMCSlopeMod <- 0  # This modifies the slope of the aux. conc gradient for PMC or XMC
  Inv_EnvironmentalModifier <- numeric(30001)

  # Calculate the rate of change of auxin concentration along a radial gradient

  if (AuxinConcChangeMethod == 0) {
    # This calculates the rate of change radially of auxin as a function of increase in young leaves, lagged

    if (LeafModifierAuxin[day] > 0.05) {
      AuxConcChangeRate[day] <- MinAuxConcChangeRate / LeafModifierAuxin[day]
    } else {
      AuxConcChangeRate[day] <- MinAuxConcChangeRate / 0.05
    }

    if (LeafModifierAuxin[day] < 1) {
      AuxinPeak[day] <- MaxAuxConc * ((1 - LeafModifierAuxin[day]) ^ 0.005)
      # This ensures that when the peak is higher, the width is less and vice versa: see Uggla et al 2001.
    } else {
      AuxinPeak[day] <- MaxAuxConc * (0.01 ^ 0.005)
    }

  } else if (AuxinConcChangeMethod == 1) {

    AuxConcChangeRate[day] <- ifelse((sqrt(LeafModifierAuxin[day]) * fEnvironment[day]) > 0.05,
                                     MinAuxConcChangeRate / (sqrt(LeafModifierAuxin[day]) * fEnvironment[day]),
                                     MinAuxConcChangeRate / 0.05)

    if (LeafModifierAuxin[day] < 1 && fEnvironment[day] < 1) {
      AuxinPeak[day] <- MaxAuxConc * ((sqrt(1 - LeafModifierAuxin[day]) * (1 - fEnvironment[day])) ^ 0.005)
    } else {
      AuxinPeak[day] <- MaxAuxConc * (0.01 ^ 0.005)
    }
  }

  if (GreenHt[day] > MinHtToCrownBase) {
    AuxinPeak[day] <- AuxinPeak[day] * ((MinHtToCrownBase / GreenHt[day]) ^ 0.025)
  }

  #****************************************************************************

  # AHES: dendro data assimulation currently disabled
 # if (runoptions_usedendrometerdata$checked) {
 #   if (stemactivitymodifier[day] == 0) {
 #     stemactivitymodifier[day] <- 0.75
 #   }
 #   if (dendrodate == logdate[day + 1]) {
 #     AuxConcChangeRate[day] <- MinAuxConcChangeRate * sqrt(1 / StemActivityModifier[day])
 #   }
 # }

  if (AuxConcChangeRate[day] == 0) {
    AuxConcChangeRate[day] <- MinAuxConcChangeRate
  }

  if (RadPosition[n] > RadPosition[InitialCell[n]]) {
    DistFromInitial[n] <- RadPosition[n] - RadPosition[InitialCell[n]]
    PMCSlopeMod <- PMCMod
    # The rate of decline of auxin conc appears to be higher on the phloem side
    # This modifier increases the steepness of the change on the PMC side
    # Physiologically, does it reduce efflux carrier effectiveness?
  } else {
    DistFromInitial[n] <- RadPosition[InitialCell[n]] - RadPosition[n]
    PMCSlopeMod <- 1
  }

  #****************************************************************************
  # Calculate the auxin concentration for each cell:
  AuxinConc[n] <- AuxinPeak[day] * exp((DistFromInitial[n] * ((-1 * AuxConcChangeRate[day]) * PMCSlopeMod)))
  #****************************************************************************

  if (MeristematicStatus[n] == "MERISTEMATIC") {
    CumulativeAuxinScore[n] <- CumulativeAuxinScore[n] + AuxinConc[n]
    # The accumulation of a "morphogen" score based on Barlow and Luck 2004
    # We assume the score is only accumulated for meristematic cells
  }

 # AuxConcChangeRate <<- AuxConcChangeRate
  AuxinConc <<- AuxinConc
  DistFromInitial <<- DistFromInitial
  AuxinPeak <<- AuxinPeak
}

#AHES added "day" here
CalculateMaxDaysBeforeThickening <- function(TypeofCell,day) {
  if (TypeofCell == "VESSEL") {
    return ((MaxDaysBeforeThickeningVess/2) * (TreeHeight[day]^(0.2)))
  } else {
    return (MaxDaysBeforeThickeningFib)
  }

}


#' Determine cell identity (fibres vs. vessels) using the concept of a
#' summed auxin (or other morphogenic) value (Barlow and Luck)
#'
#' @details This function determines the cell identity (fibres vs. vessels) based on a summed auxin score.
#' If the cumulative auxin score of a cell falls within the specified range, the cell has the potential to become a vessel.
#' The function also considers vessel proximity and cambium width to determine the final cell type.
#'
#' @param TotalLoopSize total numnber of alive cells ([TODO] I THINK..)
#' @param CellNumber number of cell which needs checking
#' @param day simulation day
#'
#' @return The cell type (FIBRE or VESSEL) based on the determined identity.
#'
#' @author David Drew
#' @details
#' Code translated from Pascal to R by: ChatGPT, Annemarie Eckes-Shephard
#' @keywords  cell identity, vessel formation, auxin score
#' @export
#' @example
#' # Call the function
#' cell_type <- XylemDetermine_Barlow(CumulativeAuxinScore, MinVesselAuxinScore, MaxVesselAuxinScore, RadPosition, MinInitialPosition, CambiumWidth, Menu_PhysOptions_VesselForm)
#'
#' # Print the cell type
#' print(cell_type)
#'
XylemDetermine_Barlow <- function(TotalLoopSize,n,day) {
  # AHES got a cell number input, and change cellnumber to n within this function. We are calling this function within the cell number loop in function in DetermineCells.R
  InitialLoop <<- FALSE

  if (CumulativeAuxinScore[n] >= MinVesselAuxinScore && CumulativeAuxinScore[n] <= MaxVesselAuxinScore) {
    # The cell now has the potential to become a vessel due to being within the ranges of a critical "auxin score"

    VesselProximityCalculation(TotalLoopSize,n)

    if (VesselProximityFlag != TRUE) {
      if (CambiumWidth[day - 1] > 2) {
        if (Menu_PhysOptions_VesselForm != TRUE) {
          # A limit is introduced to prevent vessels forming at the boundary of initials and causing large distortion
          if (RadPosition < MinInitialPosition[day]) {
            CellType[n] <- "VESSEL"
          } else {
            CellType[n] <- "FIBRE"
          }
        } else {
          CellType[n] <- "FIBRE"
        }
      } else {
        CellType[n] <- "FIBRE"
      }
    } else {
      CellType[n] <- "FIBRE"
    }
  } else {
    CellType[n] <- "FIBRE"
  }

  CellType <<- CellType
}

#' Determine cell identity (fibres vs. vessels) using the concept of auxin canalisation
#' and the resultant switching of genetic programming (Sachs, 1981 etc)
#'
#' @details This function determines the cell identity (fibres vs. vessels) based on auxin canalisation.
#' If the previous auxin canalisation flag of a cell is true, the function considers vessel proximity and cambium width
#' to determine the final cell type.
#'
#' @param TotalLoopSize total numnber of alive cells ([TODO] I THINK..)
#' @param CellNumber number of cell which needs checking
#' @param day simulation day
#'
#' @return The cell type (FIBRE or VESSEL) based on the determined identity.
#'
#' @author David Drew
#' @details
#' Code translated from Pascal to R by: ChatGPT, Annemarie Eckes-Shephard
#' @keywords  cell identity, vessel formation, auxin canalisation
#' @export
#' @example
#' # Call the function
#' cell_type <- XylemDetermine_Canalisation(PreviousAuxinCanal, RadPosition, MinInitialPosition, CambiumWidth, Menu_PhysOptions_VesselForm)
#'
#' # Print the cell type
#' print(cell_type)
#'
XylemDetermine_Canalisation <- function(TotalLoopSize,n,day) {
  if (PreviousAuxinCanal[n] == TRUE) {
    InitialLoop <<- FALSE

    VesselProximityCalculation(TotalLoopSize, n)

    if (VesselProximityFlag != TRUE) {
      if (CambiumWidth[day - 1] > 2) {
        if (VesselForm != TRUE) {
          # A limit is introduced to prevent vessels forming at the boundary of initials and causing large distortion
          if (RadPosition < MinInitialPosition[day]) {
            CellType[n] <- "VESSEL"
          } else {
            CellType[n] <- "FIBRE"
          }
        } else {
          CellType[n] <- "FIBRE"
        }
      } else {
        CellType[n] <- "FIBRE"
      }
    } else {
      CellType[n] <- "FIBRE"
    }
  } else {
    CellType[n] <- "FIBRE"
  }

  CellType <<- CellType
}


#' Calculate the proximity of the nearest distance and raise a "flag" if a vessel
#' is too close for a potential vessel to actually become a vessel
#'
#' @details This function calculates the proximity of the nearest distance between cells
#' and raises a flag if a vessel is too close to a potential vessel, indicating that it cannot become a vessel.
#'
#' @author David Drew
#' @details
#' Code translated from Pascal to R by: ChatGPT, Annemarie Eckes-Shephard
#' @keywords  vessel proximity, distance calculation, flag
#' @export
#' @example
#' # Call the function
#' VesselProximityCalculation()
#'
VesselProximityCalculation <- function(TotalLoopSize, n) {
  # again, creating Cellnumber (n) as input into the loop. It stays constant here.
  VesselProximityFlag <- FALSE

  for (e in 1:TotalLoopSize) {
    positioncellcounter <- CellKey[e]

    # Because we use this loop, it assumes that only vessels which are still alive can exert "distance control" on any future vessels...
    if (positioncellcounter != n) {
      if (CellType[positioncellcounter] == "VESSEL") {
        if ((TanPosition[positioncellcounter] >= TanPosition[n])) {
          if (((TanPosition[positioncellcounter] - cellTD[positioncellcounter] / 2) - (TanPosition[n] + cellTD[n] / 2)) < MinInterVesselDist) {
            if ((RadPosition[positioncellcounter] >= RadPosition[n])) {
              if (((RadPosition[positioncellcounter] - cellRD[positioncellcounter] / 2) - (RadPosition[n] + cellRD[n] / 2)) < MinInterVesselDist) {
                VesselProximityFlag <- TRUE
              }
            } else if ((RadPosition[positioncellcounter] <= RadPosition[n])) {
              if (((RadPosition[n] - cellRD[n] / 2) - (RadPosition[positioncellcounter] + cellRD[positioncellcounter] / 2)) < MinInterVesselDist) {
                VesselProximityFlag <- TRUE
              }
            }
          }
        } else if ((TanPosition[positioncellcounter] <= TanPosition[n])) {
          if (((TanPosition[n] - cellTD[n] / 2) - (TanPosition[positioncellcounter] + cellTD[positioncellcounter] / 2)) < MinInterVesselDist) {
            if ((RadPosition[positioncellcounter] >= RadPosition[n])) {
              if (((RadPosition[positioncellcounter] - cellRD[positioncellcounter] / 2) - (RadPosition[n] + cellRD[n] / 2)) < MinInterVesselDist) {
                VesselProximityFlag <- TRUE
              }
            } else if ((RadPosition[positioncellcounter] <= RadPosition[n])) {
              if (((RadPosition[n] - cellRD[n] / 2) - (RadPosition[positioncellcounter] + cellRD[positioncellcounter] / 2)) < MinInterVesselDist) {
                VesselProximityFlag <- TRUE
              }
            }
          }
        }
      }
    }
  }

  VesselProximityFlag <<- VesselProximityFlag
}


#' Perform cell apoptosis assessment
#'
#' This function assesses whether a cell must undergo apoptosis based on the criteria specified in the Pascal procedure.
#'
#' @param n cellnumber  - index of the cell to evaluate
#' @param day simulation day
#'
#' @details This function evaluates whether a cell must undergo apoptosis by comparing the number of days since secondary thickening
#' with the critical thickening time threshold. If the cell has been thickening or differentiating for a set maximum period,
#' it is marked as dead. The relevant vectors are updated accordingly.
#'
#' @return None (modifies input vectors in-place)
#' @return DaysSinceSecThickening A vector containing the number of days since secondary thickening for each cell
#' @return CritThickeningTimeThreshold A vector containing the critical thickening time threshold for each cell
#' @return JustDiedFlag A vector indicating whether a cell has just died
#' @return DaysSinceDeath A vector containing the number of days since death for each cell
#' @return WallThickRate A vector containing the wall thickening rate for each cell
#' @return CellDead A vector indicating whether a cell is dead
#' @return DifferentiationStatus A vector indicating the differentiation status of each cell
#' @return RadPosition A vector containing the radial position of each cell
#' @return MaxDifferentiatedCellPosition A vector containing the maximum radial position of differentiated cells for each day
#' @return MaxDeadCellPosition The maximum radial position of dead cells
#'
#' @author David Drew
#' @details
#' Code translated from Pascal to R by: ChatGPT, Annemarie Eckes-Shephard
#' @keywords cell, apoptosis, differentiation
#' @export
#' @example
#' Apoptosis(cellnumber, DaysSinceSecThickening, CritThickeningTimeThreshold, JustDiedFlag, DaysSinceDeath,
#' WallThickRate, CellDead, DifferentiationStatus, RadPosition, MaxDifferentiatedCellPosition,
#' MaxDeadCellPosition)
Apoptosis <- function(n,day) {
  # n: The number of the cell undergoing apoptosis

  if (DaysSinceSecThickening[n] > CritThickeningTimeThreshold[n]) {
    # If the cell has been thickening or differentiating for a set max period, it must die!

    if (CellDead[n] != TRUE) {
      JustDiedFlag[n] <- TRUE
      DaysSinceDeath[n] <- 0
    }

    # The cell has just died: NB for calculation of mean for the day of wall thick.

    WallThickRate[n] <- 0

    CellDead[n] <- TRUE

    DifferentiationStatus[n] <- "DEAD"

    if (RadPosition[n] > MaxDifferentiatedCellPosition[day]) {
      MaxDifferentiatedCellPosition[day] <- RadPosition[n]
    }

    if (CellType[n] == "FIBRE" || CellType[n] == "VESSEL") {
      if (RadPosition[n] > MaxDeadCellPosition) {
        MaxDeadCellPosition <- RadPosition[n]
      }
    }
  }

  assign("JustDiedFlag", JustDiedFlag, envir = .GlobalEnv)
  assign("DaysSinceDeath", DaysSinceDeath, envir = .GlobalEnv)
  assign("WallThickRate", WallThickRate, envir = .GlobalEnv)
  assign("CellDead", CellDead, envir = .GlobalEnv)
  assign("DifferentiationStatus", DifferentiationStatus, envir = .GlobalEnv)
  assign("MaxDifferentiatedCellPosition", MaxDifferentiatedCellPosition, envir = .GlobalEnv)
  assign("MaxDeadCellPosition", MaxDeadCellPosition, envir = .GlobalEnv)

}


#################


#' GetNightHours
#'
#' Calculates the number of night hours based on input parameters.
#'
#' @details This function calculates the number of night hours based on the input date, latitude, rainfall, minimum rainfall, solar radiation, and solar radiation from the previous day.
#'
#' @param MyInputDate A string representing the input date.
#' @param MyLat A real number representing the latitude.
#' @param MyRainfall A real number representing the rainfall.
#' @param MyMinRainfall A real number representing the minimum rainfall.
#' @param MySolarRad A real number representing the solar radiation.
#' @param MySolarRadYesterday A real number representing the solar radiation from the previous day.
#' @param day simulation day
#'
#' @return The number of night hours as a real number.
#'
#' @author David Drew
#' @details
#' Code translated from Pascal to R by: ChatGPT, Annemarie Eckes-Shephard
#' @keywords "night hours", "day length", "shrinkage", "sun down"
#' @export
#' @example
GetNightHours <- function(MyInputDate, MyLat, MyRainfall, MyMinRainfall, MySolarRad, MySolarRadYesterday,day) {
  DayNumber <- 0
  RelativeDayLength <- 0
  RelativeShrinkLength <- 0

  if (logdate[day] != "") {
    DayNumber <- lubridate::yday(logdate[day]) # replaces GetDayofYear function
  }

  RelativeDayLength <- GetDayLength(MyLat, DayNumber)
  RelativeShrinkLength <- GetDayLength(MyLat / 4, DayNumber)

  if (MyRainfall > MyMinRainfall) {
    if (MySolarRad >= MySolarRadYesterday * 0.9) {
      # if the incident radiation is reduced relative to yesterday, we
      # assume the rainfall happened during the day and that shrinkage
      # was temporarily ceased
      Result <- 24
    } else {
      Result <- (1 - RelativeShrinkLength) * 24
    }
    # This gives an indication of the relative amount of time the sun is "down"
  } else {
    Result <- (1 - RelativeShrinkLength) * 24
  }
  # This gives an indication of the relative amount of time the sun is "down"

  #update the number of nightours
  NightHours[day] <<- (1 - RelativeDayLength) * 24.

  return(NightHours[day]) # [TODO] AHES double check this translation. Should be NightHours[day?] ( maybe even without the [day])
  # also, how is "Result" utilised here?
}

#' GetDayLength
#'
#' Calculates the fraction of daylight hours based on latitude and day of the year.
#'
#' @details This function calculates the fraction of daylight hours based on the latitude and day of the year. It uses mathematical formulas to determine the sunrise and sunset times.
#'
#' @param Lat A real number representing the latitude.
#' @param DayOfYear An integer representing the day of the year.
#'
#' @return The fraction of daylight hours as a real number.
#'
#' @author David Drew
#' @details
#' Code translated from Pascal to R by: ChatGPT, Annemarie Eckes-Shephard
#' @keywords "day length", "daylight hours", "latitude", "sunrise", "sunset"
#' @export
#' @example
GetDayLength <- function(Lat, DayOfYear) {
  SLAt <- sin(pi * Lat / 180)
  cLat <- cos(pi * Lat / 180)
  sinDec <- 0.4 * sin(0.0172 * (DayOfYear - 80))
  cosH0 <- -sinDec * SLAt / (cLat * sqrt(1 - sinDec^2))

  if (cosH0 > 1) {
    Result <- 0
  } else if (cosH0 < -1) {
    Result <- 1
  } else {
    Result <- acos(cosH0) / pi
  }

  return(Result)
}


#' CalculateInitialActualGrowthRates
#'
#' Calculates the upper and lower actual growth rates based on the potential radial and tangential growth rates and user-defined proportions.
#'
#' @details This function calculates the upper and lower tangential growth rates from the potential radial
#' and tangential growth rates based on user-defined upper and lower proportions.
#' If the cell type is "VESSEL," the lower radial growth rate is determined by multiplying
#' the potential radial growth rate by the lower growth proportion, and the upper radial growth
#' rate is determined by subtracting the lower radial growth rate from the potential radial growth rate.
#'  If the cell type is not "VESSEL," the lower radial growth rate is set to 0, and the upper
#'  radial growth rate remains the same as the potential radial growth rate.
#'   The lower tangential growth rate is half of the potential tangential growth rate,
#'   and the upper tangential growth rate is also half of the potential tangential growth rate.
#' @param n CellNumber index of the cell that is currently to be treated by this function.
#' @param PotCellTanGrowthrate potential tangential growth rate calculated in calling function CalculateCellGrowthPotential()
#' @param PotCellRadGrowthrate potential radial growth rate calculated in calling function CalculateCellGrowthPotential()
#' @author David Drew
#' @details
#' Code translated from Pascal to R by: ChatGPT, Annemarie Eckes-Shephard
#' @keywords "actual growth rates", "radial growth", "tangential growth"
#' @export
#' @example
CalculateInitialActualGrowthRates <- function(n,PotCellTanGrowthRate,PotCellRadGrowthRate) {
  if (CellType[n] == "VESSEL") {
    LowerRadGrowthRate[n] <- PotCellRadGrowthRate * (LowerGrowthProportion/ 10)
    UpperRadGrowthRate[n] <- PotCellRadGrowthRate * (1 - (LowerGrowthProportion / 10))
  } else {
    LowerRadGrowthRate[n] <- 0
    UpperRadGrowthRate[n] <- PotCellRadGrowthRate
  }
  LowerTanGrowthRate[n] <- PotCellTanGrowthRate / 2
  UpperTanGrowthRate[n] <- PotCellTanGrowthRate / 2

  # [TODO] AHES Not efficient. For every update in cell index the whole vector gets exposed back into globalenv
  UpperTanGrowthRate <<- UpperTanGrowthRate
  UpperRadGrowthRate <<- UpperRadGrowthRate
  LowerTanGrowthRate <<- LowerTanGrowthRate
  LowerRadGrowthRate <<- LowerRadGrowthRate

}


