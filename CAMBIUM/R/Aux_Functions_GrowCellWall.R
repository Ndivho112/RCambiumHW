#' Calculate Primary Wall Thick Rate
#'
#' Calculates the primary wall thickening rate for a cell.
#'
#' @details This function calculates the primary wall thickening rate based on the differentiation status
#' of the cell and the lumen volume. If the cell is not undergoing secondary thickening or does not have
#' a new secondary wall, the lumen volume is calculated assuming a primary wall thickness of 0.25um.
#' The cell wall cross-sectional area is updated accordingly, and the primary wall thickening rate is
#' determined as the difference between the updated cell wall cross-sectional area and the previous value.
#' If the updated cell wall cross-sectional area is smaller than the previous value, the primary wall
#' thickening rate is set to 0.
#'
#' @param n The cell number for which to calculate the primary wall thickening rate.
#'
#' @return None (the function updates variables)
#'
#' @author David Drew
#' @details
#' Code translated from Pascal to R by: ChatGPT, Annemarie Eckes-Shephard
#' @keywords primary wall, thickening, rate, differentiation, lumen volume
#' @export
CalculatePrimaryWallThickRate <- function(n) {
  # Warning if n equals 66 or greater
  if (n >= 66) {
    warning("n is out of valid range: ", n)
    return(NULL)  # Exit function gracefully
  }

  # Check if n is within a valid range
  if (n < 1 || n > length(CellType)) {
    stop("Error: Index n is out of bounds.")
  }

  # Initialize vectors if not already done
  if (!exists("CellWallCSArea0")) CellWallCSArea0 <- numeric(30001)

  # Check DifferentiationStatus and NewSecondaryWallFlag
  if (DifferentiationStatus[n] != "SECONDARYTHICKENING" || NewSecondaryWallFlag[n] == FALSE) {
    print(paste("in CalculatePrimaryWallThickRate at n =", n))

    # Calculating the lumen volume for cells not in secondary wall thickening
    LumenVolume[n] <- BodyVolume(CellType[n], ((cellRD[n] + cellTD[n]) / 2 - PRIMARYWALLTHICKNESS * 2),
                                 ((cellRD[n] + cellTD[n]) / 2) - PRIMARYWALLTHICKNESS * 2, CellLength[n])

    # Ensure LumenVolume[n] is valid before proceeding
    if (is.na(LumenCSArea[n])) {
      stop(paste("problem: LumenCSArea[n] = NA at index", n))
      return(NULL)
    }

    # Calculating the lumen cross-sectional area
    LumenCSArea[n] <- BodyCSArea(CellType[n], LumenVolume[n], CellLength[n], cellRD[n], cellTD[n])

    # Check if LumenCSArea[n] is NA
    if (is.na(LumenCSArea[n])) {
      warning(paste("Warning: LumenCSArea[n] is NA for n =", n))
      return(NULL)  # Exit function to prevent further calculation errors
    } else {
      print(paste("LumenCSArea[n] =", LumenCSArea[n]))
    }

    CellWallCSArea0[n] <- CellWallCSArea[n]
    CellWallCSArea[n] <- CellCSArea[n] - LumenCSArea[n]

    # Ensure CellWallCSArea[n] is valid
    if (is.na(CellWallCSArea[n])) {
      PrimaryWallThickRate[n] <- NA
      warning(paste("Warning: CellWallCSArea[n] is NA for n =", n))
    } else if (CellWallCSArea[n] < CellWallCSArea0[n]) {
      PrimaryWallThickRate[n] <- 0
    } else {
      PrimaryWallThickRate[n] <- CellWallCSArea[n] - CellWallCSArea0[n]
    }

    # Print current values for debugging
    cat("n:", n, "| CellWallCSArea[n]:", CellWallCSArea[n],
        "| CellWallCSArea0[n]:", CellWallCSArea0[n], "\n")

    # Release to global environment only after ensuring all are valid
    LumenCSArea           <<- LumenCSArea
    CellWallCSArea        <<- CellWallCSArea
    PrimaryWallThickRate  <<- PrimaryWallThickRate
    LumenVolume           <<- LumenVolume
  }
}

#' Calculate Potential Wall Thick Rate
#'
#' Calculates the potential wall thickening rate for each cell.
#'
#' @details This function calculates the potential wall thickening rate based on the cell type. If the cell type
#' is not a ray, ray initial X, or ray initial P, the function calculates the maximum thickening cell position and
#' then proceeds with the calculation of the wall thickening rate. The potential volumetric wall thickening rate is
#' calculated as the product of photosynthate per cell for the current day and the wall conversion factor, adjusted
#' to the appropriate units. The potential volumetric wall thickening rate is then converted to a gain in volume
#' and subtracted from the lumen volume to obtain the potential lumen volume. If the potential lumen volume is greater
#' than zero, the potential lumen cross-sectional area is calculated using the body cross-sectional area function.
#' Finally, the potential area wall thickening rate is obtained by subtracting the potential lumen cross-sectional area
#' from the current lumen cross-sectional area. If the cell type is a ray, ray initial X, or ray initial P, the
#' potential area wall thickening rate is set to zero since rays are not allowed to secondary thicken.
#'
#' @param n The cell number for which to calculate the potential wall thickening rate.
#' @param d simulation day
#' @return None (the function updates variables)
#'
#' @author David Drew
#' @details
#' Code translated from Pascal to R by: ChatGPT, Annemarie Eckes-Shephard
#' @keywords potential wall thickening, rate, cell type, lumen volume, cross-sectional area
#' @export
#' @example
CalculatePotentialWallThickRate <- function(day,n) {

  if (CellType[n] != "RAY" && CellType[n] != "RAYINITIALX" && CellType[n] != "RAYINITIALP") {
    if (RadPosition[n] > MaxThickeningCellPosition && !CellDead[n]) {
      MaxThickeningCellPosition <- RadPosition[n]
    }

    # The calculation of the rate of wall thickening rate
    PotentialVolumetricWallThickRate[n] <- (PhotosynthatePerCell[day] * WallConversionFactor) * 10^12
    # The wall conversion factor refers to wall specific volume (cc/g) and PNS/cell is given in g. Therefore we
    # convert cc of wall material laid down to um3 of wall material laid down

    PotentialVolumetricWallThickRate[n] <- PotentialVolumetricWallThickRate[n]
    # Wall thick rate in gain in volume

    if (LumenVolume[n] > PotentialVolumetricWallThickRate[n]) {
      PotentialLumenVolume[n] <- LumenVolume[n] - PotentialVolumetricWallThickRate[n]
    } else {
      PotentialLumenVolume[n] <- 0
    }

    if (PotentialLumenVolume[n] > 0) {
      PotentialLumenCSArea[n] <- BodyCSArea(CellType[n], PotentialLumenVolume[n], CellLength[n], cellRD[n], cellTD[n])
    } else {
      PotentialLumenCSArea[n] <- 0
    }

    PotentialAreaWallThickRate[n] <- LumenCSArea[n] - PotentialLumenCSArea[n]

  } else {
    PotentialAreaWallThickRate[n] <- 0

    #release into globalenv:
    PotentialLumenCSArea             <<- PotentialLumenCSArea
    PotentialAreaWallThickRate       <<- PotentialAreaWallThickRate
    PotentialLumenVolume             <<- PotentialLumenVolume
    PotentialVolumetricWallThickRate <<- PotentialVolumetricWallThickRate
    MaxThickeningCellPosition        <<- MaxThickeningCellPosition

  }
}


#' Calculate Secondary Wall Thick Rate
#'
#' Calculates the amount of transverse wall area and wall thickness added to each cell.
#'
#' @details This function calculates the secondary wall thickening rate based on the differentiation status
#' of the cell. If the cell is undergoing secondary thickening, the new secondary wall flag is set to true.
#' The maximum wall thickening rate is determined based on the user-specified maximum for the species and the
#' potential area wall thickening rate. If the potential area wall thickening rate exceeds the maximum, the
#' wall thickening rate is limited to the maximum value. If the potential area wall thickening rate is less
#' than the minimum wall thickening rate for storage use, the wall thickening rate is set to the minimum value
#' if there is wall thickening stored mass available; otherwise, it is set to the potential area wall thickening
#' rate. If the cell is not undergoing secondary thickening, the wall thickening rate is set to 0.
#'
#' @param n The cell number for which to calculate the secondary wall thickening rate.
#'
#' @return None (the function updates variables)
#'
#' @author David Drew
#' @details
#' Code translated from Pascal to R by: ChatGPT, Annemarie Eckes-Shephard
#' @keywords secondary wall, thickening, rate, differentiation, maximum rate, storage use
#' @export
#' @example
CalculateSecondaryWallThickRate <- function(day,n) {

  if (DifferentiationStatus[n] == "SECONDARYTHICKENING") {
    NewSecondaryWallFlag[n] <- TRUE

    #AHES this replaces function ModifiedMaxWallThickRate
    if(ThickRateEnvironment ==TRUE){
      MaxWallThickeningRate[day] <-  MaxWallThickRate * (1 - fTemp[day])
    }else{
      MaxWallThickeningRate[day] <-  MaxWallThickRate
    }
    #MaxWallThickeningRate[day] <- ModifiedMaxWallThickRate(CB_ThickRateEnvironment)

    if (PotentialAreaWallThickRate[n] > MaxWallThickeningRate[day]) {
      WallThickRate[n] <- MaxWallThickeningRate[day]
      # We limit the wall thickening rate to whatever the user has specified to be the maximum for this species
    } else {
      if (PotentialAreaWallThickRate[n] < MinWallThickRateStorageUse) {
        if (WallThickStoreMass > 0) {
          WallThickRate[n] <- MinWallThickRateStorageUse
        } else {
          WallThickRate[n] <- PotentialAreaWallThickRate[n]
        }
      } else {
        WallThickRate[n] <- PotentialAreaWallThickRate[n]
      }
    }
  } else {
    WallThickRate[n] <- 0
  }


  #release to globalenv
  NewSecondaryWallFlag <<- NewSecondaryWallFlag
  MaxWallThickeningRate <<- MaxWallThickeningRate
  WallThickRate <<- WallThickRate

}



#' CalculateStorageChange
#'
#' Calculates the change in storage for a given cell.
#'
#' @param n The cell number index
#' @param day simulation day
#'
#' @details
#' This function calculates the potential storage increase and decrease volumes and masses for a specific cell based on its properties and status.
#' The calculated values are used to update the storage variables.
#'
#' @return None
#'
#' @author David Drew
#' @details
#' Code translated from Pascal to R by: ChatGPT, Annemarie Eckes-Shephard
#' @keywords Calculate storage change, potential, volume, mass, cell
#' @export
#' @example
CalculateStorageChange <- function(day,n) {
  PotentialStorageIncreaseVolume <- 0
  PotentialStorageDecreaseVolume <- 0

  if (CellType[n] == "FIBRE" || CellType[n] == "XYLEMMOTHERCELL" || CellType[n] == "CAMBIALINITIAL") {
    # To standardize between runs, only fibres, xmc's, and initials are used
    StorageCellCount[day] <- StorageCellCount[day] + 1

    if (DifferentiationStatus[n] == "SECONDARYTHICKENING") {
      if (PotentialAreaWallThickRate[n] > MaxWallThickeningRate[day]) {
        PotentialStorageIncreaseVolume <- LumenVolume[n] - (BodyVolume(CellType[n], (sqrt(((LumenCSArea[n] - (PotentialAreaWallThickRate[n] - MaxWallThickeningRate[day]))) / pi)) * 2,
                                                                                (sqrt(((LumenCSArea[n] - (PotentialAreaWallThickRate[n] - MaxWallThickeningRate[day]))) / pi)) * 2,
                                                                                CellLength[n]))
        # Volume in um3 for cell c

        PotentialStorageIncreaseMass <- ((PotentialStorageIncreaseVolume / 10^12) * (1 / WallConversionFactor) * 1000000) + PotentialStorageIncreaseMass
        # Mass in ug for cell c
      } else {
        if (PotentialAreaWallThickRate[n] < MinWallThickRateStorageUse) {
          # If the potential wall thickening rate from allocated PNS alone is less than the minimum required for the use of stored carbs
          if ((MinWallThickRateStorageUse - PotentialAreaWallThickRate[n]) < LumenCSArea[n]) {
            PotentialStorageDecreaseVolume <- LumenVolume[n] - (BodyVolume(CellType[n], (sqrt(((LumenCSArea[n] - (MinWallThickRateStorageUse - PotentialAreaWallThickRate[n]))) / pi)) * 2,
                                                                                    (sqrt(((LumenCSArea[n] - (MinWallThickRateStorageUse - PotentialAreaWallThickRate[n]))) / pi)) * 2,
                                                                                    CellLength[n]))
            # This ensures that the cell wall is not going to be larger than the lumen making a wall area > cell area
          } else {
            PotentialStorageDecreaseVolume <- LumenVolume[n]
          }

          PotentialStorageDecreaseMass <- ((PotentialStorageDecreaseVolume / 10^12) * (1 / WallConversionFactor) * 1000000) + PotentialStorageDecreaseMass
        }
      }
    } else {
      # We calculate the contribution to or withdrawal from the "WallThickStoreArea" for primary wall thickening cells here
      # because the PotentialAreaWallThickRate has been properly refreshed

      if (PotentialAreaWallThickRate[n] > PrimaryWallThickRate[n]) {
        # Calculate potential increase in storage volume
        PotentialStorageIncreaseVolume <- LumenVolume[n] - BodyVolume(CellType[n], sqrt(((LumenCSArea[n] - (PotentialAreaWallThickRate[n] - PrimaryWallThickRate[n]))/pi))*2,
                                                                               sqrt(((LumenCSArea[n] - (PotentialAreaWallThickRate[n] - PrimaryWallThickRate[n]))/pi))*2,
                                                                               CellLength[n])
        # Volume in um3

        PotentialStorageIncreaseMass <- ((PotentialStorageIncreaseVolume / 10^12) * (1 / WallConversionFactor) * 1000000) + PotentialStorageIncreaseMass
        # Mass in ug
      } else {
        if ((PrimaryWallThickRate[n] - PotentialAreaWallThickRate[n]) < LumenCSArea[n]) {
          # Calculate potential decrease in storage volume
          PotentialStorageDecreaseVolume <- LumenVolume[n] - BodyVolume(CellType[n], sqrt(((LumenCSArea[n] - (PrimaryWallThickRate[n] - PotentialAreaWallThickRate[n]))/pi))*2,
                                                                                 sqrt(((LumenCSArea[n] - (PrimaryWallThickRate[n] - PotentialAreaWallThickRate[n]))/pi))*2,
                                                                                 CellLength[n])
        } else {
          PotentialStorageDecreaseVolume <- LumenVolume[n]
        }

        PotentialStorageDecreaseMass <- ((PotentialStorageDecreaseVolume / 10^12) * (1 / WallConversionFactor) * 1000000) + PotentialStorageDecreaseMass

        if ( (((PotentialStorageDecreaseMass / StorageCellCount[day]) * TotalCellsinStem[day]) / TotalDryMass) <= 0) {
          UpperRadGrowthRate[n] <- 0
          PrimaryWallThickRate[n] <- 0

          PNSLimitedGrowthStopped[day] <- TRUE
        }
      }
    }
  }

  # Release objects into the global environment
  StorageCellCount <<- StorageCellCount
  PotentialStorageIncreaseMass <<- PotentialStorageIncreaseMass
  PotentialStorageDecreaseMass <<- PotentialStorageDecreaseMass
  UpperRadGrowthRate <<- UpperRadGrowthRate
  PrimaryWallThickRate <<- PrimaryWallThickRate
  PNSLimitedGrowthStopped <<- PNSLimitedGrowthStopped
  PotentialStorageIncreaseVolume <<- PotentialStorageIncreaseVolume
  PotentialStorageDecreaseVolume <<- PotentialStorageDecreaseVolume
}



#' CalculateStorage
#'
#' Calculates the storage amount and updates the wall thickness storage mass.
#'
#' @details
#' This function calculates the total storage amount based on the potential storage increase mass
#' and the number of cells in the storage. It then updates the wall thickness storage mass.
#' @param n The cell number index
#' @param day simulation day
#' @return None
#'
#' @author David Drew
#' @details
#' Code translated from Pascal to R by: ChatGPT, Annemarie Eckes-Shephard
#' @keywords  Calculate storage, wall thickness, mass, cells, potential
#' @export
#' @example
CalculateStorage <- function(day) {
  CellDryMass <- rep(0, 30001) # Create an array of CellDryMass

  if (StorageCellCount[day] > 0) {
    TotalStorageAmount <- (PotentialStorageIncreaseMass / StorageCellCount[day]) * TotalCellsinStem[day]
  } else {
    TotalStorageAmount <- 0
  }
  # The total amount of material that is available to be stored in the whole stem,
  # based on the cells counted in the modelled population.
  # Total cells in stem is calculated based on the mean volume of fibres, xmc's or initials.
  TotalDryMass = 1.2# [TODO] 17.05.2023 [HACK] big hack!! sort out!!!
  CellSpecificStorageIncreaseMass <- TotalStorageAmount / TotalDryMass
  # The cell specific storage (ug) per unit dry mass (mg).

  if (CellSpecificStorageIncreaseMass > MaxSDR) {
    CellSpecificStorageIncreaseMass <- MaxSDR
  }
  # The allocation to storage cannot exceed a maximum daily rate.

  if (PNSLimitedGrowthStopped[day] != TRUE) {
    if (StorageCellCount[day] > 0) {
      WallThickStoreMass <- WallThickStoreMass + CellSpecificStorageIncreaseMass - ((PotentialStorageDecreaseMass / StorageCellCount[day]) * TotalCellsinStem[day]) / TotalDryMass
    }
  }

  if (WallThickStoreMass < 0) {
    WallThickStoreMass <- 0
  }

  if (WallThickStoreMass > MaxStorageTotal) {
    WallThickStoreMass <- MaxStorageTotal
  }


  #release to globalenv
  PotentialStorageIncreaseVolume <<- PotentialStorageIncreaseVolume
  PotentialStorageDecreaseVolume <<- PotentialStorageDecreaseVolume
  StorageCellCount <<- StorageCellCount
  PotentialStorageIncreaseMass <<- PotentialStorageIncreaseMass
  PotentialStorageDecreaseMass <<- PotentialStorageDecreaseMass
  UpperRadGrowthRate <<- UpperRadGrowthRate
  PrimaryWallThickRate <<- PrimaryWallThickRate
  PNSLimitedGrowthStopped <<- PNSLimitedGrowthStopped

}



#' Calculate Wall Thickness
#'
#' Calculates the wall thickness of cells.
#'
#' @details This function calculates the wall thickness of cells based on their differentiation status.
#' @param n The cell number index
#'
#' @author David Drew
#' @details
#' Code translated from Pascal to R by: ChatGPT, Annemarie Eckes-Shephard
#' @keywords  cell wall thickness, differentiation status, lumen area, cell wall area
#' @export
#' @example
CalculateWallThickness <- function(n) {
  if (DifferentiationStatus[n] == "SECONDARYTHICKENING") {
    if (LumenCSArea[n] > WallThickRate[n]) {
      LumenCSArea[n] <- LumenCSArea[n] - WallThickRate[n]
    } else {
      LumenCSArea[n] <- 0
    }

    LumenVolume[n] <- BodyVolume(CellType[n], (sqrt((LumenCSArea[n] / pi))) * 2, (sqrt((LumenCSArea[n] / pi))) * 2, CellLength[n])

    CellWallCSArea[n] <- CellCSArea[n] - LumenCSArea[n]
  }

  if (CellWallCSArea[n] > CellCSArea[n]) {
    LumenCSArea[n] <- 0
    CellWallCSArea[n] <- CellCSArea[n]
    LumenVolume[n] <- 0
  }

  CellWallThickness[n] <- ((sqrt(CellCSArea[n] / 3.14)) - sqrt(LumenCSArea[n] / 3.14))


  LumenVolume <<- LumenVolume
  CellWallThickness <<- CellWallThickness
  CellWallCSArea <<- CellWallCSArea
  LumenCSArea <<- LumenCSArea
}



