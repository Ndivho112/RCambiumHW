#' Calculate Cell-Specific Allocation
#'
#' This function calculates the amount of photosynthate allocated to the stem that is available to each cell.
#'
#' @details The function calculates the number of cells in the stem, the volume of living xylem tissue, the total dry mass of the stem, the volume of dead tissue in the stem, and the average amount of photosynthate that can be allocated to each living cell in the stem.
#'
#' @param day simulation day
#' @return None
#'
#' @author David Drew
#' @details
#' Code translated from Pascal to R by: ChatGPT, Annemarie Eckes-Shephard
#' @keywords Allocation, Stem, Photosynthate
#' @export
#' @example
CalculateCellSpecificAllocation <- function(day) {

  # Calculate the number of cells in the stem
  # We calculate the volume of the stem (i.e., up to green height): all units m
  # TotalStemVolume (function) local variable, but updated, because  useful as output for plots:
  TotalStemVolume[day] <- ConeVolume((DBH_CAMBIUM[day]/100), (DBH_CAMBIUM[day]/100), (TreeHeight[day]))

  # We calculate the radial width (on both sides) of living xylem tissue: all units m

  if (DBH_CAMBIUM[day] > SAPWOODWIDTH*2) {
    if (ConeVolume(((DBH_CAMBIUM[day]-((SAPWOODWIDTH*2)/1000))/100), ((DBH_CAMBIUM[day]-((SAPWOODWIDTH*2)/1000))/100), (TreeHeight[day])) < TotalStemVolume[day]) {
      SapWoodVolume[day] <- TotalStemVolume[day] - ConeVolume(((DBH_CAMBIUM[day]-((SAPWOODWIDTH*2)/1000))/100), ((DBH_CAMBIUM[day]-((SAPWOODWIDTH*2)/1000))/100), (TreeHeight[day]))
    } else {
      SapWoodVolume[day] <- TotalStemVolume[day]
    }
  } else {
    SapWoodVolume[day] <- TotalStemVolume[day]
  }

  if (MeanSapwoodDensity > 0) {
    TotalDryMass <- (MeanSapwoodDensity * SapWoodVolume[day]) * 1000000
    # The total average dry mass of the stem is calculated (in mg)
  } else {
    TotalDryMass <- (b0Density/2 * SapWoodVolume[day]) * 1000000
  }

  LivingXylemTissueWidth[day] <- 2*(MaxInitialPosition[day] - MaxDeadCellPosition) / 1000000

  # We calculate the volume of dead tissue in the stem: all units m

  DeadStemVolume[day] <- ConeVolume((DBH_CAMBIUM[day]/100 - LivingXylemTissueWidth[day]), (DBH_CAMBIUM[day]/100 - LivingXylemTissueWidth[day]), (TreeHeight[day]))

  # We calculate the volume of living xylem tissue: units converted to um3
  LivingXylemTissueVolume[day] <- (TotalStemVolume[day] - DeadStemVolume[day]) * 10^18

  TotalCellsinStem[day] <- LivingXylemTissueVolume[day] / AverageCellVolume[day]

  if (TotalCellsinStem[day] > 0) {
    # Calculate the average amount of photosynthate which can be allocated to all
    # living cells in the stem of the tree

    # The stem allocation mean is converted to g at this point from kg
    if (day > (1/PhloemTransportLag) * (GreenHt[day])) {
      PhotosynthatePerCell[day] <- (StemAllocationMean[round(day - ((1/PhloemTransportLag) * (GreenHt[day])))] * 1000) / TotalCellsinStem[day]
      # PNS per cell in g...
    } else {
      PhotosynthatePerCell[day] <- (StemAllocationMean[day] * 1000) / TotalCellsinStem[day]
      # PNS per cell in g...
    }
  }

  if (PhotosynthatePerCell[day] < 0) {
    PhotosynthatePerCell[day] <- 0
  }

  assign("PhotosynthatePerCell", PhotosynthatePerCell, envir = .GlobalEnv)
  assign("TotalCellsinStem", TotalCellsinStem, envir = .GlobalEnv)
  assign("LivingXylemTissueVolume", LivingXylemTissueVolume, envir = .GlobalEnv)
  assign("TotalStemVolume", TotalStemVolume, envir = .GlobalEnv)
  assign("TotalDryMass", TotalDryMass, envir = .GlobalEnv)
  assign("SapWoodVolume", SapWoodVolume, envir = .GlobalEnv)
}
