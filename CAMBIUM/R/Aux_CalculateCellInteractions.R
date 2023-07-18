#' Calculate the maximum distance pithward that the cell can expand
#'
#' This function calculates the maximum distance pithward that the cell can expand based on certain conditions.
#'
#' @details The function checks if the potential growth of the cell would infringe upon another cell's barkward edge. If it does, and the other cell meets certain conditions, the maximum crushable distance is updated.
#'
#' @param n The cell number for which to calculate the crushable distance.
#' @param positioncellcounter The position cell counter.
#'
#' @return The updated MaxRadNonCrushableCellDistanceLower value.
#'
#' @author David Drew
#' @details
#' Code translated from Pascal to R by: ChatGPT, Annemarie Eckes-Shephard
#' @keywords CalculateRadCrushableDistance
#' @export
#' @example
CalculateRadCrushableDistance <- function(n, positioncellcounter) {

  if ((RadPosition[n] - cellTD[n]/2) < RadPosition[positioncellcounter] &&
      (RadPosition[n] + cellTD[n]/2) > RadPosition[positioncellcounter]) {

    if (RadPosition[positioncellcounter] < RadPosition[n]) {
      # The potential growth of "n" would infringe upon "positioncellcounter's" barkward edge

      if (DifferentiationStatus[positioncellcounter] == "SECONDARYTHICKENING" ||
          CellType[positioncellcounter] == "VESSEL" ||
          CellType[positioncellcounter] == "CAMBIALINITIAL" ||
          CellType[positioncellcounter] == "XYLEMMOTHERCELL" ||
          CellType[positioncellcounter] == "PHLOEMMOTHERCELL" ||
          CellType[positioncellcounter] == "RAYINITIALC" ||
          CellType[positioncellcounter] == "RAYINITIALX" ||
          CellType[positioncellcounter] == "RAYINITIALP" ||
          CellType[positioncellcounter] == "RAYINITIAL" ||
          CellType[positioncellcounter] == "RAY" ||
          CellType[positioncellcounter] == "VESSEL" ||
          CellType[positioncellcounter] == "" ||
          (CellDead[positioncellcounter] == TRUE && CellCrushed[positioncellcounter] != TRUE)) {

        if ((RadPosition[positioncellcounter] + cellRD[positioncellcounter]/2) >= MaxRadNonCrushableCellDistanceLower[n]) {
          MaxRadNonCrushableCellDistanceLower[n] <<- RadPosition[positioncellcounter] + cellRD[positioncellcounter]/2
        }
      }
    }
  }
# exposed to globenv above. sometimes no update ( many if-statements until that last evaluation
# really takes place) , so not executing this code here.
# return(MaxRadNonCrushableCellDistanceLower)
}


#' CalculateTanCrushableDistance
#'
#' Calculate the maximum distance left and right of the current cell that it can expand tangentially
#'
#' @details This function calculates the maximum distance left and right of the current cell that it can expand tangentially based on certain conditions. It checks if the current cell's radial position and radius overlap with another cell's radial position and radius. If they do, it evaluates the differentiation status and cell type of the other cell to determine if it can affect the expansion of the current cell. The function updates the maximum non-crushable distances accordingly.
#' @param n cell number index
#'
#' @return None
#'
#' @author David Drew
#' @details
#' code translated from Pascal to R by: ChatGPT, Annemarie Eckes-Shephard
#' @keywords  CalculateTanCrushableDistance
#' @export
#' @example
CalculateTanCrushableDistance <- function(n,PositionCellCounter) {
  #******************************************************************************
  # Calculate the maximum distance left and right of the current cell that it can expand tangentially
  #******************************************************************************

  if ((RadPosition[n] + cellRD[n]/2) > (RadPosition[PositionCellCounter] - cellRD[PositionCellCounter]/2) &&
      (RadPosition[n] - cellRD[n]/2) < (RadPosition[PositionCellCounter] + cellRD[PositionCellCounter]/2)) {

    if (DifferentiationStatus[PositionCellCounter] == "SECONDARYTHICKENING" ||
        CellType[PositionCellCounter] == "RAY" ||
        CellType[PositionCellCounter] == "VESSEL" ||
        (CellDead[PositionCellCounter] == TRUE && CellCrushed[PositionCellCounter] != TRUE) ||
        CellType[PositionCellCounter] == "CAMBIALINITIAL" ||
        (CellType[PositionCellCounter] == "XYLEMMOTHERCELL" && XMCCrush == FALSE &&
         (RadPosition[InitialCell[PositionCellCounter]] - RadPosition[PositionCellCounter]) < 35) ||
        CellType[PositionCellCounter] == "RAYINITIAL" ||
        CellType[PositionCellCounter] == "RAYINITIALC" ||
        CellType[PositionCellCounter] == "RAYINITIALP" ||
        CellType[PositionCellCounter] == "RAYINITIALX") {
      if (TanPosition[PositionCellCounter] < TanPosition[n]) {
        if ((TanPosition[PositionCellCounter] + cellTD[PositionCellCounter]/2) > MaxTanNonCrushableCellDistanceLower[n]) {
          # The potential growth of "n" would infringe upon "positioncellcounter's"
          # right edge
          MaxTanNonCrushableCellDistanceLower[n] <<- TanPosition[PositionCellCounter] + cellTD[PositionCellCounter]/2
        }
      } else if (TanPosition[PositionCellCounter] > TanPosition[n]) {
        if ((TanPosition[PositionCellCounter] - cellTD[PositionCellCounter]/2) < MaxTanNonCrushableCellDistanceUpper[n]) {
          # The potential growth of "n" would infringe upon "positioncellcounter's"
          # left edge
          MaxTanNonCrushableCellDistanceUpper[n] <<- TanPosition[PositionCellCounter] - cellTD[PositionCellCounter]/2
        }
      }
    }
  }
  #released to globalenv above
}

#' ActualGrowth_CrushableDistanceConstraint
#'
#' This function calculates the actual growth as constrained by nearby cells that are "incompressible" based on a "maximum non-crushable distance".
#'
#' @details This function calculates the actual growth rates of the cell, taking into account the constraints imposed by neighboring cells that are "incompressible". It checks if the lower radial growth rate or the upper tangential growth rate of the cell violate the maximum non-crushable distances. If they do, the growth rates are adjusted accordingly.
#' @param n The cell number for which to calculate the crushable distance.
#'
#' @return None
#'
#' @author David Drew
#' @details
#' code translated from Pascal to R by: ChatGPT, Annemarie Eckes-Shephard
#' @keywords  ActualGrowth_CrushableDistanceConstraint
#' @export
#' @example
ActualGrowth_CrushableDistanceConstraint <- function(n) {
  PressureModifier <- NULL

  #******************************************************************************
  # This function calculates the actual growth as constrained by cells nearby which
  # are "incompressible": based on a "maximum non-crushable distance"
  #******************************************************************************

  if (RadPosition[n] - cellRD[n]/2 - LowerRadGrowthRate[n] < MaxRadNonCrushableCellDistanceLower[n]) {

    if (LowerRadGrowthRate[n] > (RadPosition[n] - cellRD[n]/2) - MaxRadNonCrushableCellDistanceLower[n]) {
      LowerRadGrowthRate[n] <- (RadPosition[n] - cellRD[n]/2) - MaxRadNonCrushableCellDistanceLower[n]
    } else {
      LowerRadGrowthRate[n] <- LowerRadGrowthRate[n]
    }

  }

  if ((LowerTanGrowthRate[n] > 0) || (UpperTanGrowthRate[n] > 0)) {

    if ((TanPosition[n] - cellTD[n]/2 - LowerTanGrowthRate[n]) < MaxTanNonCrushableCellDistanceLower[n]) {
      LowerTanGrowthRate[n] <- (TanPosition[n] - cellTD[n]/2) - MaxTanNonCrushableCellDistanceLower[n]
    }

    if ((TanPosition[n] + cellTD[n]/2 + UpperTanGrowthRate[n]) > MaxTanNonCrushableCellDistanceUpper[n]) {
      UpperTanGrowthRate[n] <- MaxTanNonCrushableCellDistanceUpper[n] - (TanPosition[n] + cellTD[n]/2)
    }

  } else {
    LowerTanGrowthRate[n] <- 0
    UpperTanGrowthRate[n] <- 0
  }

  UpperTanGrowthRate <<- UpperTanGrowthRate
  LowerTanGrowthRate <<- LowerTanGrowthRate
}


#' ActualGrowth_RadTanRatioConstraint
#'
#' This function calculates the actual growth rates of the cell, taking into account the radial-tangential ratio constraint. It ensures that the ratio of the sum of tangential growth rates to the sum of radial growth rates does not exceed a specified threshold.
#'
#' @details This function checks if the current radial and tangential growth rates, along with their upper and lower limits, violate the radial-tangential ratio constraint. If the constraint is violated, the growth rates are adjusted accordingly to satisfy the constraint.
#'
#' @return None
#'
#' @author David Drew
#' @details
#' code translated from Pascal to R by: ChatGPT, Annemarie Eckes-Shephard
#' @keywords  ActualGrowth_RadTanRatioConstraint
#' @export
#' @example
ActualGrowth_RadTanRatioConstraint <- function(n) {
  if (cellRD[n] > 0 && cellTD[n] > 0) {
    if ((cellTD[n] + UpperTanGrowthRate[n] + LowerTanGrowthRate[n]) / (cellRD[n] + LowerRadGrowthRate[n] + UpperRadGrowthRate[n]) > RadTanExtRatio) {
      if (CellType[n] == "VESSEL") {
        if ((((cellRD[n] + LowerRadGrowthRate[n] + UpperRadGrowthRate[n]) * RadTanExtRatio) - cellTD[n]) < UpperTanGrowthRate[n]) {
          UpperTanGrowthRate[n] <- (((cellRD[n] + LowerRadGrowthRate[n] + UpperRadGrowthRate[n]) * RadTanExtRatio) - cellTD[n])
        }
        if ((((cellRD[n] + LowerRadGrowthRate[n] + UpperRadGrowthRate[n]) * RadTanExtRatio) - cellTD[n]) < LowerTanGrowthRate[n]) {
          if (LowerRadGrowthRate[n] > 0) {
            LowerTanGrowthRate[n] <- (((cellRD[n] + LowerRadGrowthRate[n] + UpperRadGrowthRate[n]) * RadTanExtRatio) - cellTD[n])
          }
        }
      } else {
        if ((((cellRD[n] + LowerRadGrowthRate[n] + UpperRadGrowthRate[n]) * RadTanExtRatio) - cellTD[n]) < UpperTanGrowthRate[n]) {
          UpperTanGrowthRate[n] <- ((cellRD[n] + LowerRadGrowthRate[n] + UpperRadGrowthRate[n]) * RadTanExtRatio) - cellTD[n]
        }
      }
    }
    if ((cellRD[n] + UpperRadGrowthRate[n] + LowerRadGrowthRate[n]) / (cellTD[n] + LowerTanGrowthRate[n] + UpperTanGrowthRate[n]) > RadTanExtRatio) {
      if (CellType[n] == "VESSEL") {
        if ((((cellTD[n] + LowerTanGrowthRate[n] + UpperTanGrowthRate[n]) * RadTanExtRatio) - cellRD[n]) < UpperRadGrowthRate[n]) {
          UpperRadGrowthRate[n] <- (((cellTD[n] + LowerTanGrowthRate[n] + UpperTanGrowthRate[n]) * RadTanExtRatio) - cellRD[n])
        }
        if ((((cellTD[n] + LowerTanGrowthRate[n] + UpperTanGrowthRate[n]) * RadTanExtRatio) - cellRD[n]) < LowerRadGrowthRate[n]) {
          if (LowerRadGrowthRate[n] > 0) {
            if (LowerRadGrowthRate[n] > (((cellTD[n] + LowerTanGrowthRate[n] + UpperTanGrowthRate[n]) * RadTanExtRatio) - cellRD[n])) {
              LowerRadGrowthRate[n] <- (((cellTD[n] + LowerTanGrowthRate[n] + UpperTanGrowthRate[n]) * RadTanExtRatio) - cellRD[n])
            } else {
              LowerRadGrowthRate[n] <- LowerRadGrowthRate[n]
            }
          }
        }
      } else {
        if ((((cellTD[n] + LowerTanGrowthRate[n] + UpperTanGrowthRate[n]) * RadTanExtRatio) - cellRD[n]) < UpperRadGrowthRate[n]) {
          UpperRadGrowthRate[n] <- ((cellTD[n] + LowerTanGrowthRate[n] + UpperTanGrowthRate[n]) * RadTanExtRatio) - cellRD[n]
        }
      }
    }
    # release to globalenv
    LowerRadGrowthRate <<- LowerRadGrowthRate
    UpperRadGrowthRate <<- UpperRadGrowthRate
    UpperTanGrowthRate <<- UpperTanGrowthRate
    LowerTanGrowthRate <<- LowerTanGrowthRate

  }

}


#' CalculateRadialShift
#'
#' Calculate/re-calculate the total growth "behind" each cell to use for individual cell positions.
#'
#' @details This function calculates the radial shift of cells based on various conditions and parameters.
#'
#' @return Returns the updated values of NewRadialShift.
#'
#' @author David Drew
#' @details
#' Code translated from Pascal to R by: ChatGPT, Annemarie Eckes-Shephard
#' @keywords  calculate, radial shift, cell position
#' @export
#' @example
CalculateRadialShift <- function() {
  # Here we calculate/re-calculate the total growth "behind" each cell
  # to use to calculate individual cell positions
  VesselStoppedGrowingFlag <- rep(NA,length(TotalCells)) #only used internally

  for (loopnumber in 1:TotalLoopSize) {
    cellnumber <- CellKey[loopnumber]

    for (i in 1:TotalLoopSize) {
      positioncellcounter <- CellKey[i]

      if ((TanPosition[positioncellcounter] < TanPosition[cellnumber] + cellTD[cellnumber]/2) &
          (TanPosition[positioncellcounter] > TanPosition[cellnumber] - cellTD[cellnumber]/2)) {

        if (RadPosition[cellnumber] < RadPosition[positioncellcounter]) {
          if ((DifferentiationStatus[positioncellcounter] == "SECONDARYTHICKENING") &
              (CellType[positioncellcounter] == "VESSEL")) {
            VesselStoppedGrowingFlag[cellnumber] <- TRUE
          } else {
            VesselStoppedGrowingFlag[cellnumber] <- FALSE
          }
        }
      }
    }
  }

  for (loopnumber in 1:TotalLoopSize) {
    cellnumber <- CellKey[loopnumber]

    if (CritWallThickTimeFlag[cellnumber]) {
      if (cellRD[cellnumber] < MinRDSecThick) {
        if (UpperRadGrowthRate[cellnumber] == 0) {
          UpperRadGrowthRate[cellnumber] <- MinRDSecThick - cellRD[cellnumber]
        }
      }
    }

    for (i in 1:TotalLoopSize) {
      positioncellcounter <- CellKey[i]

      if ((TanPosition[positioncellcounter] < TanPosition[cellnumber] + cellTD[cellnumber]/2) &
          (TanPosition[positioncellcounter] > TanPosition[cellnumber] - cellTD[cellnumber]/2)) {

        if (RadPosition[cellnumber] < RadPosition[positioncellcounter]) {
          if (!VesselStoppedGrowingFlag[cellnumber]) {
            NewRadialShift[positioncellcounter] <- NewRadialShift[positioncellcounter] + UpperRadGrowthRate[cellnumber]
            # The total growth of cells pithward of the current cell
          }
        }
      }
    }
  }

  NewRadialShift <<- NewRadialShift
  UpperRadGrowthRate <<- UpperRadGrowthRate
}


#' Calculate Actual Size
#'
#' This function calculates the actual size of cells based on growth rates.
#'
#' @details The function updates the values of cellRD, cellTD, RadPosition, TanPosition, CellLength, CellVolume, and CellCSArea for each cell.
#'
#' @param cellnumber The cell number to calculate the actual size for.
#' @param positioncellcounter passed on function CrushCells.
#' @return NULL (The function updates the provided arrays by reference.)
#'
#' @author David Drew
#' @details
#' code translated from Pascal to R by: ChatGPT, Annemarie Eckes-Shephard
#' @keywords "Calculate Actual Size", cell growth, cell size
#' @export
#' @example
CalculateActualSize <- function(n,positioncellcounter) {

  cellRD0 <- cellRD[n]
  cellTD0 <- cellTD[n] # cellTD0 not used anywhere
  # to be in keeping with the logic below, I would use cellRD0 and cellTD0 for the below two lines ( or leave these two out alltogether?!)
  cellRD[n] <- cellRD[n] + UpperRadGrowthRate[n] + LowerRadGrowthRate[n]
  cellTD[n] <- cellTD[n] + UpperTanGrowthRate[n] + LowerTanGrowthRate[n]

  if (cellRD[n] < 0 || cellTD[n] < 0){
    CrushCells(positioncellcounter)# wants positioncellcounter
  }


  RadPosition0 <- RadPosition[n]
  TanPosition0 <- TanPosition[n] # TanPosition0 could be directly replaced with TanPosition..

  RadPosition[n] <- RadPosition0 + (UpperRadGrowthRate[n] - LowerRadGrowthRate[n])/2
  TanPosition[n] <- TanPosition0 + (UpperTanGrowthRate[n] - LowerTanGrowthRate[n])/2

  if (CellType[n] != "VESSEL"){
    CellLength[n] <- CalculateCellLength(CellType[n], cellRD[n])
  }else{
    CellLength[n] <- 500
  }


  CellVolume[n] <- BodyVolume(CellType[n], (cellRD[n] + cellTD[n])/2,
                                       (cellRD[n] + cellTD[n])/2, CellLength[n])

  CellCSArea[n] <- BodyCSArea(CellType[n], CellVolume[n], CellLength[n], RD = cellRD[n], TD = cellTD[n])


  # release to globalenv
  CellCSArea <<- CellCSArea
  CellVolume <<- CellVolume
  RadPosition <<- RadPosition
  TanPosition <<- TanPosition
  CellLength <<- CellLength
  cellRD <<- cellRD
  cellTD <<- cellTD

}


#' Crush Cells
#'
#' This function crushes cells based on their type and differentiation status.
#'
#' @details It checks the cell type and differentiation status of each cell and crushes the cells that meet certain conditions. The crushed cells have their cell dimensions and volumes set to zero, and their status is updated accordingly.
#'
#' @author David Drew
#' @details
#' Code translated from Pascal to R by: ChatGPT, Annemarie Eckes-Shephard
#'
#' @export
CrushCells <- function(positioncellcounter) {
  print(positioncellcounter)
  if (CellType[positioncellcounter] != "CAMBIALINITIAL" &&
      CellType[positioncellcounter] != "PHLOEMMOTHERCELL" &&
      CellType[positioncellcounter] != "RAY" &&
      CellType[positioncellcounter] != "RAYINITIALX" &&
      CellType[positioncellcounter] != "RAYINITIALC" &&
      CellType[positioncellcounter] != "RAYINITIALP" &&
      DifferentiationStatus[positioncellcounter] != "SECONDARYTHICKENING") {
    if ((CellType[positioncellcounter] != "XYLEMMOTHERCELL" && XMCCrush == FALSE) ||
        XMCCrush == TRUE) {
      cellTD[positioncellcounter] <<- 0
      cellRD[positioncellcounter] <<- 0
      cellcrushed[positioncellcounter] <<- TRUE
      celldead[positioncellcounter] <<- TRUE
      MeristematicStatus[positioncellcounter] <<- "DEAD"
      DifferentiationStatus[positioncellcounter] <<- "DEAD"
      CellVolume[positioncellcounter] <<- 0
      LumenVolume[positioncellcounter] <<- 0
      CellCSArea[positioncellcounter] <<- 0
      CellWallCSArea[positioncellcounter] <<- 0
      CellWallThickness[positioncellcounter] <<- 0
    }
  }
}


#' Remove Crushed Cells
#'
#' Crush and remove all cells which fall within the boundaries of the current "cellnumber".
#'
#' @details This function checks if a cell falls within the boundaries defined by the current "cellnumber" in terms of its radial and tangential positions. If a cell falls within these boundaries, it is considered crushed and removed from the system.
#'
#' @param positioncellcounter The position of the current cell being checked.
#' @param cellnumber The current cell number defining the boundaries.
#'
#' @author David Drew
#' @details
#' Code translated from Pascal to R by: ChatGPT, Annemarie Eckes-Shephard
#' @keywords Remove Crushed Cells
#' @export
#' @example
RemoveCrushedCells <- function(positioncellcounter, cellnumber) {
  if ((TanPosition[positioncellcounter] + cellTD[positioncellcounter]/2) < (TanPosition[cellnumber] + cellTD[cellnumber]/1.65) &&
      (TanPosition[positioncellcounter] - cellTD[positioncellcounter]/2) > (TanPosition[cellnumber] - cellTD[cellnumber]/1.65) &&
      (RadPosition[positioncellcounter] + cellRD[positioncellcounter]/2) < (RadPosition[cellnumber] + cellRD[cellnumber]/1.65) &&
      (RadPosition[positioncellcounter] - cellRD[positioncellcounter]/2) > (RadPosition[cellnumber] - cellRD[cellnumber]/1.65)) {

    CrushCells(positioncellcounter)
  }
}
