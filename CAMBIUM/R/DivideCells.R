#' Divide Cells
#'
#' Loop through the entire population of cells and divide those cells that meet the requirements for mitosis and cell division.
#'
#' @details This function iterates over the cell population and divides cells that meet the conditions for division based on various parameters.
#'
#' @return None
#'
#' @author David Drew
#' @details Code translated from Pascal to R by: ChatGPT, Annemarie Eckes-Shephard
#' @keywords Cell Division
#' @export
#' @example
DivideCells <- function(TotalLoopSize,day) {

  #[TODO] These can be created and extended to on the fly in the function later
  #i, j, k:integer; done
  #cellRD_mem : double; done
  #DivisionFactor : double; done
  CritCellCycleDuration <- rep(NA,TotalCells)

  if ((PhotosynthatePerCell[day] > MinAllocatedPNSForGrowth / 1e6) ||
      (WallThickStoreMass > MaxStorageTotal * 0.25)) {
    # Division can only occur if some PNS is allocated or there is a fair amount in storage

    if (((MaxTempMean[day] + MinTempMean[day]) / 2 > MinTempCambialGrowth) &&
        ((MaxTempMean[day] + MinTempMean[day]) / 2 < MaxTempCambialGrowth)) {

      TooManyCellsFlag <- FALSE
      # The minimum temp for division is just an on/off.

      for (LoopNumber in 1:TotalLoopSize) {
        n <- CellKey[LoopNumber] # changed cellnumber to n
        # Control loop size: Ensure n is within bounds
        if (n > 66) {
          n <- 66
        }
        if (n <= 0 || n > length(CellKey)) {
        print(n)
        }

        if (CellType[n] != "PHLOEMMOTHERCELL") {

          CritCellCycleDuration[n] <- MinTimeForMitosis * (2 - (AuxinConc[n] / AuxinPeak[day]))
          CritCellCycleDuration[n] <- CritCellCycleDuration[n] ^ CellCycleSenstoAuxin
        } else {
          CritCellCycleDuration[n] <- 0.1
        }

        if (!CellDead[n]) {
          # We maintain this check as cells may have died in the previous module

          if ((AuxinConc[n] / AuxinPeak[day] >= (MinAuxConcDivision / AuxinPeak[day]) * fCytokinin[day]) &&
              (MeristematicStatus[n] == "MERISTEMATIC")) {

            if ((cellRD[n] > MinDDivision) &&
                (TimeSinceMitosis[n] > CritCellCycleDuration[n])) {

              TotalCells <- TotalCells + 1
              TotalLoopSize <- TotalLoopSize + 1
              CellKey[TotalLoopSize] <- TotalCells

              # Control point to check size of modelled pop. not too large
              if ((TotalCells >= (MAXCELLPOPULATION - 100)) && (TooManyCellsFlag != TRUE)) {
                TooManyCellsFlag <- TRUE
                # The flag just prevents the message popping up continuously until the loop completes.
                ModelProblem <- TRUE
                memo_Warnings$lines.add(paste('The cell population exceeded the maximum number permissable in the present version of the model (',
                                              as.character(MAXCELLPOPULATION), '). The model was stopped on ', logdate[day], sep = ''))
                ModelProblemErrorMessage <- paste('The cell population exceeded the maximum number permissable in the present version of the model (',
                                                  as.character(MAXCELLPOPULATION), '). The model has been stopped on ', logdate[day], sep = '')
              }

              DivisionFactor <- runif(1)

              if (DivisionFactor < 0.4) {
                DivisionFactor <- 0.4
              } else if (DivisionFactor > 0.6) {
                DivisionFactor <- 0.6
              }
              # Division is somewhat random to prevent "groups" dividing at the same time
              print("cell division")
              print("##################################################################################")
              cellRD_mem <- cellRD[n]

              cellRD[n] <- cellRD[n] * DivisionFactor

              # We assume that the length does not change with division...

              CellVolume[n] <- BodyVolume(CellType[n], (cellRD[n] + cellTD[n]) / 2, (cellRD[n] + cellTD[n]) / 2, CellLength[n])
              LumenVolume[n] <- BodyVolume(CellType[n], (cellRD[n] + cellTD[n]) / 2 - (PRIMARYWALLTHICKNESS * 2), (cellRD[n] + cellTD[n]) / 2 - (PRIMARYWALLTHICKNESS * 2), CellLength[n])

              LumenCSArea[n] <- BodyCSArea(CellType[n], LumenVolume[n], CellLength[n],RD = cellRD[n],TD = cellTD[n])
              CellCSArea[n] <- BodyCSArea(CellType[n], CellVolume[n], CellLength[n],RD = cellRD[n],TD = cellTD[n])

              CellWallCSArea[n] <- CellCSArea[n] - LumenCSArea[n]

              TimeSinceMitosis[n] <- 0

              # Now define general various properties for the newly formed cell (whose number is now the "TotalCells" value which cell was its mother)
              print("dividion occured")
              TimeSinceMitosis[TotalCells] <- 0

              MeristematicStatus[TotalCells] <- "MERISTEMATIC"

              DifferentiationStatus[TotalCells] <- "GROWING"

              MotherCellNumber[TotalCells] <- n

              AuxinConc[TotalCells] <- AuxinConc[n]

              cellRD[TotalCells] <- cellRD_mem * (1 - DivisionFactor)
              cellTD[TotalCells] <- cellTD[n]

              # We assume that the length does not change with division...

              CellType[TotalCells] <- "FUSINITIAL"

              CellLength[TotalCells] <- CellLength[n]

              CellVolume[TotalCells] <- BodyVolume(CellType[TotalCells], (cellRD[TotalCells] + cellTD[TotalCells]) / 2, (cellRD[TotalCells] + cellTD[TotalCells]) / 2, CellLength[TotalCells])
              LumenVolume[TotalCells] <- BodyVolume(CellType[TotalCells], (cellRD[TotalCells] + cellTD[TotalCells]) / 2 - (PRIMARYWALLTHICKNESS * 2), (cellRD[TotalCells] + cellTD[TotalCells]) / 2 - (PRIMARYWALLTHICKNESS * 2), CellLength[TotalCells])

              LumenCSArea[TotalCells] <- BodyCSArea(CellType[TotalCells], LumenVolume[TotalCells], CellLength[TotalCells])
              CellCSArea[TotalCells] <- BodyCSArea(CellType[TotalCells], CellVolume[TotalCells], CellLength[TotalCells])

              CellWallCSArea[TotalCells] <- CellCSArea[TotalCells] - LumenCSArea[TotalCells]

              TanPosition[TotalCells] <- TanPosition[n]

              FormationDate[TotalCells] <- logdate[day]
              FormationDay[TotalCells] <- day

              UpperTanInitialNeighbour[TotalCells] <- UpperTanInitialNeighbour[n]
              LowerTanInitialNeighbour[TotalCells] <- LowerTanInitialNeighbour[n]

              if (CellType[n] == "CAMBIALINITIAL" || CellType[n] == "RAYINITIALC") {
                # If the cell is an initial cell

                PreviousAuxinCanal[n] <- TRUE
                CurrentAuxinCanal[n] <- FALSE
                # The daughter takes over the auxin canal status

                CurrentAuxinCanal[TotalCells] <- TRUE
                InitialCell[TotalCells] <- TotalCells
                InitialCell[n] <- TotalCells

                for (i in 1:TotalLoopSize) {
                  # Redefine the "initial cell" for all cells in this file

                  j <- CellKey[i]

                  if (InitialCell[j] == n) {
                    InitialCell[j] <- TotalCells
                  }
                }

                if (CellType[n] == "CAMBIALINITIAL") {
                  CellType[TotalCells] <- "CAMBIALINITIAL"
                } else {
                  CellType[TotalCells] <- "RAYINITIALC"
                }
                print(paste("n =", n))
                print(paste("UpperRadNeighbour[n] =", UpperRadNeighbour[n]))
                print(paste("CellType[UpperRadNeighbour[n]] =", CellType[UpperRadNeighbour[n]]))

                if (!is.na(UpperRadNeighbour[n]) && !is.na(CellType[UpperRadNeighbour[n]]) &&
                    (CellType[UpperRadNeighbour[n]] == "PHLOEMMOTHERCELL" ||
                     CellType[UpperRadNeighbour[n]] == "RAYINITIALP" ||
                     CambiumWidth[day] < 7)) {
                  # If at least one PMC exists or "foliage conditions" are bad
                  # then produce XMC's

                  if (CellType[n] == "CAMBIALINITIAL") {
                    CellType[n] <- "XYLEMMOTHERCELL"
                  } else {
                    CellType[n] <- "RAYINITIALX"
                  }

                  # The daughter cell is positioned "bark-ward" of the mother cell

                  RadPosition[TotalCells] <- RadPosition[n] - (cellRD_mem/2) + cellRD[n] + (cellRD[TotalCells]/2)
                  RadPosition[n] <- RadPosition[n] - (cellRD_mem/2) + cellRD[n]/2

                } else {
                  # if no PMC's exist, or at least conditions are OK

                  if (CellType[n] == "CAMBIALINITIAL") {
                    CellType[n] <- "PHLOEMMOTHERCELL"
                  } else {
                    CellType[n] <- "RAYINITIALP"
                  }

                  # The daughter cell is positioned "pith-ward" of the mother cell

                  RadPosition[TotalCells] <- RadPosition[n] - (cellRD_mem/2) + cellRD[TotalCells]/2
                  RadPosition[n] <- RadPosition[n] - (cellRD_mem/2) + cellRD[TotalCells] + (cellRD[n]/2)
                }
              } else {
                InitialCell[TotalCells] <- InitialCell[n]

                if (CellType[n] == "XYLEMMOTHERCELL" || CellType[n] == "RAYINITIALX") {
                  if (CellType[n] == "XYLEMMOTHERCELL") {
                    CellType[TotalCells] <- "XYLEMMOTHERCELL"
                  } else {
                    CellType[TotalCells] <- "RAYINITIALX"
                  }

                  # The daughter cell is positioned "bark-ward" of the mother cell

                  RadPosition[TotalCells] <- RadPosition[n] - (cellRD_mem/2) + (cellRD[n]) + (cellRD[TotalCells]/2)
                  RadPosition[n] <- RadPosition[n] - (cellRD_mem/2) + cellRD[n]/2

                } else if (CellType[n] == "PHLOEMMOTHERCELL" || CellType[n] == "RAYINITIALP") {
                  if (CellType[n] == "PHLOEMMOTHERCELL") {
                    CellType[TotalCells] <- "PHLOEMMOTHERCELL"
                  } else {
                    CellType[TotalCells] <- "RAYINITIALP"
                  }

                  # The daughter cell is positioned "pith-ward" of the mother cell

                  RadPosition[TotalCells] <- RadPosition[n] - (cellRD_mem/2) + cellRD[TotalCells]/2
                  RadPosition[n] <- RadPosition[n] - (cellRD_mem/2) + (cellRD[TotalCells]) + (cellRD[n]/2)
                }

              }#if (CellType[cellnumber] = CAMBIALINITIAL)
            }#if (cellRD[cellnumber] > MinDDivision)
          }#if (auxinconc[cellnumber]
        }#if celldead[cellnumber]<>true
      }#for LoopNumber :=1


    } else { #if ((MaxTempMean[logday]
      print(paste('WARNING:Temperature was unsuitable for cell division on', logdate[day]))
    }

  } else {#if (PhotosynthatePerCell[logday]
    print(paste('WARNING:No photosynthate was available for metabolic activity and cell plate formation on', logdate[day]))
  }


  # Assign objects to the global environment
  CritCellCycleDuration <<- CritCellCycleDuration
  TooManyCellsFlag <<- TooManyCellsFlag
  TotalCells <<- TotalCells
  TotalLoopSize <<- TotalLoopSize
  CellKey <<- CellKey
  cellRD <<- cellRD
  CellVolume <<- CellVolume
  LumenVolume <<- LumenVolume
  LumenCSArea <<- LumenCSArea
  CellCSArea <<- CellCSArea
  CellWallCSArea <<- CellWallCSArea
  TimeSinceMitosis <<- TimeSinceMitosis
  MeristematicStatus <<- MeristematicStatus
  DifferentiationStatus <<- DifferentiationStatus
  MotherCellNumber <<- MotherCellNumber
  AuxinConc <<- AuxinConc
  CellType <<- CellType
  CellLength <<- CellLength
  TanPosition <<- TanPosition
  FormationDate <<- FormationDate
  FormationDay <<- FormationDay
  UpperTanInitialNeighbour <<- UpperTanInitialNeighbour
  LowerTanInitialNeighbour <<- LowerTanInitialNeighbour
  UpperRadNeighbour <<- UpperRadNeighbour
  LowerRadNeighbour <<- UpperRadNeighbour
  PreviousAuxinCanal <<- PreviousAuxinCanal
  CurrentAuxinCanal <<- CurrentAuxinCanal
  InitialCell <<- InitialCell
  RadPosition <<- RadPosition

}
