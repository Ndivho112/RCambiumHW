#' Grow Cell Wall
#'
#' The main wall thickening module.
#'
#' @details This function calculates the growth of the cell wall for each cell in the loop.
#' It performs calculations for primary wall thickening rate, potential wall thickening rate,
#' secondary wall thickening rate, storage change, and wall thickness.
#' @author David Drew
#' @details
#' Code translated from Pascal to R by: ChatGPT, Annemarie Eckes-Shephard
#' @keywords cell wall, growth, thickening
#' @export
#'
#' @return None (the function performs calculations and updates variables)
#'
GrowCellWall <- function(day,TotalLoopSize) {
  PotentialStorageIncreaseMass <<- 0
  PotentialStorageDecreaseMass <<- 0

  # Perform calculations for each cell in the loop
  for (Loopnumber in 1:TotalLoopSize) {
    n <- CellKey[Loopnumber] # AHES changed CellNumber ( in .pas) to n, as only exposed within this function

    if (CellDead[n] != TRUE && CellCrushed[n] != TRUE) {
      CalculatePrimaryWallThickRate(n)
      CalculatePotentialWallThickRate(day,n)
      CalculateSecondaryWallThickRate(day,n)
      CalculateStorageChange(day,n)
    }
  }

  CalculateStorage(day)

  # Calculate wall thickness for relevant cell types
  for (Loopnumber in 1:TotalLoopSize) {
    n <- CellKey[Loopnumber]

    if (CellType[n] != "RAY" && CellType[n] != "RAYINITIALX" && CellType[n] != "RAYINITIALP") {
      if (CellCrushed[n] != TRUE && CellCrushed[n] != TRUE) {
        CalculateWallThickness(n)
      }
    }
  }
}
