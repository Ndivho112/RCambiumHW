#' DetermineEarlyCambialCells
#'
#' Determines the identity of all cells in the cambium at initialization.
#'
#' @details This function iterates through each cell and determines its identity based on the cell type and radial position.
#' If the cell is a fusiform initial or cambial initial, its identity is determined as either phloem mother cell or xylem mother cell, depending on the radial position.
#' If the cell is a ray initial, its identity is determined as either ray initial (primary) or ray initial (xylem), depending on the radial position.
#' The meristematic status of all cells is set to "MERISTEMATIC" and the differentiation status is set to "GROWING".
#'
#' @return None
#'
#' @author David Drew
#' @details
#' Code translated from Pascal to R by: ChatGPT, Annemarie Eckes-Shephard
#' @keywords xylem parameters, cell identity, cambium
#' @export
#' @example
DetermineEarlyCambialCells <- function() {

  for (CellNumber in 1:TotalCells) {
    if (CellType[CellNumber] == "FUSINITIAL" || CellType[CellNumber] == "CAMBIALINITIAL") {
      # Fusiform initials
      if (RadPosition[CellNumber] > RadPosition[InitialCell[CellNumber]]) {
        # [TODO] radial position issue here
        CellType[CellNumber] <- "PHLOEMMOTHERCELL"
      } else if (CellType[CellNumber] != "CAMBIALINITIAL") {
        CellType[CellNumber] <- "XYLEMMOTHERCELL"
      }
    } else {
      # Ray initials
      if (RadPosition[CellNumber] > RadPosition[InitialCell[CellNumber]]) {
        CellType[CellNumber] <- "RAYINITIALP"
      } else if (CellType[CellNumber] != "RAYINITIALC") {
        CellType[CellNumber] <- "RAYINITIALX"
      }
    }
    MeristematicStatus[CellNumber] <- "MERISTEMATIC"
    DifferentiationStatus[CellNumber] <- "GROWING"
  }

  # release into global env:
  CellType <<- CellType
  MeristematicStatus <<- MeristematicStatus
  DifferentiationStatus <<- DifferentiationStatus
}
