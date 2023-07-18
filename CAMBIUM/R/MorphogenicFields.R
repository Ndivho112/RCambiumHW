#' Morphogenic Fields
#'
#' This function calculates the morphogenic fields.
#'
#' @param TotalLoopSize number of alive cells
#' @param day simulation day
#' @details The function iterates over the total loop size and performs calculations for each cell.
#'
#' @author David Drew
#' @details
#' Code translated from Pascal to R by: ChatGPT, Annemarie Eckes-Shephard
#' @export
#' @example MorphogenicFields()
MorphogenicFields <- function(day,TotalLoopSize) {
  for (LoopNumber in 1:TotalLoopSize) {
    CellNumber <- CellKey[LoopNumber]

    AuxinField(day,CellNumber)

    # Calculate the auxin concentration at a given distance from the cambial initial

    #SucroseField() [TODO] both these functions are empty in .pas code

    # If phloem/xylem = f{sucrose/auxin} calc here.

    #SuSyField() both these functions are empty in .pas code
  }
}
