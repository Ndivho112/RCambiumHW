#' CalculateGrowthIncrement
#'
#' Calculates the growth increment based on the given parameters.
#'
#' @details This function calculates the growth increment for a specific cell based on various factors such as CAMBIALINITIAL, RAYINITIALC, MERISTEMATIC, and SECONDARYTHICKENING.
#'
#' @param TotalLoopSize
#' @param day The current day.
#' @param CellNumber The number of cells.
#'
#' @return The calculated growth increment.
#'
#' @author David Drew
#' @details Code translated from Pascal to R by: ChatGPT, Annemarie Eckes-Shephard
#' @keywords "growth increment", "cell growth"
#' @export
#' @examples
#' CalculateGrowthIncrement(CellType = "CAMBIALINITIAL", CellTD = 1, TanPosition = 0.5, RadPosition = 0.2, CellKey = 123, TotalLoopSize = 100, day = 1, CellNumber = 10)
CalculateGrowthIncrement <- function(TotalLoopSize, day, CellNumber) {

  # Calculate the growth increment based on the given parameters
  growthIncrement <- 0

  if (CellType == "CAMBIALINITIAL" && CellTD >= 0.1) {
    growthIncrement <- TanPosition * RadPosition * (CellKey / (TotalLoopSize * day))
  }

  if (CellType == "RAYINITIALC" && CellTD >= 0.05) {
    growthIncrement <- (1 - TanPosition) * (1 - RadPosition) * (CellKey / (TotalLoopSize * day))
  }

  if (CellType == "MERISTEMATIC" || CellType == "SECONDARYTHICKENING") {
    growthIncrement <- (TanPosition + RadPosition) * (CellKey / (TotalLoopSize * day))
  }

  return(growthIncrement)
}
