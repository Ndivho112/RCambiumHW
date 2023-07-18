#' CoreModuleList
#'
#' This function performs a series of calculations and operations related to cell development and growth.
#'
#' @details This function executes several subroutines to perform various tasks, including updating status information, refreshing the main form, calculating cell neighbors, calculating morphogenic fields, determining cell-specific durations, determining cells, calculating cell-specific allocation, dividing cells, calculating cell growth potential, growing cell wall, calculating cell interactions, drawing environmental graphs, and writing daily data.
#'
#' @return This function does not return any value.
#'
#' @author David Drew
#' @details Code translated from Pascal to R by: ChatGPT, Annemarie Eckes-Shephard
#' @keywords cell development, growth, calculations, interactions, graphs, data
#' @export
#' @example
CoreModuleList <- function(day,CellIdentityDeterminationMethod) {
  #StatusInformation()

  if(debug_print==TRUE){
    print(day)
  }
  #form_Main$Refresh()

  #This loop counts the number of different cell types present.
  #for(day in 2:4){# testing, OK -> no technical errors
  #  SumCountLoop() # AHES
  #}
  SumCountLoop(day) # AHES

  if(debug_print==TRUE){
    print(paste("TotalLoopSize in CoreModulelist ",TotalLoopSize))
  }
  # Calculate the cells which neighbour each cell
  CalculateNeighbours(TotalLoopSize) # AHES

  # Calculate the morphogenic fields which act over the CZ
  MorphogenicFields(day,TotalLoopSize) # AHES

  # Calculate the duration secondary thickening for day d
  # what about enlargement?
  ThickeningDuration <- c(ThickeningDuration,CalculateDevelopmentalDurations(day)) # AHES

  # Determine each cell
  DetermineCells(CellIdentityDeterminationMethod,TotalLoopSize,day) # AHES

  print(TotalCells)
  if(debug_print==TRUE){
    print(CellType[is.na(CellType)]) # AHES 15.05.2023 right now we don't get Xylem. But maybe that is ok and will come with some more
  }

  # Determine the amount of photosynthate available per cell
  CalculateCellSpecificAllocation(day) # AHES

  # Divide all "eligible" cells
  DivideCells(TotalLoopSize,day) # AHES.  [TODO]
  print(TotalCells[is.na(TotalCells)])
  # Calculate the potential growth rate of all cells
  CalculateCellGrowthPotential(day,TotalLoopSize) # AHES

  # Embark on process of secondary thickening and cell death
  GrowCellWall(day,TotalLoopSize) # AHES

  # Calculate interactions and actual growth rates of all cells
  CalculateCellInteractions(day) # AHES

  #DrawEnvironmentalGraphs()

  #WriteDailyData()
}


