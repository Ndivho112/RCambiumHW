#' Calculate Neighbours
#'
#' This function calculates the lower and upper radial as well as lower and upper tangential neighbours for each cell.
#'
#' @details The function iterates through each cell and determines its neighbours based on radial and tangential positions.
#'
#' @author David Drew
#' @details
#' Code translated from Pascal to R by: ChatGPT, Annemarie Eckes-Shephard
#'
#' @export
#'
CalculateNeighbours <- function(TotalLoopSize) {
  for (a in 1:TotalLoopSize) {
    n <- CellKey[a] # n used to be cellnumber in .pas. changed here, because it is used only within this function.
    LowerNeighbourStop <- FALSE
    UpperNeighbourStop <- FALSE
    LowerTanNeighbourStop <- FALSE
    UpperTanNeighbourStop <- FALSE

    for (b in 1:TotalLoopSize) {
      e <- CellKey[b]

      #************************************************************************
      test1 <- (RadPosition[e] - cellRD[e]/2)
      test2 <- (RadPosition[n] + cellRD[n]/2)
      test3 <- RadPosition[e] + cellRD[e]/2
      test4 <- RadPosition[n] - cellRD[n]/2
      # This allocation of variables is to deal with a strange bug which was detected
      # and could not be diagnosed
      #************************************************************************

      if (TanPosition[e] > TanPosition[n] - cellTD[n]/2 &&
          TanPosition[e] < TanPosition[n] + cellTD[n]/2) {

        #**********************************************************************
        # Calculate lower and upper radial neighbours
        if (RadPosition[e] > RadPosition[n] &&
            test1 == test2 &&
            !UpperNeighbourStop) {

          UpperRadNeighbour[n] <- e
          UpperNeighbourStop <- TRUE

        } else if (RadPosition[e] < RadPosition[n] &&
                   test3 == test4 &&
                   !LowerNeighbourStop) {

          LowerRadNeighbour[n] <- e
          LowerNeighbourStop <- TRUE

        }
      }
      #**********************************************************************

      #**********************************************************************
      # Calculate lower and upper tangential neighbours
      if (RadPosition[e] > RadPosition[n] - cellRD[n]/2 &&
          RadPosition[e] < RadPosition[n] + cellRD[n]/2) {

        if (TanPosition[e] > TanPosition[n] - cellTD[n]/2 - cellTD[e] &&
            TanPosition[e] < TanPosition[n] - cellTD[n]/2 &&
            !LowerTanNeighbourStop) {

          LowerTanNeighbour[n] <- e
          LowerTanNeighbourStop <- TRUE

        } else if (TanPosition[e] > TanPosition[n] + cellTD[n]/2 &&
                   TanPosition[e] < TanPosition[n] + cellTD[n]/2 + cellTD[e] &&
                   !UpperTanNeighbourStop) {

          UpperTanNeighbour[n] <- e
          UpperTanNeighbourStop <- TRUE

        }
        #**********************************************************************
      }

      if (CellType[n] == "CAMBIALINITIAL" || CellType[n] == "RAYINITIALC") {
        if (CellType[e] == "CAMBIALINITIAL" || CellType[e] == "RAYINITIALC") {
          if (TanPosition[e] > TanPosition[n] - cellTD[n]/2 - cellTD[e] &&
              TanPosition[e] < TanPosition[n] - cellTD[n]/2) {

            LowerTanInitialNeighbour[n] <- e

          } else if (TanPosition[e] > TanPosition[n] + cellTD[n]/2 &&
                     TanPosition[e] < TanPosition[n] + cellTD[n]/2 + cellTD[e]) {

            UpperTanInitialNeighbour[n] <- e

          }
        }
      }

    }
  }

  # Release vectors into global environment
  assign("LowerTanInitialNeighbour", LowerTanInitialNeighbour, envir = globalenv())
  assign("UpperTanInitialNeighbour", UpperTanInitialNeighbour, envir = globalenv())

}

