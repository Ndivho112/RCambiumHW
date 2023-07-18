#' BuildCellPopulation
#'
#' Build an initial population of homogeneous cells which will form the basis for the modeling population.
#'
#' @details This function builds an initial population of cells for modeling purposes. It assigns formation dates, cell types, positions, and other properties to each cell.
#' The population is based on user-specified parameters and calculations.
#'
#' @return None
#'
#' @author David Drew
#' @details
#' Code translated from Pascal to R by: ChatGPT, Annemarie Eckes-Shephard
#'
#' @keywords xylem parameters, reading parameters
#' @export
#' @example
BuildCellPopulation <- function() {

  #***************************************
  #Build an initial population of homogenous
  #cells which will form the basis for the modelling
  #population
  #***************************************

  # Variable declarations
  # //The count of the number of tangential files is calculated as f{user-specified modelling width, the maximum tangential width
  # //between a cell and a ray and the specified initial tangential cell width

  TanCount_Total <- RadialCellFiles_Position + 2
  # additional optionfor initialising TanCount_Total ,switch not implemented :
  #{(((TanWidth - ((cmb_ModellingWidth.ItemIndex+2)*RayWidth)) / InitialTanWidth) + (cmb_ModellingWidth.ItemIndex+2));}
  TanCount_FibVess <- RadialCellFiles_Position
  # additional option for initialising TanCount_FibVess, switch not implemented:
  #{round(((TanWidth - ((cmb_ModellingWidth.ItemIndex+2)*RayWidth)) / InitialTanWidth));  }

  #	//We build a population that is radially as wide as the initialised start CZ width
  TotalCells <- round(TanCount_Total * InitialCZWidth)
  FileCount <- 0

  # Main loop for building cell population

  # Could work more "object oriented"? That could speed things up.
  # I will keep this cell-loop here because then I don't have to fiddle with the auxiliary functions
  # I will initialise the FormationDate, CellType etc vectors here with length TotalCells,
  # and later add to these vectors, rather than create a vector of unknown size to start off with.
  Cells_init <-600
  FormationDate <- rep(NA,Cells_init)
  FormationDay  <- rep(NA,Cells_init)
  CellType      <- rep(NA,Cells_init)
  cellTD        <- rep(NA,Cells_init)
  cellRD        <- rep(NA,Cells_init)
  MeristematicStatus <- rep(NA,Cells_init)
  DistFromRay   <- rep(NA,Cells_init)
  #TanPosition   <- rep(NA,Cells_init)
  RadPosition   <- rep(0,Cells_init)
  MinInitialPosition  <- rep(0,totaldays)
  MaxInitialPosition  <- rep(0,totaldays)
  MinXMCPosition <-  rep(0,totaldays)
  MaxXMCPosition <-  rep(0,totaldays)
  CellLength    <- rep(NA,Cells_init)
  CellVolume    <- rep(NA,Cells_init)
  LumenVolume   <- rep(NA,Cells_init)
  LumenCSArea   <- rep(NA,Cells_init)
  CellCSArea    <- rep(NA,Cells_init)
  CellWallCSArea<- rep(NA,Cells_init)
  TimeSinceMitosis<- rep(NA,Cells_init)
  InitialCell <- rep(NA,Cells_init) #//The number of the initial cell from which the cell is "descended"
  LowerTanNeighbour <- rep(NA,Cells_init)
  UpperTanNeighbour <- rep(NA,Cells_init)
  UpperRadNeighbour <- rep(NA,Cells_init)
  LowerRadNeighbour <- rep(NA,Cells_init)
  CurrentAuxinCanal <- rep(NA,Cells_init)
  DifferentiationStatus <- rep('',Cells_init)

  FileCount = 0

  for (CellNumber in 1:TotalCells) {

    # Assign an arbitrary date that is prior to/at the start date of modelling
    FormationDate[CellNumber] <- as.Date(logdate[1],tryFormats = c("%d/%m/%Y"))
    FormationDay[CellNumber]  <- 1

    FileCount <- FileCount + 1

    if (FileCount == 1) {
      CellType[CellNumber] <- "RAYINITIAL"
      cellTD[CellNumber] <- RayWidth
      cellRD[CellNumber] <- InitialRadWidth
      MeristematicStatus[CellNumber] <- "MERISTEMATIC"
      DistFromRay[CellNumber] <- 0
      TanPosition[CellNumber] <- cellTD[CellNumber] / 2
    } else {
      if (FileCount == TanCount_Total) {
        CellType[CellNumber] <- "RAYINITIAL"
        cellTD[CellNumber] <- RayWidth
        cellRD[CellNumber] <- InitialRadWidth
        MeristematicStatus[CellNumber] <- "MERISTEMATIC"
        DistFromRay[CellNumber] <- 0
      } else {
        CellType[CellNumber] <- "FUSINITIAL"
        cellTD[CellNumber] <- InitialTanWidth
        cellRD[CellNumber] <- InitialRadWidth
        MeristematicStatus[CellNumber] <- "MERISTEMATIC"
        DistFromRay[CellNumber] <- 0
      }
    }

      CellLength[CellNumber]  <- CalculateCellLength(CellType[CellNumber], cellRD[CellNumber])
      CellVolume[CellNumber]  <- BodyVolume(CellType[CellNumber], (cellRD[CellNumber] + cellTD[CellNumber]) / 2, (cellRD[CellNumber] + cellTD[CellNumber]) / 2, CellLength[CellNumber])
      LumenVolume[CellNumber] <- BodyVolume(CellType[CellNumber], (cellRD[CellNumber] + cellTD[CellNumber]) / 2 - (PRIMARYWALLTHICKNESS * 2), (cellRD[CellNumber] + cellTD[CellNumber]) / 2 - (PRIMARYWALLTHICKNESS * 2), CellLength[CellNumber])
      LumenCSArea[CellNumber] <- BodyCSArea(CellType[CellNumber], LumenVolume[CellNumber], CellLength[CellNumber],
                                            cellRD[CellNumber], cellTD[CellNumber])
      CellCSArea[CellNumber]  <- BodyCSArea(CellType[CellNumber], CellVolume[CellNumber], CellLength[CellNumber],cellRD[CellNumber], cellTD[CellNumber])
      CellWallCSArea[CellNumber] <- CellCSArea[CellNumber] - LumenCSArea[CellNumber]



    if (StartSizeVariation == TRUE) {
      cellRD[CellNumber] <- cellRD[CellNumber] * runif(1)
    }

    if (cellRD[CellNumber] < InitialRadWidth * 0.5) {
      cellRD[CellNumber] <- InitialRadWidth * 0.5
    }

    TanPosition[CellNumber] <- cellTD[CellNumber]/2 + ((FileCount - 2) * InitialTanWidth) + RayWidth

    if (FileCount >= TanCount_Total) {
      FileCount <- 0
    }

    # The following loop approach works at the initial stage since smaller cell numbers will still be
    # "xylem-ward" of larger cell numbers
    for (i in 1:CellNumber) {
      if (TanPosition[i] == TanPosition[CellNumber]) {

        if (i < CellNumber) {

          RadPosition[CellNumber] <- RadPosition[i] + (cellRD[i]/2) + (cellRD[CellNumber]/2)
        }
      }
    }

    # So that cells will start dividing immediately:
    TimeSinceMitosis[CellNumber] <- round(MinTimeForMitosis)
   }

    #initialCell is when testing only 10,11 or 12. does this make sense?
    # what exactly is an "initial" cell?
    # makes sense if we have both phloem and xylem and division will happen in both ways.
    # with TrackBar_RadialCellFiles_Position=1, and others at default, the auxin channel is here:
    #> CurrentAuxinCanal
    #[1]   NA   NA   NA   NA   NA   NA   NA   NA   NA TRUE TRUE TRUE   NA   NA   NA   NA   NA   NA
    #[19]   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA

    for (CellNumber in 1:TotalCells) {

      for (e in 1:TotalCells) {# probably most important for plotting?

        if (TanPosition[e] == TanPosition[CellNumber]) {
          if ((RadPosition[e] + cellRD[e]) >= (InitialCZWidth/2)*(InitialRadWidth/2) &&
              (RadPosition[e] - cellRD[e]) <= (InitialCZWidth/2)*(InitialRadWidth/2)) {
            InitialCell[CellNumber] <- e
          }
        }



        if ((RadPosition[e] > RadPosition[CellNumber] - cellRD[CellNumber]/2) &&
            (RadPosition[e] < RadPosition[CellNumber] + cellRD[CellNumber]/2)) {

          # Type 1: intercepting cells at their "right" edge

          if ((TanPosition[e] > TanPosition[CellNumber] - cellTD[CellNumber]/2) &&
              (TanPosition[e] < TanPosition[CellNumber])) {
            LowerTanNeighbour[CellNumber] <- e
          } else if ((TanPosition[e] > TanPosition[CellNumber]) &&
                     (TanPosition[e] < TanPosition[CellNumber] + cellTD[CellNumber]/2)) {
            UpperTanNeighbour[CellNumber] <- e
          }
        }

        if ((TanPosition[e] > TanPosition[CellNumber] - cellTD[CellNumber]/2) &&
            (TanPosition[e] < TanPosition[CellNumber] + cellTD[CellNumber]/2)) {

          if ((RadPosition[e] > RadPosition[CellNumber]) &&
              (RadPosition[e] < RadPosition[CellNumber] + cellRD[CellNumber])) {
            UpperRadNeighbour[CellNumber] <- e
          } else if ((RadPosition[e] > RadPosition[CellNumber] - cellRD[CellNumber]) &&
                     (RadPosition[e] < RadPosition[CellNumber])) {
            LowerRadNeighbour[CellNumber] <- e
          }
        }
      }# probably most important for plotting?

      if (RadPosition[CellNumber] == RadPosition[InitialCell[CellNumber]]) {
        if (CellType[CellNumber] == "FUSINITIAL") {
          # If the radial position of the cell is equal to the radial position
          # of the cambial initial
          CurrentAuxinCanal[CellNumber] <- TRUE
          CellType[CellNumber] <- "CAMBIALINITIAL"
        } else {
          CurrentAuxinCanal[CellNumber] <- TRUE
          CellType[CellNumber] <- "RAYINITIALC"
        }

        #[TODO] at this stage I dont see why MinInitialPostition must be traced/recorded
        # in a per day vector. I am just overriding this as single value here:
        # [UPDATE] I think it has to be a vector. Min values might vary from day to day, maybe because of auxin gradients?
        # didn't check this mechanism exists in the code. But the code requires a vector further downstream and
        # I don't want to make many more changes that slow down  progress
        if (RadPosition[CellNumber] < MinInitialPosition[1]) {# at runday 0, index 1
          MinInitialPosition[1] <- RadPosition[CellNumber]
        }
        if (RadPosition[CellNumber] > MaxInitialPosition[1]) {
          MaxInitialPosition[1] <- RadPosition[CellNumber]
        }
      } else {
        if (RadPosition[CellNumber] < RadPosition[InitialCell[CellNumber]]) {
          if (RadPosition[CellNumber] < MinXMCPosition[1]) { # at runday 0, index 1
            MinXMCPosition[1] <- RadPosition[CellNumber]
          }
          if (RadPosition[CellNumber] > MaxXMCPosition[1]) {
            MaxXMCPosition[1] <- RadPosition[CellNumber]
          }
        }
      }


    }

  ###[TODO] smaity check does this need to be exported to globalenv?
    InitialLoop <- TRUE
    StartingCellPopulationSize <- 0
    # This is for the first loop in the main loop

    # release into global environment. Can be done at the end of the function:

    assign("FormationDate", FormationDate, envir = .GlobalEnv)
    assign("FormationDay", FormationDay, envir = .GlobalEnv)
    assign("CellType", CellType, envir = .GlobalEnv)
    assign("cellTD", cellTD, envir = .GlobalEnv)
    assign("cellRD", cellRD, envir = .GlobalEnv)
    assign("MeristematicStatus", MeristematicStatus, envir = .GlobalEnv)
    assign("DistFromRay", DistFromRay, envir = .GlobalEnv)
    assign("TanPosition", TanPosition, envir = .GlobalEnv)
    assign("RadPosition", RadPosition, envir = .GlobalEnv)

    assign("CellLength", CellLength, envir = .GlobalEnv)
    assign("CellVolume", CellVolume, envir = .GlobalEnv)
    assign("LumenVolume", LumenVolume, envir = .GlobalEnv)
    assign("LumenCSArea", LumenCSArea, envir = .GlobalEnv)
    assign("CellCSArea", CellCSArea, envir = .GlobalEnv)
    assign("CellWallCSArea", CellWallCSArea, envir = .GlobalEnv)
    assign("TimeSinceMitosis", TimeSinceMitosis, envir = .GlobalEnv)
    assign("InitialCell", InitialCell, envir = .GlobalEnv)
    assign("LowerTanNeighbour", LowerTanNeighbour, envir = .GlobalEnv)
    assign("UpperTanNeighbour", UpperTanNeighbour, envir = .GlobalEnv)
    assign("UpperRadNeighbour", UpperRadNeighbour, envir = .GlobalEnv)
    assign("LowerRadNeighbour", LowerRadNeighbour, envir = .GlobalEnv)
    assign("CurrentAuxinCanal", CurrentAuxinCanal, envir = .GlobalEnv)

    assign("MinInitialPosition", MinInitialPosition, envir = .GlobalEnv)
    assign("MaxInitialPosition", MaxInitialPosition, envir = .GlobalEnv)
    assign("MinXMCPosition", MinXMCPosition, envir = .GlobalEnv)
    assign("MaxXMCPosition", MaxXMCPosition, envir = .GlobalEnv)
    assign("DifferentiationStatus", DifferentiationStatus, envir = .GlobalEnv)
    TotalCells <<- TotalCells

}#end
