#' @title run_CAMBIUM
#'
#' @author David Drew
#' @details
#' code translated from Pascal to R by: ChatGPT, Annemarie Eckes-Shephard. This was the former MainLoop procedure in the Pascal code.
#' @param filename_forcing string with (path/to/)forcing.csv. Must comply with current CABALA output naming conventions
#' [TODO]names must be made more universally understandable + metadata /description + units provided.
#' @param filename_param string with (path/to/)parameters.csv. Names must comply with current names in parameters.csv
#'
#' @export
#' @description Main Model Loop of CAMBIUM.This function runs the main loop of the CAMBIUM model, which iterates through time and updates the state of the system at each time step. The function returns the final state of the system at the end of the simulation.
#' @example
#' runCambium()
#' filename_forcing = "/Users/annemarie/Documents/1_TreeMort/2_Analysis/3_analysis_WFM/Models/CAMBIUM/Forcing_Data_Parameters/CABALAOutputs.csv"
#' filename_param = "/Users/annemarie/Documents/1_TreeMort/2_Analysis/3_analysis_WFM/Models/CAMBIUM/Forcing_Data_Parameters/XylemParameters.csv"
run_CAMBIUM <- function(filename_forcing,filename_param,run_settings) {


  RunStart <- Sys.time() # to measure runtime

  #run_settings <- t(run_settings)
  #run_settings[2,c(1,2,5,10:12,16,17,19,20)] <- as.logical(run_settings[2,c(1,2,5,10:12,16,17,19,20)])
  #run_settings[2,c(3,4,6:9,14,15,18)] <- as.numeric(run_settings[2,c(3,4,6:9,14,15,18)])
  #run_settings[2,13] <- as.character(run_settings[2,13])
  #must be "free-standing" run settings
  # InstructionName <- as.list(run_settings$Instruction_Name)
  #names(InstructionName) <- run_settings$Instruction_Name
  #for(i in seq_along(run_settings$Instruction_Name)) {
  #  assign(run_settings$Instruction_Name[i],run_settings$Instruction_Value[i],envir = globalenv())
  #}
  # the above doesnt want to work quite yet, so for testing I am going to be using this still:
  MyErrorString <- ""
  FieldChecks <- FALSE
  RunDuration <- 0
  WaterStressShape <<- TRUE # Menu_PhysOptions_WaterStressShape
  AuxSens <<- TRUE # Menu_PhysOptions_AuxSens
  CumTempDuration <<- 15
  RadialCellFiles_Position <<- 4
  StartSizeVariation <<- TRUE
  SegmentLengthFibres <<- 200
  SegmentLengthVessels <<- 400
  AuxinConcChangeMethod <<- 0
  ThickeningDurationControl <<- 0
  SecThickDur <<- FALSE
  #[TODO] the two above do they need to work together somehow?
  MinSizeSecThickSet <<- FALSE
  GrowthPhaseMinSize <<- FALSE
  CellIdentityDeterminationMethod <<- "Canalisation"  # otpions: "Barlow" (Barlow and Luck) or "Canalisation" (Sachs, 1981 etc)
  TempAcclimation <<- 20
  SiteLat <<- -33
  ThickRateEnvironment <<- TRUE
  InterCellAdjustment <<- TRUE
  MaxIntCellAdjust <<-     8
  XMCCrush <<- TRUE
  VesselForm <<- TRUE
  TESTING <<- FALSE
  #tryCatch({
  #form_Main$refresh()

  print("Status: Initialising variables...")
  #form_Main$refresh()
  InitialiseVariables()  # AHES

  print( "Status: Reading CABALA output data...")
  #form_Main$refresh()
  ReadForcingInput(filename_forcing) # replaces ReadCABALAOutput() in .pas # AHES

  print( "Status: Reading xylem parameters...")
  #form_Main$refresh()
  ReadParameters(filename_param)  # AHES

  #if (ModelProblem != TRUE && ModelStop != TRUE) {
  print(  "Initialising model..." )
  # form_Main$refresh()

  CalculateModifiers()  # AHES

  #WritetoRunLog()

  #		//Initialise start variables for a realistic simulation start
  InitialiseModelRun()

  day <- 2 # have to start with 2, because R works with index 1, vs pascal with index 0

  print(  "Building the initial cell population...")
  #form_Main$refresh()
  BuildCellPopulation()  # AHES


  if(debug_print==TRUE){
    print(CellType[is.na(CellType)])# Testing
  }

  print(  "Determining initial cell population...")
  #form_Main$refresh()
  DetermineEarlyCambialCells()  # AHES

  if(debug_print==TRUE){
    print(CellType[is.na(CellType)])# Testing
  }

  totaldays=665
  for (day in 2:totaldays) {
    #  if (ModelStop != TRUE && ModelProblem != TRUE) {
    print(paste("Modelling cell development on", logdate[day],
                "for CABALA scenario", RunNumber[day]))

    CoreModuleList(day,CellIdentityDeterminationMethod)  # AHES (biological operations happen here)
  }
  #   }
  # }
  # }, error = function(E) {
  #   form_Main$ProgressBar_Main$position <- 0
  #   MyErrorString <- as.character(E$message)
  #   message(paste("The model run could not be successfully completed due to the following problem:",
  #                MyErrorString), type = "error")
  #  ModelProblem <- TRUE
  #})

  if (ModelProblem == TRUE && ModelProblemErrorMessage != "") {
    if (toomanycellsflag != TRUE)
      message(ModelProblemErrorMessage, type = "error")
    else
      message(ModelProblemErrorMessage, type = "message")
  }

  RunDuration <- difftime(Sys.time(), RunStart)

  print(paste("CAMBIUM Run duration was :", RunDuration))
}
