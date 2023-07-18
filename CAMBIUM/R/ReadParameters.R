#' ReadParameters
#'
#' Read in all xylem parameters from the xylem parameters table.
#'
#' @details This function reads in various xylem parameters from a table and assigns them to appropriate variables.
#' [TODO] can turn print statements to warning messages where appropriate
#' @return This function "releases" the parameters into the R environment so it can be picked up by other functions.
#' This is a legacy to make the pascal code translation easier.
#'
#' @author David Drew
#' @details
#' code translated from Pascal to R by: ChatGPT, Annemarie Eckes-Shephard
#' @keywords xylem parameters, reading parameters
#' @export
#' @example
#' filename <- "/Users/annemarie/Documents/1_TreeMort/2_Analysis/3_analysis_WFM/Models/CAMBIUM/Forcing_Data_Parameters/XylemParameters.csv"
#' ReadParameters(filename)
#' filename <- /Users/annemarie/Documents/1_TreeMort/2_Analysis/3_analysis_WFM/Models/CAMBIUM/Forcing_Data_Parameters/XylemParameters_testing_error_messages.csv
ReadParameters <- function(filename){

  params <- read.csv(filename)

  #must be "free-standing" parameters
  ParameterName <- as.list(params$ParameterValue)
  names(ParameterName) <- params$ParameterName
  for(i in seq_along(params$ParameterName)) {
    assign(params$ParameterName[i],params$ParameterValue[i],envir = globalenv())
  }


  # Parameters error checking
  if (RadialGrowthContribution < 1) {
    print("Warning: The radial growth contribution parameter should be in %, not a proportion between 0 and 1")
  }


  if (min(PreDawnWP) < WPMin) {
    WPMin <- min(PreDawnWP) - 0.5
    print("Pre-dawn water potential less than your specified minimum water potential has been found. Value changed")
  }

  if (MaxAuxConc < MinAuxConcDivision) {
    MaxAuxConc <- MinAuxConcDivision
    print("The maximum auxin concentration must be greater than the minimum auxin concentration for division. Value changed")
  }

  if (MaxDaysBeforeThickeningFib < 1) {
    MaxDaysBeforeThickeningFib <- 1
    print("The cells must be allowed at least one day for growth. Value changed to 1")
  }

  if (MaxDaysSecThickening < 1) {
    MaxDaysSecThickening <- 1
    print("The cells must be allowed at least one day for secondary thickening. Value changed to 1")
  }

  if (MaxDaysBeforeThickeningVess != MaxDaysBeforeThickeningFib) {
    MaxDaysBeforeThickeningVess <- MaxDaysBeforeThickeningFib
    print("Note: Until a modification is complete, the model does not allow differing max. expansion days for fibres and vessels.")
  }

  if (MaxLeafAllocation < MaxDailyFoliageAllocation/2) {
    print(paste('Warning: The maximum daily allocation to foliage in the CABALA run you have used is less than half than the specified parameter maximum daily foliage allocation:',
                MaxLeafAllocation,'<',MaxDailyFoliageAllocation,"/2"))
  }

  if (MaxLeafAllocation > MaxDailyFoliageAllocation) {
    MaxDailyFoliageAllocation <- MaxLeafAllocation
    print('The maximum daily allocation to foliage in the CABALA run you have used is greater than the specified parameter maximum daily foliage allocation. Value changed.')
  }

  if (MinAuxConcDivision < 0) {
    print('The minimum relative auxin concentration for cell division must be greater than 0. Value set to 0.1')
    MinAuxConcDivision <- 0.1
  }

  if (MinWallThickRateStorageUse > MaxWallThickRate) {
    print(paste('The minimum wall thickening rate before stored carbohydrates are used must be less than the maximum wall thickening rate. Value set to', MaxWallThickRate))
    MinWallThickRateStorageUse <- MaxWallThickRate
  }

  if (RadTanExtRatio > 3) {
    print('The parameter controlling rad/tan extensibility ratio cannot exceed 3. It has been set to 3')
    RadTanExtRatio <- 3
  }

  if (WPMax > 0) {
    print('The water potential maximum for the tree cannot be a positive number. Value changed to -0.3')
    WPMax <- -0.3
  }

  if (PotentialCondRatio > 10) {
    print('The parameter controlling relative vessel:fibre conductivity ratio cannot exceed 10. It has been set to 10')
    PotentialCondRatio <- 10
  }

  if (WallExtensRatio > 10) {
    print('The parameter controlling relative vessel:fibre extensibility ratio cannot exceed 10. It has been set to 10')
    WallExtensRatio <- 10
  }

  if (MaxRDcz <= MinDDivision) {
    MaxRDcz <- MinDDivision + 1
    print('The model will not work properly if the minimum diameter for cell division is greater than or equal to the maximum allowable size of initials. This has been modified accordingly')
  }

  if ((MaxRDcz - MinDDivision)/MaxRDcz > 0.5) {
    MinDDivision <- MaxRDcz * 0.5
    print('The model has limited the difference between the maximum RD of cambial initials and the minimum RD for division')
  }

  if (MinRDSecThick > MaxRDcz) {
    MinRDSecThick <- MaxRDcz
    print('The model has limited the minimum radial diameter required for secondary thickening to the maximum RD allowable in the CZ')
  }

  if (MaxTDcz > 20) {
    MaxTDcz <- 20
    print('The tangential diameter of cells in the cambial zone is currently limited to 20 um. Value changed')
  }

  if (MinAuxConcDivision < 0.005) {
    MinAuxConcDivision <- 0.005
    print('The model does not allow a minimum relative auxin concentration permitted for division below 0.005. Value changed')
  }
  #//****************************************************************************
  # Check that sensitivities are all normalised: 0 to 1
  if (AuxinSenstoLeafGrowth > 1 || AuxinSenstoLeafGrowth < 0) {
    memo_warnings.Visible <- TRUE
    AuxinSenstoLeafGrowth <- 0.5
    print("The auxin sensitivity to leaf growth must be between 0 and 1: value has been set to 0.5")
  }

  if (AuxinWallLoosenessSensitivity > 1 || AuxinWallLoosenessSensitivity < 0) {
    memo_warnings.Visible <- TRUE
    AuxinWallLoosenessSensitivity <- 0.5
    print("The wall looseness sensitivity to auxin must be between 0 and 1: value has been set to 0.5")
  }

  if (WallThickDurationSensEnvironment > 1 || WallThickDurationSensEnvironment < 0) {
    memo_warnings.Visible <- TRUE
    WallThickDurationSensEnvironment <- 0.5
    print("The sensitivity of wall thickening duration to environment must be between 0 and 1: value has been set to 0.5")
  }

  if (WaterDeficitSensitivityGrowth > 1 || WaterDeficitSensitivityGrowth < 0) {
    memo_warnings.Visible <- TRUE
    WaterDeficitSensitivityGrowth <- 0.5
    print("The sensitivity of cell growth rate to low water potential must be between 0 and 1: value has been set to 0.5")
  }

  if (CellCycleSenstoAuxin > 1 || CellCycleSenstoAuxin < 0) {
    memo_warnings.Visible <- TRUE
    CellCycleSenstoAuxin <- 0.5
    print("The sensitivity of the cell cycle duration to auxin must be between 0 and 1: value has been set to 0.5")
  }

}


