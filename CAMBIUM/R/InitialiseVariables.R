#' InitialiseVariables
#'
#' Initialises physiological parameters and variables in the model.
#' @author David Drew
#' @details
#' code translated from Pascal to R by: ChatGPT, Annemarie Eckes-Shephard
#' @export
InitialiseVariables  <- function() {
  #use <<- to expose variables to environment. Not good practice in R, but this
  # is in keeping with how the pascal code runs.
  # these are global parameters and variables

  DrawShift  <<- 0
  RunDateTime  <<- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  AcclimationFactor  <<- 0
  CellDeadFlag  <<- FALSE
  CellNumber  <<- 0
  DrawShift  <<- 0
  DrawShiftCounter  <<- 1
  FSegmentNumber  <<- 2 #[TODO] AHES changed from 1 to 2 to address index issue between .pas and R but double-check.
  VSegmentNumber <<- 2 # S.A.

  FThickDurCount  <<- 0
  InitialLoop  <<- FALSE
  LatitudeWarningFlag  <<- FALSE
  LoopNumber  <<- 0
  MaxDeadCellPosition  <<- -200
  Maxetacr  <<- 0
  MaxGPP  <<- 0
  MaxGreenHt  <<- 0
  MaxGs  <<- 0
  MaxLeafAllocation  <<- 0
  MinLeafAllocation  <<- 0
  MaxPreDawnWP  <<- 0
  MaxSoilWater  <<- 0
  MaxStemAllocation  <<- 0
  MaxTreeHeight  <<- 0
  MaxVPD  <<- 0
  MeanWoodDensity  <<- 0
  MinTotalContributingGrowth  <<- 1000
  MinQa  <<- 0
  MissingOutputFieldError  <<- FALSE
  ModelProblem  <<- FALSE
  ModelProblemErrorMessage  <<- ""
  ParametersListing  <<- ""
  RunNumber  <<- 0
  StartingCellPopulationSize  <<- 0
  TooManyCellsFlag  <<- FALSE

  g <- 1000
  cells_init <- 330
  MaxCellPosition <<- rep(0,g)# [TODO], this could/should be made more flexible i.e. added to, not initialised with a value
  MaxInitialPosition   <<- rep(0,g)# [TODO],see above
  MaximumInitialWidth  <<- rep(0,g)# [TODO],see above
  MaxXMCPosition       <<- rep(0,g)# [TODO],see above
  MaxRadNonCrushableCellDistanceLower <<- rep(0,g)    # cells_init[TODO] here g should be "cell number" but where do I initialise it? and can use it at this stage?
  #MaxRadNonCrushableCellDistanceUpper <<- rep(10000,cells_init)  # AHES why does this one not exist?
  MaxTanNonCrushableCellDistanceLower <<- rep(0,g)    # [TODO],see above
  MaxTanNonCrushableCellDistanceUpper <<- rep(10000,g) # [TODO],see above
  DBH_CAMBIUM  <<- rep(0,g)# [TODO],see above
  TotalLoopSize <<- 1 # initialised with 0, but then in line 3077 in Main.pas, gets a +1, so maybe initialise with 1 here?
  # but dont know what trouble this may cause- just making a note, and maybe get back to this.
  CellKey <<- rep(0,g) # [TODO],see above
  CellLength <<- 250

  #translated whole lot with Pascal, not sanity-checked:
  AuxConcChangeRate <<- rep(0, g)
  AuxinConc <<- rep(0, g)
  AuxinPeak <<- rep(0, g)
  AverageCellVolume <<- rep(0, g)
  AverageLumenVolume <<- rep(0, g)
  CambialInitials <<- rep(0, g)
  CambiumWidth <<- rep(0, g)
  CambiumWidth_averaged <<- rep(0, g)
  CellCountedFlag <<- rep(FALSE, g)
  CellCrushed <<- rep(FALSE, g)
  CellDead <<- rep(FALSE, g)
  CellDetermined <<- rep(FALSE, g)
  CellKey <<- rep(0, g)
  CellLength <<- rep(250, g)
  CellType <- rep("", g)
  CellType <- rep("", g)
  CellWallCSArea <<- rep(0, g)
  CellWallCSArea0  <<- rep(0, g) #[TODO] why is this needed and recorded, can it be updated on the fly within CalculatePrimaryWallThickRate?
  CellWallThickness <<- rep(0, g)
  ConductingXylemPosition <<- rep(-200, g)
  CritPreThickeningTimeThreshold <<- rep(0, g)
  CritThickeningTimeThreshold <<- rep(0, g)
  CritWallThickTimeFlag <<- rep(FALSE, g)
  ConductiveStatus <<- rep("NONCONDUCTING", g)
  CumulativeAuxinScore <<- rep(0, g)
  CurrentAuxinCanal <<- rep(FALSE, g)
  DaysSinceCZExit <<- rep(0, g)
  DaysSinceDeath <<- rep(0, g)
  DaysSinceSecThickening <<- rep(0, g)
  DBH_CABALA <<- rep(0, g)
  DBH_CAMBIUM <<- rep(0, g)
  DeadCells <<- rep(0, g)
  DeadStemVolume <<- rep(0, g)
  DifferentiationStatus <- rep("", g)
  DistFromInitial <<- rep(0, g)
  distfromray <<- rep(0, g)
  EnergyFactor <<- rep(0, g)
  etacr <<- rep(0, g)
  etaf <<- rep(0, g)
  etas <<- rep(0, g)
  ExtractedWoodDensity <<- rep(0, g)
  fCytokinin <<- rep(0, g)
  fEnvironment <<- rep(0, g)
  FibreSegmentCount <<- rep(0, g)
  FibreSegmentRDSum <<- rep(0, g)
  FibreSegmentWASum <<- rep(0, g)
  FibreSegmentWTSum <<- rep(0, g)
  FinalGrowthSpurtFlag <<- rep(FALSE, g)
  FoliageAllocationMean <<- rep(0, g)
  FoliageAllocationMean <<- rep(0, g)
  FormationDate <<- rep("", g)
  FormationDay <<- rep(0, g)
  FSegmentActive <<- rep(TRUE, g)
  FSegmentEndDay <<- rep(0, g)
  FSegmentStartDay <<- rep(30000, g)
  fTemp <<- rep(0, g)
  FThickDurMean <<- rep(0, g)
  fWaterStress <<- rep(0, g)
  GrowingCells <<- rep(0, g)
  GrowingDays <<- rep(0, g)
  GPP <<- rep(0, g)
  GreenHt <<- rep(0, g)
  GrowingFinishedFlag <<- rep(FALSE, g)
  GrowthStopDay <<- rep(0, g)
  gs1 <<- rep(0, g)
  gs2 <<- rep(0, g)
  HourlyGrowthRate_dendrodata <<- rep(0, g)
  HourlyGrowthRate_dendrodata_averaged <<- rep(0, g)
  IncrementHours <<- rep(0, g)
  IncrementDuration_DendroData <<- rep(0, g)
  IncrementDuration_DendroData_averaged <<- rep(0, g)
  InitialiseLumenTest <<- rep(FALSE, g)
  JustDiedFlag <<- rep(FALSE, g)
  LeafGrowthModifier <<- rep(0, g)
  LeafModifierAuxin <<- rep(0, g)
  LivingCellCount <<- rep(0, g)
  LivingXylemTissueVolume <<- rep(0, g)
  LivingXylemTissueWidth <<- rep(0, g)
  LowerRadGrowthRate <<- rep(0, g)
  LowerRadGrowthRate <<- rep(0, g)
  LowerRadNeighbour <<- rep(0, g)
  LowerTanGrowthRate <<- rep(0, g)
  LowerTanInitialNeighbour <<- rep(0, g)
  LowerTanNeighbour <<- rep(0, g)
  LumenCSArea <<- rep(0, g)
  LumenVolume <<- rep(0, g)
  Form_segmentMaturingFibresCount <<- rep(0, g)
  MaturingVesselsCount <<- rep(0, g)
  MaxCellPosition <<- rep(0, g)
  MaxCellRadGrowthRate <<- rep(0, g)
  MaxDifferentiatedCellPosition <<- rep(0, g)
  MaturingFibresCount <<- rep(0, g)
  MaturingVesselsCount <<- rep(0, g)
  JustDeadFibresCount <<- rep(0, g)


  #AHES commented out environmental
  #vectors here as they are treated differently in this r version
  # i.e. initialised and filled in function ReadForcingInput.R
  #MaxTemp <<- rep(0, g)
  #MaxTempMean <<- rep(0, g)
  MaxWallThickeningRate <<- rep(0, g)
  MeanCambiumWidth <<- rep(0, g)
  MeanFibreRD <<- rep(0, g)
  MeanFibreWallArea <<- rep(0, g)
  MeanFibreWallThickness <<- rep(0, g)
  MeanFibGrowthDuration <<- rep(0, g)
  MeanVessGrowthDuration <<- rep(0, g)
  MeanVesselRD <<- rep(0, g)
  MeristematicStatus <<- rep("", g)
  MinInitialPosition <<- rep(0, g)
  MinXMCPosition <<- rep(0, g)
  MotherCellNumber <<- rep(0, g)
  NewRadialShift <<- rep(0, g)

  PhloemMotherCells <<- rep(0, g)
  PhotosynthatePerCell <<- rep(0, g)
  PNSLimitedGrowthStopped <<- rep(FALSE, g)
  PotentialVolumetricWallThickRate <<- rep(0, g)
  PotCellRadGrowthRate <<- rep(0, g)
  PotCellTanGrowthRate <<- rep(0, g)
  PreviousAuxinCanal <<- rep(FALSE, g)
  PrimaryWallThickRate <<- rep(0, g)
  SapWoodVolume <<- rep(0,g)
  RadialShift <<- rep(0, g)
  RadPosition <<- rep(0, g)
  ShapeParameter <<- rep(0, g)
  StemActivityModifier <<- rep(0, g)
  StemAllocationMean <<- rep(0, g)
  StorageCellCount <<- rep(0, g)
  TanPosition <<- rep(0, g)
  ThickeningCells <<- rep(0, g)
  ThickeningDuration <<- 0
  TimeSinceMitosis <<- rep(0, g)
  TotalCellsinStem <<- rep(0, g)
  TotalStemVolume <<- rep(0, g)
  UpperRadGrowthRate <<- rep(0, g)
  LowerRadGrowthRate <<- rep(0, g)
  UpperRadNeighbour <<- rep(0, g)
  UpperTanGrowthRate <<- rep(0, g)
  UpperTanGrowthrate <<- rep(0, g)
  UpperTanInitialNeighbour <<- rep(0, g)
  UpperTanNeighbour <<- rep(0, g)
  VesselSegmentCount <<- rep(0, g)
  VesselSegmentRDSum <<- rep(0, g)
  VF <<- rep(0, g)
  VSegmentActive <<- rep(TRUE, g)
  VSegmentEndDay <<- rep(0, g)
  VSegmentStartDay <<- rep(0, g)
  VThickDurMean   <<- rep(0, g)
  WallThickRate   <<- rep(0, g)
  RelativeRadWallExtensibility <<- rep(0, g)
  RelativeTanWallExtensibility <<- rep(0, g)
  wf <<- rep(0, g)
  XylemMotherCells <<- rep(0, g)
  MinXylemWaterPotential <<- rep(0,g) # [TODO]totaldays should be length of simulation ( days)
  NewSecondaryWallFlag <<- rep(FALSE,g)
  NightHours <<- rep(0,g) #filled in GetNightHours. but is it used anywhere? or just a record?


  NewRadialShift <<- rep(0, g)
  RadialShift    <<- rep(0, g)
  RadPosition    <<- rep(0, g)
  AdjustedFlag   <<- rep(FALSE, cells_init)
  AdjustmentCompleteFlag <<- rep(FALSE, cells_init)



  MeanFibRadGrowthRate  <<- rep(0,g)
  MeanFibRadGrowthRateCounter <<- rep(0,g)

  VThickDurCount  <<- 0
  WallThickStoreArea  <<-0
  WallThickStoreMass  <<- 0
  WallThickStoreVolume  <<- 0


  PotentialLumenVolume <<- rep(0,g)
  PotentialLumenCSArea <<- rep(0,g)
  PotentialAreaWallThickRate <<- rep(0,g)
  PotentialVolumetricWallthickRate <<- rep(0,g)
  EnergyFactor    <<- rep(0, g)
  EquilibrationRate  <<- rep(0, g)
  ExpansionHours  <<- rep(0, g)
  ShrinkageHours  <<- rep(0, g)

  StemAllocationMean<<- rep(0, g)
  StorageCellCount<<- rep(0, g)
  TempDropRate <<- rep(0, g)


}
