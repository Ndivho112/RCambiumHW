#' CalculateCellGrowthPotential
#'
#' Calculates the cell growth potential based on the given climate forcings and parameters
#'
#' @details This function calculates the cell growth potential by considering factors such as temperature, water potential, and photosynthate availability.
#'
#' @return
#'
#' @author David Drew
#' @details Code translated from Pascal to R by: ChatGPT, Annemarie Eckes-Shephard
#' @keywords growth increment, cell growth,growth potential,
#' @export
CalculateCellGrowthPotential <- function(day, TotalLoopSize) {
  # If the mean daily minimum temperature is greater than the minimum temperature for cambial growth,
  # increase the acclimation factor by 0.01. Otherwise, decrease it by 0.01.
  if (MinTempMean[day] > MinTempCambialGrowth) {
    AcclimationFactor <- AcclimationFactor + 0.01
  } else {
    AcclimationFactor <- AcclimationFactor - 0.01
  }

  # Ensure that the acclimation factor is not less than 1.
  if (AcclimationFactor < 1) {
    AcclimationFactor <- 1
  }

  # Limit the acclimation factor to the value specified in the edit_TempAcclimation field.
  if (AcclimationFactor >= as.numeric(TempAcclimation)) {
    AcclimationFactor <- as.numeric(TempAcclimation)
  }

   #//****************************************************************************
   # //Calculate the hourly values interpolated between daily minima and maxima

  # Calculate the potential xylem water potential based on soil water and other factors.
  if (MaxSoilWater > SoilWater[day]) {
    if (MaxWaterPotentialDrop * ((VPD[day] / MaxVPD) * (1 - SoilWater[day] / MaxSoilWater))^0.25 > MinWaterPotentialDrop) {
      MinXylemWaterPotential[day] <- PreDawnWP[day] - MaxWaterPotentialDrop * ((VPD[day] / MaxVPD) * (1 - SoilWater[day] / MaxSoilWater))^0.25
    } else {
      MinXylemWaterPotential[day] <- PreDawnWP[day] - MinWaterPotentialDrop
    }
  } else {
    MinXylemWaterPotential[day] <- PreDawnWP[day] - MinWaterPotentialDrop
  }

  # Calculate the number of hours of expansion based on night hours and other factors.
  #	//The number of hours of expansion is calculated from the proportion of the day for which the tree is in darkness or when there is daytime rain.
  # a bit clumsy, as day still has to be passed into the function, but that way at least not the vectors have to be passed into the function.
  ExpansionHours[day] <- GetNightHours(logdate[day], as.numeric(SiteLat), Rainfall[day], MinRainForDaytimeExpansion, Qa[day], Qa[day-1],day)


  # We assume expansion hours actually begin about 3 hours before sunset.
  if (ExpansionHours[day] <= (24 - ExpansionHoursBeforeSunset)) {
    ExpansionHours[day] <- ExpansionHours[day] + ExpansionHoursBeforeSunset
  }


  ShrinkageHours[day] <- 24 - ExpansionHours[day]

  EquilibrationRate[day] <- (PreDawnWP[day + 1] - MinXylemWaterPotential[day]) / ExpansionHours[day]

  TempDropRate[day] <- (MaxTemp[day] - MinTemp[day + 1]) / ExpansionHours[day]
  #//*****************************************************************************

  EnergyFactor[day] <- Qa[day] / MaxQa

  if (TreeHeight[day] > 0) {
    MaximumInitialWidth[day] <- MaximumInitialWidth[day] - MaximumInitialWidth[day] * (1 / TreeHeight[day])
  }

  if (MaximumInitialWidth[day] > MaxRDcz) {
    MaximumInitialWidth[day] <- MaxRDcz
  } else if (MaximumInitialWidth[day] < MaxRDcz * 0.75) {
    MaximumInitialWidth[day] <- MaxRDcz * 0.75
  }


  if ( (PhotosynthatePerCell[day] > MinAllocatedPNSForGrowth / 1000000) ||
    (WallThickStoreMass > MaxStorageTotal * 0.25) ){#||
    #(DendroDate == LogDate[day + 1]) ) {. #AHES commented out, because dendrometers not used yet.

    #AHES commented out dendrometer observations lines
     if (((MaxTempMean[day] + MinTempMean[day]) / 2 > MinTempCambialGrowth) &&
          ((MaxTempMean[day] + MinTempMean[day]) / 2 < MaxTempCambialGrowth) ) {#||
       # (DendroDate == LogDate[day + 1]) ) { #AHES commented out, because dendrometers not used yet.

          # If we read in dendrometer data and there is data for "logdate," we always enter the loop

         # if (DendroDate == logdate[day + 1]) {
        #    HourlyGrowthRate_dendrodata_averaged[day] <- HourlyGrowthRate_dendrodata_averaged[day] * RadialGrowthContribution
            # We modify the Hourly growth rate to reflect the contribution of radial cell growth to total stem expansion...
        #  }

          NewDayMarker <- TRUE

          for (LoopNumber in 1:TotalLoopSize) {

            n <- CellKey[LoopNumber] # changed  what was CellNumber in .pas to n as  this is. internal to this function

            if ((DifferentiationStatus[n] == "GROWING") ||
              (MeristematicStatus[n] == "MERISTEMATIC") ) {

                if ((CellDead[n] != TRUE) &&
                  (CellCrushed[n] != TRUE) ) {

                    if ( ((MeristematicStatus[n] == "MERISTEMATIC") &&
                        (cellRD[n] < MaximumInitialWidth[day])  )||
                        (MeristematicStatus[n] == "DIFFERENTIATING") ) {
                        # To ensure the cambial cells do not grow too large

                        if ( ( InterCellAdjustment == TRUE &&
                            RadPosition[InitialCell[n]] - MinInitialPosition[day] < MaxIntCellAdjust ) ||
                            (MeristematicStatus[n] != "MERISTEMATIC") ||
                            (InterCellAdjustment != TRUE) ) {

                            # If the distance between initials has become too wide, we want to slow down the furthest one by having it not grow...
                            # In effect, we are restricting the growth of cells based on the idea that too much tension will develop as walls try to accommodate
                            # varying growth in adjoining files of cells

                          #  if (DendroDate == LogDate[day + 1]) {
                          #    GrowCells_RelativeMethod_DendrometerData()
                          #    Form_DashBoard.Image2.Visible <- TRUE
                          #    Form_DashBoard.Image1.Visible <- FALSE
                          #  } else {
                          #    GrowCells_RelativeMethod_ModelledData()
                          #    Form_DashBoard.Image2.Visible <- FALSE
                          #    Form_DashBoard.Image1.Visible <- TRUE
                          #  }

                            if (MeristematicStatus[n] == "MERISTEMATIC") {
                             # // We limit the RD of meristematic cells
                              if (cellRD[n] + PotCellRadGrowthRate[n] > MaximumInitialWidth[day]) {
                                PotCellRadGrowthRate[n]  <- MaximumInitialWidth[day] - cellRD[n]
                              }
                            }
                          } else { #InterCellAdjustment
                            PotCellRadGrowthRate[n] <- 0
                            PotCellTanGrowthRate[n] <- 0
                          }
                        } else {#meristematicstatus
                          PotCellRadGrowthRate[n] <- 0
                          PotCellTanGrowthRate[n] <- 0
                        }
                    } else {#celldead
                      PotCellRadGrowthRate[n] <- 0
                      PotCellTanGrowthRate[n] <- 0
                    }
              } else {#DifferentiationStatus
                PotCellRadGrowthRate[n] <- 0
                PotCellTanGrowthRate[n] <- 0
              }

            #calculates actual growth rates and releases those into the global enviornment.
            CalculateInitialActualGrowthRates(n,PotCellTanGrowthRate[n],PotCellRadGrowthRate[n]);

              if (PotCellRadGrowthRate[n] > MaxCellRadGrowthRate[day]) {
                MaxCellRadGrowthRate[day] <- PotCellRadGrowthRate[n]
              }
            }
        } else {
          warning(paste0('Temperature was unsuitable for cell growth on ', logdate[day]))
        }
      } else {
        warning(paste0('No photosynthate was available to maintain cell wall thickness and cell growth on ', logdate[day]))
      }



  # release objects to globalenv:

  AcclimationFactor <<-  AcclimationFactor
  MinXylemWaterPotential <<- MinXylemWaterPotential
  ExpansionHours <<- ExpansionHours
  ShrinkageHours <<- ShrinkageHours
  EquilibrationRate <<- EquilibrationRate
  TempDropRate <<- TempDropRate
  EnergyFactor <<- EnergyFactor
  MaximumInitialWidth <<- MaximumInitialWidth
  MaxCellRadGrowthRate <<- MaxCellRadGrowthRate


 }


