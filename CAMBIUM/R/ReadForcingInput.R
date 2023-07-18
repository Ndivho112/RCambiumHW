#' ReadForcingInput
#'
#' Function that reads daily forcing input into the model. [TODO] Forcings are:
#' wDate, ScenarioID, ASWtree, G, NPP, minmonthpredawn, greenht, ht, etacr, etaf, wf, etas, diam, tn, tx, qa, gs1, gs2, rain, stocking, vpdnow
#' #[TODO] this now works with the cabala spreadsheet only.
#'  the input header names have to be described and re-named to better reflect
#'  what they actually are. they are better described already in the vector
#'  names within the model.
#'  [TODO] this is not only "forcing", but also some initial values , e.g. DBH_CABALAStart
#' @author David Drew
#' @details
#' code translated from Pascal to R by: ChatGPT, Annemarie Eckes-Shephard
#' replaces ReadCabalaInput procedure in original code to make CAMBIUM more flexible and work with other daily inputs.
#' @export
#' @example
#' filename = "/Users/annemarie/Documents/1_TreeMort/2_Analysis/3_analysis_WFM/Models/CAMBIUM/Forcing_Data_Parameters/CABALAOutputs.csv"
#' ReadForcingInput(filename)
ReadForcingInput <- function(filename,StartDate =NULL,EndDate = NULL) {

  input <<- read.csv(filename)

  day <- 1

  MaxLeafAllocation <<- 0
  MinHtToCrownBase  <<- 100
  MinLeafAllocation <<- 100
  MinTempMean <<- 0
  MaxTempMean <<- 0

  PDWPAlarm <<- FALSE

  #while (!$eof && day <= 18500 && !ModelStop && !modelproblem) {
  #no longer read line by line ( i.e. day by day), but just hand over to vector.
  #[TODO] this now work with the cabala spreadsheet.
  # the input header names have to be described and re-named to better reflect
  # what they actually are. they are better described already in the vector
  # names within the model.
    logdate <<- input$wDate
    RunNumber <<- input$ScenarioID
    SoilWater <<- input$ASWtree
    GPP <<- input$G
    NPP <<- input$NPP
    PreDawnWP <<- input$minmonthpredawn
    GreenHt <<- input$greenht
    TreeHeight <<- input$ht
    #LeafAllocation <<- input$LeafAllocation
    #HtToCrownBase <<- input$HtToCrownBase MinHtToCrownBase dealt with above
    etacr <<- input$etacr
    etaf <<- input$etaf
    wf <<- input$wf
    etas <<- input$etas
    DBH_CABALA <<- input$diam
    treeheight <<- input$ht

    MinTemp <<- input$tn
    MaxTemp <<- input$tx
    Qa      <<- input$qa
    gs1     <<- input$gs1
    gs2     <<- input$gs2
    Rainfall <<- input$rain
    SPH <<- input$stocking
    VPD <<- input$vpdnow
    DBH_CABALAStart <<- DBH_CABALA[1]

# here, again, don't go through individual days, but directly find maximum value, by using max()


    #//NPPTree in kg...
    NPPTree <<- (NPP/SPH)*1000;
    NPPTree[which(SPH==0)] <- 0 # to adhere with logic of .pas code.

      MaxSoilWater <<- max(SoilWater)

      MaxGPP <<- max(GPP)

      MaxGreenHt <<- max(GreenHt)

      MaxTreeHeight <<- max(TreeHeight)

      #[TODO] Why is this update happening? the minimum is close to 0 in this dataset.
      # why not initialise with 1.1 or something? or does it need to be stand-specific?
      if (sum(GreenHt > 1) >1) { # identify any GreenHt >1
        idx <- which(GreenHt>1)
        if (sum(GreenHt[idx] < MinHtToCrownBase)>1) { #check any GreenHt[>1] < MinHtToCrownBase
          MinHtToCrownBase <<- min(GreenHt[idx]) #assign minimum to MinHtToCrownBase
        }
      }

      if (sum(etaf * NPP > MaxLeafAllocation)>1) {
        MaxLeafAllocation <<- max(etaf * NPP)
      }

      #check for any instances where etaf * NPP < MinLeafAllocation
      #and
      # etaf * NPP > 0
      if (sum(etaf * NPP < MinLeafAllocation)>1 && sum(etaf * NPP > 0)>0) {
        idx <- which(etaf * NPP > 0)
        MinLeafAllocation <<- min(etaf[idx] * NPP[idx])
      }

      if (sum(etas * NPP > MaxStemAllocation)>1) {
        idx <- which(etas * NPP > MaxStemAllocation)
        MaxStemAllocation <<- min(etas[idx] * NPP[idx])
      }


      MaxPreDawnWP <<- max(PreDawnWP)

      #MinHtToCrownBase <<- min(HtToCrownBase)

      MinQa <<- min(Qa)

      MaxQa <<- max(Qa)

      MaxVPD <<- max(VPD)

      # Calculate maxima and minima for later calculation of modifiers
      if (sum(PreDawnWP > 0)>0) {
        PDWPAlarm <<- TRUE
      }

        MaxGs <<- max((gs1 + gs2) / 2)

        MaxSoilWater <<- max(SoilWater)

        MaxVPD <<- max(VPD)

        MaxGPP <<- max(GPP)

        MaxPreDawnWP <<- max(PreDawnWP)

        MaxEtacr <<- max(etacr)

        MaxGreenHt <<- max(GreenHt)


# sanity-checks:
    if (sum(PreDawnWP > 0) > 0) {
      stop('A pre-dawn water potential value exceeding 0 was detected. Model stopped')
    } else if (sum(MaxEtacr == 0) > 0) {
      stop('The maximum allocation to coarse roots was found to be 0. Model stopped')
    } else if (sum(MaxGPP == 0) > 0) {
      stop('The maximum GPP was found to be 0. Model stopped')
    } else if (sum(MaxLeafAllocation == 0) > 0) {
      stop('The maximum allocation to foliage was found to be 0. Model stopped')
    } else if (sum(MaxSoilWater == 0) > 0) {
      stop('The maximum soil water was found to be 0. Model stopped')
    }

    if (as.Date(logdate[2],tryFormats = c("%d/%m/%Y")) - as.Date(logdate[1],tryFormats = c("%d/%m/%Y")) > 1) {
      stop('Your input dataset was not run on a daily time-step. CAMBIUM requires daily data. Model stopped')
    }

    ## [TODO] here, in the pascal code the data would be read in a second time. not sure why. omitting for the moment

  #obtain total simulation days
  totaldays <<- dim(input)[1]
}
