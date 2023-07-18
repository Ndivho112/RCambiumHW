#' InitialiseModelRun
#'
#' Initialises the model run by setting initial values for various parameters.
#'
#' @author David Drew, ChatGPT, Annemarie Eckes-Shephard
#'
#' @usage InitialiseModelRun()
#' @export
InitialiseModelRun <- function() {

  #use <<- to expose variables to environment. Not good practice in R, but this
  # is in keeping with how the pascal code runs.
  # these are global parameters and variables

  # If the user does not override initial values:
  #[TODO] create switch between numbers and
  # the currently outcommented bits, so that the user could modify these
  # through inputs.
  InitialCZWidth <<- 11  # NCZMax * 0.5
  InitialTanWidth <<- 16  # MaxTDcz * 0.8
  InitialRadWidth <<- 10  # MaxRDcz * 0.8
  RayWidth <<- 17.5
  InitialConductingXylemPosition <<- 200
  WallThickStoreMass <<- 75

  # //**********************************************************************
  # //Counters and monitoring variables
  MaxThickeningCellPosition <<- 0 # AHES had to initialise this value here. not done in .pas but needed for R
  MaxCellRD <<- 0 #AHES added this here, but I think this can just be removed from the code, unless we want it as output, which it currently doesnt seem to be. We should
  # want this to be MaxCellRD per growing season to be the most informative. That needs a bit more work, because we need to build a recording system for that.
  # //**********************************************************************

 # /***Define the constants in the model
  #//It is planned that many of these will be be available as a text file for user
 # //editting as the model develops

  AUXINSHAPEPARAMETER <<- 0.05

  #seems to be a colour?
  #DARKWOOD <<- $425E85

  #//The maximum size of the cell population (and loop size)
  MAXCELLPOPULATION <<- 30000

  MAXFIBRELENGTH <<- 1000

  SAPWOODWIDTH <<- 2000

  #//The thickness of the primary wall
  PRIMARYWALLTHICKNESS <<- 0.25



}
