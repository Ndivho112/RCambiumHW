#' InitialiseModelRun
#'
#' Initialises the model run by setting initial values for various parameters.
#'
#' @author David Drew, ChatGPT, Annemarie Eckes-Shephard
#'
#' @usage InitialiseModelRun()
#' @export
# Create a data frame to represent the initialization values
initialization_table <- data.frame(
  Parameter = c(
    "InitialCZWidth", "InitialTanWidth", "InitialRadWidth", "RayWidth",
    "InitialConductingXylemPosition", "WallThickStoreMass",
    "MaxThickeningCellPosition", "MaxCellRD", "AUXINSHAPEPARAMETER",
    "MAXCELLPOPULATION", "MAXFIBRELENGTH", "SAPWOODWIDTH",
    "PRIMARYWALLTHICKNESS"
  ),
  Value = c(
    11, 16, 10, 17.5,
    200, 75,
    0, 0, 0.05,
    30000, 1000, 2000,
    0.25
  ),
  Description = c(
    "Initial central zone width", "Initial tangential width", "Initial radial width", "Width of the ray",
    "Position of the initial conducting xylem", "Mass of the wall thick store",
    "Maximum thickening cell position", "Maximum cell radial dimension", "Parameter for auxin shape",
    "Maximum size of the cell population", "Maximum fibre length", "Width of the sapwood",
    "Thickness of the primary wall"
  )
)

# Print the table
print(initialization_table)
