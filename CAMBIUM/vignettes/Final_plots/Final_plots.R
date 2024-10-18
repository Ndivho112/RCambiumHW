# Load the necessary library
library(ggplot2)

# Define the file path for the data (change this to your file path)
data_file <- "./Output_results.csv"

# Load the data from a CSV file into a data frame
data <- read.csv(data_file)  # If your file is CSV

# Function for scatter plots between numeric variables, without titles and with units in axis labels
plot_scatter <- function(x_var, y_var, x_unit = "", y_unit = "") {
  p <- ggplot(data, aes_string(x = x_var, y = y_var)) +
    geom_point(color = "forestgreen") +  # Changed point color to forest green
    labs(x = paste(x_var, x_unit), y = paste(y_var, y_unit)) +  # Added units
    theme_minimal()
  return(p)
}

# Function for line plots (useful for time-like variables), without titles and with units in axis labels
plot_line <- function(x_var, y_var, x_unit = "", y_unit = "") {
  p <- ggplot(data, aes_string(x = x_var, y = y_var)) +
    geom_line(color = "firebrick") +  # Changed line color to firebrick red
    labs(x = paste(x_var, x_unit), y = paste(y_var, y_unit)) +  # Added units
    theme_minimal()
  return(p)
}

# Updated Units dictionary with AverageLumenVolume as m^3
units_dict <- list(
  "AuxinConc" = "(mol/m^3)",  # Updated from μmol/L to mol/m^3
  "AuxinPeak" = "(mol/m^3)",  # Updated
  "DaysSinceCZExit" = "(days)",
  "AuxConcChangeRate" = "(mol/day)",
  "DaysSinceSecThickening" = "(days)",
  "CellVolume" = "(m^3)",
  "LumenVolume" = "(m^3)",
  "AverageLumenVolume" = "(m^3)",  # Added this line
  "CellWallThickness" = "(m)",  # Updated from μm to m
  "CambiumWidth" = "(m)",  # Updated from mm to m
  "CellLength" = "(m)",  # Updated from μm to m
  "AverageCellVolume" = "(m^3)",
  "CritCellCycleDuration" = "(days)",
  "CellWallCSArea" = "(m^2)",
  "CellWallCSArea0" = "(m^2)",
  "ConductingXylemPosition" = "(unitless)",
  "CellCSArea" = "(m^2)",
  "CellRD" = "(m)",  # Updated from μm to m
  "CellTD" = "(m)"   # Updated from μm to m
)

# Pairs to plot
plot_pairs <- list(
  c("AuxinConc", "DaysSinceCZExit"),
  c("AuxConcChangeRate", "DaysSinceSecThickening"),
  c("CellVolume", "AuxinConc"),
  c("LumenVolume", "DaysSinceSecThickening"),
  c("CellWallThickness", "DaysSinceSecThickening"),
  c("CambiumWidth", "DaysSinceCZExit"),
  c("CellLength", "AuxinConc"),
  c("AverageCellVolume", "CritCellCycleDuration"),
  c("CellWallCSArea", "AuxinConc"),
  c("CellWallCSArea0", "CellWallThickness"),
  c("CellLength", "DaysSinceCZExit"),
  c("AverageLumenVolume", "DaysSinceCZExit"),  # AverageLumenVolume plot added here
  c("ConductingXylemPosition", "CambiumWidth"),
  c("LumenVolume", "CellWallThickness"),
  c("CellCSArea", "DaysSinceSecThickening")
)

# Create a directory to save plots
dir.create("plots", showWarnings = FALSE)

# Loop through each pair and create the plots
for (pair in plot_pairs) {
  x_var <- pair[1]
  y_var <- pair[2]

  # Get units from the dictionary
  x_unit <- ifelse(x_var %in% names(units_dict), units_dict[[x_var]], "")
  y_unit <- ifelse(y_var %in% names(units_dict), units_dict[[y_var]], "")

  # Generate scatter plot
  scatter_plot <- plot_scatter(x_var, y_var, x_unit, y_unit)
  print(scatter_plot)
  ggsave(filename = paste0("plots/scatter_", x_var, "_vs_", y_var, ".png"), plot = scatter_plot)

  # Generate line plot if x variable is time-related
  if (grepl("Days|Time", x_var)) {
    line_plot <- plot_line(x_var, y_var, x_unit, y_unit)
    print(line_plot)
    ggsave(filename = paste0("plots/line_", x_var, "_vs_", y_var, ".png"), plot = line_plot)
  }
}
