# Load necessary libraries
library(ggplot2)
library(rayshader)

# Define the file path for the data (change this to your file path)
data_file <- "./Output_results.csv"

# Load the data from a CSV file into a data frame
data <- read.csv(data_file)  # If your file is CSV

# Create directory for custom plots
dir.create("custom_plots", showWarnings = FALSE)

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

# Loop through each pair and create the 3D plots
for (pair in plot_pairs) {
  x_var <- pair[1]
  y_var <- pair[2]

  # Get units from the dictionary
  x_unit <- ifelse(x_var %in% names(units_dict), units_dict[[x_var]], "")
  y_unit <- ifelse(y_var %in% names(units_dict), units_dict[[y_var]], "")

  # Create 3D binning (optional)
  if (grepl("Auxin|Cell", x_var)) {
    # Example of 3D binning using rayshader
    pp <- ggplot(data, aes_string(x = x_var, y = y_var)) +
      geom_hex(bins = 20, size = 0.5, color = "black") +
      scale_fill_viridis_c(option = "C")

    # Plot with rayshader for 3D effect
    plot_gg(pp, width = 5, height = 4, scale = 300, raytrace = FALSE, preview = TRUE)
    plot_gg(pp, width = 5, height = 4, scale = 300, multicore = TRUE, windowsize = c(1000, 800))

    # Set up camera movements
    render_camera(fov = 70, zoom = 0.5, theta = 130, phi = 35)

    # Save the 3D plots as an MP4 file
    video_file <- paste0("custom_plots/3Dplot_", x_var, "_vs_", y_var, ".mp4")
    render_movie(filename = video_file, frames = 60, duration = 5, res = 300)
  }
}
