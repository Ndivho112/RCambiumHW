# ──────────────────────────────────────────────────────────────────────────────
# PNG 2D
# ──────────────────────────────────────────────────────────────────────────────
# Load necessary libraries
library(ggplot2)
library(rayshader)

# Define the file path for the data (change this to your file path)
data_file <- "./Output_results.csv"

# Load the data from a CSV file into a data frame
data <- read.csv(data_file)  # If your file is CSV

# Create directory for custom plots
dir.create("custom_plots", showWarnings = FALSE)

# Updated Units dictionary
units_dict <- list(
  "AuxinConc" = "(mol/m^3)",
  "AuxinPeak" = "(mol/m^3)",
  "DaysSinceCZExit" = "(days)",
  "AuxConcChangeRate" = "(mol/day)",
  "DaysSinceSecThickening" = "(days)",
  "CellVolume" = "(m^3)",
  "LumenVolume" = "(m^3)",
  "AverageLumenVolume" = "(m^3)",
  "CellWallThickness" = "(m)",
  "CambiumWidth" = "(m)",
  "CellLength" = "(m)",
  "AverageCellVolume" = "(m^3)",
  "CritCellCycleDuration" = "(days)",
  "CellWallCSArea" = "(m^2)",
  "CellWallCSArea0" = "(m^2)",
  "ConductingXylemPosition" = "(unitless)",
  "CellCSArea" = "(m^2)",
  "CellRD" = "(m)",
  "CellTD" = "(m)"
)

# Pairs to plot
plot_pairs <- list(
  c("AuxinConc", "DaysSinceCZExit"),
  c("CellVolume", "AuxinConc"),
  c("LumenVolume", "DaysSinceSecThickening"),
  c("CellWallThickness", "DaysSinceSecThickening"),
  c("CambiumWidth", "DaysSinceCZExit"),
  c("CellLength", "AuxinConc"),
  c("AverageCellVolume", "CritCellCycleDuration"),
  c("CellWallCSArea", "AuxinConc"),
  c("CellLength", "DaysSinceCZExit"),
  c("AverageLumenVolume", "DaysSinceCZExit"),
  c("ConductingXylemPosition", "CambiumWidth"),
  c("LumenVolume", "CellWallThickness"),
  c("CellCSArea", "DaysSinceSecThickening")
)

# Loop through each pair and create the PNG images
for (pair in plot_pairs) {
  x_var <- pair[1]
  y_var <- pair[2]

  # Create the ggplot object
  pp <- ggplot(data, aes_string(x = x_var, y = y_var)) +
    geom_hex(bins = 20, size = 0.5, color = "black") +
    scale_fill_viridis_c(option = "C") +
    labs(x = paste(x_var, units_dict[[x_var]], sep = " "),
         y = paste(y_var, units_dict[[y_var]], sep = " "))

  # Save the static image
  image_file <- paste0("custom_plots/2Dplot_", x_var, "_vs_", y_var, ".png")

  # Use ggsave to save the plot
  ggsave(image_file, plot = pp, width = 8, height = 6, dpi = 300)  # Save the plot with high resolution
}
# ──────────────────────────────────────────────────────────────────────────────
# Video
# ──────────────────────────────────────────────────────────────────────────────
# Load necessary libraries
library(ggplot2)
library(rayshader)

# Define the file path for the data (change this to your file path)
data_file <- "./Output_results.csv"

# Load the data from a CSV file into a data frame
data <- read.csv(data_file)  # If your file is CSV

# Updated Units dictionary
units_dict <- list(
  "AuxinConc" = "(mol/m^3)",
  "AuxinPeak" = "(mol/m^3)",
  "DaysSinceCZExit" = "(days)",
  "AuxConcChangeRate" = "(mol/day)",
  "DaysSinceSecThickening" = "(days)",
  "CellVolume" = "(m^3)",
  "LumenVolume" = "(m^3)",
  "AverageLumenVolume" = "(m^3)",
  "CellWallThickness" = "(m)",
  "CambiumWidth" = "(m)",
  "CellLength" = "(m)",
  "AverageCellVolume" = "(m^3)",
  "CritCellCycleDuration" = "(days)",
  "CellWallCSArea" = "(m^2)",
  "CellWallCSArea0" = "(m^2)",
  "ConductingXylemPosition" = "(unitless)",
  "CellCSArea" = "(m^2)",
  "CellRD" = "(m)",
  "CellTD" = "(m)"
)

# Pairs to plot
plot_pairs <- list(
  c("AuxinConc", "DaysSinceCZExit"),
  c("CellVolume", "AuxinConc"),
  c("LumenVolume", "DaysSinceSecThickening"),
  c("CellWallThickness", "DaysSinceSecThickening"),
  c("CambiumWidth", "DaysSinceCZExit"),
  c("CellLength", "AuxinConc"),
  c("AverageCellVolume", "CritCellCycleDuration"),
  c("CellWallCSArea", "AuxinConc"),
  c("CellLength", "DaysSinceCZExit"),
  c("AverageLumenVolume", "DaysSinceCZExit"),
  c("ConductingXylemPosition", "CambiumWidth"),
  c("LumenVolume", "CellWallThickness"),
  c("CellCSArea", "DaysSinceSecThickening")
)

# Loop through each pair and create the MP4 videos
for (pair in plot_pairs) {
  x_var <- pair[1]
  y_var <- pair[2]

  # Create the ggplot object
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

# ──────────────────────────────────────────────────────────────────────────────
# End of Script
# ──────────────────────────────────────────────────────────────────────────────
