# ──────────────────────────────────────────────────────────────────────────────
# Full Script for Custom 2D Plots and 3D Videos
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
  "cellRD" = "(µm)",
  "cellTD" = "(µm)"
)

# List of variables to prioritize on the x-axis
preferred_x_vars <- c("DaysSinceCZExit", "DaysSinceSecThickening", "CritCellCycleDuration")

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

# Adjust pairs to prioritize preferred variables on the x-axis
plot_pairs <- lapply(plot_pairs, function(pair) {
  # Check if either element in the pair is one of the preferred x-axis variables
  if (pair[1] %in% preferred_x_vars) {
    return(pair)  # Keep as is if the x_var is already preferred
  } else if (pair[2] %in% preferred_x_vars) {
    return(rev(pair))  # Swap to place preferred variable on the x-axis
  } else {
    return(pair)  # Leave unchanged if neither is preferred
  }
})

# ──────────────────────────────────────────────────────────────────────────────
# PNG 2D Plots
# ──────────────────────────────────────────────────────────────────────────────

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
  ggsave(image_file, plot = pp, width = 8, height = 6, dpi = 300)  # Save plot with high resolution
}

# ──────────────────────────────────────────────────────────────────────────────
# MP4 3D Video Plots
# ──────────────────────────────────────────────────────────────────────────────

for (pair in plot_pairs) {
  x_var <- pair[1]
  y_var <- pair[2]

  # Create the ggplot object
  pp <- ggplot(data, aes_string(x = x_var, y = y_var)) +
    geom_hex(bins = 20, size = 0.5, color = "black") +
    scale_fill_viridis_c(option = "C") +
    labs(x = paste(x_var, units_dict[[x_var]], sep = " "),
         y = paste(y_var, units_dict[[y_var]], sep = " "))

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
