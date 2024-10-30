# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)  # For data manipulation
library(patchwork)  # For combining plots

# ──────────────────────────────────────────────────────────────────────────────
# Data and Plotting Setup
# ──────────────────────────────────────────────────────────────────────────────

# Define the file path for the data
data_file <- "./Output_results.csv"

# Load the data from a CSV file into a data frame
data <- read.csv(data_file)

# Function to create scatter plots with outlines and units in axis labels
plot_scatter <- function(x_var, y_var, x_unit = "", y_unit = "") {
  ggplot(data, aes_string(x = x_var, y = y_var)) +
    geom_point(color = "#FF6F61", fill = "#FFABAB", size = 3, shape = 21, stroke = 1) +
    labs(x = paste(x_var, x_unit), y = paste(y_var, y_unit)) +
    theme_minimal(base_size = 14) +  # Increased base size for better visibility
    theme(
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10),
      plot.title = element_text(size = 14)
    )
}

# Units for variables
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
  "CellRD" = "(µm)",
  "CellTD" = "(µm)"
)

# Define pairs of variables to plot
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
  c("AverageLumenVolume", "DaysSinceCZExit"),
  c("ConductingXylemPosition", "CambiumWidth"),
  c("LumenVolume", "CellWallThickness"),
  c("CellCSArea", "DaysSinceSecThickening"),
  c("cellRD", "DaysSinceSecThickening"),
  c("cellTD", "DaysSinceSecThickening")
)

# ──────────────────────────────────────────────────────────────────────────────
# Plot Generation
# ──────────────────────────────────────────────────────────────────────────────

# Create a directory for plots if it doesn't exist
dir.create("plots", showWarnings = FALSE)

# Generate scatter plots for each variable pair
for (pair in plot_pairs) {
  # Ensure x-axis is either DaysSinceCZExit, DaysSinceSecThickening, or CritCellCycleDuration if available
  if ("DaysSinceCZExit" %in% pair) {
    x_var <- "DaysSinceCZExit"
    y_var <- setdiff(pair, x_var)
  } else if ("DaysSinceSecThickening" %in% pair) {
    x_var <- "DaysSinceSecThickening"
    y_var <- setdiff(pair, x_var)
  } else if ("CritCellCycleDuration" %in% pair) {
    x_var <- "CritCellCycleDuration"
    y_var <- setdiff(pair, x_var)
  } else {
    # Default if none of the three are in the pair
    x_var <- pair[1]
    y_var <- pair[2]
  }

  # Retrieve units from the dictionary
  x_unit <- units_dict[[x_var]] %||% ""  # Use NULL coalescing
  y_unit <- units_dict[[y_var]] %||% ""  # Use NULL coalescing

  # Generate and save scatter plot
  scatter_plot <- plot_scatter(x_var, y_var, x_unit, y_unit)
  ggsave(filename = paste0("plots/scatter_", x_var, "_vs_", y_var, ".png"), plot = scatter_plot, width = 8, height = 6)  # Adjusted plot size
}

# ──────────────────────────────────────────────────────────────────────────────
# Plotting Mean CellRD and CellTD per Day
# ──────────────────────────────────────────────────────────────────────────────
# Calculate mean cellRD and cellTD per day
mean_cellRD_cellTD_per_day <- data %>%
  group_by(DaysSinceCZExit) %>%
  summarize(mean_cellRD = mean(cellRD, na.rm = TRUE),
            mean_cellTD = mean(cellTD, na.rm = TRUE)) %>%
  drop_na()

# Check for empty data frame
if (nrow(mean_cellRD_cellTD_per_day) == 0) {
  stop("No valid data to plot. Check for NA values in input data.")
}

# Create scatter plot for mean cellRD
plot_mean_cellRD <- ggplot(mean_cellRD_cellTD_per_day, aes(x = DaysSinceCZExit, y = mean_cellRD)) +
  geom_point(color = "#0072B2", size = 3) +
  geom_line(color = "#0072B2", linewidth = 1.5) +
  labs(x = "Days Since CZ Exit (days)", y = "Mean cellRD (µm)", title = "Mean cellRD Over Time") +
  theme_minimal(base_size = 14) +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 14))

# Create scatter plot for mean cellTD
plot_mean_cellTD <- ggplot(mean_cellRD_cellTD_per_day, aes(x = DaysSinceCZExit, y = mean_cellTD)) +
  geom_point(color = "#D55E00", size = 3) +
  geom_line(color = "#D55E00", linewidth = 1.5) +
  labs(x = "Days Since CZ Exit (days)", y = "Mean cellTD (µm)", title = "Mean cellTD Over Time") +
  theme_minimal(base_size = 14) +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 14))

# Combine the two plots vertically
combined_plot <- plot_mean_cellRD / plot_mean_cellTD

# Display and save the combined plot
print(combined_plot)
ggsave(filename = "plots/mean_cellRD_cellTD_combined.png", plot = combined_plot, width = 8, height = 10)  # Adjusted plot size
