# ──────────────────────────────────────────────────────────────────────────────
# Cellular Automata Plot Script
# ──────────────────────────────────────────────────────────────────────────────

# ──── Section 1: Load Necessary Libraries ────────────────────────────────────
library(ggplot2)
library(RColorBrewer)
library(gridExtra) # For arranging plots
# Install and load patchwork if needed
# install.packages("patchwork")
library(patchwork)

# ──── Section 2: Read Data ───────────────────────────────────────────────────
# Assuming the file has been read into 'data_file'
data_file <- read.csv("./Output_results.csv")

# ──── Section 3: Create Output Directory ──────────────────────────────────────
# Check and create 'my_plots' directory if it doesn't exist
if (!dir.exists("my_plots")) {
  dir.create("my_plots")
}

# ──── Section 4: Define Color Palette ──────────────────────────────────────────
# Create a colorful palette for cell types
cell_type_colors <- brewer.pal(n = length(unique(data_file$CellType)), "Set3")

# ──── Section 5: Generate Plots ────────────────────────────────────────────────
# ──── Subsection 5.1: Cell Type Only ───────────────────────────────────────────
plot1 <- ggplot(data_file, aes(x = RadPosition, y = TanPosition, fill = as.factor(CellType))) +
  geom_point(shape = 21, color = "black", size = 4, stroke = 0.3) + # Points with black border
  scale_fill_manual(values = cell_type_colors) + # Apply the color palette to cell types
  labs(fill = "Cell Type") + # Remove title here
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12)
  ) +
  guides(fill = guide_legend(override.aes = list(size = 5))) # Make legend dots bigger for clarity

# Save Plot 1
ggsave("my_plots/cell_type_plot.png", plot = plot1, width = 8, height = 6)

# ──── Subsection 5.2: Cell Wall Thickness Only ─────────────────────────────────
plot2 <- ggplot(data_file, aes(x = RadPosition, y = TanPosition, size = CellWallThickness)) +
  geom_point(shape = 21, fill = "steelblue", color = "black", stroke = 0.3) + # Points in blue with size as thickness
  labs(size = "Wall Thickness") + # Remove title here
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12)
  )

# Save Plot 2
ggsave("my_plots/cell_wall_thickness_plot.png", plot = plot2, width = 8, height = 6)

# ──── Subsection 5.3: Combined Cell Type and Wall Thickness ─────────────────────
plot3 <- ggplot(data_file, aes(x = RadPosition, y = TanPosition,
                               fill = as.factor(CellType), size = CellWallThickness)) +
  geom_point(shape = 21, color = "black", stroke = 0.3) + # Points with both fill and size
  scale_fill_manual(values = cell_type_colors) + # Apply the color palette to cell types
  labs(fill = "Cell Type", size = "Wall Thickness") + # Remove title here
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12)
  ) +
  guides(fill = guide_legend(override.aes = list(size = 5))) # Make legend dots bigger for clarity

# Save Plot 3
ggsave("my_plots/combined_cell_type_wall_thickness_plot.png", plot = plot3, width = 8, height = 6)

# ──── Section 6: Raster Cell ──────────────────────────────────────────────────
# Repeat the plots using gridExtra and patchwork

# Arrange the three plots into a raster layout
combined_plot <- grid.arrange(plot1, plot2, plot3, nrow = 2)

# Save the combined raster plot
ggsave("my_plots/raster_combined_plot_gridextra.png", combined_plot, width = 12, height = 8)

# Combine the three plots using patchwork and arrange them into a grid
combined_plot_patchwork <- (plot1 | plot2) / plot3

# Save the combined raster plot
ggsave("my_plots/raster_combined_plot_patchwork.png", combined_plot_patchwork, width = 12, height = 8)

# ──────────────────────────────────────────────────────────────────────────────
# End of Script
# ──────────────────────────────────────────────────────────────────────────────
