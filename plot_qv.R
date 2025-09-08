#!/usr/bin/env Rscript
setwd("/group/soranzo/thomas.dudley/repo_locityper/locityper_326/qv_plot")
# ---------------------------
# Plot stacked bars QV by region (Y fisso 0–100)
# ---------------------------
# Load libraries
library(ggplot2)
library(dplyr)
library(data.table)
library(cowplot)
library(grid)   # per unit()
library(tidyr)  # per pivot_wider

input_file <- "plotting_table.tsv"
output <- "qv.png"

# Read input data
qv_summary <- fread(input_file)

# Define quality levels (devono combaciare con i valori in 'quality')
quality_levels <- c("very low: <= 17", "low: >17, <= 23", "mid: >23, <=33", "high: >33")

# -------- Ordinamento regioni ALFABETICO --------
# Ordine alfabetico case-insensitive, mantenendo l'ortografia originale
#region_order <- qv_summary %>%
#  distinct(region) %>%
#  arrange(region) %>%
#  pull(region)

# ------ ORDINAMENTO PER QV --------

region_order <- qv_summary %>%
  filter(quality == "high: >33") %>%
  group_by(region) %>%
  summarize(high_pct = sum(percent)) %>%
  arrange(desc(high_pct))

region_order <- region_order$region




# -------- Pulizia e ordinamento dati --------
qv_summary_sorted <- qv_summary %>%
  mutate(
    quality = factor(quality, levels = quality_levels),
    region  = factor(region, levels = region_order)
  ) %>%
  arrange(region, quality)

# Calcola conteggi campioni per regione (per etichette)
region_samples <- qv_summary %>%
  group_by(region) %>%
  summarise(sample_count = sum(n, na.rm = TRUE), .groups = "drop") %>%
  mutate(region = factor(region, levels = region_order)) %>%
  arrange(region)

# Max 4 righe
num_regions_qv <- length(region_order)
num_rows_qv <- 2
qv_bars_per_row <- ceiling(num_regions_qv / num_rows_qv)

# Crea i plot per riga
qv_bar_plots <- list()

for (i in 1:num_rows_qv) {
  start_idx <- (i - 1) * qv_bars_per_row + 1
  end_idx <- min(i * qv_bars_per_row, num_regions_qv)
  if (start_idx > num_regions_qv) break

  regions_in_row <- region_order[start_idx:end_idx]

  row_data <- qv_summary_sorted %>%
    filter(region %in% regions_in_row) %>%
    mutate(region = factor(region, levels = regions_in_row))

  region_counts <- region_samples %>%
    filter(region %in% regions_in_row) %>%
    mutate(flag_low = (sample_count / 2) < (max(sample_count, na.rm = TRUE) / 2))

  # Plot con Y fissata a 0–100; etichette posizionate a 102 e clipping disattivato
  p <- ggplot(row_data, aes(x = region, y = percent, fill = quality)) +
    geom_bar(stat = "identity", position = "stack") +
    scale_fill_manual(values = c(
      "high: >33"        = "#4CAF50",
      "mid: >23, <=33"   = "#FFC107",
      "low: >17, <= 23"  = "#FF8C00",
      "very low: <= 17"  = "#F44336"
    )) +
    # Etichette numeriche: mostra sample_count/2
    geom_text(
      data = region_counts,
      aes(
        x = region,
        y = 102,  # poco sopra 100
        label = floor(sample_count / 2),
        color = flag_low
      ),
      inherit.aes = FALSE,
      angle = 90,
      hjust = -0.1,
      vjust = 0.5,
      size = 5.8
    ) +
    scale_color_manual(values = c("FALSE" = "black", "TRUE" = "red"), guide = "none") +
    labs(x = if (i == num_rows_qv) "region" else "", y = "pct of QVs") +
    theme_bw() +
    theme(
      axis.title = element_text(size = 20),
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
      axis.text = element_text(size = 18),
      legend.position = "top",
      legend.direction = "horizontal",
      legend.box = "horizontal",
      legend.spacing.x = unit(2, "cm"),
      legend.text = element_text(size = 14, margin = margin(r = 25)),
      legend.title = element_text(size = 18, margin = margin(r = 25)),
      legend.key.size = unit(0.8, "line"),
      legend.box.margin = margin(b = 20)
      #plot.margin = margin(t = 30, r = 5, b = 30, l = 5)                  #margin(t = 15, r = 5, b = 5, l = 5)  # spazio per etichette sopra 100
    ) +
    guides(fill = guide_legend(nrow = 1, byrow = TRUE)) +
    scale_y_continuous(
      #limits = c(0, 110),                      # <<< fisso a 100
      breaks = seq(0, 100, by = 10),
      expand = expansion(mult = c(0, 0))      # niente padding extra
    ) +
    coord_cartesian(clip = "off")              # mostra le etichette a y = 102

  qv_bar_plots[[i]] <- p
}

# Dimensioni immagine
qv_plot_width <- max(15, qv_bars_per_row * 0.5)
qv_plot_height <- 5 * num_rows_qv

# Combina i plot (4 righe al max)
qv_combined_plot <- plot_grid(plotlist = qv_bar_plots, ncol = 1, align = 'v', axis = 'lr')

# Salva PNG
ggsave(paste0(output),
       plot = qv_combined_plot,
       width = qv_plot_width,
       height = qv_plot_height,
       limitsize = FALSE)

