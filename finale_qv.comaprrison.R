setwd("/group/soranzo/thomas.dudley/confronto/locityper_database")

# Load libraries
library(ggplot2)
library(dplyr)
library(data.table)
library(cowplot)
library(grid)
library(tidyr)  

locityper_file <- fread("locityper.qv_bar.tsv")
cosigt_file <- fread("cosigt.qv_bar.tsv")

quality_levels <- c("very low: <= 17", "low: >17, <= 23", "mid: >23, <=33", "high: >33")

region_order <- locityper_file %>%
  distinct(region) %>%
  arrange(region) %>%
  pull(region)

locityper_sorted <- locityper_file %>%
  mutate(
    quality = factor(quality, levels = quality_levels),
  ) %>%
  arrange(quality)

cosigt_sorted <- cosigt_file %>%
  mutate(
    quality = factor(quality, levels = quality_levels),
  ) %>%
  arrange(quality)

locityper_sorted$type <- 1
cosigt_sorted$type <- 2

region_samples_locityper <- locityper_sorted %>%
  group_by(region) %>%
  summarise(sample_count = sum(n, na.rm = TRUE) / 2, .groups = "drop") %>%
  mutate(region = factor(region, levels = region_order)) %>%
  arrange(region)

region_samples_cosigt <- cosigt_sorted %>%
  group_by(region) %>%
  summarise(sample_count = sum(n, na.rm = TRUE) / 2, .groups = "drop") %>%
  mutate(region = factor(region, levels = region_order)) %>%
  arrange(region)

region_samples_locityper$type <- "LociT"
region_samples_cosigt$type <- "cosigt"

region_counts_row <- rbind(region_samples_locityper,region_samples_cosigt)

region_counts_row <- region_counts_row %>%
  mutate(flag_low = (sample_count) < (max(sample_count, na.rm = TRUE)))


qv_summary_sorted <- rbind(locityper_sorted,cosigt_sorted)

row_data_row <- qv_summary_sorted %>%
  mutate(
    region  = factor(region, levels = region_order),
    quality = factor(quality, levels = quality_levels),
    type    = factor(type, levels = c(1,2), labels = c("LociT","cosigt"))
  )

weights <- c(17,23,33,43)
names(weights) <- c("very low: <= 17","low: >17, <= 23","mid: >23, <=33","high: >33")

total_table <- row_data_row %>%
  mutate(
    quality=(as.character(quality)),
    percent=as.numeric(as.character(percent)),
    weight=unname(weights[quality]),
    score = weight * percent) %>%
  group_by(region,type) %>%
  summarise(total = sum(score), .groups = "drop")

difference_table <- total_table %>%
  arrange(region, desc(type)) %>%
  group_by(region) %>%
  summarise(difference = diff(total), .groups = "drop") %>%
  arrange(desc(difference)) %>%        # <<-- differenza dal maggiore al minore
  mutate(ordering = row_number())

#row_data_row <- row_data_row %>%
#left_join(difference_table %>% select(region, ordering), by="region")

#row_data_row$region <- factor(row_data_row$region, levels = difference_table$region[order(row_data_row$ordering)])

region_ordering <- difference_table %>%
  arrange(ordering) %>%
  pull(region)

row_data_row <- row_data_row %>%
  left_join(difference_table %>% select(region, ordering), by = "region") %>%
  mutate(region = factor(region, levels = region_ordering))

region_counts_row <- region_counts_row %>%
  mutate(region = factor(region, levels = region_ordering))


num_regions_qv <- length(region_order)
num_rows_qv <- 4
qv_bars_per_row <- ceiling(num_regions_qv / num_rows_qv)

qv_bar_plots <- list()
for (i in 1:num_rows_qv) {
  start_idx <- (i - 1) * qv_bars_per_row + 1
  end_idx <- min(i * qv_bars_per_row, num_regions_qv)
  if (start_idx > num_regions_qv) break
  
  regions_in_row <- region_ordering[start_idx:end_idx]
  
  row_data <- row_data_row %>%
    filter(region %in% regions_in_row) %>%
    mutate(region = factor(region, levels = region_ordering))
  
  region_counts <- region_counts_row %>%
    filter(region %in% regions_in_row) %>%
    mutate(flag_low = (sample_count) < (max(sample_count, na.rm = TRUE)))
  
  
  p <- ggplot(row_data, aes(x = type, y = percent, fill = quality)) +
    geom_col(position = "stack") +                      # stack per quality
    facet_grid(~ region, scales = "free_x", space = "free_x") +  # 2 barre per regione
    scale_fill_manual(values = c(
      "high: >33"        = "#4CAF50",
      "mid: >23, <=33"   = "#FFC107",
      "low: >17, <= 23"  = "#FF8C00",
      "very low: <= 17"  = "#F44336"
    )) +
    geom_text(
      data = region_counts,
      aes(
        x = type,
        y = 102,  # poco sopra 100
        label = floor(sample_count),
        color = flag_low
      ),
      inherit.aes = FALSE,
      angle = 90,
      hjust = -0.1,
      vjust = 0.5,
      size = 3
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
      plot.margin = margin(t = 30, r = 5, b = 30, l = 5)                  #margin(t = 15, r = 5, b = 5, l = 5)  # spazio per etichette sopra 100
    ) +
    guides(fill = guide_legend(nrow = 1, byrow = TRUE)) +
    scale_y_continuous(
      limits = c(0, 110),              # più headroom
      breaks = seq(0, 100, by = 10),
      expand = expansion(mult = c(0, 0.02))  # un filo di margine top
    ) +
    coord_cartesian(clip = "off")
  
  qv_bar_plots[[i]] <- p
  
}

# Dimensioni immagine
qv_plot_width <- max(15, qv_bars_per_row * 1.0)
qv_plot_height <- 5 * num_rows_qv

# Combina i plot (4 righe al max)
qv_combined_plot <- plot_grid(plotlist = qv_bar_plots, ncol = 1, align = 'v', axis = 'lr')


# Salva PNG
ggsave(filename = "qv_bar.png",
       plot = qv_combined_plot,
       width = qv_plot_width,
       height = qv_plot_height,
       limitsize = FALSE)




