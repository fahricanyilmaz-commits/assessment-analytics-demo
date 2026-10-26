# Draw 15 new figures from this project's fictional CSVs only.

library(ggplot2)

items <- read.csv("data/item_register.csv", stringsAsFactors = FALSE)
profiles <- read.csv("data/option_profiles.csv", stringsAsFactors = FALSE)
curves <- read.csv("data/information_curves.csv", stringsAsFactors = FALSE)
residual_pairs <- read.csv("data/residual_pairs.csv", stringsAsFactors = FALSE)

figure_dir <- file.path("www", "figures")
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
theme_demo <- theme_minimal(base_size = 13) +
  theme(panel.grid.minor = element_blank(), plot.title = element_text(face = "bold"),
        legend.position = "bottom")
manifest_rows <- list()

save_figure <- function(plot, title, family) {
  number <- length(manifest_rows) + 1L
  path <- file.path(figure_dir, sprintf("figure_%02d.png", number))
  ggsave(path, plot = plot + labs(title = title) + theme_demo,
         width = 8, height = 5, units = "in", dpi = 120,
         bg = "white", device = "png")
  manifest_rows[[number]] <<- data.frame(
    figure_id = sprintf("F%02d", number), title = title,
    family = family, file = path, stringsAsFactors = FALSE
  )
}

for (wave in c("Cycle A", "Cycle B")) for (form in paste("Form", 1:3)) {
  x <- items[items$wave == wave & items$form == form, ]
  plot <- ggplot(x, aes(facility, fill = domain)) +
    geom_histogram(binwidth = 0.1, boundary = 0, color = "white") +
    facet_wrap(~domain) + scale_fill_manual(values = c("#167c81", "#dc9250")) +
    scale_x_continuous(limits = c(0, 1)) +
    labs(x = "Fictional item facility", y = "Generated items", fill = NULL)
  save_figure(plot, paste("Facility ·", wave, "·", form), "Item response")
}

for (form in paste("Form", 1:3)) {
  item <- items$item_id[items$wave == "Cycle A" & items$form == form][1L]
  x <- profiles[profiles$item_id == item, ]
  x$rest_score_group <- factor(x$rest_score_group, levels = paste("Group", 1:5))
  plot <- ggplot(x, aes(rest_score_group, proportion, color = option,
                        group = option)) +
    geom_line(linewidth = 1) + geom_point(size = 2) +
    scale_color_brewer(palette = "Dark2") +
    scale_y_continuous(limits = c(0, 1)) +
    labs(x = "Synthetic rest-score group", y = "Option share", color = "Option")
  save_figure(plot, paste("Option profiles ·", form), "Response options")
}

for (form in paste("Form", 1:3)) {
  x <- curves[curves$wave == "Cycle A" & curves$form == form, ]
  plot <- ggplot(x, aes(theta, information, color = domain)) +
    geom_line(linewidth = 1.1) +
    scale_color_manual(values = c("#167c81", "#dc9250")) +
    labs(x = "Synthetic ability", y = "Test information", color = NULL)
  save_figure(plot, paste("Information · Cycle A ·", form), "Ability precision")
}

for (form in paste("Form", 1:3)) {
  x <- residual_pairs[residual_pairs$wave == "Cycle B" &
                        residual_pairs$form == form, ]
  plot <- ggplot(x, aes(q3_centered, fill = domain)) +
    geom_histogram(binwidth = 0.04, color = "white", alpha = 0.85) +
    facet_wrap(~domain) +
    scale_fill_manual(values = c("#167c81", "#dc9250")) +
    scale_x_continuous(breaks = c(-0.1, 0, 0.1)) +
    labs(x = "Centered fictional residual association", y = "Item pairs", fill = NULL) +
    theme(legend.position = "none")
  save_figure(plot, paste("Residual pairs · Cycle B ·", form), "Model diagnostics")
}

manifest <- do.call(rbind, manifest_rows)
stopifnot(nrow(manifest) == 15L, !anyDuplicated(manifest$file),
          all(file.exists(manifest$file)),
          all(file.info(manifest$file)$size > 0L))
write.csv(manifest, "data/figure_manifest.csv", row.names = FALSE)
cat("Created 15 fictional PNG figures and a public figure manifest.\n")
