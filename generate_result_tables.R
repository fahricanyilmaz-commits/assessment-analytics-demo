# Independent result-table designs built only from this project's synthetic CSVs.
items <- read.csv("data/item_register.csv", stringsAsFactors = FALSE)
profiles <- read.csv("data/option_profiles.csv", stringsAsFactors = FALSE)
curves <- read.csv("data/information_curves.csv", stringsAsFactors = FALSE)
evidence <- read.csv("data/review_examples.csv", stringsAsFactors = FALSE)
pairs <- read.csv("data/residual_pairs.csv", stringsAsFactors = FALSE)
stopifnot(nrow(items) == 144L, nrow(profiles) > 0L, nrow(curves) > 0L)

topics <- c("Item performance", "Response options", "Models and precision",
            "Score scenarios", "Review workflow")
dir.create("data/results", showWarnings = FALSE)
manifest_rows <- list()
topic_counts <- integer(5)

add_table <- function(topic_id, slug, title, values) {
  stopifnot(nrow(values) > 0L, !anyNA(values), !anyDuplicated(names(values)),
            all(grepl("^[a-z0-9_]+$", names(values))))
  topic_counts[topic_id] <<- topic_counts[topic_id] + 1L
  result_id <- topic_counts[topic_id]
  path <- sprintf("data/results/topic_%02d/%s.csv", topic_id, slug)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  write.csv(values, path, row.names = FALSE)
  kinds <- vapply(values, function(x) {
    if (is.integer(x)) "integer" else if (is.numeric(x)) "number" else "category"
  }, character(1))
  kinds[grepl("(^|_)id$", names(kinds))] <- "identifier"
  manifest_rows[[length(manifest_rows) + 1L]] <<- data.frame(
    topic_id = topic_id, topic = topics[topic_id], result_id = result_id,
    key = sprintf("T%d-R%03d", topic_id, result_id), title = title,
    path = path, report_table = result_id <= 3L,
    column_count = ncol(values), columns = paste(names(values), collapse = "|"),
    kinds = paste(kinds, collapse = "|"), stringsAsFactors = FALSE)
}

form_count <- aggregate(item_id ~ wave + form + domain, items, length)
names(form_count)[4] <- "item_count"
form_facility <- aggregate(facility ~ wave + form + domain, items, mean)
names(form_facility)[4] <- "mean_facility"
form_summary <- merge(form_count, form_facility)
form_summary$mean_facility <- round(form_summary$mean_facility, 3)
facility_bands <- items[c("wave", "form", "domain")]
facility_bands$band <- cut(items$facility, c(-Inf, .30, .70, Inf),
                           labels = c("Low", "Middle", "High"))
facility_bands <- as.data.frame(table(facility_bands), stringsAsFactors = FALSE)
names(facility_bands)[5] <- "item_count"
item_ranks <- items[c("item_id", "wave", "form", "domain", "facility")]
item_ranks$facility_rank <- rank(-item_ranks$facility, ties.method = "first")
item_ranks <- item_ranks[order(item_ranks$facility_rank), ]
add_table(1, "item_register", "Generated item register", items)
add_table(1, "item_metrics", "Response metrics by item",
          items[c("item_id", "wave", "form", "domain", "sample_n", "facility", "rest_r")])
add_table(1, "form_summary", "Form and domain summary", form_summary)
add_table(1, "facility_bands", "Facility bands", facility_bands)
add_table(1, "simulation_inputs", "Item simulation inputs",
          items[c("item_id", "key", "simulation_a", "simulation_b")])
add_table(1, "facility_ranking", "Facility ranking", item_ranks)

option_data <- merge(profiles, items[c("item_id", "wave", "form", "domain", "key")],
                     by = "item_id", sort = FALSE)
option_data <- option_data[order(option_data$item_id, option_data$rest_score_group,
                                 option_data$option), ]
option_data$is_key <- option_data$option == option_data$key
group_shares <- aggregate(proportion ~ rest_score_group + option, option_data, mean)
names(group_shares)[3] <- "mean_option_share"
group_shares$mean_option_share <- round(group_shares$mean_option_share, 4)
option_totals <- aggregate(count ~ wave + form + option, option_data, sum)
option_ranges <- aggregate(proportion ~ item_id + option, option_data,
                           function(x) round(max(x) - min(x), 4))
names(option_ranges)[3] <- "share_range"
add_table(2, "option_profiles", "Option shares by score group", option_data)
add_table(2, "key_profiles", "Key shares by score group",
          option_data[option_data$is_key, c("item_id", "rest_score_group", "proportion", "count")])
add_table(2, "distractor_profiles", "Distractor shares by score group",
          option_data[!option_data$is_key, c("item_id", "rest_score_group", "option", "proportion")])
add_table(2, "group_shares", "Mean share by group and option", group_shares)
add_table(2, "option_totals", "Option counts by form", option_totals)
add_table(2, "option_ranges", "Within-item option share ranges", option_ranges)

curve_grid <- merge(items[c("item_id", "wave", "form", "domain", "simulation_a", "simulation_b")],
                    data.frame(theta = c(-2, -1, 0, 1, 2)))
curve_grid <- curve_grid[order(curve_grid$item_id, curve_grid$theta), ]
curve_grid$probability <- round(plogis(curve_grid$simulation_a *
                                        (curve_grid$theta - curve_grid$simulation_b)), 4)
rasch_grid <- curve_grid[c("item_id", "theta", "simulation_b")]
rasch_grid$probability <- round(plogis(rasch_grid$theta - rasch_grid$simulation_b), 4)
rasch_grid$simulation_b <- NULL
residual_summary <- aggregate(q3_centered ~ wave + form + domain, pairs,
                              function(x) round(mean(abs(x)), 4))
names(residual_summary)[4] <- "mean_absolute_centered_q3"
model_notes <- data.frame(
  illustration = c("Generated 2PL", "Unit-slope sensitivity"),
  slope_rule = c("Item-specific generated discrimination", "Discrimination fixed to one"),
  estimation = c("Simulation input; no model fitted", "Illustration; no model fitted"))
add_table(3, "two_pl_inputs", "Generated 2PL inputs",
          items[c("item_id", "wave", "form", "domain", "simulation_a", "simulation_b")])
add_table(3, "response_curve_grid", "Illustrative response curves", curve_grid)
add_table(3, "rasch_unit_slope", "Unit-slope sensitivity curves", rasch_grid)
add_table(3, "residual_pairs", "Generated residual pairs", pairs)
add_table(3, "residual_summary", "Residual-pair summary", residual_summary)
add_table(3, "model_notes", "Model illustration notes", model_notes)

group_keys <- unique(items[c("wave", "form", "domain")])
group_keys <- group_keys[order(group_keys$wave, group_keys$form, group_keys$domain), ]
by_group <- function(fun) do.call(rbind, lapply(seq_len(nrow(group_keys)), function(i) {
  row <- group_keys[i, ]
  x <- curves[curves$wave == row$wave & curves$form == row$form &
                curves$domain == row$domain, ]
  data.frame(wave = row$wave, form = row$form, domain = row$domain, fun(x))
}))
peak_information <- by_group(function(x) {
  peak <- x[which.max(x$information), ]
  data.frame(peak_theta = peak$theta, peak_information = peak$information)
})
curve_ranges <- by_group(function(x) data.frame(
  min_information = min(x$information), max_information = max(x$information),
  min_local_se = min(x$local_se), max_local_se = max(x$local_se)))
parameter_summary <- aggregate(cbind(simulation_a, simulation_b) ~ wave + form + domain,
                               items, mean)
names(parameter_summary)[4:5] <- c("mean_discrimination", "mean_location")
parameter_summary[4:5] <- lapply(parameter_summary[4:5], round, 3)
add_table(3, "information_curves", "Theoretical information curves", curves)
add_table(3, "peak_information", "Peak information by form", peak_information)
add_table(3, "center_precision", "Precision at ability zero",
          curves[abs(curves$theta) < 1e-9, ])
add_table(3, "tail_precision", "Precision at ability tails",
          curves[abs(abs(curves$theta) - 2) < 1e-9, ])
add_table(3, "parameter_summary", "Generated parameter summary", parameter_summary)
add_table(3, "curve_ranges", "Information and error ranges", curve_ranges)

scoring <- expand.grid(form = paste("Form", 1:3), domain = c("Domain A", "Domain B"),
                       raw_score = 0:12, stringsAsFactors = FALSE)
scoring$linear_score <- round(100 * scoring$raw_score / 12, 1)
form_exponent <- c("Form 1" = .90, "Form 2" = .94, "Form 3" = .98)
scoring$curved_score <- round(100 * (scoring$raw_score / 12)^
                                form_exponent[scoring$form], 1)
scoring$score_difference <- round(scoring$curved_score - scoring$linear_score, 1)
score_examples <- expand.grid(form = paste("Form", 1:3), raw_a = c(0, 3, 6, 9, 12),
                              raw_b = c(0, 3, 6, 9, 12), stringsAsFactors = FALSE)
score_examples$overall_linear <- round(100 * (score_examples$raw_a + score_examples$raw_b) / 24, 1)
score_examples$classification <- ifelse(score_examples$overall_linear >= 65,
                                        "Above demo threshold", "Below demo threshold")
form_rules <- data.frame(form = names(form_exponent), curved_exponent = unname(form_exponent),
                         demo_threshold = 65L, rule_status = "Illustrative only")
add_table(4, "linear_conversion", "Linear score conversion",
          scoring[c("form", "domain", "raw_score", "linear_score")])
add_table(4, "curved_conversion", "Illustrative curved conversion",
          scoring[c("form", "domain", "raw_score", "curved_score")])
add_table(4, "conversion_difference", "Difference between conversions", scoring)
add_table(4, "score_examples", "Hypothetical score pairs", score_examples)
add_table(4, "threshold_examples", "Examples near the demo threshold",
          score_examples[abs(score_examples$overall_linear - 65) <= 15, ])
add_table(4, "form_rules", "Invented form rules", form_rules)

review_items <- items[items$review_flag != "Routine", ]
review_layers <- items[c("item_id", "wave", "form", "domain", "review_flag")]
review_layers$review_layer <- ifelse(review_layers$review_flag == "Routine", "Neither",
                                     ifelse(items$position %% 2L == 0L, "Layer A", "Layer B"))
flag_counts <- as.data.frame(table(items[c("wave", "form", "review_flag")]),
                             stringsAsFactors = FALSE)
names(flag_counts)[4] <- "item_count"
form_workload <- aggregate(review_flag ~ wave + form + domain, items,
                           function(x) sum(x != "Routine"))
names(form_workload)[4] <- "flagged_items"
rule_definitions <- data.frame(
  flag = c("Low facility", "High facility", "Low item-rest association"),
  demo_rule = c("Facility below 0.25", "Facility above 0.85",
                "Item-rest association below 0.15"),
  interpretation = "Illustrative review prompt, not an operational decision")
add_table(5, "review_queue", "Generated review queue", evidence)
add_table(5, "review_flags", "Flagged item metrics",
          review_items[c("item_id", "wave", "form", "domain", "facility", "rest_r", "review_flag")])
add_table(5, "flag_counts", "Flag counts by form", flag_counts)
add_table(5, "review_layers", "Illustrative review layers", review_layers)
add_table(5, "form_workload", "Review workload by form", form_workload)
add_table(5, "rule_definitions", "Illustrative flag rules", rule_definitions)

manifest <- do.call(rbind, manifest_rows)
stopifnot(nrow(manifest) == 36L, identical(topic_counts, c(6L, 6L, 12L, 6L, 6L)),
          !anyDuplicated(manifest$path), !anyDuplicated(manifest$key))
write.csv(manifest, "data/table_manifest.csv", row.names = FALSE)
cat("Created 36 independent synthetic result tables across five invented topics.\n")
