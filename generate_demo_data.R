# Fully fictional assessment demonstration. No study files are read.
set.seed(618402)
dir.create("data", showWarnings = FALSE)

waves <- c("Cycle A", "Cycle B")
forms <- paste("Form", 1:3)
domains <- c("Domain A", "Domain B")
options <- LETTERS[1:4]
n_people <- 420L
n_items <- 24L

item_rows <- list()
profile_rows <- list()
residual_rows <- list()
idx <- 0L

for (wave in waves) for (form in forms) {
  people <- seq_len(n_people)
  theta_a <- rnorm(n_people)
  theta_b <- 0.55 * theta_a + sqrt(1 - 0.55^2) * rnorm(n_people)
  responses <- matrix(NA_integer_, n_people, n_items)
  expected <- matrix(NA_real_, n_people, n_items)
  chosen <- matrix("", n_people, n_items)
  form_items <- vector("list", n_items)

  for (position in seq_len(n_items)) {
    domain <- domains[1L + (position > 12L)]
    theta <- if (domain == "Domain A") theta_a else theta_b
    a <- round(runif(1, 0.65, 2.05), 3)
    b <- round(rnorm(1, 0, 0.9), 3)
    key <- sample(options, 1)
    probability <- plogis(a * (theta - b))
    expected[, position] <- probability
    correct <- rbinom(n_people, 1, probability)
    distractors <- options[options != key]
    answer <- ifelse(correct == 1, key, sample(distractors, n_people, TRUE))
    responses[, position] <- correct
    chosen[, position] <- answer
    idx <- idx + 1L
    form_items[[position]] <- data.frame(
      item_id = sprintf("DEMO-%03d", idx), wave = wave, form = form,
      position = position, domain = domain, key = key,
      simulation_a = a, simulation_b = b, stringsAsFactors = FALSE
    )
  }

  group_items <- do.call(rbind, form_items)
  for (position in seq_len(n_items)) {
    item <- group_items[position, ]
    own <- which(group_items$domain == item$domain)
    rest <- rowSums(responses[, own, drop = FALSE]) - responses[, position]
    facility <- mean(responses[, position])
    rest_r <- suppressWarnings(cor(responses[, position], rest))
    if (!is.finite(rest_r)) rest_r <- NA_real_
    flag <- if (facility < 0.25) "Low facility" else if (facility > 0.85) "High facility" else if (!is.na(rest_r) && rest_r < 0.15) "Low item-rest association" else "Routine"
    item$sample_n <- n_people
    item$facility <- round(facility, 3)
    item$rest_r <- round(rest_r, 3)
    item$review_flag <- flag
    item_rows[[length(item_rows) + 1L]] <- item

    groups <- cut(rank(rest, ties.method = "random"),
                  breaks = seq(0, n_people, length.out = 6),
                  include.lowest = TRUE, labels = paste("Group", 1:5))
    for (group in levels(groups)) for (option in options) {
      selected <- groups == group
      profile_rows[[length(profile_rows) + 1L]] <- data.frame(
        item_id = item$item_id, rest_score_group = group, option = option,
        proportion = round(mean(chosen[selected, position] == option), 4),
        count = sum(chosen[selected, position] == option),
        group_n = sum(selected), stringsAsFactors = FALSE
      )
    }
  }
  residual <- responses - expected
  for (domain in domains) {
    positions <- which(group_items$domain == domain)
    pairs <- utils::combn(positions, 2)
    q3 <- apply(pairs, 2, function(pair) cor(residual[, pair[1]], residual[, pair[2]]))
    for (k in seq_len(ncol(pairs))) {
      residual_rows[[length(residual_rows) + 1L]] <- data.frame(
        wave = wave, form = form, domain = domain,
        item_i = group_items$item_id[pairs[1, k]],
        item_j = group_items$item_id[pairs[2, k]],
        q3 = round(q3[k], 4), q3_centered = round(q3[k] - mean(q3), 4),
        stringsAsFactors = FALSE
      )
    }
  }
}

items <- do.call(rbind, item_rows)
profiles <- do.call(rbind, profile_rows)
residual_pairs <- do.call(rbind, residual_rows)
theta_grid <- seq(-3.5, 3.5, by = 0.1)
curve_rows <- list()
for (wave in waves) for (form in forms) for (domain in domains) {
  subset <- items[items$wave == wave & items$form == form & items$domain == domain, ]
  info <- vapply(theta_grid, function(theta) {
    p <- plogis(subset$simulation_a * (theta - subset$simulation_b))
    sum(subset$simulation_a^2 * p * (1 - p))
  }, numeric(1))
  curve_rows[[length(curve_rows) + 1L]] <- data.frame(
    wave = wave, form = form, domain = domain, theta = theta_grid,
    information = round(info, 4), local_se = round(1 / sqrt(info), 4),
    stringsAsFactors = FALSE
  )
}
curves <- do.call(rbind, curve_rows)
evidence <- items[items$review_flag != "Routine",
                  c("item_id", "wave", "form", "domain", "facility", "rest_r", "review_flag")]
names(evidence)[names(evidence) == "review_flag"] <- "illustrative_reason"

write.csv(items, "data/item_register.csv", row.names = FALSE, na = "")
write.csv(profiles, "data/option_profiles.csv", row.names = FALSE)
write.csv(curves, "data/information_curves.csv", row.names = FALSE)
write.csv(evidence, "data/review_examples.csv", row.names = FALSE)
write.csv(residual_pairs, "data/residual_pairs.csv", row.names = FALSE)
cat("Created fictional demo data: ", nrow(items), " items; ",
    nrow(profiles), " option profile rows.\n", sep = "")
