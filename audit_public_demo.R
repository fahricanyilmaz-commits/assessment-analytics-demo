# Exercise the public dashboard locally after regeneration and before publication.
source("app.R", local = TRUE)

expected_tabs <- c("Overview", "Item review", "Evidence explorer",
                   "Ability precision", "Scoring", "Results library")
ui_html <- as.character(ui)
if (!all(vapply(expected_tabs, grepl, logical(1), x = ui_html, fixed = TRUE)))
  stop("A required navigation view is missing.")
source_text <- paste(readLines("app.R", warn = FALSE), collapse = "\n")
button_matches <- regmatches(source_text, gregexpr('downloadButton\\("[a-z_]+"', source_text))[[1L]]
handler_matches <- regmatches(source_text, gregexpr('output\\$download_[a-z_]+ <- downloadHandler', source_text))[[1L]]
button_ids <- sub('^downloadButton\\("([a-z_]+)"$', '\\1', button_matches)
handler_ids <- sub('^output\\$(download_[a-z_]+) <- downloadHandler$', '\\1', handler_matches)
if (!setequal(button_ids, handler_ids)) stop("A download button has no matching handler.")

render_ids <- c(
  "overview_plot", "overview_table", "item_details", "item_summary",
  "item_curve", "irt_summary", "irt_pairs", "irt_fit", "irt_intervals",
  "irt_model_record", "nominal_plot", "nominal_parameters", "rasch_plot",
  "rasch_summary", "rasch_fit_record", "rasch_link_record", "rasch_anchor_record",
  "option_plot", "option_table", "item_register", "item_evidence",
  "evidence_plot", "evidence_table", "result_context", "result_table",
  "result_plot", "figure_view", "topic_definitions", "precision_plot",
  "precision_table", "score_a", "score_b", "score_total", "score_class",
  "score_plot", "score_table", "demo_preview", "catalogue_table"
)
download_ids <- c(
  "download_filtered", "download_ctt", "download_irt", "download_irt_pairs",
  "download_nominal", "download_nominal_parameters", "download_rasch",
  "download_item_record", "download_item_evidence", "download_nominal_pdf",
  "download_empirical_pdf", "download_note", "download_evidence",
  "download_result", "download_figure", "download_curve", "download_file",
  "download_catalogue"
)

shiny::testServer(server, {
  session$setInputs(
    item_wave = "Cycle A", item_component = "Component A", item_form = "Form 1",
    item_domain = "Domain A", item_layer = "All", review_only = FALSE,
    item_search = "", item_id = "DEMO-001", reason = "All",
    result_topic = "1", result_key = "T1-R001",
    figure_file = figure_manifest$file[1],
    theta_range = c(-3, 3), precision_wave = "Cycle A",
    precision_form = "Form 1", precision_domain = "Domain A",
    precision_measure = "information", precision_sensitivity = TRUE,
    score_form = "Form 1", raw_a = 6, raw_b = 6, cut = 65,
    reviewer = "Demo reviewer", decision = "Pending", rationale = "Illustrative only",
    demo_file = "Item register", catalogue_search = "",
    report_tables_only = TRUE
  )
  stopifnot(nrow(filtered_items()) == 12L,
            selected_item()$item_id == "DEMO-001",
            nrow(selected_profiles()) == 20L,
            nrow(precision_display()) == 142L,
            identical(sort(unique(precision_display()$model)),
                      sort(c("Generated 2PL", "Unit-slope illustration"))),
            nrow(selected_result()) > 0L,
            nrow(score_values()) == 2L)

  for (id in render_ids) {
    value <- tryCatch(output[[id]], error = function(error) {
      stop("Output failed: ", id, " — ", conditionMessage(error))
    })
    if (is.null(value)) stop("Output returned NULL: ", id)
  }
  for (id in download_ids) {
    path <- tryCatch(output[[id]], error = function(error) {
      stop("Download failed: ", id, " — ", conditionMessage(error))
    })
    if (length(path) != 1L || !file.exists(path) || file.info(path)$size == 0L)
      stop("Download is empty or missing: ", id)
    if (grepl("_pdf$", id) &&
        !identical(rawToChar(readBin(path, "raw", n = 5L)), "%PDF-"))
      stop("Download is not a PDF: ", id)
  }

  session$setInputs(item_search = "DEMO-001")
  stopifnot(nrow(filtered_items()) == 1L)
  session$setInputs(item_search = "", review_only = TRUE)
  stopifnot(all(filtered_items()$review_flag != "Routine"))
  session$setInputs(review_only = FALSE, item_layer = "Layer A")
  stopifnot(all(filtered_items()$review_layer == "Layer A"))
  session$setInputs(item_layer = "All", precision_sensitivity = FALSE)
  stopifnot(nrow(precision_display()) == 71L)
  first_form_score <- score_values()[["Domain A"]][2]
  session$setInputs(score_form = "Form 3")
  stopifnot(score_values()[["Domain A"]][2] != first_form_score)
  session$setInputs(report_tables_only = FALSE)
  stopifnot(nrow(result_catalogue[result_catalogue$topic_id == 1L, ]) >
              sum(result_catalogue$topic_id == 1L & result_catalogue$report_table))
})

cat("Six-view functional audit passed.\n")
