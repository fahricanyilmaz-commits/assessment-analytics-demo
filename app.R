Sys.setenv(R_USER_CACHE_DIR = file.path(tempdir(), "assessment-demo-cache"))
library(shiny)
library(bslib)
library(ggplot2)

# This app reads only files created by the generators in this folder.
demo_files <- c(
  "Item register" = "data/item_register.csv",
  "Option profiles" = "data/option_profiles.csv",
  "Information curves" = "data/information_curves.csv",
  "Review examples" = "data/review_examples.csv",
  "Residual pairs" = "data/residual_pairs.csv"
)
stopifnot(all(file.exists(demo_files)))
items <- read.csv(demo_files[["Item register"]], stringsAsFactors = FALSE)
items$component <- ifelse(items$domain == "Domain A", "Component A", "Component B")
items$review_layer <- ifelse(items$review_flag == "Routine", "Neither",
                             ifelse(items$position %% 2L == 0L, "Layer A", "Layer B"))
items$skill_code <- sprintf("SKILL-%02d", (items$position - 1L) %% 6L + 1L)
profiles <- read.csv(demo_files[["Option profiles"]], stringsAsFactors = FALSE)
curves <- read.csv(demo_files[["Information curves"]], stringsAsFactors = FALSE)
evidence <- read.csv(demo_files[["Review examples"]], stringsAsFactors = FALSE)
residual_pairs <- read.csv(demo_files[["Residual pairs"]], stringsAsFactors = FALSE)
figure_manifest <- read.csv("data/figure_manifest.csv", stringsAsFactors = FALSE)
stopifnot(nrow(figure_manifest) == 15L, all(file.exists(figure_manifest$file)))
atlas_files <- c("data/atlases/nominal_option_curves_demo.pdf",
                 "data/atlases/empirical_option_profiles_demo.pdf")
stopifnot(all(file.exists(atlas_files)))

manifest <- read.csv("data/table_manifest.csv", stringsAsFactors = FALSE,
                     check.names = FALSE)
stopifnot(nrow(manifest) == 36L, !anyDuplicated(manifest$key),
          all(file.exists(manifest$path)))
topic_names <- unique(manifest$topic[order(manifest$topic_id)])
result_catalogue <- manifest[, c("topic_id", "topic", "result_id", "title",
                                 "key", "path", "report_table")]

theme_plot <- theme_minimal(base_size = 13) +
  theme(panel.grid.minor = element_blank(), plot.title = element_text(face = "bold"),
        legend.position = "bottom")
show_table <- function(x, n = 20) {
  if (!nrow(x)) return(data.frame(Message = "No rows match the current filters."))
  utils::head(x, n)
}
metric <- function(label, value, note = NULL) {
  div(class = "metric", div(class = "metric-label", label),
      div(class = "metric-value", value), div(class = "metric-note", note))
}

ui <- page_navbar(
  title = div(class = "brand", "Assessment Analytics", span(class = "brand-tag", "DEMO")),
  id = "main_nav", fillable = FALSE,
  theme = bs_theme(version = 5, primary = "#167c81", bg = "#f4f7f9",
                   fg = "#1d3141", base_font = font_collection("Arial", "sans-serif")),
  header = tagList(
    tags$head(tags$style(HTML("
      .navbar {background:#13263f!important;padding:12px 24px;box-shadow:0 2px 10px #13263f20}
      .navbar .nav-link,.navbar-brand {color:#edf5fa!important}
      .brand {font-weight:700;letter-spacing:.02em}.brand-tag {font-size:10px;background:#38b8ae;
        color:#12303d;border-radius:4px;padding:4px 7px;margin-left:9px;vertical-align:middle}
      .demo-banner {background:#fff4d9;border-left:4px solid #d3992f;padding:14px 20px;
        margin:18px 0 24px;border-radius:4px;line-height:1.5}
      .intro {margin:28px 0 23px}.intro h2 {font-weight:750;color:#13263f;margin:0 0 8px}
      .intro p {color:#526579;max-width:1000px}.metric {background:white;border:1px solid #dde6ed;
        border-radius:10px;padding:20px 22px;height:100%;box-shadow:0 3px 12px #13263f08}
      .metric-label {font-size:12px;color:#687a88;font-weight:700;text-transform:uppercase}
      .metric-value {font-size:32px;color:#13263f;font-weight:700;margin:5px 0}
      .metric-note {font-size:12px;color:#718391}.bslib-card {border:1px solid #dde6ed;
        border-radius:10px;box-shadow:0 3px 12px #13263f08}.card-header {font-weight:700}
      .table {font-size:12px}.table-responsive {overflow-x:auto}.foot {color:#637587;
        font-size:12px;margin:26px 0}.shiny-plot-output {max-width:100%}
    "))),
    div(class = "container-fluid", div(class = "demo-banner",
      tags$strong("Fictional demonstration."),
      " Every value, identifier, response, rule and result in this app was generated for the demo."))
  ),

  nav_panel("Overview",
    div(class = "intro", h2("Explore an assessment workflow"),
        p("A public demonstration of item review, response options, measurement precision and score scenarios using generated data.")),
    layout_columns(
      metric("Generated items", nrow(items), "Across fictional cycles and forms"),
      metric("Fictional cycles", length(unique(items$wave)), "Cycle A and Cycle B"),
      metric("Forms per cycle", length(unique(items$form)), "24 items per form"),
      metric("Review examples", nrow(evidence), "Illustrative screening rules"),
      col_widths = c(3, 3, 3, 3)),
    br(), layout_columns(
      card(card_header("Facility distribution"), plotOutput("overview_plot", height = 320)),
      card(card_header("How to use this demo"),
           tags$ol(tags$li("Filter the generated item register and select an item."),
                   tags$li("Inspect response traces and an illustrative 2PL curve."),
                   tags$li("Compare information curves within a chosen fictional form."),
                   tags$li("Try hypothetical scoring rules in the calculator.")),
           p("Plots and statistics describe generated responses only. They are not findings about a real assessment.")),
      col_widths = c(6, 6)),
    card(card_header("Generated data summary"), tableOutput("overview_table"))
  ),

  nav_panel("Item review",
    div(class = "intro", h2("Inspect generated items"),
        p("Choose a fictional item to see its response pattern and the parameters used to generate it.")),
    layout_sidebar(fillable = FALSE, fill = FALSE,
      sidebar = sidebar(width = 280,
        selectInput("item_wave", "Cycle", c("All", unique(items$wave))),
        selectInput("item_component", "Component", c("All", "Component A", "Component B")),
        selectInput("item_form", "Form", c("All", unique(items$form))),
        selectInput("item_domain", "Domain", c("All", unique(items$domain))),
        selectInput("item_layer", "Review layer", c("All", "Layer A", "Layer B", "Neither")),
        checkboxInput("review_only", "Review-index items only", FALSE),
        textInput("item_search", "Search item ID, skill or position"),
        selectInput("item_id", "Selected item", choices = items$item_id),
        downloadButton("download_filtered", "Download filtered items")),
      card(card_header("Selected item"), uiOutput("item_details"),
        navset_card_tab(
          nav_panel("CTT", tableOutput("item_summary"),
                    p("Facility and item-rest association are calculated from generated responses."),
                    plotOutput("option_plot", height = 320),
                    h4("Response-option statistics"), tableOutput("option_table"),
                    downloadButton("download_ctt", "Download filtered CTT results")),
          nav_panel("Primary IRT", plotOutput("item_curve", height = 350),
                    p("The curve uses the known 2PL parameters of the simulation. It is not a fitted estimate."),
                    h4("Generated parameter record"), tableOutput("irt_summary"),
                    h4("Illustrative item-fit diagnostic"), tableOutput("irt_fit"),
                    h4("Parameter uncertainty"), tableOutput("irt_intervals"),
                    h4("Model record"), tableOutput("irt_model_record"),
                    h4("Residual pairs involving this item"), tableOutput("irt_pairs"),
                    downloadButton("download_irt", "Download filtered IRT records"),
                    downloadButton("download_irt_pairs", "Download this item's residual pairs")),
          nav_panel("Nominal response", plotOutput("nominal_plot", height = 350),
                    p("Option probabilities follow the 2PL correct-response probability, with the remaining probability shared by three distractors. This is an illustration, not a fitted nominal model."),
                    sliderInput("theta_range", "Simulated ability range", -4, 4, c(-3, 3), step = 0.5),
                    h4("Illustrative option parameters"), tableOutput("nominal_parameters"),
                    downloadButton("download_nominal", "Download filtered nominal records"),
                    downloadButton("download_nominal_parameters", "Download this item's option parameters")),
          nav_panel("Rasch sensitivity", plotOutput("rasch_plot", height = 350),
                    p("This sensitivity curve fixes discrimination to one and uses the generated item location. It has no empirical calibration claim."),
                    tableOutput("rasch_summary"),
                    h4("Form estimation record"), tableOutput("rasch_fit_record"),
                    h4("Illustrative form linking"), tableOutput("rasch_link_record"),
                    h4("Anchor membership"), tableOutput("rasch_anchor_record"),
                    downloadButton("download_rasch", "Download filtered Rasch records"))
        )),
      card(card_header("Filtered item register"), tableOutput("item_register"))
    ),
    card(card_header("Review evidence for the selected item"), tableOutput("item_evidence"),
         downloadButton("download_item_record", "Download full item record"),
         downloadButton("download_item_evidence", "Download review evidence")),
    card(card_header("Generated option atlases"),
         downloadButton("download_nominal_pdf", "Illustrative option curves PDF"),
         downloadButton("download_empirical_pdf", "Generated option profiles PDF")),
    card(card_header("Download an illustrative review note"),
      layout_columns(textInput("reviewer", "Reviewer alias (optional)"),
                     selectInput("decision", "Decision", c("Pending", "Retain", "Review", "Revise")),
                     textAreaInput("rationale", "Optional rationale", rows = 2),
                     col_widths = c(3, 3, 6)),
      downloadButton("download_note", "Download note"),
      p("The note is generated when downloaded and is not stored by the app.")),
    h3("Illustrative screening evidence"),
    p("Simple rules flag some generated items to demonstrate a review queue. These flags do not imply a real item or study finding."),
    layout_columns(
      card(card_header("Facility and item-rest association"), plotOutput("evidence_plot", height = 420)),
      card(card_header("Review examples"), selectInput("reason", "Reason",
        c("All", unique(evidence$illustrative_reason))), tableOutput("evidence_table"),
        downloadButton("download_evidence", "Download examples")),
      col_widths = c(6, 6))
  ),

  nav_panel("Evidence explorer",
    div(class = "intro", h2("Explore generated result tables"),
        p("Five invented analytical topics contain 36 result CSVs built from generated data.")),
    layout_sidebar(fillable = FALSE, fill = FALSE,
      sidebar = sidebar(width = 310,
        selectInput("result_topic", "Analytical topic",
                    choices = setNames(as.character(seq_along(topic_names)), topic_names)),
        checkboxInput("report_tables_only", "Show report tables only", TRUE),
        selectInput("result_key", "Result table", choices = NULL),
        downloadButton("download_result", "Download selected table"),
        tags$hr(),
        selectInput("figure_file", "Generated figure",
                    choices = setNames(figure_manifest$file, figure_manifest$title)),
        downloadButton("download_figure", "Download figure PNG")),
      card(card_header("Selected generated result"),
           uiOutput("result_context"),
           div(class = "table-responsive", tableOutput("result_table"))),
      card(card_header("Selected table preview"), plotOutput("result_plot", height = 350),
           p("This preview is drawn live from the selected fictional CSV.")),
      card(card_header("Figure viewer"), uiOutput("figure_view")),
      card(card_header("Topic scope and interpretation"), tableOutput("topic_definitions"))
    )
  ),

  nav_panel("Ability precision",
    div(class = "intro", h2("Where the fictional form is informative"),
        p("Theoretical information follows from the parameters used to simulate each item. Each selection is on its own generated scale.")),
    layout_sidebar(fillable = FALSE, fill = FALSE,
      sidebar = sidebar(width = 280,
        selectInput("precision_wave", "Cycle", unique(items$wave)),
        selectInput("precision_form", "Form", unique(items$form)),
        selectInput("precision_domain", "Domain", unique(items$domain)),
        radioButtons("precision_measure", "Display",
          c("Test information" = "information", "Local standard error" = "local_se")),
        checkboxInput("precision_sensitivity", "Include supporting model", TRUE),
        downloadButton("download_curve", "Download curve values")),
      card(card_header("Generated information curve"), plotOutput("precision_plot", height = 430),
           p("Local standard error is 1 / sqrt(information). It is an illustration, not an estimate of precision for any real examination."),
           tableOutput("precision_table"))
    )
  ),

  nav_panel("Scoring",
    div(class = "intro", h2("Try fictional score rules"),
        p("Adjust the raw scores and threshold. The conversions below are invented for interaction design and have no operational use.")),
    layout_sidebar(fillable = FALSE, fill = FALSE,
      sidebar = sidebar(width = 280,
        selectInput("score_form", "Fictional form", unique(items$form)),
        sliderInput("raw_a", "Domain A raw score", 0, 12, 6, step = 1),
        sliderInput("raw_b", "Domain B raw score", 0, 12, 6, step = 1),
        sliderInput("cut", "Overall passing threshold", 0, 100, 65, step = 1)),
      layout_columns(
        metric("Domain A", textOutput("score_a", inline = TRUE), "Linear reported score"),
        metric("Domain B", textOutput("score_b", inline = TRUE), "Linear reported score"),
        metric("Overall", textOutput("score_total", inline = TRUE), "Mean of the domains"),
        metric("Classification", textOutput("score_class", inline = TRUE), "Threshold selected above"),
        col_widths = c(3, 3, 3, 3)),
      br(), card(card_header("Comparison of two invented conversions"),
                 plotOutput("score_plot", height = 360), tableOutput("score_table"))
    )
  ),

  nav_panel("Results library",
    div(class = "intro", h2("Browse the full synthetic catalogue"),
        p("The library has 36 generated CSVs in five invented topics. All names, columns and values are fictional.")),
    layout_columns(metric("Result tables", nrow(result_catalogue), "Separate generated CSVs"),
                   metric("Topics", length(topic_names), "Generic analytical areas"),
                   metric("Core CSVs", length(demo_files), "Plus the separate result tables"),
                   col_widths = c(4, 4, 4)),
    br(), card(card_header("Catalogue"),
               textInput("catalogue_search", "Search by topic or result key"),
               div(class = "table-responsive", tableOutput("catalogue_table")),
               downloadButton("download_catalogue", "Download catalogue")),
    card(card_header("Generated source files"),
         selectInput("demo_file", "File", choices = names(demo_files)),
         div(class = "table-responsive", tableOutput("demo_preview")),
         downloadButton("download_file", "Download selected CSV")),
    div(class = "foot", "Assessment Analytics Demo · All labels and data are fictional")
  ),
  footer = div(class = "container-fluid foot", "Fictional demonstration · No real exam, organization or participant data")
)

server <- function(input, output, session) {
  filtered_items <- reactive({
    x <- items
    if (!is.null(input$item_wave) && input$item_wave != "All") x <- x[x$wave == input$item_wave, ]
    if (!is.null(input$item_component) && input$item_component != "All") x <- x[x$component == input$item_component, ]
    if (!is.null(input$item_form) && input$item_form != "All") x <- x[x$form == input$item_form, ]
    if (!is.null(input$item_domain) && input$item_domain != "All") x <- x[x$domain == input$item_domain, ]
    if (!is.null(input$item_layer) && input$item_layer != "All") x <- x[x$review_layer == input$item_layer, ]
    if (isTRUE(input$review_only)) x <- x[x$review_flag != "Routine", ]
    if (!is.null(input$item_search) && nzchar(trimws(input$item_search))) {
      query <- tolower(trimws(input$item_search))
      x <- x[grepl(query, tolower(x$item_id), fixed = TRUE) |
               grepl(query, tolower(x$skill_code), fixed = TRUE) |
               grepl(query, as.character(x$position), fixed = TRUE), ]
    }
    x
  })
  observe({
    x <- filtered_items()
    chosen <- if (!is.null(input$item_id) && input$item_id %in% x$item_id)
      input$item_id else if (nrow(x)) x$item_id[1] else character()
    updateSelectInput(session, "item_id", choices = x$item_id, selected = chosen)
  })
  selected_item <- reactive({
    req(input$item_id)
    x <- items[items$item_id == input$item_id, , drop = FALSE]
    req(nrow(x) == 1)
    x
  })
  output$overview_plot <- renderPlot({
    ggplot(items, aes(facility, fill = domain)) +
      geom_histogram(binwidth = 0.1, color = "white") +
      scale_fill_manual(values = c("#167c81", "#dc9250")) +
      labs(x = "Facility", y = "Generated items", fill = "Domain") + theme_plot
  })
  output$overview_table <- renderTable({
    n <- aggregate(item_id ~ wave + form + domain, items, length)
    f <- aggregate(facility ~ wave + form + domain, items, mean)
    names(n)[names(n) == "item_id"] <- "Items"
    names(f)[names(f) == "facility"] <- "Mean facility"
    x <- merge(n, f)
    x[["Mean facility"]] <- round(x[["Mean facility"]], 3)
    x
  }, rownames = FALSE, striped = TRUE)
  output$item_details <- renderUI({
    x <- selected_item()
    tagList(h3(x$item_id), p(paste(x$wave, "·", x$form, "·", x$domain,
                                   "· Position", x$position)),
            layout_columns(metric("Facility", sprintf("%.3f", x$facility)),
                           metric("Item-rest r", sprintf("%.3f", x$rest_r)),
                           metric("Review flag", x$review_flag),
                           col_widths = c(4, 4, 4)))
  })
  output$item_summary <- renderTable({
    x <- selected_item()
    data.frame(Statistic = c("Generated response count", "Key", "Facility",
                               "Item-rest association", "Simulation discrimination", "Simulation difficulty"),
               Value = c(x$sample_n, x$key, x$facility, x$rest_r,
                         x$simulation_a, x$simulation_b))
  }, rownames = FALSE, striped = TRUE)
  output$item_curve <- renderPlot({
    x <- selected_item()
    t <- seq(-3.5, 3.5, length.out = 200)
    y <- plogis(x$simulation_a * (t - x$simulation_b))
    ggplot(data.frame(ability = t, probability = y), aes(ability, probability)) +
      geom_line(color = "#167c81", linewidth = 1.2) +
      scale_y_continuous(limits = c(0, 1)) +
      labs(title = x$item_id, x = "Simulated ability", y = "Probability of correct response") + theme_plot
  })
  output$irt_summary <- renderTable({
    x <- selected_item()
    data.frame(Parameter = c("Model", "Discrimination", "Location", "Observed facility",
                             "Generated sample"),
               Value = c("Illustrative 2PL", x$simulation_a, x$simulation_b,
                         x$facility, x$sample_n))
  }, rownames = FALSE, striped = TRUE)
  output$irt_pairs <- renderTable({
    id <- selected_item()$item_id
    x <- residual_pairs[residual_pairs$item_i == id | residual_pairs$item_j == id, ]
    x <- x[order(abs(x$q3_centered), decreasing = TRUE), ]
    show_table(x, 12)
  }, rownames = FALSE, striped = TRUE)
  selected_pairs <- reactive({
    id <- selected_item()$item_id
    residual_pairs[residual_pairs$item_i == id | residual_pairs$item_j == id, ]
  })
  output$irt_fit <- renderTable(data.frame(
    Diagnostic = "Fitted item-fit statistic", Value = "Not calculated: parameters are simulation inputs"),
    rownames = FALSE, striped = TRUE)
  output$irt_intervals <- renderTable(data.frame(
    Parameter = c("Discrimination", "Location"),
    Interval = c("Not estimated in this demonstration", "Not estimated in this demonstration")),
    rownames = FALSE, striped = TRUE)
  output$irt_model_record <- renderTable(data.frame(
    Model = "Generated 2PL response process", Estimation = "No model fitting",
    Sample = selected_item()$sample_n), rownames = FALSE, striped = TRUE)
  nominal_parameters <- reactive({
    x <- selected_item()
    data.frame(option = LETTERS[1:4], key = LETTERS[1:4] == x$key,
               illustration = ifelse(LETTERS[1:4] == x$key,
                                     "2PL key probability", "One-third of remaining probability"),
               simulation_a = x$simulation_a, simulation_b = x$simulation_b)
  })
  output$nominal_parameters <- renderTable(nominal_parameters(), rownames = FALSE, striped = TRUE)
  output$nominal_plot <- renderPlot({
    x <- selected_item()
    theta <- seq(input$theta_range[1], input$theta_range[2], length.out = 180)
    correct <- plogis(x$simulation_a * (theta - x$simulation_b))
    z <- do.call(rbind, lapply(LETTERS[1:4], function(option) {
      data.frame(theta = theta, option = option,
                 probability = if (option == x$key) correct else (1 - correct) / 3)
    }))
    ggplot(z, aes(theta, probability, color = option)) + geom_line(linewidth = 1) +
      scale_y_continuous(limits = c(0, 1)) +
      scale_color_manual(values = c(A = "#167c81", B = "#dc9250",
                                    C = "#6658a8", D = "#66842f")) +
      labs(x = "Simulated ability", y = "Illustrative option probability",
           color = "Option", title = paste(x$item_id, "· key", x$key)) + theme_plot
  })
  output$rasch_plot <- renderPlot({
    x <- selected_item()
    theta <- seq(-3.5, 3.5, length.out = 180)
    ggplot(data.frame(theta = theta, probability = plogis(theta - x$simulation_b)),
           aes(theta, probability)) + geom_line(color = "#dc9250", linewidth = 1.1) +
      scale_y_continuous(limits = c(0, 1)) +
      labs(x = "Simulated ability", y = "Probability of correct response",
           title = paste(x$item_id, "· unit-slope sensitivity")) + theme_plot
  })
  output$rasch_summary <- renderTable({
    x <- selected_item()
    data.frame(Parameter = c("Discrimination", "Location", "Purpose"),
               Value = c("1 (fixed)", x$simulation_b, "Illustrative sensitivity"))
  }, rownames = FALSE, striped = TRUE)
  output$rasch_fit_record <- renderTable(data.frame(
    Record = "Unit-slope sensitivity", Estimation = "No Rasch fitting performed",
    Items_in_form = 24L), rownames = FALSE, striped = TRUE)
  output$rasch_link_record <- renderTable(data.frame(
    Record = "Form linking", Value = "Not calculated in this synthetic demonstration"),
    rownames = FALSE, striped = TRUE)
  output$rasch_anchor_record <- renderTable(data.frame(
    Record = "Anchor membership", Value = "No anchor set is defined for generated items"),
    rownames = FALSE, striped = TRUE)
  selected_profiles <- reactive(profiles[profiles$item_id == selected_item()$item_id, ])
  output$option_plot <- renderPlot({
    x <- selected_profiles()
    ggplot(x, aes(rest_score_group, proportion, color = option, group = option)) +
      geom_line(linewidth = 1) + geom_point(size = 2) +
      scale_y_continuous(limits = c(0, 1)) +
      scale_color_manual(values = c(A = "#167c81", B = "#dc9250", C = "#6658a8", D = "#66842f")) +
      labs(x = "Rest score group", y = "Proportion choosing option", color = "Option") + theme_plot
  })
  output$option_table <- renderTable(show_table(selected_profiles(), 20), rownames = FALSE, striped = TRUE)
  output$item_register <- renderTable(show_table(filtered_items(), 20), rownames = FALSE, striped = TRUE)
  output$download_filtered <- downloadHandler(
    filename = function() "generated_items_filtered.csv",
    content = function(file) write.csv(filtered_items(), file, row.names = FALSE))
  output$download_ctt <- downloadHandler(
    filename = function() "generated_ctt_results.csv",
    content = function(file) write.csv(filtered_items()[, c("item_id", "wave", "form", "domain",
      "sample_n", "key", "facility", "rest_r", "review_flag")], file, row.names = FALSE))
  output$download_irt <- downloadHandler(
    filename = function() "generated_irt_simulation_inputs.csv",
    content = function(file) write.csv(filtered_items()[, c("item_id", "wave", "form", "domain",
      "simulation_a", "simulation_b", "sample_n")], file, row.names = FALSE))
  output$download_irt_pairs <- downloadHandler(
    filename = function() paste0(selected_item()$item_id, "_residual_pairs.csv"),
    content = function(file) write.csv(selected_pairs(), file, row.names = FALSE))
  output$download_nominal <- downloadHandler(
    filename = function() "generated_nominal_illustrations.csv",
    content = function(file) write.csv(filtered_items()[, c("item_id", "wave", "form", "domain",
      "key", "simulation_a", "simulation_b")], file, row.names = FALSE))
  output$download_nominal_parameters <- downloadHandler(
    filename = function() paste0(selected_item()$item_id, "_option_parameters.csv"),
    content = function(file) write.csv(nominal_parameters(), file, row.names = FALSE))
  output$download_rasch <- downloadHandler(
    filename = function() "generated_rasch_sensitivity_inputs.csv",
    content = function(file) write.csv(transform(filtered_items()[, c("item_id", "wave", "form",
      "domain", "simulation_b")], fixed_discrimination = 1), file, row.names = FALSE))
  selected_evidence <- reactive(evidence[evidence$item_id == selected_item()$item_id, ])
  output$item_evidence <- renderTable(show_table(selected_evidence()), rownames = FALSE, striped = TRUE)
  output$download_item_record <- downloadHandler(
    filename = function() paste0(selected_item()$item_id, "_record.csv"),
    content = function(file) write.csv(selected_item(), file, row.names = FALSE))
  output$download_item_evidence <- downloadHandler(
    filename = function() paste0(selected_item()$item_id, "_evidence.csv"),
    content = function(file) write.csv(selected_evidence(), file, row.names = FALSE))
  output$download_nominal_pdf <- downloadHandler(
    filename = function() "nominal_option_curves_demo.pdf",
    content = function(file) file.copy("data/atlases/nominal_option_curves_demo.pdf", file))
  output$download_empirical_pdf <- downloadHandler(
    filename = function() "empirical_option_profiles_demo.pdf",
    content = function(file) file.copy("data/atlases/empirical_option_profiles_demo.pdf", file))
  output$download_note <- downloadHandler(
    filename = function() paste0(selected_item()$item_id, "_review_note.csv"),
    content = function(file) write.csv(data.frame(item_id = selected_item()$item_id,
      reviewer_alias = input$reviewer, decision = input$decision,
      rationale = input$rationale), file, row.names = FALSE))
  output$evidence_plot <- renderPlot({
    ggplot(items, aes(facility, rest_r, color = review_flag)) + geom_point(size = 2.5, alpha = 0.8) +
      scale_color_manual(values = c("Routine" = "#97a8b6", "Low facility" = "#bb634d",
                                    "High facility" = "#dc9250", "Low item-rest association" = "#6658a8")) +
      labs(x = "Facility", y = "Item-rest association", color = "Illustrative flag") + theme_plot
  })
  filtered_evidence <- reactive({
    if (is.null(input$reason) || input$reason == "All") evidence else
      evidence[evidence$illustrative_reason == input$reason, ]
  })
  output$evidence_table <- renderTable(show_table(filtered_evidence(), 25), rownames = FALSE, striped = TRUE)
  output$download_evidence <- downloadHandler(
    filename = function() "generated_review_examples.csv",
    content = function(file) write.csv(filtered_evidence(), file, row.names = FALSE))
  observe({
    req(input$result_topic)
    x <- result_catalogue[result_catalogue$topic_id == as.integer(input$result_topic), ]
    if (isTRUE(input$report_tables_only)) x <- x[x$report_table, ]
    updateSelectInput(session, "result_key",
                      choices = setNames(x$key, paste(x$key, x$title)))
  })
  output$topic_definitions <- renderTable({
    data.frame(Topic = topic_names,
      Scope = c("Generated item performance summaries",
                "Simulated response-option choices and traces",
                "Response curves, residual pairs, information and local-error summaries",
                "Invented score conversions and threshold scenarios",
                "Illustrative item-review queues and rules"),
      Interpretation = "Synthetic showcase only; no study estimates")
  }, rownames = FALSE, striped = TRUE)
  selected_result_meta <- reactive({
    req(input$result_key)
    x <- result_catalogue[result_catalogue$key == input$result_key, ]
    req(nrow(x) == 1)
    x
  })
  selected_result <- reactive({
    m <- selected_result_meta()
    read.csv(m$path, stringsAsFactors = FALSE, check.names = FALSE)
  })
  output$result_context <- renderUI({
    x <- selected_result_meta()
    p(tags$strong(x$key), " · ", x$topic, " · ", nrow(selected_result()),
      " generated rows. Every value is synthetic.")
  })
  output$result_table <- renderTable(show_table(selected_result(), 30),
                                      rownames = FALSE, striped = TRUE)
  output$download_result <- downloadHandler(
    filename = function() basename(selected_result_meta()$path),
    content = function(file) file.copy(selected_result_meta()$path, file))
  selected_figure <- reactive({
    req(input$figure_file)
    x <- figure_manifest[figure_manifest$file == input$figure_file, ]
    req(nrow(x) == 1L)
    x
  })
  output$figure_view <- renderUI({
    x <- selected_figure()
    tags$img(src = paste0("figures/", basename(x$file)), alt = x$title,
             style = "width:100%;height:auto;border:1px solid #dde6ed;border-radius:6px")
  })
  output$download_figure <- downloadHandler(
    filename = function() basename(selected_figure()$file),
    content = function(file) file.copy(selected_figure()$file, file))
  output$result_plot <- renderPlot({
    x <- selected_result()
    numeric_columns <- names(x)[vapply(x, is.numeric, logical(1))]
    if (length(numeric_columns) >= 2L)
      return(ggplot(x, aes(x = .data[[numeric_columns[1L]]],
                           y = .data[[numeric_columns[2L]]])) +
               geom_point(color = "#167c81", alpha = 0.75, size = 2) +
               labs(x = numeric_columns[1L], y = numeric_columns[2L]) + theme_plot)
    if (length(numeric_columns) == 1L) {
      x$demo_row <- seq_len(nrow(x))
      return(ggplot(x, aes(x = demo_row, y = .data[[numeric_columns[1L]]])) +
               geom_line(color = "#167c81", linewidth = 1) +
               labs(x = "Synthetic row", y = numeric_columns[1L]) + theme_plot)
    }
    x$demo_label <- as.character(x[[1L]])
    ggplot(utils::head(x, 15), aes(x = demo_label)) +
      geom_bar(fill = "#167c81") + coord_flip() +
      labs(x = names(x)[1L], y = "Generated rows") + theme_plot
  })
  selected_curve <- reactive({
    req(input$precision_wave, input$precision_form, input$precision_domain)
    curves[curves$wave == input$precision_wave & curves$form == input$precision_form &
             curves$domain == input$precision_domain, ]
  })
  precision_display <- reactive({
    x <- selected_curve()
    x$model <- "Generated 2PL"
    if (isTRUE(input$precision_sensitivity)) {
      subset <- items[items$wave == input$precision_wave & items$form == input$precision_form &
                        items$domain == input$precision_domain, ]
      support <- x
      support$information <- vapply(support$theta, function(theta) {
        p <- plogis(theta - subset$simulation_b)
        sum(p * (1 - p))
      }, numeric(1))
      support$local_se <- 1 / sqrt(support$information)
      support$model <- "Unit-slope illustration"
      x <- rbind(x, support)
    }
    x
  })
  output$precision_plot <- renderPlot({
    x <- precision_display()
    measure <- input$precision_measure
    ggplot(x, aes(theta, .data[[measure]], color = model)) + geom_line(linewidth = 1.15) +
      scale_color_manual(values = c("Generated 2PL" = "#167c81", "Unit-slope illustration" = "#dc9250")) +
      labs(x = "Simulated ability", y = if (measure == "information") "Test information" else "Local standard error",
           title = paste(input$precision_wave, input$precision_form, input$precision_domain)) + theme_plot
  })
  output$precision_table <- renderTable(show_table(precision_display(), 12), rownames = FALSE, striped = TRUE)
  output$download_curve <- downloadHandler(
    filename = function() "generated_information_curve.csv",
    content = function(file) write.csv(precision_display(), file, row.names = FALSE))
  score_values <- reactive({
    req(input$score_form)
    a <- 100 * input$raw_a / 12
    b <- 100 * input$raw_b / 12
    exponent <- c("Form 1" = 0.90, "Form 2" = 0.94, "Form 3" = 0.98)[[input$score_form]]
    data.frame(Scenario = c("Linear", "Illustrative curved"),
               `Domain A` = round(c(a, 100 * (input$raw_a / 12)^exponent), 1),
               `Domain B` = round(c(b, 100 * (input$raw_b / 12)^exponent), 1),
               check.names = FALSE) |>
      transform(Overall = round((`Domain A` + `Domain B`) / 2, 1),
                Classification = ifelse((`Domain A` + `Domain B`) / 2 >= input$cut,
                                        "Pass", "Below threshold"))
  })
  output$score_a <- renderText(sprintf("%.1f", score_values()[["Domain A"]][1]))
  output$score_b <- renderText(sprintf("%.1f", score_values()[["Domain B"]][1]))
  output$score_total <- renderText(sprintf("%.1f", score_values()$Overall[1]))
  output$score_class <- renderText(score_values()$Classification[1])
  output$score_table <- renderTable(score_values(), rownames = FALSE, striped = TRUE)
  output$score_plot <- renderPlot({
    req(input$score_form)
    exponent <- c("Form 1" = 0.90, "Form 2" = 0.94, "Form 3" = 0.98)[[input$score_form]]
    x <- data.frame(raw = rep(0:12, 2), score = c(100 * (0:12) / 12,
      100 * ((0:12) / 12)^exponent),
      scenario = rep(c("Linear", "Illustrative curved"), each = 13))
    ggplot(x, aes(raw, score, color = scenario)) + geom_line(linewidth = 1.1) +
      geom_point(size = 1.5) + geom_hline(yintercept = input$cut, linetype = "dashed") +
      scale_color_manual(values = c("Linear" = "#167c81", "Illustrative curved" = "#dc9250")) +
      labs(x = "Raw score in one fictional domain", y = "Illustrative reported score", color = "Scenario",
           title = input$score_form) + theme_plot
  })
  selected_file <- reactive({ req(input$demo_file); demo_files[[input$demo_file]] })
  output$demo_preview <- renderTable({
    show_table(read.csv(selected_file(), stringsAsFactors = FALSE), 15)
  }, rownames = FALSE, striped = TRUE)
  output$download_file <- downloadHandler(
    filename = function() basename(selected_file()),
    content = function(file) file.copy(selected_file(), file))
  filtered_catalogue <- reactive({
    x <- result_catalogue[, c("key", "topic", "title")]
    q <- trimws(input$catalogue_search %||% "")
    if (nzchar(q)) x <- x[grepl(q, x$key, ignore.case = TRUE, fixed = TRUE) |
                            grepl(q, x$topic, ignore.case = TRUE, fixed = TRUE), ]
    x
  })
  output$catalogue_table <- renderTable(show_table(filtered_catalogue(), 35),
                                         rownames = FALSE, striped = TRUE)
  output$download_catalogue <- downloadHandler(
    filename = function() "synthetic_result_catalogue.csv",
    content = function(file) write.csv(filtered_catalogue(), file, row.names = FALSE))
}

`%||%` <- function(x, y) if (is.null(x)) y else x
shinyApp(ui, server)
