# Run from the public-demo folder before committing or deploying.
manifest <- read.csv("data/table_manifest.csv", stringsAsFactors = FALSE,
                     check.names = FALSE)
if (nrow(manifest) != 36L || anyDuplicated(manifest$key) ||
    anyDuplicated(manifest$path) ||
    !all(grepl("^data/results/topic_0[1-5]/[a-z0-9_]+\\.csv$", manifest$path)) ||
    !all(manifest$report_table %in% c(TRUE, FALSE)))
  stop("The public result manifest is invalid.")
if (!identical(as.integer(table(factor(manifest$topic_id, levels = 1:5))),
               c(6L, 6L, 12L, 6L, 6L)))
  stop("The public result manifest has unexpected topic counts.")
for (i in seq_len(nrow(manifest))) {
  columns <- strsplit(manifest$columns[i], "|", fixed = TRUE)[[1L]]
  kinds <- strsplit(manifest$kinds[i], "|", fixed = TRUE)[[1L]]
  if (length(columns) != manifest$column_count[i] ||
      length(kinds) != length(columns) || anyDuplicated(columns) ||
      !all(grepl("^[a-z0-9_]+$", columns)) ||
      !all(kinds %in% c("identifier", "category", "number", "integer", "date", "text")))
    stop("Unsafe or inconsistent schema in public result manifest: ", manifest$key[i])
}
figure_manifest <- read.csv("data/figure_manifest.csv", stringsAsFactors = FALSE)
if (nrow(figure_manifest) != 15L || anyDuplicated(figure_manifest$file) ||
    !all(grepl("^www/figures/figure_[0-9]{2}\\.png$", figure_manifest$file)) ||
    !all(file.exists(figure_manifest$file)) ||
    !all(file.info(figure_manifest$file)$size > 0L))
  stop("The public figure manifest is invalid.")
allowed <- c(
  ".gitignore", "README.md", "app.R", "generate_demo_data.R",
  "generate_result_tables.R", "generate_figures.R", "generate_atlases.py",
  "data/table_manifest.csv", "data/figure_manifest.csv",
  "verify_public_demo.R", "audit_public_demo.R",
  "data/item_register.csv", "data/option_profiles.csv",
  "data/information_curves.csv", "data/review_examples.csv",
  "data/residual_pairs.csv",
  "data/atlases/nominal_option_curves_demo.pdf",
  "data/atlases/empirical_option_profiles_demo.pdf",
  manifest$path, figure_manifest$file
)
files <- list.files(".", recursive = TRUE, all.files = TRUE,
                    include.dirs = FALSE, no.. = TRUE)
files <- files[!grepl("^\\.git/", files)]
unexpected <- setdiff(files, allowed)
missing <- setdiff(allowed, files)
if (length(unexpected) || length(missing)) {
  stop("Public file manifest mismatch. Unexpected: ", paste(unexpected, collapse = ", "),
       "; missing: ", paste(missing, collapse = ", "))
}
if (any(file.info(files)$isdir)) stop("Unexpected directory in public file manifest.")
if (any(Sys.readlink(files) != "")) stop("Symlinks are not allowed in the public demo.")

text_files <- c("README.md", "app.R", "generate_demo_data.R",
                "generate_result_tables.R", "generate_figures.R", "generate_atlases.py",
                "audit_public_demo.R")
contents <- paste(vapply(text_files, function(path) {
  paste(readLines(path, warn = FALSE), collapse = "\n")
}, character(1)), collapse = "\n")
if (grepl("/Users/|/home/|\\.\\./", contents))
  stop("A private path or legacy asset reference appears in public text.")

for (i in seq_len(nrow(manifest))) {
  actual <- names(read.csv(manifest$path[i], nrows = 0L, check.names = FALSE))
  expected <- strsplit(manifest$columns[i], "|", fixed = TRUE)[[1L]]
  if (!identical(actual, expected))
    stop("Public result CSV has the wrong columns: ", manifest$key[i])
}

items <- read.csv("data/item_register.csv", stringsAsFactors = FALSE)
if (nrow(items) != 144L || !all(grepl("^DEMO-[0-9]{3}$", items$item_id)) ||
    !setequal(items$wave, c("Cycle A", "Cycle B")) ||
    !setequal(items$domain, c("Domain A", "Domain B")) ||
    !setequal(items$form, paste("Form", 1:3)) ||
    !all(items$key %in% LETTERS[1:4])) {
  stop("Generated item data do not match the expected fictional design.")
}
public_assets <- sort(c(list.files("data", full.names = TRUE, recursive = TRUE),
                        list.files("www", full.names = TRUE, recursive = TRUE)))
expected_hashes <- unname(tools::md5sum(public_assets))
old_dir <- getwd()
tmp_dir <- tempfile("demo-verification-")
dir.create(tmp_dir)
dir.create(file.path(tmp_dir, "data"))
setwd(tmp_dir)
source(file.path(old_dir, "generate_demo_data.R"), local = TRUE)
source(file.path(old_dir, "generate_result_tables.R"), local = TRUE)
source(file.path(old_dir, "generate_figures.R"), local = TRUE)
python <- Sys.getenv("DEMO_PYTHON", "python3")
Sys.setenv(ASSESSMENT_DEMO_ROOT = tmp_dir)
atlas_result <- suppressWarnings(system2(python,
  file.path(old_dir, "generate_atlases.py"), stdout = TRUE, stderr = TRUE))
Sys.unsetenv("ASSESSMENT_DEMO_ROOT")
if (!is.null(attr(atlas_result, "status")) && attr(atlas_result, "status") != 0L)
  stop("PDF atlas regeneration failed: ", paste(atlas_result, collapse = "\n"))
regenerated_paths <- sort(c(list.files("data", full.names = TRUE, recursive = TRUE),
                            list.files("www", full.names = TRUE, recursive = TRUE)))
regenerated_hashes <- unname(tools::md5sum(regenerated_paths))
setwd(old_dir)
if (!identical(public_assets, regenerated_paths) ||
    !identical(expected_hashes, regenerated_hashes)) {
  differing <- union(setdiff(public_assets, regenerated_paths),
                     setdiff(regenerated_paths, public_assets))
  if (identical(public_assets, regenerated_paths))
    differing <- public_assets[expected_hashes != regenerated_hashes]
  stop("Public assets differ from a clean generator run: ",
       paste(utils::head(differing, 12L), collapse = ", "))
}
cat("Public demo manifest and generated data checks passed.\n")
