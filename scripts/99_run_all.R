source("scripts/00_config.R")
source(project_file("scripts", "00_utils.R"))

for (path in c(DERIVED_DIR, TABLE_DIR, FIGURE_DIR)) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
}

for (script in c("01_prepare_data.R", "02_estimate.R", "03_figures.R", "04_tables.R")) {
  message("Running scripts/", script)
  source(project_file("scripts", script), local = new.env(parent = globalenv()))
}
