verify_sources <- function() {
  found <- purrr::map_chr(names(RAW_FILES), \(f) digest::digest(file = file.path(RAW_DIR, f), algo = "sha256"))
  bad <- names(RAW_FILES)[found != RAW_FILES]
  if (length(bad) > 0) stop("Hash mismatch: ", paste(bad, collapse = ", "))
  invisible(TRUE)
}

# The two controlled polls are downloaded from Dataverse into an ignored cache
# and checked against the recorded hashes.
fetch_control_files <- function(manifest = CONTROL_MANIFEST, cache = CACHE_DIR) {
  files <- read_strict_csv(manifest)
  dir.create(cache, recursive = TRUE, showWarnings = FALSE)
  paths <- file.path(cache, files$file)
  purrr::walk2(files$url, paths, \(url, path) {
    if (!file.exists(path)) utils::download.file(url, path, mode = "wb", quiet = TRUE)
  })
  observed <- purrr::map_chr(paths, \(path) digest::digest(file = path, algo = "sha256"))
  if (!identical(observed, files$sha256)) {
    stop("Checksum mismatch: ", paste(files$file[observed != files$sha256], collapse = ", "))
  }
  rlang::set_names(paths, tools::file_path_sans_ext(files$file))
}

item_panel <- function(items = read_strict_csv(ITEMS_FILE)) {
  purrr::pmap(items, \(poll, file, item, t1, t2, ...) {
    data <- read_strict_csv(file.path(RAW_DIR, file)) |>
      assertr::assert(assertr::in_set(0, 1, allow.na = TRUE), dplyr::all_of(c(t1, t2)))
    tibble::tibble(
      poll = poll, item = item, respondent = paste(poll, seq_len(nrow(data))),
      t1 = answer_state(data[[t1]]), t2 = answer_state(data[[t2]])
    )
  }) |>
    purrr::list_rbind() |>
    dplyr::left_join(dplyr::select(items, poll, item, statement, key), by = c("poll", "item"))
}

# America in One Room 2019: "About how many undocumented immigrants are in the
# US?" 10, 20, 30, or 40 million; about 10 to 11 million lived in the U.S.
# "Couldn't say," skipped, and refused count as not answering.
read_a1r_immigrants <- function(path) {
  data <- readr::read_tsv(path, show_col_types = FALSE)
  state <- \(x) dplyr::case_when(x == 1 ~ "correct", x %in% 2:4 ~ "incorrect", .default = "dk")
  tibble::tibble(
    study = "America in One Room 2019", id = seq_len(nrow(data)), treated = data$CONDITION,
    panel = data$POST == 1,
    t1 = state(data$PK3), t2 = dplyr::if_else(data$POST == 1, state(data$T2PK3), NA_character_),
    weight = dplyr::if_else(data$CONDITION == 1, data$WEIGHT_DELEGATE, data$WEIGHT_CONTROL)
  ) |>
    dplyr::filter(panel)
}

# America in One Room: Climate 2021. "Rising temperatures are caused by human
# activities that emit greenhouse gases," 0 (strongly disagree) to 10
# (strongly agree). Confident denial is a rating of 0 (strict) or 0-1
# (lenient); "couldn't say" and skipped count as not answering.
read_climate_denial <- function(path) {
  data <- readr::read_tsv(path, show_col_types = FALSE)
  rating <- \(x) dplyr::if_else(x %in% 0:10, as.numeric(x), NA_real_)
  attended <- data$P_DELEGATE == 1
  control <- data$P_TREATMENT == 0 & data$P_DELEGATE == 0
  tibble::tibble(
    study = "America in One Room: Climate 2021", id = seq_len(nrow(data)),
    treated = as.numeric(data$P_TREATMENT == 1), panel = attended | control,
    r1 = rating(data$Q1B), r2 = rating(data$T2Q1B), r3 = rating(data$T3Q1B),
    weight = data$WEIGHT1
  ) |>
    dplyr::filter(panel)
}

verify_sources()
paths <- fetch_control_files()
prepared <- list(
  panel = item_panel(),
  a1r = read_a1r_immigrants(paths[["a1r"]]),
  climate = read_climate_denial(paths[["climate"]])
)
saveRDS(prepared, PREPARED_DATA_FILE)
