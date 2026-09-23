raw_files <- c(
  aus.csv = "c9cb05f1fb001335dcd846e50d91f4047c2d9fef888438b2a3456eb524ffb2c5",
  btp04GE.csv = "9db2f15672b73e74523e12cab74b55d8057ff396d28d968b943fb4ccf318f9e3",
  dk.csv = "b9d9892b5e0c406ae77c41ef54cb82479bb55506c5c806332f22f3029d1a2cbb",
  ukbge.csv = "8d371a1174d89df3ddb207e1e9e2242416a1753a5fd0e1d6aac0ebf8de91a2c5",
  ukcrime.csv = "228425f12056cd9e9b8770943433958545e1fb5667191a6d63de5dcd1268412e",
  ukeu.csv = "9077fdea79264d77748337d3b8bb7197865391bb48d5850a9e060be761905a22",
  ukhealth.csv = "671bf82144935f33351d627b7996c05b91585f7adf192734b94a2e655ad960b2"
)

verify_sources <- function() {
  found <- purrr::map_chr(names(raw_files), \(f) digest::digest(file = file.path("data", "raw", f), algo = "sha256"))
  bad <- names(raw_files)[found != raw_files]
  if (length(bad) > 0) stop("Hash mismatch: ", paste(bad, collapse = ", "))
  invisible(TRUE)
}

read_strict_csv <- function(path, ...) {
  data <- readr::read_csv(path, show_col_types = FALSE, ...)
  if (nrow(readr::problems(data)) > 0) stop("Parsing problems in ", path)
  data
}

# The two controlled polls are downloaded from Dataverse into an ignored cache
# and checked against the recorded hashes.
fetch_control_files <- function(manifest = file.path("data", "control_files.csv"), cache = file.path("data", "cache")) {
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

# The Cor and Sood release scores each answer 1 (correct), 0 (incorrect), or
# missing (don't know or no answer); rows are participants in both waves.
answer_state <- function(x) {
  dplyr::case_when(x == 1 ~ "correct", x == 0 ~ "incorrect", is.na(x) ~ "dk") |>
    factor(levels = c("correct", "dk", "incorrect"))
}

item_panel <- function(items = read_strict_csv(file.path("docs", "items.csv"))) {
  purrr::pmap(items, \(poll, file, item, t1, t2, ...) {
    data <- read_strict_csv(file.path("data", "raw", file)) |>
      assertr::assert(assertr::in_set(0, 1, allow.na = TRUE), dplyr::all_of(c(t1, t2)))
    tibble::tibble(
      poll = poll, item = item, respondent = paste(poll, seq_len(nrow(data))),
      t1 = answer_state(data[[t1]]), t2 = answer_state(data[[t2]])
    )
  }) |>
    purrr::list_rbind() |>
    dplyr::left_join(dplyr::select(items, poll, item, statement, key), by = c("poll", "item"))
}
