# Share answering incorrectly before and after, and the paired change.
item_changes <- function(panel) {
  panel |>
    dplyr::mutate(before = as.numeric(t1 == "incorrect"), after = as.numeric(t2 == "incorrect")) |>
    dplyr::group_by(poll, item, statement) |>
    dplyr::summarise(
      n = dplyr::n(),
      incorrect_t1 = mean(before), incorrect_t2 = mean(after),
      dk_t1 = mean(t1 == "dk"), dk_t2 = mean(t2 == "dk"),
      correct_t1 = mean(t1 == "correct"), correct_t2 = mean(t2 == "correct"),
      change = mean(after - before), std_error = stats::sd(after - before) / sqrt(n),
      .groups = "drop"
    ) |>
    dplyr::mutate(lower = change - qnorm(0.975) * std_error, upper = change + qnorm(0.975) * std_error)
}

transitions <- function(panel) {
  panel |>
    dplyr::count(t1, t2) |>
    dplyr::group_by(t1) |>
    dplyr::mutate(share = n / sum(n), from = sum(n)) |>
    dplyr::ungroup()
}

# Where do those who start wrong end up, compared with those who start
# without an answer? Linear probability with item fixed effects; standard
# errors clustered by respondent, who answered several items.
conversion <- function(panel) {
  data <- dplyr::filter(panel, t1 %in% c("incorrect", "dk")) |>
    dplyr::mutate(started_wrong = as.numeric(t1 == "incorrect"), key = paste(poll, item))
  purrr::map(c("correct", "dk", "incorrect"), \(end) {
    data$outcome <- as.numeric(data$t2 == end)
    fit <- lm(outcome ~ started_wrong + key, data = data)
    se <- sqrt(sandwich::vcovCL(fit, cluster = data$respondent, type = "HC1")["started_wrong", "started_wrong"])
    tibble::tibble(
      end = end, from_dk = mean(data$outcome[data$started_wrong == 0]),
      from_wrong = mean(data$outcome[data$started_wrong == 1]),
      difference = coef(fit)[["started_wrong"]], std_error = se,
      lower = difference - qnorm(0.975) * se, upper = difference + qnorm(0.975) * se,
      n_wrong = sum(data$started_wrong), n_dk = sum(1 - data$started_wrong)
    )
  }) |>
    purrr::list_rbind()
}

pooled_change <- function(panel) {
  data <- dplyr::mutate(panel,
    change = as.numeric(t2 == "incorrect") - as.numeric(t1 == "incorrect"),
    key = paste(poll, item)
  )
  fit <- lm(change ~ 1, data = data)
  se <- sqrt(sandwich::vcovCL(fit, cluster = data$respondent, type = "HC1")[1, 1])
  tibble::tibble(
    estimate = coef(fit)[[1]], std_error = se,
    lower = estimate - qnorm(0.975) * se, upper = estimate + qnorm(0.975) * se,
    incorrect_t1 = mean(data$t1 == "incorrect"), incorrect_t2 = mean(data$t2 == "incorrect"),
    items = dplyr::n_distinct(data$key), polls = dplyr::n_distinct(data$poll)
  )
}

# ANCOVA on the randomized (A1R: invited vs uninvited) comparison: the later
# outcome on treatment and the baseline outcome.
ancova <- function(data, outcome, baseline, weights = NULL) {
  data <- dplyr::filter(data, !is.na(.data[[outcome]]), !is.na(.data[[baseline]]))
  w <- if (is.null(weights)) NULL else data[[weights]]
  fit <- lm(reformulate(c("treated", baseline), outcome), data = data, weights = w)
  se <- sqrt(sandwich::vcovHC(fit, type = "HC1")["treated", "treated"])
  tibble::tibble(
    estimate = coef(fit)[["treated"]], std_error = se,
    lower = estimate - qnorm(0.975) * se, upper = estimate + qnorm(0.975) * se,
    treated_before = mean(data[[baseline]][data$treated == 1]),
    treated_after = mean(data[[outcome]][data$treated == 1]),
    control_before = mean(data[[baseline]][data$treated == 0]),
    control_after = mean(data[[outcome]][data$treated == 0]),
    n_treated = sum(data$treated == 1), n_control = sum(data$treated == 0)
  )
}

controlled_effects <- function(data) {
  a1r <- data$a1r |>
    dplyr::mutate(
      wrong1 = as.numeric(t1 == "incorrect"), wrong2 = as.numeric(t2 == "incorrect"),
      right1 = as.numeric(t1 == "correct"), right2 = as.numeric(t2 == "correct")
    )
  climate <- data$climate |>
    dplyr::mutate(
      deny1 = as.numeric(r1 == 0), deny2 = as.numeric(r2 == 0), deny3 = as.numeric(r3 == 0),
      lenient1 = as.numeric(r1 <= 1), lenient2 = as.numeric(r2 <= 1), lenient3 = as.numeric(r3 <= 1)
    )
  climate_study <- "America in One Room: Climate 2021"
  specs <- tibble::tribble(
    ~data_name, ~study, ~measure, ~wave, ~outcome, ~baseline,
    "a1r", "America in One Room 2019", "Overestimates undocumented immigrants", "After", "wrong2", "wrong1",
    "a1r", "America in One Room 2019", "Right number of undocumented immigrants", "After", "right2", "right1",
    "climate", climate_study, "Denies human-caused warming (0)", "After", "deny2", "deny1",
    "climate", climate_study, "Denies human-caused warming (0-1)", "After", "lenient2", "lenient1",
    "climate", climate_study, "Denies human-caused warming (0)", "A year later", "deny3", "deny1",
    "climate", climate_study, "Denies human-caused warming (0-1)", "A year later", "lenient3", "lenient1",
    "climate", climate_study, "Agreement, 0-10", "After", "r2", "r1",
    "climate", climate_study, "Agreement, 0-10", "A year later", "r3", "r1"
  )
  data <- list(a1r = a1r, climate = climate)
  purrr::pmap(specs, \(data_name, study, measure, wave, outcome, baseline) {
    dplyr::bind_rows(
      ancova(data[[data_name]], outcome, baseline) |> dplyr::mutate(weighting = "unweighted"),
      ancova(data[[data_name]], outcome, baseline, "weight") |> dplyr::mutate(weighting = "weighted")
    ) |>
      dplyr::mutate(study = study, measure = measure, wave = wave, .before = 1)
  }) |>
    purrr::list_rbind()
}

write_output <- \(x, name) readr::write_csv(x, file.path(TABLE_DIR, name), na = "")

prepared <- readRDS(PREPARED_DATA_FILE)
panel <- prepared$panel
write_output(item_changes(panel), "item_changes.csv")
write_output(transitions(panel), "transitions.csv")
write_output(conversion(panel), "conversion.csv")
write_output(pooled_change(panel), "pooled_change.csv")
panel |>
  dplyr::group_by(poll) |>
  dplyr::summarise(
    answers = dplyr::n(), respondents = dplyr::n_distinct(respondent), items = dplyr::n_distinct(item)
  ) |>
  write_output("polls.csv")

write_output(controlled_effects(prepared), "controlled_effects.csv")
