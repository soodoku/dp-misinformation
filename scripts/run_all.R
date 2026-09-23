purrr::walk(list.files("R", full.names = TRUE), source)

verify_sources()
dir.create("tabs", showWarnings = FALSE)
write_output <- \(x, name) readr::write_csv(x, file.path("tabs", name), na = "")

panel <- item_panel()
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

write_output(controlled_effects(fetch_control_files()), "controlled_effects.csv")
