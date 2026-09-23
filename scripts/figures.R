purrr::walk(list.files("R", full.names = TRUE), source)

dir.create("figs", showWarnings = FALSE)
read_tab <- \(name) readr::read_csv(file.path("tabs", name), show_col_types = FALSE)

short <- c(
  life = "More lifers than rest of EC (T)", brussels_tax = "Income tax set in Brussels (F)",
  libdem_eu = "Lib Dems least pro-EU (F)", unemployment = "Unemployment above Germany's (F)",
  nhs_spending = "NHS spending doubled (T)", private_care = "Most use private care (F)",
  breast_screening = "All women screened free (F)", flag = "Flag will change (F)",
  anthem = "Anthem will change (F)", commonwealth_games = "Games participation will change (F)",
  fines = "Could be fined for deficits (T)", interest_rates = "Sets own interest rates (F)",
  taxation = "Sets own tax rates (T)", coins = "Coins have a national side (T)",
  iraq_911 = "Iraq involved in 9/11 (F)", wmd = "WMD found in Iraq (F)",
  drug_prices = "Drugs cost more in Canada (F)", bush_vietnam = "Bush in Texas Air Guard (T)",
  kerry_vietnam = "Kerry decorated in Vietnam (T)"
)
changes <- read_tab("item_changes.csv") |>
  dplyr::mutate(
    label = paste0(short[item], "  ", round(100 * incorrect_t1), "% to ", round(100 * incorrect_t2), "%"),
    poll = factor(poll, levels = unique(poll))
  ) |>
  dplyr::group_by(poll) |>
  dplyr::mutate(label = forcats::fct_reorder(label, change)) |>
  dplyr::ungroup()

change_plot <- ggplot2::ggplot(changes, ggplot2::aes(change, label)) +
  geom_zero() +
  geom_estimate() +
  ggplot2::facet_wrap(~poll, ncol = 1, scales = "free_y", space = "free_y") +
  ggplot2::scale_x_continuous(labels = \(x) sprintf("%+d", round(100 * x))) +
  ggplot2::labs(x = "Change in share wrong (percentage points)", y = NULL) +
  theme_evidence() +
  ggplot2::theme(strip.text = ggplot2::element_text(hjust = 0))
save_evidence(change_plot, "figs/item_changes", width = 6.5, height = 7.8)

state_labels <- c(correct = "Right", dk = "Don't know", incorrect = "Wrong")
flows <- read_tab("transitions.csv") |>
  dplyr::filter(t1 %in% c("incorrect", "dk")) |>
  dplyr::mutate(
    wilson(n, from),
    start = factor(paste("Before:", tolower(state_labels[t1])), levels = c("Before: wrong", "Before: don't know")),
    end = factor(state_labels[t2], levels = rev(state_labels))
  )
flow_plot <- ggplot2::ggplot(flows, ggplot2::aes(estimate, end, colour = start)) +
  geom_estimate(position = ggplot2::position_dodge(width = 0.5)) +
  ggplot2::scale_colour_manual(values = c("Before: wrong" = "#B2182B", "Before: don't know" = "grey45"), name = NULL) +
  ggplot2::scale_x_continuous(labels = scales::label_percent(), limits = c(0, 0.75)) +
  ggplot2::labs(x = "Share of answers, after deliberating", y = "After") +
  theme_evidence() +
  ggplot2::theme(legend.position = "top", legend.justification = "left")
save_evidence(flow_plot, "figs/transitions", width = 6, height = 2.8)

effects <- read_tab("controlled_effects.csv") |>
  dplyr::filter(weighting == "unweighted", !grepl("Agreement", measure)) |>
  dplyr::mutate(label = paste0(measure, ", ", tolower(wave))) |>
  dplyr::mutate(label = factor(label, levels = rev(unique(label))))
effect_plot <- ggplot2::ggplot(effects, ggplot2::aes(estimate, label)) +
  geom_zero() +
  geom_estimate() +
  ggplot2::facet_wrap(~study, ncol = 1, scales = "free_y", space = "free_y") +
  ggplot2::scale_x_continuous(labels = \(x) sprintf("%+d", round(100 * x))) +
  ggplot2::labs(x = "Effect of attending (percentage points)", y = NULL) +
  theme_evidence() +
  ggplot2::theme(strip.text = ggplot2::element_text(hjust = 0))
save_evidence(effect_plot, "figs/controlled", width = 6.5, height = 3.6)
