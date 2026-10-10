PROJECT_ROOT <- rprojroot::find_root(rprojroot::has_file("DESCRIPTION"))
project_file <- function(...) file.path(PROJECT_ROOT, ...)

RAW_DIR <- project_file("data", "raw")
CACHE_DIR <- project_file("data", "cache")
DERIVED_DIR <- project_file("data", "derived")
TABLE_DIR <- project_file("tabs")
FIGURE_DIR <- project_file("figs")
ITEMS_FILE <- project_file("docs", "items.csv")
CONTROL_MANIFEST <- project_file("data", "control_files.csv")
PREPARED_DATA_FILE <- file.path(DERIVED_DIR, "prepared_data.rds")

RAW_FILES <- c(
  aus.csv = "c9cb05f1fb001335dcd846e50d91f4047c2d9fef888438b2a3456eb524ffb2c5",
  btp04GE.csv = "9db2f15672b73e74523e12cab74b55d8057ff396d28d968b943fb4ccf318f9e3",
  dk.csv = "b9d9892b5e0c406ae77c41ef54cb82479bb55506c5c806332f22f3029d1a2cbb",
  ukbge.csv = "8d371a1174d89df3ddb207e1e9e2242416a1753a5fd0e1d6aac0ebf8de91a2c5",
  ukcrime.csv = "228425f12056cd9e9b8770943433958545e1fb5667191a6d63de5dcd1268412e",
  ukeu.csv = "9077fdea79264d77748337d3b8bb7197865391bb48d5850a9e060be761905a22",
  ukhealth.csv = "671bf82144935f33351d627b7996c05b91585f7adf192734b94a2e655ad960b2"
)

ITEM_LABELS <- c(
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

COLORS <- c(wrong = "#B2182B", dont_know = "grey45", reference = "grey60")
FIGURE_SIZES <- list(
  item_changes = c(width = 6.5, height = 7.8),
  transitions = c(width = 6, height = 2.8),
  controlled = c(width = 6.5, height = 3.6)
)
FIGURE_DPI <- 180

theme_paper <- function(base_size = 11, base_family = "sans") {
  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold"),
      plot.caption = ggplot2::element_text(hjust = 0),
      plot.title.position = "plot"
    )
}

TABLE_STYLE <- list(
  size = "small",
  column_sep = "6pt",
  row_spacing = 1,
  items_align = "lp{10cm}c"
)
