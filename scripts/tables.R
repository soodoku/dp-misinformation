purrr::walk(list.files("R", full.names = TRUE), source)

changes <- read_tab("item_changes.csv")
pooled <- read_tab("pooled_change.csv")
conv <- read_tab("conversion.csv")
polls <- read_tab("polls.csv")
effects <- read_tab("controlled_effects.csv")
trans <- read_tab("transitions.csv")

conv_row <- \(e) conv[conv$end == e, ]
eff <- \(m, w, weighting = "unweighted") {
  effects[effects$measure == m & effects$wave == w & effects$weighting == weighting, ]
}
from_correct_wrong <- trans$share[trans$t1 == "correct" & trans$t2 == "incorrect"]
item <- \(i) changes[changes$item == i, ]
imm <- eff("Overestimates undocumented immigrants", "After")
right <- eff("Right number of undocumented immigrants", "After")
deny2 <- eff("Denies human-caused warming (0)", "After")
deny3 <- eff("Denies human-caused warming (0)", "A year later")
agree2 <- eff("Agreement, 0-10", "After")
agree3 <- eff("Agreement, 0-10", "A year later")
signed <- \(x) sprintf("%+d", round(100 * x))

values <- c(
  nItems = pooled$items, nPolls = pooled$polls, nRespondents = count(sum(polls$respondents)),
  nAnswers = count(sum(polls$answers)),
  wrongBefore = pct(pooled$incorrect_t1), wrongAfter = pct(pooled$incorrect_t2),
  pooledChange = pts(-pooled$estimate), pooledLower = pts(-pooled$upper), pooledUpper = pts(-pooled$lower),
  nFell = sum(changes$upper < 0), nRose = sum(changes$lower > 0),
  wrongToRight = pct(conv_row("correct")$from_wrong), dkToRight = pct(conv_row("correct")$from_dk),
  rightDiff = signed(conv_row("correct")$difference), rightDiffLower = signed(conv_row("correct")$lower),
  rightDiffUpper = signed(conv_row("correct")$upper),
  wrongToWrong = pct(conv_row("incorrect")$from_wrong), dkToWrong = pct(conv_row("incorrect")$from_dk),
  wrongToDk = pct(conv_row("dk")$from_wrong), dkToDk = pct(conv_row("dk")$from_dk),
  nStartWrong = count(conv_row("correct")$n_wrong), nStartDk = count(conv_row("correct")$n_dk),
  rightToWrong = pct(from_correct_wrong),
  flagBefore = pct(item("flag")$incorrect_t1), flagAfter = pct(item("flag")$incorrect_t2),
  screeningBefore = pct(item("breast_screening")$incorrect_t1),
  screeningAfter = pct(item("breast_screening")$incorrect_t2),
  iraqBefore = pct(item("iraq_911")$incorrect_t1), iraqAfter = pct(item("iraq_911")$incorrect_t2),
  brusselsBefore = pct(item("brussels_tax")$incorrect_t1), brusselsAfter = pct(item("brussels_tax")$incorrect_t2),
  immEffect = pts(-imm$estimate), immLower = pts(-imm$upper), immUpper = pts(-imm$lower),
  immTreatedBefore = pct(imm$treated_before), immTreatedAfter = pct(imm$treated_after),
  immControlBefore = pct(imm$control_before), immControlAfter = pct(imm$control_after),
  immWeighted = pts(-eff("Overestimates undocumented immigrants", "After", "weighted")$estimate),
  rightTreatedBefore = pct(right$treated_before), rightTreatedAfter = pct(right$treated_after),
  nImmTreated = imm$n_treated, nImmControl = imm$n_control,
  denyTreatedBefore = pct(deny2$treated_before, 1), denyTreatedAfter = pct(deny2$treated_after, 1),
  denyControlBefore = pct(deny2$control_before, 1), denyControlAfter = pct(deny2$control_after, 1),
  denyEffect = sprintf("%.1f", -100 * deny2$estimate), denyLower = sprintf("%.1f", -100 * deny2$upper),
  denyUpper = sprintf("%.1f", -100 * deny2$lower),
  denyLaterEffect = sprintf("%.1f", -100 * deny3$estimate), denyLaterLower = sprintf("%.1f", -100 * deny3$upper),
  denyLaterUpper = sprintf("%.1f", -100 * deny3$lower),
  nDenyTreated = count(deny2$n_treated), nDenyControl = deny2$n_control,
  nDenyLaterTreated = deny3$n_treated, nDenyLaterControl = deny3$n_control,
  agreeEffect = sprintf("%.1f", agree2$estimate), agreeLaterEffect = sprintf("%.1f", agree3$estimate),
  agreeLaterLower = sprintf("%.1f", agree3$lower), agreeLaterUpper = sprintf("%.1f", agree3$upper)
)
write_macros(values, "tabs/macros.tex")

items <- read_strict_csv(file.path("docs", "items.csv"))
items |>
  dplyr::mutate(
    poll = dplyr::if_else(duplicated(poll), "", poll),
    statement = latex_escape(statement),
    key = dplyr::if_else(key, "T", "F")
  ) |>
  dplyr::select(poll, statement, key) |>
  write_table("tabs/items.tex", "lp{10cm}c", c("Poll", "Statement", "Key"))
