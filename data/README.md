# Data

| File | Contents | Source | License |
|---|---|---|---|
| `raw/ukcrime.csv`, `raw/ukeu.csv`, `raw/ukbge.csv`, `raw/ukhealth.csv`, `raw/aus.csv`, `raw/dk.csv`, `raw/btp04GE.csv` | Participants' answers to each knowledge item at the start and end of seven Deliberative Polls, coded 1 (correct), 0 (incorrect), or missing (don't know or no answer) | Cor and Sood, *Guessing and Forgetting* replication data, [doi:10.7910/DVN/HZHVCU](https://doi.org/10.7910/DVN/HZHVCU) (`data.zip`) | CC0 1.0 |
| `control_files.csv` | URLs and SHA-256 hashes for America in One Room 2019 ([doi:10.7910/DVN/KJ8IH2](https://doi.org/10.7910/DVN/KJ8IH2)) and its 2021 climate edition ([doi:10.7910/DVN/IIOG1S](https://doi.org/10.7910/DVN/IIOG1S)), downloaded into `cache/` | Harvard Dataverse | CC0 1.0 |

`R/sources.R` checks every file against its hash before any analysis.

The column for each item, its wording, and the source for its key are in
`docs/items.csv`. The public columns were matched to the questionnaire items by
comparing their counts of correct, incorrect, and missing answers with the
original files. Items dropped as ambiguous, and items and control groups not in
the public release, are listed in `docs/exclusions.csv`.
