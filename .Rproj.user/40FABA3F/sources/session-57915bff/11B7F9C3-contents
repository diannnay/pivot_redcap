# PivotRedcapTable

An R package for transforming REDCap pseudo-tables into analyzable format.

## Functions

- `pivot_redcap_table()` — pivots REDCap checkbox/repeating instrument exports from wide format to tidy long/wide format. Handles single-choice and 
  multiple-choice question families.
- `label_from_metadata()` — applies human-readable labels to coded vectors 
  using REDCap metadata.
- `meta_first()` — selects a canonical metadata field from a family of 
  similarly-named REDCap fields.

## Installation

```r
devtools::install_github("diannnay/pivot_redcap")
```

## Usage

```r
library(PivotRedcapTable)

pivoted_df <- pivot_redcap_table(df, families, constant_cols = c("record_id", "redcap_event_name"))
```

## Requirements

- R >= 4.1.0
- Imports: dplyr, tidyr, purrr, stringr, haven
