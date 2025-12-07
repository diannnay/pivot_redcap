#' Pivot REDCap data table into long and wide formats by column families
#'
#' This function takes a REDCap-style dataframe and a list of column families,
#' converts relevant columns to consistent data types, pivots them longer,
#' extracts slot indices and answer indices, and optionally pivots wider based
#' on single or multiple-choice questions. The resulting list of pivoted 
#' families is merged into one dataframe.
#'
#' @param df A data.frame containing the REDCap data.
#' @param families A character vector of family prefixes to pivot.
#' @param constant_cols A character vector of columns to keep unchanged (default: c("record_id", "redcap_event_name")).
#' @param indx_slot Integer specifying which capture from the regex to use for the index (default: 1).
#' @param verbose Logical; if TRUE, prints messages about data type harmonization and pivoting progress (default: FALSE).
#'
#' @return A data.frame where all specified families are pivoted and merged, with consistent types and slot indices extracted.
#'
#' @details
#' The input dataframe `df` must follow the REDCap variable naming conventions for families:
#' 
#' 1. **Family prefix**: the base variable name corresponding to one question. This prefix is shared across multiple variables related to that question.
#' 2. **Index slot**: the first number immediately following the family prefix, indicating the slot or instance number for the question.
#' 3. **Answer option**: for multiple-choice questions, the second number in the variable name corresponds to the answer choice.
#' 4. **Checkbox code**: if present, a third number may correspond to a checkbox code (currently ignored by this function).
#' 
#' Examples of valid variable names:
#' - `"eval_type_1"` or `"eval_type1"` : single-choice question, first slot
#' - `"eval_type_1_2"` or `"eval_type1_2"`: multiple-choice question, first slot, second answer option
#' - `"eval_type_1_2_3"` or `"eval_type1_2_3"`: multiple-choice with checkbox code (ignored)
#'
#' @examples
#' \dontrun{
#' pivoted_df <- pivot_redcap_table(sample_2_df.csv, families_100mmc)
#' }
#'
#' @export 
pivot_redcap_table<- function(df, families, constant_cols = c( "record_id", "redcap_event_name") , indx_slot =1, verbose =FALSE ) {  

  # Detect column families -------------------
column_family_map<- list()
  for (i in seq_along(families) ) {
    family_name <- families[[i]]
    obj_name <- paste0(families[[i]])  
  
  list_family_cols <- grep(
    pattern = paste0("^", families[[i]], "[\\0-9_]+$"),
    x = names(df),
    value = TRUE
  )        
  column_family_map[[obj_name]] <- list_family_cols
  }

# guarding against erroring on incomplete data:
for (family_name in names(column_family_map)) {
  cols <- column_family_map[[family_name]]
    has_columns <- length(cols) > 0
    if (!has_columns) {warning(paste0("Family '", family_name, "' has no columns."))   }
}
column_family_map <- column_family_map[sapply(column_family_map, length) > 0]

# --- Index handling -----------------------------------------------------
# gather all slot indices present in any family
indx_slot <- 1
index_pattern <- function (family_prefix) {paste0(
  "^", family_prefix, # E.g., ^eval_type
  "_?", # Make the underscore immediately following the prefix OPTIONAL
  "(\\d+)",     # CAPTURE 1 (Mandatory) < -  index_slot
  "(?:_", "(\\d+)", ")?", # CAPTURE 2 (Optional ) < - answer index for multiple choice questions.
  "(?:_", "(\\d+)", ")?"  # CAPTURE 3 (Optional)< - will be ignored for now and can expand incase of aditional checkboxes
)}
extract_idx_scalar <- function(x, family_prefix) {
  m <- stringr::str_match(x, index_pattern(family_prefix))
  m[, indx_slot + 1]
}
extract_answer_index <- function(x, family_prefix) {
  m <- stringr::str_match(x, index_pattern(family_prefix))
  m[, indx_slot + 2]
}

# harmonize data types (especially for across missing columns)-------------------------------------------
pick_type <- function(x) {
  if (is.factor(x)) return("factor")
  if (is.character(x)) return("character")
  if (is.numeric(x)) return("numeric")   # includes integer
  if (is.logical(x)) return("logical")
  return("character")  }

for (family_key in names(column_family_map)) {
  # Modifying data types one group at a time:
  fam_cols <- intersect(column_family_map[[family_key]], names(df))
  if (length(fam_cols) == 0) next
  # --- find first column with actual data ---
  first_with_data <- NULL
  for (col in fam_cols) {
    if (any(!is.na(df[[col]]))) {
      first_with_data <- col
      break
    }
  }
  if (is.null(first_with_data)) {
    warning("Family '", family_key, "' has NO data in any column — skipping.")
    next
  }
  # determine target type
  raw_type <- pick_type(df[[first_with_data]])
  target <- if (raw_type == "logical") "character" else raw_type
  
  if (verbose) message("Family '", family_key, 
                       "': first column with data = ", first_with_data,
                       " → target = ", target)
  # convert all columns to the target type
  for (col in fam_cols) {
    before <- class(df[[col]])[1]
    
    if (target == "factor") {
      df[[col]] <- as.factor(df[[col]])
    } else if (target == "character") {
      df[[col]] <- as.character(df[[col]])
    } else if (target == "numeric") {
      df[[col]] <- suppressWarnings(as.numeric(as.character(df[[col]])))
    }
    after <- class(df[[col]])[1]
    if (verbose) message("  - ", col, ": ", before, " → ", after)
  }
}
# --- PIVOTING  FAMILIES--------------------------------

pivoted_families_list <- list()
   for (i in seq_along(column_family_map)) {
    family_name    <- names(column_family_map)[i]
    family_colnames<-column_family_map[[i]]
    family_prefix  <-family_name
    
    pivoted_family_df <-df %>%
      dplyr::select(constant_cols, all_of(family_colnames) ) %>%
      tidyr::pivot_longer(
        cols = dplyr::all_of(family_colnames),
        names_to = "col_name",
        values_to = "value"    ) %>% #  This will be either 1/0 var for multiple choices, text or NA for text or drop down fields, 
# extract index: 
      dplyr::mutate(index = extract_idx_scalar(col_name, .env$family_prefix)) %>%
      dplyr::select(constant_cols,index, col_name, value)
# standardize dfs for merging:
    group_df<-pivoted_family_df %>%
      group_by (across(constant_cols), index  ) %>%
      summarize (count= n(), .groups = "drop")
    if( max(group_df$count)==1) { #  indicates that there is only one answer option per slot_index
      pivoted_family_df <- pivoted_family_df %>%
        mutate(col_name = sub("(\\D+).*", "\\1", col_name))%>%
        tidyr::pivot_wider(names_from = col_name,values_from = value)
      print ( paste0("Family '", family_prefix, "' is pivoted"))
        }
    if( max(group_df$count)>1) { #  indicates that there are multiple variables for a slot_index => multiple choice question 
      pivoted_family_df <- pivoted_family_df %>%
        mutate(col_name = paste0( .env$family_prefix, "_", extract_answer_index(col_name, .env$family_prefix))) %>%
         group_by (across(constant_cols), index  ) %>%
         pivot_wider(names_from = col_name, values_from = value )
      print ( paste0("Family '", family_prefix, "' is pivoted"))
        }
    pivoted_families_list[[i]] <- pivoted_family_df
    }
merged_families_df <- pivoted_families_list %>%
  reduce(function(x, y) full_join(x, y, by = c(constant_cols, "index")))
print ( paste0("Pivoted families'", family_prefix, "' were merged"))
merged_families_df 
   }

