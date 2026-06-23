#' Apply value labels to a vector using REDCap metadata
#'
#' Converts a vector of coded values into a factor with human-readable labels
#' using choice definitions from REDCap-style metadata. The function parses the
#' `select_choices_or_calculations` field to extract code–label mappings and
#' applies them as factor levels.
#'
#' If no matching metadata is found for the specified field name, or if the field
#' does not define selectable choices, the original vector is returned unchanged
#' with a warning.
#'
#' @param vec A vector of coded values (numeric or character) to be labeled.
#' @param field_name Character string giving the metadata field name associated
#'   with `vec`.
#' @param meta_data Data frame containing REDCap-style metadata. Must include
#'   `field_name` and `select_choices_or_calculations` columns.
#'
#' @return A factor with levels corresponding to the coded values and labels
#'   derived from metadata. If metadata is missing, returns `vec` unchanged.
#'
#' @examples
#' meta <- data.frame(
#'   field_name = "learner_type",
#'   select_choices_or_calculations = "1, Student | 2, Resident | 3, Faculty"
#' )
#'
#' new_long <- data.frame(learner_group = c(1, 2, 3))
#'
#' new_long |>
#'   dplyr::mutate(
#'     learner_type_label = as.character(
#'       label_from_metadata(learner_group, "learner_type", meta)
#'     )
#'   )
#'
#'
#' @export
label_from_metadata <- function(vec, field_name, meta_data) {
  meta_row <- meta_data[meta_data$field_name == field_name, ]
  if (nrow(meta_row) == 0 || is.na(meta_row$select_choices_or_calculations)) {
    warning(paste("No metadata found for", field_name))
    return(vec)
  }

  choices <- meta_row$select_choices_or_calculations
  levels <- strsplit(choices, "\\s*\\|\\s*")[[1]]

  key_vals <- lapply(levels, function(x) {
    parts <- strsplit(x, ",\\s*")[[1]]
    code <- parts[1]
    label <- paste(parts[-1], collapse = ",")  # Support commas in labels
    c(code, label)
  })

  key_vals_mat <- do.call(rbind, key_vals)
  codes <- key_vals_mat[, 1]
  labels <- key_vals_mat[, 2]

  factor(as.character(vec), levels = codes, labels = labels)
}
