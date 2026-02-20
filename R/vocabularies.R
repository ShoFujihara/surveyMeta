#' Controlled Vocabulary Functions
#'
#' @description
#' Load and manage controlled vocabularies (CESSDA topics, SSJDA topics,
#' DDI sampling procedures, DDI modes of collection).
#'
#' @name vocabularies
NULL

#' Load a controlled vocabulary file
#'
#' @param vocab_name Character. One of "cessda_topics", "ssjda_topics",
#'   "sampling_procedures", "collection_modes".
#' @return A data.frame with vocabulary entries.
#' @export
load_vocabulary <- function(vocab_name) {
  valid <- c("cessda_topics", "ssjda_topics",
             "sampling_procedures", "collection_modes")
  if (!vocab_name %in% valid) {
    cli::cli_abort("{.arg vocab_name} must be one of: {.val {valid}}")
  }

  path <- system.file("extdata", paste0(vocab_name, ".csv"),
                       package = "surveyMeta")
  if (path == "") {
    cli::cli_abort("Vocabulary file not found: {vocab_name}.csv")
  }

  readr::read_csv(path, show_col_types = FALSE)
}

#' Format vocabulary for prompt embedding
#'
#' Convert a vocabulary data.frame to a text list suitable for embedding
#' in the system prompt.
#'
#' @param vocab A data.frame from [load_vocabulary()].
#' @param code_col Character. Column name for codes.
#' @param label_col Character. Column name for display labels.
#' @return Character string with formatted vocabulary list.
#' @keywords internal
format_vocabulary_for_prompt <- function(vocab, code_col = "code",
                                         label_col = NULL) {
  if (is.null(label_col)) {
    # Auto-detect: prefer Japanese label
    if ("label_ja" %in% names(vocab)) {
      label_col <- "label_ja"
    } else if ("label_en" %in% names(vocab)) {
      label_col <- "label_en"
    } else {
      label_col <- code_col
    }
  }

  lines <- paste0("- ", vocab[[code_col]], ": ", vocab[[label_col]])
  paste(lines, collapse = "\n")
}

#' Convert vocabulary labels between languages and codes
#'
#' Convert between DDI codes, English labels, and Japanese labels for
#' controlled vocabulary fields (sampling_procedure, mode_of_collection,
#' cessda_topic, ssjda_topic).
#'
#' @param values Character vector. Values to convert.
#' @param vocab_name Character. One of "cessda_topics", "ssjda_topics",
#'   "sampling_procedures", "collection_modes".
#' @param from Character. Source format: "code", "label_en", "label_ja".
#' @param to Character. Target format: "code", "label_en", "label_ja".
#'
#' @return Character vector of converted values. Unmatched values are
#'   returned as-is with a warning.
#'
#' @examples
#' \dontrun{
#' convert_labels(
#'   c("\u975e\u78ba\u7387: \u30af\u30aa\u30fc\u30bf\u62bd\u51fa"),
#'   "sampling_procedures", from = "label_ja", to = "code"
#' )
#' # => "Nonprobability.Quota"
#'
#' convert_labels(
#'   "Nonprobability.Quota",
#'   "sampling_procedures", from = "code", to = "label_en"
#' )
#' # => "Nonprobability: Quota"
#' }
#'
#' @export
convert_labels <- function(values, vocab_name, from = "label_ja", to = "code") {
  vocab <- load_vocabulary(vocab_name)

  if (!from %in% names(vocab)) {
    cli::cli_abort("{.arg from} column {.val {from}} not found in {.val {vocab_name}}.")
  }
  if (!to %in% names(vocab)) {
    cli::cli_abort("{.arg to} column {.val {to}} not found in {.val {vocab_name}}.")
  }

  idx <- match(values, vocab[[from]])
  converted <- vocab[[to]][idx]

  # Warn about unmatched values
  unmatched <- is.na(idx)
  if (any(unmatched)) {
    bad <- values[unmatched]
    cli::cli_warn("Unmatched values returned as-is: {.val {bad}}")
    converted[unmatched] <- values[unmatched]
  }

  converted
}

#' Convert all vocabulary fields in metadata between formats
#'
#' Converts sampling_procedure, mode_of_collection, cessda_topic, and
#' ssjda_topic fields in a metadata list between label formats.
#'
#' @param metadata Named list from \code{\link{extract_metadata}}.
#' @param from Character. Source format: "code", "label_en", "label_ja".
#' @param to Character. Target format: "code", "label_en", "label_ja".
#'
#' @return The metadata list with converted vocabulary fields.
#'
#' @examples
#' \dontrun{
#' result <- extract_metadata("survey.pdf")
#' # Convert Japanese labels to DDI codes
#' result_codes <- convert_metadata_labels(result, from = "label_ja", to = "code")
#' # Convert to English labels
#' result_en <- convert_metadata_labels(result, from = "label_ja", to = "label_en")
#' }
#'
#' @export
convert_metadata_labels <- function(metadata, from = "label_ja", to = "code") {
  field_vocab <- list(
    sampling_procedure = "sampling_procedures",
    mode_of_collection = "collection_modes",
    cessda_topic = "cessda_topics",
    ssjda_topic = "ssjda_topics"
  )

  for (field in names(field_vocab)) {
    vals <- metadata[[field]]
    if (is.null(vals)) next

    vals_vec <- if (is.list(vals)) unlist(vals) else vals
    if (length(vals_vec) == 0) next

    # Skip if target column doesn't exist in this vocabulary
    vocab <- load_vocabulary(field_vocab[[field]])
    if (!to %in% names(vocab)) {
      cli::cli_warn("Column {.val {to}} not available for {.val {field}}, skipping.")
      next
    }
    if (!from %in% names(vocab)) {
      cli::cli_warn("Column {.val {from}} not available for {.val {field}}, skipping.")
      next
    }

    metadata[[field]] <- convert_labels(
      vals_vec, field_vocab[[field]], from = from, to = to
    )
  }

  metadata
}

#' Normalize vocabulary field values to match canonical labels
#'
#' Fixes common LLM output variations (e.g., half-width vs full-width colons)
#' by fuzzy-matching against the vocabulary and replacing with the canonical form.
#'
#' @param metadata Named list from \code{\link{extract_metadata}}.
#' @return The metadata list with normalized vocabulary values.
#' @keywords internal
normalize_vocab_fields <- function(metadata) {
  field_vocab <- list(
    sampling_procedure = "sampling_procedures",
    mode_of_collection = "collection_modes"
  )

  for (field in names(field_vocab)) {
    vals <- metadata[[field]]
    if (is.null(vals)) next
    vals_vec <- if (is.list(vals)) unlist(vals) else vals
    if (length(vals_vec) == 0) next

    vocab <- load_vocabulary(field_vocab[[field]])
    canonical <- vocab$label_ja

    normalized <- vapply(vals_vec, function(v) {
      if (v %in% canonical) return(v)
      # Try normalizing colons: half-width `: ` <-> full-width `\uff1a`
      v_alt <- gsub(":\\s*", "\uff1a", v)
      if (v_alt %in% canonical) return(v_alt)
      v_alt2 <- gsub("\uff1a", ": ", v)
      if (v_alt2 %in% canonical) return(v_alt2)
      v
    }, character(1), USE.NAMES = FALSE)

    metadata[[field]] <- normalized
  }

  metadata
}

#' Validate extracted metadata against controlled vocabularies
#'
#' Checks that controlled vocabulary fields contain only valid codes/labels.
#'
#' @param metadata Named list from \code{\link{extract_metadata}}.
#' @param verbose Logical. Print validation results.
#'
#' @return A list with \code{valid} (logical), \code{issues} (character vector),
#'   and \code{validated} (the input metadata with invalid codes removed).
#'
#' @export
validate_metadata <- function(metadata, verbose = TRUE) {
  issues <- character(0)

  # Validate sampling_procedure (accepts code, label_ja, or label_en)
  sp_vocab <- load_vocabulary("sampling_procedures")
  sp_valid <- c(sp_vocab$code, sp_vocab$label_ja, sp_vocab$label_en)
  sp_vals <- if (is.character(metadata$sampling_procedure)) metadata$sampling_procedure else unlist(metadata$sampling_procedure)
  if (!is.null(sp_vals)) {
    bad <- sp_vals[!sp_vals %in% sp_valid]
    if (length(bad) > 0) {
      issues <- c(issues, paste0("sampling_procedure: invalid value(s): ", paste(bad, collapse = ", ")))
    }
  }

  # Validate mode_of_collection (accepts code, label_ja, or label_en)
  mc_vocab <- load_vocabulary("collection_modes")
  mc_valid <- c(mc_vocab$code, mc_vocab$label_ja, mc_vocab$label_en)
  mc_vals <- if (is.character(metadata$mode_of_collection)) metadata$mode_of_collection else unlist(metadata$mode_of_collection)
  if (!is.null(mc_vals)) {
    bad <- mc_vals[!mc_vals %in% mc_valid]
    if (length(bad) > 0) {
      issues <- c(issues, paste0("mode_of_collection: invalid value(s): ", paste(bad, collapse = ", ")))
    }
  }

  # Validate cessda_topic
  ct_vocab <- load_vocabulary("cessda_topics")
  ct_valid <- c(ct_vocab$code, ct_vocab$label_ja, ct_vocab$label_en)
  ct_vals <- if (is.character(metadata$cessda_topic)) metadata$cessda_topic else unlist(metadata$cessda_topic)
  if (!is.null(ct_vals)) {
    bad <- ct_vals[!ct_vals %in% ct_valid]
    if (length(bad) > 0) {
      issues <- c(issues, paste0("cessda_topic: invalid value(s): ", paste(bad, collapse = ", ")))
    }
  }

  # Validate data_type
  valid_types <- c(
    "\u91cf\u7684\u8abf\u67fb: \u30df\u30af\u30ed\u30c7\u30fc\u30bf",
    "\u91cf\u7684\u8abf\u67fb: \u30de\u30af\u30ed\u30c7\u30fc\u30bf",
    "\u91cf\u7684\u8abf\u67fb",
    "\u8cea\u7684\u8abf\u67fb",
    "\u5b98\u5e81\u7d71\u8a08: \u30df\u30af\u30ed\u30c7\u30fc\u30bf",
    "\u5b98\u5e81\u7d71\u8a08: \u30de\u30af\u30ed\u30c7\u30fc\u30bf",
    "\u305d\u306e\u4ed6"
  )
  if (!is.null(metadata$data_type) && !metadata$data_type %in% valid_types) {
    issues <- c(issues, paste0("data_type: invalid value: ", metadata$data_type))
  }

  # Validate unit_of_analysis
  valid_units <- c(
    "\u500b\u4eba", "\u4e16\u5e2f", "\u5bb6\u65cf", "\u7d44\u7e54",
    "\u96c6\u56e3", "\u30a4\u30d9\u30f3\u30c8", "\u5730\u7406\u7684\u5358\u4f4d", "\u305d\u306e\u4ed6"
  )
  if (!is.null(metadata$unit_of_analysis) && !metadata$unit_of_analysis %in% valid_units) {
    issues <- c(issues, paste0("unit_of_analysis: invalid value: ", metadata$unit_of_analysis))
  }

  if (verbose) {
    if (length(issues) == 0) {
      cli::cli_alert_success("All controlled vocabulary fields are valid.")
    } else {
      cli::cli_alert_warning("Validation issues found:")
      for (issue in issues) {
        cli::cli_alert_danger(issue)
      }
    }
  }

  list(
    valid = length(issues) == 0,
    issues = issues,
    n_issues = length(issues)
  )
}
