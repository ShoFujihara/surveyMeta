#' Parse API Response
#'
#' @description
#' Parse and validate the JSON response from Claude API.
#'
#' @name parse
NULL

#' Parse metadata JSON response
#'
#' @param json_string Character. Raw JSON string from API.
#' @return A named list of metadata fields.
#' @keywords internal
parse_metadata_json <- function(json_string) {
  # Strip markdown code fences if present
  json_string <- gsub("^```json\\s*", "", json_string)
  json_string <- gsub("^```\\s*", "", json_string)
  json_string <- gsub("\\s*```$", "", json_string)
  json_string <- trimws(json_string)

  tryCatch(
    jsonlite::fromJSON(json_string, simplifyVector = TRUE),
    error = function(e) {
      cli::cli_abort(c(
        "Failed to parse API response as JSON.",
        "i" = "Raw response: {.val {substr(json_string, 1, 200)}}",
        "x" = conditionMessage(e)
      ))
    }
  )
}

#' Convert metadata list to a flat data.frame
#'
#' @param metadata Named list from [parse_metadata_json()].
#' @return A single-row data.frame suitable for CSV export.
#' @export
metadata_to_dataframe <- function(metadata) {
  # Separate evidence from metadata fields
  evidence <- metadata$evidence
  fields <- metadata[names(metadata) != "evidence"]

  # Flatten list/array fields to semicolon-separated strings
  flat <- lapply(fields, function(x) {
    if (is.list(x) && !is.null(names(x))) {
      # Named list (e.g., sample_size) -> collapse as key=value
      paste(paste0(names(x), "=", x), collapse = "; ")
    } else if (is.list(x) || (is.character(x) && length(x) > 1)) {
      paste(unlist(x), collapse = "; ")
    } else if (is.null(x)) {
      NA_character_
    } else {
      as.character(x)
    }
  })

  # Flatten evidence fields with "evidence_" prefix
  if (!is.null(evidence) && is.list(evidence)) {
    evidence_flat <- lapply(evidence, function(x) {
      if (is.null(x)) NA_character_ else as.character(x)
    })
    names(evidence_flat) <- paste0("evidence_", names(evidence_flat))
    flat <- c(flat, evidence_flat)
  }

  as.data.frame(flat, stringsAsFactors = FALSE)
}
