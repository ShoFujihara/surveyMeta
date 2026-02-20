#' Evaluate Extracted Metadata Against Ground Truth
#'
#' @description
#' Compare Claude-extracted metadata against official SSJDA metadata
#' fetched via OAI-PMH, computing field-by-field similarity scores
#' using field-type-appropriate metrics.
#'
#' @name evaluate
NULL

#' Evaluate extraction quality against official metadata
#'
#' Compares extracted metadata (from \code{\link{extract_metadata}}) with
#' official metadata (from \code{\link{fetch_official_metadata}}) and
#' returns field-by-field similarity scores using appropriate metrics
#' for each field type.
#'
#' Field types and their metrics:
#' \itemize{
#'   \item \strong{Exact match fields} (data_type, unit_of_analysis): Accuracy (0 or 1)
#'   \item \strong{Set fields} (cessda_topic, sampling_procedure, mode_of_collection):
#'     Precision, Recall, F1
#'   \item \strong{Text fields} (survey_overview, survey_target, survey_conductor, etc.):
#'     Jaccard similarity on character bigrams
#'   \item \strong{Structured fields} (sample_size): Custom comparison
#' }
#'
#' @param extracted Named list from \code{\link{extract_metadata}}.
#' @param official Named list from \code{\link{fetch_official_metadata}}.
#'   If NULL and \code{survey_id} is provided, fetched automatically.
#' @param survey_id Character. SSJDA survey number. Used to fetch official
#'   metadata if \code{official} is NULL.
#'
#' @return An survey_evaluation object (data.frame) with columns: field,
#'   field_type, official, extracted, similarity, precision, recall, f1,
#'   match_type.
#'
#' @examples
#' \dontrun{
#' result <- extract_metadata("survey.pdf")
#' eval_result <- evaluate_extraction(result, survey_id = "0987")
#' print(eval_result)
#' }
#'
#' @export
evaluate_extraction <- function(extracted, official = NULL, survey_id = NULL) {
  if (is.null(official) && is.null(survey_id)) {
    cli::cli_abort("Provide either {.arg official} metadata or {.arg survey_id}.")
  }

  if (is.null(official)) {
    cli::cli_alert_info("Fetching official metadata for survey {.val {survey_id}}...")
    official <- fetch_official_metadata(survey_id)
  }

  # Field definitions with types
  field_defs <- list(
    list(name = "survey_overview",    type = "text"),
    list(name = "data_type",          type = "exact"),
    list(name = "survey_target",      type = "text"),
    list(name = "unit_of_analysis",   type = "exact"),
    list(name = "sample_size",        type = "text"),
    list(name = "survey_date",        type = "text"),
    list(name = "geographic_coverage", type = "text"),
    list(name = "sampling_procedure", type = "set"),
    list(name = "mode_of_collection", type = "set"),
    list(name = "survey_conductor",   type = "text"),
    list(name = "cessda_topic",       type = "set")
  )

  results <- vector("list", length(field_defs))

  for (i in seq_along(field_defs)) {
    fd <- field_defs[[i]]
    f <- fd$name
    ftype <- fd$type

    off_raw <- official[[f]]
    ext_raw <- extracted[[f]]
    off_val <- flatten_value(off_raw)
    ext_val <- flatten_value(ext_raw)

    row <- list(
      field = f,
      field_type = ftype,
      official = if (is.na(off_val)) NA_character_ else off_val,
      extracted = if (is.na(ext_val)) NA_character_ else ext_val,
      similarity = NA_real_,
      precision = NA_real_,
      recall = NA_real_,
      f1 = NA_real_,
      match_type = "no_ground_truth"
    )

    if (is.na(off_val)) {
      # No ground truth
      results[[i]] <- row
      next
    }

    if (is.na(ext_val)) {
      row$similarity <- 0
      row$match_type <- "missing"
      results[[i]] <- row
      next
    }

    if (ftype == "exact") {
      match <- tolower(trimws(off_val)) == tolower(trimws(ext_val))
      row$similarity <- if (match) 1.0 else 0.0
      row$match_type <- if (match) "exact" else "mismatch"

    } else if (ftype == "set") {
      off_set <- to_set(off_raw)
      ext_set <- to_set(ext_raw)
      prf <- set_precision_recall_f1(off_set, ext_set)
      row$precision <- prf$precision
      row$recall <- prf$recall
      row$f1 <- prf$f1
      row$similarity <- prf$f1
      row$match_type <- if (prf$f1 >= 1.0) "exact" else
        if (prf$f1 >= 0.8) "high" else
        if (prf$f1 >= 0.5) "medium" else "low"

    } else {
      # text similarity
      sim <- jaccard_similarity(off_val, ext_val)
      row$similarity <- sim
      row$match_type <- if (sim >= 1.0) "exact" else
        if (sim >= 0.8) "high" else
        if (sim >= 0.5) "medium" else "low"
    }

    results[[i]] <- row
  }

  # Build data.frame
  df <- data.frame(
    field = vapply(results, `[[`, character(1), "field"),
    field_type = vapply(results, `[[`, character(1), "field_type"),
    official = vapply(results, `[[`, character(1), "official"),
    extracted = vapply(results, `[[`, character(1), "extracted"),
    similarity = vapply(results, `[[`, numeric(1), "similarity"),
    precision = vapply(results, `[[`, numeric(1), "precision"),
    recall = vapply(results, `[[`, numeric(1), "recall"),
    f1 = vapply(results, `[[`, numeric(1), "f1"),
    match_type = vapply(results, `[[`, character(1), "match_type"),
    stringsAsFactors = FALSE
  )

  # Summary stats
  valid_sims <- df$similarity[!is.na(df$similarity)]
  set_f1s <- df$f1[df$field_type == "set" & !is.na(df$f1)]
  text_sims <- df$similarity[df$field_type == "text" & !is.na(df$similarity)]
  exact_acc <- df$similarity[df$field_type == "exact" & !is.na(df$similarity)]

  attr(df, "mean_similarity") <- if (length(valid_sims) > 0) mean(valid_sims) else NA
  attr(df, "n_exact") <- sum(df$match_type == "exact", na.rm = TRUE)
  attr(df, "n_fields") <- length(valid_sims)
  attr(df, "mean_set_f1") <- if (length(set_f1s) > 0) mean(set_f1s) else NA
  attr(df, "mean_text_sim") <- if (length(text_sims) > 0) mean(text_sims) else NA
  attr(df, "exact_accuracy") <- if (length(exact_acc) > 0) mean(exact_acc) else NA

  class(df) <- c("survey_evaluation", "data.frame")
  df
}

#' Convert a value to a set of strings for set comparison
#' @keywords internal
to_set <- function(x) {
  if (is.null(x)) return(character(0))
  if (is.list(x)) x <- unlist(x)
  x <- as.character(x)
  x <- trimws(x)
  x <- x[nchar(x) > 0]
  # Split on semicolons or pipes (common in flattened values)
  x <- unlist(strsplit(x, "\\s*[;|]\\s*"))
  x <- trimws(x)
  x[nchar(x) > 0]
}

#' Compute precision, recall, and F1 for set comparison
#' @keywords internal
set_precision_recall_f1 <- function(reference, predicted) {
  if (length(reference) == 0 && length(predicted) == 0) {
    return(list(precision = 1.0, recall = 1.0, f1 = 1.0))
  }
  if (length(predicted) == 0) {
    return(list(precision = 0.0, recall = 0.0, f1 = 0.0))
  }
  if (length(reference) == 0) {
    return(list(precision = NA_real_, recall = NA_real_, f1 = NA_real_))
  }

  # Normalize for comparison (lowercase, trim) and deduplicate
  ref_norm <- unique(tolower(trimws(reference)))
  pred_norm <- unique(tolower(trimws(predicted)))

  tp <- sum(pred_norm %in% ref_norm)
  precision <- tp / length(pred_norm)
  recall <- tp / length(ref_norm)
  f1 <- if (precision + recall == 0) 0 else 2 * precision * recall / (precision + recall)

  list(precision = precision, recall = recall, f1 = f1)
}

#' Flatten a metadata value to a single string for comparison
#' @keywords internal
flatten_value <- function(x) {
  if (is.null(x)) return(NA_character_)
  if (is.list(x)) {
    vals <- unlist(x)
    if (length(vals) == 0) return(NA_character_)
    return(paste(vals, collapse = "; "))
  }
  if (length(x) == 0) return(NA_character_)
  if (length(x) > 1) return(paste(x, collapse = "; "))
  result <- as.character(x)
  if (is.na(result) || nchar(result) == 0) return(NA_character_)
  result
}

#' Token-level Jaccard similarity for Japanese/mixed text
#' @keywords internal
jaccard_similarity <- function(a, b) {
  tokenize <- function(s) {
    s <- tolower(trimws(s))
    # Split on whitespace, punctuation, and generate character bigrams
    words <- unlist(strsplit(s, "[\\s\\p{P}]+", perl = TRUE))
    words <- words[nchar(words) > 0]
    # Also add character bigrams for Japanese
    chars <- strsplit(s, "")[[1]]
    chars <- chars[!grepl("[\\s\\p{P}]", chars, perl = TRUE)]
    bigrams <- if (length(chars) >= 2) {
      vapply(seq_len(length(chars) - 1), function(i) {
        paste(chars[i:(i+1)], collapse = "")
      }, character(1))
    } else {
      character(0)
    }
    unique(c(words, bigrams))
  }

  tokens_a <- tokenize(a)
  tokens_b <- tokenize(b)

  if (length(tokens_a) == 0 && length(tokens_b) == 0) return(1.0)
  if (length(tokens_a) == 0 || length(tokens_b) == 0) return(0.0)

  intersection <- length(intersect(tokens_a, tokens_b))
  union_size <- length(union(tokens_a, tokens_b))

  intersection / union_size
}

#' Print method for evaluation results
#' @param x An survey_evaluation object.
#' @param ... Additional arguments (unused).
#' @export
print.survey_evaluation <- function(x, ...) {
  cat("=== SSJDA Metadata Extraction Evaluation ===\n\n")

  for (i in seq_len(nrow(x))) {
    sim <- x$similarity[i]
    ftype <- x$field_type[i]
    icon <- if (is.na(sim)) "\u2014" else if (sim >= 0.8) "\u2713" else if (sim >= 0.5) "\u25cb" else "\u2717"

    if (ftype == "set" && !is.na(x$f1[i])) {
      detail <- sprintf("P=%.2f R=%.2f F1=%.2f", x$precision[i], x$recall[i], x$f1[i])
    } else {
      detail <- if (is.na(sim)) "N/A" else sprintf("%.3f", sim)
    }

    cat(sprintf("  %s %-22s  %-24s  [%s]\n", icon, x$field[i], detail, x$match_type[i]))
  }

  mean_sim <- attr(x, "mean_similarity")
  n_exact <- attr(x, "n_exact")
  n_fields <- attr(x, "n_fields")
  mean_f1 <- attr(x, "mean_set_f1")
  mean_text <- attr(x, "mean_text_sim")
  exact_acc <- attr(x, "exact_accuracy")

  cat(sprintf("\n  Overall: mean=%.3f (%d/%d exact)\n", mean_sim, n_exact, n_fields))
  if (!is.na(exact_acc)) cat(sprintf("  Categorical accuracy: %.1f%%\n", exact_acc * 100))
  if (!is.na(mean_f1))   cat(sprintf("  Set fields mean F1: %.3f\n", mean_f1))
  if (!is.na(mean_text))  cat(sprintf("  Text fields mean Jaccard: %.3f\n", mean_text))

  invisible(x)
}
