#' Variable-Level Metadata Extraction
#'
#' @description
#' Extract variable labels and value labels from questionnaire PDFs.
#' Produces structured codebook data that can be exported to Excel or
#' DDI XML format.
#'
#' @name variables
NULL

#' Extract variable-level metadata from a questionnaire
#'
#' Reads a questionnaire PDF and extracts all question items with their
#' variable labels, value labels, question types, and skip conditions.
#'
#' @param file_path Character. Path to the questionnaire PDF.
#' @param api_key Character. API key. If NULL (default), auto-detected from
#'   environment variables.
#' @param model Character. Model to use.
#' @param max_tokens Integer. Maximum tokens in the response.
#' @param pdf_mode Character. How to read the PDF: "text", "vision", or
#'   "hybrid" (default).
#' @param verbose Logical. Print progress messages.
#'
#' @return A data.frame with columns: question_number, label, type, code,
#'   value_label, condition, page. One row per value label (long format).
#'   Variables without value labels (numeric/text) have a single row with
#'   NA code and value_label.
#'
#' @examples
#' \dontrun{
#' vars <- extract_variables("questionnaire.pdf")
#' head(vars)
#'
#' # Export as Excel codebook
#' export_codebook(vars, "codebook.xlsx")
#' }
#'
#' @export
extract_variables <- function(file_path,
                              api_key = NULL,
                              model = "claude-sonnet-4-5-20250929",
                              max_tokens = 16384L,
                              pdf_mode = c("hybrid", "text", "vision"),
                              verbose = TRUE) {

  pdf_mode <- match.arg(pdf_mode)
  provider <- detect_provider(model)

  if (is.null(api_key)) {
    api_key <- switch(provider,
      anthropic = Sys.getenv("ANTHROPIC_API_KEY"),
      google = Sys.getenv("GEMINI_API_KEY")
    )
  }

  if (verbose) {
    provider_label <- switch(provider, anthropic = "Claude", google = "Gemini")
    cli::cli_alert_info("Variable extraction: {.val {provider_label}} ({.val {model}})")
  }

  # Build variable extraction prompt
  prompt_path <- system.file("prompts", "variable_prompt.txt",
                              package = "surveyMeta")
  if (prompt_path == "") {
    cli::cli_abort("Variable extraction prompt not found.")
  }
  system_prompt <- readr::read_file(prompt_path)

  # Prepare request (reuse existing infrastructure)
  if (pdf_mode == "text") {
    api_args <- prepare_text_request(file_path, verbose = verbose)
  } else if (pdf_mode == "vision") {
    api_args <- prepare_vision_request(file_path, provider = provider,
                                        verbose = verbose)
  } else {
    api_args <- prepare_hybrid_request(file_path, provider = provider,
                                        verbose = verbose)
  }

  # Override user message prefix for variable extraction
  if (!is.null(api_args$user_message)) {
    api_args$user_message <- sub(
      "\u30e1\u30bf\u30c7\u30fc\u30bf\u3092\u62bd\u51fa",
      "\u5909\u6570\u30e9\u30d9\u30eb\u30fb\u5024\u30e9\u30d9\u30eb\u3092\u62bd\u51fa",
      api_args$user_message
    )
  }

  # Call API
  if (verbose) cli::cli_progress_step("Calling API for variable extraction...")
  start_time <- Sys.time()

  if (provider == "google") {
    response <- call_gemini(
      system_prompt = system_prompt,
      user_message = api_args$user_message,
      user_parts = api_args$user_parts,
      api_key = api_key,
      model = model,
      max_tokens = max_tokens,
      response_schema = gemini_variable_schema()
    )
  } else {
    response <- call_claude(
      system_prompt = system_prompt,
      user_message = api_args$user_message,
      user_content = api_args$user_content,
      api_key = api_key,
      model = model,
      max_tokens = max_tokens,
      tool_schema = variable_tool_schema()
    )
  }

  elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  if (verbose) {
    cli::cli_alert_success("API response received in {round(elapsed, 1)} seconds.")
  }

  # Parse response
  if (is.list(response)) {
    parsed <- response
  } else {
    parsed <- parse_metadata_json(response)
  }

  variables_list <- parsed$variables
  if (is.null(variables_list) || length(variables_list) == 0) {
    cli::cli_warn("No variables extracted from the document.")
    return(data.frame(
      question_number = character(0),
      label = character(0),
      type = character(0),
      code = integer(0),
      value_label = character(0),
      condition = character(0),
      page = integer(0),
      stringsAsFactors = FALSE
    ))
  }

  # Convert to long-format data.frame
  df <- variables_to_dataframe(variables_list)

  # Attach metadata
  attr(df, "source_files") <- file_path
  attr(df, "model") <- model
  attr(df, "elapsed_seconds") <- elapsed
  attr(df, "usage") <- attr(response, "usage")
  attr(df, "n_variables") <- length(variables_list)

  class(df) <- c("survey_variables", "data.frame")

  if (verbose) {
    cli::cli_alert_success(
      "Extracted {length(variables_list)} variables ({nrow(df)} rows in codebook)."
    )
  }

  df
}

#' Convert variable list to long-format data.frame
#' @param variables_list List of variable objects from API response.
#' @return A data.frame in long format.
#' @keywords internal
variables_to_dataframe <- function(variables_list) {
  rows <- list()

  for (v in variables_list) {
    qnum <- v$question_number %||% NA_character_
    label <- v$label %||% NA_character_
    vtype <- v$type %||% NA_character_
    condition <- v$condition
    if (is.null(condition)) condition <- NA_character_
    page <- v$page
    if (is.null(page)) page <- NA_integer_

    values <- v$values
    if (is.null(values) || length(values) == 0) {
      # No value labels (numeric/text variable)
      rows[[length(rows) + 1]] <- data.frame(
        question_number = qnum,
        label = label,
        type = vtype,
        code = NA_integer_,
        value_label = NA_character_,
        condition = condition,
        page = page,
        stringsAsFactors = FALSE
      )
    } else {
      for (val in values) {
        code <- val$code
        if (is.null(code)) code <- NA_integer_
        vlabel <- val$label %||% NA_character_
        rows[[length(rows) + 1]] <- data.frame(
          question_number = qnum,
          label = label,
          type = vtype,
          code = as.integer(code),
          value_label = vlabel,
          condition = condition,
          page = page,
          stringsAsFactors = FALSE
        )
      }
    }
  }

  do.call(rbind, rows)
}

#' Print method for survey_variables
#' @param x A survey_variables object.
#' @param ... Additional arguments (unused).
#' @export
print.survey_variables <- function(x, ...) {
  n_vars <- attr(x, "n_variables") %||% length(unique(x$question_number))
  n_values <- sum(!is.na(x$code))
  model <- attr(x, "model") %||% "unknown"

  cat(sprintf("=== Variable-Level Metadata ===\n"))
  cat(sprintf("  Variables: %d\n", n_vars))
  cat(sprintf("  Value labels: %d\n", n_values))
  cat(sprintf("  Model: %s\n\n", model))

  # Show first few variables
  vars <- unique(x$question_number)
  show_n <- min(10, length(vars))
  for (i in seq_len(show_n)) {
    v <- vars[i]
    subset <- x[x$question_number == v, , drop = FALSE]
    vtype <- subset$type[1]
    label <- subset$label[1]
    n_vals <- sum(!is.na(subset$code))

    if (n_vals > 0) {
      codes <- subset$code[!is.na(subset$code)]
      labels <- subset$value_label[!is.na(subset$code)]
      show_vals <- seq_len(min(3, length(codes)))
      val_summary <- paste(
        sprintf("%d=%s", codes[show_vals], labels[show_vals]),
        collapse = ", "
      )
      if (n_vals > 3) val_summary <- paste0(val_summary, ", ...")
      cat(sprintf("  %-8s %-30s [%s] %s\n", v, label, vtype, val_summary))
    } else {
      cat(sprintf("  %-8s %-30s [%s]\n", v, label, vtype))
    }
  }

  if (length(vars) > show_n) {
    cat(sprintf("  ... and %d more variables\n", length(vars) - show_n))
  }

  invisible(x)
}

#' Export variable metadata as a codebook
#'
#' @param variables A survey_variables data.frame from [extract_variables()],
#'   or any data.frame with columns: question_number, label, type, code,
#'   value_label.
#' @param path Character. Output file path.
#' @param format Character. Output format: "xlsx" (default), "csv", or "json".
#'
#' @export
export_codebook <- function(variables,
                            path,
                            format = c("xlsx", "csv", "json")) {
  format <- match.arg(format)

  switch(format,
    xlsx = {
      if (!requireNamespace("writexl", quietly = TRUE)) {
        cli::cli_abort("Package {.pkg writexl} is required for Excel export.")
      }
      writexl::write_xlsx(variables, path)
    },
    csv = {
      readr::write_csv(variables, path)
    },
    json = {
      # Convert back to nested structure for JSON
      json_data <- dataframe_to_variables_list(variables)
      json_str <- jsonlite::toJSON(
        list(variables = json_data),
        auto_unbox = TRUE, pretty = TRUE, force = TRUE
      )
      readr::write_file(json_str, path)
    }
  )

  cli::cli_alert_success("Codebook exported to {.file {path}}")
  invisible(path)
}

#' Convert long-format data.frame back to nested variable list
#' @keywords internal
dataframe_to_variables_list <- function(df) {
  vars <- unique(df$question_number)
  result <- vector("list", length(vars))

  for (i in seq_along(vars)) {
    v <- vars[i]
    subset <- df[df$question_number == v, , drop = FALSE]
    row1 <- subset[1, ]

    values <- list()
    if (any(!is.na(subset$code))) {
      for (j in seq_len(nrow(subset))) {
        if (!is.na(subset$code[j])) {
          values[[length(values) + 1]] <- list(
            code = subset$code[j],
            label = subset$value_label[j]
          )
        }
      }
    }

    result[[i]] <- list(
      question_number = row1$question_number,
      label = row1$label,
      type = row1$type,
      values = values,
      condition = if (is.na(row1$condition)) NULL else row1$condition,
      page = if (is.na(row1$page)) NULL else row1$page
    )
  }

  result
}
