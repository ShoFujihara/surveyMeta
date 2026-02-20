#' Extract Metadata from Survey Documents
#'
#' @description
#' Main function to extract structured metadata from social survey documents
#' using the Claude API or Gemini API. Supports PDF and text files.
#'
#' @name extract
NULL

#' Extract metadata from a survey document
#'
#' Reads a survey document (questionnaire, report, etc.), sends it to
#' Claude or Gemini API with controlled vocabulary context, and returns
#' structured metadata conforming to the JDCat/DDI schema.
#'
#' @param file_path Character. Path to the survey document (PDF or text).
#'   Can also be a character vector of multiple file paths for the same survey
#'   (e.g., questionnaire + report).
#' @param api_key Character. API key. If NULL (default), auto-detected from
#'   environment variables: `ANTHROPIC_API_KEY` for Claude models,
#'   `GEMINI_API_KEY` for Gemini models.
#' @param model Character. Model to use. Claude models (e.g.,
#'   "claude-sonnet-4-5-20250929") or Gemini models (e.g., "gemini-2.0-flash").
#' @param max_tokens Integer. Maximum tokens in the response.
#' @param pdf_mode Character. How to read PDF files:
#'   \describe{
#'     \item{"text"}{Extract text using pdftools (with layout-aware fallback).
#'       Most cost-efficient.}
#'     \item{"vision"}{Send PDFs directly to the API's vision capability.
#'       Best for scanned/image PDFs.}
#'     \item{"hybrid"}{(Default) Assess PDF quality first; use text extraction
#'       for good/fair quality, vision for poor quality PDFs.}
#'   }
#' @param verbose Logical. Print progress messages.
#'
#' @return A named list of extracted metadata fields.
#'
#' @examples
#' \dontrun{
#' # Claude (default)
#' result <- extract_metadata("survey_report.pdf")
#'
#' # Gemini (free tier available)
#' result <- extract_metadata("survey_report.pdf", model = "gemini-2.0-flash")
#'
#' # Multiple documents for one survey
#' result <- extract_metadata(c("questionnaire.pdf", "report.pdf"))
#'
#' # Force vision mode for scanned PDFs
#' result <- extract_metadata("scanned_doc.pdf", pdf_mode = "vision")
#'
#' # Export to CSV
#' export_metadata(result, "metadata.csv")
#' }
#'
#' @export
extract_metadata <- function(file_path,
                             api_key = NULL,
                             model = "claude-sonnet-4-5-20250929",
                             max_tokens = 8192L,
                             pdf_mode = c("hybrid", "text", "vision"),
                             verbose = TRUE) {

  pdf_mode <- match.arg(pdf_mode)
  provider <- detect_provider(model)

  # Auto-detect API key from environment
  if (is.null(api_key)) {
    api_key <- switch(provider,
      anthropic = Sys.getenv("ANTHROPIC_API_KEY"),
      google = Sys.getenv("GEMINI_API_KEY")
    )
  }

  if (verbose) {
    provider_label <- switch(provider, anthropic = "Claude", google = "Gemini")
    cli::cli_alert_info("Provider: {.val {provider_label}}, Model: {.val {model}}")
    cli::cli_alert_info("PDF mode: {.val {pdf_mode}}")
  }

  # Build system prompt with vocabularies
  if (verbose) cli::cli_progress_step("Building prompt with vocabularies...")
  system_prompt <- build_system_prompt()

  # Route based on pdf_mode and provider
  if (pdf_mode == "text") {
    api_args <- prepare_text_request(file_path, verbose = verbose)
  } else if (pdf_mode == "vision") {
    api_args <- prepare_vision_request(file_path, provider = provider,
                                        verbose = verbose)
  } else {
    api_args <- prepare_hybrid_request(file_path, provider = provider,
                                        verbose = verbose)
  }

  # Call API
  if (verbose) cli::cli_progress_step("Calling {provider_label} API ({model})...")
  start_time <- Sys.time()

  if (provider == "google") {
    response <- call_gemini(
      system_prompt = system_prompt,
      user_message = api_args$user_message,
      user_parts = api_args$user_parts,
      api_key = api_key,
      model = model,
      max_tokens = max_tokens
    )
  } else {
    response <- call_claude(
      system_prompt = system_prompt,
      user_message = api_args$user_message,
      user_content = api_args$user_content,
      api_key = api_key,
      model = model,
      max_tokens = max_tokens
    )
  }

  elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  if (verbose) {
    cli::cli_alert_success("API response received in {round(elapsed, 1)} seconds.")
  }

  # Parse response
  if (verbose) cli::cli_progress_step("Parsing response...")
  if (is.list(response)) {
    # tool_use / structured JSON mode: response is already a named list
    metadata <- response
  } else {
    # Text mode fallback: parse JSON string
    metadata <- parse_metadata_json(response)
  }

  # Normalize vocabulary fields (fix colon variants etc.)
  metadata <- normalize_vocab_fields(metadata)

  # Attach metadata about the extraction
  attr(metadata, "source_files") <- file_path
  attr(metadata, "model") <- model
  attr(metadata, "pdf_mode") <- pdf_mode
  attr(metadata, "extraction_time") <- Sys.time()
  attr(metadata, "elapsed_seconds") <- elapsed
  attr(metadata, "usage") <- attr(response, "usage")

  if (verbose) cli::cli_alert_success("Metadata extraction complete.")

  metadata
}

#' Prepare text-mode API request
#'
#' Extracts text from all files using pdftools (with layout fallback)
#' and builds a plain text user message.
#'
#' @param file_path Character vector of file paths.
#' @param verbose Logical.
#' @return A list with `user_message` (character) and `user_content` (NULL).
#' @keywords internal
prepare_text_request <- function(file_path, verbose = TRUE) {
  if (verbose) cli::cli_progress_step("Reading document(s) [text mode]...")

  texts <- vapply(file_path, function(f) {
    if (verbose) cli::cli_alert_info("Reading: {.file {basename(f)}}")
    extract_text(f)
  }, character(1))

  combined_text <- paste(texts, collapse = "\n\n========== Next Document ==========\n\n")

  est_tokens <- estimate_tokens(combined_text)
  if (verbose) {
    cli::cli_alert_info(
      "Document: ~{est_tokens} tokens ({nchar(combined_text)} characters)"
    )
  }

  # Degraded mode for very large documents
  if (est_tokens > 150000) {
    if (verbose) {
      cli::cli_alert_warning(
        "Document exceeds 150k tokens (~{est_tokens}). Activating degraded mode."
      )
    }
    combined_text <- extract_key_sections(combined_text, max_tokens = 120000L)
    est_tokens <- estimate_tokens(combined_text)
    if (verbose) {
      cli::cli_alert_info("After section extraction: ~{est_tokens} tokens")
    }
  }

  user_message <- paste0(
    "\u4ee5\u4e0b\u306e\u8abf\u67fb\u6587\u66f8\u304b\u3089",
    "\u30e1\u30bf\u30c7\u30fc\u30bf\u3092\u62bd\u51fa\u3057\u3066\u304f\u3060\u3055\u3044\u3002\n\n",
    "=== \u8abf\u67fb\u6587\u66f8 ===\n\n",
    combined_text
  )

  list(user_message = user_message, user_content = NULL, user_parts = NULL)
}

#' Prepare vision-mode API request
#'
#' Sends all PDF files as base64 blocks to the API's vision capability.
#' Non-PDF files are sent as text.
#'
#' @param file_path Character vector of file paths.
#' @param provider Character. "anthropic" or "google".
#' @param verbose Logical.
#' @return A list with provider-appropriate content fields.
#' @keywords internal
prepare_vision_request <- function(file_path, provider = "anthropic",
                                    verbose = TRUE) {
  if (verbose) cli::cli_progress_step("Preparing document(s) [vision mode]...")

  if (provider == "google") {
    user_parts <- build_gemini_parts(
      file_paths = file_path,
      text_files = character(0)
    )
    return(list(user_message = NULL, user_content = NULL, user_parts = user_parts))
  }

  user_content <- build_vision_content(
    file_paths = file_path,
    text_files = character(0)
  )

  list(user_message = NULL, user_content = user_content, user_parts = NULL)
}

#' Prepare hybrid-mode API request
#'
#' Assesses PDF quality for each file. Good/fair quality PDFs are sent as
#' extracted text; poor quality PDFs are sent as base64 document blocks
#' via the API's vision capability.
#'
#' @param file_path Character vector of file paths.
#' @param provider Character. "anthropic" or "google".
#' @param verbose Logical.
#' @return A list with provider-appropriate content fields.
#' @keywords internal
prepare_hybrid_request <- function(file_path, provider = "anthropic",
                                    verbose = TRUE) {
  if (verbose) cli::cli_progress_step("Assessing document quality [hybrid mode]...")

  text_files <- character(0)
  vision_files <- character(0)
  extracted_texts <- list()

  for (f in file_path) {
    ext <- tolower(tools::file_ext(f))

    if (ext != "pdf") {
      # Non-PDF: always text
      text_files <- c(text_files, f)
      next
    }

    # Extract text and assess quality
    if (verbose) cli::cli_alert_info("Assessing: {.file {basename(f)}}")
    text <- tryCatch(extract_text_pdf(f), error = function(e) "")
    quality <- attr(text, "pdf_quality")
    if (is.null(quality)) quality <- assess_pdf_quality(text)

    if (verbose) {
      cli::cli_alert_info(
        "  Quality: {quality$grade} (score={quality$score})"
      )
    }

    if (quality$grade == "poor") {
      vision_files <- c(vision_files, f)
      if (verbose) cli::cli_alert_warning("  -> Using vision mode for {.file {basename(f)}}")
    } else {
      text_files <- c(text_files, f)
      extracted_texts[[f]] <- text
      if (verbose) cli::cli_alert_success("  -> Using text mode for {.file {basename(f)}}")
    }
  }

  # If all files are text mode, use simple text request
  if (length(vision_files) == 0) {
    combined_text <- character(0)
    for (f in text_files) {
      txt <- if (f %in% names(extracted_texts)) {
        extracted_texts[[f]]
      } else {
        extract_text(f)
      }
      combined_text <- c(combined_text, txt)
    }
    combined_text <- paste(combined_text,
                           collapse = "\n\n========== Next Document ==========\n\n")

    est_tokens <- estimate_tokens(combined_text)
    if (est_tokens > 150000) {
      if (verbose) {
        cli::cli_alert_warning(
          "Document exceeds 150k tokens (~{est_tokens}). Activating degraded mode."
        )
      }
      combined_text <- extract_key_sections(combined_text, max_tokens = 120000L)
    }

    user_message <- paste0(
      "\u4ee5\u4e0b\u306e\u8abf\u67fb\u6587\u66f8\u304b\u3089",
      "\u30e1\u30bf\u30c7\u30fc\u30bf\u3092\u62bd\u51fa\u3057\u3066\u304f\u3060\u3055\u3044\u3002\n\n",
      "=== \u8abf\u67fb\u6587\u66f8 ===\n\n",
      combined_text
    )
    return(list(user_message = user_message, user_content = NULL,
                user_parts = NULL))
  }

  # Mixed mode: use provider-specific content blocks
  if (provider == "google") {
    user_parts <- build_gemini_parts(
      file_paths = file_path,
      text_files = text_files,
      extracted_texts = extracted_texts
    )
    return(list(user_message = NULL, user_content = NULL, user_parts = user_parts))
  }

  user_content <- build_vision_content(
    file_paths = file_path,
    text_files = text_files,
    extracted_texts = extracted_texts
  )

  list(user_message = NULL, user_content = user_content, user_parts = NULL)
}

#' Export metadata to file
#'
#' @param metadata Named list from [extract_metadata()], or a data.frame
#'   from [metadata_to_dataframe()].
#' @param path Character. Output file path.
#' @param format Character. Output format: "csv", "xlsx", or "json".
#'
#' @export
export_metadata <- function(metadata, path,
                            format = c("csv", "xlsx", "json")) {
  format <- match.arg(format)

  switch(format,
    csv = {
      df <- if (is.data.frame(metadata)) metadata else metadata_to_dataframe(metadata)
      readr::write_csv(df, path)
    },
    xlsx = {
      if (!requireNamespace("writexl", quietly = TRUE)) {
        cli::cli_abort("Package {.pkg writexl} is required for Excel export.")
      }
      df <- if (is.data.frame(metadata)) metadata else metadata_to_dataframe(metadata)
      writexl::write_xlsx(df, path)
    },
    json = {
      json_str <- jsonlite::toJSON(metadata, auto_unbox = TRUE, pretty = TRUE,
                                    force = TRUE)
      readr::write_file(json_str, path)
    }
  )

  cli::cli_alert_success("Metadata exported to {.file {path}}")
  invisible(path)
}

#' Generate an HTML review report for extracted metadata
#'
#' Renders an RMarkdown template showing extracted vs official metadata
#' for human review and approval.
#'
#' @param extracted Named list from \code{\link{extract_metadata}}.
#' @param survey_id Character. SSJDA survey number.
#' @param official Named list (optional). If NULL, fetched via OAI-PMH.
#' @param output_file Character. Output HTML path.
#'
#' @return The output file path (invisibly).
#' @export
generate_review <- function(extracted, survey_id,
                            official = NULL,
                            output_file = NULL) {
  if (!requireNamespace("rmarkdown", quietly = TRUE)) {
    cli::cli_abort("Package {.pkg rmarkdown} is required for review generation.")
  }

  if (is.null(official)) {
    official <- tryCatch(
      fetch_official_metadata(survey_id),
      error = function(e) NULL
    )
  }

  if (is.null(output_file)) {
    output_file <- paste0("review_", survey_id, ".html")
  }

  template <- system.file("templates", "review_template.Rmd",
                           package = "surveyMeta")
  if (template == "") {
    cli::cli_abort("Review template not found.")
  }

  rmarkdown::render(
    template,
    output_file = normalizePath(output_file, mustWork = FALSE),
    params = list(
      survey_id = survey_id,
      extracted = extracted,
      official = official
    ),
    quiet = TRUE
  )

  cli::cli_alert_success("Review report generated: {.file {output_file}}")
  invisible(output_file)
}

#' Batch extract metadata from multiple surveys
#'
#' @param file_list A list where each element is a character vector of file
#'   paths for one survey. Or a simple character vector (one file per survey).
#' @param ... Additional arguments passed to [extract_metadata()].
#'
#' @return A data.frame with one row per survey.
#' @export
batch_extract_metadata <- function(file_list, ...) {
  if (is.character(file_list)) {
    file_list <- as.list(file_list)
  }

  n <- length(file_list)
  cli::cli_alert_info("Processing {n} survey(s)...")

  results <- vector("list", n)

  for (i in seq_along(file_list)) {
    cli::cli_h2("Survey {i}/{n}")
    tryCatch({
      meta <- extract_metadata(file_list[[i]], ...)
      results[[i]] <- metadata_to_dataframe(meta)
    }, error = function(e) {
      cli::cli_alert_danger("Failed: {conditionMessage(e)}")
      results[[i]] <<- NULL
    })
  }

  # Combine results
  valid <- Filter(Negate(is.null), results)
  if (length(valid) == 0) {
    cli::cli_abort("All extractions failed.")
  }

  do.call(rbind, valid)
}
