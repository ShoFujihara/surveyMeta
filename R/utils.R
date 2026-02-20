#' Utility Functions
#'
#' @description
#' Helper functions for reading survey documents, classifying document types,
#' and managing token budgets.
#'
#' @name utils
NULL

#' Extract text from a document
#'
#' Read text content from PDF, text, or other supported file formats.
#'
#' @param file_path Character. Path to the document file.
#' @return Character string of the extracted text.
#' @export
extract_text <- function(file_path) {
  if (!file.exists(file_path)) {
    cli::cli_abort("File not found: {.file {file_path}}")
  }

  ext <- tolower(tools::file_ext(file_path))

  switch(ext,
    pdf = extract_text_pdf(file_path),
    txt = readr::read_file(file_path),
    md = readr::read_file(file_path),
    cli::cli_abort("Unsupported file type: {.val {ext}}. Supported: pdf, txt, md.")
  )
}

#' Extract text from PDF
#'
#' @param file_path Character. Path to PDF file.
#' @param use_ocr Logical. If TRUE and pdftools returns little text,
#'   attempt OCR with tesseract.
#' @return Character string.
#' @keywords internal
extract_text_pdf <- function(file_path, use_ocr = FALSE) {
  if (!requireNamespace("pdftools", quietly = TRUE)) {
    cli::cli_abort("Package {.pkg pdftools} is required for PDF reading.")
  }
  pages <- pdftools::pdf_text(file_path)
  text <- paste(pages, collapse = "\n\n--- Page Break ---\n\n")

  # Assess quality
  quality <- assess_pdf_quality(text)
  if (quality$grade == "poor") {
    cli::cli_warn(c(
      "PDF text quality is {.val poor} (score={quality$score}).",
      "i" = "File: {.file {basename(file_path)}}",
      "i" = "Japanese ratio: {quality$japanese_ratio * 100}%, Garbled: {quality$garbled_ratio * 100}%"
    ))
  }

  # Layout-aware fallback for poor quality text
  if (quality$grade == "poor") {
    layout_text <- extract_text_pdf_layout(file_path)
    if (!is.null(layout_text)) {
      layout_quality <- assess_pdf_quality(layout_text)
      if (layout_quality$score > quality$score) {
        cli::cli_alert_info("Layout-aware extraction improved quality: {quality$grade} -> {layout_quality$grade}")
        text <- layout_text
        quality <- layout_quality
      }
    }
  }

  # OCR fallback for scanned PDFs
  if (use_ocr && quality$grade == "poor") {
    if (requireNamespace("tesseract", quietly = TRUE)) {
      cli::cli_alert_info("Poor quality detected; attempting OCR...")
      imgs <- pdftools::pdf_convert(file_path, dpi = 300)
      ocr_texts <- vapply(imgs, function(img) {
        tesseract::ocr(img, engine = tesseract::tesseract("jpn"))
      }, character(1))
      file.remove(imgs)
      text <- paste(ocr_texts, collapse = "\n\n--- Page Break ---\n\n")
      quality <- assess_pdf_quality(text)
    } else {
      cli::cli_warn("Poor PDF quality but {.pkg tesseract} not available for OCR.")
    }
  }

  attr(text, "pdf_quality") <- quality
  text
}

#' Count approximate tokens for Japanese text
#'
#' Rough estimate: ~1.5 characters per token for Japanese text.
#'
#' @param text Character string.
#' @return Integer. Estimated token count.
#' @keywords internal
estimate_tokens <- function(text) {
  as.integer(nchar(text) / 1.5)
}

#' Assess PDF text extraction quality
#'
#' Computes quality metrics for text extracted from a PDF to detect
#' scanned images, garbled text, or poor extraction results.
#'
#' @param text Character. Extracted text from a PDF.
#' @return A list with class \code{pdf_quality}: \code{score} (0-1),
#'   \code{grade} ("good"/"fair"/"poor"), and diagnostic metrics.
#' @export
assess_pdf_quality <- function(text) {
  lines <- strsplit(text, "\n")[[1]]
  n_lines <- length(lines)

  if (n_lines == 0 || nchar(trimws(text)) == 0) {
    result <- list(
      score = 0, grade = "poor",
      n_chars = 0L, n_lines = 0L,
      empty_line_ratio = 1, mean_line_length = 0,
      japanese_ratio = 0, garbled_ratio = 0,
      has_headings = FALSE
    )
    class(result) <- "pdf_quality"
    return(result)
  }

  n_chars <- nchar(text)

  # Empty line ratio
  empty_lines <- sum(nchar(trimws(lines)) == 0)
  empty_line_ratio <- empty_lines / n_lines

  # Mean line length (non-empty lines)
  nonempty <- lines[nchar(trimws(lines)) > 0]
  mean_line_length <- if (length(nonempty) > 0) mean(nchar(nonempty)) else 0

  # Japanese character ratio (hiragana + katakana + kanji)
  jp_chars <- nchar(gsub("[^\u3040-\u309f\u30a0-\u30ff\u4e00-\u9fff]", "", text))
  # ASCII + common punctuation
  ascii_chars <- nchar(gsub("[^ -~]", "", text))
  total_nonspace <- n_chars - nchar(gsub("\\S", "", text))
  japanese_ratio <- if (total_nonspace > 0) jp_chars / total_nonspace else 0

  # Garbled character ratio (replacement chars, unusual Unicode)
  garbled <- nchar(gsub("[^\ufffd\ufffe\uffff]", "", text))
  garbled_ratio <- if (n_chars > 0) garbled / n_chars else 0

  # Heading detection (numbered sections, Japanese heading patterns)
  has_headings <- grepl(
    "(\u7b2c.+\u7ae0|\u7b2c.+\u7bc0|\\d+[.\\uff0e].+|[IVX]+[.\\uff0e]|^#+\\s)",
    text, perl = TRUE
  )

  # Composite score (0-1, higher = better quality)
  score <- 0
  # Japanese content present (up to 0.4)
  score <- score + min(japanese_ratio * 2, 0.4)
  # Reasonable line lengths (up to 0.2)
  score <- score + if (mean_line_length > 5 && mean_line_length < 200) 0.2 else 0
  # Not too many empty lines (up to 0.2)
  score <- score + max(0, 0.2 - empty_line_ratio * 0.4)
  # Low garbled ratio (up to 0.1)
  score <- score + max(0, 0.1 - garbled_ratio * 10)
  # Has structure/headings (0.1)
  score <- score + if (has_headings) 0.1 else 0

  grade <- if (score >= 0.6) "good" else if (score >= 0.3) "fair" else "poor"

  result <- list(
    score = round(score, 3),
    grade = grade,
    n_chars = n_chars,
    n_lines = n_lines,
    empty_line_ratio = round(empty_line_ratio, 3),
    mean_line_length = round(mean_line_length, 1),
    japanese_ratio = round(japanese_ratio, 3),
    garbled_ratio = round(garbled_ratio, 3),
    has_headings = has_headings
  )
  class(result) <- "pdf_quality"
  result
}

#' @export
print.pdf_quality <- function(x, ...) {
  icon <- switch(x$grade, good = "\u2713", fair = "\u25cb", poor = "\u2717")
  cli::cli_text("{icon} PDF quality: {.val {x$grade}} (score={x$score})")
  cli::cli_text("  Characters: {format(x$n_chars, big.mark = ',')}, Lines: {x$n_lines}")
  cli::cli_text("  Japanese: {x$japanese_ratio * 100}%, Garbled: {x$garbled_ratio * 100}%")
  cli::cli_text("  Empty lines: {x$empty_line_ratio * 100}%, Mean line length: {x$mean_line_length}")
  invisible(x)
}

#' Classify survey document type
#'
#' Classifies a PDF or text file as one of: "report" (survey report/overview),
#' "codebook" (variable codebook), "questionnaire" (survey questionnaire),
#' "supplement" (occupational history etc.), or "other".
#'
#' Classification is based on filename patterns and first-page content.
#'
#' @param file_path Character. Path to the document.
#' @return A list with: \code{type} (character), \code{priority} (integer,
#'   lower = higher priority for metadata extraction),
#'   \code{tokens} (estimated token count).
#' @export
classify_document <- function(file_path) {
  fname <- tolower(basename(file_path))
  text <- tryCatch(extract_text(file_path), error = function(e) "")
  tokens <- estimate_tokens(text)
  first_page <- substr(text, 1, 2000)

  type <- if (grepl("\u30b3\u30fc\u30c9\u30d6\u30c3\u30af|codebook|cb_", fname)) {
    "codebook"
  } else if (grepl("\u5831\u544a\u66f8|\u6982\u8981|report|summary|01_", fname)) {
    "report"
  } else if (grepl("\u8abf\u67fb\u7968|\u8cea\u554f|\u7968_|questionnaire", fname)) {
    "questionnaire"
  } else if (grepl("\u8077\u6b74|\u7d4c\u6b74|\u5e74\u8868", fname)) {
    "supplement"
  } else {
    if (grepl("\u8abf\u67fb\u306e\u6982\u8981|\u8abf\u67fb\u76ee\u7684|\u6a19\u672c\u8a2d\u8a08|\u56de\u53ce\u7387", first_page)) {
      "report"
    } else if (grepl("\u5909\u6570\u540d|\u5024\u30e9\u30d9\u30eb|\u5ea6\u6570\u5206\u5e03", first_page)) {
      "codebook"
    } else if (grepl("\\bQ\\d|\\b\u554f\\d|\u3042\u3066\u306f\u307e\u308b\u3082\u306e", first_page)) {
      "questionnaire"
    } else {
      "other"
    }
  }

  priority <- switch(type,
    codebook = 1L, report = 2L, questionnaire = 3L,
    supplement = 4L, 5L
  )

  list(type = type, priority = priority, tokens = tokens,
       file_path = file_path, file_name = basename(file_path))
}

#' Select documents within a token budget
#'
#' Given a set of document file paths, classifies each, then selects
#' the most informative documents that fit within the token budget.
#'
#' Priority order: codebook > report > questionnaire > supplement > other.
#'
#' @param file_paths Character vector. Paths to candidate documents.
#' @param token_budget Integer. Maximum total tokens. Default 180000
#'   (leaving room for system prompt within 200k context).
#' @param verbose Logical. Print classification results.
#'
#' @return Character vector of selected file paths, ordered by priority.
#' @export
select_documents <- function(file_paths, token_budget = 180000L, verbose = TRUE) {
  classified <- lapply(file_paths, classify_document)

  if (verbose) {
    cli::cli_h3("Document classification:")
    for (doc in classified) {
      cli::cli_alert_info(
        "{doc$file_name}: {doc$type} (priority={doc$priority}, ~{doc$tokens} tokens)"
      )
    }
  }

  ord <- order(
    vapply(classified, `[[`, integer(1), "priority"),
    vapply(classified, `[[`, integer(1), "tokens")
  )
  classified <- classified[ord]

  selected <- character(0)
  remaining <- token_budget
  for (doc in classified) {
    if (doc$tokens <= remaining) {
      selected <- c(selected, doc$file_path)
      remaining <- remaining - doc$tokens
    } else if (verbose) {
      cli::cli_alert_warning(
        "Skipping {doc$file_name} (~{doc$tokens} tokens, budget remaining: {remaining})"
      )
    }
  }

  if (verbose) {
    total <- token_budget - remaining
    cli::cli_alert_success(
      "Selected {length(selected)} documents (~{total} tokens, budget: {token_budget})"
    )
  }

  selected
}

#' Extract key sections from survey document text
#'
#' When a document is too large for the token budget, this function extracts
#' the most relevant sections using heading patterns and keyword proximity.
#' This is NOT RAG; it is a rule-based pre-filter that preserves the
#' "read the whole document" philosophy for the selected sections.
#'
#' @param text Character. Full document text.
#' @param max_tokens Integer. Target token count for extracted sections.
#' @param context_chars Integer. Characters to include around each keyword match.
#'
#' @return Character string of extracted sections, with page markers preserved.
#' @export
extract_key_sections <- function(text, max_tokens = 60000L, context_chars = 2000L) {
  # Target character count (Japanese ~1.5 chars per token)
  max_chars <- as.integer(max_tokens * 1.5)

  # Split into pages
  pages <- strsplit(text, "\n*--- Page Break ---\n*")[[1]]

  # Keywords that signal metadata-relevant sections
  section_keywords <- c(
    # Survey overview / methodology
    "\u8abf\u67fb\u306e\u6982\u8981", "\u8abf\u67fb\u76ee\u7684",
    "\u8abf\u67fb\u65b9\u6cd5", "\u8abf\u67fb\u8a2d\u8a08",
    "\u6a19\u672c\u8a2d\u8a08", "\u6a19\u672c\u62bd\u51fa",
    "\u56de\u53ce\u7387", "\u6709\u52b9\u56de\u7b54",
    "\u8abf\u67fb\u671f\u9593", "\u8abf\u67fb\u5bfe\u8c61",
    "\u8abf\u67fb\u5730\u57df", "\u8abf\u67fb\u5b9f\u65bd",
    # Sampling and collection
    "\u7121\u4f5c\u70ba\u62bd\u51fa", "\u5c64\u5316",
    "\u96c6\u843d\u62bd\u51fa", "\u591a\u6bb5\u62bd\u51fa",
    "\u90f5\u9001\u8abf\u67fb", "\u8a2a\u554f\u8abf\u67fb",
    "\u30a6\u30a7\u30d6\u8abf\u67fb", "\u30a4\u30f3\u30bf\u30fc\u30cd\u30c3\u30c8\u8abf\u67fb",
    "\u81ea\u8a18\u5f0f", "\u4ed6\u8a18\u5f0f",
    # Headings
    "\u7b2c\\d+\u7ae0", "\u7b2c\\d+\u7bc0"
  )

  # Score each page by keyword density
  page_scores <- vapply(seq_along(pages), function(i) {
    p <- pages[[i]]
    # Count keyword matches
    hits <- sum(vapply(section_keywords, function(kw) {
      grepl(kw, p, perl = TRUE)
    }, logical(1)))
    # First and last pages often have key info
    bonus <- if (i <= 2 || i >= length(pages) - 1) 2L else 0L
    hits + bonus
  }, integer(1))

  # Always include first 2 pages (title, overview)
  selected_pages <- seq_len(min(2, length(pages)))
  current_chars <- sum(nchar(pages[selected_pages]))

  # Add pages by score (descending), respecting budget
  ranked <- order(page_scores, decreasing = TRUE)
  for (idx in ranked) {
    if (idx %in% selected_pages) next
    page_chars <- nchar(pages[[idx]])
    if (current_chars + page_chars > max_chars) next
    selected_pages <- c(selected_pages, idx)
    current_chars <- current_chars + page_chars
  }

  # Sort pages back to original order
  selected_pages <- sort(unique(selected_pages))

  # Build output with page markers
  sections <- vapply(selected_pages, function(i) {
    paste0("[Page ", i, "]\n", pages[[i]])
  }, character(1))

  result <- paste(sections, collapse = "\n\n--- Page Break ---\n\n")

  n_total <- length(pages)
  n_selected <- length(selected_pages)
  if (n_selected < n_total) {
    cli::cli_alert_info(
      "Degraded mode: selected {n_selected}/{n_total} pages (~{estimate_tokens(result)} tokens)"
    )
  }

  result
}

#' Encode a file as base64
#'
#' @param file_path Character. Path to file.
#' @return Character string of base64-encoded content.
#' @keywords internal
file_to_base64 <- function(file_path) {
  raw_bytes <- readBin(file_path, "raw", file.info(file_path)$size)
  jsonlite::base64_enc(raw_bytes)
}

#' Layout-aware PDF text extraction
#'
#' Uses [pdftools::pdf_data()] to extract word positions and reconstruct
#' text with proper reading order. Handles multi-column layouts and tables
#' better than [pdftools::pdf_text()] for some PDFs.
#'
#' @param file_path Character. Path to PDF file.
#' @return Character string of extracted text, or NULL on failure.
#' @keywords internal
extract_text_pdf_layout <- function(file_path) {
  page_data <- tryCatch(
    pdftools::pdf_data(file_path),
    error = function(e) NULL
  )

  if (is.null(page_data) || length(page_data) == 0) {
    return(NULL)
  }

  pages_text <- vapply(page_data, function(df) {
    if (nrow(df) == 0) return("")

    # Sort by y (top to bottom), then x (left to right)
    df <- df[order(df$y, df$x), ]

    # Group words into lines: words within 3px vertically = same line
    df$line_group <- cumsum(c(0, diff(df$y) > 3))

    lines <- tapply(seq_len(nrow(df)), df$line_group, function(idx) {
      words <- df[idx, ]
      words <- words[order(words$x), ]

      # Build line: large x-gaps become tabs (column separator)
      result <- words$text[1]
      if (nrow(words) > 1) {
        for (i in 2:nrow(words)) {
          gap <- words$x[i] - (words$x[i - 1] + words$width[i - 1])
          sep <- if (gap > 30) "\t" else if (words$space[i - 1]) " " else ""
          result <- paste0(result, sep, words$text[i])
        }
      }
      result
    })

    paste(lines, collapse = "\n")
  }, character(1))

  paste(pages_text, collapse = "\n\n--- Page Break ---\n\n")
}

#' Build content parts for Gemini API
#'
#' Creates a list of Gemini-format content parts mixing text and inline PDF
#' data for sending to the Gemini API.
#'
#' @param file_paths Character vector. Paths to documents.
#' @param text_files Character vector. Subset of file_paths to send as text.
#'   Files not in text_files are sent as inline_data PDF blocks.
#' @param extracted_texts Named character vector. Pre-extracted text keyed by
#'   file path, used for files in text_files.
#'
#' @return A list of Gemini content parts.
#' @keywords internal
build_gemini_parts <- function(file_paths, text_files = character(0),
                                extracted_texts = NULL) {
  parts <- list(
    list(
      text = paste0(
        "\u4ee5\u4e0b\u306e\u8abf\u67fb\u6587\u66f8\u304b\u3089",
        "\u30e1\u30bf\u30c7\u30fc\u30bf\u3092\u62bd\u51fa\u3057\u3066\u304f\u3060\u3055\u3044\u3002"
      )
    )
  )

  for (f in file_paths) {
    ext <- tolower(tools::file_ext(f))

    if (f %in% text_files) {
      # Send as extracted text
      txt <- if (!is.null(extracted_texts) && f %in% names(extracted_texts)) {
        extracted_texts[[f]]
      } else {
        extract_text(f)
      }
      parts <- c(parts, list(list(
        text = paste0("=== ", basename(f), " ===\n\n", txt)
      )))
    } else if (ext == "pdf") {
      # Send PDF as inline_data (Gemini)
      pdf_info <- pdftools::pdf_info(f)
      n_pages <- pdf_info$pages
      cli::cli_alert_info(
        "Sending {.file {basename(f)}} as inline PDF ({n_pages} pages, ~{n_pages * 258} tokens)"
      )
      b64 <- file_to_base64(f)
      parts <- c(parts, list(list(
        inline_data = list(
          mime_type = "application/pdf",
          data = b64
        )
      )))
    } else {
      # Non-PDF: always send as text
      txt <- extract_text(f)
      parts <- c(parts, list(list(
        text = paste0("=== ", basename(f), " ===\n\n", txt)
      )))
    }
  }

  parts
}

#' Build content blocks for Vision API
#'
#' Creates a list of content blocks mixing text and PDF document blocks
#' for sending to the Claude API with vision support.
#'
#' @param file_paths Character vector. Paths to documents.
#' @param text_files Character vector. Subset of file_paths to send as text.
#'   Files not in text_files are sent as base64 document blocks.
#' @param extracted_texts Named character vector. Pre-extracted text keyed by
#'   file path, used for files in text_files.
#'
#' @return A list of content blocks suitable for the Claude API messages.
#' @keywords internal
build_vision_content <- function(file_paths, text_files = character(0),
                                 extracted_texts = NULL) {
  content <- list(
    list(
      type = "text",
      text = paste0(
        "\u4ee5\u4e0b\u306e\u8abf\u67fb\u6587\u66f8\u304b\u3089",
        "\u30e1\u30bf\u30c7\u30fc\u30bf\u3092\u62bd\u51fa\u3057\u3066\u304f\u3060\u3055\u3044\u3002"
      )
    )
  )

  for (f in file_paths) {
    ext <- tolower(tools::file_ext(f))

    if (f %in% text_files) {
      # Send as extracted text
      txt <- if (!is.null(extracted_texts) && f %in% names(extracted_texts)) {
        extracted_texts[[f]]
      } else {
        extract_text(f)
      }
      content <- c(content, list(list(
        type = "text",
        text = paste0("=== ", basename(f), " ===\n\n", txt)
      )))
    } else if (ext == "pdf") {
      # Send PDF as document block (Claude Vision)
      pdf_info <- pdftools::pdf_info(f)
      n_pages <- pdf_info$pages
      cli::cli_alert_info(
        "Sending {.file {basename(f)}} as PDF document ({n_pages} pages, ~{n_pages * 1600} tokens)"
      )
      b64 <- file_to_base64(f)
      content <- c(content, list(list(
        type = "document",
        source = list(
          type = "base64",
          media_type = "application/pdf",
          data = b64
        )
      )))
    } else {
      # Non-PDF: always send as text
      txt <- extract_text(f)
      content <- c(content, list(list(
        type = "text",
        text = paste0("=== ", basename(f), " ===\n\n", txt)
      )))
    }
  }

  content
}
