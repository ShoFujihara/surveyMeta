#' LLM API Communication
#'
#' @description
#' Functions for communicating with LLM APIs (Anthropic Claude, Google Gemini).
#'
#' @name api
NULL

#' Detect LLM provider from model name
#'
#' @param model Character. Model identifier (e.g., "claude-sonnet-4-5-20250929",
#'   "gemini-2.0-flash").
#' @return Character. Either "anthropic" or "google".
#' @keywords internal
detect_provider <- function(model) {
  if (grepl("^claude-", model)) return("anthropic")
  if (grepl("^gemini-", model)) return("google")
  cli::cli_abort(
    "Unknown model provider for {.val {model}}. Expected {.val claude-*} or {.val gemini-*}."
  )
}

#' Define the metadata extraction tool schema
#'
#' Returns the tool definition for structured metadata extraction via
#' Claude's tool_use feature.
#'
#' @return A list defining the tool schema.
#' @keywords internal
metadata_tool_schema <- function() {
  list(
    name = "extract_survey_metadata",
    description = paste0(
      "\u8abf\u67fb\u6587\u66f8\u304b\u3089\u62bd\u51fa\u3057\u305f\u30e1\u30bf\u30c7\u30fc\u30bf\u3092\u69cb\u9020\u5316\u3057\u3066\u51fa\u529b\u3059\u308b\u3002",
      "\u5404\u30d5\u30a3\u30fc\u30eb\u30c9\u306f\u7c21\u6f54\u306b\u4e8b\u5b9f\u306e\u307f\u3092\u8a18\u8ff0\u3057\u3001",
      "\u7406\u7531\u30fb\u8aac\u660e\u30fb\u6839\u62e0\u306f\u4e00\u5207\u542b\u3081\u306a\u3044\u3053\u3068\u3002"
    ),
    input_schema = list(
      type = "object",
      required = c("title", "survey_overview", "data_type", "data_language",
                    "survey_target", "unit_of_analysis", "sample_size",
                    "survey_date", "geographic_coverage",
                    "sampling_procedure", "mode_of_collection",
                    "survey_conductor", "main_survey_items",
                    "cessda_topic", "ssjda_topic", "evidence"),
      properties = list(
        title = list(
          type = "string",
          description = paste0(
            "\u8abf\u67fb\u540d\u3002\u6587\u66f8\u306e\u30bf\u30a4\u30c8\u30eb\u30da\u30fc\u30b8\u3084\u8868\u7d19\u304b\u3089\u62bd\u51fa\u3002",
            "\u300c\u7b2cN\u56de\u300d\u300c20XX\u5e74\u300d\u7b49\u306e\u60c5\u5831\u304c\u3042\u308c\u3070\u542b\u3081\u308b\u3002",
            "\u4f8b\uff1a\u300c2015\u5e74 \u793e\u4f1a\u968e\u5c64\u3068\u793e\u4f1a\u79fb\u52d5\u5168\u56fd\u8abf\u67fb\uff08SSM\u8abf\u67fb\uff09\u300d"
          )
        ),
        survey_overview = list(
          type = "string",
          description = paste0(
            "\u8abf\u67fb\u306e\u76ee\u7684\u30fb\u80cc\u666f\u30922-3\u6587\u3067\u8981\u7d04\u3002",
            "\u300c\u672c\u8abf\u67fb\u306f\u3001\u25cb\u25cb\u304c\u25b3\u25b3\u5e74\u306b\u5b9f\u65bd\u3057\u305f\u3001",
            "\u25a1\u25a1\u3092\u5bfe\u8c61\u3068\u3057\u305f\u8abf\u67fb\u3067\u3042\u308a\u3001\u00d7\u00d7\u3092\u660e\u3089\u304b\u306b\u3059\u308b\u3082\u306e\u3067\u3042\u308b\u3002\u300d",
            "\u306e\u3088\u3046\u306a\u66f8\u304d\u65b9\u304c\u671b\u307e\u3057\u3044\u3002"
          )
        ),
        data_type = list(
          type = "string",
          enum = c(
            "\u91cf\u7684\u8abf\u67fb: \u30df\u30af\u30ed\u30c7\u30fc\u30bf",
            "\u91cf\u7684\u8abf\u67fb: \u30de\u30af\u30ed\u30c7\u30fc\u30bf",
            "\u91cf\u7684\u8abf\u67fb",
            "\u8cea\u7684\u8abf\u67fb",
            "\u5b98\u5e81\u7d71\u8a08: \u30df\u30af\u30ed\u30c7\u30fc\u30bf",
            "\u5b98\u5e81\u7d71\u8a08: \u30de\u30af\u30ed\u30c7\u30fc\u30bf",
            "\u305d\u306e\u4ed6"
          ),
          description = "\u30c7\u30fc\u30bf\u30bf\u30a4\u30d7\u3002\u307b\u3068\u3093\u3069\u306e\u793e\u4f1a\u8abf\u67fb\u30c7\u30fc\u30bf\u306f\u300c\u91cf\u7684\u8abf\u67fb: \u30df\u30af\u30ed\u30c7\u30fc\u30bf\u300d\u3002"
        ),
        data_language = list(
          type = "string",
          enum = c("jpn", "eng", "zho", "kor", "fra", "deu", "spa",
                   "por", "ara", "hin", "other"),
          description = paste0(
            "\u30c7\u30fc\u30bf\u306e\u8a00\u8a9e\u3002ISO 639-3\u30b3\u30fc\u30c9\u3067\u6307\u5b9a\u3002",
            "\u65e5\u672c\u8a9e\u306e\u8abf\u67fb\u306f\u300cjpn\u300d\u3001\u82f1\u8a9e\u306f\u300ceng\u300d\u3002"
          )
        ),
        survey_target = list(
          type = "string",
          description = "\u6bcd\u96c6\u56e3\u306e\u5b9a\u7fa9\uff08\u4f8b\uff1a\u300c\u5168\u56fd\u306e20\u6b73\u4ee5\u4e0a\u306e\u7537\u5973\u300d\uff09"
        ),
        unit_of_analysis = list(
          type = "string",
          enum = c("\u500b\u4eba", "\u4e16\u5e2f", "\u5bb6\u65cf", "\u7d44\u7e54",
                   "\u96c6\u56e3", "\u30a4\u30d9\u30f3\u30c8", "\u5730\u7406\u7684\u5358\u4f4d",
                   "\u305d\u306e\u4ed6"),
          description = "\u8abf\u67fb\u5bfe\u8c61\u306e\u5358\u4f4d"
        ),
        sample_size = list(
          type = "object",
          description = "\u8a08\u753b\u6a19\u672c\u6570\u3001\u6709\u52b9\u56de\u7b54\u6570\u3001\u56de\u53ce\u7387\u3002\u4e0d\u660e\u306a\u9805\u76ee\u306fnull\u3002",
          properties = list(
            planned = list(type = c("integer", "null"),
                          description = "\u8a08\u753b\u6a19\u672c\u6570"),
            valid_responses = list(type = c("integer", "null"),
                                   description = "\u6709\u52b9\u56de\u7b54\u6570"),
            response_rate = list(type = c("number", "null"),
                                 description = "\u56de\u53ce\u7387\uff08%\uff09")
          )
        ),
        survey_date = list(
          type = "string",
          description = "\u8abf\u67fb\u5b9f\u65bd\u671f\u9593\uff08\u4f8b\uff1a\u300c2023\u5e7410\u67081\u65e5\uff5e12\u670815\u65e5\u300d\uff09"
        ),
        reference_period = list(
          type = c("string", "null"),
          description = "\u30c7\u30fc\u30bf\u304c\u53c2\u7167\u3059\u308b\u6642\u671f\u3002\u8abf\u67fb\u6642\u70b9\u3068\u7570\u306a\u308b\u5834\u5408\u306e\u307f\u3002"
        ),
        geographic_coverage = list(
          type = "string",
          description = "\u5730\u7406\u7684\u7bc4\u56f2\u3002\u300c\u5168\u56fd\u300d\u3067\u306f\u306a\u304f\u300c\u65e5\u672c\u300d\u3068\u8a18\u8f09\u3002"
        ),
        sampling_procedure = list(
          type = "array",
          items = list(type = "string"),
          description = "\u6a19\u672c\u62bd\u51fa\u65b9\u6cd5\u3002\u7d71\u5236\u8a9e\u5f59\u306e\u65e5\u672c\u8a9e\u30e9\u30d9\u30eb\u304b\u3089\u9078\u629e\u3002"
        ),
        mode_of_collection = list(
          type = "array",
          items = list(type = "string"),
          description = "\u30c7\u30fc\u30bf\u53ce\u96c6\u65b9\u6cd5\u3002\u7d71\u5236\u8a9e\u5f59\u306e\u65e5\u672c\u8a9e\u30e9\u30d9\u30eb\u304b\u3089\u9078\u629e\u3002"
        ),
        survey_conductor = list(
          type = "string",
          description = "\u8abf\u67fb\u5b9f\u65bd\u6a5f\u95a2\u3002\u5f62\u5f0f\uff1a\u300c\u25cb\u25cb\uff0c\u5b9f\u67fb\u306f\u25b3\u25b3\u300d"
        ),
        main_survey_items = list(
          type = "array",
          items = list(type = "string"),
          description = "\u8abf\u67fb\u7968\u306e\u8cea\u554f\u9805\u76ee\u3092\u7db2\u7f85\u7684\u306b\u5217\u6319\u3002\u5404\u9805\u76ee\u306f\u5909\u6570\u540d\u3068\u8cea\u554f\u5185\u5bb9\u3092\u542b\u3080\u3002"
        ),
        cessda_topic = list(
          type = "array",
          items = list(type = "string"),
          description = "CESSDA Topic Classification v2.0\u306e\u65e5\u672c\u8a9e\u30e9\u30d9\u30eb\u304b\u3089\u9078\u629e\u3002"
        ),
        ssjda_topic = list(
          type = "array",
          items = list(type = "string"),
          description = "SSJDA\u30aa\u30ea\u30b8\u30ca\u30eb\u30c8\u30d4\u30c3\u30af\u306e\u65e5\u672c\u8a9e\u30e9\u30d9\u30eb\u304b\u3089\u9078\u629e\u3002"
        ),
        evidence = list(
          type = "object",
          description = paste0(
            "\u5404\u30e1\u30bf\u30c7\u30fc\u30bf\u30d5\u30a3\u30fc\u30eb\u30c9\u306e\u62bd\u51fa\u6839\u62e0\u3002",
            "\u30d5\u30a3\u30fc\u30eb\u30c9\u540d\u3092\u30ad\u30fc\u3068\u3057\u3001",
            "\u539f\u6587\u304b\u3089\u306e\u5f15\u7528\uff08200\u5b57\u4ee5\u5185\uff09\u3068\u30da\u30fc\u30b8\u756a\u53f7\u3092\u5024\u3068\u3059\u308b\u3002",
            "\u5f15\u7528\u306e\u672b\u5c3e\u306b\u300c\uff08p.X\uff09\u300d\u306e\u5f62\u5f0f\u3067\u30da\u30fc\u30b8\u756a\u53f7\u3092\u4ed8\u8a18\u3002",
            "\u6587\u66f8\u306b\u660e\u793a\u7684\u306a\u8a18\u8f09\u304c\u306a\u3044\u30d5\u30a3\u30fc\u30eb\u30c9\u306fnull\u3002"
          ),
          properties = list(
            title = list(type = c("string", "null")),
            survey_overview = list(type = c("string", "null")),
            data_type = list(type = c("string", "null")),
            data_language = list(type = c("string", "null")),
            survey_target = list(type = c("string", "null")),
            unit_of_analysis = list(type = c("string", "null")),
            sample_size = list(type = c("string", "null")),
            survey_date = list(type = c("string", "null")),
            reference_period = list(type = c("string", "null")),
            geographic_coverage = list(type = c("string", "null")),
            sampling_procedure = list(type = c("string", "null")),
            mode_of_collection = list(type = c("string", "null")),
            survey_conductor = list(type = c("string", "null")),
            main_survey_items = list(type = c("string", "null")),
            cessda_topic = list(type = c("string", "null")),
            ssjda_topic = list(type = c("string", "null"))
          )
        )
      )
    )
  )
}

#' Call Claude API
#'
#' Send a message to the Claude API and return the response.
#' Supports both free-form text and structured tool_use output.
#'
#' @param system_prompt Character. System prompt.
#' @param user_message Character. User message (document text). Used when
#'   sending plain text. Ignored if \code{user_content} is provided.
#' @param user_content List. Structured content blocks for the user message
#'   (e.g., text + PDF document blocks for Vision mode). If provided,
#'   \code{user_message} is ignored.
#' @param api_key Character. Anthropic API key.
#' @param model Character. Model identifier.
#' @param max_tokens Integer. Maximum tokens in response.
#' @param temperature Numeric. Sampling temperature (0 for deterministic).
#' @param use_tool_use Logical. If TRUE, use tool_use for structured output.
#'
#' @return Character string of the API response content (text mode), or
#'   a named list (tool_use mode).
#' @keywords internal
call_claude <- function(system_prompt,
                        user_message = NULL,
                        user_content = NULL,
                        api_key = Sys.getenv("ANTHROPIC_API_KEY"),
                        model = "claude-sonnet-4-5-20250929",
                        max_tokens = 8192L,
                        temperature = 0,
                        use_tool_use = TRUE,
                        tool_schema = NULL) {


  if (is.null(user_message) && is.null(user_content)) {
    cli::cli_abort("Either {.arg user_message} or {.arg user_content} must be provided.")
  }

  if (nchar(api_key) == 0) {
    cli::cli_abort(c(
      "Anthropic API key not found.",
      "i" = "Set {.envvar ANTHROPIC_API_KEY} or pass {.arg api_key} directly."
    ))
  }

  # Build system prompt with cache_control for prompt caching
  system_content <- list(
    list(
      type = "text",
      text = system_prompt,
      cache_control = list(type = "ephemeral")
    )
  )

  # Build user message: structured content blocks or plain text
  if (!is.null(user_content)) {
    msg_content <- user_content
  } else {
    msg_content <- user_message
  }

  # Detect if we're sending PDF document blocks (need beta header)
  has_pdf_docs <- FALSE
  if (is.list(msg_content)) {
    has_pdf_docs <- any(vapply(msg_content, function(block) {
      identical(block$type, "document")
    }, logical(1)))
  }

  body <- list(
    model = model,
    max_tokens = max_tokens,
    temperature = temperature,
    system = system_content,
    messages = list(
      list(role = "user", content = msg_content)
    )
  )

  # Add tool_use if requested
  if (use_tool_use) {
    schema <- tool_schema %||% metadata_tool_schema()
    body$tools <- list(schema)
    body$tool_choice <- list(type = "tool", name = schema$name)
  }

  # Build beta header: prompt caching + PDF support if needed
  beta_features <- "prompt-caching-2024-07-31"
  if (has_pdf_docs) {
    beta_features <- paste(beta_features, "pdfs-2024-09-25", sep = ",")
  }

  resp <- httr2::request("https://api.anthropic.com/v1/messages") |>
    httr2::req_headers(
      `x-api-key` = api_key,
      `anthropic-version` = "2023-06-01",
      `anthropic-beta` = beta_features,
      `content-type` = "application/json"
    ) |>
    httr2::req_body_json(body, auto_unbox = TRUE) |>
    httr2::req_timeout(300) |>
    httr2::req_error(body = function(resp) {
      b <- httr2::resp_body_json(resp)
      b$error$message %||% "Unknown API error"
    }) |>
    httr2::req_perform()

  result <- httr2::resp_body_json(resp)

  # Report cache usage if available
  usage <- result$usage
  if (!is.null(usage$cache_creation_input_tokens) ||
      !is.null(usage$cache_read_input_tokens)) {
    cache_write <- usage$cache_creation_input_tokens %||% 0
    cache_read <- usage$cache_read_input_tokens %||% 0
    cli::cli_alert_info(
      "Token usage: input={usage$input_tokens}, output={usage$output_tokens}, cache_write={cache_write}, cache_read={cache_read}"
    )
  }

  # Build usage info for cost calculation
  usage_info <- list(
    input_tokens = usage$input_tokens %||% 0L,
    output_tokens = usage$output_tokens %||% 0L,
    cache_creation_input_tokens = usage$cache_creation_input_tokens %||% 0L,
    cache_read_input_tokens = usage$cache_read_input_tokens %||% 0L
  )

  content_blocks <- result$content

  attach_usage <- function(obj) {
    attr(obj, "usage") <- usage_info
    obj
  }

  if (use_tool_use) {
    # Extract tool_use block
    tool_blocks <- Filter(function(x) x$type == "tool_use", content_blocks)
    if (length(tool_blocks) == 0) {
      cli::cli_warn("No tool_use response; falling back to text parsing.")
      text_blocks <- Filter(function(x) x$type == "text", content_blocks)
      if (length(text_blocks) == 0) {
        cli::cli_abort("No content in API response.")
      }
      return(attach_usage(
        paste(vapply(text_blocks, function(x) x$text, character(1)),
              collapse = "\n")
      ))
    }
    return(attach_usage(tool_blocks[[1]]$input))
  }

  # Text mode
  text_blocks <- Filter(function(x) x$type == "text", content_blocks)
  if (length(text_blocks) == 0) {
    cli::cli_abort("No text content in API response.")
  }
  attach_usage(
    paste(vapply(text_blocks, function(x) x$text, character(1)), collapse = "\n")
  )
}

#' Convert metadata schema to Gemini response_json_schema format
#'
#' Adapts the Claude tool_use schema for Gemini's structured output.
#' Gemini's response_json_schema does not support union types like
#' \code{type: ["string", "null"]}, so these are simplified.
#'
#' @return A list suitable for Gemini's \code{response_json_schema}.
#' @keywords internal
gemini_response_schema <- function() {
  schema <- metadata_tool_schema()$input_schema

  # Recursively clean type arrays (Gemini doesn't support ["string", "null"])
  clean_types <- function(obj) {
    if (!is.list(obj)) return(obj)
    if (!is.null(obj$type) && length(obj$type) > 1) {
      obj$type <- setdiff(obj$type, "null")[1]
    }
    if (!is.null(obj$properties)) {
      obj$properties <- lapply(obj$properties, clean_types)
    }
    if (!is.null(obj$items)) {
      obj$items <- clean_types(obj$items)
    }
    # Remove Claude-specific description field from evidence sub-properties
    obj
  }

  clean_types(schema)
}

#' Define the variable extraction tool schema
#'
#' Returns the tool definition for extracting variable-level metadata
#' (variable labels, value labels) from questionnaire documents.
#'
#' @return A list defining the tool schema.
#' @keywords internal
variable_tool_schema <- function() {
  list(
    name = "extract_survey_variables",
    description = paste0(
      "\u8abf\u67fb\u7968\u304b\u3089\u5909\u6570\u30e9\u30d9\u30eb\u30fb",
      "\u5024\u30e9\u30d9\u30eb\u3092\u69cb\u9020\u5316\u3057\u3066\u62bd\u51fa\u3059\u308b\u3002"
    ),
    input_schema = list(
      type = "object",
      required = c("variables"),
      properties = list(
        variables = list(
          type = "array",
          description = paste0(
            "\u8abf\u67fb\u7968\u306e\u5168\u8cea\u554f\u9805\u76ee\u3002",
            "\u5404\u8981\u7d20\u306f1\u3064\u306e\u5909\u6570\u306b\u5bfe\u5fdc\u3002"
          ),
          items = list(
            type = "object",
            required = c("question_number", "label", "type", "values"),
            properties = list(
              question_number = list(
                type = "string",
                description = paste0(
                  "\u8cea\u554f\u756a\u53f7\uff08\u4f8b: \"Q1\", \"Q2-1\", \"F1\"\uff09\u3002",
                  "\u30de\u30c8\u30ea\u30af\u30b9\u8cea\u554f\u306f\u5c55\u958b\u3057\u3066",
                  "\u500b\u5225\u306e\u756a\u53f7\u3092\u4ed8\u4e0e\u3002"
                )
              ),
              label = list(
                type = "string",
                description = paste0(
                  "\u5909\u6570\u30e9\u30d9\u30eb\uff08\u8cea\u554f\u6587\u306e\u8981\u7d04\uff09\u3002",
                  "30\u5b57\u7a0b\u5ea6\u3067\u7c21\u6f54\u306b\u3002"
                )
              ),
              type = list(
                type = "string",
                enum = c("single", "multiple", "numeric", "text", "matrix",
                         "scale", "ranking", "other"),
                description = paste0(
                  "\u8cea\u554f\u30bf\u30a4\u30d7\u3002",
                  "single=\u5358\u4e00\u9078\u629e, multiple=\u8907\u6570\u9078\u629e, ",
                  "numeric=\u6570\u5024\u5165\u529b, text=\u81ea\u7531\u8a18\u8ff0, ",
                  "matrix=\u30de\u30c8\u30ea\u30af\u30b9, scale=\u5c3a\u5ea6, ",
                  "ranking=\u9806\u4f4d, other=\u305d\u306e\u4ed6"
                )
              ),
              values = list(
                type = "array",
                description = paste0(
                  "\u5024\u30e9\u30d9\u30eb\u3002\u5404\u8981\u7d20\u306fcode\u3068label\u306e\u30da\u30a2\u3002",
                  "\u6570\u5024\u5165\u529b\u30fb\u81ea\u7531\u8a18\u8ff0\u306f\u7a7a\u914d\u5217\u3002"
                ),
                items = list(
                  type = "object",
                  required = c("code", "label"),
                  properties = list(
                    code = list(
                      type = c("integer", "null"),
                      description = "\u30b3\u30fc\u30c9\u5024\uff08\u6574\u6570\uff09"
                    ),
                    label = list(
                      type = "string",
                      description = "\u5024\u30e9\u30d9\u30eb"
                    )
                  )
                )
              ),
              condition = list(
                type = c("string", "null"),
                description = paste0(
                  "\u56de\u7b54\u6761\u4ef6\u30fb\u30b9\u30ad\u30c3\u30d7\u6761\u4ef6\u3002",
                  "\u4f8b: \"Q3=1\u306e\u65b9\u306e\u307f\"\u3002\u306a\u3051\u308c\u3070null\u3002"
                )
              ),
              page = list(
                type = c("integer", "null"),
                description = "\u8abf\u67fb\u7968\u306e\u30da\u30fc\u30b8\u756a\u53f7"
              )
            )
          )
        )
      )
    )
  )
}

#' Convert variable schema to Gemini format
#' @keywords internal
gemini_variable_schema <- function() {
  schema <- variable_tool_schema()$input_schema

  clean_types <- function(obj) {
    if (!is.list(obj)) return(obj)
    if (!is.null(obj$type) && length(obj$type) > 1) {
      obj$type <- setdiff(obj$type, "null")[1]
    }
    if (!is.null(obj$properties)) {
      obj$properties <- lapply(obj$properties, clean_types)
    }
    if (!is.null(obj$items)) {
      obj$items <- clean_types(obj$items)
    }
    obj
  }

  clean_types(schema)
}

#' Call Google Gemini API
#'
#' Send a request to the Gemini API with structured JSON output.
#'
#' @param system_prompt Character. System instruction.
#' @param user_message Character. Plain text user message. Ignored if
#'   \code{user_parts} is provided.
#' @param user_parts List. Gemini-format content parts (text + inline_data).
#'   If provided, \code{user_message} is ignored.
#' @param api_key Character. Google API key.
#' @param model Character. Gemini model identifier.
#' @param max_tokens Integer. Maximum output tokens.
#' @param temperature Numeric. Sampling temperature.
#'
#' @return A named list of extracted metadata (parsed JSON), with a
#'   \code{usage} attribute.
#' @keywords internal
call_gemini <- function(system_prompt,
                        user_message = NULL,
                        user_parts = NULL,
                        api_key = Sys.getenv("GEMINI_API_KEY"),
                        model = "gemini-2.0-flash",
                        max_tokens = 8192L,
                        temperature = 0,
                        response_schema = NULL) {

  if (is.null(user_message) && is.null(user_parts)) {
    cli::cli_abort("Either {.arg user_message} or {.arg user_parts} must be provided.")
  }

  if (nchar(api_key) == 0) {
    cli::cli_abort(c(
      "Google API key not found.",
      "i" = "Set {.envvar GEMINI_API_KEY} or pass {.arg api_key} directly."
    ))
  }

  # Build user content parts
  if (!is.null(user_parts)) {
    parts <- user_parts
  } else {
    parts <- list(list(text = user_message))
  }

  body <- list(
    system_instruction = list(
      parts = list(list(text = system_prompt))
    ),
    contents = list(
      list(
        role = "user",
        parts = parts
      )
    ),
    generation_config = list(
      temperature = temperature,
      max_output_tokens = max_tokens,
      response_mime_type = "application/json",
      response_json_schema = response_schema %||% gemini_response_schema()
    )
  )

  endpoint <- paste0(
    "https://generativelanguage.googleapis.com/v1beta/models/",
    model, ":generateContent"
  )

  resp <- httr2::request(endpoint) |>
    httr2::req_headers(
      `x-goog-api-key` = api_key,
      `content-type` = "application/json"
    ) |>
    httr2::req_body_json(body, auto_unbox = TRUE) |>
    httr2::req_timeout(300) |>
    httr2::req_error(body = function(resp) {
      b <- httr2::resp_body_json(resp)
      b$error$message %||% "Unknown API error"
    }) |>
    httr2::req_perform()

  result <- httr2::resp_body_json(resp)

  # Extract usage metadata
  usage_meta <- result$usageMetadata
  usage_info <- list(
    input_tokens = usage_meta$promptTokenCount %||% 0L,
    output_tokens = usage_meta$candidatesTokenCount %||% 0L,
    cache_creation_input_tokens = 0L,
    cache_read_input_tokens = 0L
  )

  cli::cli_alert_info(
    "Token usage: input={usage_info$input_tokens}, output={usage_info$output_tokens}"
  )

  # Check candidates
  candidates <- result$candidates
  if (is.null(candidates) || length(candidates) == 0) {
    cli::cli_abort("No candidates in Gemini API response.")
  }

  finish_reason <- candidates[[1]]$finishReason
  if (!identical(finish_reason, "STOP")) {
    cli::cli_warn("Gemini finish reason: {.val {finish_reason}}")
  }

  text <- candidates[[1]]$content$parts[[1]]$text
  if (is.null(text)) {
    cli::cli_abort("No text content in Gemini API response.")
  }

  # Parse JSON response
  parsed <- tryCatch(
    jsonlite::fromJSON(text, simplifyVector = FALSE),
    error = function(e) {
      cli::cli_abort(c(
        "Failed to parse Gemini JSON response.",
        "i" = e$message
      ))
    }
  )

  attr(parsed, "usage") <- usage_info
  parsed
}
