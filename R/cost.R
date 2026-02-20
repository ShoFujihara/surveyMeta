#' Cost and TCO Calculation
#'
#' @description
#' Functions for calculating API cost and total cost of ownership (TCO)
#' for metadata extraction.
#'
#' @name cost
NULL

#' Model pricing table (USD per million tokens)
#'
#' @return A named list of pricing for supported models.
#' @keywords internal
model_pricing <- function() {
  list(
    # Anthropic Claude models
    "claude-sonnet-4-5-20250929" = list(
      input = 3.00,
      output = 15.00,
      cache_write = 3.75,
      cache_read = 0.30
    ),
    "claude-opus-4-6" = list(
      input = 15.00,
      output = 75.00,
      cache_write = 18.75,
      cache_read = 1.50
    ),
    "claude-haiku-4-5-20251001" = list(
      input = 0.80,
      output = 4.00,
      cache_write = 1.00,
      cache_read = 0.08
    ),
    # Google Gemini models
    "gemini-2.0-flash" = list(
      input = 0.10,
      output = 0.40,
      cache_write = 0,
      cache_read = 0
    ),
    "gemini-2.5-pro" = list(
      input = 1.25,
      output = 10.00,
      cache_write = 0,
      cache_read = 0
    ),
    "gemini-2.5-flash" = list(
      input = 0.15,
      output = 0.60,
      cache_write = 0,
      cache_read = 0
    )
  )
}

#' Calculate API cost from extraction result
#'
#' Computes the API cost in USD based on token usage and model pricing.
#'
#' @param metadata Named list from [extract_metadata()], with usage attributes.
#' @return A list with cost breakdown and total.
#' @export
cost_summary <- function(metadata) {
  model <- attr(metadata, "model")
  usage <- attr(metadata, "usage")
  elapsed <- attr(metadata, "elapsed_seconds")

  if (is.null(usage)) {
    cli::cli_abort(c(
      "No usage information found.",
      "i" = "Ensure metadata was created with the current version of {.fn extract_metadata}."
    ))
  }

  pricing <- model_pricing()
  model_price <- pricing[[model]]

  if (is.null(model_price)) {
    cli::cli_warn("Unknown model {.val {model}}; using Sonnet pricing as fallback.")
    model_price <- pricing[["claude-sonnet-4-5-20250929"]]
  }

  # Calculate costs (pricing is per million tokens)
  input_cost <- (usage$input_tokens / 1e6) * model_price$input
  output_cost <- (usage$output_tokens / 1e6) * model_price$output
  cache_write_cost <- (usage$cache_creation_input_tokens / 1e6) * model_price$cache_write
  cache_read_cost <- (usage$cache_read_input_tokens / 1e6) * model_price$cache_read
  total_cost <- input_cost + output_cost + cache_write_cost + cache_read_cost

  result <- list(
    model = model,
    input_tokens = usage$input_tokens,
    output_tokens = usage$output_tokens,
    cache_write_tokens = usage$cache_creation_input_tokens,
    cache_read_tokens = usage$cache_read_input_tokens,
    input_cost_usd = input_cost,
    output_cost_usd = output_cost,
    cache_write_cost_usd = cache_write_cost,
    cache_read_cost_usd = cache_read_cost,
    total_cost_usd = total_cost,
    elapsed_seconds = elapsed %||% NA_real_
  )

  class(result) <- "survey_cost_summary"
  result
}

#' @export
print.survey_cost_summary <- function(x, ...) {
  cli::cli_h2("API Cost Summary")
  cli::cli_text("Model: {.val {x$model}}")
  cli::cli_text("")

  cli::cli_h3("Token Usage")
  cli::cli_text("  Input:       {format(x$input_tokens, big.mark = ',')}")
  cli::cli_text("  Output:      {format(x$output_tokens, big.mark = ',')}")
  if (x$cache_write_tokens > 0 || x$cache_read_tokens > 0) {
    cli::cli_text("  Cache write: {format(x$cache_write_tokens, big.mark = ',')}")
    cli::cli_text("  Cache read:  {format(x$cache_read_tokens, big.mark = ',')}")
  }

  cli::cli_h3("Cost (USD)")
  cli::cli_text("  Input:       ${format(round(x$input_cost_usd, 4), nsmall = 4)}")
  cli::cli_text("  Output:      ${format(round(x$output_cost_usd, 4), nsmall = 4)}")
  if (x$cache_write_cost_usd > 0 || x$cache_read_cost_usd > 0) {
    cli::cli_text("  Cache write: ${format(round(x$cache_write_cost_usd, 4), nsmall = 4)}")
    cli::cli_text("  Cache read:  ${format(round(x$cache_read_cost_usd, 4), nsmall = 4)}")
  }
  cli::cli_text("  -----")
  cli::cli_text("  Total:       ${format(round(x$total_cost_usd, 4), nsmall = 4)}")

  if (!is.na(x$elapsed_seconds)) {
    cli::cli_text("")
    cli::cli_text("Processing time: {round(x$elapsed_seconds, 1)} seconds")
  }

  invisible(x)
}
