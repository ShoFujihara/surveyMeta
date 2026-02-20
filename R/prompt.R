#' Prompt Construction
#'
#' @description
#' Build the system prompt with embedded controlled vocabularies.
#'
#' @name prompt
NULL

#' Build the system prompt
#'
#' Load the system prompt template and embed all controlled vocabularies.
#'
#' @return Character string of the complete system prompt.
#' @export
build_system_prompt <- function() {
  # Load template

template_path <- system.file("prompts", "system_prompt.txt",
                                package = "surveyMeta")
  if (template_path == "") {
    cli::cli_abort("System prompt template not found.")
  }
  template <- readr::read_file(template_path)

  # Load and format vocabularies
  cessda <- load_vocabulary("cessda_topics")
  ssjda <- load_vocabulary("ssjda_topics")
  sampling <- load_vocabulary("sampling_procedures")
  modes <- load_vocabulary("collection_modes")

  # Replace placeholders
  template <- gsub("\\{\\{CESSDA_TOPICS\\}\\}",
                    format_vocabulary_for_prompt(cessda), template)
  template <- gsub("\\{\\{SSJDA_TOPICS\\}\\}",
                    format_vocabulary_for_prompt(ssjda, "code", "label_ja"),
                    template)
  template <- gsub("\\{\\{SAMPLING_PROCEDURES\\}\\}",
                    format_vocabulary_for_prompt(sampling, "label_ja", "code"),
                    template)
  template <- gsub("\\{\\{COLLECTION_MODES\\}\\}",
                    format_vocabulary_for_prompt(modes, "label_ja", "code"),
                    template)

  template
}
