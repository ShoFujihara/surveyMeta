#' Fetch Official Metadata from SSJDA via OAI-PMH
#'
#' @description
#' Retrieve DDI 2.5 metadata from SSJDA Direct's OAI-PMH endpoint
#' and parse it into the same structure as \code{\link{extract_metadata}}.
#'
#' @name oai
NULL

#' Fetch official metadata for a survey from SSJDA
#'
#' Queries the SSJDA OAI-PMH endpoint and parses the DDI 2.5 XML response
#' into a named list matching the package's metadata schema.
#'
#' @param survey_id Character. SSJDA survey number (e.g. "0987").
#' @param lang Character. Language for metadata ("ja" or "en").
#'
#' @return A named list with fields matching the output of
#'   \code{\link{extract_metadata}}.
#'
#' @examples
#' \dontrun{
#' official <- fetch_official_metadata("0987")
#' official$survey_overview
#' official$cessda_topic
#' }
#'
#' @export
fetch_official_metadata <- function(survey_id, lang = "ja") {
  base_url <- "https://ssjda.iss.u-tokyo.ac.jp/Direct/oai2/"
  identifier <- paste0("oai:ssjda.iss.u-tokyo.ac.jp:", survey_id)

  resp <- httr2::request(base_url) |>
    httr2::req_url_query(
      verb = "GetRecord",
      metadataPrefix = "oai_ddi25",
      identifier = identifier
    ) |>
    httr2::req_timeout(30) |>
    httr2::req_perform()

  xml_text <- httr2::resp_body_string(resp)

  # Check for OAI-PMH error
  if (grepl("<error", xml_text)) {
    cli::cli_abort("Survey ID {.val {survey_id}} not found in SSJDA OAI-PMH.")
  }

  parse_ddi_xml(xml_text, lang = lang)
}

#' Parse DDI 2.5 XML into metadata list
#'
#' @param xml_text Character. Raw XML string from OAI-PMH response.
#' @param lang Character. Language ("ja" or "en").
#'
#' @return Named list of metadata fields.
#' @keywords internal
parse_ddi_xml <- function(xml_text, lang = "ja") {
  # Extract content using regex (avoids xml2 dependency)
  xl <- function(tag, attr_filter = NULL) {
    if (!is.null(attr_filter)) {
      pattern <- paste0("<", tag, "[^>]*", attr_filter, "[^>]*><!\\[CDATA\\[([^]]*?)\\]\\]><\\/", tag, ">")
      m <- regmatches(xml_text, regexpr(pattern, xml_text, perl = TRUE))
      if (length(m) == 0 || nchar(m) == 0) {
        pattern2 <- paste0("<", tag, "[^>]*", attr_filter, "[^>]*>([^<]*)<\\/", tag, ">")
        m <- regmatches(xml_text, regexpr(pattern2, xml_text, perl = TRUE))
      }
    } else {
      pattern <- paste0("<", tag, "><!\\[CDATA\\[([^]]*?)\\]\\]><\\/", tag, ">")
      m <- regmatches(xml_text, regexpr(pattern, xml_text, perl = TRUE))
      if (length(m) == 0 || nchar(m) == 0) {
        pattern2 <- paste0("<", tag, ">([^<]*)<\\/", tag, ">")
        m <- regmatches(xml_text, regexpr(pattern2, xml_text, perl = TRUE))
      }
    }
    if (length(m) == 0 || nchar(m) == 0) return(NULL)
    # Extract inner content
    gsub(".*><!\\[CDATA\\[|\\]\\]><.*|.*>([^<]*)<.*", "\\1", m, perl = TRUE)
  }

  # Extract all matches for a tag
  xl_all <- function(tag, attr_filter = NULL) {
    if (!is.null(attr_filter)) {
      pattern <- paste0("<", tag, "[^>]*", attr_filter, "[^>]*>(?:<!\\[CDATA\\[)?([^<\\]]*?)(?:\\]\\]>)?<\\/", tag, ">")
    } else {
      pattern <- paste0("<", tag, ">(?:<!\\[CDATA\\[)?([^<\\]]*?)(?:\\]\\]>)?<\\/", tag, ">")
    }
    m <- gregexpr(pattern, xml_text, perl = TRUE)
    matches <- regmatches(xml_text, m)[[1]]
    if (length(matches) == 0) return(NULL)
    gsub(paste0(".*<", tag, "[^>]*>(?:<!\\[CDATA\\[)?|(?:\\]\\]>)?<\\/", tag, ">.*"), "",
         matches, perl = TRUE)
  }

  lang_attr <- paste0('xml:lang="', lang, '"')

  # Split XML into language-specific stdyInfo blocks
  stdy_pattern <- paste0('<stdyInfo\\s+xml:lang="', lang, '">(.*?)</stdyInfo>')
  stdy_match <- regmatches(xml_text, regexpr(stdy_pattern, xml_text, perl = TRUE))
  stdy_block <- if (length(stdy_match) > 0) stdy_match[1] else xml_text

  method_pattern <- paste0('<method\\s+xml:lang="', lang, '">(.*?)</method>')
  method_match <- regmatches(xml_text, regexpr(method_pattern, xml_text, perl = TRUE))
  method_block <- if (length(method_match) > 0) method_match[1] else xml_text

  # Extract fields from language-specific blocks
  extract_from <- function(block, tag, attr_filter = NULL) {
    if (!is.null(attr_filter)) {
      pattern <- paste0("<", tag, "[^>]*", attr_filter, "[^>]*>(?:<!\\[CDATA\\[)?([^<\\]]*?)(?:\\]\\]>)?<\\/", tag, ">")
    } else {
      pattern <- paste0("<", tag, "[^>]*>(?:<!\\[CDATA\\[)?([^<\\]]*?)(?:\\]\\]>)?<\\/", tag, ">")
    }
    m <- regmatches(block, regexpr(pattern, block, perl = TRUE))
    if (length(m) == 0 || nchar(m) == 0) return(NULL)
    sub(paste0(".*<", tag, "[^>]*>(?:<!\\[CDATA\\[)?"), "",
        sub(paste0("(?:\\]\\]>)?<\\/", tag, ">.*"), "", m, perl = TRUE),
        perl = TRUE)
  }

  extract_all_from <- function(block, tag, attr_filter = NULL) {
    if (!is.null(attr_filter)) {
      pattern <- paste0("<", tag, "[^>]*", attr_filter, "[^>]*>(?:<!\\[CDATA\\[)?([^<\\]]*?)(?:\\]\\]>)?<\\/", tag, ">")
    } else {
      pattern <- paste0("<", tag, "[^>]*>(?:<!\\[CDATA\\[)?([^<\\]]*?)(?:\\]\\]>)?<\\/", tag, ">")
    }
    m <- gregexpr(pattern, block, perl = TRUE)
    matches <- regmatches(block, m)[[1]]
    if (length(matches) == 0) return(NULL)
    vapply(matches, function(x) {
      sub(paste0(".*<", tag, "[^>]*>(?:<!\\[CDATA\\[)?"), "",
          sub(paste0("(?:\\]\\]>)?<\\/", tag, ">.*"), "", x, perl = TRUE),
          perl = TRUE)
    }, character(1), USE.NAMES = FALSE)
  }

  # Survey overview (abstract)
  overview <- extract_from(stdy_block, "abstract", lang_attr)
  overview <- gsub("^\\s+|\\s+$", "", overview)

  # Data type
  data_type <- extract_from(stdy_block, "dataKind", lang_attr)

  # Survey target (universe)
  survey_target <- extract_from(stdy_block, "universe", lang_attr)

  # Unit of analysis
  unit <- extract_from(stdy_block, "anlyUnit", lang_attr)

  # Sample size (from respRate)
  resp_rate_raw <- extract_from(method_block, "respRate", lang_attr)
  resp_rate_raw <- if (!is.null(resp_rate_raw)) gsub("<br>|<br/>|<br />", "; ", resp_rate_raw) else NULL

  # Survey date (collDate)
  coll_start <- regmatches(method_block,
    regexpr('collDate[^>]*event="start"[^>]*date="([^"]+)"', method_block, perl = TRUE))
  coll_end <- regmatches(method_block,
    regexpr('collDate[^>]*event="end"[^>]*date="([^"]+)"', method_block, perl = TRUE))
  start_date <- if (length(coll_start) > 0) sub('.*date="([^"]+)".*', "\\1", coll_start) else NULL
  end_date <- if (length(coll_end) > 0) sub('.*date="([^"]+)".*', "\\1", coll_end) else NULL
  survey_date <- if (!is.null(start_date) && !is.null(end_date)) {
    paste0(start_date, "\uff5e", end_date)
  } else if (!is.null(start_date)) {
    start_date
  } else {
    NULL
  }

  # Geographic coverage
  geo <- extract_from(stdy_block, "geogCover", lang_attr)

  # Sampling procedure
  samp <- extract_from(method_block, "sampProc", lang_attr)

  # Mode of collection
  coll_mode <- extract_from(method_block, "collMode", lang_attr)

  # Survey conductor (AuthEnty)
  citation_pattern <- paste0('<citation\\s+xml:lang="', lang, '">(.*?)</citation>')
  citation_match <- regmatches(xml_text, regexpr(citation_pattern, xml_text, perl = TRUE))
  citation_block <- if (length(citation_match) > 0) citation_match[1] else xml_text
  conductor <- extract_from(citation_block, "AuthEnty", lang_attr)
  conductor <- if (!is.null(conductor)) gsub("^\\s+|\\s+$", "", conductor) else NULL

  # CESSDA topics
  cessda_topics <- extract_all_from(stdy_block, "topcClas", 'vocab="CESSDATopic"')

  # Build result
  list(
    survey_overview = overview,
    data_type = data_type,
    survey_target = survey_target,
    unit_of_analysis = unit,
    sample_size = resp_rate_raw,
    survey_date = survey_date,
    geographic_coverage = geo,
    sampling_procedure = samp,
    mode_of_collection = coll_mode,
    survey_conductor = conductor,
    cessda_topic = cessda_topics
  )
}
