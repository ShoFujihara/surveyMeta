test_that("load_vocabulary works for all vocabularies", {
  vocabs <- c("cessda_topics", "ssjda_topics",
              "sampling_procedures", "collection_modes")
  for (v in vocabs) {
    df <- load_vocabulary(v)
    expect_s3_class(df, "data.frame")
    expect_gt(nrow(df), 0)
  }
})

test_that("build_system_prompt returns non-empty string", {
  prompt <- build_system_prompt()
  expect_type(prompt, "character")
  expect_gt(nchar(prompt), 1000)
  # Check all placeholders were replaced

  expect_false(grepl("\\{\\{", prompt))
})

test_that("parse_metadata_json handles valid JSON", {
  json <- '{"survey_overview": "test", "data_type": "量的調査"}'
  result <- parse_metadata_json(json)
  expect_type(result, "list")
  expect_equal(result$survey_overview, "test")
})

test_that("parse_metadata_json strips code fences", {
  json <- '```json\n{"survey_overview": "test"}\n```'
  result <- parse_metadata_json(json)
  expect_equal(result$survey_overview, "test")
})

test_that("metadata_to_dataframe flattens correctly", {
  meta <- list(
    survey_overview = "test survey",
    cessda_topic = c("1", "2.1"),
    sample_size = list(planned = 1000, valid_responses = 800, response_rate = 0.8)
  )
  df <- metadata_to_dataframe(meta)
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 1)
  expect_true(grepl(";", df$cessda_topic))
})
