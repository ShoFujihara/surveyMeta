# Tests for variables.R

test_that("variables_to_dataframe handles single choice variable", {
  vars <- list(
    list(
      question_number = "Q1",
      label = "性別",
      type = "single",
      values = list(
        list(code = 1, label = "男性"),
        list(code = 2, label = "女性")
      ),
      condition = NULL,
      page = 1
    )
  )

  df <- variables_to_dataframe(vars)
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 2)
  expect_equal(df$question_number, c("Q1", "Q1"))
  expect_equal(df$code, c(1L, 2L))
  expect_equal(df$value_label, c("男性", "女性"))
  expect_equal(df$type, c("single", "single"))
})

test_that("variables_to_dataframe handles numeric variable (no values)", {
  vars <- list(
    list(
      question_number = "Q2",
      label = "年齢",
      type = "numeric",
      values = list(),
      condition = NULL,
      page = 1
    )
  )

  df <- variables_to_dataframe(vars)
  expect_equal(nrow(df), 1)
  expect_true(is.na(df$code))
  expect_true(is.na(df$value_label))
})

test_that("variables_to_dataframe handles condition and page", {
  vars <- list(
    list(
      question_number = "Q3SQ",
      label = "学部名",
      type = "text",
      values = list(),
      condition = "Q3=4の方のみ",
      page = 3
    )
  )

  df <- variables_to_dataframe(vars)
  expect_equal(df$condition, "Q3=4の方のみ")
  expect_equal(df$page, 3L)
})

test_that("variables_to_dataframe handles multiple variables", {
  vars <- list(
    list(question_number = "Q1", label = "性別", type = "single",
         values = list(list(code = 1, label = "男"), list(code = 2, label = "女")),
         condition = NULL, page = 1),
    list(question_number = "Q2", label = "年齢", type = "numeric",
         values = list(), condition = NULL, page = 1),
    list(question_number = "Q3", label = "学歴", type = "single",
         values = list(list(code = 1, label = "中学"), list(code = 2, label = "高校"),
                       list(code = 3, label = "大学")),
         condition = NULL, page = 2)
  )

  df <- variables_to_dataframe(vars)
  expect_equal(nrow(df), 6)  # 2 + 1 + 3
  expect_equal(length(unique(df$question_number)), 3)
})

test_that("dataframe_to_variables_list roundtrips correctly", {
  vars_in <- list(
    list(question_number = "Q1", label = "性別", type = "single",
         values = list(list(code = 1L, label = "男性"), list(code = 2L, label = "女性")),
         condition = NULL, page = NULL)
  )

  df <- variables_to_dataframe(vars_in)
  vars_out <- dataframe_to_variables_list(df)

  expect_equal(length(vars_out), 1)
  expect_equal(vars_out[[1]]$question_number, "Q1")
  expect_equal(vars_out[[1]]$label, "性別")
  expect_equal(length(vars_out[[1]]$values), 2)
  expect_equal(vars_out[[1]]$values[[1]]$code, 1L)
  expect_null(vars_out[[1]]$condition)
})

test_that("variable_tool_schema returns valid schema", {
  schema <- variable_tool_schema()
  expect_equal(schema$name, "extract_survey_variables")
  expect_true("variables" %in% schema$input_schema$required)
  expect_true("items" %in% names(schema$input_schema$properties$variables))
})

test_that("gemini_variable_schema strips null types", {
  schema <- gemini_variable_schema()
  # Check that condition type is "string" not c("string", "null")
  item_props <- schema$properties$variables$items$properties
  expect_equal(length(item_props$condition$type), 1)
})
