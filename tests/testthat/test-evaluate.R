# Tests for evaluate.R

test_that("set_precision_recall_f1 handles basic cases", {
  # Perfect match
  res <- set_precision_recall_f1(c("a", "b"), c("a", "b"))
  expect_equal(res$precision, 1.0)
  expect_equal(res$recall, 1.0)
  expect_equal(res$f1, 1.0)

  # Partial match
  res <- set_precision_recall_f1(c("a", "b", "c"), c("a", "b"))
  expect_equal(res$precision, 1.0)
  expect_equal(res$recall, 2/3)

  # No match
  res <- set_precision_recall_f1(c("a"), c("b"))
  expect_equal(res$precision, 0.0)
  expect_equal(res$recall, 0.0)
  expect_equal(res$f1, 0.0)
})

test_that("set_precision_recall_f1 deduplicates predicted values", {
  # Regression: duplicate predictions should not inflate tp count

  res <- set_precision_recall_f1(c("a", "b"), c("a", "a", "a"))
  expect_equal(res$precision, 1.0)  # 1 unique predicted, 1 correct
  expect_equal(res$recall, 0.5)     # 1 of 2 reference found
  expect_lte(res$recall, 1.0)       # recall must never exceed 1
})

test_that("set_precision_recall_f1 deduplicates reference values", {
  res <- set_precision_recall_f1(c("a", "a", "b"), c("a", "b"))
  expect_equal(res$recall, 1.0)  # 2 unique reference, 2 found
})

test_that("set_precision_recall_f1 handles empty inputs", {
  # Both empty

  res <- set_precision_recall_f1(character(0), character(0))
  expect_equal(res$f1, 1.0)

  # Empty predicted
  res <- set_precision_recall_f1(c("a"), character(0))
  expect_equal(res$f1, 0.0)

  # Empty reference
  res <- set_precision_recall_f1(character(0), c("a"))
  expect_true(is.na(res$f1))
})

test_that("set_precision_recall_f1 is case-insensitive", {
  res <- set_precision_recall_f1(c("Hello", "WORLD"), c("hello", "world"))
  expect_equal(res$f1, 1.0)
})

test_that("jaccard_similarity returns 1 for identical strings", {
  expect_equal(jaccard_similarity("test string", "test string"), 1.0)
})

test_that("jaccard_similarity returns 0 for completely different strings", {
  sim <- jaccard_similarity("abc", "xyz")
  expect_equal(sim, 0.0)
})

test_that("jaccard_similarity handles Japanese text", {
  sim <- jaccard_similarity("日本全国", "日本全国の調査")
  expect_gt(sim, 0.0)
  expect_lte(sim, 1.0)
})

test_that("flatten_value handles various input types", {
  expect_true(is.na(flatten_value(NULL)))
  expect_true(is.na(flatten_value(character(0))))
  expect_true(is.na(flatten_value(list())))
  expect_equal(flatten_value("hello"), "hello")
  expect_equal(flatten_value(c("a", "b")), "a; b")
  expect_equal(flatten_value(list("x", "y")), "x; y")
})
