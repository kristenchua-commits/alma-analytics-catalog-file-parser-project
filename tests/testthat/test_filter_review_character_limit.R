project_root <- normalizePath(file.path(testthat::test_path(), "..", ".."), mustWork = TRUE)
source(file.path(project_root, "R", "export", "export_filter_review.R"))

testthat::test_that("short filter value lists remain inline", {
  values <- sprintf("'%02d'", seq_len(13L))
  criterion <- format_filter_list_criterion("field_name", "IN", values)

  testthat::expect_match(criterion, "'13'", fixed = TRUE)
  testthat::expect_false(grepl("see Value List Summary", criterion, fixed = TRUE))
})

testthat::test_that("the filter review threshold is 32,767 characters", {
  prefix_and_suffix_length <- nchar("f IN ()", type = "chars")
  exact_value <- strrep(
    "x",
    filter_review_cell_character_limit - prefix_and_suffix_length
  )
  over_value <- paste0(exact_value, "x")

  exact_criterion <- format_filter_list_criterion("f", "IN", exact_value)
  over_criterion <- format_filter_list_criterion("f", "IN", over_value)

  testthat::expect_equal(
    nchar(exact_criterion, type = "chars"),
    filter_review_cell_character_limit
  )
  testthat::expect_match(exact_criterion, exact_value, fixed = TRUE)
  testthat::expect_identical(
    over_criterion,
    "f IN (1 values; see Value List Summary and companion CSV)"
  )
})
