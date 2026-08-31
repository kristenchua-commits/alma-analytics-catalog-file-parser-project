project_root <- normalizePath(file.path(testthat::test_path(), "..", ".."), mustWork = TRUE)

testthat::test_that("all-review workbook includes inline and saved rows by type", {
  testthat::skip_if_not_installed("openxlsx2")
  testthat::skip_if_not_installed("readxl")

  input_dir <- tempfile("all-review-input-")
  dir.create(input_dir)
  on.exit(unlink(input_dir, recursive = TRUE), add = TRUE)
  output_file <- file.path(input_dir, "review.xlsx")

  write.csv(data.frame(
    column_record_id = c("C1", "C2"),
    column_source = c("inline", "standalone_saved_object"),
    stringsAsFactors = FALSE
  ), file.path(input_dir, "columns.csv"), row.names = FALSE, na = "")
  write.csv(data.frame(
    rule_id = c("RC1-C1-FORMULA", "SC1-R1"), rule_type = "Column",
    source_type = c("inline", "standalone_saved_object"),
    record_scope = c("report_inline", "standalone_saved_object"),
    rule_name = c("Inline Column", "Saved Bin"), rule_index = 1,
    rule_kind = c("formula", "when"), criterion_text = c("formula", "field = 'x'"),
    review_label = c("Calculated value", "x"), explanation = "",
    criterion_text_category = c("formula", "base formula"), join_operator = "",
    value_count = "", report_catalog_index = c(1, ""),
    report_title = c("Report A", ""), report_path = c("/reports/a", ""),
    report_column_index = c(1, ""), definition_catalog_index = c("", 2),
    definition_name = c("", "Saved Bin"), definition_path = c("", "/shared/bin"),
    stringsAsFactors = FALSE
  ), file.path(input_dir, "column_rules.csv"), row.names = FALSE, na = "")
  write.csv(data.frame(
    filter_record_id = "F1", filter_source = "inline", stringsAsFactors = FALSE
  ), file.path(input_dir, "filters.csv"), row.names = FALSE, na = "")
  write.csv(data.frame(
    rule_id = "RF1-R1", rule_type = "Filter", source_type = "inline",
    record_scope = "report_inline", rule_name = "Report A", rule_index = 1,
    criterion_text = "field = 'y'", review_label = "", explanation = "",
    criterion_text_category = "", join_operator = "and", value_count = 1,
    report_catalog_index = 1, report_title = "Report A", report_path = "/reports/a",
    report_filter_index = 1, definition_catalog_index = "", definition_name = "",
    definition_path = "", stringsAsFactors = FALSE
  ), file.path(input_dir, "filter_rules.csv"), row.names = FALSE, na = "")
  write.csv(data.frame(
    rule_id = "RF1-R1", source_type = "inline", rule_name = "Report A",
    field = "field", value_index = 1, value = "y", stringsAsFactors = FALSE
  ), file.path(input_dir, "filter_value_lists.csv"), row.names = FALSE, na = "")

  result <- system2(
    file.path(R.home("bin"), "Rscript"),
    c(
      file.path(project_root, "scripts", "build_all_column_filter_bin_review.R"),
      input_dir, output_file
    ),
    stdout = TRUE, stderr = TRUE
  )
  testthat::expect_true(file.exists(output_file), info = paste(result, collapse = "\n"))
  testthat::expect_setequal(
    readxl::excel_sheets(output_file),
    c(
      "Summary", "All Review", "Column Review", "Bin Review", "Filter Review",
      "Columns Inventory", "Filters Inventory", "Filter Values"
    )
  )
  review <- readxl::read_excel(output_file, sheet = "All Review", skip = 3)
  testthat::expect_equal(review$rule_type, c("Column", "Bin", "Filter"))
  testthat::expect_equal(review$source_type, c("inline", "standalone_saved_object", "inline"))
  testthat::expect_equal(review$report_title[[1L]], "Report A")
  testthat::expect_equal(review$definition_name[[2L]], "Saved Bin")
})
