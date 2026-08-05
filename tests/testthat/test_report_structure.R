project_root <- normalizePath(file.path(testthat::test_path(), "..", ".."), mustWork = TRUE)
source(file.path(project_root, "R", "extract", "report_xml_helpers.R"))
source(file.path(project_root, "R", "extract", "extract_report_columns.R"))
source(file.path(project_root, "R", "extract", "extract_report_filters.R"))
source(file.path(project_root, "R", "export", "export_report_dependencies.R"))

testthat::test_that("report columns, filters, and dependencies include inline and saved definitions", {
  report_path <- "/shared/Test Report"
  saved_column_path <- "/shared/Test Saved Column"
  saved_filter_path <- "/shared/Test Saved Filter"
  report_xml <- paste0(
    "<saw:report xmlns:saw='com.siebel.analytics.web/report/v1.1' ",
    "xmlns:sawx='com.siebel.analytics.web/expression/v1.1' ",
    "xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>",
    "<saw:criteria subjectArea='&quot;Physical Items&quot;'>",
    "<saw:columns>",
    "<saw:column xsi:type='saw:regularColumn' columnID='inline-1'>",
    "<saw:columnFormula><sawx:expr xsi:type='sawx:sqlExpression'>",
    "&quot;Location&quot;.&quot;Library Code&quot;",
    "</sawx:expr></saw:columnFormula></saw:column>",
    "<saw:column xsi:type='saw:savedRegularColumnRef' columnID='saved-1' ",
    "path='", saved_column_path, "'/>",
    "</saw:columns>",
    "<saw:filter><sawx:expr xsi:type='sawx:logical' op='and'>",
    "<sawx:expr xsi:type='sawx:comparison' op='equal'>",
    "<sawx:expr xsi:type='sawx:sqlExpression'>",
    "&quot;Location&quot;.&quot;Library Code&quot;",
    "</sawx:expr><sawx:expr xsi:type='xsd:string'>MAIN</sawx:expr>",
    "</sawx:expr>",
    "<sawx:expr xsi:type='sawx:savedFilter' name='Test Saved Filter' path='",
    saved_filter_path, "'/>",
    "</sawx:expr></saw:filter>",
    "</saw:criteria></saw:report>"
  )
  catalog <- data.frame(
    catalog_index = 1:3,
    object_kind = c("report", "saved_column", "filter"),
    object_title = c("Test Report", "Test Saved Column", "Test Saved Filter"),
    original_path = c(report_path, saved_column_path, saved_filter_path),
    xml_text = c(report_xml, "<savedColumnObject/>", "<savedFilterObject/>"),
    stringsAsFactors = FALSE
  )
  input_path <- tempfile(fileext = ".rds")
  output_dir <- tempfile("report-structure-")
  dir.create(output_dir)
  on.exit(unlink(c(input_path, output_dir), recursive = TRUE), add = TRUE)
  saveRDS(catalog, input_path)

  columns <- extract_report_columns(
    input_path,
    file.path(output_dir, "report_saved_and_non_saved_columns.csv")
  )
  filters <- extract_report_filters(
    input_path,
    file.path(output_dir, "report_saved_and_non_saved_filters.csv")
  )
  dependencies <- export_report_dependencies(
    input_path,
    columns,
    filters,
    file.path(output_dir, "report_dependencies.csv")
  )

  testthat::expect_equal(nrow(columns), 2L)
  testthat::expect_setequal(columns$column_source, c("inline", "saved_reference"))
  testthat::expect_equal(columns$report_subject_area, rep("Physical Items", 2L))
  testthat::expect_equal(nrow(filters), 2L)
  testthat::expect_setequal(filters$filter_source, c("inline", "saved_reference"))
  testthat::expect_match(filters$filter_text[filters$filter_source == "inline"], "MAIN")
  testthat::expect_equal(nrow(dependencies), 2L)
  testthat::expect_true(all(dependencies$is_resolved))
  testthat::expect_setequal(
    dependencies$dependency_type,
    c("saved_column", "saved_filter")
  )
  testthat::expect_true(all(file.exists(file.path(output_dir, c(
    "report_saved_and_non_saved_columns.csv",
    "report_saved_and_non_saved_filters.csv"
  )))))
})
