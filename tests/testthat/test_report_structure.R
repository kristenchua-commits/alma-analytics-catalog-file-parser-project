project_root <- normalizePath(file.path(testthat::test_path(), "..", ".."), mustWork = TRUE)
source(file.path(project_root, "R", "extract", "report_xml_helpers.R"))
source(file.path(project_root, "R", "extract", "extract_columns.R"))
source(file.path(project_root, "R", "extract", "extract_filters.R"))
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
    "path='", saved_column_path, "'><saw:columnHeading><saw:caption>",
    "<saw:text>Short report heading</saw:text></saw:caption></saw:columnHeading>",
    "</saw:column>",
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
    subject_area = rep("Physical Items", 3L),
    original_path = c(report_path, saved_column_path, saved_filter_path),
    xml_text = c(
      report_xml,
      paste0(
        "<savedColumnObject xmlns:saw='com.siebel.analytics.web/report/v1.1'>",
        "<saw:column><saw:columnHeading><saw:caption>",
        "<saw:text>Canonical saved heading</saw:text>",
        "</saw:caption></saw:columnHeading></saw:column></savedColumnObject>"
      ),
      paste0(
        "<savedFilterObject xmlns:saw='com.siebel.analytics.web/report/v1.1' ",
        "xmlns:sawx='com.siebel.analytics.web/expression/v1.1' ",
        "xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>",
        "<saw:filter><sawx:expr xsi:type='sawx:comparison' op='equal'>",
        "<sawx:expr xsi:type='sawx:sqlExpression'>saved_field</sawx:expr>",
        "<sawx:expr xsi:type='xsd:string'>saved_value</sawx:expr>",
        "</sawx:expr></saw:filter></savedFilterObject>"
      )
    ),
    stringsAsFactors = FALSE
  )
  input_path <- tempfile(fileext = ".rds")
  output_dir <- tempfile("report-structure-")
  dir.create(output_dir)
  on.exit(unlink(c(input_path, output_dir), recursive = TRUE), add = TRUE)
  saveRDS(catalog, input_path)

  columns_result <- extract_columns(
    input_path,
    file.path(output_dir, "columns.csv"),
    file.path(output_dir, "column_rules.csv")
  )
  filters_result <- extract_filters(
    input_path,
    file.path(output_dir, "filters.csv"),
    file.path(output_dir, "filter_rules.csv"),
    file.path(output_dir, "filter_value_lists.csv")
  )
  dependencies <- export_report_dependencies(
    input_path,
    columns_result$columns,
    filters_result$filters,
    file.path(output_dir, "report_dependencies.csv")
  )

  columns <- columns_result$columns
  filters <- filters_result$filters
  testthat::expect_equal(nrow(columns), 3L)
  testthat::expect_setequal(
    columns$column_source,
    c("inline", "saved_reference", "standalone_saved_object")
  )
  testthat::expect_equal(
    columns$report_subject_area[columns$record_scope == "report_column"],
    rep("Physical Items", 2L)
  )
  saved_row <- columns[columns$column_source == "saved_reference", ]
  testthat::expect_equal(saved_row$column_name, "Canonical saved heading")
  testthat::expect_equal(saved_row$report_display_name, "Short report heading")
  testthat::expect_equal(saved_row$definition_name, "Test Saved Column")
  standalone_column <- columns[columns$record_scope == "standalone_saved_object", ]
  testthat::expect_true(standalone_column$is_referenced)
  testthat::expect_equal(nrow(filters), 3L)
  testthat::expect_setequal(
    filters$filter_source,
    c("inline", "saved_reference", "standalone_saved_object")
  )
  testthat::expect_match(filters$filter_text[filters$filter_source == "inline"], "MAIN")
  testthat::expect_equal(nrow(dependencies), 2L)
  testthat::expect_true(all(dependencies$is_resolved))
  testthat::expect_setequal(
    dependencies$dependency_type,
    c("saved_column", "saved_filter")
  )
  testthat::expect_true(all(file.exists(file.path(output_dir, c(
    "columns.csv", "column_rules.csv", "filters.csv", "filter_rules.csv",
    "filter_value_lists.csv"
  )))))
})
