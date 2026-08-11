project_root <- normalizePath(file.path(testthat::test_path(), "..", ".."), mustWork = TRUE)
source(file.path(project_root, "R", "extract", "report_xml_helpers.R"))
source(file.path(project_root, "R", "extract", "extract_columns.R"))
source(file.path(project_root, "R", "extract", "extract_filters.R"))

testthat::test_that("general branches retain unreferenced standalone objects", {
  saved_column_xml <- paste0(
    "<savedColumnObject xmlns:saw='com.siebel.analytics.web/report/v1.1' ",
    "xmlns:sawx='com.siebel.analytics.web/expression/v1.1' ",
    "xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>",
    "<saw:column><saw:columnFormula><sawx:expr xsi:type='sawx:sqlExpression'>",
    "&quot;Location&quot;.&quot;Library Code&quot;",
    "</sawx:expr></saw:columnFormula><saw:columnHeading><saw:caption>",
    "<saw:text>Library Code</saw:text></saw:caption></saw:columnHeading>",
    "</saw:column></savedColumnObject>"
  )
  saved_filter_xml <- paste0(
    "<savedFilterObject xmlns:saw='com.siebel.analytics.web/report/v1.1' ",
    "xmlns:sawx='com.siebel.analytics.web/expression/v1.1' ",
    "xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>",
    "<saw:filter><sawx:expr xsi:type='sawx:comparison' op='equal'>",
    "<sawx:expr xsi:type='sawx:sqlExpression'>",
    "&quot;Location&quot;.&quot;Library Code&quot;</sawx:expr>",
    "<sawx:expr xsi:type='xsd:string'>MAIN</sawx:expr>",
    "</sawx:expr></saw:filter></savedFilterObject>"
  )
  catalog <- data.frame(
    catalog_index = 1:2,
    object_kind = c("saved_column", "filter"),
    object_title = c("Unused Saved Column", "Unused Saved Filter"),
    subject_area = c("Physical Items", "Physical Items"),
    original_path = c("/shared/Unused Saved Column", "/shared/Unused Saved Filter"),
    xml_text = c(saved_column_xml, saved_filter_xml),
    stringsAsFactors = FALSE
  )
  output_dir <- tempfile("general-branches-")
  dir.create(output_dir)
  on.exit(unlink(output_dir, recursive = TRUE), add = TRUE)
  input_path <- file.path(output_dir, "catalog_extract.rds")
  saveRDS(catalog, input_path)

  columns <- extract_columns(
    input_path, file.path(output_dir, "columns.csv"),
    file.path(output_dir, "column_rules.csv")
  )
  filters <- extract_filters(
    input_path, file.path(output_dir, "filters.csv"),
    file.path(output_dir, "filter_rules.csv"),
    file.path(output_dir, "filter_value_lists.csv")
  )

  testthat::expect_equal(nrow(columns$columns), 1L)
  testthat::expect_equal(columns$columns$column_source, "standalone_saved_object")
  testthat::expect_false(columns$columns$is_referenced)
  testthat::expect_equal(nrow(columns$rules), 1L)
  testthat::expect_equal(nrow(filters$filters), 1L)
  testthat::expect_equal(filters$filters$filter_source, "standalone_saved_object")
  testthat::expect_false(filters$filters$is_referenced)
  testthat::expect_equal(nrow(filters$rules), 1L)
  testthat::expect_match(filters$rules$criterion_text, "MAIN")
})
