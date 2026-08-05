if (requireNamespace("testthat", quietly = TRUE)) {
  testthat::test_dir("tests/testthat", reporter = "summary")
} else {
  message("Package 'testthat' is unavailable; running the fixture assertions with base R.")
  source("R/run_parsing_pipeline.R")

  fixture <- "data/filters_annual_statistics_2025_26.catalog"
  output_dir <- tempfile("filter-pipeline-output-")
  dir.create(output_dir)
  on.exit(unlink(output_dir, recursive = TRUE), add = TRUE)

  catalog <- run_parsing_pipeline(fixture, output_dir)
  expected_files <- c(
    "catalog_extract.rds",
    "catalog_extract_summary.csv",
    "catalog_metadata_inventory.csv",
    "xml_tag_inventory.csv",
    "filter_objects.rds",
    "filter_objects_summary.csv",
    "filter_criteria.csv",
    "filter_review.xlsx",
    "filter_review.csv",
    "filter_review_value_lists.csv"
  )

  stopifnot(
    nrow(catalog) > 0L,
    any(catalog$object_kind == "filter"),
    all(file.exists(file.path(output_dir, expected_files))),
    !file.exists(file.path(output_dir, "saved_columns.csv")),
    !file.exists(file.path(output_dir, "saved_column_review.xlsx")),
    !file.exists(file.path(output_dir, "report_saved_and_non_saved_columns.csv")),
    !file.exists(file.path(output_dir, "report_saved_and_non_saved_filters.csv")),
    !file.exists(file.path(output_dir, "report_dependencies.csv"))
  )

  report_path <- "/shared/Test Report"
  saved_column_path <- "/shared/Test Saved Column"
  saved_filter_path <- "/shared/Test Saved Filter"
  report_xml <- paste0(
    "<saw:report xmlns:saw='com.siebel.analytics.web/report/v1.1' ",
    "xmlns:sawx='com.siebel.analytics.web/expression/v1.1' ",
    "xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>",
    "<saw:criteria subjectArea='&quot;Physical Items&quot;'><saw:columns>",
    "<saw:column xsi:type='saw:regularColumn' columnID='inline-1'>",
    "<saw:columnFormula><sawx:expr xsi:type='sawx:sqlExpression'>",
    "&quot;Location&quot;.&quot;Library Code&quot;</sawx:expr></saw:columnFormula>",
    "</saw:column><saw:column xsi:type='saw:savedRegularColumnRef' ",
    "columnID='saved-1' path='", saved_column_path, "'/></saw:columns>",
    "<saw:filter><sawx:expr xsi:type='sawx:logical' op='and'>",
    "<sawx:expr xsi:type='sawx:comparison' op='equal'>",
    "<sawx:expr xsi:type='sawx:sqlExpression'>",
    "&quot;Location&quot;.&quot;Library Code&quot;</sawx:expr>",
    "<sawx:expr xsi:type='xsd:string'>MAIN</sawx:expr></sawx:expr>",
    "<sawx:expr xsi:type='sawx:savedFilter' name='Test Saved Filter' path='",
    saved_filter_path, "'/></sawx:expr></saw:filter></saw:criteria></saw:report>"
  )
  report_catalog <- data.frame(
    catalog_index = 1:3,
    object_kind = c("report", "saved_column", "filter"),
    object_title = c("Test Report", "Test Saved Column", "Test Saved Filter"),
    original_path = c(report_path, saved_column_path, saved_filter_path),
    xml_text = c(report_xml, "<savedColumnObject/>", "<savedFilterObject/>"),
    stringsAsFactors = FALSE
  )
  report_input <- tempfile(fileext = ".rds")
  report_output <- tempfile("report-structure-")
  dir.create(report_output)
  saveRDS(report_catalog, report_input)
  report_columns <- extract_report_columns(
    report_input,
    file.path(report_output, "report_saved_and_non_saved_columns.csv")
  )
  report_filters <- extract_report_filters(
    report_input,
    file.path(report_output, "report_saved_and_non_saved_filters.csv")
  )
  report_dependencies <- export_report_dependencies(
    report_input,
    report_columns,
    report_filters,
    file.path(report_output, "report_dependencies.csv")
  )
  stopifnot(
    nrow(report_columns) == 2L,
    nrow(report_filters) == 2L,
    nrow(report_dependencies) == 2L,
    all(report_dependencies$is_resolved)
  )

  equality_condition <- xml2::read_xml(paste0(
    "<condition xmlns:sawx='com.siebel.analytics.web/expression/v1.1' ",
    "xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>",
    "<sawx:expr xsi:type='sawx:comparison' op='equal'>",
    "<sawx:expr xsi:type='sawx:sqlExpression'>&quot;Borrower Details&quot;.&quot;User Group&quot;</sawx:expr>",
    "<sawx:expr xsi:type='xsd:string'>UCM Faculty</sawx:expr>",
    "</sawx:expr></condition>"
  ))
  list_condition <- xml2::read_xml(paste0(
    "<condition xmlns:sawx='com.siebel.analytics.web/expression/v1.1' ",
    "xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>",
    "<sawx:expr xsi:type='sawx:list' op='in'>",
    "<sawx:expr xsi:type='sawx:sqlExpression'>field_name</sawx:expr>",
    "<sawx:expr xsi:type='xsd:string'>Library Staff</sawx:expr>",
    "<sawx:expr xsi:type='xsd:string'>UCM Staff</sawx:expr>",
    "</sawx:expr></condition>"
  ))
  stopifnot(
    identical(
      format_saved_column_condition(equality_condition),
      "\"Borrower Details\".\"User Group\" = 'UCM Faculty'"
    ),
    identical(
      format_saved_column_condition(list_condition),
      "field_name IN ('Library Staff', 'UCM Staff')"
    )
  )
  message("Base-R fixture assertions passed.")
}
