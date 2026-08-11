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
    "catalog_extract.rds", "catalog_extract_summary.csv",
    "catalog_metadata_inventory.csv", "xml_tag_inventory.csv",
    "filters.csv", "filter_rules.csv", "filter_value_lists.csv"
  )
  stopifnot(
    nrow(catalog) > 0L,
    any(catalog$object_kind == "filter"),
    all(file.exists(file.path(output_dir, expected_files))),
    !file.exists(file.path(output_dir, "columns.csv")),
    !file.exists(file.path(output_dir, "column_rules.csv")),
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
    "columnID='saved-1' path='", saved_column_path, "'>",
    "<saw:columnHeading><saw:caption><saw:text>Short report heading</saw:text>",
    "</saw:caption></saw:columnHeading></saw:column></saw:columns>",
    "<saw:filter><sawx:expr xsi:type='sawx:logical' op='and'>",
    "<sawx:expr xsi:type='sawx:comparison' op='equal'>",
    "<sawx:expr xsi:type='sawx:sqlExpression'>",
    "&quot;Location&quot;.&quot;Library Code&quot;</sawx:expr>",
    "<sawx:expr xsi:type='xsd:string'>MAIN</sawx:expr></sawx:expr>",
    "<sawx:expr xsi:type='sawx:savedFilter' name='Test Saved Filter' path='",
    saved_filter_path, "'/></sawx:expr></saw:filter></saw:criteria></saw:report>"
  )
  saved_column_xml <- paste0(
    "<savedColumnObject xmlns:saw='com.siebel.analytics.web/report/v1.1' ",
    "xmlns:sawx='com.siebel.analytics.web/expression/v1.1' ",
    "xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>",
    "<saw:column><saw:columnFormula><sawx:expr xsi:type='sawx:sqlExpression'>",
    "saved_column_formula</sawx:expr></saw:columnFormula>",
    "<saw:columnHeading><saw:caption><saw:text>Canonical saved heading</saw:text>",
    "</saw:caption></saw:columnHeading></saw:column></savedColumnObject>"
  )
  saved_filter_xml <- paste0(
    "<savedFilterObject xmlns:saw='com.siebel.analytics.web/report/v1.1' ",
    "xmlns:sawx='com.siebel.analytics.web/expression/v1.1' ",
    "xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>",
    "<saw:filter><sawx:expr xsi:type='sawx:comparison' op='equal'>",
    "<sawx:expr xsi:type='sawx:sqlExpression'>saved_field</sawx:expr>",
    "<sawx:expr xsi:type='xsd:string'>saved_value</sawx:expr>",
    "</sawx:expr></saw:filter></savedFilterObject>"
  )
  report_catalog <- data.frame(
    catalog_index = 1:3,
    object_kind = c("report", "saved_column", "filter"),
    object_title = c("Test Report", "Test Saved Column", "Test Saved Filter"),
    subject_area = rep("Physical Items", 3L),
    original_path = c(report_path, saved_column_path, saved_filter_path),
    xml_text = c(report_xml, saved_column_xml, saved_filter_xml),
    stringsAsFactors = FALSE
  )
  report_input <- tempfile(fileext = ".rds")
  report_output <- tempfile("general-structure-")
  dir.create(report_output)
  saveRDS(report_catalog, report_input)
  columns <- extract_columns(
    report_input, file.path(report_output, "columns.csv"),
    file.path(report_output, "column_rules.csv")
  )
  filters <- extract_filters(
    report_input, file.path(report_output, "filters.csv"),
    file.path(report_output, "filter_rules.csv"),
    file.path(report_output, "filter_value_lists.csv")
  )
  dependencies <- export_report_dependencies(
    report_input, columns$columns, filters$filters,
    file.path(report_output, "report_dependencies.csv")
  )
  stopifnot(
    nrow(columns$columns) == 3L,
    all(c("inline", "saved_reference", "standalone_saved_object") %in%
      columns$columns$column_source),
    columns$columns$column_name[columns$columns$column_source == "saved_reference"] ==
      "Canonical saved heading",
    nrow(filters$filters) == 3L,
    all(c("inline", "saved_reference", "standalone_saved_object") %in%
      filters$filters$filter_source),
    nrow(dependencies) == 2L,
    all(dependencies$is_resolved)
  )

  equality_condition <- xml2::read_xml(paste0(
    "<condition xmlns:sawx='com.siebel.analytics.web/expression/v1.1' ",
    "xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>",
    "<sawx:expr xsi:type='sawx:comparison' op='equal'>",
    "<sawx:expr xsi:type='sawx:sqlExpression'>field_name</sawx:expr>",
    "<sawx:expr xsi:type='xsd:string'>value</sawx:expr>",
    "</sawx:expr></condition>"
  ))
  stopifnot(identical(format_column_condition(equality_condition), "field_name = 'value'"))
  message("Base-R fixture assertions passed.")
}
