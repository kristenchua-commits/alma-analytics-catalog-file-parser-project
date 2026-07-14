if (requireNamespace("testthat", quietly = TRUE)) {
  testthat::test_dir("tests/testthat", reporter = "summary")
} else {
  message("Package 'testthat' is unavailable; running the fixture assertions with base R.")
  source("R/run_pipeline.R")

  fixture <- "data/filters_annual_statistics_2025_26.catalog"
  output_dir <- tempfile("filter-pipeline-output-")
  dir.create(output_dir)
  on.exit(unlink(output_dir, recursive = TRUE), add = TRUE)

  catalog <- run_pipeline(fixture, output_dir)
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
    !file.exists(file.path(output_dir, "saved_column_review.xlsx"))
  )
  message("Base-R fixture assertions passed.")
}
