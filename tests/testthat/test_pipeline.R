project_root <- normalizePath(file.path(testthat::test_path(), "..", ".."), mustWork = TRUE)
original_working_directory <- setwd(project_root)
source("R/run_parsing_pipeline.R")
setwd(original_working_directory)

testthat::test_that("the filter fixture runs through the complete filter branch", {
  fixture <- file.path(
    project_root,
    "data", "filters_annual_statistics_2025_26.catalog"
  )
  output_dir <- tempfile("filter-pipeline-output-")
  dir.create(output_dir)
  on.exit(unlink(output_dir, recursive = TRUE), add = TRUE)

  original_working_directory <- setwd(project_root)
  on.exit(setwd(original_working_directory), add = TRUE)
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

  testthat::expect_true(nrow(catalog) > 0L)
  testthat::expect_true(any(catalog$object_kind == "filter"))
  testthat::expect_true(all(file.exists(file.path(output_dir, expected_files))))
  testthat::expect_false(file.exists(file.path(output_dir, "saved_columns.csv")))
  testthat::expect_false(file.exists(file.path(output_dir, "saved_column_review.xlsx")))
  testthat::expect_false(file.exists(file.path(output_dir, "report_columns.csv")))
  testthat::expect_false(file.exists(file.path(output_dir, "report_filters.csv")))
  testthat::expect_false(file.exists(file.path(output_dir, "report_dependencies.csv")))
})
