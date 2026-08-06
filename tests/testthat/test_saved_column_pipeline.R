project_root <- normalizePath(file.path(testthat::test_path(), "..", ".."), mustWork = TRUE)
source(file.path(project_root, "R", "extract", "extract_saved_column_objects.R"))
source(file.path(project_root, "R", "extract", "extract_saved_columns.R"))
source(file.path(project_root, "R", "export", "export_saved_column_review.R"))

testthat::test_that("saved-column outputs consume the saved-column object intermediate", {
  saved_xml <- paste0(
    "<savedColumnObject xmlns:saw='com.siebel.analytics.web/report/v1.1' ",
    "xmlns:sawx='com.siebel.analytics.web/expression/v1.1' ",
    "xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>",
    "<saw:column><saw:columnFormula><sawx:expr xsi:type='sawx:sqlExpression'>",
    "&quot;Location&quot;.&quot;Library Code&quot;",
    "</sawx:expr></saw:columnFormula><saw:columnHeading><saw:caption>",
    "<saw:text>Library Code</saw:text></saw:caption></saw:columnHeading>",
    "</saw:column></savedColumnObject>"
  )
  catalog <- data.frame(
    catalog_index = 1:2,
    object_kind = c("saved_column", "report"),
    object_title = c("Test Saved Column", "Unrelated Report"),
    subject_area = c("Physical Items", "Physical Items"),
    original_path = c("/shared/Test Saved Column", "/shared/Unrelated Report"),
    xml_text = c(saved_xml, "<report/>"),
    stringsAsFactors = FALSE
  )
  output_dir <- tempfile("saved-column-pipeline-")
  dir.create(output_dir)
  on.exit(unlink(output_dir, recursive = TRUE), add = TRUE)
  catalog_path <- file.path(output_dir, "catalog_extract.rds")
  objects_path <- file.path(output_dir, "saved_column_objects.rds")
  summary_path <- file.path(output_dir, "saved_column_objects_summary.csv")
  columns_path <- file.path(output_dir, "saved_columns.csv")
  review_xlsx <- file.path(output_dir, "saved_column_review.xlsx")
  review_csv <- file.path(output_dir, "saved_column_review.csv")
  saveRDS(catalog, catalog_path)

  objects <- extract_saved_column_objects(catalog_path, objects_path, summary_path)
  columns <- extract_saved_columns(objects_path, columns_path)
  review <- export_saved_column_review(objects_path, review_xlsx, review_csv)

  testthat::expect_equal(nrow(objects), 1L)
  testthat::expect_equal(objects$object_kind, "saved_column")
  testthat::expect_true("xml_text" %in% names(readRDS(objects_path)))
  testthat::expect_false("xml_text" %in% names(read.csv(summary_path)))
  testthat::expect_equal(columns$saved_column_object_index, 1L)
  testthat::expect_equal(nrow(review$review), 1L)
  testthat::expect_true(all(file.exists(c(
    objects_path, summary_path, columns_path, review_xlsx, review_csv
  ))))
})
