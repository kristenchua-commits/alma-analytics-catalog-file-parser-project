# Run all extraction, inspection, parsing, and export stages.
pipeline_source_file <- tryCatch(
  normalizePath(sys.frame(1)$ofile, mustWork = TRUE),
  error = function(e) NA_character_
)
if (is.na(pipeline_source_file)) {
  candidates <- c(
    file.path(getwd(), "R", "run_pipeline.R"),
    file.path(getwd(), "run_pipeline.R")
  )
  candidates <- candidates[file.exists(candidates)]
  if (!length(candidates)) stop("Could not determine the project root for run_pipeline.R.")
  pipeline_source_file <- normalizePath(candidates[1L], mustWork = TRUE)
}
pipeline_project_root <- dirname(dirname(pipeline_source_file))

pipeline_files <- c(
  "R/io/read_catalog_file.R",
  "R/io/read_catalog_metadata.R",
  "R/extract/extract_catalog.R",
  "R/inspect/inspect_catalog_metadata.R",
  "R/extract/extract_xml_tag_inventory.R",
  "R/extract/extract_saved_columns.R",
  "R/extract/extract_filter_objects.R",
  "R/export/export_filter_criteria.R",
  "R/export/export_filter_review.R",
  "R/export/export_saved_column_review.R"
)
for (pipeline_file in pipeline_files) {
  source(file.path(pipeline_project_root, pipeline_file))
}

run_pipeline <- function(
    catalog_path,
    output_dir = file.path(pipeline_project_root, "output")) {
  if (!file.exists(catalog_path)) {
    project_catalog_path <- file.path(pipeline_project_root, catalog_path)
    if (file.exists(project_catalog_path)) catalog_path <- project_catalog_path
  }
  if (!file.exists(catalog_path)) stop("Catalog file not found: ", catalog_path)
  if (!grepl("^(/|[A-Za-z]:[/\\\\])", output_dir)) {
    output_dir <- file.path(pipeline_project_root, output_dir)
  }
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  extract_catalog(catalog_path, output_dir, progress = interactive())
  catalog_rds <- file.path(output_dir, "catalog_extract.rds")
  inspect_catalog_metadata(
    catalog_rds,
    file.path(output_dir, "catalog_metadata_inventory.csv")
  )
  extract_xml_tag_inventory(
    catalog_rds,
    file.path(output_dir, "xml_tag_inventory.csv")
  )

  catalog <- readRDS(catalog_rds)
  if (any(catalog$object_kind == "saved_column")) {
    extract_saved_columns(catalog_rds, file.path(output_dir, "saved_columns.csv"))
    export_saved_column_review(
      catalog_rds,
      file.path(output_dir, "saved_column_review.xlsx"),
      file.path(output_dir, "saved_column_review.csv")
    )
  } else {
    message("No saved-column objects; skipping saved-column outputs")
  }

  if (any(catalog$object_kind == "filter")) {
    filter_rds <- file.path(output_dir, "filter_objects.rds")
    extract_filter_objects(
      catalog_rds,
      filter_rds,
      file.path(output_dir, "filter_objects_summary.csv")
    )
    export_filter_criteria(filter_rds, file.path(output_dir, "filter_criteria.csv"))
    export_filter_review(
      filter_rds,
      file.path(output_dir, "filter_review.xlsx"),
      file.path(output_dir, "filter_review.csv"),
      file.path(output_dir, "filter_review_value_lists.csv")
    )
  } else {
    message("No filter objects; skipping filter outputs")
  }

  message("Pipeline complete: ", normalizePath(output_dir))
  invisible(catalog)
}
