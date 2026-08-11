# Run all extraction, inspection, parsing, and export stages.
pipeline_source_file <- tryCatch(
  normalizePath(sys.frame(1)$ofile, mustWork = TRUE),
  error = function(e) NA_character_
)
if (is.na(pipeline_source_file)) {
  candidates <- c(
    file.path(getwd(), "R", "run_parsing_pipeline.R"),
    file.path(getwd(), "run_parsing_pipeline.R")
  )
  candidates <- candidates[file.exists(candidates)]
  if (!length(candidates)) {
    stop("Could not determine the project root for run_parsing_pipeline.R.")
  }
  pipeline_source_file <- normalizePath(candidates[1L], mustWork = TRUE)
}
pipeline_project_root <- dirname(dirname(pipeline_source_file))

pipeline_files <- c(
  "R/io/read_catalog_file.R",
  "R/io/read_catalog_metadata.R",
  "R/extract/extract_catalog.R",
  "R/inspect/inspect_catalog_metadata.R",
  "R/extract/extract_xml_tag_inventory.R",
  "R/extract/report_xml_helpers.R",
  "R/extract/extract_columns.R",
  "R/extract/extract_filters.R",
  "R/export/export_report_dependencies.R"
)
for (pipeline_file in pipeline_files) {
  source(file.path(pipeline_project_root, pipeline_file))
}

run_parsing_pipeline <- function(
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
  column_results <- NULL
  if (any(catalog$object_kind %in% c("report", "saved_column"))) {
    column_results <- extract_columns(
      catalog_rds,
      file.path(output_dir, "columns.csv"),
      file.path(output_dir, "column_rules.csv")
    )
  } else {
    message("No report or saved-column objects; skipping column outputs")
  }

  filter_results <- NULL
  if (any(catalog$object_kind %in% c("report", "filter"))) {
    filter_results <- extract_filters(
      catalog_rds,
      file.path(output_dir, "filters.csv"),
      file.path(output_dir, "filter_rules.csv"),
      file.path(output_dir, "filter_value_lists.csv")
    )
  } else {
    message("No report or saved-filter objects; skipping filter outputs")
  }

  if (any(catalog$object_kind == "report") &&
      !is.null(column_results) && !is.null(filter_results)) {
    export_report_dependencies(
      catalog_rds,
      column_results$columns,
      filter_results$filters,
      file.path(output_dir, "report_dependencies.csv")
    )
  } else {
    message("No report dependency output was required")
  }

  message("Pipeline complete: ", normalizePath(output_dir))
  invisible(catalog)
}
