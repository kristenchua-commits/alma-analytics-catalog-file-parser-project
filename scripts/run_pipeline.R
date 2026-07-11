# Run all extraction, inspection, parsing, and export stages.
source("scripts/extract_scripts/extract_XMLFileList.R")
source("scripts/inventory/inspect_catalog_metadata.R")
source("scripts/extract_scripts/extract_XMLTagInventory.r")
source("scripts/extract_scripts/extract_SavedColumn.R")
source("scripts/extract_scripts/extract_XML_to_FilterObject.r")
source("scripts/extract_scripts/export_filter_criteria.R")
source("scripts/extract_scripts/export_filter_review.R")

run_pipeline <- function(catalog_path, output_dir = "output") {
  if (!file.exists(catalog_path)) stop("Catalog file not found: ", catalog_path)
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
  } else {
    message("No saved-column objects; skipping saved_columns.csv")
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

if (!interactive() && sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  if (!length(args)) stop("Usage: Rscript scripts/run_pipeline.R path/to/file.catalog [output_dir]")
  run_pipeline(args[1L], if (length(args) > 1L) args[2L] else "output")
}
