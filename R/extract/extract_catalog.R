# Stage 1: extract XML objects and aligned object metadata from a .catalog file.
extract_catalog <- function(catalog_path, output_dir = "output", progress = interactive()) {
  catalog <- read_catalog_file(
    catalog_path,
    keep_xml = FALSE,
    keep_strings = TRUE,
    progress = progress
  )
  metadata <- read_catalog_metadata(catalog)

  catalog_extract <- metadata
  catalog_extract$xml_text <- catalog$xml_text

  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  rds_path <- file.path(output_dir, "catalog_extract.rds")
  summary_path <- file.path(output_dir, "catalog_extract_summary.csv")

  saveRDS(catalog_extract, rds_path)
  write.csv(metadata, summary_path, row.names = FALSE, na = "")

  message("Wrote ", rds_path, " (", nrow(catalog_extract), " objects)")
  message("Wrote ", summary_path)
  invisible(catalog_extract)
}
