# Stage 1: extract XML objects and aligned object metadata from a .catalog file.
source("scripts/script_helper_functions/read_catalog_file.R")
source("scripts/script_helper_functions/read_catalog_metadata.R")

resolve_catalog_path <- function(args = commandArgs(trailingOnly = TRUE)) {
  if (length(args) && nzchar(args[1L])) return(args[1L])

  files <- list.files("data", pattern = "\\.catalog$", full.names = TRUE,
                      recursive = TRUE, ignore.case = TRUE)
  if (!length(files)) stop("No .catalog files found under data/.")
  if (!interactive()) {
    if (length(files) > 1L) {
      stop("Multiple catalog files found. Pass one path as the first argument.")
    }
    return(files[1L])
  }

  selection <- utils::menu(basename(files), title = "Choose a .catalog file")
  if (selection < 1L) stop("No catalog file selected.")
  files[selection]
}

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

if (sys.nframe() == 0L) {
  extract_catalog(resolve_catalog_path())
}
