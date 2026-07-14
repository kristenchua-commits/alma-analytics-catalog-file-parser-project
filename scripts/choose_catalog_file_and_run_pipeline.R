# Interactive entry point for selecting a catalog and running the pipeline.
source("R/choose_catalog_file.R")

if (!interactive() && sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  if (!length(args)) {
    stop(paste(
      "Usage: Rscript scripts/choose_catalog_file_and_run_pipeline.R",
      "path/to/file.catalog [output_dir]"
    ))
  }
  run_catalog_pipeline(
    catalog_path = args[1L],
    output_dir = if (length(args) > 1L) args[2L] else "output"
  )
}
