# Command-line entry point for the complete catalog parsing pipeline.
source("R/run_pipeline.R")

if (!interactive() && sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  if (!length(args)) {
    stop("Usage: Rscript scripts/run_pipeline.R path/to/file.catalog [output_dir]")
  }

  run_pipeline(
    catalog_path = args[1L],
    output_dir = if (length(args) > 1L) args[2L] else "output"
  )
}
