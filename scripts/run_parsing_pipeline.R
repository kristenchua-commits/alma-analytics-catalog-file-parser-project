# Command-line entry point for the complete catalog parsing pipeline.
script_source_file <- tryCatch(
  normalizePath(sys.frame(1)$ofile, mustWork = TRUE),
  error = function(e) NA_character_
)
if (is.na(script_source_file)) {
  file_argument <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(file_argument) && file.exists(sub("^--file=", "", file_argument[1L]))) {
    script_source_file <- normalizePath(sub("^--file=", "", file_argument[1L]), mustWork = TRUE)
  } else {
    search_directory <- normalizePath(getwd(), mustWork = TRUE)
    repeat {
      candidate <- file.path(
        search_directory,
        "scripts",
        "run_parsing_pipeline.R"
      )
      if (file.exists(candidate)) {
        script_source_file <- normalizePath(candidate, mustWork = TRUE)
        break
      }
      parent_directory <- dirname(search_directory)
      if (identical(parent_directory, search_directory)) {
        stop("Could not find the project root. Open the .Rproj file or setwd() to the repository.")
      }
      search_directory <- parent_directory
    }
  }
}
script_project_root <- dirname(dirname(script_source_file))
source(file.path(script_project_root, "R", "run_parsing_pipeline.R"))

if (!interactive() && sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  if (!length(args)) {
    stop(paste(
      "Usage: Rscript scripts/run_parsing_pipeline.R",
      "path/to/file.catalog [output_dir]"
    ))
  }

  run_parsing_pipeline(
    catalog_path = args[1L],
    output_dir = if (length(args) > 1L) args[2L] else file.path(script_project_root, "output")
  )
}
