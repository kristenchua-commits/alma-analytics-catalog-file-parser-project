# Interactive wrapper for selecting a .catalog file and running the pipeline.
source("scripts/run_pipeline.R")

find_catalog_files <- function(
    search_directories = c("data", path.expand("~/Downloads"))) {
  existing_directories <- search_directories[dir.exists(search_directories)]
  if (!length(existing_directories)) return(character(0))

  files <- unlist(lapply(existing_directories, function(directory) {
    list.files(
      directory,
      pattern = "\\.catalog$",
      full.names = TRUE,
      recursive = TRUE,
      ignore.case = TRUE
    )
  }), use.names = FALSE)

  unique(normalizePath(files, mustWork = TRUE))
}

catalog_file_label <- function(path) {
  repository_root <- normalizePath(getwd(), mustWork = TRUE)
  downloads_root <- normalizePath(path.expand("~/Downloads"), mustWork = FALSE)
  normalized_path <- normalizePath(path, mustWork = TRUE)

  if (startsWith(normalized_path, paste0(repository_root, "/"))) {
    relative_path <- substring(normalized_path, nchar(repository_root) + 2L)
    return(paste0("[Repository] ", relative_path))
  }
  if (startsWith(normalized_path, paste0(downloads_root, "/"))) {
    relative_path <- substring(normalized_path, nchar(downloads_root) + 2L)
    return(paste0("[Downloads] ", relative_path))
  }

  paste0("[Other] ", normalized_path)
}

choose_catalog_file <- function() {
  catalog_files <- find_catalog_files()
  browse_label <- "Browse for a .catalog file elsewhere..."

  if (interactive() && length(catalog_files)) {
    labels <- vapply(catalog_files, catalog_file_label, character(1))
    selection <- utils::menu(
      c(labels, browse_label),
      title = "Choose an Alma Analytics .catalog file"
    )

    if (selection == 0L) stop("No catalog file selected.")
    if (selection <= length(catalog_files)) return(catalog_files[selection])
  }

  if (!interactive()) {
    stop("Interactive file selection requires RStudio or an interactive R session.")
  }

  selected_file <- file.choose()
  if (!grepl("\\.catalog$", selected_file, ignore.case = TRUE)) {
    stop("The selected file is not a .catalog file: ", selected_file)
  }
  normalizePath(selected_file, mustWork = TRUE)
}

run_catalog_pipeline <- function(catalog_path = NULL, output_dir = "output") {
  if (is.null(catalog_path)) catalog_path <- choose_catalog_file()

  if (!file.exists(catalog_path)) {
    stop("Catalog file not found: ", catalog_path)
  }
  if (!grepl("\\.catalog$", catalog_path, ignore.case = TRUE)) {
    stop("Input must be a .catalog file: ", catalog_path)
  }

  catalog_path <- normalizePath(catalog_path, mustWork = TRUE)
  message("Selected catalog: ", catalog_path)
  run_pipeline(catalog_path = catalog_path, output_dir = output_dir)
}

# In RStudio, run:
# source("choose_catalog_file_and_run_pipeline.R")
# catalog <- run_catalog_pipeline()

if (!interactive() && sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  if (!length(args)) {
    stop(paste(
      "Usage: Rscript choose_catalog_file_and_run_pipeline.R",
      "path/to/file.catalog [output_dir]"
    ))
  }
  run_catalog_pipeline(
    catalog_path = args[1L],
    output_dir = if (length(args) > 1L) args[2L] else "output"
  )
}
