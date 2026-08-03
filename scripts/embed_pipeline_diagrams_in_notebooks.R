# Embed pipeline PNGs as notebook attachments so private GitHub repositories
# can render them without unauthenticated raw-image requests.

if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("The jsonlite package is required to embed notebook images.")
}

embed_png_attachment <- function(
    notebook_path,
    image_path,
    heading,
    attachment_name,
    alt_text) {
  lines <- readLines(notebook_path, warn = FALSE)
  heading_pattern <- paste0('"## ', heading, '\\n"')
  heading_line <- grep(heading_pattern, lines, fixed = TRUE)
  if (length(heading_line) != 1L) {
    stop("Could not uniquely locate notebook heading: ", heading)
  }

  cell_starts <- which(trimws(lines[seq_len(heading_line)]) == "{")
  cell_start <- max(cell_starts)
  later_lines <- seq.int(heading_line, length(lines))
  cell_end_candidates <- later_lines[trimws(lines[later_lines]) %in% c("},", "}")]
  if (!length(cell_end_candidates)) stop("Could not locate the notebook cell boundary.")
  cell_end <- cell_end_candidates[[1L]]

  cell_lines <- seq.int(cell_start, cell_end)
  metadata_line <- cell_lines[grepl('"metadata":', lines[cell_lines], fixed = TRUE)]
  image_line <- cell_lines[
    grepl("![", lines[cell_lines], fixed = TRUE) &
      grepl(attachment_name, lines[cell_lines], fixed = TRUE)
  ]
  if (length(metadata_line) != 1L || length(image_line) != 1L) {
    stop("Could not uniquely locate the target cell metadata and image reference.")
  }

  image_size <- file.info(image_path)$size
  image_raw <- readBin(image_path, what = "raw", n = image_size)
  encoded_image <- gsub(
    "[[:space:]]",
    "",
    jsonlite::base64_enc(image_raw)
  )
  attachment_lines <- c(
    '   "attachments": {',
    paste0('    "', attachment_name, '": {'),
    paste0('     "image/png": "', encoded_image, '"'),
    "    }",
    "   },"
  )

  lines[image_line] <- paste0(
    '    "![', alt_text, '](attachment:', attachment_name, ')\\n",'
  )
  attachment_key <- cell_lines[grepl(
    paste0('"', attachment_name, '": {'),
    lines[cell_lines],
    fixed = TRUE
  )]
  if (length(attachment_key) == 1L) {
    lines[attachment_key + 1L] <- attachment_lines[[3L]]
  } else if (!length(attachment_key)) {
    lines <- append(lines, attachment_lines, after = metadata_line)
  } else {
    stop("The target notebook cell contains duplicate image attachments.")
  }

  temporary_path <- tempfile(
    pattern = "notebook-with-attachment-",
    tmpdir = dirname(notebook_path),
    fileext = ".ipynb"
  )
  on.exit(unlink(temporary_path), add = TRUE)
  writeLines(lines, temporary_path, useBytes = TRUE)
  temporary_json <- paste(readLines(temporary_path, warn = FALSE), collapse = "\n")
  if (!jsonlite::validate(temporary_json)) {
    validation_error <- tryCatch(
      {
        jsonlite::fromJSON(temporary_json, simplifyVector = FALSE)
        "Unknown JSON validation error."
      },
      error = function(error) conditionMessage(error)
    )
    stop(
      "Embedding the image produced invalid JSON: ", notebook_path,
      "\n", validation_error
    )
  }
  if (!file.rename(temporary_path, notebook_path)) {
    stop("Could not replace notebook after validating the embedded image.")
  }
  message("Embedded ", attachment_name, " in ", notebook_path)
}

embed_png_attachment(
  notebook_path = file.path(
    "documentation", "notebooks",
    "campus_saved_column_filter_documentation_construction.ipynb"
  ),
  image_path = file.path(
    "documentation", "images", "run_report_builder_pipeline_diagram.png"
  ),
  heading = "Report-builder pipeline map",
  attachment_name = "run_report_builder_pipeline_diagram.png",
  alt_text = paste(
    "Alma Analytics report-builder pipeline from parser review outputs through",
    "campus exclusions-documentation workbooks"
  )
)

embed_png_attachment(
  notebook_path = file.path(
    "documentation", "notebooks", "alma_analytics_catalog_file_parser.ipynb"
  ),
  image_path = file.path(
    "documentation", "images", "run_parser_pipeline_diagram.png"
  ),
  heading = "Automated parser pipeline map",
  attachment_name = "run_parser_pipeline_diagram.png",
  alt_text = paste(
    "Alma Analytics catalog parser pipeline showing every processing script,",
    "input, intermediate dataset, and output"
  )
)
