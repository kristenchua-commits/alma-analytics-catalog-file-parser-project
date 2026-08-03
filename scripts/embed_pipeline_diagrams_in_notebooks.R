# Embed pipeline PNGs as standard notebook display outputs. GitHub reliably
# renders saved image/png outputs, including for private repositories.

if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("The jsonlite package is required to embed notebook images.")
}

encode_png <- function(image_path) {
  image_size <- file.info(image_path)$size
  image_raw <- readBin(image_path, what = "raw", n = image_size)
  gsub("[[:space:]]", "", jsonlite::base64_enc(image_raw))
}

find_cell_bounds <- function(lines, matching_line) {
  cell_starts <- which(lines[seq_len(matching_line)] == "  {")
  cell_start <- max(cell_starts)
  later_lines <- seq.int(matching_line, length(lines))
  cell_end_candidates <- later_lines[lines[later_lines] %in% c("  },", "  }")]
  if (!length(cell_end_candidates)) stop("Could not locate a notebook cell boundary.")
  c(start = cell_start, end = cell_end_candidates[[1L]])
}

remove_markdown_attachment <- function(lines, heading, image_name) {
  heading_pattern <- paste0('"## ', heading, '\\n"')
  heading_line <- grep(heading_pattern, lines, fixed = TRUE)
  if (length(heading_line) != 1L) {
    stop("Could not uniquely locate notebook heading: ", heading)
  }
  bounds <- find_cell_bounds(lines, heading_line)
  cell_lines <- seq.int(bounds[["start"]], bounds[["end"]])

  image_line <- cell_lines[
    grepl("![", lines[cell_lines], fixed = TRUE) &
      grepl(image_name, lines[cell_lines], fixed = TRUE)
  ]
  if (length(image_line) == 1L) lines <- lines[-image_line]

  heading_line <- grep(heading_pattern, lines, fixed = TRUE)
  bounds <- find_cell_bounds(lines, heading_line)
  cell_lines <- seq.int(bounds[["start"]], bounds[["end"]])
  attachment_start <- cell_lines[
    grepl('"attachments": {', lines[cell_lines], fixed = TRUE)
  ]
  if (length(attachment_start) == 1L) {
    attachment_key <- cell_lines[
      grepl(paste0('"', image_name, '": {'), lines[cell_lines], fixed = TRUE)
    ]
    if (length(attachment_key) != 1L) {
      stop("Could not uniquely locate the embedded Markdown attachment.")
    }
    attachment_end <- attachment_key + 3L
    lines <- lines[-seq.int(attachment_start, attachment_end)]
  }

  lines
}

embed_png_output <- function(
    notebook_path,
    image_path,
    heading,
    image_name,
    alt_text) {
  lines <- readLines(notebook_path, warn = FALSE)
  encoded_image <- encode_png(image_path)
  marker <- paste0('"pipeline_diagram": "', image_name, '"')
  existing_marker <- grep(marker, lines, fixed = TRUE)

  if (length(existing_marker) == 1L) {
    bounds <- find_cell_bounds(lines, existing_marker)
    cell_lines <- seq.int(bounds[["start"]], bounds[["end"]])
    png_line <- cell_lines[grepl('"image/png":', lines[cell_lines], fixed = TRUE)]
    if (length(png_line) != 1L) stop("Could not locate the saved PNG output.")
    lines[png_line] <- paste0('      "image/png": "', encoded_image, '",')
  } else if (!length(existing_marker)) {
    lines <- remove_markdown_attachment(lines, heading, image_name)
    heading_pattern <- paste0('"## ', heading, '\\n"')
    heading_line <- grep(heading_pattern, lines, fixed = TRUE)
    bounds <- find_cell_bounds(lines, heading_line)
    output_cell <- c(
      "  {",
      '   "cell_type": "code",',
      '   "execution_count": null,',
      '   "metadata": {',
      paste0('    "pipeline_diagram": "', image_name, '"'),
      "   },",
      '   "outputs": [',
      "    {",
      '     "data": {',
      paste0('      "image/png": "', encoded_image, '",'),
      '      "text/plain": [',
      paste0('       "<', alt_text, '>"'),
      "      ]",
      "     },",
      '     "metadata": {},',
      '     "output_type": "display_data"',
      "    }",
      "   ],",
      '   "source": [',
      '    "# Embedded pipeline diagram for GitHub and Jupyter previews.\\n"',
      "   ]",
      "  },"
    )
    lines <- append(lines, output_cell, after = bounds[["end"]])
  } else {
    stop("The notebook contains duplicate pipeline-diagram output cells.")
  }

  temporary_path <- tempfile(
    pattern = "notebook-with-image-output-",
    tmpdir = dirname(notebook_path),
    fileext = ".ipynb"
  )
  on.exit(unlink(temporary_path), add = TRUE)
  writeLines(lines, temporary_path, useBytes = TRUE)
  temporary_json <- paste(readLines(temporary_path, warn = FALSE), collapse = "\n")
  if (!jsonlite::validate(temporary_json)) {
    stop("Embedding the image produced invalid JSON: ", notebook_path)
  }
  if (!file.rename(temporary_path, notebook_path)) {
    stop("Could not replace notebook after validating the embedded image.")
  }
  message("Embedded saved image output in ", notebook_path)
}

embed_png_output(
  notebook_path = file.path(
    "documentation", "notebooks",
    "campus_saved_column_filter_documentation_construction.ipynb"
  ),
  image_path = file.path(
    "documentation", "images", "run_report_builder_pipeline_diagram.png"
  ),
  heading = "Report-builder pipeline map",
  image_name = "run_report_builder_pipeline_diagram.png",
  alt_text = paste(
    "Alma Analytics report-builder pipeline from parser review outputs through",
    "campus exclusions-documentation workbooks"
  )
)

embed_png_output(
  notebook_path = file.path(
    "documentation", "notebooks", "alma_analytics_catalog_file_parser.ipynb"
  ),
  image_path = file.path(
    "documentation", "images", "run_parser_pipeline_diagram.png"
  ),
  heading = "Automated parser pipeline map",
  image_name = "run_parser_pipeline_diagram.png",
  alt_text = paste(
    "Alma Analytics catalog parser pipeline showing every processing script,",
    "input, intermediate dataset, and output"
  )
)
