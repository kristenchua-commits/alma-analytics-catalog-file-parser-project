# Stage 2: flatten column definitions from saved-column objects.
extract_saved_columns <- function(
    input_path = "output/saved_column_objects.rds",
    output_path = "output/saved_columns.csv") {
  if (!requireNamespace("xml2", quietly = TRUE)) stop("Package 'xml2' is required.")
  if (!file.exists(input_path)) stop("Missing input file: ", input_path)
  catalog <- readRDS(input_path)
  required <- c(
    "saved_column_object_index", "catalog_index", "object_kind",
    "object_title", "subject_area", "original_path", "xml_text"
  )
  missing <- setdiff(required, names(catalog))
  if (length(missing)) stop("Input is missing columns: ", paste(missing, collapse = ", "))

  if (!nrow(catalog) || any(catalog$object_kind != "saved_column")) {
    stop("Input must contain only saved-column objects.")
  }

  clean_text <- function(node) {
    value <- trimws(gsub("[[:space:]]+", " ", xml2::xml_text(node)))
    if (!nzchar(value)) NA_character_ else value
  }
  safe_attr <- function(node, name) {
    value <- xml2::xml_attr(node, name)
    if (length(value) == 0L || is.na(value) || !nzchar(value)) NA_character_ else value
  }
  first_caption <- function(node) {
    caption <- xml2::xml_find_first(node, ".//*[local-name()='caption']/*[local-name()='text']")
    if (inherits(caption, "xml_missing")) NA_character_ else clean_text(caption)
  }

  rows <- list()
  for (i in seq_len(nrow(catalog))) {
    doc <- tryCatch(xml2::read_xml(catalog$xml_text[i]), error = function(e) NULL)
    if (is.null(doc)) next
    nodes <- xml2::xml_find_all(doc, ".//*[local-name()='column']")
    if (!length(nodes)) {
      # Some saved-column objects describe one column directly at the root.
      nodes <- xml2::xml_root(doc)
    }
    rows[[length(rows) + 1L]] <- do.call(rbind, lapply(seq_along(nodes), function(j) {
      node <- nodes[[j]]
      expr <- xml2::xml_find_first(node, ".//*[local-name()='expr']")
      data.frame(
        saved_column_object_index = catalog$saved_column_object_index[i],
        catalog_index = catalog$catalog_index[i],
        saved_column_index = j,
        object_title = catalog$object_title[i],
        subject_area = catalog$subject_area[i],
        original_path = catalog$original_path[i],
        column_caption = first_caption(node),
        column_id = safe_attr(node, "columnID"),
        column_path = safe_attr(node, "path"),
        data_type = safe_attr(node, "type"),
        formula = if (inherits(expr, "xml_missing")) NA_character_ else clean_text(expr),
        stringsAsFactors = FALSE
      )
    }))
  }
  if (!length(rows)) stop("No saved-column rows were extracted.")
  saved_columns <- do.call(rbind, rows)
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  write.csv(saved_columns, output_path, row.names = FALSE, na = "")
  message("Wrote ", output_path, " (", nrow(saved_columns), " rows)")
  invisible(saved_columns)
}
