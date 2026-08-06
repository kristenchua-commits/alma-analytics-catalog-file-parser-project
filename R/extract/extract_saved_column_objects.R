# Stage 2: select saved-column objects for detailed parsing and review.
extract_saved_column_objects <- function(
    input_path = "output/catalog_extract.rds",
    output_rds = "output/saved_column_objects.rds",
    output_csv = "output/saved_column_objects_summary.csv") {
  if (!file.exists(input_path)) stop("Missing input file: ", input_path)
  catalog <- readRDS(input_path)
  required <- c("catalog_index", "object_kind", "xml_text")
  missing <- setdiff(required, names(catalog))
  if (length(missing)) stop("Input is missing columns: ", paste(missing, collapse = ", "))

  saved_column_objects <- catalog[catalog$object_kind == "saved_column", , drop = FALSE]
  if (!nrow(saved_column_objects)) stop("No saved-column objects were found.")
  saved_column_objects$saved_column_object_index <- seq_len(nrow(saved_column_objects))
  saved_column_objects <- saved_column_objects[c(
    "saved_column_object_index",
    setdiff(names(saved_column_objects), "saved_column_object_index")
  )]

  dir.create(dirname(output_rds), recursive = TRUE, showWarnings = FALSE)
  saveRDS(saved_column_objects, output_rds)
  write.csv(
    saved_column_objects[setdiff(names(saved_column_objects), "xml_text")],
    output_csv,
    row.names = FALSE,
    na = ""
  )
  message("Wrote ", output_rds, " (", nrow(saved_column_objects), " saved columns)")
  message("Wrote ", output_csv)
  invisible(saved_column_objects)
}
