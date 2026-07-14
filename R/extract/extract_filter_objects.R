# Stage 2: select saved filter objects for criteria parsing.
extract_filter_objects <- function(
    input_path = "output/catalog_extract.rds",
    output_rds = "output/filter_objects.rds",
    output_csv = "output/filter_objects_summary.csv") {
  if (!file.exists(input_path)) stop("Missing input file: ", input_path)
  catalog <- readRDS(input_path)
  required <- c("catalog_index", "object_kind", "xml_text")
  missing <- setdiff(required, names(catalog))
  if (length(missing)) stop("Input is missing columns: ", paste(missing, collapse = ", "))

  filter_objects <- catalog[catalog$object_kind == "filter", , drop = FALSE]
  if (!nrow(filter_objects)) stop("No filter objects were found.")
  filter_objects$filter_object_index <- seq_len(nrow(filter_objects))
  filter_objects <- filter_objects[c(
    "filter_object_index", setdiff(names(filter_objects), "filter_object_index")
  )]

  dir.create(dirname(output_rds), recursive = TRUE, showWarnings = FALSE)
  saveRDS(filter_objects, output_rds)
  write.csv(
    filter_objects[setdiff(names(filter_objects), "xml_text")],
    output_csv,
    row.names = FALSE,
    na = ""
  )
  message("Wrote ", output_rds, " (", nrow(filter_objects), " filters)")
  message("Wrote ", output_csv)
  invisible(filter_objects)
}
