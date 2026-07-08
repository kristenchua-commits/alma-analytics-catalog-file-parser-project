# scripts/export_XML_to_FilterObject.R

input_path <- "output/catalog_extract.rds"
output_rds <- "output/filter_objects.rds"
output_csv <- "output/filter_objects.csv"

if (!file.exists(input_path)) {
  stop("Missing input file: ", input_path, "\nRun extract_XMLFileList.r first.")
}

catalog_all <- readRDS(input_path)

if (!"xml_text" %in% names(catalog_all)) {
  stop("Input data must contain an xml_text column.")
}

if (!"item_kind" %in% names(catalog_all)) {
  stop("Input data must contain an item_kind column.")
}

filter_objects <- subset(catalog_all, item_kind == "Filter")

if (!nrow(filter_objects)) {
  stop("No filter objects were found in the input data.")
}

filter_objects$filter_object_index <- seq_len(nrow(filter_objects))

filter_objects <- filter_objects[, c(
  "filter_object_index",
  "catalog_index",
  "source_file_name",
  "item_name",
  "item_label",
  "item_kind",
  "subject_area",
  "original_path",
  "root_name",
  "xml_text"
)]

dir.create(dirname(output_rds), showWarnings = FALSE, recursive = TRUE)

saveRDS(filter_objects, output_rds)
write.csv(filter_objects, output_csv, row.names = FALSE)

cat("Wrote:\n")
cat(" - ", output_rds, "\n", sep = "")
cat(" - ", output_csv, "\n", sep = "")
cat("Rows: ", nrow(filter_objects), "\n", sep = "")