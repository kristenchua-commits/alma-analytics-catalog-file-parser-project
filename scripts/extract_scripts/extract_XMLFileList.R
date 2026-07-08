library(xml2)

source("scripts/read_catalog_metadata.R")
source("scripts/script_helper_functions/read_catalog_file.R")

pick_catalog_file <- function() {
  path <- system(
    "osascript -e 'POSIX path of (choose file with prompt \"Choose a .catalog file\")'",
    intern = TRUE
  )
  
  if (length(path) == 0 || !nzchar(path)) {
    stop("No file selected.")
  }
  
  path
}

catalog_path <- pick_catalog_file()
cat("Selected file:", catalog_path, "\n")

catalog_meta <- read_catalog_metadata(catalog_path, keep_strings = TRUE)
catalog_xml <- read_catalog_file(catalog_path, keep_xml = FALSE, keep_strings = TRUE)

if (nrow(catalog_meta) == 0) {
  stop("No metadata records were found in the .catalog file.")
}

if (nrow(catalog_xml) == 0) {
  stop("No XML documents were found in the .catalog file.")
}

n <- min(nrow(catalog_meta), nrow(catalog_xml))
if (nrow(catalog_meta) != nrow(catalog_xml)) {
  warning(
    "Metadata rows (", nrow(catalog_meta),
    ") do not match XML rows (", nrow(catalog_xml),
    "). Truncating to ", n, "."
  )
}

catalog_meta <- catalog_meta[seq_len(n), , drop = FALSE]
catalog_xml <- catalog_xml[seq_len(n), , drop = FALSE]

catalog_all <- cbind(
  catalog_meta,
  catalog_xml[, c("xml_root_name", "subject_area", "xml_text")]
)

catalog_all$item_label <- ifelse(
  !is.na(catalog_all$item_name) & nzchar(catalog_all$item_name),
  catalog_all$item_name,
  paste0("Block ", catalog_all$catalog_index)
)

catalog_all$item_kind <- ifelse(
  grepl("filter", catalog_all$xml_root_name, ignore.case = TRUE),
  "Filter",
  ifelse(
    grepl("column", catalog_all$xml_root_name, ignore.case = TRUE),
    "Column",
    "Other"
  )
)

catalog_all$root_name <- catalog_all$xml_root_name

output_dir <- "output"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

saveRDS(
  catalog_all,
  file = file.path(output_dir, "catalog_extract.rds")
)

write.csv(
  catalog_all[, c(
    "catalog_index",
    "source_file_name",
    "item_name",
    "item_label",
    "item_kind",
    "subject_area",
    "original_path",
    "root_name"
  )],
  file = file.path(output_dir, "catalog_extract_summary.csv"),
  row.names = FALSE
)

catalog_filters <- subset(catalog_all, item_kind == "Filter")
catalog_columns <- subset(catalog_all, item_kind == "Column")

saveRDS(catalog_filters, file = file.path(output_dir, "filter_objects.rds"))
saveRDS(catalog_columns, file = file.path(output_dir, "column_objects.rds"))

cat("Wrote:\n")
cat(" - output/catalog_extract.rds\n")
cat(" - output/catalog_extract_summary.csv\n")
cat(" - output/filter_objects.rds\n")
cat(" - output/column_objects.rds\n")
cat("\nCounts:\n")
cat(" - total objects: ", nrow(catalog_all), "\n", sep = "")
cat(" - filters: ", nrow(catalog_filters), "\n", sep = "")
cat(" - columns: ", nrow(catalog_columns), "\n", sep = "")