# Stage 2 inspection: summarize object types and metadata completeness.
inspect_catalog_metadata <- function(
    input_path = "output/catalog_extract.rds",
    output_path = "output/catalog_metadata_inventory.csv") {
  if (!file.exists(input_path)) stop("Missing input file: ", input_path)
  catalog <- readRDS(input_path)

  required <- c("object_kind", "xml_root_name", "object_signature",
                "object_title", "original_path", "subject_area")
  missing <- setdiff(required, names(catalog))
  if (length(missing)) stop("Input is missing columns: ", paste(missing, collapse = ", "))

  keys <- unique(catalog[c("object_kind", "xml_root_name", "object_signature")])
  rows <- lapply(seq_len(nrow(keys)), function(i) {
    selected <- catalog$object_kind == keys$object_kind[i] &
      catalog$xml_root_name == keys$xml_root_name[i] &
      catalog$object_signature == keys$object_signature[i]
    block <- catalog[selected, , drop = FALSE]
    data.frame(
      object_kind = keys$object_kind[i],
      xml_root_name = keys$xml_root_name[i],
      object_signature = keys$object_signature[i],
      object_count = nrow(block),
      missing_title_count = sum(is.na(block$object_title) | !nzchar(block$object_title)),
      missing_path_count = sum(is.na(block$original_path) | !nzchar(block$original_path)),
      missing_subject_area_count = sum(is.na(block$subject_area) | !nzchar(block$subject_area)),
      stringsAsFactors = FALSE
    )
  })

  inventory <- do.call(rbind, rows)
  inventory <- inventory[order(inventory$object_kind, inventory$xml_root_name), ]
  rownames(inventory) <- NULL
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  write.csv(inventory, output_path, row.names = FALSE, na = "")
  message("Wrote ", output_path, " (", nrow(inventory), " patterns)")
  invisible(inventory)
}

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  inspect_catalog_metadata(
    if (length(args)) args[1L] else "output/catalog_extract.rds",
    if (length(args) > 1L) args[2L] else "output/catalog_metadata_inventory.csv"
  )
}
