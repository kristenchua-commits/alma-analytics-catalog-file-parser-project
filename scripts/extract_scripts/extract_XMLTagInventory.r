# Stage 2 inventory: list XML element paths, depths, attributes, and values.
extract_xml_tag_inventory <- function(
    input_path = "output/catalog_extract.rds",
    output_path = "output/xml_tag_inventory.csv") {
  if (!requireNamespace("xml2", quietly = TRUE)) stop("Package 'xml2' is required.")
  if (!file.exists(input_path)) stop("Missing input file: ", input_path)
  catalog <- readRDS(input_path)
  if (!"xml_text" %in% names(catalog)) stop("Input must contain xml_text.")

  clean_values <- function(value) {
    value <- trimws(gsub("[[:space:]]+", " ", value))
    value[!nzchar(value)] <- NA_character_
    value
  }
  format_attributes <- function(node) {
    attrs <- xml2::xml_attrs(node)
    if (!length(attrs)) return(NA_character_)
    paste(paste0(names(attrs), "=", attrs), collapse = "; ")
  }

  rows <- list()
  for (i in seq_len(nrow(catalog))) {
    doc <- tryCatch(xml2::read_xml(catalog$xml_text[i]), error = function(e) NULL)
    if (is.null(doc)) next
    nodes <- xml2::xml_find_all(doc, ".//*")
    if (!length(nodes)) next
    paths <- xml2::xml_path(nodes)
    depths <- lengths(regmatches(paths, gregexpr("/", paths, fixed = TRUE))) - 1L
    values <- rep(NA_character_, length(nodes))
    leaf <- xml2::xml_length(nodes) == 0L
    values[leaf] <- clean_values(xml2::xml_text(nodes[leaf]))
    rows[[length(rows) + 1L]] <- data.frame(
      catalog_index = catalog$catalog_index[i],
      object_title = catalog$object_title[i],
      object_kind = catalog$object_kind[i],
      node_index = seq_along(nodes),
      tag_name = xml2::xml_name(nodes),
      nesting_level = depths,
      tag_path = paths,
      attributes = vapply(nodes, format_attributes, character(1)),
      value = values,
      stringsAsFactors = FALSE
    )
  }
  if (!length(rows)) stop("No XML tags were extracted.")
  inventory <- do.call(rbind, rows)
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  write.csv(inventory, output_path, row.names = FALSE, na = "")
  message("Wrote ", output_path, " (", nrow(inventory), " tags)")
  invisible(inventory)
}

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  extract_xml_tag_inventory(
    if (length(args)) args[1L] else "output/catalog_extract.rds",
    if (length(args) > 1L) args[2L] else "output/xml_tag_inventory.csv"
  )
}
