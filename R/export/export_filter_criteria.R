# Stage 3: flatten filter expression trees into spreadsheet-friendly criteria.
export_filter_criteria <- function(
    input_path = "output/filter_objects.rds",
    output_path = "output/filter_criteria.csv") {
  if (!requireNamespace("xml2", quietly = TRUE)) stop("Package 'xml2' is required.")
  if (!file.exists(input_path)) stop("Missing input file: ", input_path)
  filters <- readRDS(input_path)
  required <- c("filter_object_index", "catalog_index", "object_title", "xml_text")
  missing <- setdiff(required, names(filters))
  if (length(missing)) stop("Input is missing columns: ", paste(missing, collapse = ", "))

  clean_text <- function(node) {
    value <- trimws(gsub("[[:space:]]+", " ", xml2::xml_text(node)))
    value[!nzchar(value)] <- NA_character_
    value
  }
  safe_attr <- function(node, name) {
    value <- xml2::xml_attr(node, name)
    if (length(value) == 0L || is.na(value) || !nzchar(value)) NA_character_ else value
  }
  rows <- list()
  for (i in seq_len(nrow(filters))) {
    doc <- tryCatch(xml2::read_xml(filters$xml_text[i]), error = function(e) NULL)
    if (is.null(doc)) next
    nodes <- xml2::xml_find_all(doc, ".//*[local-name()='filter']//*[local-name()='expr']")
    if (!length(nodes)) next

    paths <- xml2::xml_path(nodes)
    parent_paths <- sub("/[^/]+$", "", paths)
    parent_index <- match(parent_paths, paths)
    slash_count <- lengths(regmatches(paths, gregexpr("/", paths, fixed = TRUE)))
    depth <- slash_count - min(slash_count) + 1L
    node_type <- xml2::xml_attr(nodes, "type")
    operator <- xml2::xml_attr(nodes, "op")
    child_count <- tabulate(parent_index[!is.na(parent_index)], nbins = length(nodes))
    values <- rep(NA_character_, length(nodes))
    leaf <- child_count == 0L
    values[leaf] <- clean_text(nodes[leaf])
    label <- ifelse(!is.na(operator) & nzchar(operator), operator, node_type)
    criteria <- values
    criteria[!leaf] <- paste0(label[!leaf], " (", child_count[!leaf], " child expressions)")

    rows[[length(rows) + 1L]] <- data.frame(
      filter_object_index = filters$filter_object_index[i],
      catalog_index = filters$catalog_index[i],
      object_title = filters$object_title[i],
      subject_area = filters$subject_area[i],
      original_path = filters$original_path[i],
      criteria_index = seq_along(nodes),
      parent_criteria_index = parent_index,
      nesting_level = depth,
      node_type = node_type,
      operator = operator,
      value = values,
      criteria = criteria,
      stringsAsFactors = FALSE
    )
  }
  if (!length(rows)) stop("No filter criteria were extracted.")
  criteria <- do.call(rbind, rows)
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  write.csv(criteria, output_path, row.names = FALSE, na = "")
  message("Wrote ", output_path, " (", nrow(criteria), " criteria rows)")
  invisible(criteria)
}
