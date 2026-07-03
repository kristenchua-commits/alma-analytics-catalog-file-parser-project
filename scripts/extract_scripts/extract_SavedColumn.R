# scripts/extract_SavedColumn.R

library(xml2)

input_path <- "output/catalog_extract.rds"
output_path <- "output/saved_columns.csv"

if (!file.exists(input_path)) {
  stop("Missing input file: ", input_path, "\nRun extract_XMLFileList.r first.")
}

catalog_all <- readRDS(input_path)

if (!"xml_text" %in% names(catalog_all)) {
  stop("Input data must contain an xml_text column.")
}

default_if_missing <- function(x, default) {
  if (is.null(x) || length(x) == 0 || all(is.na(x)) || !nzchar(x[1])) {
    default
  } else {
    x[1]
  }
}

clean_one_line <- function(x) {
  x <- as.character(x)
  x <- gsub("[\r\n\t]+", " ", x)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}

safe_attr <- function(node, attr_name) {
  if (is.null(node) || length(node) == 0) return(NA_character_)
  
  val <- xml_attr(node, attr_name)
  if (is.na(val) || !nzchar(val)) return(NA_character_)
  val
}

safe_xsi_type <- function(node) {
  if (is.null(node) || length(node) == 0) return(NA_character_)
  
  val <- xml_attr(node, "xsi:type")
  if (is.na(val) || !nzchar(val)) {
    val <- xml_attr(node, "type")
  }
  
  if (is.na(val) || !nzchar(val)) return(NA_character_)
  val
}

saved_column_label <- function(node) {
  if (is.null(node) || length(node) == 0) return(NA_character_)
  
  # Prefer meaningful attributes first
  path <- safe_attr(node, "path")
  if (!is.na(path)) return(path)
  
  column_id <- safe_attr(node, "columnID")
  if (!is.na(column_id)) return(column_id)
  
  txt <- clean_one_line(xml_text(node))
  if (nzchar(txt)) return(txt)
  
  NA_character_
}

saved_column_criteria <- function(node) {
  if (is.null(node) || length(node) == 0) return(NA_character_)
  
  # Try descendant expr nodes first
  expr_nodes <- xml_find_all(node, ".//*[local-name()='expr']")
  if (length(expr_nodes)) {
    txt <- clean_one_line(paste(xml_text(expr_nodes), collapse = " "))
    if (nzchar(txt)) return(txt)
  }
  
  txt <- clean_one_line(xml_text(node))
  if (nzchar(txt)) return(txt)
  
  NA_character_
}

extract_saved_columns_from_doc <- function(doc, row_meta) {
  col_nodes <- xml_find_all(doc, ".//*[local-name()='columns']//*[local-name()='column']")
  
  if (!length(col_nodes)) return(NULL)
  
  out <- vector("list", length(col_nodes))
  
  for (j in seq_along(col_nodes)) {
    node <- col_nodes[[j]]
    
    out[[j]] <- data.frame(
      saved_object_index = row_meta$catalog_index[1],
      saved_column_index = j,
      parent_saved_column_index = NA_integer_,
      depth = 1L,
      
      source_file_name = row_meta$source_file_name[1],
      item_name = row_meta$item_label[1],
      subject_area = row_meta$subject_area[1],
      original_path = row_meta$original_path[1],
      
      saved_column_name = saved_column_label(node),
      column_id = safe_attr(node, "columnID"),
      path = safe_attr(node, "path"),
      xsi_type = safe_xsi_type(node),
      operator = safe_attr(node, "op"),
      criteria = saved_column_criteria(node),
      
      stringsAsFactors = FALSE
    )
  }
  
  do.call(rbind, out)
}

all_rows <- list()

for (i in seq_len(nrow(catalog_all))) {
  row <- catalog_all[i, , drop = FALSE]
  xml_txt <- as.character(row$xml_text[1])
  
  if (is.na(xml_txt) || !nzchar(xml_txt)) next
  
  doc <- tryCatch(
    read_xml(xml_txt),
    error = function(e) NULL
  )
  if (is.null(doc)) next
  
  block_df <- extract_saved_columns_from_doc(doc, row)
  if (is.null(block_df) || nrow(block_df) == 0) next
  
  all_rows[[length(all_rows) + 1L]] <- block_df
}

if (!length(all_rows)) {
  stop("No saved columns were extracted.")
}

final_df <- do.call(rbind, all_rows)

dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
write.csv(final_df, output_path, row.names = FALSE)

cat("Wrote:", output_path, "\n")
cat("Rows:", nrow(final_df), "\n")