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

unwrap_xml_doc <- function(x) {
  if (inherits(x, "xml_document")) return(x)
  if (is.list(x) && length(x) == 1 && inherits(x[[1]], "xml_document")) return(x[[1]])
  x
}

default_if_missing <- function(x, default) {
  if (is.null(x) || length(x) == 0 || all(is.na(x)) || !nzchar(x[1])) {
    default
  } else {
    x[1]
  }
}

extract_expr_tree <- function(node) {
  if (is.na(node) || length(node) == 0) return(NULL)
  
  node_type <- xml_attr(node, "type")
  op <- xml_attr(node, "op")
  children <- xml_children(node)
  expr_children <- children[xml_name(children) == "expr"]
  
  label <- if (length(expr_children) == 0) {
    txt <- trimws(xml_text(node))
    
    if (node_type == "xsd:string") {
      paste0("string: ", shQuote(txt))
    } else if (node_type == "xsd:date") {
      paste0("date: ", txt)
    } else if (nzchar(txt)) {
      paste0(default_if_missing(node_type, "expr"), ": ", txt)
    } else {
      default_if_missing(node_type, "expr")
    }
  } else if (node_type == "sawx:logical") {
    paste0("logical (", toupper(default_if_missing(op, "and")), ")")
  } else if (node_type == "sawx:comparison") {
    paste0("comparison (", default_if_missing(op, "op"), ")")
  } else if (node_type == "sawx:list") {
    paste0("list (", toupper(default_if_missing(op, "in")), ")")
  } else {
    default_if_missing(node_type, "expr")
  }
  
  list(
    label = label,
    children = lapply(expr_children, extract_expr_tree)
  )
}

extract_report_summary <- function(doc) {
  root <- xml_root(doc)
  
  filter_node <- xml_find_first(doc, ".//*[local-name()='filter']")
  expr_node <- if (!is.na(filter_node)) {
    xml_find_first(filter_node, ".//*[local-name()='expr']")
  } else {
    NA
  }
  
  list(
    subject_area = xml_attr(root, "subjectArea"),
    filter_tree = if (!is.na(expr_node)) extract_expr_tree(expr_node) else NULL
  )
}

print_expr_tree <- function(tree, indent = "") {
  if (is.null(tree)) return(invisible(NULL))
  
  cat(indent, "- ", tree$label, "\n", sep = "")
  
  if (length(tree$children)) {
    for (child in tree$children) {
      print_expr_tree(child, paste0(indent, "  "))
    }
  }
  
  invisible(NULL)
}

xml_doc_to_text <- function(doc) {
  doc <- unwrap_xml_doc(doc)
  if (is.null(doc) || (is.atomic(doc) && all(is.na(doc)))) return(NA_character_)
  paste0(as.character(doc), collapse = "")
}

# -------------------------
# Main
# -------------------------

catalog_path <- pick_catalog_file()
cat("Selected file:", catalog_path, "\n")

catalog_meta <- read_catalog_metadata(catalog_path, keep_strings = TRUE)
catalog_xml  <- read_catalog_file(catalog_path, keep_xml = TRUE, keep_strings = TRUE)

if (nrow(catalog_meta) == 0) {
  stop("No metadata records were found in the .catalog file.")
}

if (nrow(catalog_xml) == 0) {
  stop("No XML documents were found in the .catalog file.")
}

if (nrow(catalog_meta) != nrow(catalog_xml)) {
  warning(
    "Metadata rows (", nrow(catalog_meta),
    ") do not match XML rows (", nrow(catalog_xml), "). Joining by catalog_index."
  )
}

catalog_all <- merge(
  catalog_meta,
  catalog_xml,
  by = "catalog_index",
  all = TRUE,
  suffixes = c("_meta", "_xml")
)

# Preserve a readable label for each saved filter
catalog_all$item_label <- ifelse(
  !is.na(catalog_all$item_name) & nzchar(catalog_all$item_name),
  catalog_all$item_name,
  paste0("Block ", catalog_all$catalog_index)
)

# Convert XML docs to plain text so they survive saveRDS()
catalog_all$xml_text <- vapply(catalog_all$xml, xml_doc_to_text, character(1))

# Optional parsed tree for console inspection / debugging
catalog_all$filter_tree <- lapply(catalog_all$xml_text, function(xml_txt) {
  if (is.na(xml_txt) || !nzchar(xml_txt)) return(NULL)
  doc <- xml2::read_xml(xml_txt)
  extract_report_summary(doc)$filter_tree
})

# Optional console summary
cat("\nFound", nrow(catalog_all), "combined block(s)\n\n")

for (i in seq_len(nrow(catalog_all))) {
  cat("============================\n")
  cat("Block", catalog_all$catalog_index[i], "\n")
  cat("Item name:", catalog_all$item_label[i], "\n")
  cat("Subject area:", catalog_all$subject_area[i], "\n")
  cat("Original path:", catalog_all$original_path[i], "\n")
  cat("============================\n\n")
  
  print_expr_tree(catalog_all$filter_tree[[i]])
  cat("\n")
}

# Save a clean extract for rendering
output_dir <- "output"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
str(catalog_all)
catalog_out <- catalog_all[, c(
  "catalog_index",
  "source_file_name_meta",
  "item_name",
  "item_label",
  "subject_area",
  "original_path",
  "object_signature",
  "owner_id",
  "creator_id",
  "item_type",
  "created_year",
  "created_month",
  "created_day",
  "created_hour",
  "created_minute",
  "created_second",
  "wc_build",
  "wc_desc",
  "source_file_name_xml",
  "root_name",
  "xml_text"
)]

names(catalog_out)[names(catalog_out) == "source_file_name_meta"] <- "source_file_name"
catalog_out$source_file_name_xml <- NULL

saveRDS(
  catalog_out,
  file = file.path(output_dir, "catalog_extract.rds")
)

write.csv(
  catalog_out[, c(
    "catalog_index",
    "source_file_name",
    "item_name",
    "item_label",
    "subject_area",
    "original_path",
    "root_name"
  )],
  file = file.path(output_dir, "catalog_extract_summary.csv"),
  row.names = FALSE
)

cat("Wrote:\n")
cat(" - output/catalog_extract.rds\n")
cat(" - output/catalog_extract_summary.csv\n")