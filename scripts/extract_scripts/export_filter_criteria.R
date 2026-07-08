# scripts/export_filter_criteria.R

library(xml2)

input_path <- "output/filter_objects.rds"
output_path <- "output/filter_criteria.csv"

if (!file.exists(input_path)) {
  stop("Missing input file: ", input_path, "\nRun export_XML_to_FilterObject.R first.")
}

filter_objects <- readRDS(input_path)

if (!"xml_text" %in% names(filter_objects)) {
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

node_operator <- function(node) {
  if (is.null(node) || length(node) == 0) return(NA_character_)
  
  op <- xml_attr(node, "op")
  if (!is.na(op) && nzchar(op)) return(op)
  
  node_type <- xml_attr(node, "type")
  if (node_type %in% c("sawx:logical", "sawx:comparison", "sawx:list")) {
    return(node_type)
  }
  
  NA_character_
}

node_criteria_text <- function(node) {
  if (is.null(node) || length(node) == 0) return(NA_character_)
  
  node_type <- xml_attr(node, "type")
  op <- xml_attr(node, "op")
  children <- xml_children(node)
  expr_children <- children[xml_name(children) == "expr"]
  
  if (length(expr_children) == 0) {
    txt <- clean_one_line(xml_text(node))
    if (!nzchar(txt)) return(NA_character_)
    
    if (node_type == "xsd:string") return(shQuote(txt))
    if (node_type == "xsd:date") return(txt)
    
    return(txt)
  }
  
  if (node_type == "sawx:logical") {
    parts <- vapply(expr_children, node_criteria_text, character(1))
    parts <- parts[nzchar(parts)]
    
    if (!length(parts)) return(NA_character_)
    
    joiner <- paste0(" ", toupper(default_if_missing(op, "and")), " ")
    return(paste0("(", paste(parts, collapse = joiner), ")"))
  }
  
  if (node_type == "sawx:comparison") {
    parts <- vapply(expr_children, node_criteria_text, character(1))
    parts <- parts[nzchar(parts)]
    
    if (!length(parts)) return(NA_character_)
    
    if (op == "between" && length(parts) >= 3) {
      return(paste0(parts[1], " BETWEEN ", parts[2], " AND ", parts[3]))
    }
    
    if (op == "notEqual" && length(parts) >= 2) {
      return(paste0(parts[1], " <> ", parts[2]))
    }
    
    if (length(parts) >= 2) {
      return(paste0(parts[1], " ", toupper(op), " ", parts[2]))
    }
    
    return(paste(parts, collapse = ", "))
  }
  
  if (node_type == "sawx:list") {
    parts <- vapply(expr_children, node_criteria_text, character(1))
    parts <- parts[nzchar(parts)]
    
    if (!length(parts)) return(NA_character_)
    
    lhs <- parts[1]
    rhs <- parts[-1]
    
    if (!length(rhs)) return(lhs)
    
    if (op == "notIn") {
      return(paste0(lhs, " NOT IN (", paste(rhs, collapse = ", "), ")"))
    }
    
    if (op == "in") {
      return(paste0(lhs, " IN (", paste(rhs, collapse = ", "), ")"))
    }
    
    return(paste0(lhs, " ", toupper(default_if_missing(op, "list")), " (", paste(rhs, collapse = ", "), ")"))
  }
  
  if (node_type %in% c("sawx:sql", "sawx:sqlExpression")) {
    txt <- clean_one_line(xml_text(node))
    if (nzchar(txt)) return(txt)
  }
  
  txt <- clean_one_line(xml_text(node))
  if (nzchar(txt)) return(txt)
  
  NA_character_
}

flatten_expr_tree <- function(node,
                              filter_object_index,
                              item_name,
                              subject_area,
                              original_path,
                              state,
                              parent_criteria_index = NA_integer_,
                              depth = 1L) {
  if (is.null(node) || length(node) == 0) return(invisible(NULL))
  
  state$criteria_counter <- state$criteria_counter + 1L
  this_criteria_index <- state$criteria_counter
  
  node_type <- xml_attr(node, "type")
  operator_value <- node_operator(node)
  
  operator_index <- NA_integer_
  if (!is.na(operator_value) && nzchar(operator_value)) {
    state$operator_counter <- state$operator_counter + 1L
    operator_index <- state$operator_counter
  }
  
  row <- data.frame(
    filter_object_index = filter_object_index,
    item_name = item_name,
    subject_area = subject_area,
    original_path = original_path,
    criteria_index = this_criteria_index,
    parent_criteria_index = parent_criteria_index,
    depth = depth,
    operator_index = operator_index,
    node_type = default_if_missing(node_type, NA_character_),
    operator = ifelse(is.na(operator_value) || !nzchar(operator_value), NA_character_, operator_value),
    criteria = node_criteria_text(node),
    stringsAsFactors = FALSE
  )
  
  state$rows[[length(state$rows) + 1L]] <- row
  
  children <- xml_children(node)
  expr_children <- children[xml_name(children) == "expr"]
  
  if (length(expr_children)) {
    for (child in expr_children) {
      flatten_expr_tree(
        node = child,
        filter_object_index = filter_object_index,
        item_name = item_name,
        subject_area = subject_area,
        original_path = original_path,
        state = state,
        parent_criteria_index = this_criteria_index,
        depth = depth + 1L
      )
    }
  }
  
  invisible(NULL)
}

all_rows <- list()

for (i in seq_len(nrow(filter_objects))) {
  row <- filter_objects[i, , drop = FALSE]
  xml_txt <- as.character(row$xml_text[1])
  
  if (is.na(xml_txt) || !nzchar(xml_txt)) next
  
  doc <- tryCatch(
    read_xml(xml_txt),
    error = function(e) NULL
  )
  if (is.null(doc)) next
  
  filter_node <- xml_find_first(doc, ".//*[local-name()='filter']")
  if (is.na(filter_node)) next
  
  expr_node <- xml_find_first(filter_node, ".//*[local-name()='expr']")
  if (is.na(expr_node)) next
  
  state <- new.env(parent = emptyenv())
  state$criteria_counter <- 0L
  state$operator_counter <- 0L
  state$rows <- list()
  
  flatten_expr_tree(
    node = expr_node,
    filter_object_index = row$filter_object_index[1],
    item_name = row$item_name[1],
    subject_area = row$subject_area[1],
    original_path = row$original_path[1],
    state = state,
    parent_criteria_index = NA_integer_,
    depth = 1L
  )
  
  block_df <- do.call(rbind, state$rows)
  all_rows[[length(all_rows) + 1L]] <- block_df
}

if (!length(all_rows)) {
  stop("No filter criteria rows were extracted.")
}

final_df <- do.call(rbind, all_rows)

dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
write.csv(final_df, output_path, row.names = FALSE)

cat("Wrote:", output_path, "\n")
cat("Rows:", nrow(final_df), "\n")