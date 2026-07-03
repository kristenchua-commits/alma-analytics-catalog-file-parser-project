library(xml2)

default_if_missing <- function(x, default) {
  if (is.null(x) || length(x) == 0 || all(is.na(x)) || !nzchar(x[1])) {
    default
  } else {
    x[1]
  }
}

node_criteria_text <- function(node) {
  if (is.na(node) || length(node) == 0) return(NA_character_)
  
  node_type <- xml2::xml_attr(node, "type")
  op <- xml2::xml_attr(node, "op")
  children <- xml2::xml_children(node)
  expr_children <- children[xml2::xml_name(children) == "expr"]
  
  # Leaf nodes
  if (length(expr_children) == 0) {
    txt <- trimws(xml2::xml_text(node))
    
    if (!nzchar(txt)) return(NA_character_)
    
    if (node_type == "xsd:string") return(shQuote(txt))
    if (node_type == "xsd:date") return(txt)
    
    return(txt)
  }
  
  # Logical groups
  if (node_type == "sawx:logical") {
    parts <- vapply(expr_children, node_criteria_text, character(1))
    parts <- parts[nzchar(parts)]
    
    if (!length(parts)) return(NA_character_)
    
    joiner <- paste0(" ", toupper(default_if_missing(op, "and")), " ")
    return(paste0("(", paste(parts, collapse = joiner), ")"))
  }
  
  # Comparison nodes
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
      return(paste0(parts[1], " ", op, " ", parts[2]))
    }
    
    return(paste(parts, collapse = ", "))
  }
  
  # List nodes
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
    
    return(paste0(lhs, " ", toupper(op), " ", paste(rhs, collapse = ", ")))
  }
  
  # Fallback
  xml2::xml_text(node)
}

flatten_expr_tree <- function(node, item_name, parent_index = NA_integer_, depth = 1L, state = NULL) {
  if (is.null(state)) {
    state <- new.env(parent = emptyenv())
    state$counter <- 0L
    state$rows <- list()
  }
  
  if (is.na(node) || length(node) == 0) return(state)
  
  state$counter <- state$counter + 1L
  this_index <- state$counter
  
  node_type <- xml2::xml_attr(node, "type")
  op <- xml2::xml_attr(node, "op")
  
  row <- data.frame(
    item_name = item_name,
    filter_index = this_index,
    parent_index = parent_index,
    depth = depth,
    node_type = default_if_missing(node_type, NA_character_),
    logical_operator = ifelse(is.na(op) || !nzchar(op), NA_character_, op),
    criteria = node_criteria_text(node),
    stringsAsFactors = FALSE
  )
  
  state$rows[[length(state$rows) + 1L]] <- row
  
  children <- xml2::xml_children(node)
  expr_children <- children[xml2::xml_name(children) == "expr"]
  
  if (length(expr_children)) {
    for (child in expr_children) {
      flatten_expr_tree(
        child,
        item_name = item_name,
        parent_index = this_index,
        depth = depth + 1L,
        state = state
      )
    }
  }
  
  state
}

export_filter_rows <- function(catalog_all, output_csv = "output/filter_rows.csv") {
  out_rows <- list()
  
  for (i in seq_len(nrow(catalog_all))) {
    row <- catalog_all[i, , drop = FALSE]
    xml_txt <- as.character(row$xml_text[1])
    
    if (is.na(xml_txt) || !nzchar(xml_txt)) next
    
    doc <- xml2::read_xml(xml_txt)
    root <- xml2::xml_root(doc)
    
    filter_node <- xml2::xml_find_first(doc, ".//*[local-name()='filter']")
    if (is.na(filter_node)) next
    
    expr_node <- xml2::xml_find_first(filter_node, ".//*[local-name()='expr']")
    if (is.na(expr_node)) next
    
    state <- flatten_expr_tree(
      expr_node,
      item_name = row$item_label[1],
      parent_index = NA_integer_,
      depth = 1L
    )
    
    block_df <- do.call(rbind, state$rows)
    block_df$subject_area <- row$subject_area[1]
    block_df$original_path <- row$original_path[1]
    block_df$catalog_index <- row$catalog_index[1]
    
    out_rows[[length(out_rows) + 1L]] <- block_df
  }
  
  final_df <- do.call(rbind, out_rows)
  
  dir.create(dirname(output_csv), recursive = TRUE, showWarnings = FALSE)
  write.csv(final_df, output_csv, row.names = FALSE)
  
  final_df
}