library(visNetwork)
library(htmlwidgets)

# If you saved the extract as an RDS, this script stays purely in rendering mode.
extract_path <- "output/catalog_extract.rds"

if (!file.exists(extract_path)) {
  stop("Missing extract file: ", extract_path, "\nRun extract_XMLFileList.r first.")
}

catalog_all <- readRDS(extract_path)

# -----------------------------
# Helpers
# -----------------------------
default_if_missing <- function(x, default) {
  if (is.null(x) || length(x) == 0 || all(is.na(x)) || !nzchar(x[1])) {
    default
  } else {
    x[1]
  }
}

unwrap_xml_doc <- function(x) {
  if (inherits(x, "xml_document")) return(x)
  if (is.list(x) && length(x) == 1 && inherits(x[[1]], "xml_document")) return(x[[1]])
  x
}

sanitize_id <- function(x) {
  x <- as.character(x)
  x <- gsub("[^A-Za-z0-9_]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)
  if (!nzchar(x)) x <- "node"
  x
}

extract_expr_tree <- function(node) {
  if (is.na(node) || length(node) == 0) return(NULL)
  
  node_type <- xml2::xml_attr(node, "type")
  op <- xml2::xml_attr(node, "op")
  children <- xml2::xml_children(node)
  expr_children <- children[xml2::xml_name(children) == "expr"]
  
  label <- if (length(expr_children) == 0) {
    txt <- trimws(xml2::xml_text(node))
    
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

tree_to_visnetwork <- function(catalog_all) {
  nodes <- data.frame(
    id = character(0),
    label = character(0),
    group = character(0),
    title = character(0),
    stringsAsFactors = FALSE
  )
  
  edges <- data.frame(
    from = character(0),
    to = character(0),
    stringsAsFactors = FALSE
  )
  
  add_node <- function(id, label, group, title = label) {
    nodes <<- rbind(
      nodes,
      data.frame(
        id = id,
        label = label,
        group = group,
        title = title,
        stringsAsFactors = FALSE
      )
    )
  }
  
  add_edge <- function(from, to) {
    edges <<- rbind(
      edges,
      data.frame(
        from = from,
        to = to,
        stringsAsFactors = FALSE
      )
    )
  }
  
  walk_tree <- function(tree, parent_id, prefix, counter_env) {
    if (is.null(tree)) return(invisible(NULL))
    
    counter_env$counter <- counter_env$counter + 1L
    my_id <- paste0(prefix, "_", counter_env$counter)
    
    add_node(
      id = my_id,
      label = tree$label,
      group = "expr",
      title = tree$label
    )
    add_edge(parent_id, my_id)
    
    if (length(tree$children)) {
      for (child in tree$children) {
        walk_tree(child, my_id, prefix, counter_env)
      }
    }
    
    invisible(my_id)
  }
  
  # Root node
  source_file <- attr(catalog_all, "source_file_name")
  if (is.null(source_file) || !nzchar(source_file)) {
    source_file <- "catalog file"
  }
  
  add_node(
    id = "catalog_root",
    label = source_file,
    group = "root",
    title = source_file
  )
  
  # Group by subject area, then item label, then expression tree
  subject_areas <- unique(catalog_all$subject_area)
  subject_areas <- subject_areas[!is.na(subject_areas) & nzchar(subject_areas)]
  
  subject_ids <- setNames(character(0), character(0))
  
  for (sa in subject_areas) {
    sa_id <- paste0("subject_", sanitize_id(sa))
    subject_ids[[sa]] <- sa_id
    
    add_node(
      id = sa_id,
      label = sa,
      group = "subject",
      title = sa
    )
    add_edge("catalog_root", sa_id)
  }
  
  # Some rows may have NA subject_area
  if (any(is.na(catalog_all$subject_area) | !nzchar(catalog_all$subject_area))) {
    sa_id <- "subject_Unknown"
    add_node(
      id = sa_id,
      label = "Unknown subject area",
      group = "subject",
      title = "Unknown subject area"
    )
    add_edge("catalog_root", sa_id)
  }
  
  for (i in seq_len(nrow(catalog_all))) {
    row <- catalog_all[i, ]
    
    subject_area <- row$subject_area
    if (is.na(subject_area) || !nzchar(subject_area)) {
      subject_area <- "Unknown subject area"
      sa_id <- "subject_Unknown"
    } else {
      sa_id <- subject_ids[[subject_area]]
      if (is.null(sa_id) || !nzchar(sa_id)) {
        sa_id <- paste0("subject_", sanitize_id(subject_area))
      }
    }
    
    item_label <- row$item_label
    if (is.na(item_label) || !nzchar(item_label)) {
      item_label <- paste0("Block ", row$catalog_index)
    }
    
    item_id <- paste0("item_", row$catalog_index)
    add_node(
      id = item_id,
      label = item_label,
      group = "item",
      title = paste0(
        "Item: ", item_label, "\n",
        "Subject area: ", subject_area, "\n",
        "Path: ", ifelse(is.na(row$original_path), "", row$original_path)
      )
    )
    add_edge(sa_id, item_id)
    
    # Expand the filter tree under each item
    xml_txt <- as.character(catalog_all$xml_text[i])
    
    if (length(xml_txt) == 1L && !is.na(xml_txt) && nzchar(xml_txt)) {
      doc <- xml2::read_xml(xml_txt)
      filter_node <- xml2::xml_find_first(doc, ".//*[local-name()='filter']")
      expr_node <- if (!is.na(filter_node)) {
        xml2::xml_find_first(filter_node, ".//*[local-name()='expr']")
      } else {
        NA
      }
      
      if (!is.na(expr_node)) {
        tree <- extract_expr_tree(expr_node)
        counter_env <- new.env(parent = emptyenv())
        counter_env$counter <- 0L
        walk_tree(tree, item_id, prefix = paste0("item_", row$catalog_index), counter_env)
      }
    }
  }
  
  list(nodes = nodes, edges = edges)
}

# -----------------------------
# Build and render
# -----------------------------
graph <- tree_to_visnetwork(catalog_all)

visNetwork(graph$nodes, graph$edges, width = "100%", height = "900px") |>
  visNodes(
    shape = "box",
    font = list(size = 13),
    margin = 10
  ) |>
  visEdges(
    arrows = list(to = list(enabled = TRUE, scaleFactor = 0.7))
  ) |>
  visGroups(
    groupname = "root",
    color = list(background = "#ffe6cc", border = "#d79b00")
  ) |>
  visGroups(
    groupname = "subject",
    color = list(background = "#d5e8d4", border = "#82b366")
  ) |>
  visGroups(
    groupname = "item",
    color = list(background = "#dae8fc", border = "#6c8ebf")
  ) |>
  visGroups(
    groupname = "expr",
    color = list(background = "#fff2cc", border = "#d6b656")
  ) |>
  visOptions(
    highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE),
    nodesIdSelection = TRUE
  ) |>
  visPhysics(stabilization = TRUE) |>
  visHierarchicalLayout(
    enabled = TRUE,
    direction = "UD",
    sortMethod = "directed",
    levelSeparation = 130,
    nodeSpacing = 160
  )