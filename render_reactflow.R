library(xml2)
library(jsonlite)

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
    } else if (nzchar(txt)) {
      paste0(default_if_missing(node_type, "expr"), ": ", txt)
    } else {
      default_if_missing(node_type, "expr")
    }
  } else if (node_type == "sawx:logical") {
    paste0("logical (", toupper(default_if_missing(op, "and")), ")")
  } else if (node_type == "sawx:comparison") {
    paste0("comparison (", default_if_missing(op, "op"), ")")
  } else {
    default_if_missing(node_type, "expr")
  }
  
  list(
    label = label,
    children = lapply(expr_children, extract_expr_tree)
  )
}

unwrap_xml_doc <- function(x) {
  if (inherits(x, "xml_document")) return(x)
  if (is.list(x) && length(x) == 1 && inherits(x[[1]], "xml_document")) return(x[[1]])
  x
}

tree_to_reactflow <- function(tree, prefix = "n", spacing_x = 220, spacing_y = 120) {
  if (is.null(tree)) {
    return(list(nodes = list(), edges = list()))
  }
  
  counter <- 0L
  
  new_id <- function() {
    counter <<- counter + 1L
    paste0(prefix, counter)
  }
  
  layout_node <- function(node, depth = 0, next_x = 0) {
    node_id <- new_id()
    
    if (is.null(node$children) || length(node$children) == 0) {
      x_center <- next_x
      
      rf_node <- list(
        id = node_id,
        type = "default",
        position = list(
          x = x_center * spacing_x,
          y = depth * spacing_y
        ),
        data = list(
          label = node$label
        )
      )
      
      return(list(
        nodes = list(rf_node),
        edges = list(),
        next_x = next_x + 1,
        center_x = x_center,
        id = node_id
      ))
    }
    
    child_results <- list()
    child_centers <- numeric(0)
    all_nodes <- list()
    all_edges <- list()
    current_x <- next_x
    
    for (child in node$children) {
      if (is.null(child)) next
      
      res <- layout_node(child, depth = depth + 1, next_x = current_x)
      child_results[[length(child_results) + 1]] <- res
      
      all_nodes <- c(all_nodes, res$nodes)
      all_edges <- c(all_edges, res$edges)
      
      child_centers <- c(child_centers, res$center_x)
      current_x <- res$next_x
    }
    
    if (length(child_centers) == 0) {
      x_center <- next_x
    } else {
      x_center <- mean(child_centers)
    }
    
    rf_node <- list(
      id = node_id,
      type = "default",
      position = list(
        x = x_center * spacing_x,
        y = depth * spacing_y
      ),
      data = list(
        label = node$label
      )
    )
    
    for (cr in child_results) {
      for (child_node in cr$nodes) {
        if (identical(child_node$id, cr$id)) {
          all_edges[[length(all_edges) + 1]] <- list(
            id = paste0(node_id, "-", child_node$id),
            source = node_id,
            target = child_node$id
          )
          break
        }
      }
    }
    
    list(
      nodes = c(list(rf_node), all_nodes),
      edges = all_edges,
      next_x = current_x,
      center_x = x_center,
      id = node_id
    )
  }
  
  result <- layout_node(tree, depth = 0, next_x = 0)
  
  list(
    nodes = result$nodes,
    edges = result$edges
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

sanitize_filename <- function(x) {
  x <- ifelse(is.na(x) | !nzchar(x), "unknown", x)
  gsub("[^A-Za-z0-9_-]+", "_", x)
}

# -------------------------
# Main script
# -------------------------

catalog_path <- if (length(commandArgs(trailingOnly = TRUE)) >= 1) {
  commandArgs(trailingOnly = TRUE)[1]
} else {
  pick_catalog_file()
}

output_path <- if (length(commandArgs(trailingOnly = TRUE)) >= 2) {
  commandArgs(trailingOnly = TRUE)[2]
} else {
  "output/reactflow/catalog_tree.json"
}

cat("Selected file:", catalog_path, "\n")
cat("Writing output to:", output_path, "\n")

catalog_tbl <- read_catalog_file(catalog_path, keep_xml = TRUE)

if (nrow(catalog_tbl) == 0) {
  stop("No XML documents were found in the .catalog file.")
}

blocks <- vector("list", nrow(catalog_tbl))

for (i in seq_len(nrow(catalog_tbl))) {
  doc <- unwrap_xml_doc(catalog_tbl$xml[[i]])
  summary <- extract_report_summary(doc)
  
  rf <- tree_to_reactflow(summary$filter_tree, prefix = paste0("b", i, "_"))
  
  blocks[[i]] <- list(
    catalog_index = i,
    subject_area = summary$subject_area,
    nodes = rf$nodes,
    edges = rf$edges
  )
  
  cat("Built React Flow block", i, "for subject area:", summary$subject_area, "\n")
}

out <- list(
  source_file = catalog_path,
  generated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
  blocks = blocks
)

dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
write_json(out, output_path, pretty = TRUE, auto_unbox = TRUE)

cat("Done.\n")