library(shiny)
library(visNetwork)
library(xml2)

extract_path <- "output/catalog_extract.rds"

if (!file.exists(extract_path)) {
  stop("Missing extract file: ", extract_path, "\nRun extract_XMLFileList.r first.")
}

catalog_all <- readRDS(extract_path)

if (!"xml_text" %in% names(catalog_all)) {
  stop("catalog_extract.rds must contain xml_text. Re-run extract_XMLFileList.r.")
}

default_if_missing <- function(x, default) {
  if (is.null(x) || length(x) == 0 || all(is.na(x)) || !nzchar(x[1])) {
    default
  } else {
    x[1]
  }
}

extract_campus <- function(item_name) {
  if (is.na(item_name) || !nzchar(item_name)) return("Unknown")
  
  campuses <- c("UCB", "UCD", "UCI", "UCLA", "UCM", "UCR", "UCSB", "UCSC", "UCSD", "UCSF")
  for (campus in campuses) {
    if (grepl(campus, item_name, fixed = TRUE)) return(campus)
  }
  
  if (grepl("GLOBAL", item_name, ignore.case = TRUE)) return("Global")
  if (grepl("FULFILLMENT", item_name, ignore.case = TRUE)) return("Fulfillment")
  if (grepl("PHYSICAL", item_name, ignore.case = TRUE)) return("Physical")
  
  "Unknown"
}

catalog_all$campus <- if ("campus" %in% names(catalog_all)) {
  ifelse(is.na(catalog_all$campus) | !nzchar(catalog_all$campus), "Unknown", catalog_all$campus)
} else {
  vapply(catalog_all$item_label, extract_campus, character(1))
}

catalog_all$subject_area <- ifelse(
  is.na(catalog_all$subject_area) | !nzchar(catalog_all$subject_area),
  "Unknown subject area",
  catalog_all$subject_area
)

catalog_all$item_label <- ifelse(
  is.na(catalog_all$item_label) | !nzchar(catalog_all$item_label),
  paste0("Block ", catalog_all$catalog_index),
  catalog_all$item_label
)

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

extract_container_tree <- function(container_node, container_name) {
  if (is.na(container_node) || length(container_node) == 0) return(NULL)
  
  expr_children <- xml2::xml_children(container_node)
  expr_children <- expr_children[xml2::xml_name(expr_children) == "expr"]
  
  list(
    label = container_name,
    children = lapply(expr_children, extract_expr_tree)
  )
}

extract_report_tree <- function(xml_txt) {
  if (is.na(xml_txt) || !nzchar(xml_txt)) return(NULL)
  
  doc <- xml2::read_xml(xml_txt)
  root <- xml2::xml_root(doc)
  
  containers <- xml2::xml_children(root)
  containers <- containers[xml2::xml_name(containers) %in% c("filter", "criteria")]
  
  if (!length(containers)) return(NULL)
  
  children <- list()
  for (cnode in containers) {
    cname <- xml2::xml_name(cnode)
    children[[length(children) + 1]] <- extract_container_tree(cnode, cname)
  }
  
  list(
    label = default_if_missing(xml2::xml_attr(root, "subjectArea"), "report"),
    children = children
  )
}

tree_to_visnetwork <- function(tree, item_label, subject_area, original_path) {
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
      data.frame(from = from, to = to, stringsAsFactors = FALSE)
    )
  }
  
  walk_tree <- function(node, parent_id, prefix, counter_env) {
    if (is.null(node)) return(invisible(NULL))
    
    counter_env$counter <- counter_env$counter + 1L
    my_id <- paste0(prefix, "_", counter_env$counter)
    
    add_node(my_id, node$label, "expr", node$label)
    add_edge(parent_id, my_id)
    
    if (length(node$children)) {
      for (child in node$children) {
        walk_tree(child, my_id, prefix, counter_env)
      }
    }
    
    invisible(my_id)
  }
  
  add_node(
    id = "root",
    label = item_label,
    group = "item",
    title = paste0(
      "Item: ", item_label, "\n",
      "Subject area: ", subject_area, "\n",
      "Path: ", original_path
    )
  )
  
  if (!is.null(tree) && length(tree$children)) {
    counter_env <- new.env(parent = emptyenv())
    counter_env$counter <- 0L
    walk_tree(tree, "root", prefix = gsub("[^A-Za-z0-9_]+", "_", item_label), counter_env = counter_env)
  }
  
  list(nodes = nodes, edges = edges)
}

ui <- fluidPage(
  titlePanel("Alma Catalog Explorer"),
  sidebarLayout(
    sidebarPanel(
      selectInput("campus", "Campus", choices = c("All", sort(unique(catalog_all$campus)))),
      selectInput("subject_area", "Subject area", choices = c("All")),
      selectInput("item_label", "Filter item", choices = c("All"))
    ),
    mainPanel(
      visNetworkOutput("graph", height = "900px")
    )
  )
)

server <- function(input, output, session) {
  
  observeEvent(input$campus, {
    dat <- catalog_all
    if (input$campus != "All") {
      dat <- dat[dat$campus == input$campus, , drop = FALSE]
    }
    subjects <- c("All", sort(unique(dat$subject_area)))
    updateSelectInput(session, "subject_area", choices = subjects, selected = "All")
  }, ignoreInit = FALSE)
  
  observeEvent(list(input$campus, input$subject_area), {
    dat <- catalog_all
    if (input$campus != "All") {
      dat <- dat[dat$campus == input$campus, , drop = FALSE]
    }
    if (input$subject_area != "All") {
      dat <- dat[dat$subject_area == input$subject_area, , drop = FALSE]
    }
    items <- c("All", sort(unique(dat$item_label)))
    updateSelectInput(session, "item_label", choices = items, selected = "All")
  }, ignoreInit = FALSE)
  
  output$graph <- renderVisNetwork({
    dat <- catalog_all
    
    if (input$campus != "All") {
      dat <- dat[dat$campus == input$campus, , drop = FALSE]
    }
    if (input$subject_area != "All") {
      dat <- dat[dat$subject_area == input$subject_area, , drop = FALSE]
    }
    if (input$item_label != "All") {
      dat <- dat[dat$item_label == input$item_label, , drop = FALSE]
    }
    
    if (nrow(dat) == 0) {
      return(
        visNetwork(
          data.frame(id = "empty", label = "No matching filters", group = "item"),
          data.frame(from = character(0), to = character(0))
        ) |>
          visNodes(shape = "box")
      )
    }
    
    row <- dat[1, , drop = FALSE]
    tree <- extract_report_tree(as.character(row$xml_text))
    graph <- tree_to_visnetwork(
      tree = tree,
      item_label = row$item_label,
      subject_area = row$subject_area,
      original_path = row$original_path
    )
    
    visNetwork(graph$nodes, graph$edges, width = "100%", height = "900px") |>
      visNodes(shape = "box", font = list(size = 13), margin = 14) |>
      visEdges(
        arrows = list(to = list(enabled = TRUE, scaleFactor = 0.7)),
        smooth = FALSE
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
      visHierarchicalLayout(
        enabled = TRUE,
        direction = "UD",
        sortMethod = "directed",
        levelSeparation = 220,
        nodeSpacing = 300,
        treeSpacing = 340,
        blockShifting = TRUE,
        edgeMinimization = TRUE,
        parentCentralization = TRUE
      ) |>
      visPhysics(
        enabled = TRUE,
        solver = "hierarchicalRepulsion",
        hierarchicalRepulsion = list(
          nodeDistance = 220,
          springLength = 220,
          centralGravity = 0.0,
          damping = 0.12,
          avoidOverlap = 1
        ),
        stabilization = list(enabled = TRUE, iterations = 300)
      )
  })
}

shinyApp(ui, server)