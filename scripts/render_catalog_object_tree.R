# Render a standalone, collapsible tree from catalog_extract_summary.csv.
#
# The CSV does not contain object-to-object dependency metadata. This diagram
# therefore represents the containment relationships encoded by original_path:
# catalog folders contain subfolders and catalog objects.

html_escape <- function(value) {
  value <- as.character(value)
  value[is.na(value)] <- ""
  value <- gsub("&", "&amp;", value, fixed = TRUE)
  value <- gsub("<", "&lt;", value, fixed = TRUE)
  value <- gsub(">", "&gt;", value, fixed = TRUE)
  value <- gsub('"', "&quot;", value, fixed = TRUE)
  gsub("'", "&#39;", value, fixed = TRUE)
}

normalize_catalog_path <- function(path) {
  path <- trimws(as.character(path))
  path <- gsub("/+", "/", path)
  path <- sub("/$", "", path)
  ifelse(startsWith(path, "/"), path, paste0("/", path))
}

catalog_path_parts <- function(path) {
  path <- sub("^/", "", normalize_catalog_path(path))
  parts <- strsplit(path, "/", fixed = TRUE)[[1L]]
  parts[nzchar(parts)]
}

build_catalog_tree <- function(catalog) {
  required <- c("object_title", "object_kind", "original_path")
  missing_columns <- setdiff(required, names(catalog))
  if (length(missing_columns)) {
    stop(
      "Input CSV is missing required column(s): ",
      paste(missing_columns, collapse = ", ")
    )
  }

  invalid_path <- is.na(catalog$original_path) |
    !nzchar(trimws(catalog$original_path))
  if (any(invalid_path)) {
    stop("original_path is blank for row(s): ",
         paste(which(invalid_path), collapse = ", "))
  }

  catalog$original_path <- normalize_catalog_path(catalog$original_path)
  nodes <- list(list(
    id = "root", parent = NA_character_, label = "Catalog objects",
    node_type = "root", kind = "", path = "/", row_index = NA_integer_
  ))
  folder_ids <- character()

  add_folder <- function(id, parent, label, path) {
    if (!(id %in% folder_ids)) {
      nodes[[length(nodes) + 1L]] <<- list(
        id = id, parent = parent, label = label, node_type = "folder",
        kind = "", path = path, row_index = NA_integer_
      )
      folder_ids <<- c(folder_ids, id)
    }
  }

  for (row_index in seq_len(nrow(catalog))) {
    parts <- catalog_path_parts(catalog$original_path[[row_index]])
    if (!length(parts)) {
      stop("original_path does not identify an object in row ", row_index)
    }

    parent <- "root"
    if (length(parts) > 1L) {
      for (depth in seq_len(length(parts) - 1L)) {
        folder_path <- paste0("/", paste(parts[seq_len(depth)], collapse = "/"))
        folder_id <- paste0("folder:", folder_path)
        add_folder(folder_id, parent, parts[[depth]], folder_path)
        parent <- folder_id
      }
    }

    title <- trimws(as.character(catalog$object_title[[row_index]]))
    if (is.na(title) || !nzchar(title)) title <- parts[[length(parts)]]
    kind <- trimws(as.character(catalog$object_kind[[row_index]]))
    if (is.na(kind) || !nzchar(kind)) kind <- "unknown"
    nodes[[length(nodes) + 1L]] <- list(
      id = paste0("object:", row_index), parent = parent, label = title,
      node_type = "object", kind = kind,
      path = catalog$original_path[[row_index]], row_index = row_index
    )
  }

  node_table <- do.call(rbind, lapply(nodes, as.data.frame,
                                      stringsAsFactors = FALSE))
  rownames(node_table) <- NULL
  list(nodes = node_table, catalog = catalog)
}

render_catalog_object_tree <- function(
    input_csv = "output/catalog_extract_summary.csv",
    output_html = "output/catalog_object_tree.html",
    open_depth = 6L) {
  if (!file.exists(input_csv)) stop("Input CSV does not exist: ", input_csv)
  open_depth <- as.integer(open_depth)
  if (is.na(open_depth) || open_depth < 0L) {
    stop("open_depth must be a non-negative integer")
  }

  catalog <- utils::read.csv(
    input_csv,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    na.strings = character()
  )
  tree <- build_catalog_tree(catalog)
  nodes <- tree$nodes
  catalog <- tree$catalog

  children_by_parent <- split(nodes$id[nodes$id != "root"],
                              nodes$parent[nodes$id != "root"])
  node_by_id <- split(nodes, nodes$id)
  object_paths <- catalog$original_path

  descendant_count <- function(node) {
    if (node$node_type == "root") return(nrow(catalog))
    if (node$node_type == "object") return(1L)
    prefix <- paste0(node$path, "/")
    sum(startsWith(object_paths, prefix))
  }

  render_node <- function(id, depth = 0L) {
    node <- node_by_id[[id]][1L, , drop = FALSE]
    child_ids <- children_by_parent[[id]]

    if (!length(child_ids)) {
      row <- catalog[node$row_index, , drop = FALSE]
      description <- if ("web_catalog_description" %in% names(row)) {
        row$web_catalog_description[[1L]]
      } else ""
      search_text <- paste(node$label, node$kind, node$path, description)
      tooltip <- paste0(node$path, if (nzchar(description)) {
        paste0(" — ", description)
      } else "")
      return(paste0(
        '<li class="node object kind-', html_escape(node$kind),
        '" data-node-type="object" data-search="',
        html_escape(tolower(search_text)), '">',
        '<span class="leaf" title="', html_escape(tooltip), '">',
        '<span class="object-marker" aria-hidden="true"></span>',
        '<span class="label">', html_escape(node$label), '</span>',
        '<span class="badge">', html_escape(node$kind), '</span>',
        '</span></li>'
      ))
    }

    child_nodes <- lapply(child_ids, function(child_id) {
      node_by_id[[child_id]][1L, , drop = FALSE]
    })
    folder_first <- vapply(child_nodes, function(child) {
      child$node_type != "folder"
    }, logical(1L))
    labels <- vapply(child_nodes, function(child) tolower(child$label),
                     character(1L))
    child_ids <- child_ids[order(folder_first, labels)]
    child_html <- vapply(child_ids, render_node, character(1L),
                         depth = depth + 1L)
    count <- descendant_count(node)
    is_open <- if (depth < open_depth) " open" else ""
    node_class <- if (node$node_type == "root") "root" else "folder"
    return(paste0(
      '<li class="node ', node_class,
      '" data-node-type="folder" data-search="',
      html_escape(tolower(node$label)), '"><details', is_open, '>',
      '<summary><span class="folder-marker" aria-hidden="true"></span>',
      '<span class="label">', html_escape(node$label), '</span>',
      '<span class="count">', count, ' object',
      if (count == 1L) "" else "s", '</span></summary>',
      '<ul>', paste0(child_html, collapse = "\n"), '</ul>',
      '</details></li>'
    ))
  }

  kind_counts <- sort(table(catalog$object_kind), decreasing = TRUE)
  kind_summary <- paste(
    paste0(html_escape(names(kind_counts)), ": ", as.integer(kind_counts)),
    collapse = " &middot; "
  )
  source_files <- if ("source_file_name" %in% names(catalog)) {
    paste(unique(catalog$source_file_name), collapse = ", ")
  } else basename(input_csv)

  css <- paste0(c(
    ":root { color-scheme: light; font-family: Inter, ui-sans-serif, system-ui, -apple-system, BlinkMacSystemFont, \"Segoe UI\", sans-serif; color: #172b4d; background: #f6f8fb; }",
    "* { box-sizing: border-box; }",
    "body { margin: 0; padding: 2rem; }",
    ".page { max-width: 1180px; margin: 0 auto; }",
    "h1 { margin: 0 0 .35rem; font-size: 1.75rem; }",
    ".subtitle, .summary { color: #5e6c84; margin: .25rem 0; }",
    ".toolbar { position: sticky; top: 0; z-index: 2; display: flex; gap: .75rem; align-items: center; flex-wrap: wrap; padding: 1rem 0; background: #f6f8fb; }",
    "input { flex: 1 1 320px; border: 1px solid #b3bac5; border-radius: 8px; padding: .72rem .85rem; font: inherit; background: white; }",
    "button { border: 1px solid #8993a4; border-radius: 7px; padding: .62rem .85rem; color: #172b4d; background: white; cursor: pointer; }",
    "button:hover { background: #ebecf0; }",
    ".tree-card { padding: 1.25rem; border: 1px solid #dfe1e6; border-radius: 12px; background: white; box-shadow: 0 3px 14px rgba(9, 30, 66, .08); overflow-x: auto; }",
    "ul { list-style: none; margin: 0; padding-left: 1.4rem; }",
    ".tree > li { padding-left: 0; }",
    ".tree ul { margin-left: .32rem; border-left: 1px solid #cfd6df; }",
    ".node { position: relative; min-width: max-content; }",
    ".tree ul > .node::before { content: \"\"; position: absolute; left: -1.08rem; top: 1rem; width: .88rem; border-top: 1px solid #cfd6df; }",
    "summary, .leaf { display: flex; align-items: center; gap: .5rem; min-height: 2rem; padding: .2rem .4rem; border-radius: 5px; }",
    "summary { cursor: pointer; font-weight: 600; }",
    "summary:hover, .leaf:hover { background: #f1f4f8; }",
    ".folder-marker { width: .9rem; height: .68rem; border-radius: 2px; background: #ffcc66; box-shadow: inset 0 0 0 1px #b78116; }",
    ".object-marker { width: .72rem; height: .9rem; border-radius: 2px; background: #dce6f6; box-shadow: inset 0 0 0 1px #6b7c93; }",
    ".count { color: #6b778c; font-size: .78rem; font-weight: 500; }",
    ".badge { margin-left: .2rem; border-radius: 999px; padding: .12rem .48rem; font-size: .72rem; color: #344563; background: #dfe1e6; }",
    ".kind-report .badge { background: #dceafa; }",
    ".kind-dashboard .badge { background: #e7ddf5; }",
    ".kind-dashboard_page .badge { background: #f1e7fa; }",
    ".kind-saved_column .badge { background: #ddf3e4; }",
    ".kind-filter .badge { background: #fff1cc; }",
    ".hidden { display: none !important; }",
    ".no-results { display: none; color: #6b778c; padding: 1rem; }",
    ".no-results.visible { display: block; }",
    "@media (max-width: 700px) { body { padding: 1rem; } .tree-card { padding: .75rem; } }"
  ), collapse = "\n")

  js <- paste0(c(
    "const search = document.getElementById('tree-search');",
    "const root = document.getElementById('catalog-tree');",
    "const noResults = document.getElementById('no-results');",
    "function setAll(open) { root.querySelectorAll('details').forEach(d => d.open = open); }",
    "document.getElementById('expand-all').addEventListener('click', () => setAll(true));",
    "document.getElementById('collapse-all').addEventListener('click', () => setAll(false));",
    "search.addEventListener('input', () => {",
    "  const query = search.value.trim().toLowerCase();",
    "  const objects = Array.from(root.querySelectorAll('.object'));",
    "  objects.forEach(node => node.classList.toggle('hidden', query && !node.dataset.search.includes(query)));",
    "  const folders = Array.from(root.querySelectorAll('.folder, .root')).reverse();",
    "  folders.forEach(node => {",
    "    const ownMatch = query && node.dataset.search.includes(query);",
    "    const childMatch = Array.from(node.querySelector(':scope > details > ul').children).some(child => !child.classList.contains('hidden'));",
    "    node.classList.toggle('hidden', Boolean(query) && !ownMatch && !childMatch);",
    "    if (query && (ownMatch || childMatch)) node.querySelector(':scope > details').open = true;",
    "  });",
    "  noResults.classList.toggle('visible', objects.every(node => node.classList.contains('hidden')));",
    "});"
  ), collapse = "\n")

  html <- paste0(
    '<!doctype html><html lang="en"><head><meta charset="utf-8">',
    '<meta name="viewport" content="width=device-width, initial-scale=1">',
    '<title>Alma Analytics catalog object tree</title><style>', css,
    '</style></head><body><main class="page">',
    '<h1>Alma Analytics catalog object tree</h1>',
    '<p class="subtitle">Folder containment derived from <code>original_path</code> in ',
    html_escape(basename(input_csv)), '.</p>',
    '<p class="summary"><strong>', nrow(catalog), '</strong> objects from ',
    html_escape(source_files), '<br>', kind_summary, '</p>',
    '<div class="toolbar"><input id="tree-search" type="search" ',
    'placeholder="Search titles, types, paths, or descriptions" ',
    'aria-label="Search catalog tree"><button id="expand-all" type="button">',
    'Expand all</button><button id="collapse-all" type="button">Collapse all</button></div>',
    '<section class="tree-card" aria-label="Catalog hierarchy">',
    '<ul id="catalog-tree" class="tree">', render_node("root"), '</ul>',
    '<div id="no-results" class="no-results">No matching objects.</div>',
    '</section></main><script>', js, '</script></body></html>'
  )

  dir.create(dirname(output_html), recursive = TRUE, showWarnings = FALSE)
  writeLines(html, output_html, useBytes = TRUE)
  invisible(normalizePath(output_html, mustWork = FALSE))
}

render_catalog_object_tree_png <- function(
    input_csv = "output/catalog_extract_summary.csv",
    output_png = "documentation/images/catalog_object_tree.png",
    width = 14,
    row_height = 0.20,
    resolution = 140) {
  if (!file.exists(input_csv)) stop("Input CSV does not exist: ", input_csv)
  catalog <- utils::read.csv(
    input_csv,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    na.strings = character()
  )
  tree <- build_catalog_tree(catalog)
  nodes <- tree$nodes
  catalog <- tree$catalog
  object_paths <- normalize_catalog_path(catalog$original_path)

  folder_nodes <- nodes[nodes$node_type == "folder", , drop = FALSE]
  folder_counts <- vapply(folder_nodes$path, function(path) {
    sum(startsWith(object_paths, paste0(path, "/")))
  }, integer(1L))
  common_folders <- folder_nodes[folder_counts == nrow(catalog), , drop = FALSE]
  if (nrow(common_folders)) {
    common_depths <- lengths(strsplit(sub("^/", "", common_folders$path),
                                      "/", fixed = TRUE))
    display_root <- common_folders[which.max(common_depths), , drop = FALSE]
    root_id <- display_root$id[[1L]]
    common_path <- display_root$path[[1L]]
    keep <- nodes$id == root_id |
      startsWith(nodes$path, paste0(common_path, "/"))
    nodes <- nodes[keep, , drop = FALSE]
    nodes$parent[nodes$id == root_id] <- NA_character_
    common_depth <- max(common_depths)
  } else {
    root_id <- "root"
    common_path <- "/"
    common_depth <- 0L
  }

  path_depth <- function(path) {
    if (identical(path, "/")) return(0L)
    length(catalog_path_parts(path))
  }
  nodes$depth <- vapply(nodes$path, path_depth, integer(1L)) - common_depth
  nodes$depth[nodes$id == root_id] <- 0L
  children_by_parent <- split(nodes$id[!is.na(nodes$parent)],
                              nodes$parent[!is.na(nodes$parent)])

  object_nodes <- nodes[nodes$node_type == "object", , drop = FALSE]
  object_nodes <- object_nodes[order(object_nodes$path, object_nodes$kind,
                                     object_nodes$label), , drop = FALSE]
  node_y <- stats::setNames(rep(NA_real_, nrow(nodes)), nodes$id)
  node_y[object_nodes$id] <- rev(seq_len(nrow(object_nodes)))

  unresolved <- nodes$id[is.na(node_y)]
  while (length(unresolved)) {
    resolved_this_pass <- character()
    for (id in unresolved) {
      child_ids <- children_by_parent[[id]]
      if (length(child_ids) && all(!is.na(node_y[child_ids]))) {
        node_y[[id]] <- mean(range(node_y[child_ids]))
        resolved_this_pass <- c(resolved_this_pass, id)
      }
    }
    if (!length(resolved_this_pass)) {
      stop("Could not resolve vertical positions for all tree nodes")
    }
    unresolved <- setdiff(unresolved, resolved_this_pass)
  }

  palette <- c(
    report = "#2F80C9",
    dashboard = "#8055B5",
    dashboard_page = "#A66BC7",
    saved_column = "#319B5D",
    filter = "#D49B18",
    unknown = "#6B778C"
  )
  object_colors <- unname(palette[object_nodes$kind])
  object_colors[is.na(object_colors)] <- palette[["unknown"]]
  image_height <- max(12, nrow(object_nodes) * row_height)
  dir.create(dirname(output_png), recursive = TRUE, showWarnings = FALSE)
  grDevices::png(
    output_png,
    width = width,
    height = image_height,
    units = "in",
    res = resolution,
    bg = "white"
  )
  on.exit(grDevices::dev.off(), add = TRUE)
  graphics::par(mar = c(1.2, 0.8, 3.5, 0.8), family = "sans", xpd = NA)
  max_depth <- max(nodes$depth)
  graphics::plot.new()
  graphics::plot.window(
    xlim = c(-0.2, max_depth + 2.5),
    ylim = c(0.3, nrow(object_nodes) + 0.7)
  )

  for (i in seq_len(nrow(nodes))) {
    if (is.na(nodes$parent[[i]])) next
    parent_index <- match(nodes$parent[[i]], nodes$id)
    parent_x <- nodes$depth[[parent_index]]
    parent_y <- node_y[[nodes$parent[[i]]]]
    child_x <- nodes$depth[[i]]
    child_y <- node_y[[nodes$id[[i]]]]
    elbow_x <- parent_x + 0.45
    graphics::segments(parent_x, parent_y, elbow_x, parent_y,
                       col = "#B8C0CC", lwd = 0.8)
    graphics::segments(elbow_x, parent_y, elbow_x, child_y,
                       col = "#B8C0CC", lwd = 0.8)
    graphics::segments(elbow_x, child_y, child_x, child_y,
                       col = "#B8C0CC", lwd = 0.8)
  }

  folder_nodes <- nodes[nodes$node_type != "object", , drop = FALSE]
  graphics::points(
    folder_nodes$depth,
    node_y[folder_nodes$id],
    pch = 15,
    cex = 0.65,
    col = "#D99A16"
  )
  graphics::text(
    folder_nodes$depth + 0.10,
    node_y[folder_nodes$id] + 0.30,
    labels = folder_nodes$label,
    adj = c(0, 0.5),
    cex = 0.58,
    font = 2,
    col = "#5C430C"
  )
  graphics::points(
    object_nodes$depth,
    node_y[object_nodes$id],
    pch = 16,
    cex = 0.42,
    col = object_colors
  )
  graphics::text(
    object_nodes$depth + 0.10,
    node_y[object_nodes$id],
    labels = paste0(object_nodes$label, "  [", object_nodes$kind, "]"),
    adj = c(0, 0.5),
    cex = 0.55,
    col = "#172B4D"
  )

  graphics::mtext(
    "Alma Analytics Catalog Object Structure",
    side = 3,
    line = 2.1,
    font = 2,
    cex = 1.2,
    col = "#172B4D"
  )
  graphics::mtext(
    paste0(nrow(catalog), " objects grouped by folder beneath ", common_path),
    side = 3,
    line = 0.8,
    cex = 0.68,
    col = "#6B778C"
  )
  legend_kinds <- names(palette)[names(palette) %in% unique(object_nodes$kind)]
  graphics::legend(
    "topright",
    legend = legend_kinds,
    col = palette[legend_kinds],
    pch = 16,
    cex = 0.55,
    bty = "n",
    ncol = length(legend_kinds),
    inset = c(0, -0.008)
  )

  invisible(normalizePath(output_png, mustWork = FALSE))
}

if (!interactive() && sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  input_csv <- if (length(args) >= 1L) args[[1L]] else {
    "output/catalog_extract_summary.csv"
  }
  output_html <- if (length(args) >= 2L) args[[2L]] else {
    "output/catalog_object_tree.html"
  }
  open_depth <- if (length(args) >= 3L) args[[3L]] else 6L
  diagram_path <- render_catalog_object_tree(input_csv, output_html, open_depth)
  message("Wrote ", diagram_path)
  if (length(args) >= 4L && nzchar(args[[4L]])) {
    image_path <- render_catalog_object_tree_png(input_csv, args[[4L]])
    message("Wrote ", image_path)
  }
}
