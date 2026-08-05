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

add_preparation_dashboard_relationships <- function(nodes, catalog_rds) {
  if (!file.exists(catalog_rds)) return(nodes)
  if (!requireNamespace("xml2", quietly = TRUE)) {
    warning("Package 'xml2' is unavailable; omitting dashboard references")
    return(nodes)
  }

  catalog <- readRDS(catalog_rds)
  required <- c("object_kind", "original_path", "xml_text")
  if (!all(required %in% names(catalog))) {
    warning("Catalog RDS lacks fields needed for dashboard references")
    return(nodes)
  }
  catalog$original_path <- normalize_catalog_path(catalog$original_path)
  prep_dashboards <- which(
    catalog$object_kind == "dashboard" &
      grepl(
        "/Preparation review reports/UC[^/]+ preparation review reports dashboard/",
        catalog$original_path
      )
  )
  if (!length(prep_dashboards)) return(nodes)

  object_node_id <- function(path, kind) {
    matches <- which(
      nodes$node_type == "object" & nodes$path == path & nodes$kind == kind
    )
    if (length(matches)) nodes$id[[matches[[1L]]]] else NA_character_
  }
  resolve_reference <- function(source_path, reference_path) {
    if (startsWith(reference_path, "/")) {
      normalize_catalog_path(reference_path)
    } else {
      normalize_catalog_path(file.path(dirname(source_path), reference_path))
    }
  }

  reference_nodes <- list()
  reference_index <- 0L
  for (dashboard_row in prep_dashboards) {
    dashboard_path <- catalog$original_path[[dashboard_row]]
    dashboard_id <- object_node_id(dashboard_path, "dashboard")
    if (is.na(dashboard_id)) next
    dashboard_xml <- xml2::read_xml(catalog$xml_text[[dashboard_row]])
    page_refs <- xml2::xml_find_all(
      dashboard_xml,
      ".//*[local-name()='dashboardPageRef']"
    )

    for (page_ref in page_refs) {
      page_path <- resolve_reference(
        dashboard_path,
        xml2::xml_attr(page_ref, "path")
      )
      page_id <- object_node_id(page_path, "dashboard_page")
      if (is.na(page_id)) next

      # For the relationship view, the explicit dashboardPageRef becomes the
      # page's semantic parent instead of its catalog storage folder.
      nodes$parent[nodes$id == page_id] <- dashboard_id
      page_row <- match(page_path, catalog$original_path)
      if (is.na(page_row)) next
      page_xml <- xml2::read_xml(catalog$xml_text[[page_row]])
      report_refs <- xml2::xml_find_all(
        page_xml,
        ".//*[local-name()='reportRef']"
      )

      for (report_ref in report_refs) {
        report_path <- resolve_reference(
          page_path,
          xml2::xml_attr(report_ref, "path")
        )
        report_row <- match(report_path, catalog$original_path)
        if (is.na(report_row) || catalog$object_kind[[report_row]] != "report") {
          next
        }
        reference_index <- reference_index + 1L
        source_group <- sub(
          " preparation review reports$",
          "",
          basename(dirname(report_path)),
          ignore.case = TRUE
        )
        reference_nodes[[reference_index]] <- data.frame(
          id = paste0("report-reference:", reference_index),
          parent = page_id,
          label = paste0(basename(report_path), " — ", source_group),
          node_type = "reference",
          kind = "report_reference",
          path = report_path,
          row_index = report_row,
          stringsAsFactors = FALSE
        )
      }
    }
  }

  if (length(reference_nodes)) {
    nodes <- rbind(nodes, do.call(rbind, reference_nodes))
    rownames(nodes) <- NULL
  }
  nodes
}

extract_preparation_dashboard_relationships <- function(
    input_csv = "output/catalog_extract_summary.csv",
    catalog_rds = sub("_summary[.]csv$", ".rds", input_csv)) {
  catalog <- utils::read.csv(
    input_csv,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    na.strings = character()
  )
  nodes <- add_preparation_dashboard_relationships(
    build_catalog_tree(catalog)$nodes,
    catalog_rds
  )
  references <- nodes[nodes$node_type == "reference", , drop = FALSE]
  if (!nrow(references)) return(data.frame())
  node_by_id <- split(nodes, nodes$id)

  rows <- lapply(seq_len(nrow(references)), function(i) {
    reference <- references[i, , drop = FALSE]
    page <- node_by_id[[reference$parent]][1L, , drop = FALSE]
    dashboard <- node_by_id[[page$parent]][1L, , drop = FALSE]
    source_group <- sub(
      " preparation review reports$",
      "",
      basename(dirname(reference$path)),
      ignore.case = TRUE
    )
    dashboard_folder <- basename(dirname(dashboard$path))
    campus <- sub(" preparation review reports dashboard$", "",
                  dashboard_folder, ignore.case = TRUE)
    data.frame(
      campus = campus,
      dashboard = dashboard_folder,
      dashboard_path = dashboard$path,
      page = page$label,
      page_path = page$path,
      report = basename(reference$path),
      report_path = reference$path,
      source_group = source_group,
      stringsAsFactors = FALSE
    )
  })
  relationships <- do.call(rbind, rows)
  source_order <- match(
    relationships$source_group,
    c("Electronic", "Fulfillment", "Physical")
  )
  page_order <- match(
    relationships$page,
    c(
      "eResources - Library Categories",
      "Fulfillment - Check-out Type",
      "Fulfillment - User Group",
      "Fulfillment - User Groups",
      "Physical Items - Library Categories"
    )
  )
  relationships[order(relationships$campus, source_order, page_order),
                , drop = FALSE]
}

render_preparation_dashboard_relationships_png <- function(
    input_csv = "output/catalog_extract_summary.csv",
    output_png = "documentation/images/preparation_review_dashboard_relationships.png",
    catalog_rds = sub("_summary[.]csv$", ".rds", input_csv),
    width = 18,
    height = 32,
    resolution = 140) {
  relationships <- extract_preparation_dashboard_relationships(
    input_csv,
    catalog_rds
  )
  if (!nrow(relationships)) {
    stop("No preparation-review dashboard relationships were found")
  }

  campuses <- unique(relationships$campus)
  counts <- table(relationships$campus)
  if (any(counts != 4L)) {
    stop("Expected four linked reports for every campus dashboard")
  }
  dir.create(dirname(output_png), recursive = TRUE, showWarnings = FALSE)
  grDevices::png(
    output_png,
    width = width,
    height = height,
    units = "in",
    res = resolution,
    bg = "white"
  )
  on.exit(grDevices::dev.off(), add = TRUE)
  graphics::par(mar = c(0.5, 0.5, 2.7, 0.5), family = "sans", xpd = NA)
  graphics::plot.new()
  block_height <- 5.25
  top <- length(campuses) * block_height
  graphics::plot.window(xlim = c(0, 18), ylim = c(0, top + 1.6))

  dashboard_x <- 2.35
  page_x <- 7.55
  report_x <- 14.2
  dashboard_width <- 3.9
  page_width <- 4.25
  report_width <- 6.0
  box_height <- 0.72

  draw_box <- function(x, y, box_width, box_height, label, fill,
                       border = "#52606D", cex = 0.58, font = 1) {
    graphics::rect(
      x - box_width / 2,
      y - box_height / 2,
      x + box_width / 2,
      y + box_height / 2,
      col = fill,
      border = border,
      lwd = 1
    )
    lines <- strwrap(label, width = max(18L, floor(box_width * 11)))
    line_step <- 0.20
    start_y <- y + (length(lines) - 1L) * line_step / 2
    graphics::text(
      x,
      start_y - (seq_along(lines) - 1L) * line_step,
      labels = lines,
      cex = cex,
      font = font,
      col = "#172B4D"
    )
  }

  group_fill <- c(
    Electronic = "#DCEBFA",
    Fulfillment = "#FFF1CC",
    Physical = "#DDF3E4"
  )
  for (campus_index in seq_along(campuses)) {
    campus <- campuses[[campus_index]]
    campus_rows <- relationships[relationships$campus == campus,
                                 , drop = FALSE]
    block_top <- top - (campus_index - 1L) * block_height
    row_y <- block_top - c(0.85, 1.85, 2.85, 3.85)
    dashboard_y <- mean(range(row_y))

    if (campus_index %% 2L == 0L) {
      graphics::rect(
        0.15, block_top - 4.65, 17.85, block_top + 0.05,
        col = "#F7F9FC", border = NA
      )
    }

    # Draw connectors first so the opaque boxes always sit above the lines.
    dashboard_right <- dashboard_x + dashboard_width / 2
    page_left <- page_x - page_width / 2
    page_right <- page_x + page_width / 2
    report_left <- report_x - report_width / 2
    branch_x <- page_left - 0.42
    graphics::segments(
      dashboard_right, dashboard_y, branch_x, dashboard_y,
      col = "#16838B", lwd = 1.4
    )
    graphics::segments(
      branch_x, min(row_y), branch_x, max(row_y),
      col = "#16838B", lwd = 1.4
    )
    graphics::segments(
      branch_x, row_y, page_left, row_y,
      col = "#16838B", lwd = 1.4
    )
    graphics::arrows(
      page_right, row_y, report_left, row_y,
      length = 0.07, angle = 22, col = "#16838B", lwd = 1.2
    )

    draw_box(
      dashboard_x, dashboard_y, dashboard_width, 1.05,
      campus_rows$dashboard[[1L]], "#E7DDF5", cex = 0.61, font = 2
    )
    for (row_index in seq_len(4L)) {
      relationship <- campus_rows[row_index, , drop = FALSE]
      draw_box(
        page_x, row_y[[row_index]], page_width, box_height,
        relationship$page, "#F1E7FA", cex = 0.56
      )
      report_label <- paste0(
        relationship$source_group, " report\n", relationship$report
      )
      draw_box(
        report_x, row_y[[row_index]], report_width, box_height,
        report_label, group_fill[[relationship$source_group]], cex = 0.53
      )
    }
  }

  header_y <- top + 0.65
  graphics::text(dashboard_x, header_y, "Campus preparation-review dashboard",
                 font = 2, cex = 0.78, col = "#172B4D")
  graphics::text(page_x, header_y, "Dashboard pages",
                 font = 2, cex = 0.78, col = "#172B4D")
  graphics::text(report_x, header_y, "Referenced campus reports",
                 font = 2, cex = 0.78, col = "#172B4D")
  graphics::mtext(
    "Campus Preparation-Review Dashboard Composition",
    side = 3, line = 1.6, font = 2, cex = 1.25, col = "#172B4D"
  )
  graphics::mtext(
    "Explicit sawd:dashboardPageRef and sawd:reportRef relationships from catalog_extract.rds",
    side = 3, line = 0.45, cex = 0.72, col = "#6B778C"
  )

  invisible(normalizePath(output_png, mustWork = FALSE))
}

collapse_objects_for_tree_overview <- function(nodes) {
  objects <- nodes[nodes$node_type == "object", , drop = FALSE]
  folders <- nodes[nodes$node_type != "object", , drop = FALSE]
  if (!nrow(objects)) return(nodes)

  group_key <- paste(objects$parent, objects$kind, sep = "\r")
  object_groups <- split(objects, group_key)
  summaries <- lapply(seq_along(object_groups), function(i) {
    group <- object_groups[[i]]
    count <- nrow(group)
    data.frame(
      id = paste0("object-summary:", i),
      parent = group$parent[[1L]],
      label = paste0(
        group$kind[[1L]], " — ", count, " object", if (count == 1L) "" else "s"
      ),
      node_type = "summary",
      kind = group$kind[[1L]],
      path = group$path[[1L]],
      row_index = NA_integer_,
      stringsAsFactors = FALSE
    )
  })
  collapsed <- rbind(folders, do.call(rbind, summaries))
  rownames(collapsed) <- NULL
  collapsed
}

render_catalog_object_tree_png <- function(
    input_csv = "output/catalog_extract_summary.csv",
    output_png = "documentation/images/catalog_object_tree.png",
    width = 22,
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

  nodes <- collapse_objects_for_tree_overview(nodes)

  path_depth <- function(path) {
    if (identical(path, "/")) return(0L)
    length(catalog_path_parts(path))
  }
  nodes$depth <- vapply(nodes$path, path_depth, integer(1L)) - common_depth
  nodes$depth[nodes$id == root_id] <- 0L
  summary_rows <- nodes$node_type == "summary"
  if (any(summary_rows)) {
    depth_by_id <- stats::setNames(nodes$depth, nodes$id)
    nodes$depth[summary_rows] <-
      depth_by_id[nodes$parent[summary_rows]] + 1L
  }
  children_by_parent <- split(nodes$id[!is.na(nodes$parent)],
                              nodes$parent[!is.na(nodes$parent)])
  node_by_id <- split(nodes, nodes$id)

  ordered_children <- function(id) {
    child_ids <- children_by_parent[[id]]
    if (!length(child_ids)) return(character())
    child_nodes <- lapply(child_ids, function(child_id) {
      node_by_id[[child_id]][1L, , drop = FALSE]
    })
    folder_first <- vapply(child_nodes, function(child) {
      child$node_type %in% c("object", "reference", "summary")
    }, logical(1L))
    labels <- vapply(child_nodes, function(child) tolower(child$label),
                     character(1L))
    child_ids[order(folder_first, labels)]
  }

  preorder <- function(id) {
    child_ids <- ordered_children(id)
    c(id, unlist(lapply(child_ids, preorder), use.names = FALSE))
  }
  draw_order <- preorder(root_id)
  if (!setequal(draw_order, nodes$id)) {
    stop("The display tree does not contain every catalog node")
  }
  nodes <- nodes[match(draw_order, nodes$id), , drop = FALSE]
  node_by_id <- split(nodes, nodes$id)
  node_y <- stats::setNames(rev(seq_len(nrow(nodes))), nodes$id)
  level_gap <- 6.5
  node_x <- stats::setNames(nodes$depth * level_gap, nodes$id)
  object_nodes <- nodes[nodes$node_type == "summary", , drop = FALSE]

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
  image_height <- max(12, nrow(nodes) * row_height)
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
  max_x <- max(node_x)
  graphics::plot.new()
  graphics::plot.window(
    xlim = c(-0.2, max_x + 8.0),
    ylim = c(0.3, nrow(nodes) + 0.7)
  )

  parent_ids <- nodes$id[vapply(nodes$id, function(id) {
    length(children_by_parent[[id]]) > 0L
  }, logical(1L))]
  for (parent_id in parent_ids) {
    child_ids <- ordered_children(parent_id)
    parent_x <- node_x[[parent_id]]
    parent_y <- node_y[[parent_id]]
    child_x <- node_x[child_ids]
    child_y <- node_y[child_ids]
    branch_x <- min(child_x) - 0.48
    parent_kind <- node_by_id[[parent_id]]$kind[[1L]]
    branch_color <- "#AEB8C5"
    branch_lty <- 1
    graphics::segments(parent_x, parent_y, branch_x, parent_y,
                       col = branch_color, lwd = 0.9, lty = branch_lty)
    if (length(child_ids) > 1L) {
      graphics::segments(branch_x, min(child_y), branch_x, max(child_y),
                         col = branch_color, lwd = 0.9, lty = branch_lty)
    }
    graphics::segments(branch_x, child_y, child_x, child_y,
                       col = branch_color, lwd = 0.9, lty = branch_lty)
  }

  folder_nodes <- nodes[nodes$node_type %in% c("folder", "root"),
                        , drop = FALSE]
  graphics::points(
    node_x[folder_nodes$id],
    node_y[folder_nodes$id],
    pch = 15,
    cex = 0.65,
    col = "#D99A16"
  )
  graphics::text(
    node_x[folder_nodes$id] + 0.12,
    node_y[folder_nodes$id] + 0.28,
    labels = folder_nodes$label,
    adj = c(0, 0.5),
    cex = 0.58,
    font = 2,
    col = "#5C430C"
  )
  graphics::points(
    node_x[object_nodes$id],
    node_y[object_nodes$id],
    pch = 16,
    cex = 0.42,
    col = object_colors
  )
  graphics::text(
    node_x[object_nodes$id] + 0.12,
    node_y[object_nodes$id],
    labels = paste0(object_nodes$label, "  [", object_nodes$kind, "]"),
    adj = c(0, 0.5),
    cex = 0.55,
    col = "#172B4D"
  )

  graphics::mtext(
    "Alma Analytics Catalog Object Tree Overview",
    side = 3,
    line = 2.1,
    font = 2,
    cex = 1.2,
    col = "#172B4D"
  )
  graphics::mtext(
    paste0(
      nrow(catalog), " objects summarized by type within folders beneath ",
      common_path
    ),
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
    relationship_path <- file.path(
      dirname(args[[4L]]),
      "preparation_review_dashboard_relationships.png"
    )
    relationship_path <- render_preparation_dashboard_relationships_png(
      input_csv,
      relationship_path
    )
    message("Wrote ", relationship_path)
  }
}
