# Create a flow diagram of the Alma Analytics .catalog parsing pipeline.

render_pipeline_diagram <- function(
    output_path = "documentation/images/run_parser_pipeline_diagram.png",
    width = 20,
    height = 12) {
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  extension <- tolower(tools::file_ext(output_path))
  if (extension == "png") {
    grDevices::png(output_path, width = width, height = height,
                   units = "in", res = 160, bg = "white")
  } else if (extension == "pdf") {
    grDevices::pdf(output_path, width = width, height = height,
                   useDingbats = FALSE)
  } else {
    stop("Unsupported output format. Use a .png or .pdf file extension.")
  }
  on.exit(grDevices::dev.off(), add = TRUE)

  graphics::par(
    mar = c(0.3, 0.3, 0.7, 0.3),
    xpd = NA,
    family = "sans",
    bg = "white"
  )
  graphics::plot.new()
  graphics::plot.window(xlim = c(0, 20), ylim = c(0, 12), asp = 1)

  palette <- list(
    input = "#DCEBFA",
    process = "#DDF3E4",
    intermediate = "#FFF1CC",
    final = "#E7DDF5",
    diagnostic = "#E8ECF1",
    border = "#425466",
    text = "#172B4D",
    arrow = "#6B7C93"
  )

  draw_box <- function(x, y, box_width, box_height, label, fill,
                       border = palette$border, font = 1, cex = 0.78) {
    graphics::rect(
      x - box_width / 2, y - box_height / 2,
      x + box_width / 2, y + box_height / 2,
      col = fill, border = border, lwd = 1.3
    )
    label_lines <- strsplit(label, "\n", fixed = TRUE)[[1L]]
    lines <- unlist(lapply(
      label_lines,
      strwrap,
      width = max(16L, floor(box_width * 12))
    ))
    line_height <- 0.22
    start_y <- y + (length(lines) - 1L) * line_height / 2
    for (i in seq_along(lines)) {
      graphics::text(x, start_y - (i - 1L) * line_height, lines[i],
                     col = palette$text, font = font, cex = cex)
    }
  }

  connect <- function(x1, y1, x2, y2, from_height = 0.35, to_height = 0.35) {
    graphics::arrows(
      x1, y1 - from_height,
      x2, y2 + to_height,
      length = 0.08, angle = 22, lwd = 1.4,
      col = palette$arrow
    )
  }

  graphics::text(
    10, 11.72,
    "Alma Analytics Catalog Parser Pipeline",
    font = 2, cex = 1.35, col = palette$text
  )
  graphics::text(
    10, 11.38,
    "Automated parser scripts, inputs, intermediate datasets, and outputs",
    cex = 0.82, col = palette$arrow
  )

  # Entry points and input.
  draw_box(5.5, 10.65, 5.0, 0.88,
           paste(
             "Optional interactive entry point",
             "scripts/choose_catalog_file_and_run_pipeline.R",
             "R/choose_catalog_file.R",
             sep = "\n"
           ),
           palette$process, font = 2, cex = 0.65)
  draw_box(14.5, 10.65, 4.5, 0.72, "Raw Alma Analytics .catalog file",
           palette$input, font = 2, cex = 0.88)
  draw_box(10, 9.55, 4.8, 0.88,
           "Pipeline orchestrator\nscripts/run_parsing_pipeline.R\nR/run_parsing_pipeline.R",
           palette$process, font = 2, cex = 0.68)
  connect(5.5, 10.65, 10, 9.55, 0.44, 0.44)
  connect(14.5, 10.65, 10, 9.55, 0.36, 0.44)

  # Shared extraction stage and its helper scripts.
  draw_box(10, 8.35, 8.0, 0.95,
           paste(
             "Extract XML objects + align catalog metadata",
             "R/extract/extract_catalog.R",
             "R/io/read_catalog_file.R",
             "R/io/read_catalog_metadata.R",
             sep = "\n"
           ),
           palette$process, font = 2, cex = 0.64)
  draw_box(10, 7.05, 4.8, 0.78,
           "catalog_extract.rds\ncatalog_extract_summary.csv",
           palette$intermediate, font = 2)
  connect(10, 9.55, 10, 8.35, 0.44, 0.48)
  connect(10, 8.35, 10, 7.05, 0.48, 0.39)

  # Three downstream branches.
  draw_box(2.7, 5.55, 4.5, 1.10,
           paste(
             "Inspection and tag inventory",
             "R/inspect/inspect_catalog_metadata.R",
             "R/extract/extract_xml_tag_inventory.R",
             sep = "\n"
           ),
           palette$diagnostic, font = 2, cex = 0.66)
  draw_box(9.2, 5.55, 5.2, 1.05,
           paste(
             "General column parsing",
             "R/extract/extract_columns.R",
             "inline + referenced + all standalone saved columns",
             sep = "\n"
           ),
           palette$process, font = 2, cex = 0.66)
  draw_box(16.3, 5.55, 5.2, 1.05,
           paste(
             "General filter parsing",
             "R/extract/extract_filters.R",
             "inline + referenced + all standalone saved filters",
             sep = "\n"
           ),
           palette$process, font = 2, cex = 0.66)
  connect(10, 7.05, 2.7, 5.55, 0.39, 0.55)
  connect(10, 7.05, 9.2, 5.55, 0.39, 0.53)
  connect(10, 7.05, 16.3, 5.55, 0.39, 0.53)

  # Inspection outputs.
  draw_box(2.7, 3.95, 4.3, 0.9,
           "Technical outputs\ncatalog_metadata_inventory.csv\nxml_tag_inventory.csv",
           palette$diagnostic)
  connect(2.7, 5.55, 2.7, 3.95, 0.55, 0.45)

  # General column outputs.
  draw_box(7.7, 3.95, 3.5, 0.82,
           "Column inventory\ncolumns.csv",
           palette$diagnostic, font = 2)
  draw_box(11.3, 3.95, 3.5, 0.82,
           "Column rule output\ncolumn_rules.csv",
           palette$final, font = 2)
  connect(9.2, 5.55, 7.7, 3.95, 0.53, 0.41)
  connect(9.2, 5.55, 11.3, 3.95, 0.53, 0.41)

  # General filter outputs.
  draw_box(14.7, 3.95, 3.4, 0.82,
           "Filter inventory\nfilters.csv",
           palette$diagnostic, font = 2)
  draw_box(18.0, 3.95, 3.4, 1.02,
           "Filter rule outputs\nfilter_rules.csv\nfilter_value_lists.csv",
           palette$final, font = 2, cex = 0.70)
  connect(16.3, 5.55, 14.7, 3.95, 0.53, 0.41)
  connect(16.3, 5.55, 18.0, 3.95, 0.53, 0.51)

  # Cross-branch dependency output.
  draw_box(11.8, 2.25, 5.5, 0.82,
           "Report-to-saved-object relationships\nR/export/export_report_dependencies.R -> report_dependencies.csv",
           palette$final, font = 2, cex = 0.68)
  connect(11.3, 3.95, 10.8, 2.25, 0.41, 0.41)
  connect(18.0, 3.95, 12.8, 2.25, 0.51, 0.41)

  # Legend.
  legend_y <- 0.78
  legend_items <- list(
    c("Input", palette$input),
    c("Processing", palette$process),
    c("Intermediate", palette$intermediate),
    c("Review output", palette$final),
    c("Optional / technical", palette$diagnostic)
  )
  legend_x <- c(3.0, 6.5, 10.0, 13.5, 17.0)
  for (i in seq_along(legend_items)) {
    graphics::rect(legend_x[i] - 0.55, legend_y - 0.16,
                   legend_x[i] - 0.20, legend_y + 0.16,
                   col = legend_items[[i]][2], border = palette$border)
    graphics::text(legend_x[i] - 0.05, legend_y, legend_items[[i]][1],
                   adj = 0, cex = 0.72, col = palette$text)
  }

  graphics::text(
    10, 0.28,
    "Run: Rscript scripts/run_parsing_pipeline.R path/to/file.catalog [output_dir]",
    cex = 0.75, col = palette$arrow
  )

  invisible(normalizePath(output_path, mustWork = FALSE))
}

if (!interactive() && sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  output_path <- if (length(args)) args[1L] else {
    "documentation/images/run_parser_pipeline_diagram.png"
  }
  diagram_path <- render_pipeline_diagram(output_path)
  message("Wrote ", diagram_path)
}
