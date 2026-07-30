# Create a flow diagram of the Alma Analytics .catalog parsing pipeline.

render_pipeline_diagram <- function(
    output_path = "docs/images/run_pipeline_diagram.png",
    width = 16,
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
  graphics::plot.window(xlim = c(0, 16), ylim = c(0, 12), asp = 1)

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
    8, 11.72,
    "Alma Analytics Catalog Parser Pipeline",
    font = 2, cex = 1.35, col = palette$text
  )
  graphics::text(
    8, 11.38,
    "Automated parser scripts, inputs, intermediate datasets, and outputs",
    cex = 0.82, col = palette$arrow
  )

  # Entry points and input.
  draw_box(4.4, 10.65, 4.5, 0.88,
           paste(
             "Optional interactive entry point",
             "scripts/choose_catalog_file_and_run_pipeline.R",
             "R/choose_catalog_file.R",
             sep = "\n"
           ),
           palette$process, font = 2, cex = 0.65)
  draw_box(11.6, 10.65, 3.8, 0.72, "Raw Alma Analytics .catalog file",
           palette$input, font = 2, cex = 0.88)
  draw_box(8, 9.55, 4.4, 0.88,
           "Pipeline orchestrator\nscripts/run_pipeline.R\nR/run_pipeline.R",
           palette$process, font = 2, cex = 0.68)
  connect(4.4, 10.65, 8, 9.55, 0.44, 0.44)
  connect(11.6, 10.65, 8, 9.55, 0.36, 0.44)

  # Shared extraction stage and its helper scripts.
  draw_box(8, 8.35, 7.4, 0.95,
           paste(
             "Extract XML objects + align catalog metadata",
             "R/extract/extract_catalog.R",
             "R/io/read_catalog_file.R",
             "R/io/read_catalog_metadata.R",
             sep = "\n"
           ),
           palette$process, font = 2, cex = 0.64)
  draw_box(8, 7.05, 4.5, 0.78,
           "catalog_extract.rds\ncatalog_extract_summary.csv",
           palette$intermediate, font = 2)
  connect(8, 9.55, 8, 8.35, 0.44, 0.48)
  connect(8, 8.35, 8, 7.05, 0.48, 0.39)

  # Three downstream branches.
  draw_box(2.45, 5.65, 4.25, 1.15,
           paste(
             "Inspection and tag inventory",
             "R/inspect/inspect_catalog_metadata.R",
             "R/extract/extract_xml_tag_inventory.R",
             sep = "\n"
           ),
           palette$diagnostic, font = 2, cex = 0.66)
  draw_box(7.15, 5.65, 4.25, 1.15,
           paste(
             "Saved-column parsing and review",
             "R/extract/extract_saved_columns.R",
             "R/export/export_saved_column_review.R",
             sep = "\n"
           ),
           palette$process, font = 2, cex = 0.66)
  draw_box(12.45, 5.78, 5.0, 0.9,
           paste(
             "Filter-object selection",
             "R/extract/extract_filter_objects.R",
             sep = "\n"
           ),
           palette$process, font = 2, cex = 0.69)
  connect(8, 7.05, 2.45, 5.65, 0.39, 0.58)
  connect(8, 7.05, 7.15, 5.65, 0.39, 0.58)
  connect(8, 7.05, 12.45, 5.78, 0.39, 0.45)

  # Inspection outputs.
  draw_box(2.45, 4.05, 4.15, 0.9,
           "Technical outputs\ncatalog_metadata_inventory.csv\nxml_tag_inventory.csv",
           palette$diagnostic)
  connect(2.45, 5.65, 2.45, 4.05, 0.58, 0.45)

  # Saved-column outputs.
  draw_box(7.15, 4.0, 4.25, 1.05,
           paste(
             "Saved-column outputs",
             "saved_columns.csv",
             "saved_column_review.xlsx",
             "saved_column_review.csv",
             sep = "\n"
           ),
           palette$final, font = 2)
  connect(7.15, 5.65, 7.15, 4.0, 0.58, 0.53)

  # Filter branch and its two exporters.
  draw_box(12.45, 4.48, 4.35, 0.78,
           "filter_objects.rds\nfilter_objects_summary.csv",
           palette$intermediate, font = 2)
  connect(12.45, 5.78, 12.45, 4.48, 0.45, 0.39)
  draw_box(10.35, 3.15, 4.0, 0.88,
           "Criteria export\nR/export/export_filter_criteria.R",
           palette$process, font = 2, cex = 0.67)
  draw_box(14.05, 3.15, 3.65, 0.88,
           "Review export\nR/export/export_filter_review.R",
           palette$process, font = 2, cex = 0.67)
  connect(12.45, 4.48, 10.35, 3.15, 0.39, 0.44)
  connect(12.45, 4.48, 14.05, 3.15, 0.39, 0.44)
  draw_box(10.35, 1.88, 3.65, 0.72,
           "filter_criteria.csv",
           palette$diagnostic, font = 2)
  draw_box(14.05, 1.78, 3.65, 1.0,
           paste(
             "Filter review outputs",
             "filter_review.xlsx",
             "filter_review.csv",
             "filter_review_value_lists.csv",
             sep = "\n"
           ),
           palette$final, font = 2, cex = 0.72)
  connect(10.35, 3.15, 10.35, 1.88, 0.44, 0.36)
  connect(14.05, 3.15, 14.05, 1.78, 0.44, 0.50)

  # Legend.
  legend_y <- 0.78
  legend_items <- list(
    c("Input", palette$input),
    c("Processing", palette$process),
    c("Intermediate", palette$intermediate),
    c("Review output", palette$final),
    c("Optional / technical", palette$diagnostic)
  )
  legend_x <- c(2.2, 5.0, 8.0, 10.9, 13.7)
  for (i in seq_along(legend_items)) {
    graphics::rect(legend_x[i] - 0.55, legend_y - 0.16,
                   legend_x[i] - 0.20, legend_y + 0.16,
                   col = legend_items[[i]][2], border = palette$border)
    graphics::text(legend_x[i] - 0.05, legend_y, legend_items[[i]][1],
                   adj = 0, cex = 0.72, col = palette$text)
  }

  graphics::text(
    8, 0.28,
    "Run: Rscript scripts/run_pipeline.R path/to/file.catalog [output_dir]",
    cex = 0.75, col = palette$arrow
  )

  invisible(normalizePath(output_path, mustWork = FALSE))
}

if (!interactive() && sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  output_path <- if (length(args)) args[1L] else "docs/images/run_pipeline_diagram.png"
  diagram_path <- render_pipeline_diagram(output_path)
  message("Wrote ", diagram_path)
}
