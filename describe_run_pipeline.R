# Create a flow diagram of the Alma Analytics .catalog parsing pipeline.

describe_run_pipeline <- function(
    output_path = "output/run_pipeline_diagram.png",
    width = 14,
    height = 9.5) {
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
  graphics::plot.window(xlim = c(0, 14), ylim = c(0, 10), asp = 1)

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
    7, 9.72,
    "Alma Analytics Catalog Parser Pipeline",
    font = 2, cex = 1.35, col = palette$text
  )
  graphics::text(
    7, 9.38,
    "Input, processing stages, intermediate datasets, and review outputs",
    cex = 0.82, col = palette$arrow
  )

  # Input and shared extraction stage.
  draw_box(7, 8.75, 3.2, 0.6, "Raw Alma Analytics .catalog file",
           palette$input, font = 2, cex = 0.88)
  draw_box(7, 7.72, 4.1, 0.72,
           "Extract XML objects + align catalog metadata\nextract_XMLFileList.R",
           palette$process, font = 2)
  draw_box(7, 6.62, 3.8, 0.72,
           "catalog_extract.rds\ncatalog_extract_summary.csv",
           palette$intermediate, font = 2)
  connect(7, 8.75, 7, 7.72, 0.30, 0.36)
  connect(7, 7.72, 7, 6.62, 0.36, 0.36)

  # Three downstream branches.
  draw_box(2.25, 5.35, 3.3, 0.76,
           "Optional inspection\nmetadata + XML tag inventory",
           palette$diagnostic, font = 2)
  draw_box(7, 5.35, 3.3, 0.76,
           "Saved-column parsing\nclassification and bin rules",
           palette$process, font = 2)
  draw_box(11.65, 5.35, 3.3, 0.76,
           "Filter-object selection\nand criteria parsing",
           palette$process, font = 2)
  connect(7, 6.62, 2.25, 5.35, 0.36, 0.38)
  connect(7, 6.62, 7, 5.35, 0.36, 0.38)
  connect(7, 6.62, 11.65, 5.35, 0.36, 0.38)

  # Optional diagnostic branch.
  draw_box(2.25, 4.05, 3.5, 0.9,
           "Technical outputs\ncatalog_metadata_inventory.csv\nxml_tag_inventory.csv",
           palette$diagnostic)
  connect(2.25, 5.35, 2.25, 4.05, 0.38, 0.45)

  # Saved-column branch.
  draw_box(7, 4.05, 3.55, 0.9,
           "Saved-column review\nsaved_column_review.xlsx\nsaved_column_review.csv",
           palette$final, font = 2)
  connect(7, 5.35, 7, 4.05, 0.38, 0.45)

  # Filter branch has one required intermediate dataset.
  draw_box(11.65, 4.18, 3.25, 0.68,
           "filter_objects.rds",
           palette$intermediate, font = 2)
  draw_box(11.65, 2.92, 3.7, 1.0,
           "Filter review\nfilter_review.xlsx\nfilter_review_value_lists.csv",
           palette$final, font = 2)
  connect(11.65, 5.35, 11.65, 4.18, 0.38, 0.34)
  connect(11.65, 4.18, 11.65, 2.92, 0.34, 0.50)

  # Supporting detail outputs retained by the full pipeline.
  draw_box(7, 2.63, 3.55, 0.82,
           "Detailed technical exports\nsaved_columns.csv\nfilter_criteria.csv",
           palette$diagnostic)
  connect(7, 4.05, 7, 2.63, 0.45, 0.41)
  connect(11.65, 4.18, 8.55, 2.82, 0.34, 0.41)

  # Legend.
  legend_y <- 1.25
  legend_items <- list(
    c("Input", palette$input),
    c("Processing", palette$process),
    c("Intermediate", palette$intermediate),
    c("Review output", palette$final),
    c("Optional / technical", palette$diagnostic)
  )
  legend_x <- c(2.0, 4.45, 7.0, 9.55, 12.2)
  for (i in seq_along(legend_items)) {
    graphics::rect(legend_x[i] - 0.55, legend_y - 0.16,
                   legend_x[i] - 0.20, legend_y + 0.16,
                   col = legend_items[[i]][2], border = palette$border)
    graphics::text(legend_x[i] - 0.05, legend_y, legend_items[[i]][1],
                   adj = 0, cex = 0.72, col = palette$text)
  }

  graphics::text(
    7, 0.65,
    "Run: Rscript scripts/run_pipeline.R path/to/file.catalog [output_dir]",
    cex = 0.75, col = palette$arrow
  )

  invisible(normalizePath(output_path, mustWork = FALSE))
}

if (!interactive() && sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  output_path <- if (length(args)) args[1L] else "output/run_pipeline_diagram.png"
  diagram_path <- describe_run_pipeline(output_path)
  message("Wrote ", diagram_path)
}
