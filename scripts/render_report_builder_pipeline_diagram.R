# Create a flow diagram of the reviewed campus report-building workflow.

render_report_builder_pipeline_diagram <- function(
    output_path = "documentation/images/run_report_builder_pipeline_diagram.png",
    width = 16,
    height = 10) {
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  extension <- tolower(tools::file_ext(output_path))
  if (extension == "png") {
    grDevices::png(
      output_path,
      width = width,
      height = height,
      units = "in",
      res = 160,
      bg = "white"
    )
  } else if (extension == "pdf") {
    grDevices::pdf(
      output_path,
      width = width,
      height = height,
      useDingbats = FALSE
    )
  } else {
    stop("Unsupported output format. Use a .png or .pdf file extension.")
  }
  on.exit(grDevices::dev.off(), add = TRUE)

  graphics::par(
    mar = c(0.25, 0.25, 0.7, 0.25),
    xpd = NA,
    family = "sans",
    bg = "white"
  )
  graphics::plot.new()
  graphics::plot.window(xlim = c(0, 16), ylim = c(0, 10), asp = 1)

  palette <- list(
    parser = "#DCEBFA",
    process = "#DDF3E4",
    review = "#FFF1CC",
    final = "#E7DDF5",
    reference = "#E8ECF1",
    border = "#425466",
    text = "#172B4D",
    arrow = "#6B7C93"
  )

  draw_box <- function(x, y, box_width, box_height, label, fill,
                       border = palette$border, font = 1, cex = 0.72) {
    graphics::rect(
      x - box_width / 2,
      y - box_height / 2,
      x + box_width / 2,
      y + box_height / 2,
      col = fill,
      border = border,
      lwd = 1.3
    )
    label_lines <- strsplit(label, "\n", fixed = TRUE)[[1L]]
    lines <- unlist(lapply(
      label_lines,
      strwrap,
      width = max(18L, floor(box_width * 12))
    ))
    line_height <- 0.20
    start_y <- y + (length(lines) - 1L) * line_height / 2
    for (i in seq_along(lines)) {
      graphics::text(
        x,
        start_y - (i - 1L) * line_height,
        lines[[i]],
        col = palette$text,
        font = font,
        cex = cex
      )
    }
  }

  connect_orthogonal <- function(points, lty = 1) {
    stopifnot(is.matrix(points), ncol(points) == 2L, nrow(points) >= 2L)
    if (nrow(points) > 2L) {
      for (i in seq_len(nrow(points) - 2L)) {
        graphics::segments(
          points[i, 1L],
          points[i, 2L],
          points[i + 1L, 1L],
          points[i + 1L, 2L],
          lwd = 1.4,
          lty = lty,
          col = palette$arrow
        )
      }
    }
    final_start <- nrow(points) - 1L
    graphics::arrows(
      points[final_start, 1L],
      points[final_start, 2L],
      points[final_start + 1L, 1L],
      points[final_start + 1L, 2L],
      length = 0.08,
      angle = 22,
      lwd = 1.4,
      lty = lty,
      col = palette$arrow
    )
  }

  graphics::text(
    8,
    9.72,
    "Alma Analytics Report Builder Pipeline",
    font = 2,
    cex = 1.35,
    col = palette$text
  )
  graphics::text(
    8,
    9.40,
    "Parser review outputs, manual normalization, validation, and campus workbook publication",
    cex = 0.80,
    col = palette$arrow
  )

  # Parser-side producers and their review outputs.
  draw_box(
    3.1, 8.50, 5.3, 0.95,
    paste(
      "Saved-column review exporter",
      "R/export/export_saved_column_review.R",
      sep = "\n"
    ),
    palette$process,
    font = 2,
    cex = 0.69
  )
  draw_box(
    8.8, 8.50, 5.0, 0.95,
    paste(
      "Filter review exporter",
      "R/export/export_filter_review.R",
      sep = "\n"
    ),
    palette$process,
    font = 2,
    cex = 0.69
  )
  draw_box(
    13.65, 8.50, 3.6, 0.82,
    "Source .catalog file\ncreation date for as-of suffix",
    palette$parser,
    font = 2,
    cex = 0.68
  )

  draw_box(
    3.1, 7.20, 4.6, 0.82,
    "saved_column_review.csv\ncanonical SQL-formatted criteria",
    palette$parser,
    font = 2,
    cex = 0.70
  )
  draw_box(
    8.8, 7.20, 4.6, 0.82,
    "filter_review.csv\nfilter_review_value_lists.csv",
    palette$parser,
    font = 2,
    cex = 0.70
  )
  connect_orthogonal(rbind(c(3.1, 8.02), c(3.1, 7.61)))
  connect_orthogonal(rbind(c(8.8, 8.02), c(8.8, 7.61)))

  # Human review checkpoint and normalized publication source.
  draw_box(
    5.7, 5.78, 6.4, 1.10,
    paste(
      "Manual combination and review",
      "assign domain, campus/global scope, labels, explanations, and campus sheets",
      "No script currently automates this checkpoint",
      sep = "\n"
    ),
    palette$review,
    font = 2,
    cex = 0.66
  )
  connect_orthogonal(rbind(
    c(3.1, 6.79),
    c(3.1, 6.55),
    c(4.4, 6.55),
    c(4.4, 6.33)
  ))
  connect_orthogonal(rbind(
    c(8.8, 6.79),
    c(8.8, 6.55),
    c(7.0, 6.55),
    c(7.0, 6.33)
  ))

  draw_box(
    5.7, 4.30, 6.2, 0.82,
    "output/normalized_combined_saved_column_and_filter_review.xlsx\nreviewed worksheets named UC*",
    palette$review,
    font = 2,
    cex = 0.68
  )
  connect_orthogonal(rbind(c(5.7, 5.23), c(5.7, 4.71)))

  # Optional notebook controller and an explicitly excluded audit branch.
  draw_box(
    12.25, 5.82, 5.7, 1.04,
    paste(
      "Optional validation and execution notebook",
      "docs/notebooks/",
      "campus_saved_column_filter_documentation_construction.ipynb",
      sep = "\n"
    ),
    palette$process,
    font = 2,
    cex = 0.62
  )
  draw_box(
    12.65, 4.25, 5.2, 0.90,
    paste(
      "Audit-only branch — not used by report builder",
      "R/export/export_filter_criteria.R -> filter_criteria.csv",
      sep = "\n"
    ),
    palette$reference,
    font = 2,
    cex = 0.62
  )

  # Publication script consumes the normalized workbook, canonical saved-column
  # review, source archive date, and optional notebook instruction.
  draw_box(
    8.0, 2.92, 9.3, 1.22,
    paste(
      "Campus documentation exporter",
      "scripts/export_documentation/export_exclusions_documentation.R",
      "validate UC sheets -> refresh saved-column criteria -> build hierarchy -> write workbooks",
      sep = "\n"
    ),
    palette$process,
    font = 2,
    cex = 0.67
  )
  connect_orthogonal(rbind(c(5.7, 3.89), c(5.7, 3.53)))
  connect_orthogonal(rbind(
    c(0.80, 7.20),
    c(0.35, 7.20),
    c(0.35, 2.92),
    c(3.35, 2.92)
  ))
  connect_orthogonal(rbind(
    c(15.45, 8.50),
    c(15.65, 8.50),
    c(15.65, 2.92),
    c(12.65, 2.92)
  ))
  connect_orthogonal(rbind(c(9.4, 5.30), c(9.4, 3.53)), lty = 2)

  draw_box(
    8.0, 1.35, 11.8, 0.98,
    paste(
      "Campus exclusions-documentation workbooks",
      "one dated .xlsx file per UC* worksheet",
      "current project snapshot: annual statistics NZ-level report/2025 - 2026/exclusions tab/",
      sep = "\n"
    ),
    palette$final,
    font = 2,
    cex = 0.69
  )
  connect_orthogonal(rbind(c(8.0, 2.31), c(8.0, 1.84)))

  # Legend and execution hint.
  legend_y <- 0.48
  legend_items <- list(
    c("Parser/source input", palette$parser),
    c("Script or notebook", palette$process),
    c("Human review", palette$review),
    c("Published output", palette$final),
    c("Not in report path", palette$reference)
  )
  legend_x <- c(1.65, 4.8, 7.75, 10.65, 13.55)
  for (i in seq_along(legend_items)) {
    graphics::rect(
      legend_x[[i]] - 0.52,
      legend_y - 0.14,
      legend_x[[i]] - 0.20,
      legend_y + 0.14,
      col = legend_items[[i]][[2L]],
      border = palette$border
    )
    graphics::text(
      legend_x[[i]] - 0.05,
      legend_y,
      legend_items[[i]][[1L]],
      adj = 0,
      cex = 0.65,
      col = palette$text
    )
  }

  invisible(normalizePath(output_path, mustWork = FALSE))
}

if (!interactive() && sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  output_path <- if (length(args)) {
    args[[1L]]
  } else {
    "documentation/images/run_report_builder_pipeline_diagram.png"
  }
  diagram_path <- render_report_builder_pipeline_diagram(output_path)
  message("Wrote ", diagram_path)
}
