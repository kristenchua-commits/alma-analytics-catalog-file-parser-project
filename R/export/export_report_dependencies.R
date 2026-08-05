# Export simplified report-to-saved-column/filter dependency relationships.
export_report_dependencies <- function(
    input_path = "output/catalog_extract.rds",
    report_columns,
    report_filters,
    output_path = "output/report_dependencies.csv") {
  if (!file.exists(input_path)) stop("Missing input file: ", input_path)
  catalog <- readRDS(input_path)
  required <- c("catalog_index", "object_kind", "object_title", "original_path")
  missing <- setdiff(required, names(catalog))
  if (length(missing)) stop("Input is missing columns: ", paste(missing, collapse = ", "))

  columns <- if (is.character(report_columns) && length(report_columns) == 1L) {
    utils::read.csv(report_columns, stringsAsFactors = FALSE, check.names = FALSE)
  } else report_columns
  filters <- if (is.character(report_filters) && length(report_filters) == 1L) {
    utils::read.csv(report_filters, stringsAsFactors = FALSE, check.names = FALSE)
  } else report_filters

  column_dependencies <- columns[
    columns$column_source == "saved_reference" &
      !is.na(columns$saved_column_path) & nzchar(columns$saved_column_path),
    c("report_catalog_index", "report_title", "report_path", "column_index",
      "display_name", "saved_column_path"),
    drop = FALSE
  ]
  if (nrow(column_dependencies)) {
    column_dependencies <- data.frame(
      report_catalog_index = column_dependencies$report_catalog_index,
      report_title = column_dependencies$report_title,
      report_path = column_dependencies$report_path,
      dependency_type = "saved_column",
      source_index = column_dependencies$column_index,
      dependency_name = column_dependencies$display_name,
      dependency_path = column_dependencies$saved_column_path,
      stringsAsFactors = FALSE
    )
  }

  filter_dependencies <- filters[
    filters$filter_source == "saved_reference" &
      !is.na(filters$saved_filter_path) & nzchar(filters$saved_filter_path),
    c("report_catalog_index", "report_title", "report_path", "filter_index",
      "saved_filter_name", "saved_filter_path"),
    drop = FALSE
  ]
  if (nrow(filter_dependencies)) {
    filter_dependencies <- data.frame(
      report_catalog_index = filter_dependencies$report_catalog_index,
      report_title = filter_dependencies$report_title,
      report_path = filter_dependencies$report_path,
      dependency_type = "saved_filter",
      source_index = filter_dependencies$filter_index,
      dependency_name = filter_dependencies$saved_filter_name,
      dependency_path = filter_dependencies$saved_filter_path,
      stringsAsFactors = FALSE
    )
  }

  dependency_parts <- Filter(nrow, list(column_dependencies, filter_dependencies))
  if (!length(dependency_parts)) {
    dependencies <- data.frame(
      dependency_index = integer(),
      report_catalog_index = integer(),
      report_title = character(),
      report_path = character(),
      dependency_type = character(),
      dependency_name = character(),
      dependency_path = character(),
      resolved_catalog_index = integer(),
      resolved_object_title = character(),
      resolved_object_kind = character(),
      is_resolved = logical(),
      stringsAsFactors = FALSE
    )
    dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
    write.csv(dependencies, output_path, row.names = FALSE, na = "")
    message("Wrote ", output_path, " (0 dependencies)")
    return(invisible(dependencies))
  }
  dependencies <- do.call(rbind, dependency_parts)
  dependencies <- unique(dependencies[c(
    "report_catalog_index", "report_title", "report_path", "dependency_type",
    "dependency_name", "dependency_path"
  )])
  target_index <- match(dependencies$dependency_path, catalog$original_path)
  dependencies$resolved_catalog_index <- catalog$catalog_index[target_index]
  dependencies$resolved_object_title <- catalog$object_title[target_index]
  dependencies$resolved_object_kind <- catalog$object_kind[target_index]
  dependencies$is_resolved <- !is.na(target_index)
  dependencies$dependency_index <- seq_len(nrow(dependencies))
  dependencies <- dependencies[c(
    "dependency_index", "report_catalog_index", "report_title", "report_path",
    "dependency_type", "dependency_name", "dependency_path",
    "resolved_catalog_index", "resolved_object_title", "resolved_object_kind",
    "is_resolved"
  )]
  rownames(dependencies) <- NULL
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  write.csv(dependencies, output_path, row.names = FALSE, na = "")
  message("Wrote ", output_path, " (", nrow(dependencies), " dependencies)")
  invisible(dependencies)
}
