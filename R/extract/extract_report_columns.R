# Extract every selected column embedded in report criteria.
extract_report_columns <- function(
    input_path = "output/catalog_extract.rds",
    output_path = "output/report_saved_and_non_saved_columns.csv") {
  if (!requireNamespace("xml2", quietly = TRUE)) stop("Package 'xml2' is required.")
  if (!file.exists(input_path)) stop("Missing input file: ", input_path)
  catalog <- readRDS(input_path)
  required <- c(
    "catalog_index", "object_kind", "object_title", "original_path", "xml_text"
  )
  missing <- setdiff(required, names(catalog))
  if (length(missing)) stop("Input is missing columns: ", paste(missing, collapse = ", "))
  reports <- catalog[catalog$object_kind == "report", , drop = FALSE]
  if (!nrow(reports)) stop("No report objects were found.")

  rows <- list()
  for (i in seq_len(nrow(reports))) {
    doc <- tryCatch(xml2::read_xml(reports$xml_text[[i]]), error = function(e) NULL)
    if (is.null(doc)) next
    criteria <- xml2::xml_find_first(doc, ".//*[local-name()='criteria']")
    columns <- xml2::xml_find_all(
      criteria,
      "./*[local-name()='columns']/*[local-name()='column']"
    )
    if (!length(columns)) next
    criteria_subject_area <- report_xml_safe_attr(criteria, "subjectArea")
    if (!is.na(criteria_subject_area)) {
      criteria_subject_area <- sub('^"', "", sub('"$', "", criteria_subject_area))
    }

    rows[[length(rows) + 1L]] <- do.call(rbind, lapply(seq_along(columns), function(j) {
      column <- columns[[j]]
      xml_type <- report_xml_local_type(column)
      saved_path <- report_xml_safe_attr(column, "path")
      source <- if (!is.na(saved_path) || identical(xml_type, "savedRegularColumnRef")) {
        "saved_reference"
      } else if (identical(xml_type, "regularColumn")) {
        "inline"
      } else "unknown"
      formula_node <- xml2::xml_find_first(
        column,
        "./*[local-name()='columnFormula']/*[local-name()='expr']"
      )
      formula_type <- report_xml_local_type(formula_node)
      formula <- if (!is.na(formula_type) && formula_type == "binned") {
        report_xml_first_text(
          formula_node,
          ".//*[local-name()='baseFormula']/*[local-name()='expr']"
        )
      } else report_xml_clean_text(formula_node)
      bin_rule_count <- length(xml2::xml_find_all(
        formula_node,
        ".//*[local-name()='rules']/*[local-name()='when']"
      ))
      table_heading <- report_xml_first_text(
        column,
        "./*[local-name()='tableHeading']//*[local-name()='caption']/*[local-name()='text']"
      )
      column_heading <- report_xml_first_text(
        column,
        "./*[local-name()='columnHeading']//*[local-name()='caption']/*[local-name()='text']"
      )
      display_name <- if (!is.na(column_heading)) column_heading else if (!is.na(formula)) {
        formula
      } else if (!is.na(saved_path)) basename(saved_path) else NA_character_

      data.frame(
        report_catalog_index = reports$catalog_index[[i]],
        report_title = reports$object_title[[i]],
        report_subject_area = criteria_subject_area,
        report_path = reports$original_path[[i]],
        column_index = j,
        column_source = source,
        column_xml_type = xml_type,
        column_id = report_xml_safe_attr(column, "columnID"),
        display_name = display_name,
        table_heading = table_heading,
        column_heading = column_heading,
        formula_type = formula_type,
        formula = formula,
        bin_rule_count = bin_rule_count,
        saved_column_path = saved_path,
        stringsAsFactors = FALSE
      )
    }))
  }
  report_columns <- if (length(rows)) do.call(rbind, rows) else data.frame(
    report_catalog_index = integer(),
    report_title = character(),
    report_subject_area = character(),
    report_path = character(),
    column_index = integer(),
    column_source = character(),
    column_xml_type = character(),
    column_id = character(),
    display_name = character(),
    table_heading = character(),
    column_heading = character(),
    formula_type = character(),
    formula = character(),
    bin_rule_count = integer(),
    saved_column_path = character(),
    stringsAsFactors = FALSE
  )
  rownames(report_columns) <- NULL
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  write.csv(report_columns, output_path, row.names = FALSE, na = "")
  message("Wrote ", output_path, " (", nrow(report_columns),
          " saved and non-saved report-column rows)")
  invisible(report_columns)
}
