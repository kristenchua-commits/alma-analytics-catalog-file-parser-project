# Extract logical leaf terms from filters embedded in report criteria.
extract_report_filters <- function(
    input_path = "output/catalog_extract.rds",
    output_path = "output/report_saved_and_non_saved_filters.csv",
    value_preview_limit = 10L) {
  if (!requireNamespace("xml2", quietly = TRUE)) stop("Package 'xml2' is required.")
  if (!file.exists(input_path)) stop("Missing input file: ", input_path)
  value_preview_limit <- as.integer(value_preview_limit)
  if (is.na(value_preview_limit) || value_preview_limit < 1L) {
    stop("value_preview_limit must be a positive integer")
  }
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
    root <- xml2::xml_find_first(
      criteria,
      "./*[local-name()='filter']/*[local-name()='expr']"
    )
    if (inherits(root, "xml_missing")) next
    criteria_subject_area <- report_xml_safe_attr(criteria, "subjectArea")
    if (!is.na(criteria_subject_area)) {
      criteria_subject_area <- sub('^"', "", sub('"$', "", criteria_subject_area))
    }
    filter_index <- 0L

    visit <- function(node, join_operator = NA_character_, group_path = "1",
                      nesting_level = 1L) {
      expression_type <- report_xml_local_type(node)
      if (!is.na(expression_type) && expression_type == "logical") {
        operator <- report_xml_safe_attr(node, "op")
        children <- xml2::xml_find_all(node, "./*[local-name()='expr']")
        for (child_index in seq_along(children)) {
          visit(
            children[[child_index]],
            join_operator = operator,
            group_path = paste0(group_path, ".", child_index),
            nesting_level = nesting_level + 1L
          )
        }
        return(invisible(NULL))
      }

      filter_index <<- filter_index + 1L
      details <- report_filter_term_details(node, value_preview_limit)
      rows[[length(rows) + 1L]] <<- data.frame(
        report_catalog_index = reports$catalog_index[[i]],
        report_title = reports$object_title[[i]],
        report_subject_area = criteria_subject_area,
        report_path = reports$original_path[[i]],
        filter_index = filter_index,
        group_path = group_path,
        nesting_level = nesting_level,
        join_operator = join_operator,
        filter_source = details$source,
        expression_type = details$expression_type,
        operator = details$operator,
        field = details$field,
        filter_text = details$filter_text,
        saved_filter_name = details$saved_filter_name,
        saved_filter_path = details$saved_filter_path,
        value_count = details$value_count,
        value_preview = details$value_preview,
        stringsAsFactors = FALSE
      )
      invisible(NULL)
    }
    visit(root)
  }
  report_filters <- if (length(rows)) do.call(rbind, rows) else data.frame(
    report_catalog_index = integer(),
    report_title = character(),
    report_subject_area = character(),
    report_path = character(),
    filter_index = integer(),
    group_path = character(),
    nesting_level = integer(),
    join_operator = character(),
    filter_source = character(),
    expression_type = character(),
    operator = character(),
    field = character(),
    filter_text = character(),
    saved_filter_name = character(),
    saved_filter_path = character(),
    value_count = integer(),
    value_preview = character(),
    stringsAsFactors = FALSE
  )
  rownames(report_filters) <- NULL
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  write.csv(report_filters, output_path, row.names = FALSE, na = "")
  message("Wrote ", output_path, " (", nrow(report_filters),
          " saved and non-saved report-filter rows)")
  invisible(report_filters)
}
