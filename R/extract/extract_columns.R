# Extract all column usages and definitions, plus normalized column rules.

column_catalog_path <- function(path) {
  if (is.na(path)) return(NA_character_)
  gsub("\\/", "/", path, fixed = TRUE)
}

column_sql_literal <- function(node) {
  value <- report_xml_clean_text(node)
  if (is.na(value)) return(NA_character_)
  value_type <- report_xml_local_type(node)
  if (!is.na(value_type) && grepl(
    "decimal|double|float|integer|long|number|short",
    value_type,
    ignore.case = TRUE
  )) return(value)
  if (!is.na(value_type) && grepl("boolean", value_type, ignore.case = TRUE)) {
    return(toupper(value))
  }
  paste0("'", gsub("'", "''", value, fixed = TRUE), "'")
}

format_column_condition <- function(condition_node) {
  expression <- xml2::xml_find_first(condition_node, "./*[local-name()='expr']")
  if (inherits(expression, "xml_missing")) return(NA_character_)
  expression_type <- report_xml_local_type(expression)
  children <- xml2::xml_find_all(expression, "./*[local-name()='expr']")
  if (!length(children)) return(report_xml_clean_text(expression))

  operator <- report_xml_safe_attr(expression, "op")
  field <- report_xml_clean_text(children[[1L]])
  if (is.na(field) || is.na(operator)) return(report_xml_clean_text(expression))

  if (!is.na(expression_type) && expression_type == "comparison") {
    if (operator == "null") return(paste(field, "IS NULL"))
    if (operator == "notNull") return(paste(field, "IS NOT NULL"))
    mapped <- c(
      equal = "=", notEqual = "<>", greater = ">", greaterThan = ">",
      greaterOrEqual = ">=", greaterThanOrEqual = ">=", less = "<",
      lessThan = "<", lessOrEqual = "<=", lessThanOrEqual = "<="
    )
    sql_operator <- unname(mapped[operator])
    if (!length(sql_operator) || is.na(sql_operator) || length(children) < 2L) {
      return(report_xml_clean_text(expression))
    }
    values <- vapply(children[-1L], column_sql_literal, character(1L))
    return(paste(field, sql_operator, paste(values, collapse = ", ")))
  }

  if (!is.na(expression_type) && expression_type == "list" && length(children) >= 2L) {
    values <- vapply(children[-1L], column_sql_literal, character(1L))
    if (operator %in% c("in", "notIn")) {
      keyword <- if (operator == "notIn") "NOT IN" else "IN"
      return(paste0(field, " ", keyword, " (", paste(values, collapse = ", "), ")"))
    }
    raw_values <- vapply(children[-1L], report_xml_clean_text, character(1L))
    wildcard_values <- switch(
      operator,
      beginsWith = paste0(raw_values, "%"),
      endsWith = paste0("%", raw_values),
      contains = paste0("%", raw_values, "%"),
      NULL
    )
    if (!is.null(wildcard_values)) {
      clauses <- paste0(
        field, " LIKE '", gsub("'", "''", wildcard_values, fixed = TRUE), "'"
      )
      return(if (length(clauses) == 1L) clauses else {
        paste0("(", paste(clauses, collapse = " OR "), ")")
      })
    }
  }

  report_xml_clean_text(expression)
}

column_definition_metadata <- function(catalog_row) {
  doc <- tryCatch(xml2::read_xml(catalog_row$xml_text[[1L]]), error = function(e) NULL)
  if (is.null(doc)) return(NULL)
  column <- xml2::xml_find_first(doc, ".//*[local-name()='column']")
  if (inherits(column, "xml_missing")) column <- xml2::xml_root(doc)
  formula_node <- xml2::xml_find_first(
    column, "./*[local-name()='columnFormula']/*[local-name()='expr']"
  )
  formula_type <- report_xml_local_type(formula_node)
  formula <- if (!is.na(formula_type) && formula_type == "binned") {
    report_xml_first_text(
      formula_node, ".//*[local-name()='baseFormula']/*[local-name()='expr']"
    )
  } else report_xml_clean_text(formula_node)
  list(
    catalog_index = catalog_row$catalog_index[[1L]],
    object_name = catalog_row$object_title[[1L]],
    subject_area = catalog_row$subject_area[[1L]],
    path = catalog_row$original_path[[1L]],
    column = column,
    xml_type = report_xml_local_type(column),
    column_id = report_xml_safe_attr(column, "columnID"),
    table_heading = report_xml_first_text(
      column,
      "./*[local-name()='tableHeading']//*[local-name()='caption']/*[local-name()='text']"
    ),
    column_heading = report_xml_first_text(
      column,
      "./*[local-name()='columnHeading']//*[local-name()='caption']/*[local-name()='text']"
    ),
    formula_type = formula_type,
    formula = formula,
    bin_rule_count = length(xml2::xml_find_all(
      formula_node, ".//*[local-name()='rules']/*[local-name()='when']"
    ))
  )
}

column_rule_rows <- function(metadata, record_scope, report = NULL, column_index = NA_integer_) {
  rules <- xml2::xml_find_all(
    metadata$column,
    ".//*[local-name()='rules']/*[local-name()='when']"
  )
  otherwise <- xml2::xml_find_first(
    metadata$column,
    ".//*[local-name()='rules']/*[local-name()='otherwise']"
  )
  report_index <- if (is.null(report)) NA_integer_ else report$catalog_index[[1L]]
  report_title <- if (is.null(report)) NA_character_ else report$object_title[[1L]]
  report_path <- if (is.null(report)) NA_character_ else report$original_path[[1L]]
  id_prefix <- if (record_scope == "standalone_saved_object") {
    paste0("SC", metadata$saved_object_index)
  } else paste0("RC", report_index, "-C", column_index)
  definition_catalog_index <- if (record_scope == "standalone_saved_object") {
    metadata$catalog_index
  } else NA_integer_
  definition_name <- if (record_scope == "standalone_saved_object") {
    metadata$object_name
  } else NA_character_
  definition_path <- if (record_scope == "standalone_saved_object") {
    metadata$path
  } else NA_character_

  make_row <- function(rule_id, rule_index, rule_kind, criterion, label) {
    data.frame(
      rule_id = rule_id,
      rule_type = "Column",
      source_type = if (record_scope == "standalone_saved_object") {
        "standalone_saved_object"
      } else "inline",
      record_scope = record_scope,
      rule_name = if (!is.na(definition_name)) {
        definition_name
      } else if (!is.na(metadata$column_heading)) metadata$column_heading else metadata$formula,
      rule_index = rule_index,
      rule_kind = rule_kind,
      criterion_text = criterion,
      review_label = label,
      explanation = "",
      criterion_text_category = metadata$formula,
      join_operator = NA_character_,
      value_count = NA_integer_,
      report_catalog_index = report_index,
      report_title = report_title,
      report_path = report_path,
      report_column_index = column_index,
      definition_catalog_index = definition_catalog_index,
      definition_name = definition_name,
      definition_path = definition_path,
      stringsAsFactors = FALSE
    )
  }

  rows <- list()
  if (length(rules)) {
    for (j in seq_along(rules)) {
      rows[[length(rows) + 1L]] <- make_row(
        paste0(id_prefix, "-R", j), j, "when",
        format_column_condition(
          xml2::xml_find_first(rules[[j]], ".//*[local-name()='condition']")
        ),
        report_xml_first_text(rules[[j]], ".//*[local-name()='value']/*[local-name()='expr']")
      )
    }
    if (!inherits(otherwise, "xml_missing")) {
      rows[[length(rows) + 1L]] <- make_row(
        paste0(id_prefix, "-OTHER"), length(rules) + 1L, "otherwise",
        "All other values",
        report_xml_first_text(otherwise, ".//*[local-name()='value']/*[local-name()='expr']")
      )
    }
  } else {
    rows[[1L]] <- make_row(
      paste0(id_prefix, "-FORMULA"), 1L, "formula", metadata$formula,
      if (!is.na(metadata$column_heading)) metadata$column_heading else "Calculated value"
    )
  }
  do.call(rbind, rows)
}

extract_columns <- function(
    input_path = "output/catalog_extract.rds",
    columns_output = "output/columns.csv",
    rules_output = "output/column_rules.csv") {
  if (!requireNamespace("xml2", quietly = TRUE)) stop("Package 'xml2' is required.")
  if (!file.exists(input_path)) stop("Missing input file: ", input_path)
  catalog <- readRDS(input_path)
  required <- c(
    "catalog_index", "object_kind", "object_title", "subject_area",
    "original_path", "xml_text"
  )
  missing <- setdiff(required, names(catalog))
  if (length(missing)) stop("Input is missing columns: ", paste(missing, collapse = ", "))

  saved_objects <- catalog[catalog$object_kind == "saved_column", , drop = FALSE]
  saved_metadata <- lapply(seq_len(nrow(saved_objects)), function(i) {
    metadata <- column_definition_metadata(saved_objects[i, , drop = FALSE])
    if (!is.null(metadata)) metadata$saved_object_index <- i
    metadata
  })
  valid_saved <- !vapply(saved_metadata, is.null, logical(1L))
  saved_metadata <- saved_metadata[valid_saved]
  saved_paths <- if (length(saved_metadata)) {
    vapply(saved_metadata, `[[`, character(1L), "path")
  } else character()
  names(saved_metadata) <- saved_paths

  reports <- catalog[catalog$object_kind == "report", , drop = FALSE]
  usage_rows <- list()
  rule_rows <- list()
  referenced_paths <- character()
  for (i in seq_len(nrow(reports))) {
    doc <- tryCatch(xml2::read_xml(reports$xml_text[[i]]), error = function(e) NULL)
    if (is.null(doc)) next
    criteria <- xml2::xml_find_first(doc, ".//*[local-name()='criteria']")
    columns <- xml2::xml_find_all(
      criteria, "./*[local-name()='columns']/*[local-name()='column']"
    )
    if (!length(columns)) next
    subject_area <- report_xml_safe_attr(criteria, "subjectArea")
    if (!is.na(subject_area)) subject_area <- sub('^"', "", sub('"$', "", subject_area))

    for (j in seq_along(columns)) {
      column <- columns[[j]]
      xml_type <- report_xml_local_type(column)
      saved_path <- column_catalog_path(report_xml_safe_attr(column, "path"))
      source <- if (!is.na(saved_path) || identical(xml_type, "savedRegularColumnRef")) {
        "saved_reference"
      } else "inline"
      if (!is.na(saved_path)) referenced_paths <- c(referenced_paths, saved_path)
      resolved <- if (!is.na(saved_path)) saved_metadata[[saved_path]] else NULL

      local_formula_node <- xml2::xml_find_first(
        column, "./*[local-name()='columnFormula']/*[local-name()='expr']"
      )
      local_formula_type <- report_xml_local_type(local_formula_node)
      local_formula <- if (!is.na(local_formula_type) && local_formula_type == "binned") {
        report_xml_first_text(
          local_formula_node, ".//*[local-name()='baseFormula']/*[local-name()='expr']"
        )
      } else report_xml_clean_text(local_formula_node)
      local_table_heading <- report_xml_first_text(
        column,
        "./*[local-name()='tableHeading']//*[local-name()='caption']/*[local-name()='text']"
      )
      local_column_heading <- report_xml_first_text(
        column,
        "./*[local-name()='columnHeading']//*[local-name()='caption']/*[local-name()='text']"
      )
      report_display_name <- if (!is.na(local_column_heading)) {
        local_column_heading
      } else if (!is.na(local_formula)) local_formula else if (!is.na(saved_path)) {
        basename(saved_path)
      } else NA_character_
      column_name <- if (!is.null(resolved) && !is.na(resolved$column_heading)) {
        resolved$column_heading
      } else if (!is.null(resolved)) resolved$object_name else report_display_name
      formula_type <- if (!is.null(resolved)) resolved$formula_type else local_formula_type
      formula <- if (!is.null(resolved)) resolved$formula else local_formula
      bin_rule_count <- if (!is.null(resolved)) resolved$bin_rule_count else {
        length(xml2::xml_find_all(
          local_formula_node, ".//*[local-name()='rules']/*[local-name()='when']"
        ))
      }

      usage_rows[[length(usage_rows) + 1L]] <- data.frame(
        record_scope = "report_column",
        report_catalog_index = reports$catalog_index[[i]],
        report_title = reports$object_title[[i]],
        report_subject_area = subject_area,
        report_path = reports$original_path[[i]],
        column_index = j,
        column_source = source,
        column_xml_type = xml_type,
        column_id = report_xml_safe_attr(column, "columnID"),
        column_name = column_name,
        report_display_name = report_display_name,
        table_heading = local_table_heading,
        column_heading = local_column_heading,
        formula_type = formula_type,
        formula = formula,
        bin_rule_count = bin_rule_count,
        definition_catalog_index = if (is.null(resolved)) NA_integer_ else resolved$catalog_index,
        definition_name = if (is.null(resolved)) NA_character_ else resolved$object_name,
        definition_table_heading = if (is.null(resolved)) NA_character_ else resolved$table_heading,
        definition_column_heading = if (is.null(resolved)) NA_character_ else resolved$column_heading,
        definition_path = saved_path,
        is_resolved = if (source == "saved_reference") !is.null(resolved) else NA,
        is_referenced = NA,
        stringsAsFactors = FALSE
      )

      if (source == "inline") {
        inline_metadata <- list(
          catalog_index = NA_integer_, object_name = NA_character_,
          subject_area = subject_area, path = NA_character_, column = column,
          column_heading = local_column_heading, formula = local_formula
        )
        rule_rows[[length(rule_rows) + 1L]] <- column_rule_rows(
          inline_metadata, "report_inline", reports[i, , drop = FALSE], j
        )
      }
    }
  }

  standalone_rows <- lapply(saved_metadata, function(metadata) data.frame(
    record_scope = "standalone_saved_object",
    report_catalog_index = NA_integer_,
    report_title = NA_character_,
    report_subject_area = NA_character_,
    report_path = NA_character_,
    column_index = NA_integer_,
    column_source = "standalone_saved_object",
    column_xml_type = metadata$xml_type,
    column_id = metadata$column_id,
    column_name = if (!is.na(metadata$column_heading)) {
      metadata$column_heading
    } else metadata$object_name,
    report_display_name = NA_character_,
    table_heading = metadata$table_heading,
    column_heading = metadata$column_heading,
    formula_type = metadata$formula_type,
    formula = metadata$formula,
    bin_rule_count = metadata$bin_rule_count,
    definition_catalog_index = metadata$catalog_index,
    definition_name = metadata$object_name,
    definition_table_heading = metadata$table_heading,
    definition_column_heading = metadata$column_heading,
    definition_path = metadata$path,
    is_resolved = TRUE,
    is_referenced = metadata$path %in% referenced_paths,
    stringsAsFactors = FALSE
  ))
  for (metadata in saved_metadata) {
    rule_rows[[length(rule_rows) + 1L]] <- column_rule_rows(
      metadata, "standalone_saved_object"
    )
  }

  column_parts <- c(usage_rows, standalone_rows)
  if (!length(column_parts)) stop("No report or standalone saved columns were found.")
  columns_output_data <- do.call(rbind, column_parts)
  columns_output_data$column_record_id <- paste0("C", seq_len(nrow(columns_output_data)))
  columns_output_data <- columns_output_data[c(
    "column_record_id", setdiff(names(columns_output_data), "column_record_id")
  )]
  column_rules <- if (length(rule_rows)) do.call(rbind, rule_rows) else data.frame()
  rownames(columns_output_data) <- NULL
  rownames(column_rules) <- NULL

  dir.create(dirname(columns_output), recursive = TRUE, showWarnings = FALSE)
  write.csv(columns_output_data, columns_output, row.names = FALSE, na = "")
  write.csv(column_rules, rules_output, row.names = FALSE, na = "")
  message("Wrote ", columns_output, " (", nrow(columns_output_data), " column records)")
  message("Wrote ", rules_output, " (", nrow(column_rules), " column rules)")
  invisible(list(columns = columns_output_data, rules = column_rules))
}
