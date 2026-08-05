# Shared helpers for extracting report-embedded columns and filters.

report_xml_clean_text <- function(node) {
  if (inherits(node, "xml_missing") || !length(node)) return(NA_character_)
  value <- trimws(gsub("[[:space:]]+", " ", xml2::xml_text(node)))
  if (!nzchar(value)) NA_character_ else value
}

report_xml_safe_attr <- function(node, name) {
  value <- xml2::xml_attr(node, name)
  if (!length(value) || is.na(value) || !nzchar(value)) NA_character_ else value
}

report_xml_local_type <- function(node) {
  value <- report_xml_safe_attr(node, "type")
  if (is.na(value)) NA_character_ else sub("^.*:", "", value)
}

report_xml_first_text <- function(node, xpath) {
  report_xml_clean_text(xml2::xml_find_first(node, xpath))
}

report_filter_operand_text <- function(node) {
  node_type <- report_xml_local_type(node)
  if (!is.na(node_type) && node_type == "columnExpression") {
    formula <- report_xml_first_text(
      node,
      ".//*[local-name()='columnFormula']/*[local-name()='expr']"
    )
    if (!is.na(formula)) return(formula)
  }
  if (!is.na(node_type) && node_type == "binned") {
    formula <- report_xml_first_text(
      node,
      ".//*[local-name()='baseFormula']/*[local-name()='expr']"
    )
    if (!is.na(formula)) return(formula)
  }
  report_xml_clean_text(node)
}

report_filter_literal <- function(node) {
  value <- report_xml_clean_text(node)
  if (is.na(value)) return(NA_character_)
  node_type <- report_xml_local_type(node)
  if (!is.na(node_type) && grepl(
    "decimal|double|float|integer|long|number|short",
    node_type,
    ignore.case = TRUE
  )) {
    return(value)
  }
  if (!is.na(node_type) && grepl("boolean", node_type, ignore.case = TRUE)) {
    return(toupper(value))
  }
  paste0("'", gsub("'", "''", value, fixed = TRUE), "'")
}

report_filter_operator <- function(operator) {
  mapped <- c(
    equal = "=",
    notEqual = "<>",
    greater = ">",
    greaterThan = ">",
    greaterOrEqual = ">=",
    greaterThanOrEqual = ">=",
    less = "<",
    lessThan = "<",
    lessOrEqual = "<=",
    lessThanOrEqual = "<=",
    "in" = "IN",
    notIn = "NOT IN",
    null = "IS NULL",
    notNull = "IS NOT NULL",
    beginsWith = "BEGINS WITH",
    endsWith = "ENDS WITH",
    contains = "CONTAINS"
  )
  value <- unname(mapped[operator])
  if (length(value) && !is.na(value)) value else operator
}

report_filter_term_details <- function(node, preview_limit = 10L) {
  expression_type <- report_xml_local_type(node)
  operator <- report_xml_safe_attr(node, "op")
  if (!is.na(expression_type) && expression_type == "savedFilter") {
    name <- report_xml_safe_attr(node, "name")
    path <- report_xml_safe_attr(node, "path")
    return(list(
      source = "saved_reference",
      expression_type = expression_type,
      operator = NA_character_,
      field = NA_character_,
      filter_text = paste0("Saved filter: ", if (is.na(name)) basename(path) else name),
      saved_filter_name = name,
      saved_filter_path = path,
      value_count = NA_integer_,
      value_preview = NA_character_
    ))
  }

  children <- xml2::xml_find_all(node, "./*[local-name()='expr']")
  if (!length(children)) {
    raw_text <- report_xml_clean_text(node)
    return(list(
      source = "inline",
      expression_type = expression_type,
      operator = operator,
      field = raw_text,
      filter_text = raw_text,
      saved_filter_name = NA_character_,
      saved_filter_path = NA_character_,
      value_count = 0L,
      value_preview = NA_character_
    ))
  }

  field <- report_filter_operand_text(children[[1L]])
  value_nodes <- if (length(children) > 1L) children[-1L] else children[0L]
  value_count <- length(value_nodes)
  preview_nodes <- utils::head(value_nodes, preview_limit)
  raw_values <- if (length(preview_nodes)) {
    vapply(preview_nodes, report_xml_clean_text, character(1L))
  } else character()
  literal_values <- if (length(preview_nodes)) {
    vapply(preview_nodes, report_filter_literal, character(1L))
  } else character()
  preview <- if (length(raw_values)) paste(raw_values, collapse = " | ") else NA_character_
  if (value_count > preview_limit) {
    preview <- paste0(preview, " | … (+", value_count - preview_limit, " more)")
  }

  sql_operator <- if (is.na(operator)) NA_character_ else report_filter_operator(operator)
  filter_text <- if (!is.na(expression_type) && expression_type %in% c("sql", "sqlExpression")) {
    report_xml_clean_text(node)
  } else if (!is.na(operator) && operator %in% c("null", "notNull")) {
    paste(field, sql_operator)
  } else if (!is.na(operator) && operator %in% c("in", "notIn")) {
    displayed <- paste(literal_values, collapse = ", ")
    if (value_count > preview_limit) {
      displayed <- paste0(displayed, ", … (+", value_count - preview_limit, " more)")
    }
    paste0(field, " ", sql_operator, " (", displayed, ")")
  } else if (!is.na(sql_operator) && length(literal_values)) {
    paste(field, sql_operator, paste(literal_values, collapse = ", "))
  } else {
    paste(stats::na.omit(c(field, sql_operator)), collapse = " ")
  }

  list(
    source = "inline",
    expression_type = expression_type,
    operator = operator,
    field = field,
    filter_text = filter_text,
    saved_filter_name = NA_character_,
    saved_filter_path = NA_character_,
    value_count = value_count,
    value_preview = preview
  )
}
