# Convert a saved-column expression node to normalized text.
saved_column_clean_text <- function(node) {
  if (inherits(node, "xml_missing") || !length(node)) return(NA_character_)
  value <- trimws(gsub("[[:space:]]+", " ", xml2::xml_text(node)))
  if (!nzchar(value)) NA_character_ else value
}

# Format an Alma expression value as a SQL literal. Alma stores string and
# numeric values in the same XML shape, so the xsi:type determines quoting.
saved_column_sql_literal <- function(node) {
  value <- saved_column_clean_text(node)
  if (is.na(value)) return(NA_character_)

  value_type <- xml2::xml_attr(node, "type")
  if (!is.na(value_type) && grepl(
    "decimal|double|float|integer|long|number|short",
    value_type,
    ignore.case = TRUE
  )) {
    return(value)
  }
  if (!is.na(value_type) && grepl("boolean", value_type, ignore.case = TRUE)) {
    return(toupper(value))
  }

  paste0("'", gsub("'", "''", value, fixed = TRUE), "'")
}

# Translate Alma saved-column XML operators to SQL criteria. Raw sawx:sql
# expressions already contain SQL and pass through unchanged.
format_saved_column_condition <- function(condition_node) {
  expression <- xml2::xml_find_first(condition_node, "./*[local-name()='expr']")
  if (inherits(expression, "xml_missing")) return(NA_character_)

  expression_type <- xml2::xml_attr(expression, "type")
  children <- xml2::xml_find_all(expression, "./*[local-name()='expr']")
  if (!length(children)) return(saved_column_clean_text(expression))

  operator <- xml2::xml_attr(expression, "op")
  field <- saved_column_clean_text(children[[1L]])
  if (is.na(field) || is.na(operator)) {
    stop("Structured saved-column expression is missing its field or operator.")
  }

  if (!is.na(expression_type) && grepl("comparison", expression_type, fixed = TRUE)) {
    if (operator == "null") return(paste(field, "IS NULL"))
    if (operator == "notNull") return(paste(field, "IS NOT NULL"))

    comparison_operators <- c(
      equal = "=",
      notEqual = "<>",
      greater = ">",
      greaterThan = ">",
      greaterOrEqual = ">=",
      greaterThanOrEqual = ">=",
      less = "<",
      lessThan = "<",
      lessOrEqual = "<=",
      lessThanOrEqual = "<="
    )
    sql_operator <- unname(comparison_operators[operator])
    if (is.na(sql_operator)) {
      stop("Unsupported Alma saved-column comparison operator: ", operator)
    }
    if (length(children) < 2L) {
      stop("Alma saved-column comparison operator has no right-hand value: ", operator)
    }
    values <- vapply(children[-1L], saved_column_sql_literal, character(1))
    return(paste(field, sql_operator, paste(values, collapse = ", ")))
  }

  if (!is.na(expression_type) && grepl("list", expression_type, fixed = TRUE)) {
    if (length(children) < 2L) {
      stop("Alma saved-column list operator has no values: ", operator)
    }
    values <- vapply(children[-1L], saved_column_sql_literal, character(1))

    if (operator %in% c("in", "notIn")) {
      sql_operator <- if (operator == "notIn") "NOT IN" else "IN"
      return(paste0(field, " ", sql_operator, " (", paste(values, collapse = ", "), ")"))
    }

    raw_values <- vapply(children[-1L], saved_column_clean_text, character(1))
    wildcard_values <- switch(
      operator,
      beginsWith = paste0(raw_values, "%"),
      endsWith = paste0("%", raw_values),
      contains = paste0("%", raw_values, "%"),
      stop("Unsupported Alma saved-column list operator: ", operator)
    )
    like_clauses <- paste0(
      field,
      " LIKE '",
      gsub("'", "''", wildcard_values, fixed = TRUE),
      "'"
    )
    if (length(like_clauses) == 1L) return(like_clauses)
    return(paste0("(", paste(like_clauses, collapse = " OR "), ")"))
  }

  saved_column_clean_text(expression)
}

# Stage 3: create a documentation-oriented saved-column review CSV.
export_saved_column_review <- function(
    input_path = "output/saved_column_objects.rds",
    output_csv = "output/saved_column_review.csv") {
  if (!requireNamespace("xml2", quietly = TRUE)) stop("Package 'xml2' is required.")
  if (!file.exists(input_path)) stop("Missing input file: ", input_path)
  catalog <- readRDS(input_path)
  required <- c(
    "saved_column_object_index", "catalog_index", "object_kind",
    "object_title", "original_path", "xml_text"
  )
  missing <- setdiff(required, names(catalog))
  if (length(missing)) stop("Input is missing columns: ", paste(missing, collapse = ", "))
  if (!nrow(catalog) || any(catalog$object_kind != "saved_column")) {
    stop("Input must contain only saved-column objects.")
  }

  clean_text <- saved_column_clean_text
  find_text <- function(node, xpath) clean_text(xml2::xml_find_first(node, xpath))

  review_rows <- list()
  object_rows <- list()
  for (i in seq_len(nrow(catalog))) {
    doc <- tryCatch(xml2::read_xml(catalog$xml_text[i]), error = function(e) NULL)
    if (is.null(doc)) next
    column <- xml2::xml_find_first(doc, ".//*[local-name()='column']")
    column_heading <- find_text(
      column,
      ".//*[local-name()='columnHeading']//*[local-name()='caption']/*[local-name()='text']"
    )
    table_heading <- find_text(
      column,
      ".//*[local-name()='tableHeading']//*[local-name()='caption']/*[local-name()='text']"
    )
    base_formula <- find_text(
      column,
      ".//*[local-name()='baseFormula']/*[local-name()='expr']"
    )
    rules <- xml2::xml_find_all(column, ".//*[local-name()='rules']/*[local-name()='when']")
    otherwise <- xml2::xml_find_first(column, ".//*[local-name()='rules']/*[local-name()='otherwise']")

    if (length(rules)) {
      for (j in seq_along(rules)) {
        review_rows[[length(review_rows) + 1L]] <- data.frame(
          saved_column_name = catalog$object_title[i],
          rule_id = paste0("SC", catalog$saved_column_object_index[i], "-R", j),
          bin_criterion = format_saved_column_condition(
            xml2::xml_find_first(rules[[j]], ".//*[local-name()='condition']")
          ),
          bin_label = find_text(rules[[j]], ".//*[local-name()='value']/*[local-name()='expr']"),
          explanation = "",
          base_formula = base_formula,
          stringsAsFactors = FALSE
        )
      }
      if (!inherits(otherwise, "xml_missing")) {
        review_rows[[length(review_rows) + 1L]] <- data.frame(
          saved_column_name = catalog$object_title[i],
          rule_id = paste0("SC", catalog$saved_column_object_index[i], "-OTHER"),
          bin_criterion = "All other values",
          bin_label = find_text(otherwise, ".//*[local-name()='value']/*[local-name()='expr']"),
          explanation = "",
          base_formula = base_formula,
          stringsAsFactors = FALSE
        )
      }
      column_type <- "Binned column"
    } else {
      formula <- find_text(column, ".//*[local-name()='columnFormula']/*[local-name()='expr']")
      review_rows[[length(review_rows) + 1L]] <- data.frame(
        saved_column_name = catalog$object_title[i],
        rule_id = paste0("SC", catalog$saved_column_object_index[i], "-FORMULA"),
        bin_criterion = formula,
        bin_label = if (is.na(column_heading)) "Calculated value" else column_heading,
        explanation = "",
        base_formula = formula,
        stringsAsFactors = FALSE
      )
      column_type <- "Calculated column"
      base_formula <- formula
    }

    object_rows[[length(object_rows) + 1L]] <- data.frame(
      saved_column_object_index = catalog$saved_column_object_index[i],
      catalog_index = catalog$catalog_index[i],
      saved_column_name = catalog$object_title[i],
      column_type = column_type,
      table_heading = table_heading,
      column_heading = column_heading,
      base_formula = base_formula,
      rule_count = if (length(rules)) length(rules) + !inherits(otherwise, "xml_missing") else 1L,
      original_path = catalog$original_path[i],
      stringsAsFactors = FALSE
    )
  }

  review <- do.call(rbind, review_rows)
  object_index <- do.call(rbind, object_rows)
  review_output <- data.frame(
    rule_name = review$saved_column_name,
    criterion_text = review$bin_criterion,
    review_label = review$bin_label,
    explanation = review$explanation,
    criterion_text_category = review$base_formula,
    rule_id = review$rule_id,
    rule_type = rep("Saved Column", nrow(review)),
    source_workbook = rep("Saved Column Review", nrow(review)),
    join_operator = rep(NA_character_, nrow(review)),
    value_count = rep(NA_integer_, nrow(review)),
    stringsAsFactors = FALSE
  )
  dir.create(dirname(output_csv), recursive = TRUE, showWarnings = FALSE)
  write.csv(review_output, output_csv, row.names = FALSE, na = "")
  message("Wrote ", output_csv, " (", nrow(review), " review rows)")
  invisible(list(review = review, object_index = object_index))
}
