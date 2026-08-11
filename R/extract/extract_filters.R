# Extract all report-filter usages and standalone saved-filter definitions.

filter_catalog_path <- function(path) {
  if (is.na(path)) return(NA_character_)
  gsub("\\/", "/", path, fixed = TRUE)
}

filter_value_rows <- function(node, rule_id, source_type, rule_name, field) {
  expression_type <- report_xml_local_type(node)
  if (is.na(expression_type) || expression_type != "list") return(NULL)
  children <- xml2::xml_find_all(node, "./*[local-name()='expr']")
  if (length(children) < 2L) return(NULL)
  values <- vapply(children[-1L], report_xml_clean_text, character(1L))
  data.frame(
    rule_id = rule_id,
    source_type = source_type,
    rule_name = rule_name,
    field = field,
    value_index = seq_along(values),
    value = values,
    stringsAsFactors = FALSE
  )
}

extract_filters <- function(
    input_path = "output/catalog_extract.rds",
    filters_output = "output/filters.csv",
    rules_output = "output/filter_rules.csv",
    values_output = "output/filter_value_lists.csv",
    value_preview_limit = 10L) {
  if (!requireNamespace("xml2", quietly = TRUE)) stop("Package 'xml2' is required.")
  if (!file.exists(input_path)) stop("Missing input file: ", input_path)
  value_preview_limit <- as.integer(value_preview_limit)
  if (is.na(value_preview_limit) || value_preview_limit < 1L) {
    stop("value_preview_limit must be a positive integer")
  }
  catalog <- readRDS(input_path)
  required <- c(
    "catalog_index", "object_kind", "object_title", "subject_area",
    "original_path", "xml_text"
  )
  missing <- setdiff(required, names(catalog))
  if (length(missing)) stop("Input is missing columns: ", paste(missing, collapse = ", "))

  saved_objects <- catalog[catalog$object_kind == "filter", , drop = FALSE]
  saved_roots <- lapply(seq_len(nrow(saved_objects)), function(i) {
    doc <- tryCatch(xml2::read_xml(saved_objects$xml_text[[i]]), error = function(e) NULL)
    if (is.null(doc)) return(NULL)
    root <- xml2::xml_find_first(doc, ".//*[local-name()='filter']/*[local-name()='expr']")
    if (inherits(root, "xml_missing")) NULL else root
  })
  names(saved_roots) <- saved_objects$original_path

  count_leaf_rules <- function(root) {
    if (is.null(root)) return(0L)
    type <- report_xml_local_type(root)
    if (!is.na(type) && type == "logical") {
      children <- xml2::xml_find_all(root, "./*[local-name()='expr']")
      return(sum(vapply(children, count_leaf_rules, integer(1L))))
    }
    1L
  }
  saved_rule_counts <- vapply(saved_roots, count_leaf_rules, integer(1L))

  lookup_saved <- function(path) {
    if (is.na(path)) return(NULL)
    index <- match(path, saved_objects$original_path)
    if (is.na(index)) NULL else list(
      catalog_index = saved_objects$catalog_index[[index]],
      object_name = saved_objects$object_title[[index]],
      subject_area = saved_objects$subject_area[[index]],
      path = saved_objects$original_path[[index]],
      root = saved_roots[[index]],
      rule_count = saved_rule_counts[[index]]
    )
  }

  filter_rows <- list()
  rule_rows <- list()
  value_rows <- list()
  referenced_paths <- character()

  add_rule <- function(
      node, rule_id, rule_index, source_type, record_scope, rule_name,
      join_operator = NA_character_, report = NULL, report_filter_index = NA_integer_,
      definition = NULL) {
    details <- report_filter_term_details(node, value_preview_limit)
    report_index <- if (is.null(report)) NA_integer_ else report$catalog_index[[1L]]
    report_title <- if (is.null(report)) NA_character_ else report$object_title[[1L]]
    report_path <- if (is.null(report)) NA_character_ else report$original_path[[1L]]
    rule_rows[[length(rule_rows) + 1L]] <<- data.frame(
      rule_id = rule_id,
      rule_type = "Filter",
      source_type = source_type,
      record_scope = record_scope,
      rule_name = rule_name,
      rule_index = rule_index,
      criterion_text = details$filter_text,
      review_label = "",
      explanation = "",
      criterion_text_category = NA_character_,
      join_operator = join_operator,
      value_count = details$value_count,
      report_catalog_index = report_index,
      report_title = report_title,
      report_path = report_path,
      report_filter_index = report_filter_index,
      definition_catalog_index = if (is.null(definition)) {
        NA_integer_
      } else definition$catalog_index,
      definition_name = if (is.null(definition)) NA_character_ else definition$object_name,
      definition_path = if (is.null(definition)) NA_character_ else definition$path,
      stringsAsFactors = FALSE
    )
    values <- filter_value_rows(node, rule_id, source_type, rule_name, details$field)
    if (!is.null(values)) value_rows[[length(value_rows) + 1L]] <<- values
    invisible(details)
  }

  reports <- catalog[catalog$object_kind == "report", , drop = FALSE]
  for (i in seq_len(nrow(reports))) {
    doc <- tryCatch(xml2::read_xml(reports$xml_text[[i]]), error = function(e) NULL)
    if (is.null(doc)) next
    criteria <- xml2::xml_find_first(doc, ".//*[local-name()='criteria']")
    root <- xml2::xml_find_first(
      criteria, "./*[local-name()='filter']/*[local-name()='expr']"
    )
    if (inherits(root, "xml_missing")) next
    subject_area <- report_xml_safe_attr(criteria, "subjectArea")
    if (!is.na(subject_area)) subject_area <- sub('^"', "", sub('"$', "", subject_area))
    filter_index <- 0L

    visit_report <- function(node, join_operator = NA_character_, group_path = "1",
                             nesting_level = 1L) {
      expression_type <- report_xml_local_type(node)
      if (!is.na(expression_type) && expression_type == "logical") {
        operator <- report_xml_safe_attr(node, "op")
        children <- xml2::xml_find_all(node, "./*[local-name()='expr']")
        for (child_index in seq_along(children)) {
          visit_report(
            children[[child_index]], operator,
            paste0(group_path, ".", child_index), nesting_level + 1L
          )
        }
        return(invisible(NULL))
      }

      filter_index <<- filter_index + 1L
      details <- report_filter_term_details(node, value_preview_limit)
      saved_path <- filter_catalog_path(details$saved_filter_path)
      definition <- lookup_saved(saved_path)
      if (!is.na(saved_path)) referenced_paths <<- c(referenced_paths, saved_path)
      filter_rows[[length(filter_rows) + 1L]] <<- data.frame(
        record_scope = "report_filter",
        report_catalog_index = reports$catalog_index[[i]],
        report_title = reports$object_title[[i]],
        report_subject_area = subject_area,
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
        value_count = details$value_count,
        value_preview = details$value_preview,
        definition_catalog_index = if (is.null(definition)) {
          NA_integer_
        } else definition$catalog_index,
        definition_name = if (is.null(definition)) {
          details$saved_filter_name
        } else definition$object_name,
        definition_path = saved_path,
        rule_count = if (is.null(definition)) {
          if (details$source == "inline") 1L else NA_integer_
        } else definition$rule_count,
        is_resolved = if (details$source == "saved_reference") {
          !is.null(definition)
        } else NA,
        is_referenced = NA,
        stringsAsFactors = FALSE
      )
      if (details$source == "inline") {
        add_rule(
          node,
          paste0("RF", reports$catalog_index[[i]], "-R", filter_index),
          filter_index, "inline", "report_inline", reports$object_title[[i]],
          join_operator, reports[i, , drop = FALSE], filter_index
        )
      }
      invisible(NULL)
    }
    visit_report(root)
  }

  standalone_rule_counter <- 0L
  for (i in seq_len(nrow(saved_objects))) {
    definition <- lookup_saved(saved_objects$original_path[[i]])
    if (is.null(definition)) next
    root_type <- report_xml_local_type(definition$root)
    filter_rows[[length(filter_rows) + 1L]] <- data.frame(
      record_scope = "standalone_saved_object",
      report_catalog_index = NA_integer_,
      report_title = NA_character_,
      report_subject_area = NA_character_,
      report_path = NA_character_,
      filter_index = NA_integer_,
      group_path = NA_character_,
      nesting_level = NA_integer_,
      join_operator = NA_character_,
      filter_source = "standalone_saved_object",
      expression_type = root_type,
      operator = report_xml_safe_attr(definition$root, "op"),
      field = NA_character_,
      filter_text = NA_character_,
      value_count = NA_integer_,
      value_preview = NA_character_,
      definition_catalog_index = definition$catalog_index,
      definition_name = definition$object_name,
      definition_path = definition$path,
      rule_count = definition$rule_count,
      is_resolved = TRUE,
      is_referenced = definition$path %in% referenced_paths,
      stringsAsFactors = FALSE
    )

    standalone_index <- 0L
    visit_saved <- function(node, join_operator = NA_character_) {
      type <- report_xml_local_type(node)
      if (!is.na(type) && type == "logical") {
        operator <- report_xml_safe_attr(node, "op")
        children <- xml2::xml_find_all(node, "./*[local-name()='expr']")
        for (child in children) visit_saved(child, operator)
        return(invisible(NULL))
      }
      standalone_index <<- standalone_index + 1L
      standalone_rule_counter <<- standalone_rule_counter + 1L
      add_rule(
        node, paste0("F", i, "-C", standalone_rule_counter),
        standalone_index, "standalone_saved_object", "standalone_saved_object",
        definition$object_name, join_operator, definition = definition
      )
      invisible(NULL)
    }
    visit_saved(definition$root)
  }

  if (!length(filter_rows)) stop("No report or standalone saved filters were found.")
  filters_output_data <- do.call(rbind, filter_rows)
  filters_output_data$filter_record_id <- paste0("F", seq_len(nrow(filters_output_data)))
  filters_output_data <- filters_output_data[c(
    "filter_record_id", setdiff(names(filters_output_data), "filter_record_id")
  )]
  filter_rules <- if (length(rule_rows)) do.call(rbind, rule_rows) else data.frame()
  filter_values <- if (length(value_rows)) do.call(rbind, value_rows) else data.frame(
    rule_id = character(), source_type = character(), rule_name = character(),
    field = character(), value_index = integer(), value = character()
  )
  rownames(filters_output_data) <- NULL
  rownames(filter_rules) <- NULL
  rownames(filter_values) <- NULL

  dir.create(dirname(filters_output), recursive = TRUE, showWarnings = FALSE)
  write.csv(filters_output_data, filters_output, row.names = FALSE, na = "")
  write.csv(filter_rules, rules_output, row.names = FALSE, na = "")
  write.csv(filter_values, values_output, row.names = FALSE, na = "")
  message("Wrote ", filters_output, " (", nrow(filters_output_data), " filter records)")
  message("Wrote ", rules_output, " (", nrow(filter_rules), " filter rules)")
  message("Wrote ", values_output, " (", nrow(filter_values), " list values)")
  invisible(list(filters = filters_output_data, rules = filter_rules, values = filter_values))
}
