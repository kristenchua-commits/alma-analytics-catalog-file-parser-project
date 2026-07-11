# Stage 3: create a concise, documentation-oriented filter review workbook.
export_filter_review <- function(
    input_path = "output/filter_objects.rds",
    output_xlsx = "output/filter_review.xlsx",
    review_csv = "output/filter_review.csv",
    values_csv = "output/filter_review_value_lists.csv") {
  if (!requireNamespace("xml2", quietly = TRUE)) stop("Package 'xml2' is required.")
  if (!requireNamespace("writexl", quietly = TRUE)) {
    stop("Package 'writexl' is required. Install it with install.packages('writexl').")
  }
  if (!file.exists(input_path)) stop("Missing input file: ", input_path)
  filters <- readRDS(input_path)

  required <- c("filter_object_index", "catalog_index", "object_title",
                "subject_area", "original_path", "xml_text")
  missing <- setdiff(required, names(filters))
  if (length(missing)) stop("Input is missing columns: ", paste(missing, collapse = ", "))

  clean_text <- function(node) {
    value <- trimws(gsub("[[:space:]]+", " ", xml2::xml_text(node)))
    if (!nzchar(value)) NA_character_ else value
  }
  node_type <- function(node) {
    value <- xml2::xml_attr(node, "type")
    if (is.na(value)) "" else value
  }
  node_operator <- function(node) {
    value <- xml2::xml_attr(node, "op")
    if (is.na(value)) "" else value
  }
  quote_value <- function(value, type) {
    if (grepl("string", type, fixed = TRUE)) {
      paste0("'", gsub("'", "''", value, fixed = TRUE), "'")
    } else {
      value
    }
  }
  operator_label <- function(operator) {
    labels <- c(equal = "=", notEqual = "<>", greater = ">",
                greaterOrEqual = ">=", less = "<", lessOrEqual = "<=",
                like = "LIKE", notLike = "NOT LIKE", isNull = "IS NULL",
                isNotNull = "IS NOT NULL")
    if (operator %in% names(labels)) labels[[operator]] else toupper(operator)
  }

  review_rows <- list()
  value_rows <- list()
  criterion_counter <- 0L

  add_rule <- function(filter_row, criterion, join_operator = "", values = character(0),
                       field = NA_character_) {
    criterion_counter <<- criterion_counter + 1L
    criterion_id <- paste0("F", filter_row$filter_object_index, "-C", criterion_counter)
    review_rows[[length(review_rows) + 1L]] <<- data.frame(
      filter_object_index = filter_row$filter_object_index,
      catalog_index = filter_row$catalog_index,
      filter_name = filter_row$object_title,
      criterion_id = criterion_id,
      join_operator = toupper(join_operator),
      filter_criterion = criterion,
      value_count = length(values),
      review_label = "",
      explanation = "",
      subject_area = filter_row$subject_area,
      original_path = filter_row$original_path,
      stringsAsFactors = FALSE
    )
    if (length(values)) {
      value_rows[[length(value_rows) + 1L]] <<- data.frame(
        filter_name = filter_row$object_title,
        criterion_id = criterion_id,
        field = field,
        value_index = seq_along(values),
        value = values,
        stringsAsFactors = FALSE
      )
    }
  }

  visit <- function(node, filter_row, join_operator = "") {
    children <- xml2::xml_find_all(node, "./*[local-name()='expr']")
    type <- node_type(node)
    operator <- node_operator(node)

    if (grepl("logical", type, fixed = TRUE)) {
      for (child in children) visit(child, filter_row, operator)
      return(invisible(NULL))
    }

    if (grepl("list", type, fixed = TRUE) && length(children) >= 2L) {
      field <- clean_text(children[[1L]])
      raw_values <- vapply(children[-1L], clean_text, character(1))
      value_types <- vapply(children[-1L], node_type, character(1))
      display_values <- mapply(quote_value, raw_values, value_types, USE.NAMES = FALSE)
      keyword <- if (operator == "notIn") "NOT IN" else "IN"
      criterion <- if (length(display_values) <= 12L) {
        paste0(field, " ", keyword, " (", paste(display_values, collapse = ", "), ")")
      } else {
        paste0(field, " ", keyword, " (", format(length(display_values), big.mark = ","),
               " values; see Value List Summary and companion CSV)")
      }
      add_rule(filter_row, criterion, join_operator, raw_values, field)
      return(invisible(NULL))
    }

    if (grepl("comparison", type, fixed = TRUE) && length(children)) {
      parts <- vapply(children, clean_text, character(1))
      part_types <- vapply(children, node_type, character(1))
      if (length(parts) > 1L) {
        parts[-1L] <- mapply(quote_value, parts[-1L], part_types[-1L], USE.NAMES = FALSE)
      }
      criterion <- if (operator == "between" && length(parts) >= 3L) {
        paste(parts[1L], "BETWEEN", parts[2L], "AND", parts[3L])
      } else if (length(parts) >= 2L) {
        paste(parts[1L], operator_label(operator), parts[2L])
      } else {
        paste(parts[1L], operator_label(operator))
      }
      add_rule(filter_row, criterion, join_operator)
      return(invisible(NULL))
    }

    if (!length(children)) {
      add_rule(filter_row, clean_text(node), join_operator)
      return(invisible(NULL))
    }

    for (child in children) visit(child, filter_row, join_operator)
    invisible(NULL)
  }

  for (i in seq_len(nrow(filters))) {
    filter_row <- filters[i, , drop = FALSE]
    doc <- tryCatch(xml2::read_xml(filter_row$xml_text), error = function(e) NULL)
    if (is.null(doc)) next
    root <- xml2::xml_find_first(doc, ".//*[local-name()='filter']/*[local-name()='expr']")
    if (!inherits(root, "xml_missing")) visit(root, filter_row)
  }

  if (!length(review_rows)) stop("No review criteria were extracted.")
  review <- do.call(rbind, review_rows)
  value_lists <- if (length(value_rows)) do.call(rbind, value_rows) else data.frame(
    filter_name = character(0), criterion_id = character(0), field = character(0),
    value_index = integer(0), value = character(0)
  )
  object_index <- unique(filters[c(
    "filter_object_index", "catalog_index", "object_title", "subject_area", "original_path"
  )])
  names(object_index)[names(object_index) == "object_title"] <- "filter_name"

  dir.create(dirname(output_xlsx), recursive = TRUE, showWarnings = FALSE)
  write.csv(review, review_csv, row.names = FALSE, na = "")
  write.csv(value_lists, values_csv, row.names = FALSE, na = "")
  review_sheet <- review[c(
    "filter_name", "criterion_id", "join_operator", "filter_criterion",
    "value_count", "review_label", "explanation"
  )]
  value_list_summary <- if (nrow(value_lists)) {
    groups <- split(value_lists, value_lists$criterion_id)
    do.call(rbind, lapply(groups, function(group) data.frame(
      filter_name = group$filter_name[1L],
      criterion_id = group$criterion_id[1L],
      field = group$field[1L],
      value_count = nrow(group),
      sample_values = paste(utils::head(group$value, 10L), collapse = ", "),
      stringsAsFactors = FALSE
    )))
  } else {
    data.frame(filter_name = character(0), criterion_id = character(0),
               field = character(0), value_count = integer(0),
               sample_values = character(0))
  }
  writexl::write_xlsx(
    list("Filter Review" = review_sheet, "Value List Summary" = value_list_summary,
         "Object Index" = object_index),
    output_xlsx
  )
  message("Wrote ", output_xlsx, " (", nrow(review), " review rows)")
  message("Wrote ", values_csv, " (", nrow(value_lists), " list values)")
  invisible(list(review = review, value_lists = value_lists, object_index = object_index))
}

if (!interactive() && sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  export_filter_review(
    if (length(args)) args[1L] else "output/filter_objects.rds",
    if (length(args) > 1L) args[2L] else "output/filter_review.xlsx"
  )
}
