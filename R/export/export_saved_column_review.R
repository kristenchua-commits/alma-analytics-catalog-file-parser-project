# Stage 3: create a documentation-oriented saved-column review workbook.
export_saved_column_review <- function(
    input_path = "output/catalog_extract.rds",
    output_xlsx = "output/saved_column_review.xlsx",
    output_csv = "output/saved_column_review.csv") {
  if (!requireNamespace("xml2", quietly = TRUE)) stop("Package 'xml2' is required.")
  if (!requireNamespace("writexl", quietly = TRUE)) {
    stop("Package 'writexl' is required. Install it with install.packages('writexl').")
  }
  if (!file.exists(input_path)) stop("Missing input file: ", input_path)
  catalog <- readRDS(input_path)
  required <- c("catalog_index", "object_kind", "object_title", "original_path", "xml_text")
  missing <- setdiff(required, names(catalog))
  if (length(missing)) stop("Input is missing columns: ", paste(missing, collapse = ", "))
  catalog <- catalog[catalog$object_kind == "saved_column", , drop = FALSE]
  if (!nrow(catalog)) stop("No saved-column objects were found.")

  clean_text <- function(node) {
    if (inherits(node, "xml_missing") || !length(node)) return(NA_character_)
    value <- trimws(gsub("[[:space:]]+", " ", xml2::xml_text(node)))
    if (!nzchar(value)) NA_character_ else value
  }
  find_text <- function(node, xpath) clean_text(xml2::xml_find_first(node, xpath))
  format_condition <- function(condition_node) {
    expression <- xml2::xml_find_first(condition_node, "./*[local-name()='expr']")
    if (inherits(expression, "xml_missing")) return(NA_character_)

    expression_type <- xml2::xml_attr(expression, "type")
    children <- xml2::xml_find_all(expression, "./*[local-name()='expr']")

    # OBIEE list expressions store the field and every allowed value in
    # separate child nodes. xml_text() concatenates them without delimiters,
    # so join the parts explicitly for a readable spreadsheet criterion.
    if (!is.na(expression_type) && grepl("list", expression_type, fixed = TRUE) &&
        length(children)) {
      parts <- vapply(children, clean_text, character(1))
      parts <- parts[!is.na(parts) & nzchar(parts)]
      if (!length(parts)) return(NA_character_)

      operator <- xml2::xml_attr(expression, "op")
      operator_label <- if (!is.na(operator) && operator == "notIn") {
        " is not equal to / is not in "
      } else {
        " is equal to / is in "
      }

      if (length(parts) == 1L) return(paste0(parts[1L], operator_label))
      return(paste0(parts[1L], operator_label, paste(parts[-1L], collapse = "; ")))
    }

    clean_text(expression)
  }

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
          rule_id = paste0("SC", i, "-R", j),
          bin_criterion = format_condition(
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
          rule_id = paste0("SC", i, "-OTHER"),
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
        rule_id = paste0("SC", i, "-FORMULA"),
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
  review_sheet <- review[c(
    "saved_column_name", "bin_criterion", "bin_label", "explanation",
    "base_formula", "rule_id"
  )]
  dir.create(dirname(output_xlsx), recursive = TRUE, showWarnings = FALSE)
  write.csv(review, output_csv, row.names = FALSE, na = "")
  writexl::write_xlsx(
    list("Saved Column Review" = review_sheet, "Object Index" = object_index),
    output_xlsx
  )
  message("Wrote ", output_xlsx, " (", nrow(review), " review rows)")
  invisible(list(review = review, object_index = object_index))
}
