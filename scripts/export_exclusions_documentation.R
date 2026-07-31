#!/usr/bin/env Rscript

# Export each campus worksheet from the normalized review workbook as a
# campus-specific "Exclusions documentation" workbook.
#
# Usage:
#   Rscript scripts/export_exclusions_documentation.R [input.xlsx] [output_dir]
#
# Defaults:
#   input.xlsx:  normalized_combined_saved_column_and_filter_review_data.xlsx
#                (with fallbacks for the repository's current output name)
#   output_dir:  output/exclusions_documentation

required_packages <- c("readxl", "openxlsx2")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0L) {
  stop(
    "Install the required R package(s) before running this script: ",
    paste(missing_packages, collapse = ", "),
    call. = FALSE
  )
}

args <- commandArgs(trailingOnly = TRUE)

input_candidates <- unique(c(
  if (length(args) >= 1L) args[[1L]] else character(),
  "normalized_combined_saved_column_and_filter_review_data.xlsx",
  file.path("output", "normalized_combined_saved_column_and_filter_review_data.xlsx"),
  file.path("output", "normalized_combined_saved_column_and_filter_review.xlsx")
))

input_path <- input_candidates[file.exists(input_candidates)][1L]
if (is.na(input_path)) {
  stop(
    "Could not find the normalized review workbook. Checked: ",
    paste(input_candidates, collapse = ", "),
    call. = FALSE
  )
}

output_dir <- if (length(args) >= 2L) {
  args[[2L]]
} else {
  file.path("output", "exclusions_documentation")
}
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

fiscal_year <- "FY 2025-26"
documentation_sheet <- "Exclusions documentation"

expected_columns <- c(
  "rule_type",
  "source_workbook",
  "Electronic/Physical/Fulfillment",
  "Campus",
  "rule_name",
  "rule_id",
  "join_operator",
  "criterion_text",
  "value_count",
  "review_label",
  "explanation",
  "criterion_text_category"
)

campus_sheets <- readxl::excel_sheets(input_path)
campus_sheets <- campus_sheets[grepl("^UC[A-Z]+$", campus_sheets)]

if (length(campus_sheets) == 0L) {
  stop(
    "No campus worksheets were found. Campus worksheet names must begin with UC.",
    call. = FALSE
  )
}

as_text <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x
}

first_seen <- function(x) {
  unique(as_text(x))
}

ordered_values <- function(x, preferred = character()) {
  values <- first_seen(x)
  c(intersect(preferred, values), setdiff(values, preferred))
}

sanitize_filename <- function(x) {
  x <- gsub("[<>:\"/\\\\|?*]", "-", x)
  x <- gsub("[[:space:]]+", " ", x)
  trimws(x)
}

append_row <- function(rows, hierarchy, level, source = NULL) {
  blank <- rep("", 11L)
  names(blank) <- c(
    "Domain", "Scope", "Rule Type", "Rule Name", "Rule ID", "Join",
    "Bin Label", "Explanation", "Value Count", "Criterion Category",
    "Source Workbook"
  )

  if (!is.null(source)) {
    blank[] <- c(
      as_text(source[["Electronic/Physical/Fulfillment"]]),
      as_text(source[["Campus"]]),
      as_text(source[["rule_type"]]),
      as_text(source[["rule_name"]]),
      as_text(source[["rule_id"]]),
      as_text(source[["join_operator"]]),
      as_text(source[["review_label"]]),
      as_text(source[["explanation"]]),
      as_text(source[["value_count"]]),
      as_text(source[["criterion_text_category"]]),
      as_text(source[["source_workbook"]])
    )
  }

  rows[[length(rows) + 1L]] <- data.frame(
    Hierarchy = hierarchy,
    Level = level,
    as.data.frame(as.list(blank), check.names = FALSE),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  rows
}

build_hierarchy <- function(data, campus) {
  data[] <- lapply(data, as_text)
  data$.source_order <- seq_len(nrow(data))
  rows <- list()
  level_number <- integer()

  domains <- ordered_values(
    data[["Electronic/Physical/Fulfillment"]],
    c("Electronic", "Physical", "Fulfillment", "")
  )

  for (domain in domains) {
    domain_label <- if (nzchar(domain)) domain else "Unclassified domain"
    rows <- append_row(rows, domain_label, "Domain")
    level_number <- c(level_number, 1L)
    domain_data <- data[data[["Electronic/Physical/Fulfillment"]] == domain, , drop = FALSE]

    scopes <- ordered_values(domain_data[["Campus"]], c("Global", campus, ""))
    for (scope in scopes) {
      scope_label <- if (nzchar(scope)) paste0(scope, " scope") else "Unclassified scope"
      rows <- append_row(rows, scope_label, "Scope")
      level_number <- c(level_number, 2L)
      scope_data <- domain_data[domain_data[["Campus"]] == scope, , drop = FALSE]

      rule_types <- ordered_values(scope_data[["rule_type"]], c("Filter", "Saved Column", ""))
      for (rule_type in rule_types) {
        type_label <- if (nzchar(rule_type)) rule_type else "Unclassified rule type"
        rows <- append_row(rows, type_label, "Rule type")
        level_number <- c(level_number, 3L)
        type_data <- scope_data[scope_data[["rule_type"]] == rule_type, , drop = FALSE]

        rule_names <- first_seen(type_data[["rule_name"]])
        for (rule_name in rule_names) {
          rule_label <- if (nzchar(rule_name)) rule_name else "Unnamed rule"
          rows <- append_row(rows, rule_label, "Rule")
          level_number <- c(level_number, 4L)
          rule_data <- type_data[type_data[["rule_name"]] == rule_name, , drop = FALSE]
          rule_data <- rule_data[order(rule_data$.source_order), , drop = FALSE]

          for (i in seq_len(nrow(rule_data))) {
            criterion <- rule_data[["criterion_text"]][[i]]
            if (!nzchar(criterion)) {
              criterion <- "(No criterion text supplied)"
            }
            rows <- append_row(
              rows,
              hierarchy = criterion,
              level = "Criterion",
              source = rule_data[i, , drop = FALSE]
            )
            level_number <- c(level_number, 5L)
          }
        }
      }
    }
  }

  list(
    documentation = do.call(rbind, rows),
    outline_level = level_number
  )
}

style_rows <- function(wb, sheet, rows, fill, font_color = "FF000000",
                       bold = TRUE, size = "11") {
  if (length(rows) == 0L) {
    return(invisible(wb))
  }
  dims <- openxlsx2::wb_dims(rows = rows, cols = 1:13)
  wb <- openxlsx2::wb_add_fill(
    wb, sheet = sheet, dims = dims,
    color = openxlsx2::wb_color(hex = fill)
  )
  wb <- openxlsx2::wb_add_font(
    wb, sheet = sheet, dims = dims,
    name = "Aptos", color = openxlsx2::wb_color(hex = font_color),
    size = size, bold = bold
  )
  wb
}

write_campus_workbook <- function(data, campus, output_file) {
  hierarchy <- build_hierarchy(data, campus)
  documentation <- hierarchy$documentation
  outline_level <- hierarchy$outline_level

  summary <- data.frame(
    Measure = c(
      "Campus worksheet",
      "Source records",
      "Filter criteria",
      "Saved-column mappings",
      "Explicit exclusion labels",
      "Distinct rules"
    ),
    Value = c(
      campus,
      nrow(data),
      sum(as_text(data$rule_type) == "Filter"),
      sum(as_text(data$rule_type) == "Saved Column"),
      sum(tolower(as_text(data$review_label)) %in% c("exclude", "excluded")),
      length(unique(as_text(data$rule_name)))
    ),
    stringsAsFactors = FALSE
  )

  wb <- openxlsx2::wb_workbook(
    creator = "UC Libraries",
    title = paste(campus, fiscal_year, "Exclusions documentation"),
    subject = "Hierarchical documentation of filters and saved-column mappings"
  )
  wb <- openxlsx2::wb_set_base_font(wb, font_size = 10, font_name = "Aptos")
  wb <- openxlsx2::wb_add_worksheet(
    wb, sheet = documentation_sheet, grid_lines = FALSE,
    tab_color = "FF1F4E78", orientation = "landscape"
  )

  wb <- openxlsx2::wb_add_data(
    wb, sheet = documentation_sheet,
    x = paste(campus, "Targeted Review", fiscal_year, "— Exclusions Documentation"),
    dims = "A1", col_names = FALSE
  )
  wb <- openxlsx2::wb_merge_cells(wb, sheet = documentation_sheet, dims = "A1:M1")
  wb <- openxlsx2::wb_add_fill(
    wb, sheet = documentation_sheet, dims = "A1:M1",
    color = openxlsx2::wb_color(hex = "FF1F4E78")
  )
  wb <- openxlsx2::wb_add_font(
    wb, sheet = documentation_sheet, dims = "A1:M1",
    name = "Aptos Display", color = openxlsx2::wb_color(hex = "FFFFFFFF"),
    size = "18", bold = TRUE
  )
  wb <- openxlsx2::wb_add_cell_style(
    wb, sheet = documentation_sheet, dims = "A1:M1",
    horizontal = "left", vertical = "center"
  )

  source_note <- paste0(
    "Source: ", basename(input_path), " / ", campus,
    ". Hierarchy: Domain > Scope > Rule type > Rule name > Criterion. ",
    "All source rows are retained."
  )
  wb <- openxlsx2::wb_add_data(
    wb, sheet = documentation_sheet, x = source_note,
    dims = "A2", col_names = FALSE
  )
  wb <- openxlsx2::wb_merge_cells(wb, sheet = documentation_sheet, dims = "A2:M2")
  wb <- openxlsx2::wb_add_font(
    wb, sheet = documentation_sheet, dims = "A2:M2",
    name = "Aptos", color = openxlsx2::wb_color(hex = "FF44546A"),
    size = "9", italic = TRUE
  )
  wb <- openxlsx2::wb_add_cell_style(
    wb, sheet = documentation_sheet, dims = "A2:M2",
    vertical = "center", wrap_text = TRUE
  )

  wb <- openxlsx2::wb_add_data(
    wb, sheet = documentation_sheet, x = summary,
    start_row = 4, start_col = 1, col_names = TRUE
  )
  wb <- openxlsx2::wb_add_fill(
    wb, sheet = documentation_sheet, dims = "A4:B4",
    color = openxlsx2::wb_color(hex = "FFD9EAF7")
  )
  wb <- openxlsx2::wb_add_font(
    wb, sheet = documentation_sheet, dims = "A4:B4",
    name = "Aptos", color = openxlsx2::wb_color(hex = "FF1F1F1F"),
    size = "10", bold = TRUE
  )
  wb <- openxlsx2::wb_add_border(
    wb, sheet = documentation_sheet, dims = "A4:B10",
    bottom_color = openxlsx2::wb_color(hex = "FFD9E2F3"),
    left_color = openxlsx2::wb_color(hex = "FFD9E2F3"),
    right_color = openxlsx2::wb_color(hex = "FFD9E2F3"),
    top_color = openxlsx2::wb_color(hex = "FFD9E2F3"),
    bottom_border = "thin", left_border = "thin",
    right_border = "thin", top_border = "thin",
    inner_hgrid = "thin", inner_hcolor = openxlsx2::wb_color(hex = "FFD9E2F3"),
    inner_vgrid = "thin", inner_vcolor = openxlsx2::wb_color(hex = "FFD9E2F3")
  )

  table_header_row <- 12L
  first_documentation_row <- table_header_row + 1L
  last_documentation_row <- table_header_row + nrow(documentation)

  wb <- openxlsx2::wb_add_data(
    wb, sheet = documentation_sheet, x = documentation,
    start_row = table_header_row, start_col = 1, col_names = TRUE
  )

  header_dims <- paste0("A", table_header_row, ":M", table_header_row)
  wb <- openxlsx2::wb_add_fill(
    wb, sheet = documentation_sheet, dims = header_dims,
    color = openxlsx2::wb_color(hex = "FF5B9BD5")
  )
  wb <- openxlsx2::wb_add_font(
    wb, sheet = documentation_sheet, dims = header_dims,
    name = "Aptos", color = openxlsx2::wb_color(hex = "FFFFFFFF"),
    size = "10", bold = TRUE
  )
  wb <- openxlsx2::wb_add_cell_style(
    wb, sheet = documentation_sheet, dims = header_dims,
    horizontal = "center", vertical = "center", wrap_text = TRUE
  )
  wb <- openxlsx2::wb_add_filter(
    wb, sheet = documentation_sheet,
    rows = table_header_row, cols = 1:13
  )

  excel_rows <- first_documentation_row:last_documentation_row
  level_rows <- split(excel_rows, documentation$Level)
  wb <- style_rows(wb, documentation_sheet, level_rows[["Domain"]], "FF1F4E78", "FFFFFFFF", TRUE, "12")
  wb <- style_rows(wb, documentation_sheet, level_rows[["Scope"]], "FFD9EAF7", "FF1F1F1F", TRUE, "11")
  wb <- style_rows(wb, documentation_sheet, level_rows[["Rule type"]], "FFE2F0D9", "FF1F1F1F", TRUE, "10")
  wb <- style_rows(wb, documentation_sheet, level_rows[["Rule"]], "FFFFF2CC", "FF1F1F1F", TRUE, "10")

  criterion_rows <- level_rows[["Criterion"]]
  if (length(criterion_rows) > 0L) {
    criterion_dims <- openxlsx2::wb_dims(rows = criterion_rows, cols = 1:13)
    wb <- openxlsx2::wb_add_fill(
      wb, sheet = documentation_sheet, dims = criterion_dims,
      color = openxlsx2::wb_color(hex = "FFFFFFFF")
    )
    wb <- openxlsx2::wb_add_border(
      wb, sheet = documentation_sheet, dims = criterion_dims,
      bottom_color = openxlsx2::wb_color(hex = "FFD9D9D9"),
      left_color = openxlsx2::wb_color(hex = "FFFFFFFF"),
      right_color = openxlsx2::wb_color(hex = "FFFFFFFF"),
      top_color = openxlsx2::wb_color(hex = "FFFFFFFF"),
      bottom_border = "thin", left_border = "none",
      right_border = "none", top_border = "none"
    )
  }

  for (level in 1:5) {
    rows_at_level <- excel_rows[outline_level == level]
    if (length(rows_at_level) > 0L) {
      wb <- openxlsx2::wb_add_cell_style(
        wb, sheet = documentation_sheet,
        dims = openxlsx2::wb_dims(rows = rows_at_level, cols = 1),
        indent = as.character(level - 1L),
        vertical = "top", wrap_text = TRUE
      )
    }
  }

  body_dims <- paste0("A", first_documentation_row, ":M", last_documentation_row)
  wb <- openxlsx2::wb_add_cell_style(
    wb, sheet = documentation_sheet, dims = body_dims,
    vertical = "top"
  )
  wb <- openxlsx2::wb_add_cell_style(
    wb, sheet = documentation_sheet,
    dims = paste0("A", first_documentation_row, ":A", last_documentation_row),
    wrap_text = TRUE
  )
  wb <- openxlsx2::wb_add_cell_style(
    wb, sheet = documentation_sheet,
    dims = paste0("F", first_documentation_row, ":M", last_documentation_row),
    wrap_text = TRUE
  )

  # Excel outline levels mirror the five visual hierarchy levels.
  wb <- openxlsx2::wb_group_rows(
    wb, sheet = documentation_sheet,
    rows = excel_rows, collapsed = FALSE, levels = outline_level
  )

  wb <- openxlsx2::wb_freeze_pane(
    wb, sheet = documentation_sheet,
    first_active_row = first_documentation_row, first_active_col = 3
  )
  wb <- openxlsx2::wb_set_col_widths(
    wb, sheet = documentation_sheet, cols = 1:13,
    widths = c(72, 13, 14, 13, 15, 42, 13, 10, 20, 42, 12, 34, 20)
  )
  wb <- openxlsx2::wb_set_row_heights(wb, sheet = documentation_sheet, rows = 1, heights = 30)
  wb <- openxlsx2::wb_set_row_heights(wb, sheet = documentation_sheet, rows = 2, heights = 30)
  wb <- openxlsx2::wb_set_row_heights(
    wb, sheet = documentation_sheet, rows = table_header_row, heights = 30
  )
  wb <- openxlsx2::wb_set_page_setup(
    wb, sheet = documentation_sheet,
    orientation = "landscape", fit_to_width = 1, fit_to_height = FALSE,
    print_title_rows = seq_len(table_header_row),
    horizontal_centered = FALSE
  )
  openxlsx2::wb_save(wb, file = output_file, overwrite = TRUE)
}

created_files <- character()

for (campus in campus_sheets) {
  data <- readxl::read_excel(input_path, sheet = campus, .name_repair = "minimal")
  missing_columns <- setdiff(expected_columns, names(data))
  if (length(missing_columns) > 0L) {
    stop(
      "Worksheet '", campus, "' is missing required column(s): ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }

  data <- data[, expected_columns, drop = FALSE]
  campus_code <- if (grepl("^UC", campus)) campus else paste0("UC", campus)
  output_name <- paste0(
    "DRAFT ", campus_code, " targeted review ", fiscal_year,
    " - exclusions documentation.xlsx"
  )
  output_file <- file.path(output_dir, sanitize_filename(output_name))
  write_campus_workbook(data, campus_code, output_file)
  created_files <- c(created_files, normalizePath(output_file, mustWork = TRUE))
  message("Created: ", output_file)
}

message(
  "Finished: ", length(created_files), " campus workbook(s) written to ",
  normalizePath(output_dir, mustWork = TRUE)
)
