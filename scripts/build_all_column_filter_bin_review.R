#!/usr/bin/env Rscript

# Build a review workbook for every parsed column, bin rule, and filter rule.
#
# Usage:
#   Rscript scripts/build_all_column_filter_bin_review.R [input_dir] [output.xlsx]
#
# Defaults:
#   input_dir:   output
#   output.xlsx: <input_dir>/normalized_combined_all_column_filter_bin_review.xlsx

required_packages <- c("openxlsx2")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1L), quietly = TRUE)
]
if (length(missing_packages)) {
  stop("Install required package(s): ", paste(missing_packages, collapse = ", "))
}

script_file <- tryCatch(
  normalizePath(sys.frame(1)$ofile, mustWork = TRUE),
  error = function(e) normalizePath("scripts/build_all_column_filter_bin_review.R", mustWork = TRUE)
)
project_root <- dirname(dirname(script_file))
args <- commandArgs(trailingOnly = TRUE)

input_dir <- if (length(args) >= 1L) args[[1L]] else file.path(project_root, "output")
if (!grepl("^(/|[A-Za-z]:[/\\\\])", input_dir)) {
  input_dir <- file.path(project_root, input_dir)
}
output_file <- if (length(args) >= 2L) {
  args[[2L]]
} else {
  file.path(input_dir, "normalized_combined_all_column_filter_bin_review.xlsx")
}
if (!grepl("^(/|[A-Za-z]:[/\\\\])", output_file)) {
  output_file <- file.path(project_root, output_file)
}

input_files <- c(
  columns = file.path(input_dir, "columns.csv"),
  column_rules = file.path(input_dir, "column_rules.csv"),
  filters = file.path(input_dir, "filters.csv"),
  filter_rules = file.path(input_dir, "filter_rules.csv"),
  filter_values = file.path(input_dir, "filter_value_lists.csv")
)
missing_files <- input_files[!file.exists(input_files)]
if (length(missing_files)) {
  stop(
    "Missing parser output(s): ", paste(basename(missing_files), collapse = ", "),
    ". Run scripts/run_parsing_pipeline.R first."
  )
}

read_parser_csv <- function(path) {
  read.csv(
    path, check.names = FALSE, stringsAsFactors = FALSE,
    na.strings = "", colClasses = "character"
  )
}

columns <- read_parser_csv(input_files[["columns"]])
column_rules <- read_parser_csv(input_files[["column_rules"]])
filters <- read_parser_csv(input_files[["filters"]])
filter_rules <- read_parser_csv(input_files[["filter_rules"]])
filter_values <- read_parser_csv(input_files[["filter_values"]])

blank_column <- function(data, name) {
  if (name %in% names(data)) data[[name]] else rep(NA_character_, nrow(data))
}

column_item_type <- ifelse(column_rules$rule_kind %in% c("when", "otherwise"), "Bin", "Column")
column_review <- data.frame(
  rule_type = column_item_type,
  source_workbook = ifelse(column_item_type == "Bin", "Bin Review", "Column Review"),
  `Electronic/Physical/Fulfillment` = NA_character_,
  Campus = NA_character_,
  rule_name = column_rules$rule_name,
  rule_id = column_rules$rule_id,
  join_operator = column_rules$join_operator,
  criterion_text = column_rules$criterion_text,
  value_count = column_rules$value_count,
  review_label = column_rules$review_label,
  explanation = column_rules$explanation,
  criterion_text_category = column_rules$criterion_text_category,
  source_type = column_rules$source_type,
  record_scope = column_rules$record_scope,
  rule_index = column_rules$rule_index,
  rule_kind = column_rules$rule_kind,
  report_catalog_index = column_rules$report_catalog_index,
  report_title = column_rules$report_title,
  report_path = column_rules$report_path,
  report_item_index = blank_column(column_rules, "report_column_index"),
  definition_catalog_index = column_rules$definition_catalog_index,
  definition_name = column_rules$definition_name,
  definition_path = column_rules$definition_path,
  check.names = FALSE,
  stringsAsFactors = FALSE
)

filter_review <- data.frame(
  rule_type = "Filter",
  source_workbook = "Filter Review",
  `Electronic/Physical/Fulfillment` = NA_character_,
  Campus = NA_character_,
  rule_name = filter_rules$rule_name,
  rule_id = filter_rules$rule_id,
  join_operator = filter_rules$join_operator,
  criterion_text = filter_rules$criterion_text,
  value_count = filter_rules$value_count,
  review_label = filter_rules$review_label,
  explanation = filter_rules$explanation,
  criterion_text_category = filter_rules$criterion_text_category,
  source_type = filter_rules$source_type,
  record_scope = filter_rules$record_scope,
  rule_index = filter_rules$rule_index,
  rule_kind = NA_character_,
  report_catalog_index = filter_rules$report_catalog_index,
  report_title = filter_rules$report_title,
  report_path = filter_rules$report_path,
  report_item_index = blank_column(filter_rules, "report_filter_index"),
  definition_catalog_index = filter_rules$definition_catalog_index,
  definition_name = filter_rules$definition_name,
  definition_path = filter_rules$definition_path,
  check.names = FALSE,
  stringsAsFactors = FALSE
)

all_review <- rbind(column_review, filter_review)
all_review <- all_review[order(
  match(all_review$rule_type, c("Column", "Bin", "Filter")),
  all_review$source_type, all_review$report_title, all_review$rule_name,
  suppressWarnings(as.integer(all_review$rule_index)), na.last = TRUE
), , drop = FALSE]
rownames(all_review) <- NULL

source_label <- function(x) {
  ifelse(x == "inline", "Inline", ifelse(x == "standalone_saved_object", "Saved", x))
}
summary_data <- data.frame(
  Metric = c(
    "Review rows", "Column rows", "Bin-rule rows", "Filter-rule rows",
    "Inline review rows", "Saved-definition review rows",
    "Column inventory rows", "Filter inventory rows", "Expanded filter-list values"
  ),
  Count = c(
    nrow(all_review), sum(all_review$rule_type == "Column"),
    sum(all_review$rule_type == "Bin"), sum(all_review$rule_type == "Filter"),
    sum(all_review$source_type == "inline"),
    sum(all_review$source_type == "standalone_saved_object"),
    nrow(columns), nrow(filters), nrow(filter_values)
  ),
  stringsAsFactors = FALSE
)
scope_summary <- as.data.frame(
  table(Type = all_review$rule_type, Source = source_label(all_review$source_type)),
  stringsAsFactors = FALSE
)
scope_summary <- scope_summary[scope_summary$Freq > 0L, , drop = FALSE]

wb <- openxlsx2::wb_workbook(creator = "Alma Analytics catalog file parser")
wb <- openxlsx2::wb_set_base_font(wb, font_size = 10, font_name = "Aptos")

add_title <- function(wb, sheet, title, subtitle, data, table_name, widths = NULL) {
  wb <- openxlsx2::wb_add_worksheet(wb, sheet = sheet, grid_lines = FALSE)
  last_col <- max(1L, ncol(data))
  title_dims <- openxlsx2::wb_dims(rows = 1L, cols = seq_len(last_col))
  wb <- openxlsx2::wb_merge_cells(wb, sheet = sheet, dims = title_dims)
  wb <- openxlsx2::wb_add_data(wb, sheet = sheet, x = title, start_row = 1L, start_col = 1L)
  wb <- openxlsx2::wb_add_fill(
    wb, sheet = sheet, dims = title_dims,
    color = openxlsx2::wb_color(hex = "FF1F4E78")
  )
  wb <- openxlsx2::wb_add_font(
    wb, sheet = sheet, dims = title_dims, name = "Aptos Display",
    size = 16, bold = TRUE, color = openxlsx2::wb_color(hex = "FFFFFFFF")
  )
  wb <- openxlsx2::wb_add_cell_style(
    wb, sheet = sheet, dims = title_dims,
    horizontal = "left", vertical = "center"
  )
  subtitle_dims <- openxlsx2::wb_dims(rows = 2L, cols = seq_len(last_col))
  wb <- openxlsx2::wb_merge_cells(wb, sheet = sheet, dims = subtitle_dims)
  wb <- openxlsx2::wb_add_data(wb, sheet = sheet, x = subtitle, start_row = 2L, start_col = 1L)
  wb <- openxlsx2::wb_add_font(
    wb, sheet = sheet, dims = subtitle_dims, name = "Aptos",
    size = 10, italic = TRUE, color = openxlsx2::wb_color(hex = "FF44546A")
  )
  wb <- openxlsx2::wb_add_cell_style(
    wb, sheet = sheet, dims = subtitle_dims,
    horizontal = "left", vertical = "center", wrap_text = TRUE
  )
  wb <- openxlsx2::wb_add_data_table(
    wb, sheet = sheet, x = data, start_row = 4L, start_col = 1L,
    table_name = table_name, table_style = "TableStyleMedium2"
  )
  wb <- openxlsx2::wb_freeze_pane(wb, sheet = sheet, first_active_row = 5L, first_active_col = 5L)
  wb <- openxlsx2::wb_set_row_heights(wb, sheet = sheet, rows = 1L, heights = 28)
  wb <- openxlsx2::wb_set_row_heights(wb, sheet = sheet, rows = 2L, heights = 30)
  if (is.null(widths)) widths <- rep(16, last_col)
  wb <- openxlsx2::wb_set_col_widths(
    wb, sheet = sheet, cols = seq_len(last_col), widths = widths
  )
  wb
}

wb <- add_title(
  wb, "Summary", "All Columns, Filters, and Bins Review",
  paste0(
    "Generated from parser CSV outputs. Review rows preserve inline report context and ",
    "standalone saved-object provenance; editable classification fields start blank."
  ),
  summary_data, "SummaryMetrics", c(38, 14)
)
wb <- openxlsx2::wb_add_data_table(
  wb, sheet = "Summary", x = scope_summary, start_row = 16L, start_col = 1L,
  table_name = "SummaryByScope", table_style = "TableStyleMedium2"
)

review_widths <- c(12, 18, 22, 14, 32, 18, 12, 65, 12, 20, 45, 40,
                   18, 22, 12, 12, 14, 32, 52, 14, 14, 30, 52)
wb <- add_title(
  wb, "All Review", "Combined Review: All Columns, Filters, and Bins",
  "One row per parsed formula, bin branch, or filter criterion. Use the first twelve columns for normalization and review.",
  all_review, "AllReview", review_widths
)
wb <- add_title(
  wb, "Column Review", "All Column Formulas",
  "Includes inline report columns and standalone saved-column definitions that are not bins.",
  all_review[all_review$rule_type == "Column", , drop = FALSE], "ColumnReview", review_widths
)
wb <- add_title(
  wb, "Bin Review", "All Bin Rules",
  "Includes every WHEN and OTHERWISE branch from inline and standalone binned columns.",
  all_review[all_review$rule_type == "Bin", , drop = FALSE], "BinReview", review_widths
)
wb <- add_title(
  wb, "Filter Review", "All Filter Criteria",
  "Includes inline report filters and every criterion from standalone saved-filter definitions.",
  all_review[all_review$rule_type == "Filter", , drop = FALSE], "FilterReview", review_widths
)
wb <- add_title(
  wb, "Columns Inventory", "Complete Column Usage and Definition Inventory",
  "Direct copy of columns.csv: report-embedded columns, saved-column references, and standalone saved-column objects.",
  columns, "ColumnsInventory", rep(18, ncol(columns))
)
wb <- add_title(
  wb, "Filters Inventory", "Complete Filter Usage and Definition Inventory",
  "Direct copy of filters.csv: inline report filters, saved-filter references, and standalone saved-filter objects.",
  filters, "FiltersInventory", rep(18, ncol(filters))
)
wb <- add_title(
  wb, "Filter Values", "Expanded Values for List Filters",
  "Complete value lists for IN and NOT IN filters; join to review rows with rule_id.",
  filter_values, "FilterValues", c(18, 18, 32, 38, 12, 42)
)

# Make the human-maintained review columns visually distinct in every review sheet.
for (sheet in c("All Review", "Column Review", "Bin Review", "Filter Review")) {
  data_rows <- nrow(if (sheet == "All Review") all_review else {
    all_review[all_review$rule_type == sub(" Review$", "", sheet), , drop = FALSE]
  })
  if (data_rows > 0L) {
    editable_dims <- openxlsx2::wb_dims(rows = 5L:(data_rows + 4L), cols = c(3L, 4L, 9L:12L))
    wb <- openxlsx2::wb_add_fill(
      wb, sheet = sheet, dims = editable_dims,
      color = openxlsx2::wb_color(hex = "FFFFF2CC")
    )
  }
}

dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)
openxlsx2::wb_save(wb, file = output_file, overwrite = TRUE)
message("Wrote ", normalizePath(output_file), " (", nrow(all_review), " review rows)")
