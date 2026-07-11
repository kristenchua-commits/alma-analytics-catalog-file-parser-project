# Extract one metadata row for every XML object returned by read_catalog_file().
read_catalog_metadata <- function(catalog, keep_strings = FALSE) {
  if (is.character(catalog) && length(catalog) == 1L) {
    if (!exists("read_catalog_file", mode = "function")) {
      source("scripts/script_helper_functions/read_catalog_file.R")
    }
    catalog <- read_catalog_file(catalog, keep_strings = TRUE)
  }

  if (!is.data.frame(catalog) || !"xml_root_name" %in% names(catalog)) {
    stop("catalog must be a .catalog path or the output of read_catalog_file().")
  }

  strings <- attr(catalog, "catalog_strings")
  if (is.null(strings) || !length(strings)) {
    stop("Catalog strings are unavailable. Call read_catalog_file(..., keep_strings = TRUE).")
  }

  metadata_strings <- unique(strings[grepl('"ItemName"\\s*:', strings, perl = TRUE)])
  object_metadata <- metadata_strings[
    grepl('"ObjectSignature"\\s*:\\s*"[^"]+"', metadata_strings, perl = TRUE)
  ]

  if (length(object_metadata) != nrow(catalog)) {
    stop(
      "Found ", length(object_metadata), " object metadata records for ",
      nrow(catalog), " XML objects; rows cannot be matched safely."
    )
  }

  unescape_json_string <- function(x) {
    x <- gsub("\\\\/", "/", x, fixed = TRUE)
    x <- gsub('\\\\"', '"', x, fixed = TRUE)
    gsub("\\\\\\\\", "\\\\", x, fixed = TRUE)
  }

  extract_string <- function(text, field) {
    pattern <- paste0('"', field, '"\\s*:\\s*"((?:[^"\\\\]|\\\\.)*)"')
    match <- regexec(pattern, text, perl = TRUE)
    value <- regmatches(text, match)[[1L]]
    if (length(value) < 2L) return(NA_character_)
    unescape_json_string(value[2L])
  }

  extract_number <- function(text, field) {
    pattern <- paste0('"', field, '"\\s*:\\s*([0-9]+(?:\\.[0-9]+)?)')
    match <- regexec(pattern, text, perl = TRUE)
    value <- regmatches(text, match)[[1L]]
    if (length(value) < 2L) return(NA_real_)
    as.numeric(value[2L])
  }

  extract_nested_id <- function(text, field) {
    pattern <- paste0('"', field, '"\\s*:\\s*\\{[^{}]*?"ID"\\s*:\\s*"([^"]+)"')
    match <- regexec(pattern, text, perl = TRUE)
    value <- regmatches(text, match)[[1L]]
    if (length(value) < 2L) return(NA_character_)
    value[2L]
  }

  classify_object <- function(root_name, signature) {
    if (grepl("filter", root_name, ignore.case = TRUE) ||
        grepl("filter", signature, ignore.case = TRUE)) return("filter")
    if (grepl("column", root_name, ignore.case = TRUE) ||
        grepl("column", signature, ignore.case = TRUE)) return("saved_column")
    if (identical(root_name, "dashboardPage")) return("dashboard_page")
    if (identical(root_name, "dashboard")) return("dashboard")
    if (identical(root_name, "report")) return("report")
    "other"
  }

  closest_folder_from_path <- function(path) {
    if (is.na(path) || !nzchar(path)) return(NA_character_)
    path <- sub("/+$", "", gsub("\\\\", "/", path))
    parent_path <- dirname(path)
    if (!nzchar(parent_path) || parent_path %in% c(".", "/")) return(NA_character_)
    basename(parent_path)
  }

  hierarchy_paths <- vapply(
    metadata_strings,
    extract_string,
    character(1),
    field = "OriginalPath"
  )
  hierarchy_titles <- vapply(
    metadata_strings,
    extract_string,
    character(1),
    field = "ItemName"
  )

  rows <- lapply(seq_along(object_metadata), function(i) {
    text <- object_metadata[[i]]
    signature <- extract_string(text, "ObjectSignature")
    root_name <- catalog$xml_root_name[i]
    original_path <- extract_string(text, "OriginalPath")

    data.frame(
      catalog_index = i,
      source_file_name = catalog$source_file_name[i],
      object_title = extract_string(text, "ItemName"),
      object_kind = classify_object(root_name, signature),
      xml_root_name = root_name,
      closest_folder = closest_folder_from_path(original_path),
      original_path = original_path,
      object_signature = signature,
      subject_area = catalog$subject_area[i],
      owner_id = extract_nested_id(text, "OwnerId"),
      creator_id = extract_nested_id(text, "CreatorId"),
      item_type_code = extract_number(text, "ItemType"),
      created_year = extract_number(text, "Year"),
      created_month = extract_number(text, "Month"),
      created_day = extract_number(text, "Day"),
      created_hour = extract_number(text, "Hour"),
      created_minute = extract_number(text, "Minute"),
      created_second = extract_number(text, "Second"),
      web_catalog_build = extract_string(text, "Build"),
      web_catalog_description = extract_string(text, "Desc"),
      stringsAsFactors = FALSE
    )
  })

  out <- do.call(rbind, rows)

  # Match each XML object to the nearest containing catalog item. Containers
  # such as dashboard/report folders exist in catalog metadata even when they
  # do not have their own embedded XML block.
  out$closest_level_object <- vapply(seq_len(nrow(out)), function(i) {
    current_path <- sub("/+$", "", out$original_path[i])
    if (is.na(current_path) || !nzchar(current_path)) return(NA_character_)

    candidate_paths <- sub("/+$", "", hierarchy_paths)
    path_matches <- vapply(candidate_paths, function(candidate_path) {
      !is.na(candidate_path) && nzchar(candidate_path) &&
        startsWith(current_path, paste0(candidate_path, "/"))
    }, logical(1))
    candidates <- which(path_matches)
    if (!length(candidates)) return(NA_character_)

    closest <- candidates[which.max(nchar(candidate_paths[candidates]))]
    hierarchy_titles[closest]
  }, character(1))

  # Keep hierarchy fields adjacent to the object's identifying columns.
  hierarchy_position <- match("closest_folder", names(out))
  out <- out[c(
    names(out)[seq_len(hierarchy_position)],
    "closest_level_object",
    setdiff(names(out)[(hierarchy_position + 1L):ncol(out)], "closest_level_object")
  )]

  if (keep_strings) {
    attr(out, "catalog_strings") <- strings
    attr(out, "metadata_strings") <- object_metadata
  }
  attr(out, "source_path") <- attr(catalog, "source_path")
  out
}
