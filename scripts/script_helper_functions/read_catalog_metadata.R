read_catalog_metadata <- function(catalog_file, keep_strings = FALSE) {
  if (!exists("read_catalog_file")) {
    source("scripts/script_helper_functions/read_catalog_file.R")
  }
  
  catalog <- read_catalog_file(catalog_file, keep_xml = FALSE, keep_strings = TRUE)
  
  strings <- attr(catalog, "catalog_strings")
  if (is.null(strings) || !length(strings)) {
    stop("No printable strings found in the catalog file.")
  }
  
  meta_strings <- strings[grepl('"ItemName"\\s*:', strings, perl = TRUE)]
  meta_strings <- unique(meta_strings)
  
  if (!length(meta_strings)) {
    stop("No metadata records with ItemName found in the catalog file.")
  }
  
  unescape_json_string <- function(x) {
    x <- gsub("\\\\\\\\", "\\\\", x, perl = TRUE)
    x <- gsub("\\\\\"", "\"", x, perl = TRUE)
    x <- gsub("\\\\/", "/", x, perl = TRUE)
    x
  }
  
  extract_json_string_field <- function(txt, field) {
    pat <- paste0('"', field, '"\\s*:\\s*"((?:[^"\\\\]|\\\\.)*)"')
    m <- regexec(pat, txt, perl = TRUE)
    hit <- regmatches(txt, m)[[1]]
    
    if (length(hit) < 2) return(NA_character_)
    unescape_json_string(hit[2])
  }
  
  extract_json_number_field <- function(txt, field) {
    pat <- paste0('"', field, '"\\s*:\\s*([0-9]+(?:\\.[0-9]+)?)')
    m <- regexec(pat, txt, perl = TRUE)
    hit <- regmatches(txt, m)[[1]]
    
    if (length(hit) < 2) return(NA_real_)
    as.numeric(hit[2])
  }
  
  extract_json_nested_id <- function(txt, field) {
    pat <- paste0('"', field, '"\\s*:\\s*\\{[^\\{\\}]*?"ID"\\s*:\\s*"([^"]+)"')
    m <- regexec(pat, txt, perl = TRUE)
    hit <- regmatches(txt, m)[[1]]
    
    if (length(hit) < 2) return(NA_character_)
    hit[2]
  }
  
  rows <- lapply(seq_along(meta_strings), function(i) {
    txt <- meta_strings[[i]]
    
    data.frame(
      catalog_index = i,
      source_file_name = basename(catalog_file),
      item_name = extract_json_string_field(txt, "ItemName"),
      original_path = extract_json_string_field(txt, "OriginalPath"),
      object_signature = extract_json_string_field(txt, "ObjectSignature"),
      owner_id = extract_json_nested_id(txt, "OwnerId"),
      creator_id = extract_json_nested_id(txt, "CreatorId"),
      item_type_code = extract_json_number_field(txt, "ItemType"),
      created_year = extract_json_number_field(txt, "Year"),
      created_month = extract_json_number_field(txt, "Month"),
      created_day = extract_json_number_field(txt, "Day"),
      created_hour = extract_json_number_field(txt, "Hour"),
      created_minute = extract_json_number_field(txt, "Minute"),
      created_second = extract_json_number_field(txt, "Second"),
      wc_build = extract_json_string_field(txt, "Build"),
      wc_desc = extract_json_string_field(txt, "Desc"),
      stringsAsFactors = FALSE
    )
  })
  
  out <- do.call(rbind, rows)
  
  if (keep_strings) {
    attr(out, "catalog_strings") <- strings
    attr(out, "metadata_strings") <- meta_strings
  }
  
  attr(out, "source_path") <- catalog_file
  attr(out, "source_file_name") <- basename(catalog_file)
  
  out
}