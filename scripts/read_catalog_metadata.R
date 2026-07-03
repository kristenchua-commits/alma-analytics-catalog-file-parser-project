read_catalog_metadata <- function(catalog_file, keep_strings = FALSE) {
  if (!file.exists(catalog_file)) {
    stop("Catalog file not found: ", catalog_file)
  }
  
  # -----------------------------
  # Helpers
  # -----------------------------
  decompress_catalog_raw <- function(raw_file) {
    tryCatch(
      memDecompress(raw_file, type = "unknown"),
      error = function(e1) {
        tryCatch(
          memDecompress(raw_file, type = "gzip"),
          error = function(e2) {
            stop("Could not decompress catalog file: ", conditionMessage(e1))
          }
        )
      }
    )
  }
  
  extract_ascii_strings <- function(raw_vec, min_len = 4L) {
    x <- as.integer(raw_vec)
    if (!length(x)) return(character(0))
    
    printable <- (x >= 32L & x <= 126L) | x %in% c(9L, 10L, 13L)
    runs <- rle(printable)
    ends <- cumsum(runs$lengths)
    starts <- ends - runs$lengths + 1L
    
    out <- character(0)
    
    for (i in seq_along(runs$values)) {
      if (!runs$values[i] || runs$lengths[i] < min_len) next
      idx <- starts[i]:ends[i]
      s <- rawToChar(as.raw(x[idx]))
      if (nzchar(trimws(s))) out <- c(out, s)
    }
    
    unique(out)
  }
  
  unescape_json_string <- function(x) {
    x <- gsub('\\\\\"', '"', x, fixed = TRUE)
    x <- gsub('\\\\/', '/', x, fixed = TRUE)
    x <- gsub('\\\\\\\\', '\\\\', x, fixed = TRUE)
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
  
  # -----------------------------
  # Read and decompress the file
  # -----------------------------
  raw_file <- readBin(catalog_file, what = "raw", n = file.info(catalog_file)$size)
  raw_decompressed <- decompress_catalog_raw(raw_file)
  
  # -----------------------------
  # Extract printable strings
  # -----------------------------
  strings <- extract_ascii_strings(raw_decompressed, min_len = 12L)
  
  # Metadata blobs are the long printable strings that contain ItemName
  meta_strings <- strings[grepl('"ItemName"\\s*:', strings, perl = TRUE)]
  meta_strings <- unique(meta_strings)
  
  if (!length(meta_strings)) {
    stop("No metadata records with ItemName found in the catalog file.")
  }
  
  # -----------------------------
  # Parse each metadata blob
  # -----------------------------
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
      item_type = extract_json_number_field(txt, "ItemType"),
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