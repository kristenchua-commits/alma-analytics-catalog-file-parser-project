decode_xml_raw <- function(xml_raw) {
  tf <- tempfile(fileext = ".bin")
  on.exit(unlink(tf), add = TRUE)
  writeBin(xml_raw, tf)
  
  candidate_encodings <- c("UTF-8", "UTF-16LE", "UTF-16BE", "latin1")
  
  for (enc in candidate_encodings) {
    txt <- tryCatch(
      paste(readLines(tf, warn = FALSE, encoding = enc), collapse = "\n"),
      error = function(e) NULL
    )
    
    if (!is.null(txt)) {
      txt2 <- trimws(txt)
      if (startsWith(txt2, "<?xml") || startsWith(txt2, "<")) {
        return(txt)
      }
    }
  }
  
  stop("Could not decode XML chunk.")
}

read_catalog_file <- function(catalog_file, keep_xml = FALSE, keep_strings = TRUE,
                              progress = interactive()) {
  if (!requireNamespace("xml2", quietly = TRUE)) {
    stop("Package 'xml2' is required.")
  }
  if (!file.exists(catalog_file)) {
    stop("Catalog file not found: ", catalog_file)
  }
  
  find_raw_pattern <- function(x, pattern_raw, start = 1L) {
    n <- length(x)
    m <- length(pattern_raw)
    
    if (m == 0 || n < m) return(integer(0))
    
    last <- n - m + 1L

    if (start > last) return(integer(0))

    # Compare each byte of the pattern across the candidate range using
    # vectorized raw operations. This avoids an R-level loop over every byte.
    candidates <- seq.int(start, last)
    matches <- x[candidates] == pattern_raw[1L]

    if (m > 1L) {
      for (j in 2:m) {
        matches <- matches & x[candidates + j - 1L] == pattern_raw[j]
      }
    }

    candidates[matches]
  }
  
  extract_ascii_strings <- function(raw_vec, min_len = 8L) {
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

  extract_item_name <- function(value) {
    if (!length(value)) return(NA_character_)

    if (is.raw(value)) {
      # Metadata records are followed by binary separator bytes. Stop at the
      # first separator before converting the JSON record to text.
      byte_values <- as.integer(value)
      separators <- which(byte_values < 32L & !byte_values %in% c(9L, 10L, 13L))
      if (length(separators)) {
        if (separators[1L] == 1L) return(NA_character_)
        value <- value[seq_len(separators[1L] - 1L)]
      }
      metadata_text <- rawToChar(value)
    } else {
      metadata_text <- value
    }
    match <- regexec(
      '"ItemName"\\s*:\\s*"((?:[^"\\\\]|\\\\.)*)"',
      metadata_text,
      perl = TRUE
    )
    value <- regmatches(metadata_text, match)[[1L]]
    if (length(value) < 2L) return(NA_character_)

    title <- value[2L]
    title <- gsub("\\\\/", "/", title, fixed = TRUE)
    title <- gsub('\\\\"', '"', title, fixed = TRUE)
    gsub("\\\\\\\\", "\\\\", title, fixed = TRUE)
  }
  
  raw_file <- readBin(catalog_file, what = "raw", n = file.info(catalog_file)$size)
  
  raw_decompressed <- tryCatch(
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

  catalog_strings <- extract_ascii_strings(raw_decompressed, min_len = 8L)
  metadata_strings <- catalog_strings[
    grepl('"ItemName"\\s*:', catalog_strings, perl = TRUE)
  ]
  object_metadata <- metadata_strings[
    grepl('"ObjectSignature"\\s*:\\s*"[^"]+"', metadata_strings, perl = TRUE)
  ]
  object_titles <- vapply(object_metadata, extract_item_name, character(1))
  
  xml_start_pat <- charToRaw("<?xml")
  meta_start_pat <- charToRaw('{"ACL"')
  
  # Find each boundary once. The previous implementation searched from every
  # XML document to the end of the file, causing quadratic runtime.
  starts <- find_raw_pattern(raw_decompressed, xml_start_pat)
  metadata_starts <- find_raw_pattern(raw_decompressed, meta_start_pat)
  
  if (!length(starts)) {
    stop("No XML found inside catalog file.")
  }

  if (length(object_titles) != length(starts)) {
    stop(
      "Found ", length(starts), " XML documents but ", length(object_titles),
      " object titles; catalog metadata could not be matched safely."
    )
  }
  
  rows <- vector("list", length(starts))
  xml_docs <- vector("list", length(starts))

  if (progress) {
    message("Found ", length(starts), " XML documents; parsing...")
    progress_bar <- utils::txtProgressBar(min = 0L, max = length(starts), style = 3L)
    on.exit(close(progress_bar), add = TRUE)
  }
  
  for (i in seq_along(starts)) {
    start_pos <- starts[i]
    
    next_xml_start <- if (i < length(starts)) starts[i + 1L] else length(raw_decompressed) + 1L
    metadata_index <- findInterval(
      start_pos + length(xml_start_pat) - 1L,
      metadata_starts
    ) + 1L
    meta_after <- if (metadata_index <= length(metadata_starts)) {
      metadata_starts[metadata_index]
    } else {
      integer(0)
    }

    end_pos <- next_xml_start - 1L
    if (length(meta_after)) {
      end_pos <- min(end_pos, meta_after[1L] - 1L)
    }
    
    if (end_pos < start_pos) {
      next
    }

    # Catalog records can include separator/control bytes between the closing
    # XML tag and their metadata. Exclude those bytes from the XML parser.
    closing_brackets <- which(
      raw_decompressed[start_pos:end_pos] == charToRaw(">")
    )
    if (!length(closing_brackets)) {
      stop("XML document ", i, " has no closing tag.")
    }
    end_pos <- start_pos + closing_brackets[length(closing_brackets)] - 1L
    
    xml_raw <- raw_decompressed[start_pos:end_pos]
    xml_text <- decode_xml_raw(xml_raw)
    
    doc <- xml2::read_xml(xml_text)
    root <- xml2::xml_root(doc)
    
    xml_docs[[i]] <- doc
    
    subject_area <- xml2::xml_attr(root, "subjectArea")
    if (!is.na(subject_area) && nzchar(subject_area)) {
      subject_area <- gsub('^"|"$', "", subject_area)
    }
    
    rows[[i]] <- list(
      catalog_index = i,
      source_file_name = basename(catalog_file),
      xml_root_name = xml2::xml_name(root),
      object_title = object_titles[i],
      subject_area = subject_area,
      xml_text = xml_text
    )

    if (progress) utils::setTxtProgressBar(progress_bar, i)
  }
  
  out <- data.frame(
    catalog_index = vapply(rows, `[[`, integer(1), "catalog_index"),
    source_file_name = vapply(rows, `[[`, character(1), "source_file_name"),
    xml_root_name = vapply(rows, `[[`, character(1), "xml_root_name"),
    object_title = vapply(rows, `[[`, character(1), "object_title"),
    subject_area = vapply(rows, `[[`, character(1), "subject_area"),
    stringsAsFactors = FALSE
  )
  
  out$xml_text <- vapply(rows, `[[`, character(1), "xml_text")
  
  if (keep_xml) {
    out$xml <- I(xml_docs)
  }
  
  if (keep_strings) {
    attr(out, "catalog_strings") <- catalog_strings
  }
  
  attr(out, "source_file_name") <- basename(catalog_file)
  attr(out, "source_path") <- catalog_file
  
  out
}
