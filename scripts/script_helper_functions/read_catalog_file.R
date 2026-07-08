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

read_catalog_file <- function(catalog_file, keep_xml = FALSE, keep_strings = TRUE) {
  if (!file.exists(catalog_file)) {
    stop("Catalog file not found: ", catalog_file)
  }
  
  find_raw_pattern <- function(x, pattern_raw, start = 1L) {
    n <- length(x)
    m <- length(pattern_raw)
    
    if (m == 0 || n < m) return(integer(0))
    
    hits <- integer(0)
    last <- n - m + 1L
    
    for (i in seq.int(start, last)) {
      if (all(x[i:(i + m - 1L)] == pattern_raw)) {
        hits <- c(hits, i)
      }
    }
    
    hits
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
  
  xml_start_pat <- charToRaw("<?xml")
  meta_start_pat <- charToRaw('{"ACL"')
  
  starts <- find_raw_pattern(raw_decompressed, xml_start_pat)
  
  if (!length(starts)) {
    stop("No XML found inside catalog file.")
  }
  
  rows <- vector("list", length(starts))
  xml_docs <- vector("list", length(starts))
  
  for (i in seq_along(starts)) {
    start_pos <- starts[i]
    
    next_xml_start <- if (i < length(starts)) starts[i + 1L] else length(raw_decompressed) + 1L
    meta_after <- find_raw_pattern(raw_decompressed, meta_start_pat, start = start_pos + length(xml_start_pat))
    
    end_pos <- next_xml_start - 1L
    if (length(meta_after)) {
      end_pos <- min(end_pos, meta_after[1L] - 1L)
    }
    
    if (end_pos < start_pos) {
      next
    }
    
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
      subject_area = subject_area,
      xml_text = xml_text
    )
  }
  
  out <- data.frame(
    catalog_index = vapply(rows, `[[`, integer(1), "catalog_index"),
    source_file_name = vapply(rows, `[[`, character(1), "source_file_name"),
    xml_root_name = vapply(rows, `[[`, character(1), "xml_root_name"),
    subject_area = vapply(rows, `[[`, character(1), "subject_area"),
    stringsAsFactors = FALSE
  )
  
  out$xml_text <- vapply(rows, `[[`, character(1), "xml_text")
  
  if (keep_xml) {
    out$xml <- I(xml_docs)
  }
  
  if (keep_strings) {
    attr(out, "catalog_strings") <- extract_ascii_strings(raw_decompressed, min_len = 8L)
  }
  
  attr(out, "source_file_name") <- basename(catalog_file)
  attr(out, "source_path") <- catalog_file
  
  out
}