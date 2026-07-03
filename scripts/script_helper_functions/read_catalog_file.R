read_catalog_file <- function(catalog_file, keep_xml = FALSE, keep_strings = TRUE) {
  if (!file.exists(catalog_file)) {
    stop("Catalog file not found: ", catalog_file)
  }
  
  # -------------------------------------------------
  # Small helpers
  # -------------------------------------------------
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
  
  extract_catalog_strings <- function(path) {
    strings_bin <- Sys.which("strings")
    if (!nzchar(strings_bin)) {
      warning("'strings' command not found; catalog_strings will be empty.")
      return(character(0))
    }
    
    out <- tryCatch(
      system2("strings", path, stdout = TRUE, stderr = FALSE),
      error = function(e) character(0)
    )
    
    out[nzchar(out)]
  }
  
  # -------------------------------------------------
  # Read and decompress file
  # -------------------------------------------------
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
  
  # -------------------------------------------------
  # Find XML blocks
  # -------------------------------------------------
  start_pat <- charToRaw("<?xml")
  starts <- find_raw_pattern(raw_decompressed, start_pat)
  
  if (!length(starts)) {
    stop("No XML found inside catalog file.")
  }
  
  rows <- vector("list", length(starts))
  xml_docs <- vector("list", length(starts))
  
  for (i in seq_along(starts)) {
    region_raw <- raw_decompressed[starts[i]:length(raw_decompressed)]
    
    end_pat <- charToRaw("</sawsf:savedFilterObject>")
    end_hits <- find_raw_pattern(region_raw, end_pat)
    
    if (!length(end_hits)) {
      stop("Could not find end tag for XML block ", i, ".")
    }
    
    end_pos <- end_hits[1] + length(end_pat) - 1L
    xml_raw <- region_raw[1:end_pos]
    
    # This should now be a plain XML text fragment
    xml_text <- rawToChar(xml_raw)
    
    doc <- xml2::read_xml(xml_text)
    root <- xml2::xml_root(doc)
    
    xml_docs[[i]] <- doc
    
    rows[[i]] <- list(
      catalog_index = i,
      source_file_name = basename(catalog_file),
      root_name = xml2::xml_name(root),
      subject_area = xml2::xml_attr(root, "subjectArea")
    )
  }
  
  out <- data.frame(
    catalog_index = vapply(rows, `[[`, integer(1), "catalog_index"),
    source_file_name = vapply(rows, `[[`, character(1), "source_file_name"),
    root_name = vapply(rows, `[[`, character(1), "root_name"),
    subject_area = vapply(rows, `[[`, character(1), "subject_area"),
    stringsAsFactors = FALSE
  )
  
  if (keep_xml) {
    out$xml <- I(xml_docs)
  }
  
  if (keep_strings) {
    attr(out, "catalog_strings") <- extract_catalog_strings(catalog_file)
  }
  
  attr(out, "source_file_name") <- basename(catalog_file)
  attr(out, "source_path") <- catalog_file
  
  out
}