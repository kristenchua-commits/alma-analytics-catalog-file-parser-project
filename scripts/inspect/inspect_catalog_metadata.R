library(xml2)

pick_catalog_file <- function() {
  path <- system(
    "osascript -e 'POSIX path of (choose file with prompt \"Choose a .catalog file\")'",
    intern = TRUE
  )
  
  if (length(path) == 0 || !nzchar(path)) {
    stop("No file selected.")
  }
  
  path
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
  
  printable <- x >= 32L & x <= 126L
  runs <- rle(printable)
  ends <- cumsum(runs$lengths)
  starts <- ends - runs$lengths + 1L
  
  out <- character(0)
  
  for (i in seq_along(runs$values)) {
    if (!runs$values[i] || runs$lengths[i] < min_len) next
    idx <- starts[i]:ends[i]
    s <- rawToChar(as.raw(x[idx]))
    if (nzchar(s)) out <- c(out, s)
  }
  
  unique(out)
}

extract_utf16_strings <- function(raw_vec, endian = c("le", "be"), min_chars = 4L) {
  endian <- match.arg(endian)
  x <- as.integer(raw_vec)
  n <- length(x)
  if (n < 2L) return(character(0))
  
  out <- character(0)
  
  # Try both byte alignments, since the XML/text may start on either offset.
  for (offset in c(1L, 2L)) {
    if (n < offset + 1L) next
    
    idx <- seq.int(offset, n - 1L, by = 2L)
    if (!length(idx)) next
    
    lo <- x[idx]
    hi <- x[idx + 1L]
    
    valid <- if (endian == "le") {
      hi == 0L & lo >= 32L & lo <= 126L
    } else {
      lo == 0L & hi >= 32L & hi <= 126L
    }
    
    if (!length(valid)) next
    
    runs <- rle(valid)
    ends <- cumsum(runs$lengths)
    starts <- ends - runs$lengths + 1L
    
    for (i in seq_along(runs$values)) {
      if (!runs$values[i] || runs$lengths[i] < min_chars) next
      
      pair_idx <- idx[starts[i]:ends[i]]
      chars <- if (endian == "le") x[pair_idx] else x[pair_idx + 1L]
      
      s <- rawToChar(as.raw(chars))
      if (nzchar(s)) out <- c(out, s)
    }
  }
  
  unique(out)
}

show_keyword_hits <- function(strings, keyword, max_hits = 30L) {
  hits <- strings[grepl(keyword, strings, ignore.case = TRUE)]
  hits <- unique(hits)
  
  cat("\n--- Keyword:", keyword, "---\n")
  if (!length(hits)) {
    cat("No hits\n")
    return(invisible(character(0)))
  }
  
  print(utils::head(hits, max_hits))
  invisible(hits)
}

inspect_xml_blocks <- function(raw_decompressed) {
  start_pat <- charToRaw("<?xml")
  starts <- find_raw_pattern(raw_decompressed, start_pat)
  
  cat("\nXML blocks found:", length(starts), "\n")
  
  if (!length(starts)) {
    return(invisible(NULL))
  }
  
  end_pat <- charToRaw("</sawsf:savedFilterObject>")
  
  for (i in seq_along(starts)) {
    region_raw <- raw_decompressed[starts[i]:length(raw_decompressed)]
    end_hits <- find_raw_pattern(region_raw, end_pat)
    
    cat("\n============================\n")
    cat("XML block", i, "\n")
    cat("============================\n")
    
    if (!length(end_hits)) {
      cat("Could not find end tag for this block.\n")
      next
    }
    
    end_pos <- end_hits[1] + length(end_pat) - 1L
    xml_raw <- region_raw[1:end_pos]
    
    xml_text <- tryCatch(
      rawToChar(xml_raw),
      error = function(e) {
        cat("Could not convert XML raw to text:", conditionMessage(e), "\n")
        return(NULL)
      }
    )
    
    if (is.null(xml_text)) next
    
    doc <- tryCatch(
      read_xml(xml_text),
      error = function(e) {
        cat("Could not parse XML:", conditionMessage(e), "\n")
        return(NULL)
      }
    )
    
    if (is.null(doc)) next
    
    root <- xml_root(doc)
    
    cat("Root node:", xml_name(root), "\n")
    cat("Attributes:\n")
    print(xml_attrs(root))
    
    cat("Child nodes:\n")
    print(xml_name(xml_children(root)))
  }
}

# -------------------------
# Main
# -------------------------

catalog_path <- pick_catalog_file()
cat("Selected file:", catalog_path, "\n")

raw_file <- readBin(catalog_path, what = "raw", n = file.info(catalog_path)$size)
raw_decompressed <- decompress_catalog_raw(raw_file)

cat("\nDecompressed bytes:", length(raw_decompressed), "\n")

ascii_strings <- extract_ascii_strings(raw_decompressed)
utf16le_strings <- extract_utf16_strings(raw_decompressed, endian = "le")
utf16be_strings <- extract_utf16_strings(raw_decompressed, endian = "be")

all_strings <- unique(c(ascii_strings, utf16le_strings, utf16be_strings))
all_strings <- all_strings[nzchar(all_strings)]

cat("\nPrintable strings found:", length(all_strings), "\n")

# Show likely naming / metadata candidates
keywords <- c(
  "UCLA", "UCB", "UCSF", "UCR", "UCD", "UCI", "UCSD", "UCSB", "UCSC", "UCM",
  "NRLF", "SRLF", "BANC", "SPECIALCOL", "SSPEC",
  "Physical", "Fulfillment", "Filter", "Filters",
  "name", "title", "label", "caption",
  "catalog", "subject"
)

for (k in keywords) {
  show_keyword_hits(all_strings, k)
}

# Print the XML block summary too
inspect_xml_blocks(raw_decompressed)

# Optional: write the strings to a text file for manual review
report_path <- file.path(
  dirname(catalog_path),
  paste0(tools::file_path_sans_ext(basename(catalog_path)), "_metadata_strings.txt")
)

writeLines(
  c(
    paste0("Catalog file: ", catalog_path),
    paste0("Decompressed bytes: ", length(raw_decompressed)),
    paste0("Printable strings found: ", length(all_strings)),
    "",
    "=== ALL STRINGS ===",
    all_strings
  ),
  report_path
)

cat("\nWrote report to:", report_path, "\n")