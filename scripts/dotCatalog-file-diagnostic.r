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

decompress_catalog_raw <- function(input_path) {
  raw_data <- readBin(input_path, what = "raw", n = file.info(input_path)$size)

  tryCatch(
    memDecompress(raw_data, type = "unknown"),
    error = function(e) memDecompress(raw_data, type = "gzip")
  )
}

find_first_byte <- function(x, value_raw) {
  pos <- which(x == value_raw)
  if (length(pos) == 0) stop("Target byte not found.")
  pos[1]
}

decode_region_from_file <- function(region_raw) {
  tf <- tempfile(fileext = ".bin")
  on.exit(unlink(tf), add = TRUE)
  writeBin(region_raw, tf)

  candidate_encodings <- c("UTF-16LE", "UTF-16BE", "UTF-8", "latin1")

  for (enc in candidate_encodings) {
    txt <- tryCatch(
      paste(readLines(tf, warn = FALSE, encoding = enc), collapse = "\n"),
      error = function(e) NULL
    )

    if (!is.null(txt)) {
      txt2 <- trimws(txt)
      if (startsWith(txt2, "<?xml") || startsWith(txt2, "<")) {
        cat("Decoded using:", enc, "\n")
        return(txt)
      }
    }
  }

  stop("Could not decode the XML region with the candidate encodings.")
}

escape_regex <- function(x) {
  gsub("([][{}()+*^$.|\\\\?])", "\\\\\\1", x, perl = TRUE)
}

extract_first_xml_doc <- function(xml_text, root_name) {
  root_esc <- escape_regex(root_name)

  pat <- paste0("(?s)<\\?xml.*?</", root_esc, "\\s*>")
  m <- regexpr(pat, xml_text, perl = TRUE)

  if (m[1] == -1) {
    stop("Could not extract a single XML document for root: ", root_name)
  }

  regmatches(xml_text, m)[[1]]
}

decompress_catalog <- function(input_path, output_path = NULL) {
  if (!file.exists(input_path)) {
    stop("File does not exist: ", input_path)
  }

  raw_data <- decompress_catalog_raw(input_path)

  start_pos <- find_first_byte(raw_data, as.raw(0x3c))  # "<"
  cat("XML starts at byte:", start_pos, "\n")

  region <- raw_data[start_pos:length(raw_data)]
  xml_text <- decode_region_from_file(region)

  m <- regexec("<([[:alnum:]_.:-]+)(\\s|>)", xml_text, perl = TRUE)
  mm <- regmatches(xml_text, m)[[1]]
  if (length(mm) < 2) stop("Could not find a root opening tag.")

  root_name <- mm[2]
  cat("Root element appears to be:", root_name, "\n")

  xml_clean <- extract_first_xml_doc(xml_text, root_name)

  if (!is.null(output_path) && nzchar(output_path)) {
    writeLines(xml_clean, output_path, useBytes = TRUE)
    cat("Saved clean XML to:", output_path, "\n")
  }

  read_xml(xml_clean)
}

catalog_path <- pick_catalog_file()

output_path <- sub(
  "\\.catalog$",
  ".xml",
  catalog_path,
  ignore.case = TRUE
)

cat("Will save to:", output_path, "\n")

doc <- decompress_catalog(catalog_path, output_path)

xml_structure(doc)

cat("\nUnique tags:\n")
print(unique(xml_name(xml_find_all(doc, ".//*"))))