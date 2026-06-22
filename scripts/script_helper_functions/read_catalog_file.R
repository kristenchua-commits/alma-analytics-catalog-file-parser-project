# =========================================================
# Helper: Read and clean an Alma Analytics .catalog file
# =========================================================

read_catalog_file <- function(catalog_file) {
  
  raw_data <- readBin(
    catalog_file,
    what = "raw",
    n = file.info(catalog_file)$size
  )
  
  decompressed <- memDecompress(raw_data, type = "unknown")
  
  decompressed_no_null <- decompressed[
    decompressed != as.raw(0)
  ]
  
  all_text_raw <- rawToChar(
    decompressed_no_null,
    multiple = FALSE
  )
  
  clean_text <- iconv(
    all_text_raw,
    from = "UTF-8",
    to = "UTF-8",
    sub = ""
  )
  
  if (is.na(clean_text)) {
    clean_text <- iconv(
      all_text_raw,
      from = "latin1",
      to = "UTF-8",
      sub = ""
    )
  }
  
  Encoding(clean_text) <- "UTF-8"
  
  return(clean_text)
}