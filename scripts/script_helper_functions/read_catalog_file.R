# =========================================================
# Helper: Read and clean an Alma Analytics .catalog file
# =========================================================

read_catalog_file <- function(catalog_file) {
  
  if (!file.exists(catalog_file)) {
    stop("Catalog file not found: ", catalog_file)
  }
  
  # Existing code that reads the catalog
  # For example:
  catalog <- xml2::read_xml(catalog_file)
  
  return(catalog)
}