# =========================================================
# Extract unique XML tags from a .catalog file
# =========================================================

library(stringr)
library(xml2)
library(tibble)
library(readr)

source("scripts/utils/read_catalog_file.R")

# Pick .catalog file
catalog_file <- file.choose()

# Read and clean .catalog file
clean_text <- read_catalog_file(catalog_file)

# Extract XML blocks from the catalog text
xml_blocks <- str_extract_all(
  clean_text,
  '(?s)<\\?xml.*?(?=<\\?xml|$)'
)[[1]]

# Helper: safely read XML block
read_xml_block <- function(xml_block) {
  tryCatch(
    read_xml(xml_block),
    error = function(e) NULL
  )
}

# Helper: extract unique tags from one XML block
extract_xml_tags <- function(xml_block, block_number) {
  doc <- read_xml_block(xml_block)
  
  if (is.null(doc)) {
    return(tibble(
      xml_block_number = block_number,
      tag_name = NA_character_,
      status = "could_not_parse"
    ))
  }
  
  tags <- unique(xml_name(xml_find_all(doc, ".//*")))
  
  tibble(
    xml_block_number = block_number,
    tag_name = tags,
    status = "parsed"
  )
}

# Extract tags from all XML blocks
tag_list <- lapply(
  seq_along(xml_blocks),
  function(i) extract_xml_tags(xml_blocks[[i]], i)
)

tag_sheet <- dplyr::bind_rows(tag_list)

# Save output
write_csv(
  tag_sheet,
  "ALMA_Analytics_XML_Tags_from_selected_catalog_file.csv"
)

# Print results
cat("Saved: ALMA_Analytics_XML_Tags_from_selected_catalog_file.csv\n\n")
print(tag_sheet, n = 100)