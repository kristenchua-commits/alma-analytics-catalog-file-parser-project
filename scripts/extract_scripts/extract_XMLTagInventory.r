# =========================================================
# Extract unique XML tags from a .catalog file
# =========================================================

library(stringr)
library(xml2)
library(tibble)
library(readr)
#list.files()

setwd("/Users/kchua/GitHub/alma-analytics-dot-catalog-parser")
source("scripts/script_helper_functions/read_catalog_file.R")

#### Uncomment to specify the name of the catalog file .catalog file
CampusLibraryCategorization_catalog_file <- "Physical Preparation Review - Campus Library Categorization Reports.catalog"
#Filters_catalog_file <- "FiltersAnnualStatistics2025-26.catalog"
#CheckOutTypeandUserGroup_catalog_file <- "Fulfillment Preparation Review Report - Check Out Type and User Group - Campus Categorization Reports.catalog"
#EResourceLibraryCategorization_catalog_file <- "Electronic preparation review reports - Campus EResource Library Categorization Reports.catalog"
#PhysicalCampusLibraryCategorization_catalog_file <- "Physical Preparation Review - Campus Library Categorization Reports.catalog
# Read and clean selected .catalog file
clean_text <- read_catalog_file(campus_library_categorization_catalog_file)

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