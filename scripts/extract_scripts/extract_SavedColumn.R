# =========================================================
# Create saved column spreadsheet from .catalog
# One row per saved column
# =========================================================

#Open the alma-analytics-dog-catalog-parser 
#RStudio will automatically set the working directory to the project root. 


setwd("/Users/kchua/GitHub/alma-analytics-dot-catalog-parser")

library(stringr)
library(xml2)
library(dplyr)
library(tibble)
library(readr)
library(writexl)


#choose file from data > catalog file folder
catalog_file <- file.choose()

source("scripts/extract_scripts/extract_SavedColumn.R")

clean_text <- read_catalog_file(catalog_file)

if (is.na(clean_text)) {
  clean_text <- iconv(
    all_text_raw,
    from = "latin1",
    to = "UTF-8",
    sub = ""
  )
}

Encoding(clean_text) <- "UTF-8"

# =========================================================
# Extract XML blocks and saved column names
# =========================================================

xml_blocks <- str_extract_all(
  clean_text,
  '(?s)<\\?xml version="1\\.0" encoding="utf-8"\\?>.*?</sawsc:savedColumnObject>'
)[[1]]

column_names <- str_extract_all(
  clean_text,
  '"ItemName"\\s*:\\s*"[^"]+"'
)[[1]]

column_names <- str_replace_all(
  column_names,
  '"ItemName"\\s*:\\s*"|"$',
  ""
)

# =========================================================
# Helper functions
# =========================================================

clean_expr <- function(x) {
  x %>%
    str_replace_all("&quot;", '"') %>%
    str_replace_all("&#39;", "'") %>%
    str_replace_all("&apos;", "'") %>%
    str_replace_all("&lt;", "<") %>%
    str_replace_all("&gt;", ">") %>%
    str_replace_all("&amp;", "&") %>%
    str_squish()
}

# =========================================================
# Parse saved columns
# =========================================================

results <- list()

for (i in seq_along(xml_blocks)) {
  
  tmp_result <- tryCatch({
    
    doc <- read_xml(xml_blocks[[i]])
    
    saved_column_node <- xml_find_first(
      doc,
      ".//*[local-name()='savedColumnObject']"
    )
    
    subject_area <- xml_attr(saved_column_node, "subjectArea")
    subject_area <- clean_expr(subject_area)
    
    column_heading <- xml_find_all(
      doc,
      ".//*[local-name()='columnHeading'] | .//*[local-name()='caption']"
    ) %>%
      xml_text() %>%
      clean_expr() %>%
      unique() %>%
      paste(collapse = " | ")
    
    formula <- xml_find_all(
      doc,
      ".//*[local-name()='expr'] | .//*[local-name()='formula']"
    ) %>%
      xml_text() %>%
      clean_expr() %>%
      unique() %>%
      paste(collapse = " | ")
    
    full_saved_column_text <- xml_text(saved_column_node) %>%
      clean_expr()
    
    tibble(
      saved_column_number = i,
      saved_column_name = ifelse(i <= length(column_names), column_names[i], NA_character_),
      subject_area = subject_area,
      column_heading = column_heading,
      formula = formula,
      full_saved_column_text = full_saved_column_text
    )
    
  }, error = function(e) {
    message("Skipping column ", i, ": ", e$message)
    NULL
  })
  
  if (!is.null(tmp_result)) {
    results[[length(results) + 1]] <- tmp_result
  }
}

column_sheet <- bind_rows(results)

# =========================================================
# Save spreadsheet
# =========================================================

write_csv(
  column_sheet,
  "ALMA_Analytics_Saved_Columns_from_selected_catalog_file.csv"
)

write_xlsx(
  column_sheet,
  "ALMA_Analytics_Saved_Columns_from_selected_catalog_file.xlsx"
)

cat("ALMA_Analytics_Saved_Columns_from_selected_catalog_file.csv\n")
cat("ALMA_Analytics_Saved_Columns_from_selected_catalog_file.xlsx\n")

print(column_sheet, n = 100)