project_root <- normalizePath(file.path(testthat::test_path(), "..", ".."), mustWork = TRUE)
source(file.path(project_root, "R", "extract", "report_xml_helpers.R"))
source(file.path(project_root, "R", "extract", "extract_columns.R"))

condition_node <- function(expression_type, operator, values, value_types) {
  child_xml <- paste0(
    "<sawx:expr xsi:type='", value_types, "'>", values, "</sawx:expr>",
    collapse = ""
  )
  xml2::read_xml(paste0(
    "<condition xmlns:sawx='com.siebel.analytics.web/expression/v1.1' ",
    "xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>",
    "<sawx:expr xsi:type='", expression_type, "' op='", operator, "'>",
    child_xml,
    "</sawx:expr></condition>"
  ))
}

testthat::test_that("saved-column XML operators are rendered as SQL", {
  field <- "&quot;Borrower Details&quot;.&quot;User Group&quot;"

  equality <- condition_node(
    "sawx:comparison",
    "equal",
    c(field, "UCM Faculty"),
    c("sawx:sqlExpression", "xsd:string")
  )
  numeric_equality <- condition_node(
    "sawx:comparison",
    "equal",
    c("institution_id", "6532"),
    c("sawx:sqlExpression", "xsd:decimal")
  )
  null_check <- condition_node(
    "sawx:comparison",
    "null",
    field,
    "sawx:sqlExpression"
  )
  in_list <- condition_node(
    "sawx:list",
    "in",
    c(field, "Library Staff", "UCM Staff"),
    c("sawx:sqlExpression", "xsd:string", "xsd:string")
  )
  prefixes <- condition_node(
    "sawx:list",
    "beginsWith",
    c("location_code", "ah", "dl"),
    c("sawx:sqlExpression", "xsd:string", "xsd:string")
  )
  suffix <- condition_node(
    "sawx:list",
    "endsWith",
    c("location_code", "zw"),
    c("sawx:sqlExpression", "xsd:string")
  )

  testthat::expect_identical(
    format_column_condition(equality),
    "\"Borrower Details\".\"User Group\" = 'UCM Faculty'"
  )
  testthat::expect_identical(
    format_column_condition(numeric_equality),
    "institution_id = 6532"
  )
  testthat::expect_identical(
    format_column_condition(null_check),
    "\"Borrower Details\".\"User Group\" IS NULL"
  )
  testthat::expect_identical(
    format_column_condition(in_list),
    "\"Borrower Details\".\"User Group\" IN ('Library Staff', 'UCM Staff')"
  )
  testthat::expect_identical(
    format_column_condition(prefixes),
    "(location_code LIKE 'ah%' OR location_code LIKE 'dl%')"
  )
  testthat::expect_identical(
    format_column_condition(suffix),
    "location_code LIKE '%zw'"
  )
})
