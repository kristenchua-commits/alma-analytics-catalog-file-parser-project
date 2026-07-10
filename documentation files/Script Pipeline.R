# install.packages("DiagrammeR")
library(DiagrammeR)

grViz("
digraph alma_pipeline {
  graph [rankdir = LR, splines = ortho, nodesep = 0.45, ranksep = 0.8]
  node  [shape = box, style = rounded, fontname = Helvetica, fontsize = 11]
  edge  [color = black, arrowsize = 0.8]

  catalog [label = 'Raw .catalog file\\n(input)']

  subgraph cluster_read {
    label = '1. Read and normalize'
    color = gray70
    style = rounded

    read_file [label = 'read_catalog_file.R\\n- decompress catalog\\n- find XML blocks\\n- decode XML text']
    read_meta [label = 'read_catalog_metadata.R\\n- uses read_catalog_file()\\n- extracts metadata fields']
    inspect_meta [label = 'inspect_catalog_metadata.R\\n- diagnostics / string search\\n- XML block summary']
  }

  subgraph cluster_extract {
    label = '2. Extract catalog objects'
    color = gray70
    style = rounded

    xmlfilelist [label = 'extract_XMLFileList.R\\n- select .catalog file\\n- align metadata + XML\\n- classify Filter / Column / Other']
    catalog_rds [label = 'output/catalog_extract.rds']
    catalog_csv [label = 'output/catalog_extract_summary.csv']
    filter_rds [label = 'output/filter_objects.rds']
    column_rds [label = 'output/column_objects.rds']
  }

  subgraph cluster_filter {
    label = '3. Traverse XML trees'
    color = gray70
    style = rounded

    export_filter_obj [label = 'export_XML_to_FilterObject.R\\n- keep Filter objects\\n- add filter_object_index']
    saved_columns [label = 'extract_SavedColumn.R\\n- read XML tree\\n- find <columns><column>\\n- extract saved column details']
    filter_criteria [label = 'export_filter_criteria.R\\n- read filter XML\\n- recurse through expr tree\\n- flatten criteria rows']
  }

  subgraph cluster_output {
    label = '4. Export outputs'
    color = gray70
    style = rounded

    filter_obj_csv [label = 'output/filter_objects.csv']
    saved_csv [label = 'output/saved_columns.csv']
    criteria_csv [label = 'output/filter_criteria.csv']
  }

  tag_inventory [label = 'extract_XMLTagInventory.R\\n- split XML blocks\\n- parse each block\\n- inventory unique XML tags']

  catalog -> read_file
  read_file -> read_meta
  read_file -> inspect_meta
  read_file -> xmlfilelist
  read_file -> tag_inventory

  xmlfilelist -> catalog_rds
  xmlfilelist -> catalog_csv
  xmlfilelist -> filter_rds
  xmlfilelist -> column_rds

  filter_rds -> export_filter_obj
  catalog_rds -> export_filter_obj
  export_filter_obj -> filter_obj_csv

  catalog_rds -> saved_columns
  saved_columns -> saved_csv

  filter_rds -> filter_criteria
  filter_criteria -> criteria_csv
}
")