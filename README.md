# Alma Analytics catalog file parser

R utilities for extracting and reviewing objects embedded in Ex Libris Alma
Analytics `.catalog` files. The pipeline preserves full XML in RDS files and
writes spreadsheet-friendly CSV files without the large `xml_text` column.

For a GitHub-rendered notebook walkthrough with annotations, R code cells, and
saved example outputs, open `alma_analytics_catalog_file_parser.ipynb`.

## Requirements

- R 4.1 or newer
- The R packages `xml2` and `writexl`

Install the packages once with `install.packages(c("xml2", "writexl"))`.

## Run the complete pipeline

From the repository root:

```sh
Rscript scripts/run_pipeline.R "data/Annual Stats FY 2025-2026.catalog"
```

### Choose a file interactively in RStudio

The wrapper searches both `data/` in this repository and your `~/Downloads`
folder. It also provides a file-browser option for files stored elsewhere.

```r
source("choose_catalog_file_and_run_pipeline.R")
catalog <- run_catalog_pipeline()
```

## Pipeline map

![Alma Analytics catalog parser pipeline showing every processing script, input, intermediate dataset, and output](output/run_pipeline_diagram.png)

The arrows show the direction of processing. Blue identifies the original
input, green identifies processing scripts, yellow identifies intermediate
datasets, purple identifies human-facing review outputs, and gray identifies
inspection or detailed technical outputs.

`choose_catalog_file_and_run_pipeline.R` is an optional interactive entry point.
It selects a `.catalog` file and passes it to `scripts/run_pipeline.R`, the
orchestrator that sources and calls the remaining scripts in dependency order.

| Stage | Script(s) | Reads | Writes |
| --- | --- | --- | --- |
| Input selection and orchestration | `choose_catalog_file_and_run_pipeline.R`; `scripts/run_pipeline.R` | Raw Alma Analytics `.catalog` file | Starts shared extraction |
| Shared catalog extraction | `scripts/extract_scripts/extract_XMLFileList.R`; `scripts/script_helper_functions/read_catalog_file.R`; `scripts/script_helper_functions/read_catalog_metadata.R` | `.catalog` file | `catalog_extract.rds`; `catalog_extract_summary.csv` |
| Metadata and XML-tag inspection | `scripts/inventory/inspect_catalog_metadata.R`; `scripts/extract_scripts/extract_XMLTagInventory.r` | `catalog_extract.rds` | `catalog_metadata_inventory.csv`; `xml_tag_inventory.csv` |
| Saved-column parsing and review | `scripts/extract_scripts/extract_SavedColumn.R`; `scripts/extract_scripts/export_saved_column_review.R` | Saved-column rows in `catalog_extract.rds` | `saved_columns.csv`; `saved_column_review.xlsx`; `saved_column_review.csv` |
| Filter-object selection | `scripts/extract_scripts/extract_XML_to_FilterObject.r` | Filter rows in `catalog_extract.rds` | `filter_objects.rds`; `filter_objects_summary.csv` |
| Detailed filter criteria | `scripts/extract_scripts/export_filter_criteria.R` | `filter_objects.rds` | `filter_criteria.csv` |
| Filter review | `scripts/extract_scripts/export_filter_review.R` | `filter_objects.rds` | `filter_review.xlsx`; `filter_review.csv`; `filter_review_value_lists.csv` |

### Shared extraction

The first stage always extracts the XML objects and aligns them with their
catalog metadata:

```sh
Rscript scripts/extract_scripts/extract_XMLFileList.R "data/example.catalog"
```

Creates:

- `output/catalog_extract.rds`: complete object metadata and XML
- `output/catalog_extract_summary.csv`: metadata only, suitable for spreadsheets

`read_catalog_file()` returns one row per XML object. `read_catalog_metadata()`
aligns object name, path, kind, signature, ownership, and timestamps to those
rows. Folder-only catalog metadata is intentionally excluded.

`catalog_extract.rds` is the shared foundation for every downstream branch.
Reports, dashboards, dashboard pages, and unrecognized objects remain available
in this file and in `catalog_extract_summary.csv`, even though they do not have
separate exporters.

### Inspection branch

```sh
Rscript scripts/inventory/inspect_catalog_metadata.R
Rscript scripts/extract_scripts/extract_XMLTagInventory.r
```

These scripts read `catalog_extract.rds` and create the metadata-pattern and XML
tag inventories.

### Saved-column branch

When the catalog contains `saved_column` objects, the orchestrator runs:

```sh
Rscript scripts/extract_scripts/extract_SavedColumn.R
Rscript scripts/extract_scripts/export_saved_column_review.R
```

The first script creates the detailed saved-column export. The review exporter
turns formulas and XML `when`, `condition`, `value`, and `otherwise` elements
into readable criteria, labels, and rule IDs.

### Filter branch

When the catalog contains `filter` objects, the orchestrator runs:

```sh
Rscript scripts/extract_scripts/extract_XML_to_FilterObject.r
Rscript scripts/extract_scripts/export_filter_criteria.R
Rscript scripts/extract_scripts/export_filter_review.R
```

The first script creates `filter_objects.rds`, the specialized intermediate
dataset used independently by both exporters. `filter_criteria.csv` contains one
row per expression-tree node, with parent/depth fields that preserve logical
structure. The review exporter creates concise business-rule rows and retains
large `IN`/`NOT IN` lists in a companion CSV.

The saved-column and filter branches are conditional: a branch is skipped when
the corresponding object type is absent.

## Outputs

| File | Purpose |
| --- | --- |
| `catalog_extract.rds` | Complete object data including raw XML |
| `catalog_extract_summary.csv` | Object names, paths, kinds, and metadata |
| `catalog_metadata_inventory.csv` | Counts and missing-field checks by object pattern |
| `xml_tag_inventory.csv` | XML tags, paths, depths, attributes, and values |
| `saved_columns.csv` | Parsed saved-column definitions |
| `saved_column_review.xlsx` | Readable bin criteria, labels, and explanation fields |
| `saved_column_review.csv` | CSV version of the saved-column review rows and rule IDs |
| `filter_objects.rds` | Full filter objects including XML |
| `filter_objects_summary.csv` | Spreadsheet-friendly filter object metadata |
| `filter_criteria.csv` | Flattened filter expressions and criteria |
| `filter_review.xlsx` | Concise documentation workbook with one row per business rule |
| `filter_review.csv` | CSV version of the primary filter-review rows |
| `filter_review_value_lists.csv` | Individual values from large `IN`/`NOT IN` lists |

Generated outputs are ignored by Git. Keep representative fixtures only when
they are deliberately needed for tests or documentation.

## Recommended repository cleanup

After reviewing any local changes, delete or untrack:

- `.DS_Store` files throughout the repository
- `.RData`, `.Rhistory`, and `.Rproj.user/` IDE state
- generated `analysis.html` and `analysis_files/`
- generated files under `output/`
- duplicate catalog copies under multiple `data/` subdirectories
- the old Google Drive workflow reference once its useful source material has
  been migrated into this repository

Do not delete the `.catalog` test fixtures until a smaller, non-sensitive test
fixture is available.
