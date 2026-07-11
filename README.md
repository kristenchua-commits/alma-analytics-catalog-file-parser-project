# Alma Analytics catalog file parser

R utilities for extracting and reviewing objects embedded in Ex Libris Alma
Analytics `.catalog` files. The pipeline preserves full XML in RDS files and
writes spreadsheet-friendly CSV files without the large `xml_text` column.

## Requirements

- R 4.1 or newer
- The R package `xml2`

Install the package once with `install.packages("xml2")`.

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

The pipeline has three stages.

### 1. Extract XML objects and metadata

```sh
Rscript scripts/extract_scripts/extract_XMLFileList.R "data/example.catalog"
```

Creates:

- `output/catalog_extract.rds`: complete object metadata and XML
- `output/catalog_extract_summary.csv`: metadata only, suitable for spreadsheets

`read_catalog_file()` returns one row per XML object. `read_catalog_metadata()`
aligns object name, path, kind, signature, ownership, and timestamps to those
rows. Folder-only catalog metadata is intentionally excluded.

### 2. Inspect and parse objects

```sh
Rscript scripts/inventory/inspect_catalog_metadata.R
Rscript scripts/extract_scripts/extract_XMLTagInventory.r
Rscript scripts/extract_scripts/extract_SavedColumn.R
Rscript scripts/extract_scripts/extract_XML_to_FilterObject.r
```

Creates metadata and tag inventories, saved-column rows, and the filtered RDS
input needed by the criteria exporter.

### 3. Export filter criteria

```sh
Rscript scripts/extract_scripts/export_filter_criteria.R
```

Creates `output/filter_criteria.csv`, with one row per expression-tree node and
parent/depth fields that preserve the filter's logical structure.

## Outputs

| File | Purpose |
| --- | --- |
| `catalog_extract.rds` | Complete object data including raw XML |
| `catalog_extract_summary.csv` | Object names, paths, kinds, and metadata |
| `catalog_metadata_inventory.csv` | Counts and missing-field checks by object pattern |
| `xml_tag_inventory.csv` | XML tags, paths, depths, attributes, and values |
| `saved_columns.csv` | Parsed saved-column definitions |
| `saved_column_review.xlsx` | Readable bin criteria, labels, and explanation fields |
| `filter_objects.rds` | Full filter objects including XML |
| `filter_objects_summary.csv` | Spreadsheet-friendly filter object metadata |
| `filter_criteria.csv` | Flattened filter expressions and criteria |
| `filter_review.xlsx` | Concise documentation workbook with one row per business rule |
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
