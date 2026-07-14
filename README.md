# Alma Analytics catalog file parser

R utilities for extracting and reviewing objects embedded in Ex Libris Alma
Analytics `.catalog` files. The pipeline preserves full XML in RDS files and
writes spreadsheet-friendly CSV files without the large `xml_text` column.

For a GitHub-rendered notebook walkthrough with annotations, R code cells, and
saved example outputs, open
[`docs/notebooks/alma_analytics_catalog_file_parser.ipynb`](docs/notebooks/alma_analytics_catalog_file_parser.ipynb).

## Requirements

- R 4.1 or newer
- The R packages `xml2` and `writexl`

Install the packages once with `install.packages(c("xml2", "writexl"))`.

## Run the complete pipeline

From the repository root:

```sh
Rscript scripts/run_pipeline.R "data/examples/annual_stats_fy_2025_2026.catalog"
```

### Choose a file interactively in RStudio

The wrapper searches both `data/` in this repository and your `~/Downloads`
folder. It also provides a file-browser option for files stored elsewhere.

```r
source("scripts/choose_catalog_file_and_run_pipeline.R")
catalog <- run_catalog_pipeline()
```

## Pipeline map

![Alma Analytics catalog parser pipeline showing every processing script, input, intermediate dataset, and output](docs/images/run_pipeline_diagram.png)

The arrows show the direction of processing. Blue identifies the original
input, green identifies processing scripts, yellow identifies intermediate
datasets, purple identifies human-facing review outputs, and gray identifies
inspection or detailed technical outputs.

`scripts/choose_catalog_file_and_run_pipeline.R` is an optional interactive
entry point. It selects a `.catalog` file and passes it to
`scripts/run_pipeline.R`, the command-line entry point for the orchestrator in
`R/run_pipeline.R`. Reusable functions live under `R/`; scripts under `scripts/`
are intentionally thin entry points.

| Stage | Script(s) | Reads | Writes |
| --- | --- | --- | --- |
| Input selection and orchestration | `scripts/choose_catalog_file_and_run_pipeline.R`; `scripts/run_pipeline.R`; `R/choose_catalog_file.R`; `R/run_pipeline.R` | Raw Alma Analytics `.catalog` file | Starts shared extraction |
| Shared catalog extraction | `R/extract/extract_catalog.R`; `R/io/read_catalog_file.R`; `R/io/read_catalog_metadata.R` | `.catalog` file | `catalog_extract.rds`; `catalog_extract_summary.csv` |
| Metadata and XML-tag inspection | `R/inspect/inspect_catalog_metadata.R`; `R/extract/extract_xml_tag_inventory.R` | `catalog_extract.rds` | `catalog_metadata_inventory.csv`; `xml_tag_inventory.csv` |
| Saved-column parsing and review | `R/extract/extract_saved_columns.R`; `R/export/export_saved_column_review.R` | Saved-column rows in `catalog_extract.rds` | `saved_columns.csv`; `saved_column_review.xlsx`; `saved_column_review.csv` |
| Filter-object selection | `R/extract/extract_filter_objects.R` | Filter rows in `catalog_extract.rds` | `filter_objects.rds`; `filter_objects_summary.csv` |
| Detailed filter criteria | `R/export/export_filter_criteria.R` | `filter_objects.rds` | `filter_criteria.csv` |
| Filter review | `R/export/export_filter_review.R` | `filter_objects.rds` | `filter_review.xlsx`; `filter_review.csv`; `filter_review_value_lists.csv` |

### Shared extraction

The first stage always uses `R/extract/extract_catalog.R` to extract the XML
objects and align them with their catalog metadata. It calls the two readers in
`R/io/`.

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

`R/inspect/inspect_catalog_metadata.R` and
`R/extract/extract_xml_tag_inventory.R` read `catalog_extract.rds` and create
the metadata-pattern and XML-tag inventories.

### Saved-column branch

When the catalog contains `saved_column` objects, the orchestrator runs
`R/extract/extract_saved_columns.R` and
`R/export/export_saved_column_review.R`.

The first script creates the detailed saved-column export. The review exporter
turns formulas and XML `when`, `condition`, `value`, and `otherwise` elements
into readable criteria, labels, and rule IDs.

### Filter branch

When the catalog contains `filter` objects, the orchestrator runs
`R/extract/extract_filter_objects.R`, `R/export/export_filter_criteria.R`, and
`R/export/export_filter_review.R`.

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

Generated files under `output/` are ignored by Git and can be deleted safely.
Selected committed examples live under `examples/output/`; documentation images
live under `docs/images/` and `docs/validation/`.

## Tests

The small filter-only catalog is maintained as a fixture under `tests/fixtures/`.
Run:

```sh
Rscript tests/testthat.R
```

The test executes the complete filter branch in a temporary directory, checks
its expected files, and verifies that saved-column outputs are skipped. It uses
`testthat` when installed and otherwise runs equivalent assertions with base R.

## Repository layout

| Directory | Purpose |
| --- | --- |
| `R/io/` | Catalog readers and metadata alignment |
| `R/extract/` | Catalog, saved-column, filter-object, and XML-tag extraction |
| `R/inspect/` | Metadata inspection |
| `R/export/` | Review and detailed exports |
| `scripts/` | Thin command-line, interactive, and diagram-rendering entry points |
| `data/examples/` | Example `.catalog` input |
| `tests/fixtures/` | Small catalog fixtures used by automated tests |
| `docs/notebooks/` | Executable notebook walkthrough |
| `docs/images/` | Generated pipeline documentation diagram |
| `docs/validation/` | Manual validation evidence |
| `examples/output/` | Deliberately committed representative outputs |
| `output/` | Disposable generated output, ignored by Git |
