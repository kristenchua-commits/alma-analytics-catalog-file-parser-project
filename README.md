# Alma Analytics catalog file parser

R utilities for extracting and reviewing objects embedded in Ex Libris Alma
Analytics `.catalog` files. The pipeline preserves full XML in RDS files and
writes spreadsheet-friendly CSV files without the large `xml_text` column.

[Open the interactive report hierarchy](https://kristenchua-commits.github.io/alma-analytics-catalog-file-parser-project/)

## Notebooks

- [`docs/notebooks/alma_analytics_catalog_file_parser.ipynb`](docs/notebooks/alma_analytics_catalog_file_parser.ipynb)
  is the main GitHub-rendered walkthrough of the parser, method selection, and
  verified example.
- [`docs/notebooks/campus_documentation_construction.ipynb`](docs/notebooks/campus_documentation_construction.ipynb)
  validates the reviewed normalized workbook, previews campus output names,
  and publishes the campus exclusions-documentation workbooks.
- [`documentation/notebooks/catalog_file_xml_and_object_structure.ipynb`](documentation/notebooks/catalog_file_xml_and_object_structure.ipynb)
  shows both the catalog object hierarchy and a collapsible view of the XML tag
  hierarchy in the current example `.catalog` file. Its R cells regenerate the
  diagrams from current pipeline outputs.

## Requirements

- R 4.1 or newer
- Core parser: the R package `xml2`
- Campus exclusions-documentation export: `readxl` and `openxlsx2`
- Optional notebook execution: an R Jupyter kernel and `IRdisplay`
- Optional test runner: `testthat`; equivalent base-R assertions run when it is
  unavailable

Install the packages needed for both documented workflows with:

```r
install.packages(c("xml2", "readxl", "openxlsx2"))
```

## Run the automated parser pipeline

From the repository root:

```sh
Rscript scripts/run_parsing_pipeline.R "data/examples/annual_stats_fy_2025_2026.catalog"
```

### Choose a file interactively in RStudio

The wrapper searches both `data/` in this repository and your `~/Downloads`
folder. It also provides a file-browser option for files stored elsewhere.

```r
source("scripts/choose_catalog_file_and_run_pipeline.R")
catalog <- run_catalog_pipeline()
```

## Parser pipeline map

![Alma Analytics catalog parser pipeline showing every processing script, input, intermediate dataset, and output](documentation/images/run_parser_pipeline_diagram.png)

The diagram covers the automated `.catalog` parser. The arrows show the
direction of processing. Blue identifies the original input, green identifies
processing scripts, yellow identifies intermediate datasets, purple identifies
human-facing review outputs, and gray identifies inspection or detailed
technical outputs. The separately reviewed publication workflow is documented
below.

### Catalog object tree

Create a searchable, collapsible tree of the catalog folders and objects in
`catalog_extract_summary.csv` with:

```sh
Rscript scripts/render_catalog_object_tree.R
```

The standalone HTML diagram is written to `output/catalog_object_tree.html`.
It uses each object's slash-delimited `original_path` to derive folder
containment, and color-codes object leaves by `object_kind`. No additional R
packages are required. An alternate input CSV, output HTML path, and initial
open depth can be supplied as positional arguments:

```sh
Rscript scripts/render_catalog_object_tree.R \
  output/catalog_extract_summary.csv \
  output/catalog_object_tree.html \
  6 \
  documentation/images/catalog_object_tree.png
```

The optional fourth argument writes a high-resolution static PNG for notebooks
and other documentation. To remain readable, this overview preserves the full
folder hierarchy but aggregates the 308 individual objects by `object_kind`
within each folder. The searchable HTML retains every object name. When the
corresponding `catalog_extract.rds` is available, the same command also writes
`documentation/images/preparation_review_dashboard_relationships.png`, a
focused three-column view derived from the explicit dashboard-to-page-to-report
XML relationships.

The summary CSV records catalog containment but not object references. The
focused dashboard diagram reads those references from `catalog_extract.rds`.

`scripts/choose_catalog_file_and_run_pipeline.R` is an optional interactive
entry point. It selects a `.catalog` file and passes it to
`scripts/run_parsing_pipeline.R`, the command-line entry point for the orchestrator in
`R/run_parsing_pipeline.R`. Reusable functions live under `R/`; scripts under `scripts/`
are intentionally thin entry points.

## Report-builder pipeline map

![Alma Analytics report builder pipeline showing parser review outputs, manual normalization, publication scripts, and campus workbooks](documentation/images/run_report_builder_pipeline_diagram.png)

Regenerate this diagram from the repository root with:

```sh
Rscript scripts/render_report_builder_pipeline_diagram.R
```

The report-builder diagram begins at the parser's general column and filter
rule outputs. It shows the manual normalization checkpoint, the optional
validation notebook, the campus documentation exporter, and the final campus
workbooks. The inventory/usage exports are shown separately because they do
not feed the report builder.

| Stage | Script(s) | Reads | Writes |
| --- | --- | --- | --- |
| Input selection and orchestration | `scripts/choose_catalog_file_and_run_pipeline.R`; `scripts/run_parsing_pipeline.R`; `R/choose_catalog_file.R`; `R/run_parsing_pipeline.R` | Raw Alma Analytics `.catalog` file | Starts shared extraction |
| Shared catalog extraction | `R/extract/extract_catalog.R`; `R/io/read_catalog_file.R`; `R/io/read_catalog_metadata.R` | `.catalog` file | `catalog_extract.rds`; `catalog_extract_summary.csv` |
| Metadata and XML-tag inspection | `R/inspect/inspect_catalog_metadata.R`; `R/extract/extract_xml_tag_inventory.R` | `catalog_extract.rds` | `catalog_metadata_inventory.csv`; `xml_tag_inventory.csv` |
| General column parsing | `R/extract/extract_columns.R` | Report and saved-column XML in `catalog_extract.rds` | `columns.csv`; `column_rules.csv` |
| General filter parsing | `R/extract/extract_filters.R` | Report and saved-filter XML in `catalog_extract.rds` | `filters.csv`; `filter_rules.csv`; `filter_value_lists.csv` |
| Report dependencies | `R/export/export_report_dependencies.R` | General column and filter records | `report_dependencies.csv` |
| Reviewed normalization | Manual review in `output/normalized_combined_saved_column_and_filter_review.xlsx` | Column and filter rule rows | Campus, domain, labels, explanations, and campus worksheets |
| Campus documentation publication | `scripts/export_documentation/export_exclusions_documentation.R` | Reviewed normalized workbook and source `.catalog` file | One exclusions-documentation workbook per `UC*` worksheet |

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
Dashboards, dashboard pages, and unrecognized objects remain available in this
file and in `catalog_extract_summary.csv`. Reports, saved columns, and saved
filters feed the two general parsing branches described below.

### Inspection branch

`R/inspect/inspect_catalog_metadata.R` and
`R/extract/extract_xml_tag_inventory.R` read `catalog_extract.rds` and create
the metadata-pattern and XML-tag inventories.

### General column branch

`R/extract/extract_columns.R` handles all column forms in one pass.
`columns.csv` contains report-embedded inline columns, report references to
saved columns, and every standalone saved-column object. The `record_scope`,
`column_source`, `is_resolved`, and `is_referenced` fields make those roles
explicit. Standalone saved columns are retained even when no report references
them.

`column_rules.csv` expands inline and standalone binned columns into readable
`when` and `otherwise` rules and retains calculated-column formulas. A report's
saved-column reference points to the standalone definition instead of
duplicating that definition's rules for every report that uses it.

### General filter branch

`R/extract/extract_filters.R` likewise handles inline report filters, report
references to saved filters, and every standalone saved-filter object.
`filters.csv` is the usage and definition inventory, while `filter_rules.csv`
contains readable leaf criteria. Complete large `IN` and `NOT IN` lists are
stored in `filter_value_lists.csv`. Unreferenced standalone saved filters remain
in the outputs.

`report_dependencies.csv` is the simplified relationship export. It contains
one row per distinct report-to-saved-column or report-to-saved-filter path and
indicates whether that target resolves to an object contained in the same
catalog extract. The general branches are conditional and are skipped only when
their relevant report and standalone object types are both absent.

### Reviewed publication workflow

`run_parsing_pipeline()` ends after creating the general column and filter rule files.
It does not create the normalized combined workbook automatically.

To create an unreviewed workbook covering every inline and standalone column,
bin branch, and filter criterion, run:

```sh
Rscript scripts/build_all_column_filter_bin_review.R
```

The script reads `columns.csv`, `column_rules.csv`, `filters.csv`,
`filter_rules.csv`, and `filter_value_lists.csv`. Its default output is
`output/normalized_combined_all_column_filter_bin_review.xlsx`. Optional first
and second arguments override the parser-output directory and workbook path.
The workbook includes combined and type-specific review sheets, full column and
filter inventories, and expanded values for list filters. Inline rows retain
their report title/path; standalone rows retain their saved-object definition
name/path.

`output/normalized_combined_saved_column_and_filter_review.xlsx` is a reviewed
input to the publication step. It combines filter and saved-column rules and
adds human-maintained fields, including:

- Electronic, Physical, or Fulfillment domain
- campus or global scope
- review labels and explanations
- campus-specific rule selection

Before constructing the campus workbooks, the publication exporter refreshes
saved-column criterion text from `output/column_rules.csv`, matching on
`rule_name` and `rule_id`. This preserves the normalized workbook's manual
campus and classification decisions while preventing older parser text from
reintroducing missing comparison operators.

After that review is complete, generate the campus workbooks with:

```sh
Rscript scripts/export_documentation/export_exclusions_documentation.R \
  output/normalized_combined_saved_column_and_filter_review.xlsx
```

By default, the generated campus workbooks are written under:

- `output/Campus FY 2025-26 annual statistics NZ-level output/2025/2026/`

Because `/` separates path components, the annual-statistics destination uses
nested `2025/2026` directories. Pass a second command-line argument to override
this default output directory.

The script processes worksheets whose names begin with `UC`. It requires these
columns:

`rule_type`, `source_workbook`, `Electronic/Physical/Fulfillment`, `Campus`,
`rule_name`, `rule_id`, `join_operator`, `criterion_text`, `value_count`,
`review_label`, `explanation`, and `criterion_text_category`.

The fiscal-year label and output filename pattern are currently configured in
`scripts/export_documentation/export_exclusions_documentation.R` for FY 2025–26. Update that
configuration before using the script for a new annual cycle.

The exporter reads the source `.catalog` file's creation date and adds it to
each filename in ISO `YYYY-MM-DD` format. The current source archive was created
on July 9, 2026.

Campus documentation filenames use this pattern:

- `UCB_targeted_review_FY2025-26_exclusions_documentation_as_of_2026-07-09.xlsx`
- `UCD_targeted_review_FY2025-26_exclusions_documentation_as_of_2026-07-09.xlsx`
- one corresponding workbook for every other normalized-workbook sheet whose
  name begins with `UC`

## Outputs

| File | Purpose |
| --- | --- |
| `catalog_extract.rds` | Complete object data including raw XML |
| `catalog_extract_summary.csv` | Object names, paths, kinds, and metadata |
| `catalog_metadata_inventory.csv` | Counts and missing-field checks by object pattern |
| `xml_tag_inventory.csv` | XML tags, paths, depths, attributes, and values |
| `columns.csv` | Report inline columns, report saved-column references, and all standalone saved-column objects |
| `column_rules.csv` | Formula and bin rules for inline and standalone columns |
| `filters.csv` | Report inline filters, report saved-filter references, and all standalone saved-filter objects |
| `filter_rules.csv` | Readable leaf criteria for inline and standalone filters |
| `filter_value_lists.csv` | Individual values from large `IN` and `NOT IN` lists |
| `report_dependencies.csv` | Distinct report-to-saved-column/filter paths with target-resolution status |
| `normalized_combined_all_column_filter_bin_review.xlsx` | Scripted review workbook for all inline and standalone columns, bin branches, filters, inventories, and expanded list values |
| `normalized_combined_saved_column_and_filter_review.xlsx` | Reviewed, manually enriched source for campus publication; not created by `run_parsing_pipeline()` |
| `Campus FY 2025-26 annual statistics NZ-level output/2025/2026/*.xlsx` | Campus-specific documentation created by the separate publication script |

Automated parser files are written directly under `output/`. The pipeline
creates that directory when necessary but does not delete unrelated or
downstream files already stored there. Documentation images live separately
under `docs/images/` and `docs/validation/`.

`column_rules.csv` and `filter_rules.csv` share core rule, criterion, and review
fields and add provenance fields that identify report context or the standalone
definition. This allows one normalized review workflow without losing where a
rule came from.

Some checked-in output files are retained as project snapshots and can predate
the current exporter schema. Run the parser into a new output directory when
you need outputs guaranteed to match the current code.

## Tests

The small filter-only catalog is maintained at
`data/filters_annual_statistics_2025_26.catalog` and used as the automated test
fixture.
Run:

```sh
Rscript tests/testthat.R
```

The test executes the complete general filter branch in a temporary directory,
checks its expected files, and verifies that column outputs are skipped when no
report or saved-column object exists. Synthetic fixtures verify inline records,
saved references, dependencies, and unreferenced standalone objects in both
general branches.
The suite uses `testthat` when installed and otherwise runs equivalent
assertions with base R. It does not currently exhaustively test saved-column
rule content, the manually normalized workbook, or the campus exclusions-documentation
exporter.

## Repository layout

| Directory | Purpose |
| --- | --- |
| `R/io/` | Catalog readers and metadata alignment |
| `R/extract/` | Catalog, general column/filter, and XML-tag extraction |
| `R/inspect/` | Metadata inspection |
| `R/export/` | Cross-object relationship exports |
| `scripts/` | Command-line and interactive parser entry points, diagram rendering, and campus documentation publication |
| `data/` | Catalog inputs, including the small filter test fixture |
| `data/examples/` | Full example `.catalog` input |
| `documentation/notebooks/` | Parser walkthrough, campus-documentation publication, and collapsible XML-structure notebooks |
| `documentation/images/` | Generated pipeline documentation diagrams |
| `documentation/validation/` | Manual validation evidence |
| `output/` | Parser outputs, reviewed normalized workbook, and campus documentation snapshots |
