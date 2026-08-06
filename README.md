# Alma Analytics catalog file parser

R utilities for extracting and reviewing objects embedded in Ex Libris Alma
Analytics `.catalog` files. The pipeline preserves full XML in RDS files and
writes spreadsheet-friendly CSV files without the large `xml_text` column.

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

The report-builder diagram begins at the parser's saved-column and filter
review outputs. It shows the manual normalization checkpoint, the optional
validation notebook, the campus documentation exporter, and the final campus
workbooks. The technical `filter_criteria.csv` branch is shown separately
because it does not feed the report builder.

| Stage | Script(s) | Reads | Writes |
| --- | --- | --- | --- |
| Input selection and orchestration | `scripts/choose_catalog_file_and_run_pipeline.R`; `scripts/run_parsing_pipeline.R`; `R/choose_catalog_file.R`; `R/run_parsing_pipeline.R` | Raw Alma Analytics `.catalog` file | Starts shared extraction |
| Shared catalog extraction | `R/extract/extract_catalog.R`; `R/io/read_catalog_file.R`; `R/io/read_catalog_metadata.R` | `.catalog` file | `catalog_extract.rds`; `catalog_extract_summary.csv` |
| Metadata and XML-tag inspection | `R/inspect/inspect_catalog_metadata.R`; `R/extract/extract_xml_tag_inventory.R` | `catalog_extract.rds` | `catalog_metadata_inventory.csv`; `xml_tag_inventory.csv` |
| Report structure extraction | `R/extract/extract_report_columns.R`; `R/extract/extract_report_filters.R`; `R/export/export_report_dependencies.R` | Report rows and XML in `catalog_extract.rds` | `report_saved_and_non_saved_columns.csv`; `report_saved_and_non_saved_filters.csv`; `report_dependencies.csv` |
| Saved-column object selection | `R/extract/extract_saved_column_objects.R` | Saved-column rows in `catalog_extract.rds` | `saved_column_objects.rds`; `saved_column_objects_summary.csv` |
| Detailed saved columns | `R/extract/extract_saved_columns.R` | `saved_column_objects.rds` | `saved_columns.csv` |
| Saved-column review | `R/export/export_saved_column_review.R` | `saved_column_objects.rds` | `saved_column_review.csv` |
| Filter-object selection | `R/extract/extract_filter_objects.R` | Filter rows in `catalog_extract.rds` | `filter_objects.rds`; `filter_objects_summary.csv` |
| Detailed filter criteria | `R/export/export_filter_criteria.R` | `filter_objects.rds` | `filter_criteria.csv` |
| Filter review | `R/export/export_filter_review.R` | `filter_objects.rds` | `filter_review.csv`; `filter_review_value_lists.csv` |
| Reviewed normalization | Manual review in `output/normalized_combined_saved_column_and_filter_review.xlsx` | Saved-column and filter review rows | Campus, domain, labels, explanations, and campus worksheets |
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
file and in `catalog_extract_summary.csv`. Reports additionally feed the report
structure branch described below.

### Inspection branch

`R/inspect/inspect_catalog_metadata.R` and
`R/extract/extract_xml_tag_inventory.R` read `catalog_extract.rds` and create
the metadata-pattern and XML-tag inventories.

### Report structure branch

When the catalog contains `report` objects, the orchestrator extracts every
column and logical filter term embedded in each report's criteria XML. This
includes definitions that were never saved as standalone catalog objects.

`report_saved_and_non_saved_columns.csv` contains every report-selected column
in one sheet. The `column_source` field distinguishes `inline` definitions from
`saved_reference` rows. `column_name` is the canonical heading from the resolved
saved-column object when available, while `report_display_name` preserves any
shorter heading applied inside the individual report. Saved references also
retain their saved-object name and full catalog path. Binned non-saved columns
retain their base formula, expression type, and rule count.

`report_saved_and_non_saved_filters.csv` contains every report filter term in
one sheet. The `filter_source` field distinguishes `inline` terms from
`saved_reference` rows. All rows retain their report context and logical join;
non-saved filters also include expression type, operator, field, formatted
text, and values. Large value lists are represented by a complete `value_count`
and a ten-value preview so CSV cells remain manageable; the original XML
remains in `catalog_extract.rds`.

`report_dependencies.csv` is the simplified relationship export. It contains
one row per distinct report-to-saved-column or report-to-saved-filter path and
indicates whether that target resolves to an object contained in the same
catalog extract.

### Saved-column branch

When the catalog contains `saved_column` objects, the orchestrator first runs
`R/extract/extract_saved_column_objects.R`. It selects the saved-column rows,
adds a stable `saved_column_object_index`, and creates
`saved_column_objects.rds` with the raw XML plus a spreadsheet-friendly
`saved_column_objects_summary.csv` without XML.

Both downstream saved-column processors read that specialized intermediate
independently. `R/extract/extract_saved_columns.R` creates the detailed column
export. `R/export/export_saved_column_review.R` turns formulas and XML `when`,
`condition`, `value`, and `otherwise` elements into SQL-formatted criteria,
labels, and rule IDs. Alma operators are rendered as SQL operators such as `=`,
`IN`, `IS NULL`, and `LIKE`.

### Filter branch

When the catalog contains `filter` objects, the orchestrator runs
`R/extract/extract_filter_objects.R`, `R/export/export_filter_criteria.R`, and
`R/export/export_filter_review.R`.

The first script creates `filter_objects.rds`, the specialized intermediate
dataset used independently by both exporters. `filter_criteria.csv` contains one
row per expression-tree node, with parent/depth fields that preserve logical
structure. The review exporter creates concise business-rule rows and retains
large `IN`/`NOT IN` lists in a companion CSV.

The report, saved-column, and filter branches are conditional: a branch is
skipped when the corresponding object type is absent.

### Reviewed publication workflow

`run_parsing_pipeline()` ends after creating the saved-column and filter review files.
It does not create the normalized combined workbook automatically.

`output/normalized_combined_saved_column_and_filter_review.xlsx` is a reviewed
input to the publication step. It combines filter and saved-column rules and
adds human-maintained fields, including:

- Electronic, Physical, or Fulfillment domain
- campus or global scope
- review labels and explanations
- campus-specific rule selection

Before constructing the campus workbooks, the publication exporter refreshes
saved-column criterion text from `output/saved_column_review.csv`, matching on
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
| `report_saved_and_non_saved_columns.csv` | All report-selected columns, with canonical saved-column names and report-specific display headings kept separate |
| `report_saved_and_non_saved_filters.csv` | All report filter terms; `filter_source` distinguishes non-saved definitions from saved-filter references |
| `report_dependencies.csv` | Distinct report-to-saved-column/filter paths with target-resolution status |
| `saved_column_objects.rds` | Saved-column object intermediate, including raw XML |
| `saved_column_objects_summary.csv` | Spreadsheet-friendly saved-column object metadata without XML |
| `saved_columns.csv` | Parsed saved-column definitions |
| `saved_column_review.csv` | CSV version of the saved-column rules in the shared review schema |
| `filter_objects.rds` | Full filter objects including XML |
| `filter_objects_summary.csv` | Spreadsheet-friendly filter object metadata |
| `filter_criteria.csv` | Flattened filter expressions and criteria |
| `filter_review.csv` | CSV version of the filter rules in the shared review schema |
| `filter_review_value_lists.csv` | Individual values from large `IN`/`NOT IN` lists |
| `normalized_combined_saved_column_and_filter_review.xlsx` | Reviewed, manually enriched source for campus publication; not created by `run_parsing_pipeline()` |
| `Campus FY 2025-26 annual statistics NZ-level output/2025/2026/*.xlsx` | Campus-specific documentation created by the separate publication script |

Automated parser files are written directly under `output/`. The pipeline
creates that directory when necessary but does not delete unrelated or
downstream files already stored there. Documentation images live separately
under `docs/images/` and `docs/validation/`.

The current exporters use the same ten-column rule schema for
`saved_column_review.csv` and `filter_review.csv`:

`rule_name`, `criterion_text`, `review_label`, `explanation`,
`criterion_text_category`, `rule_id`, `rule_type`, `source_workbook`,
`join_operator`, and `value_count`.

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

The test executes the complete filter branch in a temporary directory, checks
its expected files, and verifies that report and saved-column outputs are
skipped. Synthetic fixtures verify report references and dependencies as well
as the saved-column object intermediate and both of its downstream consumers.
The suite uses `testthat` when installed and otherwise runs equivalent
assertions with base R. It does not currently exhaustively test saved-column
rule content, the manually normalized workbook, or the campus exclusions-documentation
exporter.

## Repository layout

| Directory | Purpose |
| --- | --- |
| `R/io/` | Catalog readers and metadata alignment |
| `R/extract/` | Catalog, report-column/filter, saved-column, filter-object, and XML-tag extraction |
| `R/inspect/` | Metadata inspection |
| `R/export/` | Review and detailed exports |
| `scripts/` | Command-line and interactive parser entry points, diagram rendering, and campus documentation publication |
| `data/` | Catalog inputs, including the small filter test fixture |
| `data/examples/` | Full example `.catalog` input |
| `docs/notebooks/` | Parser walkthrough, campus-documentation publication, and collapsible XML-structure notebooks |
| `docs/images/` | Generated pipeline documentation diagram |
| `docs/validation/` | Manual validation evidence |
| `output/` | Parser outputs, reviewed normalized workbook, and campus documentation snapshots |
