# Alma Analytics catalog file parser — five-minute transcript

## Slide 1 — Alma Analytics catalog file parser

This project makes an Alma Analytics catalog understandable outside the Alma interface. A catalog file is really a bundle of XML objects: reports, saved columns, saved filters, dashboards, and related metadata. The parser turns that bundle into a set of inspectable, spreadsheet-friendly outputs while retaining the complete XML for deeper analysis. In the next five minutes, I’ll show the problem it solves, how the pipeline works, what the current example produces, and where human review fits before campus documentation is published.

## Slide 2 — The parser makes hidden catalog logic inspectable

The core problem is opacity. Inside Alma, a report may reference saved objects that live elsewhere in a shared catalog tree, and the underlying rules can be buried in nested XML. That makes it difficult to answer simple governance questions: Which objects exist? Which reports depend on which saved definitions? What does a filter or binned column actually do? The project starts by reading every object and aligning its XML with catalog metadata such as title, path, type, ownership, and timestamps. It preserves folder context, but excludes folder-only rows so the working dataset stays focused on executable or inspectable objects.

## Slide 3 — One command runs the parsing pipeline

The automated parser is intentionally a single pipeline with reusable stages. A command-line script or an optional RStudio file chooser selects a dot-catalog file. Shared extraction creates an RDS file containing metadata plus full XML, and a CSV summary without the large XML text. Inspection branches inventory metadata patterns and XML tags. The two main parsing branches then normalize columns and filters. Finally, the dependency exporter links reports to referenced saved columns and saved filters and records whether each target resolves inside the same extract. Branches are conditional, so catalogs without a relevant object type skip outputs they do not need.

## Slide 4 — The example catalog expands into evidence at several levels

The current annual-statistics example shows the scale and the reason normalization matters. It contains 308 catalog objects. Parsing yields 761 column records and 315 filter records, including both report-embedded logic and standalone saved definitions. Those records expand into 903 column rules and 323 readable filter rules. The dependency table contains 450 distinct report-to-saved-object relationships. The largest expansion is the filter value list: 150,880 individual values are separated from long `IN` and `NOT IN` expressions. That keeps the main rule tables readable without discarding the detailed criteria needed for validation.

## Slide 5 — References remain explicit instead of being flattened

A key design choice is to preserve references instead of flattening everything. Report rows identify inline logic and references to saved objects. Standalone saved columns and saved filters remain in the outputs even when no report currently references them. When a report does reference a saved definition, the dependency export records the path and whether a matching object was found in the catalog. This makes unresolved references visible and avoids duplicating a saved rule into every consuming report. The same extract can also reveal explicit dashboard-to-page-to-report relationships, complementing the folder hierarchy with actual object references.

## Slide 6 — Human review gates campus publication

The automated parser stops at general-purpose evidence. Publication is a separate, reviewed workflow. A normalized workbook combines saved-column and filter rules with human-maintained fields such as domain, campus scope, review labels, explanations, and campus-specific selection. The publication exporter refreshes saved-column criterion text from the latest parser output, while preserving those manual decisions. It then creates one exclusions-documentation workbook for each worksheet whose name begins with `UC`. This boundary is deliberate: code handles repeatable extraction and formatting, while subject-matter reviewers remain responsible for classification and interpretation.

## Slide 7 — Preserve the source. Expose the logic. Publish after review.

In short, the project provides three durable capabilities. First, it preserves a faithful machine-readable extract of the catalog. Second, it translates column, filter, and dependency logic into tables that people can review and test. Third, it supports a controlled handoff from automated evidence to campus-specific documentation. The current base-R fixture assertions pass, providing a lightweight verification path even when the optional `testthat` package is unavailable. The practical next step is to run the parser on a new catalog, review unresolved references and normalized rules, and only then publish the campus workbooks.
