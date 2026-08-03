# Catalog inputs

- `uploads/` is the recommended location for staff-provided `.catalog` files.
  After adding a file there, set the notebook's `catalog_path` to
  `data/uploads/<exact filename>.catalog`.
- `filters_annual_statistics_2025_26.catalog` is the small filter-only input
  used by the automated pipeline test.
- `examples/annual_stats_fy_2025_2026.catalog` is the full example used by the
  README and both notebook walkthroughs. In the current snapshot it contains
  308 embedded XML objects: 64 saved columns, 21 saved filters, 98 reports,
  22 dashboards, and 103 dashboard pages.

Before committing a `.catalog` file to GitHub, confirm that the repository's
access controls are appropriate for the report definitions and metadata it
contains.
