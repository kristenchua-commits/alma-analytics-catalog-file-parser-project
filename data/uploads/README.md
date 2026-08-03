# Staff-provided catalog inputs

Upload or copy staff-provided Alma Analytics `.catalog` files into this folder.
Then set `catalog_path` in
`docs/notebooks/alma_analytics_catalog_file_parser.ipynb` to the repository-root
relative path:

```r
catalog_path <- "data/uploads/<exact filename>.catalog"
```

The included demonstration file remains under `data/examples/`. GitHub displays
notebooks but does not execute the R parser, so the repository must still be run
locally or in an approved hosted R/Jupyter environment.

Before committing a `.catalog` file to GitHub, confirm that the repository's
access controls are appropriate for the report definitions and metadata it
contains.
