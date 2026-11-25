# Write Table to File

Writes a medsim_table object to a file in LaTeX, CSV, or Markdown
format.

## Usage

``` r
medsim_write_table(table, file, format = NULL)
```

## Arguments

- table:

  A medsim_table object

- file:

  Character: output file path

- format:

  Character: output format ("latex", "csv", "markdown") If NULL,
  inferred from file extension.

## Value

Invisibly returns the file path

## Examples

``` r
if (FALSE) { # \dontrun{
table <- medsim_table_accuracy(analysis)

# Write LaTeX
medsim_write_table(table, "tables/accuracy.tex")

# Write CSV
medsim_write_table(table, "tables/accuracy.csv", format = "csv")
} # }
```
