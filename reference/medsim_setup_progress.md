# Setup Progress Bar Options

Configures pbapply package options for progress bars. Automatically
adjusts based on execution environment (interactive vs batch).

## Usage

``` r
medsim_setup_progress(style = NULL, char = "=")
```

## Arguments

- style:

  Character: Progress bar style ("timer", "txt", "none")

- char:

  Character: Character to use for progress bar (default "=")

## Value

Invisibly returns previous options
