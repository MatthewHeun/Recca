# Develop a warning message for malformed Excel region names

Well-formed Excel region names are important for named regions in the
Excel workbooks created by
[`write_ecc_to_excel()`](https://matthewheun.github.io/Recca/reference/write_ecc_to_excel.md).
This function warns if the region names contain illegal characters,
start with an illegal character, resemble a cell reference, or are too
long.

## Usage

``` r
check_named_region_violations(candidate_region_names)
```

## Arguments

- candidate_region_names:

  A character vector of candidate region names.

## Value

`NULL` invisibly and a warning if any problems are detected.

## Examples

``` r
# No warning
check_named_region_violations(c("test1", "test2"))
if (FALSE) { # \dontrun{
  # Warnings
  # Illegal character
  check_named_region_violations("\\")
  check_named_region_violations("a\\")
  # Starts with illegal character
  check_named_region_violations(" ")
  # Resembles cell reference
  check_named_region_violations("B12")
  # Too long
  check_named_region_violations(strrep("x", 256))
} # }
```
