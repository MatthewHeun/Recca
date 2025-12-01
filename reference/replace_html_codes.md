# Un-escape HTML codes in text

Occasionally, we need to un-escape HTML codes in text. If `text`
contains HTML codes, they are replaced with `replacements`, which, by
default, describe replacements for "`&amp;`", "`&lt;`", and "`&gt;`"
("`&`", "`<`", and "`>`", respectively).

## Usage

``` r
replace_html_codes(
  text,
  replacements = list(c("&amp;", "&"), c("&lt;", "<"), c("&gt;", ">"))
)
```

## Arguments

- text:

  a vector (or one-dimensional list) of character strings

- replacements:

  a list of string pairs. Each pair consists of encoded string and
  unencoded string, in that order. Default is
  `list(c("&amp;", "&"), c("&lt;", "<"), c("&gt;", ">"))`

## Value

If `text` is a vector, a vector of un-encoded strings. If `text` is a
list of strings, a list of un-encoded strings of same structure. If
possible, an outgoing list has simplified structure, even to the point
of conversion to vector.

## Details

HTML codes can arrive in text read from an Excel file by the `openxlsx`
package due to a bug documented
[here](https://github.com/awalker89/openxlsx/issues/393).

## Examples

``` r
replace_html_codes(list("a", "&amp;", "&lt;", "&gt;", "bcd"))
#> [1] "a"   "&"   "<"   ">"   "bcd"
replace_html_codes(list(c("&amp;", "&amp;"), c("&lt;", "&lt;"), c("&gt;", "&gt;")))
#> [[1]]
#> [1] "&" "&"
#> 
#> [[2]]
#> [1] "<" "<"
#> 
#> [[3]]
#> [1] ">" ">"
#> 
```
