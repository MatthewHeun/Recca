# Upstream footprint and downstream effects matrices

Calculates upstream footprint matrices (**F_footprint_p** and
**F_footprint_s**) and downstream effects matrices (**F_effects_p** and
**F_effects_s**) given embodied matrices **M_p** and **M_s**. Column
sums of **F_footprint** are `1`. Row sums of **F_effects** are `1`.

## Usage

``` r
calc_F_footprint_effects(
  .Mmats = NULL,
  M_p = "M_p",
  M_s = "M_s",
  F_footprint_p = "F_footprint_p",
  F_effects_p = "F_effects_p",
  F_footprint_s = "F_footprint_s",
  F_effects_s = "F_effects_s"
)
```

## Arguments

- .Mmats:

  A data frame containing a column of embodied matrices.

- M_p:

  An embodied product matrix or name of the column in `.Mmats`
  containing same. Default is "M_p".

- M_s:

  An embodied sector matrix or name of the column in `.Mmats` containing
  same. Default is "M_s".

- F_footprint_p:

  The name for **F_footprint_p** matrices on output. Default is
  "F_footprint_p".

- F_effects_p:

  The name for **F_effects_p** matrices on output. Default is
  "F_effects_p".

- F_footprint_s:

  The name for **F_footprint_s** matrices on output. Default is
  "F_footprint_s".

- F_effects_s:

  The name for **F_effects_s** matrices on output. Default is
  "F_effects_s".

## Value

A list or data frame containing **F_footprint_p**, **F_effects_p**,
**F_footprint_s**, and **F_effects_s** matrices.
