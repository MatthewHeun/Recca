# Calculate the origin and extent for each matrix

The origin is defined as the upper-left corner of the matrix on the
worksheet. The extent is defined as the lower-right corner of the matrix
on the worksheet.

## Usage

``` r
calc_mats_locations_excel(R, U, V, Y, r_eiou, U_eiou, U_feed, S_units, pad = 2)
```

## Arguments

- R, U, V, Y, r_eiou, U_eiou, U_feed, S_units:

  Matrices to be arranged on an Excel worksheet.

- pad:

  The number of blank rows or columns between matrices.

## Value

A nested list of origins and extents.

## Details

The outer structure of the return value is matrices, in the order
provided in the argument list. The inner structure of the return value
is a list of "origin" and "extent," in that order.

This is a helper function, so it is not public.
