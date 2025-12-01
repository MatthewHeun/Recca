# Create lists of all products and industries

From matrices that describe an energy conversion chain (**R**, **U**,
**V**, and **Y**), create a list of unique products (energy carriers)
and industries (processing stages) in the energy conversion chain.

## Usage

``` r
get_all_products_and_industries(
  .sutdata,
  piece = "all",
  inf_notation = TRUE,
  notation = list(RCLabels::notations_list),
  choose_most_specific = FALSE,
  prepositions = list(RCLabels::prepositions_list),
  R = IEATools::psut_cols$R,
  U = IEATools::psut_cols$U,
  V = IEATools::psut_cols$V,
  Y = IEATools::psut_cols$Y,
  products_col = Recca::prod_ind_names_colnames$product_names,
  industries_col = Recca::prod_ind_names_colnames$industry_names
)
```

## Arguments

- .sutdata:

  A data frame or list of `matsindf` matrices.

- piece:

  A character string indicating which piece of the row or column names
  to retain, one of "all", "noun", "pps", "pref" or "suff", or a
  preposition, indicating which part of the row or column name is to be
  retained. Default is "all".

- inf_notation:

  A boolean that tells whether to infer notation. Default is `TRUE`.

- notation:

  The notation used for row and column labels. Default is
  `list(RCLabels::notations_list)`. The default value is wrapped in a
  list, because
  [`RCLabels::notations_list`](https://matthewheun.github.io/RCLabels/reference/notations_list.html)
  is, itself, a list. See `RCLabels`.

- choose_most_specific:

  A boolean that indicates whether the most-specific notation will be
  inferred when more than one of `notation` matches a row or column
  label and `allow_multiple = FALSE`. When `FALSE`, the first matching
  notation in `notations` is returned when `allow_multiple = FALSE`.
  Default is `FALSE`.

- prepositions:

  Prepositions that can be used in the row and column label. Default is
  [`RCLabels::prepositions_list`](https://matthewheun.github.io/RCLabels/reference/prepositions_list.html).

- R, U, V, Y:

  The names of PSUT matrices. See `IEAtools::psut_cols`.

- products_col:

  The name of the products column in the output list or data frame.
  Default is `Recca::prod_ind_names_colnames$product_names`.

- industries_col:

  The name of the products column in the output list or data frame.
  Default is `Recca::prod_ind_names_colnames$industry_names`.

## Value

`.sutdata` with two new columns containing the names of products and
industries.

## Details

This function is a
[`matsindf::matsindf_apply()`](https://matthewheun.github.io/matsindf/reference/matsindf_apply.html)
style function. It can accept a `matsindf` data frame in the `.df`
argument and string for the `R`, `U`, `V`, and `Y` (as column names) in
the arguments.

## Examples

``` r
ecc <- UKEnergy2000mats %>%
  tidyr::pivot_wider(names_from = "matrix.name", values_from = "matrix") %>%
  get_all_products_and_industries()
# Show all unique product (energy carrier) names in the first row of ecc
ecc[[Recca::prod_ind_names_colnames$product_names]][[1]]
#>  [1] "Crude"               "NG"                  "Crude [from Dist.]" 
#>  [4] "Crude [from Fields]" "Diesel"              "Diesel [from Dist.]"
#>  [7] "Elect"               "Elect [from Grid]"   "NG [from Dist.]"    
#> [10] "NG [from Wells]"     "Petrol"              "Petrol [from Dist.]"
# Show all unique industry (processing stage) names
# in the fourth row of ecc.
ecc[[Recca::prod_ind_names_colnames$industry_names]][[4]]
#>  [1] "Resources [of Crude]" "Resources [of NG]"    "Car engines"         
#>  [4] "Cars"                 "Crude dist."          "Diesel dist."        
#>  [7] "Elect. grid"          "Furnaces"             "Gas wells & proc."   
#> [10] "Homes"                "Light fixtures"       "NG dist."            
#> [13] "Oil fields"           "Oil refineries"       "Petrol dist."        
#> [16] "Power plants"         "Rooms"                "Truck engines"       
#> [19] "Trucks"               "Transport"            "Residential"         
```
