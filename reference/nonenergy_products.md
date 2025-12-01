# Non-energy products

A string vector containing names of products classified as "Non-energy"
by the package development team. This list include hydrocarbon-derived
materials such as "Lubricants". This list also includes products
classified as "Crude, NGL, refinery feedstocks" in the IEA World
Extended Energy Balances 2021 documentation such as "Additives/blending
components".

## Usage

``` r
nonenergy_products
```

## Format

An object of class `list` of length 7.

## Examples

``` r
Recca::nonenergy_products
#> $additives_blending_components
#> [1] "Additives/blending components"
#> 
#> $bitumen
#> [1] "Bitumen"
#> 
#> $lubricants
#> [1] "Lubricants"
#> 
#> $naphtha
#> [1] "Naphtha"
#> 
#> $paraffin_waxes
#> [1] "Paraffin waxes"
#> 
#> $refinery_feedstocks
#> [1] "Refinery feedstocks"
#> 
#> $white_spirit_and_sbp
#> [1] "White spirit & SBP"
#> 
```
