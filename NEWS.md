* Implemented energy balance checking for `new_R_ps()`
  with its own tolerance, by default `1e-6`.


# Recca 0.1.20 (2020-03-27)

* Completed the conversion of the entire package
  to enable use of `R` matrices in the PSUT framework.
  This effort involved converting several functions to accept *either* 
    1) both `R` and `V` matrices in the `R` and `V` arguments or
    2) nothing supplied to the `R` argument and `R + V` supplied to the `V` argument.
* Worked around a bug in `openxlsx::read.xlsx()` that HTML-escapes referenced cells
  that contain "&", "<", or ">".  
  See https://github.com/awalker89/openxlsx/issues/393 for details.
* Switched to using the `openxlsx` package for reading data directly out of Excel files,
  eliminating the need to export .csv files for the UKEnergy2000tidy data frame.
* `primary_aggregates()` now properly handles PSUT matrices formulated with `R` matrices.
* Fixed typos in `Recca.Rmd` vignette.


# Recca 0.1.19 (2020-02-19)

* Removed dependency on package `qgraph`.
  `qgraph` depended upon `graph`, 
  which is apparently no longer available on CRAN.
  The dependency on `qgraph` was only for graphical representations of ECCs.
  But, graphical representations of ECCs are not presently implemented in `Recca`.
  Future versions of `Recca` may implement graphical representations. 
  I can reassess the packages for graphing and Sankey diagrams 
  at a later date.


# Recca 0.1.18 (2020-01-17)

* Updates to synchronize with recent changes in other packages.
  In particular, there was one place where `group`ing on a column
  of a data frame with `NA` entries was causing a warning to be emitted.
  Now, the `grouping` happens after splitting the data frame into 
  rows with `NA` entries and those without. 
  Grouping and sorting happens only for the rows without `NA` values.


# Recca 0.1.17 (2019-02-25)

* First draft of completed vignette.
* Removed ecc_layout function. It was not being used, and there is a create_sankey function, anyway.


# Recca 0.1.16 (2019-02-16)

* Now automatically building in Travis.
* Added new `reverse()` function to change flow direction of an ECC.
  `reverse()` will be useful for downstream swim functions.
* Simplified and improved `resource_industries()` function.
    - No longer summing columns of `U` matrix.
    - Instead, cleaning 0 columns of `U` and comparing names of columns to the original `U`.
* Eliminated all `@importFrom` statements by fully-qualifying all function calls.
* Test coverage now up to 100%.


# Recca 0.1.15 (2019-01-09)

* First draft of `new-functions` vignette now complete, including demonstrations of 
  `new_R_ps()` and `new_k_ps()` functions.


# Recca 0.1.14 (2019-01-07)

* New function `make_sankey()` does what it says: makes a Sankey diagram.
  `make_sankey()` is the first function that optionally uses the resource (`R`) matrix.
* `edge_list()` also now accepts a resource (`R`) matrix on input.
* The vignette describing the `new*` functions now has a section on `new_R_ps()`.


# Recca 0.1.13 (2018-12-21)

* `calc_F_footprint_effects()` now cleans rows and columns 
  (deletes rows and columns containing all 0s)
  before calculating row and column fractions.
* `calc_F_footprint_effects()` now calculates row and column fractions
  using `matsbyname::fractionize_byname()`, 
  providing a speed boost.


# Recca 0.1.12 (2018-12-02)

* Added a vignette discussing interactions among `new_*` functions.


# Recca 0.1.11 (2018-12-02)

* Updates to embodied energy calculations
  to conform to updated `matsbyname::hatinv_byname()` which handles `0` values better.


# Recca 0.1.10 (2018-11-29)

* Now importing `magrittr` pipe at high level.
* Now using `matsbyname::hatinv_byname()` function 
  instead of `matsbyname::hatize_byname() %>% matsbyname::invert_byname()`.
* Breaking change: arguments to `Recca` functions are no longer named `*_colname`.
  I'm standardizing argument names.
  The new approach yields better code when calling functions with individual matrices:
  every matrix needs to be named.
  And when using functions to operate on a data frame the default values for column names
  mean that data frames can be piped to functions that are called with no parameters.
  Furthermore, this change allows lists output from previous functions 
  to be used in later function calls without name changes. 
  For example, `f1` produces a list `l` with named item `z`, and 
  `z` is an input to `f2`. 
  The previous naming convention would require user mapping `z` to `z_colname`:
  `f2(z_colname = l$z)`.
  The new naming convention allows
  `f2(l)`, because `l` contains named item `z`, and `f2` has an argument new named `z`
  (where it had been named `z_colname` before).
  In all ways, the new argument naming convention is more elegant than the previous `*_colname` approach.
* Building a vignette that describes the package.
* New test for a simple, 1-industry ECC.
* Now up to 316 tests.


# Recca 0.1.9 (2018-11-08)

* Breaking change: `reconstruct_UV` --> `new_Y`. 
  This change allows for several `new_*` functions that assess changes to the ECC
  when some part of the ECC changes.
* New function `new_k_ps` assesses changes to the ECC when 
  proportion of inputs to an industry change 
  under the assumption of perfect substitution.
* New functions `products_unit_homogeneous`, `inputs_unit_homogeneous`, and `outputs_unit_homogeneous`
  test whether products, inputs to all industries, and outputs from all industries are unit-homogeneous.


# Recca 0.1.8 (2018-07-27)

* Added new function `node_list` which creates a node list from an edge list.


# Recca 0.1.7 (2018-07-26)

* Added new function `edge_list` which creates an edge list from U, V, and Y matrices.


# Recca 0.1.6 (2018-07-09)

* Fixed a bug in `verify_IEATable_energy_balance`. 
  `err` --> `as.name(err)`.
* Fixed a bug in `S_units_from_tidy`.
  Needed to change an argument name for `matsindf::collapse_to_matrices`
  from `values` --> `matvals`.


# Recca 0.1.5 (2018-07-02)

* Added a column in the output of `verify_SUT_industry_production` that 
  indicates which industries are problematic.


# Recca 0.1.4 (2018-06-28)

* Fixed a bug in the `verify_IEATable_energy_balance` function: !!err --> !!as.name(err)


# Recca 0.1.3 (2018-06-27)

* Added `verify_IEATable_energy_balance` function


# Recca 0.1.2 (2018-05-20)

* All functions now use the `matsindf::matsindf_apply` function
* `F` functions now do internal energy balance checks.
* Much code and API cleanup.
* Note that some API changes are expected, 
  particularly name changes for `*_colname` arguments.
  

# Recca 0.1.1 (2018-04-16)

* New function `calc_io_mats`: 
  calculate input-output (PSUT version) matrices, including `y`, `q`, `g`, `W`, 
  `Z`, `D`, `C`, `A`, `L_ixp`, and `L_pxp`.
* New function `reconstruct_UV`:
  Reconstruct the energy conversion chain (new `U` and `V` matrices) from a new final demand matrix (`Y`)


# Recca 0.1.0 (2018-04-12)

* Initial version
* Includes:
    + `UKEnergy2000tidy`, `UKEnergy2000mats`
    + Functions to check energy balance
    + Functions to perform aggregation calculations for primary energy and final demand
    + Stub LaTeX vignette


