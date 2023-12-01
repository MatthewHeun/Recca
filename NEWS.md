---
title: "Release notes for `Recca`"
output: html_document
---


Cite all releases with doi [10.5281/zenodo.5226085](https://doi.org/10.5281/zenodo.5226085), 
which always resolves to the latest release.



* Added code of conduct and contributing pages to documentation.


# Recca 0.1.51 (2023-11-15) [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.10137045.svg)](https://doi.org/10.5281/zenodo.10137045)

* Fixed a bug in `calc_agg_eta_pfus()` where
  efficiencies were calculated incorrectly when 
  primary energy was different
  depending on the value of the `Last.stage` column.
* No new tests.
    * Still at 748 tests, all passing.
    * Test coverage remains at 100%.


# Recca 0.1.50 (2023-11-15) [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.10136470.svg)](https://doi.org/10.5281/zenodo.10136470)

* New function `calc_agg_eta_pfus()` calculates
  primary, final, useful, and services aggregates and
  associated efficiencies.
* New tests for new features.
    * Now up to 748 tests, all passing.
    * Test coverage remains at 100%.


# Recca 0.1.49 (2023-11-03) [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.10070427.svg)](https://doi.org/10.5281/zenodo.10070427)

* `remove_neu()` now also removes non-energy use products in the rows of **Y**.
* Now re-exporting `IEATools::nonenergy_use`.
* New tests for new features.
    * Now up to 727 tests, all passing.
    * Test coverage remains at 100%.


# Recca 0.1.48 (2023-10-27) [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.10052140.svg)](https://doi.org/10.5281/zenodo.10052140)

* New function `remove_neu()` eliminates "Non-energy useX"
  (where "X" is any additional string such as " in Industry")
  from the final demand matrix (**Y**)
  and swims upstream with `new_Y()` to remove any primary stage or useful stage
  energy associated with Non-energy use.
* New tests for the new function.
    * Now up to 717 tests, all passing.
    * Test coverage remains at 100%.


# Recca 0.1.47 (2023-10-23) [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.10034476.svg)](https://doi.org/10.5281/zenodo.10034476)

* Fixed a bug where EIOU efficiency calculated by calc_eta_fu_Y_eiou()
  gave the **0** matrix.
  Hat-tip to Emmanuel Aramendia for spotting this issue and assisting the debugging.
* No new tests.
    * Still at 712 tests, all passing.
    * Test coverage remains at 100%.


# Recca 0.1.46 (2023-10-16) [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.10016873.svg)](https://doi.org/10.5281/zenodo.10016873)

* Minor updates to documentation.
* The `by` argument to `pfu_aggregates()` now correctly
  chooses the default value ("Total") if unspecified.
* No new tests.
    * Still at 712 tests, all passing.
    * Test coverage remains at 100%.


# Recca 0.1.45 (2023-09-21) [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.8368788.svg)](https://doi.org/10.5281/zenodo.8368788)

* Complete aggregation vignette.
* No new tests.
    * Still at 712 tests, all passing.
    * Test coverage remains at 100%.


# Recca 0.1.44 (2023-09-21) [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.8366750.svg)](https://doi.org/10.5281/zenodo.8366750)

* Fixed a bug in `node_edge_list()` that caused failure when 
  `rowtypes` or `coltypes` columns were not present.
* No new tests.
    * Still at 712 tests, all passing.
    * Test coverage remains at 100%.


# Recca 0.1.43 (2023-08-18) [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.8264412.svg)](https://doi.org/10.5281/zenodo.8264412)

* Completed the aggregations vignette.
* New function `pfu_aggregates()` is a work in progress
  but should (eventually) make calculating primary, final, and useful
  aggregates easier.
* Breaking change: 
  `U` and `U_feed` arguments removed from `finaldemand_aggregates()`, 
  replaced by `U_eiou`, which was calculated from 
  `U` and `U_feed` internally, and 
  `U_eiou` is assumed to be present 
  in all energy conversion chain representations.
* The `by` argument to `finaldemand_aggregates()` can now be "Industry",
  which is treated as a synonym for "Sector".
* Now returning an `S_units_prime` column when chopping matrices.
* Wrapped some matrices in `as.matrix()` to guard against
  sparse matrices causing a failure in `calc_yqfgW()`.
* New tests for new features.
    * Now up to 712 tests, all passing.
    * Test coverage remains at 100%.


# Recca 0.1.42 (2023-06-02) [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.7998165.svg)](https://doi.org/10.5281/zenodo.7998165)

* Fixed a bug where writing `Matrix` objects failed in
  `write_ecc_to_excel()`. 
  Thanks to Pierre Jacques for reporting this problem.
* New function `calc_eta_fu_Y_eiou()` calculates 
  final-to-useful efficiencies for 
  every entry in **Y** and **U_EIOU** matrices, 
  responding to a feature request from Pierre Jacques.
* New tests for new features.
    * Now up to at 641 tests, all passing.
    * Test coverage remains at 100%.


# Recca 0.1.41 (2023-04-28) [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.7877495.svg)](https://doi.org/10.5281/zenodo.7877495)

* Responded to policy changes in `matsindf::matsindf_apply()`
  by setting default arguments to `NULL` for several internal functions.
* `region_aggregates()` gains `drop_na_few` argument
  that tells whether to eliminate rows 
  with `NA` in `few_colname`.
* Adjusted example ECC names. 
  Example changes include:
    - "Resources - Crude" --> "Resources [of Crude]"
    - "Crude - Fields" --> "Crude [from Fields]"
    - "MD - Car engines" --> "MD [from Car engines]"
  These changes align the example ECC with the naming conventions
  now in use throughout the package and in various pipelines.
* `chop_R()` now also chops by rows (industries), 
  since we are now including 
  all exogenous sources of energy in the **R** matrix.
* New function `get_all_products_and_industries()` generates lists
  of unique product and industry names for an ECC and 
  is aware of `RCLabels`-type `piece`s of row and column names.
* Net EROI calculations removed, only gross values calculated.
* Simplifications reduced number of tests.
    * Still at 632 tests, all passing.
    * Test coverage remains at 100%.
  

# Recca 0.1.40 (2023-01-25) [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.7571202.svg)](https://doi.org/10.5281/zenodo.7571202)

* This version contains only fixes to the continuous integration
  build system. 
  There are no changes to any code.
* Remove use of the `Remotes:` field in DESCRIPTION.
  Instead, now using `pkgdepends` syntax in
  the `extra-packages:` fields of `R_CMD-check.yaml` and `test-coverage.yaml`.
* Update to latest version of the GitHub action for
  test coverage.
* No new tests.
    * Still at 672 tests, all passing.
    * Test coverage remains at 100%.


# Recca 0.1.39 (2023-01-24) [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.7566435.svg)](https://doi.org/10.5281/zenodo.7566435)

* Tests once again all passing on 
  all test rigs of GitHub Actions.
  Tests had been failing due to new code that 
  correctly detects unit errors (inhomogeneity)
  for some upstream swim calculations
  with services as the last stage in an ECC.
* Move tests associated with `verify_cols_missing()`
  from the `Recca` package to `matsindf`.
  Those tests didn't belong in `Recca`,
  because `verify_cols_missing()` is no longer in `Recca`.
* Eliminated data frame rows for a couple tests
  that were producing `NA` values 
  due to unit inhomogeneity. 
* Switched to simpler formulas derived by MKH
  for `new_R_ps()`.
* Eliminated `tidyselect` warnings.
* `write_ecc_to_excel()` now ensures that columns of **U** and rows of **V**
  are the same, even if it means adding zero columns to **U** or 
  zero rows to **V**.
  This change makes further Excel calculations easier.
* `chopR()` now checks for the ability to swim downstream with
  **R_prime** = **R** to re-create the original energy conversion chain, 
  thereby bringing consistency with `chopY()`.
  This approach identifies numerical precision errors
  before they cause a problem.
* `chopR()` now uses `calc_io_mats(direction = "downstream")`,
  thereby consistently using `calc_io_mats()` for both
  upstream and downstream swims.
* `calc_io_mats()` gains new argument `direction`
  that tells whether the input-output matrices
  are for "upstream" or "downstream" swims.
* `finaldemand_aggregates()` now produces true `0`
  instead of a 1x1 matrix without row or column labels
  when no columns names of **U_EIOU** or **Y**
  match `fd_sectors`.
* Fixed two bugs in `new_Y()`. 
  There were compensatory formula errors
  that only appeared in some edge cases.
* Aggregations are now optional (on by default)
  in `chop_R()` and `chop_Y()`.
* Renamed `effects_aggregates()` and `footprint_aggregates()`
  to `chop_R()` and `chop_Y()`.
* New function `effects_aggregates()`
  swims downstream from the **R** matrix to 
  final demand for each column (energy carrier)
  in the **R** matrix and calculates primary and final demand aggregates.
* `footprint_aggregates()` now checks that
  the chopped ECCs sum to the original ECC
  on every calculation.
* `region_aggregates()` now correctly
  eliminates the `few_colname` when
  the incoming data frame has no rows.
* `footprint_aggregates()` now also includes the
  `*_prime_colname` matrices in the nested data frame.
* `calc_io_mats()` and `calc_L()`
  gain `method` and `tol` arguments
  to control matrix inversion.
* `calc_eta_pfd()` now includes 
  columns of names of efficiencies in its output.
  This feature will assist pivoting on efficiencies later.
* New function `footprint_aggregates()` calculates 
  primary and final demand aggregates
  for each isolated row and column in **Y**.
* `new_Y()` now also produced matrices
  **U_feed**, **U_eiou**, and **r_eiou**
  in its output.
* **W** matrix no longer included in the output of `new_Y()`.
  **W** can always be calculated from **V**^T - **U**.
* New function `write_ecc_to_excel()`
  stores ECCs in spatial format in Excel.
  All ECC matrices are written to the Excel file:
  **R**, **U**, **V**, **Y**, 
  **U_feed**, **U_eiou**, **r_eiou**, and **S_units**.
* `primary_aggregates()` and `finaldemand_aggregates()`
  no longer require `p_industries` and `fd_sectors`
  to be present in the incoming list or data frame.
  Rather, `p_industries` and `fd_sectors` are treated 
  as parameters that apply to all items in the incoming list
  or rows in the incoming data frame.
  This change brings consistency with other functions that
  use `matsindf::matsindf_apply()` internally.
* New function `group_aggregates()` that
  aggregates PSUT matrices according to an aggregation map.
* New function `despecified_aggregates()` that
  aggregates PSUT matrices to a piece of row and column labels. 
* New argument `add_net_gross_cols` on `primary_aggregates()` that tells
  whether to add both net and gross primary energy columns.
  (Net and gross primary energy aggregate columns will contain identical values,
  but the presence of both net and gross columns may make it easier to mesh with
  results from the `finaldemand_aggregates()` function, 
  which produces net and gross columns that are different.)
* `aggregate_regions()` now returns an empty data frame
  with the expected columns if the input data frame 
  has no rows.
* Many new tests for new features.
    * Now up to 672 tests, all passing.
    * Test coverage remains at 100%.


# Recca 0.1.38 (2022-04-02) [![DOI](https://zenodo.org/badge/116066838.svg)](https://zenodo.org/badge/latestdoi/116066838)

* Removed unnecessary assertion that `matsbyname` is available
  in aggregation functions.
* Eliminated `finaldemand_aggregates_with_units()`, 
  because nobody was using it.
* New function `calc_eta_pfd()` calculates gross and net
  efficiencies from primary to final demand.
* New function `region_aggregates()` calculates
  regional aggregates according to columns in a data frame.
* New look for `pkgdown` website.
* `extend_to_exergy()` gains new arguments 
  `mat_piece`, `phi_piece`, `notation`, and `prepositions`
  to enable more flexible row and column name matching 
  between the various matrices and phi vectors.
  In addition, `extend_to_exergy()` now uses `matsbyname::vec_from_store_byname()`
  internally to correctly handle hatized vector multiplication.
* Many new tests for new features.
    * Now up to 556 tests, all passing.
    * Test coverage remains at 100%.


# Recca 0.1.37 (2021-10-13) [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.5566713.svg)](https://doi.org/10.5281/zenodo.5566713)

* Added details to `Recca` vignette, including additional equations for embodied matrices. 
* New function `extend_to_exergy()` that calculates
  an exergy representation of an energy conversion chain given
  an energy representation of an energy conversion chain.
* Many new tests for new features.
    * Now up to 530 tests, all passing.
    * Test coverage remains at 100%.


# Recca 0.1.36 (2021-09-10)  [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.5500310.svg)](https://doi.org/10.5281/zenodo.5500310)

* Get started file updated. Some matrices definitions (**G**, **H**, **E**)
  needed to be updated.
* Removed redundant IO matrices (**C_feed**, **D_feed**, and **O_feed**).
* Tests
    * 512 tests
    * Test coverage remains at 100%


# Recca 0.1.35 (2021-08-20)  [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.5226086.svg)](https://doi.org/10.5281/zenodo.5226086)

* R matrix formulation now adopted for input output calculations,
  for upstream and downstream swims, as well as for embodied
  energy calculations.
* Added new file `new_R_ps_example.xlsx` that demonstrates 
  equations for swimming "downstream."
* Updated to stable and active status badges.


# Recca 0.1.34 (2021-04-11)

* Fix lingering problems in R-CMD-check after the move to GitHub Actions
  for continuous integration testing. 
* No new tests.
    * Still at 435 tests, all passing.
    * Test coverage remains at 100%.


# Recca 0.1.33 (2021-04-11)

* Move from Travis to GitHub Actions for continuous integration testing.
* No new tests.
    * Still at 435 tests, all passing.
    * Test coverage remains at 100%.


# Recca 0.1.32 (2021-03-16)

* Modified aggregation functions to use simpler (but verbose)
  `if` statements to choose aggregation functions.
  This change works around an error when calling the aggregation functions
  from other packages.
* New tests for new code paths.
    * Now at 435 tests, all passing.
    * Test coverage remains at 100%.


# Recca 0.1.31 (2021-02-25)

* New argument to aggregation functions: `pattern_type`
  which tells how to match primary and final demand sector names for aggregation.
* New tests for new feature.
    * Now 421 tests, all passing.
    * Test coverage remains at 100%.


# Recca 0.1.30 (2021-02-22)

* Fixed bugs in `finaldemand_aggregates()` and `finaldemand_aggregates_with_units()`
  where argument `fd_sectors` was not respected for gross energy aggregation.
* New and revamped tests to guard against regression.
    * Now 417 tests, all passing.
    * Test coverage remains at 100%.


# Recca 0.1.29 (2021-02-01)

* A `method_q_calculation` argument has been added to
  the `calc_io_mats()` function.
  This enables to calculate the q vector using either
  a consumption-side or supply-side perspective.


# Recca 0.1.28 (2020-12-23)

* New function `find_p_industry_names()` looks at **R**, **V**, and **Y** matrices 
  to find names of primary industries (industries that produce primary energy).
  The function looks for prefixes among row (`R` and `V`) and column (`Y`) names
  to identify primary industries.
* New tests for new function.
    * Now 416 tests, all passing.
    * Test coverage remains at 100%.


# Recca 0.1.27 (2020-12-08)

* Move IEA aggregation functions out of this package and into the `IEATools` package.
* Now using package constants for names of aggregate columns.
  Column names match similar names in `IEATools`.
* Now using better function-matching and -selecting code in `finaldemand_aggregates()`
* Fewer tests due to moving some functions out of the package.
    * Now 411 tests, all passing.
    * Test coverage remains at 100%.


# Recca 0.1.26 (2020-12-06)

* `primary_aggregates()`, `finaldemand_aggregates(), and `finaldemand_aggregates_with_units()`
  now require a column of `p_industries` or `fd_sectors` instead of a vector 
  when a `.sutdata` is a data frame.
  This change works around a problem with a common use case where 
  the caller would create the `fd_sectors` column from the column names of `Y` vectors 
  in the data frame.
* Working to modernize the Recca code.
    * Use R markdown in documentation (`` `term` `` instead of `\code{term}`).
    * Use package constants for column name arguments 
      (`Recca::sankey_cols$sankey` instead of "Sankey").
* New tests for the new feature.
    * Now up to 413 tests, all passing.
    * Test coverage remains at 100%.


# Recca 0.1.25 (2020-09-17)

* New author Emmanuel Aramendia. Welcome!
* Test coverage now up to 100%.
* Created a new function `calc_embodied_EIOU()` that calculates different embodied EIOU matrices for a given final demand
* Created a new function `calc_erois()` that calculates different vector of product-level and industry-level EROIs 
  using different assumptions.
* Created a new function `calc_E_EIOU()` that calculates the `E_EIOU` matrix of EIOU energy use 
  by unit of industry output, split by product.
  It also calculates the direct energy extension vector e_EIOU of total EIOU energy use
  by unit of industry output.
* Fixed a bug in one of the vignettes related to the move from `U_excl_EIOU` to `U_feed`.
* `prep_UK2000.R` and `prep_perfectsub.R` scripts
  fixed to work with `IEATools`, which now uses
  `U_feed` instead of `U_excl_EIOU`.
* Added tests for new features and old bugs.
    * Now up to 407 tests, all of which pass.
    * Test coverage remains at 100%.


# Recca 0.1.24 (2020-03-17)

* New tests for new features and old bugs.
    * Now up to 337 tests, all passing.
    * Test coverage remains at 100%.
* Now using `U_feed` everywhere.
* Calculate `Z_feed` and `A_feed` in `calc_io_mats()`.


# Recca 0.1.23 (2020-03-17)

* Added a `...` argument to `make_sankey()`, which passes arguments to `networkD3::sankeyNetwork()`.


# Recca 0.1.22 (2020-05-02)

* Fixed a bug in `primary_aggregates()`.
  `primary_aggregates()` had been assuming that either the **R** or the **V** matrix (but not both) 
  could contain industries counted in total primary energy supply (production, imports, exports, etc.).
  However, that is not true. 
  **R** is for resource industries, so 
  imports, exports, marine and aviation bunkers, and stock changes
  are excluded from **R**.
  However, 
  imports, exports, marine and aviation bunkers, and stock changes
  _are_ included in total primary energy supply. 
  I removed the conditional `xor` check for appearance of TPES industries in **R** and **V**.
  Everything seems to be working now.


# Recca 0.1.21 (2020-03-27)

* Now up to 332 tests, all of which pass.
* Test coverage now up to 100%.
* Implemented energy balance checking for `new_R_ps()`
  with its own tolerance, by default `1e-6`.


# Recca 0.1.20 (2020-03-27)

* Completed the conversion of the entire package
  to enable use of `R` matrices in the PSUT framework.
  This effort involved converting several functions to accept *either* 
    1) both **R** and **V** matrices in the `R` and `V` arguments or
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
* Removed ecc_layout(). It was not being used, and there is a `create_sankey()` function, anyway.


# Recca 0.1.16 (2019-02-16)

* Now automatically building in Travis.
* Added new `reverse()` function to change flow direction of an ECC.
  `reverse()` will be useful for downstream swim functions.
* Simplified and improved `resource_industries()` function.
    - No longer summing columns of **U** matrix.
    - Instead, cleaning 0 columns of **U** and comparing names of columns to the original **U**.
* Eliminated all `@importFrom` statements by fully-qualifying all function calls.
* Test coverage now up to 100%.


# Recca 0.1.15 (2019-01-09)

* First draft of `new-functions` vignette now complete, including demonstrations of 
  `new_R_ps()` and `new_k_ps()` functions.


# Recca 0.1.14 (2019-01-07)

* New function `make_sankey()` does what it says: makes a Sankey diagram.
  `make_sankey()` is the first function that optionally uses the resource (`R`) matrix.
* `edge_list()` also now accepts a resource (`R`) matrix on input.
* The vignette describing the `new*()` functions now has a section on `new_R_ps()`.


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

* Breaking change: `reconstruct_UV()` --> `new_Y()`.
  This change allows for several `new_*()` functions that assess changes to the ECC
  when some part of the ECC changes.
* New function `new_k_ps()` assesses changes to the ECC when 
  proportion of inputs to an industry change 
  under the assumption of perfect substitution.
* New functions `products_unit_homogeneous()`, `inputs_unit_homogeneous()`, and `outputs_unit_homogeneous()`
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

* All functions now use `matsindf::matsindf_apply()`.
* `F` functions now do internal energy balance checks.
* Much code and API cleanup.
* Note that some API changes are expected, 
  particularly name changes for `*_colname` arguments.
  

# Recca 0.1.1 (2018-04-16)

* New function `calc_io_mats`: 
  calculate input-output (PSUT version) matrices, including **y**, **q**, **g**, **W**, 
  **Z**, **D**, **C**, **A**, **L_ixp**, and **L_pxp**.
* New function `reconstruct_UV()`:
  Reconstruct the energy conversion chain (new `U` and `V` matrices) from a new final demand matrix (`Y`)


# Recca 0.1.0 (2018-04-12)

* Initial version
* Includes:
    + `UKEnergy2000tidy`, `UKEnergy2000mats`
    + Functions to check energy balance
    + Functions to perform aggregation calculations for primary energy and final demand
    + Stub LaTeX vignette


