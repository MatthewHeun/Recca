* Now importing `magrittr` pipe at high level.
* Now using `hatinv_byname` function instead of `hatize_byname` and `invert_byname` in succession.


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

## Many new functions

* `calc_io_mats`: 
  calculate input-output (PSUT version) matrices, including `y`, `q`, `g`, `W`, 
  `Z`, `D`, `C`, `A`, `L_ixp`, and `L_pxp`.
  
* `reconstruct_UV`:
  Reconstruct the energy conversion chain (new `U` and `V` matrices) from a new final demand matrix (`Y`)


# Recca 0.1.0 (2018-04-12)

* Initial version
* Includes:
    + `UKEnergy2000tidy`, `UKEnergy2000mats`
    + Functions to check energy balance
    + Functions to perform aggregation calculations for primary energy and final demand
    + Stub LaTeX vignette


