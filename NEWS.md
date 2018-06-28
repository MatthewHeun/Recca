# News for `Recca`

## Recca 0.1.3 (2018-06-27)

* Added `verify_IEATable_energy_balance` function


## Recca 0.1.2 (2018-05-20)

* All functions now use the `matsindf::matsindf_apply` function
* `F` functions now do internal energy balance checks.
* Much code and API cleanup.
* Note that some API changes are expected, 
  particularly name changes for `*_colname` arguments.
  

## Recca 0.1.1 (2018-04-16)

* Many new functions
    + `calc_io_mats`: 
      calculate input-output (PSUT version) matrices, including `y`, `q`, `g`, `W`, 
      `Z`, `D`, `C`, `A`, `L_ixp`, and `L_pxp`.
    + `reconstruct_UV`:
      Reconstruct the energy conversion chain (new `U` and `V` matrices) from a new final demand matrix (`Y`)


## Recca 0.1.0 (2018-04-12)

* Initial version
* Includes:
    + `UKEnergy2000tidy`, `UKEnergy2000mats`
    + Functions to check energy balance
    + Functions to perform aggregation calculations for primary energy and final demand
    + Stub LaTeX vignette


