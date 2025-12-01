# Recca

## Statement of Need

Societal energy analysis and societal exergy analysis (SEA) make
extensive use of the energy conversion chain (ECC), a description of
energy flows through society from the primary stage (resources extracted
from the environment, such as coal, oil, natural gas, wind, and solar),
to the final stage (energy purchased by consumers, such as refined
petroleum and electricity), to the useful stage (energy desired by the
end user, such as heat, motion, and light), and sometimes to energy
services (such as thermal comfort, transport, and illumination). Such
analyses are significantly easier when data are arranged in the **R**,
**U**, **V**, and **Y** matrices of the PSUT framework described by
[Heun et al. (2018)](https://doi.org/10.1016/j.apenergy.2018.05.109).

To facilitate SEA with the PSUT framework, computational tools are
needed. The computational tools should be amenable to use with
[matsindf](https://MatthewHeun.github.io/matsindf/) data frames of ECC
matrices and written with plain names for functions and data.. The
`Recca` package provides such computational tools.

## Installation

You can install `Recca` from github with:

``` r
# install devtools if not already installed
# install.packages("devtools")
devtools::install_github("MatthewHeun/Recca")
# To build vignettes locally, use
devtools::install_github("MatthewHeun/Recca", build_vignettes = TRUE)
```

## History

The functions in this package were first used in the paper [Heun et al.
(2018)](https://doi.org/10.1016/j.apenergy.2018.05.109).

## More Information

Find more information, including vignettes and function documentation,
at <https://MatthewHeun.github.io/Recca/>.

## References

Heun, Matthew Kuperus, Anne Owen, and Paul E. Brockway. 2018. “A
Physical Supply-Use Table Framework for Energy Analysis on the Energy
Conversion Chain.” *Applied Energy* 226 (September): 1134–62.
<https://doi.org/10.1016/j.apenergy.2018.05.109>.
