## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(dplyr)
library(matsbyname)
library(matsindf)
library(Recca)
library(tidyr)
library(tibble)

## ------------------------------------------------------------------------
S_units <- UKEnergy2000tidy %>%
  group_by(Country, Year, Energy.type, Last.stage) %>%
  S_units_from_tidy()
glimpse(S_units)

## ------------------------------------------------------------------------
WithNames <- UKEnergy2000tidy %>%
  # Add a column indicating the matrix in which this entry belongs (U, V, or Y).
  add_matnames_iea(use_R = TRUE) %>%
  # Add metadata columns for row names, column names, row types, and column types.
  add_row_col_meta() %>% 
  # Eliminate columns we no longer need
  select(-Ledger.side, -Flow.aggregation.point, -Flow, -Product) %>%
  mutate(
    # Ensure that all energy values are positive, as required for analysis.
    EX.ktoe = abs(EX.ktoe)
  )
head(WithNames)

## ------------------------------------------------------------------------
AsMats <- WithNames %>%
  # Collapse to matrices using functions in the matsindf package
  group_by(Country, Year, Energy.type, Last.stage, matname) %>%
  collapse_to_matrices(matnames = "matname", matvals = "EX.ktoe",
                       rownames = "rowname", colnames = "colname",
                       rowtypes = "rowtype", coltypes = "coltype") %>%
  rename(matrix.name = matname, matrix = EX.ktoe) %>%
  spread(key = matrix.name, value = matrix) %>%
  # Do a little more cleanup
  mutate(
    # Create full U matrix
    U = sum_byname(U_excl_EIOU, U_EIOU),
    # Create r_EIOU, a matrix that identifies the ratio of EIOU to other energy consumed.
    r_EIOU = elementquotient_byname(U_EIOU, U),
    r_EIOU = replaceNaN_byname(r_EIOU, val = 0)
  ) %>%
  select(-U_EIOU, -U_excl_EIOU) %>%
  # Add S_units matrices
  left_join(S_units, by = c("Country", "Year", "Energy.type", "Last.stage")) %>%
  gather(key = matrix.name, value = matrix, R, U, V, Y, r_EIOU, S_units)
glimpse(AsMats)

## ------------------------------------------------------------------------
library(tidyr)
mats <- UKEnergy2000mats %>% 
  spread(key = matrix.name, value = matrix) %>% 
  mutate(
    Last.stage = factor(Last.stage, levels = c("final", "useful", "services")),
    Energy.type = factor(Energy.type, levels = c("E.ktoe", "X.ktoe"))
  ) %>% 
  arrange(Last.stage, Energy.type) # Put rows in a natural order
# Use the calc_io_mats function with individual matrices, 
# each taken from the UKEnergy2000mats data frame.
U <- mats$U[[1]]
V <- mats$V[[1]]
Y <- mats$Y[[1]]
S_units <- mats$S_units[[1]]
IO_list <- calc_io_mats(U = U, V = V, Y = Y, S_units = S_units)

## ------------------------------------------------------------------------
class(IO_list)
names(IO_list)
IO_list[["y"]]

## ------------------------------------------------------------------------
IO_df <- mats %>% calc_io_mats()

## ------------------------------------------------------------------------
class(IO_df)
names(IO_df)
glimpse(IO_df)
IO_df[["y"]][[1]]
IO_df[["y"]][[2]]
IO_df[["y"]][[3]]
IO_df[["y"]][[4]]

