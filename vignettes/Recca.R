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
library(tidyr)
mats <- UKEnergy2000mats %>% spread(key = matrix.name, value = matrix)
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
IO_list[["L_ixp"]]

## ------------------------------------------------------------------------
# This code is very clean, because we rely upon names for columns
# given by the default arguments to calc_io_mats.
IO_df <- mats %>% calc_io_mats()

## ------------------------------------------------------------------------
class(IO_df)
names(IO_df)
glimpse(IO_df)
# We find the y vector from the the previous example.
IO_df[["y"]][[1]]
# But there are other y vectors for each row of the data frame.
IO_df[["y"]][[2]]
IO_df[["y"]][[3]]
IO_df[["y"]][[4]]

