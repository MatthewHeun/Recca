library(magrittr)
library(tidyr)


###########################################################
context("IO calculations")
###########################################################

test_that("calculating y, q, f, g, W, A, and L works as expected", {
  io_mats <- UKEnergy2000mats %>%
    spread(key = matrix.name, value = matrix) %>%
    calc_yqfgW() %>%
    calc_A() %>%
    calc_L()

  # Focus on y, q, f, g, and W
  yqfgW <- io_mats %>%
    select(Country, Year, Energy.type, Last.stage, y, q, f, g, W) %>%
    gather(key = "matnames", value = "matvals", y, q, f, g, W) %>%
    expand_to_tidy(drop = 0)
  expect_equivalent(yqfgW %>%
                      filter(Energy.type == "E.ktoe", Last.stage == "final", matnames == "f", rownames == "Crude dist.", colnames == "Product") %>% select(matvals) %>% unlist(),
                    48050)
  expect_equivalent(yqfgW %>%
                      filter(Energy.type == "E.ktoe", Last.stage == "final", matnames == "W", rownames == "Crude - Fields", colnames == "Oil fields") %>% select(matvals) %>% unlist(),
                    47500)
  expect_equivalent(yqfgW %>%
                      filter(Energy.type == "E.ktoe", Last.stage == "final", matnames == "g", rownames == "Resources - Crude", colnames == "Product") %>% select(matvals) %>% unlist(),
                    50000)
  expect_equivalent(yqfgW %>%
                      filter(Energy.type == "E.ktoe", Last.stage == "services", matnames == "y", rownames == "Illumination [lumen-hrs/yr]", colnames == "Industry") %>% select(matvals) %>% unlist(),
                    5e14)
  expect_equivalent(yqfgW %>%
                      filter(Energy.type == "X.ktoe", Last.stage == "services", matnames == "g", rownames == "Petrol dist.", colnames == "Product") %>% select(matvals) %>% unlist(),
                    27820)
  expect_equivalent(yqfgW %>%
                      filter(Energy.type == "X.ktoe", Last.stage == "services", matnames == "y", rownames == "Space heating [m3-K]", colnames == "Industry") %>% select(matvals) %>% unlist(),
                    7.5e10)

  # Focus on C and A
  CA <- io_mats %>%
    select(Country, Year, Energy.type, Last.stage, C, A) %>%
    gather(key = "matnames", value = "matvals", C, A) %>%
    expand_to_tidy(drop = 0)
  expect_equivalent(CA %>%
                      filter(Energy.type == "E.ktoe", Last.stage == "final", matnames == "A", rownames == "Crude - Dist.", colnames == "Crude - Dist.") %>% select(matvals) %>% unlist(),
                    0.01052632)
  expect_equivalent(CA %>%
                      filter(Energy.type == "E.ktoe", Last.stage == "useful", matnames == "A", rownames == "Elect - Grid", colnames == "Diesel") %>% select(matvals) %>% unlist(),
                    0.001785714)
  expect_equivalent(CA %>%
                      filter(Energy.type == "X.ktoe", Last.stage == "services", matnames == "C", rownames == "Light", colnames == "Light fixtures") %>% select(matvals) %>% unlist(),
                    1)
  expect_equivalent(CA %>%
                      filter(Energy.type == "X.ktoe", Last.stage == "services", matnames == "C", rownames == "Petrol", colnames == "Oil refineries") %>% select(matvals) %>% unlist(),
                    0.63095238095238104)

  # Focus on L matrices (L_ixp and L_pxp)
  L <- io_mats %>%
    select(Country, Year, Energy.type, Last.stage, L_ixp, L_pxp) %>%
    gather(key = "matnames", value = "matvals", L_ixp, L_pxp) %>%
    expand_to_tidy(drop = 0)
  expect_equivalent(L %>%
                      filter(Energy.type == "E.ktoe", Last.stage == "final", matnames == "L_ixp", rownames == "Resources - Crude", colnames == "Crude") %>% select(matvals) %>% unlist(),
                    1)
  expect_equivalent(L %>%
                      filter(Energy.type == "E.ktoe", Last.stage == "services", matnames == "L_pxp", rownames == "Freight [tonne-km/year]", colnames == "Diesel - Dist.") %>% select(matvals) %>% unlist(),
                    142100.9049224)
  expect_equivalent(L %>%
                      filter(Energy.type == "E.ktoe", Last.stage == "useful", matnames == "L_pxp", rownames == "Crude - Dist.", colnames == "NG - Dist.") %>% select(matvals) %>% unlist(),
                    0.006179423)
  expect_equivalent(L %>%
                      filter(Energy.type == "X.ktoe", Last.stage == "services", matnames == "L_pxp", rownames == "Crude - Dist.", colnames == "Petrol") %>% select(matvals) %>% unlist(),
                    1.1251085047589)
  expect_equivalent(L %>%
                      filter(Energy.type == "X.ktoe", Last.stage == "services", matnames == "L_pxp", rownames == "Space heating [m3-K]", colnames == "Space heating [m3-K]") %>% select(matvals) %>% unlist(),
                    1)
})


test_that("calculating IO matrices works as expected", {
  # Calculate all IO matrices
  io_mats <- UKEnergy2000mats %>%
    spread(key = matrix.name, value = matrix) %>%
    calc_io_mats()
  Recca:::test_against_file(io_mats, "expected_iomats.rds", update = FALSE)
})


test_that("calculating IO matrices works as expected", {
  # Make bogus U, V, Y, and S_units matrices
  U <- matrix(c(1, 2,
                3, 4), byrow = TRUE,
              nrow = 2, ncol = 2, dimnames = list(c("p1", "p2"), c("i1", "i2"))) %>%
    setrowtype("products") %>% setcoltype("industries")
  V <- matrix(c(1, 2,
                3, 4), byrow = TRUE,
              nrow = 2, ncol = 2, dimnames = list(c("i1", "i2"), c("p1", "p2"))) %>%
    setrowtype("industries") %>% setcoltype("products")
  Y <- matrix(c(1, 2,
                3, 4), byrow = TRUE,
              nrow = 2, ncol = 2, dimnames = list(c("p1", "p2"), c("s1", "s2"))) %>%
    setrowtype("products") %>% setcoltype("industries")
  S_units <- matrix(c(1, 0,
                      0, 1), byrow = TRUE,
                    nrow = 2, ncol = 2, dimnames = list(c("p1", "p2"), c("m", "K")))

  yqfgW <- calc_yqfgW(U_colname = U, V_colname = V, Y_colname = Y, S_units = S_units)
  # Because the units are not homogeneous, we should receive NA values for f and g vectors.
  f <- yqfgW$f
  expect_true(is.na(f[1, 1]))
  expect_true(is.na(f[2, 1]))
  g <- yqfgW$g
  expect_true(is.na(g[1, 1]))
  expect_true(is.na(g[2, 1]))
})


