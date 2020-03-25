###########################################################
context("IO calculations")
###########################################################

test_that("calculating y, q, f, g, W, A, and L works as expected", {
  io_mats <- UKEnergy2000mats %>%
    tidyr::spread(key = matrix.name, value = matrix) %>%
    calc_yqfgW() %>%
    calc_A() %>%
    calc_L()

  # Focus on y, q, f, g, and W
  yqfgW <- io_mats %>%
    dplyr::select(Country, Year, Energy.type, Last.stage, y, q, f, g, W) %>%
    tidyr::gather(key = "matnames", value = "matvals", y, q, f, g, W) %>%
    matsindf::expand_to_tidy(drop = 0)
  expect_equivalent(yqfgW %>%
                      dplyr::filter(Energy.type == IEATools::energy_types$e, Last.stage == IEATools::last_stages$final, matnames == "f", rownames == "Crude dist.", colnames == "Product") %>%
                      dplyr::select(matvals) %>%
                      unlist(),
                    48050)
  expect_equivalent(yqfgW %>%
                      dplyr::filter(Energy.type == IEATools::energy_types$e, Last.stage == IEATools::last_stages$final, matnames == "W", rownames == "Crude - Fields", colnames == "Oil fields") %>%
                      dplyr::select(matvals) %>%
                      unlist(),
                    47500)
  expect_equivalent(yqfgW %>%
                      dplyr::filter(Energy.type == IEATools::energy_types$e, Last.stage == IEATools::last_stages$final, matnames == "g", rownames == "Resources - Crude", colnames == "Product") %>%
                      dplyr::select(matvals) %>%
                      unlist(),
                    50000)
  expect_equivalent(yqfgW %>%
                      dplyr::filter(Energy.type == IEATools::energy_types$e, Last.stage == IEATools::last_stages$services, matnames == "y", rownames == "Illumination [lumen-hrs/yr]", colnames == "Industry") %>%
                      dplyr::select(matvals) %>%
                      unlist(),
                    5e14)
  expect_equivalent(yqfgW %>%
                      dplyr::filter(Energy.type == IEATools::energy_types$x, Last.stage == IEATools::last_stages$services, matnames == "g", rownames == "Petrol dist.", colnames == "Product") %>%
                      dplyr::select(matvals) %>%
                      unlist(),
                    27820)
  expect_equivalent(yqfgW %>%
                      dplyr::filter(Energy.type == IEATools::energy_types$x, Last.stage == IEATools::last_stages$services, matnames == "y", rownames == "Space heating [m3-K]", colnames == "Industry") %>%
                      dplyr::select(matvals) %>%
                      unlist(),
                    7.5e10)

  # Focus on C and A
  CA <- io_mats %>%
    dplyr::select(Country, Year, Energy.type, Last.stage, C, A) %>%
    tidyr::gather(key = "matnames", value = "matvals", C, A) %>%
    matsindf::expand_to_tidy(drop = 0)
  expect_equivalent(CA %>%
                      dplyr::filter(Energy.type == IEATools::energy_types$e, Last.stage == IEATools::last_stages$final, matnames == "A", rownames == "Crude - Dist.", colnames == "Crude - Dist.") %>%
                      dplyr::select(matvals) %>%
                      unlist(),
                    0.01052632)
  expect_equivalent(CA %>%
                      dplyr::filter(Energy.type == IEATools::energy_types$e, Last.stage == IEATools::last_stages$useful, matnames == "A", rownames == "Elect - Grid", colnames == "Diesel") %>%
                      dplyr::select(matvals) %>%
                      unlist(),
                    0.001785714)
  expect_equivalent(CA %>%
                      dplyr::filter(Energy.type == IEATools::energy_types$x, Last.stage == IEATools::last_stages$services, matnames == "C", rownames == "Light", colnames == "Light fixtures") %>%
                      dplyr::select(matvals) %>%
                      unlist(),
                    1)
  expect_equivalent(CA %>%
                      dplyr::filter(Energy.type == IEATools::energy_types$x, Last.stage == IEATools::last_stages$services, matnames == "C", rownames == "Petrol", colnames == "Oil refineries") %>%
                      dplyr::select(matvals) %>%
                      unlist(),
                    0.63095238095238104)

  # Focus on L matrices (L_ixp and L_pxp)
  L <- io_mats %>%
    dplyr::select(Country, Year, Energy.type, Last.stage, L_ixp, L_pxp) %>%
    tidyr::gather(key = "matnames", value = "matvals", L_ixp, L_pxp) %>%
    matsindf::expand_to_tidy(drop = 0)
  expect_equivalent(L %>%
                      dplyr::filter(Energy.type == IEATools::energy_types$e, Last.stage == IEATools::last_stages$final, matnames == "L_ixp", rownames == "Resources - Crude", colnames == "Crude") %>%
                      dplyr::select(matvals) %>%
                      unlist(),
                    1)
  expect_equivalent(L %>%
                      dplyr::filter(Energy.type == IEATools::energy_types$e, Last.stage == IEATools::last_stages$services, matnames == "L_pxp", rownames == "Freight [tonne-km/year]", colnames == "Diesel - Dist.") %>%
                      dplyr::select(matvals) %>%
                      unlist(),
                    142100.9049224)
  expect_equivalent(L %>%
                      dplyr::filter(Energy.type == IEATools::energy_types$e, Last.stage == IEATools::last_stages$useful, matnames == "L_pxp", rownames == "Crude - Dist.", colnames == "NG - Dist.") %>%
                      dplyr::select(matvals) %>%
                      unlist(),
                    0.006179423)
  expect_equivalent(L %>%
                      dplyr::filter(Energy.type == IEATools::energy_types$x, Last.stage == IEATools::last_stages$services, matnames == "L_pxp", rownames == "Crude - Dist.", colnames == "Petrol") %>%
                      dplyr::select(matvals) %>%
                      unlist(),
                    1.1251085047589)
  expect_equivalent(L %>%
                      dplyr::filter(Energy.type == IEATools::energy_types$e, Last.stage == IEATools::last_stages$services, matnames == "L_pxp", rownames == "Freight [tonne-km/year]", colnames == "LTH") %>%
                      dplyr::select(matvals) %>%
                      unlist(),
                    51918.7186937)
})


test_that("calculating IO matrices works as expected", {
  # Calculate all IO matrices
  L_mats <- UKEnergy2000mats %>%
    tidyr::spread(key = matrix.name, value = matrix) %>%
    calc_io_mats() %>%
    # Look at the L matrices, because they depend on everything else.
    dplyr::select(Country, Year, Energy.type, Last.stage, L_ixp, L_pxp) %>%
    tidyr::gather(key = "matnames", value = "matvals", L_ixp, L_pxp) %>%
    matsindf::expand_to_tidy(drop = 0)
  expect_equivalent(L_mats %>%
                      dplyr::filter(Energy.type == IEATools::energy_types$e, Last.stage == IEATools::last_stages$final, matnames == "L_ixp", rownames == "Power plants", colnames == "Crude - Fields") %>%
                      dplyr::select(matvals) %>%
                      unlist(),
                    0.0005505691)
  expect_equivalent(L_mats %>%
                      dplyr::filter(Energy.type == IEATools::energy_types$e, Last.stage == IEATools::last_stages$services, matnames == "L_pxp", rownames == "NG - Wells", colnames == "MD - Truck engines") %>%
                      dplyr::select(matvals) %>%
                      unlist(),
                    0.06720287)
  expect_equivalent(L_mats %>%
                      dplyr::filter(Energy.type == IEATools::energy_types$e, Last.stage == IEATools::last_stages$useful, matnames == "L_pxp", rownames == "Elect - Grid", colnames == "Light") %>%
                      dplyr::select(matvals) %>%
                      unlist(),
                    5.097294813549)
  expect_equivalent(L_mats %>%
                      dplyr::filter(Energy.type == IEATools::energy_types$x, Last.stage == IEATools::last_stages$services, matnames == "L_ixp", rownames == "Oil fields", colnames == "MD - Car engines") %>%
                      dplyr::select(matvals) %>%
                      unlist(),
                    10.84044405228)
})


test_that("calculating IO matrices works as expected", {
  # Make bogus U, V, Y, and S_units matrices
  U <- matrix(c(1, 2,
                3, 4), byrow = TRUE,
              nrow = 2, ncol = 2, dimnames = list(c("p1", "p2"), c("i1", "i2"))) %>%
    matsbyname::setrowtype("products") %>% matsbyname::setcoltype("industries")
  V <- matrix(c(1, 2,
                3, 4), byrow = TRUE,
              nrow = 2, ncol = 2, dimnames = list(c("i1", "i2"), c("p1", "p2"))) %>%
    matsbyname::setrowtype("industries") %>% matsbyname::setcoltype("products")
  Y <- matrix(c(1, 2,
                3, 4), byrow = TRUE,
              nrow = 2, ncol = 2, dimnames = list(c("p1", "p2"), c("s1", "s2"))) %>%
    matsbyname::setrowtype("products") %>% matsbyname::setcoltype("industries")
  S_units <- matrix(c(1, 0,
                      0, 1), byrow = TRUE,
                    nrow = 2, ncol = 2, dimnames = list(c("p1", "p2"), c("m", "K")))

  yqfgW <- calc_yqfgW(U = U, V = V, Y = Y, S_units = S_units)
  # Because the units are not homogeneous, we should receive NA values for f and g vectors.
  f <- yqfgW$f
  expect_true(is.na(f[1, 1]))
  expect_true(is.na(f[2, 1]))
  g <- yqfgW$g
  expect_true(is.na(g[1, 1]))
  expect_true(is.na(g[2, 1]))
})


test_that("NA in g works as expected", {
  io_mats <- UKEnergy2000mats %>%
    tidyr::spread(key = matrix.name, value = matrix) %>%
    calc_yqfgW()
  io_mats <- io_mats[1, ]
  new_g <- io_mats$g[[1]]
  new_g[1, 1] <- NA_real_
  io_mats$g[[1]] <- new_g
  actual <- io_mats %>% calc_A()
  expect_true(is.na(actual$Z[[1]]))
  expect_true(is.na(actual$C[[1]]))
  expect_true(all(is.na(actual$A[[1]])))
})

