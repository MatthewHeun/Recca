###########################################################
context("IO calculations")
###########################################################

test_that("calculating y, q, f, g, h, W, A, and L works as expected", {
  io_mats <- UKEnergy2000mats %>%
    tidyr::spread(key = matrix.name, value = matrix) %>%
    calc_yqfgW() %>%
    calc_A() %>%
    calc_L()

  # Focus on y, q, f, g, h, r, and W
  yqfgW <- io_mats %>%
    dplyr::select(Country, Year, Energy.type, Last.stage, y, q, f, g, W, r, h) %>%
    tidyr::gather(key = "matnames", value = "matvals", y, q, f, g, W, r, h) %>%
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
                      dplyr::filter(Energy.type == IEATools::energy_types$e, Last.stage == IEATools::last_stages$final, matnames == "r", rownames == "Resources - Crude", colnames == "Product") %>%
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
  expect_equivalent(yqfgW %>%
                      dplyr::filter(Energy.type == IEATools::energy_types$e, Last.stage == IEATools::last_stages$final, matnames == "r", rownames == "Resources - NG", colnames == "Product") %>%
                      dplyr::select(matvals) %>%
                      unlist(),
                    43000)
  expect_equivalent(yqfgW %>%
                      dplyr::filter(Energy.type == IEATools::energy_types$e, Last.stage == IEATools::last_stages$final, matnames == "h", rownames == "Industry", colnames == "Crude") %>%
                      dplyr::select(matvals) %>%
                      unlist(),
                    50000)
  expect_equivalent(yqfgW %>%
                      dplyr::filter(Energy.type == IEATools::energy_types$e, Last.stage == IEATools::last_stages$final, matnames == "h", rownames == "Industry", colnames == "NG") %>%
                      dplyr::select(matvals) %>%
                      unlist(),
                    43000)

  # Focus on C, D, O, and A
  CDAO <- io_mats %>%
    dplyr::select(Country, Year, Energy.type, Last.stage, C, A, D, O) %>%
    tidyr::gather(key = "matnames", value = "matvals", C, A, D, O) %>%
    matsindf::expand_to_tidy(drop = 0)

  # A matrix:
  expect_equivalent(CDAO %>%
                      dplyr::filter(Energy.type == IEATools::energy_types$e, Last.stage == IEATools::last_stages$final, matnames == "A", rownames == "Crude - Dist.", colnames == "Crude - Dist.") %>%
                      dplyr::select(matvals) %>%
                      unlist(),
                    0.01052632)
  expect_equivalent(CDAO %>%
                      dplyr::filter(Energy.type == IEATools::energy_types$e, Last.stage == IEATools::last_stages$useful, matnames == "A", rownames == "Elect - Grid", colnames == "Diesel") %>%
                      dplyr::select(matvals) %>%
                      unlist(),
                    0.001785714)
  # C matrix:
  expect_equivalent(CDAO %>%
                      dplyr::filter(Energy.type == IEATools::energy_types$x, Last.stage == IEATools::last_stages$services, matnames == "C", rownames == "Light", colnames == "Light fixtures") %>%
                      dplyr::select(matvals) %>%
                      unlist(),
                    1)
  expect_equivalent(CDAO %>%
                      dplyr::filter(Energy.type == IEATools::energy_types$x, Last.stage == IEATools::last_stages$services, matnames == "C", rownames == "Petrol", colnames == "Oil refineries") %>%
                      dplyr::select(matvals) %>%
                      unlist(),
                    0.63095238095238104)
  # D matrix:
  expect_equivalent(CDAO %>%
                      dplyr::filter(Energy.type == IEATools::energy_types$x, Last.stage == IEATools::last_stages$services, matnames == "D", rownames == "Oil fields", colnames == "Crude - Fields") %>%
                      dplyr::select(matvals) %>%
                      unlist(),
                    1)
  expect_equivalent(CDAO %>%
                      dplyr::filter(Energy.type == IEATools::energy_types$x, Last.stage == IEATools::last_stages$services, matnames == "D", rownames == "Power plants", colnames == "Elect") %>%
                      dplyr::select(matvals) %>%
                      unlist(),
                    1)

  D_x_services <- io_mats$D[[1]]
  expect_equivalent(D_x_services[["Oil fields", "Diesel"]], 0)

  sum_D <- matsbyname::colsums_byname(D_x_services)
  # All colsums should be either 1 or zero.
  expect_true(
    all(
      (sum_D == 1 || sum_D == 0)
    )
  )


  # O matrix:
  expect_equivalent(CDAO %>%
                      dplyr::filter(Energy.type == IEATools::energy_types$x, Last.stage == IEATools::last_stages$services, matnames == "O", rownames == "Resources - Crude", colnames == "Crude") %>%
                      dplyr::select(matvals) %>%
                      unlist(),
                    1)
  expect_equivalent(CDAO %>%
                      dplyr::filter(Energy.type == IEATools::energy_types$x, Last.stage == IEATools::last_stages$services, matnames == "O", rownames == "Resources - NG", colnames == "NG") %>%
                      dplyr::select(matvals) %>%
                      unlist(),
                    1)
  expect_equivalent(CDAO %>%
                      dplyr::filter(Energy.type == IEATools::energy_types$e, Last.stage == IEATools::last_stages$final, matnames == "O", rownames == "Resources - Crude", colnames == "Crude") %>%
                      dplyr::select(matvals) %>%
                      unlist(),
                    1)
  expect_equivalent(CDAO %>%
                      dplyr::filter(Energy.type == IEATools::energy_types$e, Last.stage == IEATools::last_stages$services, matnames == "O", rownames == "Resources - NG", colnames == "NG") %>%
                      dplyr::select(matvals) %>%
                      unlist(),
                    1)

  O_x_services <- io_mats$O[[1]]
  expect_equivalent(O_x_services[["Resources - Crude", "NG"]], 0)
  expect_equivalent(O_x_services[["Resources - NG", "Crude"]], 0)

  sum_O <- matsbyname::colsums_byname(O_x_services)
  # All colsums should be either 1 or zero.
  expect_true(
    all(
      (sum_O == 1 || sum_O == 0)
    )
  )


  # Focus on L matrices (L_ixp and L_pxp)
  L <- io_mats %>%
    dplyr::select(Country, Year, Energy.type, Last.stage, L_ixp, L_pxp) %>%
    tidyr::gather(key = "matnames", value = "matvals", L_ixp, L_pxp) %>%
    matsindf::expand_to_tidy(drop = 0)
  expect_equal(L %>%
                dplyr::filter(Energy.type == IEATools::energy_types$e, Last.stage == IEATools::last_stages$final, matnames == "L_ixp", rownames == "Resources - Crude", colnames == "Crude") %>%
                dplyr::select(matvals) %>%
                 dplyr::pull() %>%
                length(),
                    0)
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

  # Now, focus on whether we get the same q for the two methods: sum_U_Y_rows and sum_R_V_cols
  # First, sum_U_Y_rows
  q_mats_U_Y <- UKEnergy2000mats %>%
    tidyr::spread(key = matrix.name, value = matrix) %>%
    calc_yqfgW()

  q_mats_R_V <- UKEnergy2000mats %>%
    tidyr::spread(key = matrix.name, value = matrix) %>%
    calc_yqfgW(method_q_calculation = "sum_R_V_cols")

  expect_equal(
    q_mats_U_Y[["q"]][[1]], q_mats_R_V[["q"]][[1]]
  )

  expect_identical(
    q_mats_U_Y[["q"]][[1]], q_mats_R_V[["q"]][[1]]
  )

  expect_error(
    UKEnergy2000mats %>%
      tidyr::spread(key = matrix.name, value = matrix) %>%
      calc_yqfgW(method_q_calculation = "Method_not_supported")
  )

})


test_that("calculating Z_feed works as expected", {
  # Calculate all IO matrices
  Z_feed_mats <- UKEnergy2000mats %>%
    tidyr::spread(key = matrix.name, value = matrix) %>%
    dplyr::mutate(
      U_EIOU = matsbyname::hadamardproduct_byname(r_EIOU, U),
      U_excl_EIOU = matsbyname::difference_byname(U, U_EIOU)
    ) %>%
    calc_yqfgW() %>%
    calc_A(U = "U_excl_EIOU") %>%
    dplyr::rename(
      Z_feed = Z
    )

  # The expected values for the E_EIOU matrix and e_EIOU vector were calculated in LibreOffice.
  # See file "UK_2000_EROI_example.ods"
  # --- EAR, September 1st 2020

  # Now test Z_feed for correctness.
  Z_feed_final <- Z_feed_mats$Z_feed[[1]]
  expect_equal(Z_feed_final["Elect", "Elect. grid"], 1.019920319)
  expect_equal(Z_feed_final["NG - Dist.", "Power plants"], 2.5)
  expect_equal(Z_feed_final["Crude - Fields", "Crude dist."], 1)
  expect_equal(Z_feed_final["Petrol", "Gas wells & proc."], 0)
})


test_that("calc_io_mats give correct _feed matrices", {
  feed_mats <- UKEnergy2000mats %>%
    tidyr::spread(key = matrix.name, value = matrix) %>%
    dplyr::mutate(
      U_EIOU = matsbyname::hadamardproduct_byname(r_EIOU, U),
      U_feed = matsbyname::difference_byname(U, U_EIOU)
    ) %>%
    calc_io_mats()

  # The expected values for the E_EIOU matrix and e_EIOU vector were calculated in LibreOffice.
  # See file "UK_2000_EROI_example.ods"
  # --- EAR, September 1st 2020

  # Check Z_feed
  Z_feed_final <- feed_mats$Z_feed[[1]]
  expect_equal(Z_feed_final["Elect", "Elect. grid"], 1.019920319)
  expect_equal(Z_feed_final["NG - Dist.", "Power plants"], 2.5)
  expect_equal(Z_feed_final["Crude - Fields", "Crude dist."], 1)
  expect_equal(Z_feed_final["Petrol", "Gas wells & proc."], 0)

  # Check A_feed
  A_feed_final <- feed_mats$A_feed[[1]]
  expect_equal(A_feed_final["NG - Dist.", "Elect"], 2.5)
  expect_equal(A_feed_final["Elect", "Elect - Grid"], 1.019920319)
  expect_equal(A_feed_final["Petrol", "Petrol - Dist."], 1)
  expect_equal(A_feed_final["Crude", "Diesel"], 0)



  # Check L_pxp_feed
  L_pxp_feed_final <- feed_mats$L_pxp_feed[[1]]
  expect_equal(L_pxp_feed_final["NG", "Elect"], 2.5)
  expect_equal(L_pxp_feed_final["NG - Dist.", "Elect - Grid"], 2.549800797)
  expect_equal(L_pxp_feed_final["NG - Wells", "Elect - Grid"], 2.549800797)
  expect_equal(L_pxp_feed_final["Crude - Dist.", "Petrol"], 1)
  expect_equal(L_pxp_feed_final["Diesel", "Elect"], 0)
  expect_equal(L_pxp_feed_final["Diesel", "Diesel - Dist."], 1)


  # Check L_ixp_feed
  L_ixp_feed_final <- feed_mats$L_ixp_feed[[1]]
  expect_equal(L_ixp_feed_final["NG dist.", "Elect"], 2.5)
  expect_equal(L_ixp_feed_final["Elect. grid", "NG"], 0)
  expect_equal(L_ixp_feed_final["Gas wells & proc.", "Elect - Grid"], 2.549800797)
  expect_equal(L_ixp_feed_final["Power plants", "Elect - Grid"], 1.019920319)
  expect_equal(L_ixp_feed_final["Petrol dist.", "Petrol - Dist."], 1)


  # Check D_feed
  D_feed_final <- feed_mats$D_feed[[1]]
  D_final <- feed_mats$D[[1]]
  expect_identical(D_final, D_feed_final)

  # Check C_feed
  C_feed_final <- feed_mats$C_feed[[1]]
  C_final <- feed_mats$C[[1]]
  expect_identical(C_final, C_feed_final)

  # Check O_feed
  O_feed_final <- feed_mats$O_feed[[1]]
  O_final <- feed_mats$O[[1]]
  expect_identical(O_feed_final, O_final)
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
  f <- yqfgW[["f"]]
  expect_true(is.na(f[1, 1]))
  expect_true(is.na(f[2, 1]))
  g <- yqfgW[["g"]]
  expect_true(is.na(g[1, 1]))
  expect_true(is.na(g[2, 1]))
  # Here add for h vector.
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

