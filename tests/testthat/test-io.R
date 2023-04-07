
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
                      dplyr::filter(Energy.type == IEATools::energy_types$e, Last.stage == IEATools::last_stages$final, matnames == "W", rownames == "Crude [from Fields]", colnames == "Oil fields") %>%
                      dplyr::select(matvals) %>%
                      unlist(),
                    47500)
  expect_equivalent(yqfgW %>%
                      dplyr::filter(Energy.type == IEATools::energy_types$e, Last.stage == IEATools::last_stages$final, matnames == "r", rownames == "Resources [of Crude]", colnames == "Product") %>%
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
                      dplyr::filter(Energy.type == IEATools::energy_types$e, Last.stage == IEATools::last_stages$final, matnames == "r", rownames == "Resources [of NG]", colnames == "Product") %>%
                      dplyr::select(matvals) %>%
                      unlist(),
                    43000)
  expect_equivalent(yqfgW %>%
                      dplyr::filter(Energy.type == IEATools::energy_types$e, Last.stage == IEATools::last_stages$final, matnames == "h", rownames == "Crude", colnames == "Industry") %>%
                      dplyr::select(matvals) %>%
                      unlist(),
                    50000)
  expect_equivalent(yqfgW %>%
                      dplyr::filter(Energy.type == IEATools::energy_types$e, Last.stage == IEATools::last_stages$final, matnames == "h", rownames == "NG", colnames == "Industry") %>%
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
                      dplyr::filter(Energy.type == IEATools::energy_types$e, Last.stage == IEATools::last_stages$final, matnames == "A", rownames == "Crude [from Dist.]", colnames == "Crude [from Dist.]") %>%
                      dplyr::select(matvals) %>%
                      unlist(),
                    0.01052632)
  expect_equivalent(CDAO %>%
                      dplyr::filter(Energy.type == IEATools::energy_types$e, Last.stage == IEATools::last_stages$useful, matnames == "A", rownames == "Elect [from Grid]", colnames == "Diesel") %>%
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
                      dplyr::filter(Energy.type == IEATools::energy_types$x, Last.stage == IEATools::last_stages$services, matnames == "D", rownames == "Oil fields", colnames == "Crude [from Fields]") %>%
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
  # expect_true(
  #   all(
  #     (sum_D == 1 || sum_D == 0)
  #   )
  # )
  # Use a trick here. TRUE is counted as 1 in arithmetic, and FALSE is counted as 0.
  # When we add are_1 and are_0, we're adding logicals together.
  # If we expect everything to be either a 1 or a 0, we should get only 1 when we sum them.
  are_1 <- abs(sum_D - 1) < 1e-10
  are_0 <- abs(sum_D) < 1e-10
  expect_true(all(are_1 + are_0))


  # O matrix:
  expect_equivalent(CDAO %>%
                      dplyr::filter(Energy.type == IEATools::energy_types$x, Last.stage == IEATools::last_stages$services, matnames == "O", rownames == "Resources [of Crude]", colnames == "Crude") %>%
                      dplyr::select(matvals) %>%
                      unlist(),
                    1)
  expect_equivalent(CDAO %>%
                      dplyr::filter(Energy.type == IEATools::energy_types$x, Last.stage == IEATools::last_stages$services, matnames == "O", rownames == "Resources [of NG]", colnames == "NG") %>%
                      dplyr::select(matvals) %>%
                      unlist(),
                    1)
  expect_equivalent(CDAO %>%
                      dplyr::filter(Energy.type == IEATools::energy_types$e, Last.stage == IEATools::last_stages$final, matnames == "O", rownames == "Resources [of Crude]", colnames == "Crude") %>%
                      dplyr::select(matvals) %>%
                      unlist(),
                    1)
  expect_equivalent(CDAO %>%
                      dplyr::filter(Energy.type == IEATools::energy_types$e, Last.stage == IEATools::last_stages$services, matnames == "O", rownames == "Resources [of NG]", colnames == "NG") %>%
                      dplyr::select(matvals) %>%
                      unlist(),
                    1)

  O_x_services <- io_mats$O[[1]]
  expect_equivalent(O_x_services[["Resources [of Crude]", "NG"]], 0)
  expect_equivalent(O_x_services[["Resources [of NG]", "Crude"]], 0)

  sum_O <- matsbyname::colsums_byname(O_x_services)
  # All colsums should be either 1 or zero.
  # expect_true(
  #   all(
  #     (sum_O == 1 || sum_O == 0)
  #   )
  # )
  are_1 <- abs(sum_O - 1) < 1e-10
  are_0 <- abs(sum_O) < 1e-10
  expect_true(all(are_1 + are_0))




  # Focus on L matrices (L_ixp and L_pxp)
  L <- io_mats %>%
    dplyr::select(Country, Year, Energy.type, Last.stage, L_ixp, L_pxp) %>%
    tidyr::gather(key = "matnames", value = "matvals", L_ixp, L_pxp) %>%
    matsindf::expand_to_tidy(drop = 0)
  expect_equal(L %>%
                dplyr::filter(Energy.type == IEATools::energy_types$e, Last.stage == IEATools::last_stages$final, matnames == "L_ixp", rownames == "Resources [of Crude]", colnames == "Crude") %>%
                dplyr::select(matvals) %>%
                 dplyr::pull() %>%
                length(),
                    0)
  expect_equivalent(L %>%
                      dplyr::filter(Energy.type == IEATools::energy_types$e, Last.stage == IEATools::last_stages$services, matnames == "L_pxp", rownames == "Freight [tonne-km/year]", colnames == "Diesel [from Dist.]") %>%
                      dplyr::select(matvals) %>%
                      unlist(),
                    142100.9049224)
  expect_equivalent(L %>%
                      dplyr::filter(Energy.type == IEATools::energy_types$e, Last.stage == IEATools::last_stages$useful, matnames == "L_pxp", rownames == "Crude [from Dist.]", colnames == "NG [from Dist.]") %>%
                      dplyr::select(matvals) %>%
                      unlist(),
                    0.006179423)
  expect_equivalent(L %>%
                      dplyr::filter(Energy.type == IEATools::energy_types$x, Last.stage == IEATools::last_stages$services, matnames == "L_pxp", rownames == "Crude [from Dist.]", colnames == "Petrol") %>%
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
  expect_equal(Z_feed_final["NG [from Dist.]", "Power plants"], 2.5)
  expect_equal(Z_feed_final["Crude [from Fields]", "Crude dist."], 1)
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
  expect_equal(Z_feed_final["NG [from Dist.]", "Power plants"], 2.5)
  expect_equal(Z_feed_final["Crude [from Fields]", "Crude dist."], 1)
  expect_equal(Z_feed_final["Petrol", "Gas wells & proc."], 0)

  # Check A_feed
  A_feed_final <- feed_mats$A_feed[[1]]
  expect_equal(A_feed_final["NG [from Dist.]", "Elect"], 2.5)
  expect_equal(A_feed_final["Elect", "Elect [from Grid]"], 1.019920319)
  expect_equal(A_feed_final["Petrol", "Petrol [from Dist.]"], 1)
  expect_equal(A_feed_final["Crude", "Diesel"], 0)



  # Check L_pxp_feed
  L_pxp_feed_final <- feed_mats$L_pxp_feed[[1]]
  expect_equal(L_pxp_feed_final["NG", "Elect"], 2.5)
  expect_equal(L_pxp_feed_final["NG [from Dist.]", "Elect [from Grid]"], 2.549800797)
  expect_equal(L_pxp_feed_final["NG [from Wells]", "Elect [from Grid]"], 2.549800797)
  expect_equal(L_pxp_feed_final["Crude [from Dist.]", "Petrol"], 1)
  expect_equal(L_pxp_feed_final["Diesel", "Elect"], 0)
  expect_equal(L_pxp_feed_final["Diesel", "Diesel [from Dist.]"], 1)


  # Check L_ixp_feed
  L_ixp_feed_final <- feed_mats$L_ixp_feed[[1]]
  expect_equal(L_ixp_feed_final["NG dist.", "Elect"], 2.5)
  expect_equal(L_ixp_feed_final["Elect. grid", "NG"], 0)
  expect_equal(L_ixp_feed_final["Gas wells & proc.", "Elect [from Grid]"], 2.549800797)
  expect_equal(L_ixp_feed_final["Power plants", "Elect [from Grid]"], 1.019920319)
  expect_equal(L_ixp_feed_final["Petrol dist.", "Petrol [from Dist.]"], 1)
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
                      dplyr::filter(Energy.type == IEATools::energy_types$e, Last.stage == IEATools::last_stages$final, matnames == "L_ixp", rownames == "Power plants", colnames == "Crude [from Fields]") %>%
                      dplyr::select(matvals) %>%
                      unlist(),
                    0.0005505691)
  expect_equivalent(L_mats %>%
                      dplyr::filter(Energy.type == IEATools::energy_types$e, Last.stage == IEATools::last_stages$services, matnames == "L_pxp", rownames == "NG [from Wells]", colnames == "MD [from Truck engines]") %>%
                      dplyr::select(matvals) %>%
                      unlist(),
                    0.06720287)
  expect_equivalent(L_mats %>%
                      dplyr::filter(Energy.type == IEATools::energy_types$e, Last.stage == IEATools::last_stages$useful, matnames == "L_pxp", rownames == "Elect [from Grid]", colnames == "Light") %>%
                      dplyr::select(matvals) %>%
                      unlist(),
                    5.097294813549)
  expect_equivalent(L_mats %>%
                      dplyr::filter(Energy.type == IEATools::energy_types$x, Last.stage == IEATools::last_stages$services, matnames == "L_ixp", rownames == "Oil fields", colnames == "MD [from Car engines]") %>%
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


test_that("calc_A() works as expected for downstream swim", {
  io_mats <- UKEnergy2000mats %>%
    tidyr::spread(key = matrix.name, value = matrix) %>%
    calc_yqfgW() %>%
    calc_A(direction = "downstream")
  # Gather some row and column types for later use.
  rowtypeC_s <- io_mats[["C_s"]][[1]] %>% matsbyname::rowtype()
  coltypeC_s <- io_mats[["C_s"]][[1]] %>% matsbyname::coltype()
  rowtypeZ_s <- io_mats[["Z_s"]][[1]] %>% matsbyname::rowtype()
  coltypeZ_s <- io_mats[["Z_s"]][[1]] %>% matsbyname::coltype()

  expect_true("C_s" %in% colnames(io_mats))
  expect_true("Z_s" %in% colnames(io_mats))
  expect_true("D_s" %in% colnames(io_mats))
  expect_true("D_feed_s" %in% colnames(io_mats))
  expect_true("O_s" %in% colnames(io_mats))
  expect_true("B" %in% colnames(io_mats))

  expect_true(!("C" %in% colnames(io_mats)))
  expect_true(!("Z" %in% colnames(io_mats)))
  expect_true(!("D" %in% colnames(io_mats)))
  expect_true(!("K" %in% colnames(io_mats)))
  expect_true(!("O" %in% colnames(io_mats)))
  expect_true(!("A" %in% colnames(io_mats)))

  # Check some values in the resulting matrices.
  expect_equal(io_mats$Z_s[[1]]["Crude [from Fields]", "Oil fields"], 0.9510223490)
  expect_equal(io_mats$D_s[[2]]["Furnaces", "NG [from Dist.]"], 0.6097560976)
  expect_equal(io_mats$D_feed_s[[3]]["Power plants", "NG [from Dist.]"], 0.3902439)
  expect_equal(io_mats$O_s[[4]]["Freight [tonne-km/year]", "Transport"], 0.9527775309)
  expect_equal(io_mats$B[[3]]["Diesel [from Dist.]", "MD [from Truck engines]"], 0.009762856)

# Try when one of the f vector entries is NA
  temp <- UKEnergy2000mats %>%
    tidyr::spread(key = matrix.name, value = matrix) %>%
    # Eliminate the Services rows, because they are not unit homogeneous.
    dplyr::filter(!(.data[[Recca::psut_cols$last_stage]] %in% "Services")) %>%
    calc_yqfgW()
  # Set one of the values in the first f vector to NA for this test.
  temp$f[[1]][, 1][1] <- NA_real_
  io_mats_NA <- temp %>%
    calc_A(direction = "downstream")
  expect_equal(io_mats_NA[["C_s"]][[1]] %>% matsbyname::rowtype(), rowtypeC_s)
  expect_equal(io_mats_NA[["C_s"]][[1]] %>% matsbyname::coltype(), coltypeC_s)
  expect_equal(io_mats_NA[["Z_s"]][[1]] %>% matsbyname::rowtype(), rowtypeZ_s)
  expect_equal(io_mats_NA[["Z_s"]][[1]] %>% matsbyname::coltype(), coltypeZ_s)
  expect_true(is.na(io_mats_NA[["C_s"]][[1]]))
  expect_true(is.na(io_mats_NA[["Z_s"]][[1]]))
})


test_that("calc_G() is an alias for calc_L()", {
  io_mats_L <- UKEnergy2000mats %>%
    tidyr::spread(key = matrix.name, value = matrix) %>%
    # Eliminate the Services rows, because they are not unit homogeneous.
    dplyr::filter(!(.data[[Recca::psut_cols$last_stage]] %in% "Services")) %>%
    calc_yqfgW() %>%
    calc_A(direction = "downstream") %>%
    calc_L(direction = "downstream")
  io_mats_G <- UKEnergy2000mats %>%
    tidyr::spread(key = matrix.name, value = matrix) %>%
    # Eliminate the Services rows, because they are not unit homogeneous.
    dplyr::filter(!(.data[[Recca::psut_cols$last_stage]] %in% "Services")) %>%
    calc_yqfgW() %>%
    calc_A(direction = "downstream") %>%
    calc_G(direction = "downstream")
  expect_equal(io_mats_G, io_mats_L)
})


test_that("calc_L() works as expected for downstream swim", {
  io_mats <- UKEnergy2000mats %>%
    tidyr::spread(key = matrix.name, value = matrix) %>%
    # Last.stage == "Services" has unit inhomogeniety and bad results.
    dplyr::filter(!(.data[[Recca::psut_cols$last_stage]] %in% "Services")) %>%
    calc_yqfgW() %>%
    calc_A(direction = "downstream") %>%
    calc_L(direction = "downstream")

  expect_true("G_pxp" %in% colnames(io_mats))
  expect_true("G_ixp" %in% colnames(io_mats))

  expect_true(!("L_pxp" %in% colnames(io_mats)))
  expect_true(!("L_ixp" %in% colnames(io_mats)))

  # Check some values in the resulting matrices.
  expect_equal(io_mats$G_pxp[[1]]["Crude [from Dist.]", "Crude [from Fields]"], 0.9978994404)
  expect_equal(io_mats$G_pxp[[2]]["Elect [from Grid]", "Crude [from Fields]"], 2.175612e-04)
  expect_equal(io_mats$G_ixp[[1]]["Petrol dist.", "Petrol"], 1.0186915888)
  expect_equal(io_mats$G_ixp[[2]]["Light fixtures", "Elect [from Grid]"], 0.963300755)
})


test_that("calc_io_mats() works for downstream swim", {
  G_mats <- UKEnergy2000mats %>%
    tidyr::spread(key = matrix.name, value = matrix) %>%
    # Last.stage == "Services" has unit inhomogeniety and bad results.
    dplyr::filter(Last.stage != "Services") %>%
    calc_io_mats(direction = "Ghosh") %>%
    # Look at the G matrices, because they depend on everything else.
    dplyr::select(Country, Year, Energy.type, Last.stage, G_ixp, G_pxp)
  # Make sure these results match expected results by testing a few values.
  expect_equal(G_mats$G_pxp[[1]]["Crude [from Dist.]", "Crude [from Fields]"], 0.9978994404)
  expect_equal(G_mats$G_pxp[[2]]["Elect [from Grid]", "Crude [from Fields]"], 2.175612e-04)
  expect_equal(G_mats$G_ixp[[1]]["Petrol dist.", "Petrol"], 1.0186915888)
  expect_equal(G_mats$G_ixp[[2]]["Light fixtures", "Elect [from Grid]"], 0.963300755)
})
