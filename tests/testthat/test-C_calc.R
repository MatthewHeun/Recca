test_that("calc_C_Y_C_EIOU() works as expected with simple countries", {

  # This example is in the file called "2countries2sectors simple example v2.xlsx"
  # Country A information
  Yf_A <- matrix(c(215, 0,
                   0, 45),
                 byrow = TRUE,
                 nrow = 2, ncol = 2,
                 dimnames = list(c("Ef1", "Ef2"),
                                 c("S1", "S2"))) |>
    matsbyname::setrowtype("Product") |>
    matsbyname::setcoltype("Industry")
  Yfudetails_A <- matrix(c(20, 34.5, 0,
                           0, 0, 0,
                           0, 0, 0,
                           0, 0, 22.5),
                         byrow = TRUE,
                         nrow = 4, ncol = 3,
                         dimnames = list(c("Ef1 -> S1", "Ef1 -> S2", "Ef2 -> S1", "Ef2 -> S2"),
                                         c("Eu1 [from M1]", "Eu2 [from M2]", "Eu3 [from M3]")))
  etai_vec_A <- matrix(c(0.2,
                         0.3,
                         0.5),
                       dimnames = list(c("M1 -> Eu1",
                                         "M2 -> Eu2",
                                         "M3 -> Eu3"), "col")) |>
    matsbyname::setcoltype("Industry -> Product")
  # For simplicity, assume that Y_EIOU_f is the same as Yf.
  U_EIOU_f_A <- Yf_A
  U_EIOU_fu_details_A <- Yfudetails_A

  res_A <- calc_C_Y_C_EIOU(Y_f = Yf_A, Y_fu_details = Yfudetails_A,
                           U_eiou_f = U_EIOU_f_A, U_eiou_fu_details = U_EIOU_fu_details_A,
                           eta_i = etai_vec_A)
  expected_C_Y_A <- matrix(c(0.465116279, 0.534883721, 0,
                             0, 0, 0,
                             0, 0, 0,
                             0, 0, 1),
                           byrow = TRUE,
                           nrow = 4, ncol = 3,
                           dimnames = list(c("Ef1 -> S1",
                                             "Ef1 -> S2",
                                             "Ef2 -> S1",
                                             "Ef2 -> S2"),
                                           c("M1 -> Eu1",
                                             "M2 -> Eu2",
                                             "M3 -> Eu3")))
  expect_true(matsbyname::difference_byname(res_A$C_Y, expected_C_Y_A) |>
                matsbyname::iszero_byname())
  expect_true(matsbyname::difference_byname(res_A$C_EIOU, expected_C_Y_A) |>
                matsbyname::iszero_byname())

  # Country B information
  Yf_B <- matrix(c(300, 0,
                   0, 20),
                 byrow = TRUE,
                 nrow = 2, ncol = 2,
                 dimnames = list(c("Ef1", "Ef2"),
                                 c("S1", "S2"))) |>
    matsbyname::setrowtype("Product") |>
    matsbyname::setcoltype("Industry")
  Yfudetails_B <- matrix(c(50, 20, 0,
                           0, 0, 0,
                           0, 0, 0,
                           0, 0, 8),
                         byrow = TRUE,
                         nrow = 4, ncol = 3,
                         dimnames = list(c("Ef1 -> S1", "Ef1 -> S2", "Ef2 -> S1", "Ef2 -> S2"),
                                         c("Eu1 [from M1]", "Eu2 [from M2]", "Eu3 [from M3]")))
  etai_vec_B <- matrix(c(0.25,
                         0.2,
                         0.4),
                       dimnames = list(c("M1 -> Eu1",
                                         "M2 -> Eu2",
                                         "M3 -> Eu3"), "col")) |>
    matsbyname::setcoltype("Industry -> Product")
  # For simplicity, assume that Y_EIOU_f is the same as Yf.
  U_EIOU_f_B <- Yf_B
  U_EIOU_fu_details_B <- Yfudetails_B

  res_B <- calc_C_Y_C_EIOU(Y_f = Yf_B, Y_fu_details = Yfudetails_B,
                           U_eiou_f = U_EIOU_f_B, U_eiou_fu_details = U_EIOU_fu_details_B,
                           eta_i = etai_vec_B)
  expected_C_Y_B <- matrix(c(2/3, 1/3, 0,
                             0, 0, 0,
                             0, 0, 0,
                             0, 0, 1),
                           byrow = TRUE,
                           nrow = 4, ncol = 3,
                           dimnames = list(c("Ef1 -> S1",
                                             "Ef1 -> S2",
                                             "Ef2 -> S1",
                                             "Ef2 -> S2"),
                                           c("M1 -> Eu1",
                                             "M2 -> Eu2",
                                             "M3 -> Eu3")))
  expect_true(matsbyname::difference_byname(res_B$C_Y, expected_C_Y_B) |>
                matsbyname::iszero_byname())
  expect_true(matsbyname::difference_byname(res_B$C_EIOU, expected_C_Y_B) |>
                matsbyname::iszero_byname())

  # Work on the aggregate of countries A and B
  Yf_AB <- matsbyname::sum_byname(Yf_A, Yf_B)
  U_EIOU_f_AB <- matsbyname::sum_byname(U_EIOU_f_A, U_EIOU_f_B)
  Yfudetails_AB <- matsbyname::sum_byname(Yfudetails_A, Yfudetails_B)
  U_EIOU_fu_details_AB <- matsbyname::sum_byname(U_EIOU_fu_details_A, U_EIOU_fu_details_B)
  etai_vec_AB <- matrix(c(70/300,
                          54.5/215,
                          30.5/65),
                        nrow = 3, ncol= 1,
                        dimnames = list(c("M1 -> Eu1",
                                          "M2 -> Eu2",
                                          "M3 -> Eu3"), "col")) |>
    matsbyname::setcoltype("Industry -> Product")

  res_AB <- calc_C_Y_C_EIOU(Y_f = Yf_AB, Y_fu_details = Yfudetails_AB,
                            U_eiou_f = U_EIOU_f_AB, U_eiou_fu_details = U_EIOU_fu_details_AB,
                            eta_i = etai_vec_AB)

  expected_C_Y_AB <- matrix(c(0.582524272, 0.417475728, 0,
                              0, 0, 0,
                              0, 0, 0,
                              0, 0, 1),
                            byrow = TRUE,
                            nrow = 4, ncol = 3,
                            dimnames = list(c("Ef1 -> S1",
                                              "Ef1 -> S2",
                                              "Ef2 -> S1",
                                              "Ef2 -> S2"),
                                            c("M1 -> Eu1",
                                              "M2 -> Eu2",
                                              "M3 -> Eu3")))

  expect_true(matsbyname::difference_byname(res_AB$C_Y, expected_C_Y_AB) |>
                matsbyname::iszero_byname())
  expect_true(matsbyname::difference_byname(res_AB$C_EIOU, expected_C_Y_AB) |>
                matsbyname::iszero_byname())


  # Now try in a data frame
  df <- tibble::tibble(
    Country = c("A", "B", "AB"),
    Y___Final = list(Yf_A, Yf_B, Yf_AB),
    Y_fu_details = list(Yfudetails_A, Yfudetails_B, Yfudetails_AB),
    U_EIOU___Final = list(U_EIOU_f_A, U_EIOU_f_B, U_EIOU_f_AB),
    U_EIOU_fu_details = list(U_EIOU_fu_details_A, U_EIOU_fu_details_B, U_EIOU_fu_details_AB),
    eta_i = list(etai_vec_A, etai_vec_B, etai_vec_AB),
    expectedC_Y = list(expected_C_Y_A, expected_C_Y_B, expected_C_Y_AB),
    expectedC_EIOU = list(expected_C_Y_A, expected_C_Y_B, expected_C_Y_AB)
  )
  res_df <- df |>
    calc_C_Y_C_EIOU() |>
    dplyr::mutate(
      EqualC_Y = matsbyname::difference_byname(C_Y, expectedC_Y) |>
        matsbyname::iszero_byname(),
      EqualC_EIOU = matsbyname::difference_byname(C_EIOU, expectedC_EIOU) |>
        matsbyname::iszero_byname()
    )
  expect_true(all(unlist(res_df$EqualC_Y)))
  expect_true(all(unlist(res_df$EqualC_EIOU)))
})
