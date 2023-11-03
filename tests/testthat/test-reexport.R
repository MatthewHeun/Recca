test_that("re-exporting works correctly", {
  expect_equal(Recca::nonenergy_products,
               list(additives_blending_components = "Additives/blending components",
                    bitumen = "Bitumen",
                    lubricants = "Lubricants",
                    naphtha = "Naphtha",
                    paraffin_waxes = "Paraffin waxes",
                    refinery_feedstocks = "Refinery feedstocks",
                    white_spirit_and_sbp = "White spirit & SBP"))
})
