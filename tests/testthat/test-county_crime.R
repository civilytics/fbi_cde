test_that("enumerate_periods lists inclusive MM-YYYY months", {
  expect_equal(enumerate_periods("01-2019", "03-2019"),
               c("01-2019", "02-2019", "03-2019"))
  expect_equal(enumerate_periods("11-2020", "02-2021"),
               c("11-2020", "12-2020", "01-2021", "02-2021"))
  expect_equal(enumerate_periods("05-2022", "05-2022"), "05-2022")
})
