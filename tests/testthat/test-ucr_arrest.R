test_that("Arrests total offense count works", {
  skip_on_cran()
  expect_equal(get_arrest_count('CA0191900')[, arrest_matching_columns],
               fix_arrest_test('CA0191900'))
  expect_equal(get_arrest_count('CA0370900')[, arrest_matching_columns],
               fix_arrest_test('CA0370900'))
  expect_equal(get_arrest_count('IL0493600')[, arrest_matching_columns],
               fix_arrest_test('IL0493600'))
  expect_equal(get_arrest_count('KY0190700')[, arrest_matching_columns],
               fix_arrest_test('KY0190700'))
  expect_equal(get_arrest_count('MN0320200')[, arrest_matching_columns],
               fix_arrest_test('MN0320200'))
  expect_equal(get_arrest_count('MO0240500')[, arrest_matching_columns],
               fix_arrest_test('MO0240500'))
  expect_equal(get_arrest_count('NJ0081100')[, arrest_matching_columns],
               fix_arrest_test('NJ0081100'))
  expect_equal(get_arrest_count('NM0250100')[, arrest_matching_columns],
               fix_arrest_test('NM0250100'))
  expect_equal(get_arrest_count('OK0411500')[, arrest_matching_columns],
               fix_arrest_test('OK0411500'))
  expect_equal(get_arrest_count('OK0700200')[, arrest_matching_columns],
               fix_arrest_test('OK0700200'))
  expect_equal(get_arrest_count('OR0280000')[, arrest_matching_columns],
               fix_arrest_test('OR0280000'))
  expect_equal(get_arrest_count('OR0360100')[, arrest_matching_columns],
               fix_arrest_test('OR0360100'))
  expect_equal(get_arrest_count('PA0490400')[, arrest_matching_columns],
               fix_arrest_test('PA0490400'))
  expect_equal(get_arrest_count('SC0321600')[, arrest_matching_columns],
               fix_arrest_test('SC0321600'))
})
