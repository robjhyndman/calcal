test_that("locale", {
  x <- location(latitude = 1, longitude = 2, elevation = 3, zone = 4)
  expect_equal(vctrs::field(x, "latitude"), 1)
  expect_equal(vctrs::field(x, "longitude"), 2)
  expect_equal(vctrs::field(x, "elevation"), 3)
  expect_equal(vctrs::field(x, "zone"), 4)
  expect_error(location(latitude = -91, longitude = 2, elevation = 3, zone = 4))
  expect_error(location(latitude = 91, longitude = 2, elevation = 3, zone = 4))
  expect_error(location(latitude = 1, longitude = 181, elevation = 3, zone = 4))
  expect_error(location(
    latitude = 1,
    longitude = -181,
    elevation = 3,
    zone = 4
  ))
  expect_error(location(
    latitude = 1,
    longitude = 2,
    elevation = -421,
    zone = 4
  ))
  expect_error(location(
    latitude = 1,
    longitude = 2,
    elevation = 9000,
    zone = 4
  ))
  expect_error(location(latitude = 1, longitude = 2, elevation = 3, zone = -13))
  expect_error(location(latitude = 1, longitude = 2, elevation = 3, zone = 15))
})
