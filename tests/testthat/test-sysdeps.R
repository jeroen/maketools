test_that("sysdeps works", {
  skip_on_os('windows')
  skip_on_os('mac')
  skip_on_cran()
  sysdeps <- package_sysdeps('curl')
  expect_true(any(grepl('libcurl', package_links_to('curl'))))
  expect_true(any(grepl('libcurl', sysdeps$shlib)))
  expect_true(any(grepl('curl', sysdeps$package)))
})
