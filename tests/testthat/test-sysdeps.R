test_that("sysdeps works", {
  skip_on_os('windows')
  expect_true(any(grepl('libcurl', package_links_to('curl'))))
  sysdeps <- package_sysdeps('curl')
  expect_true(any(grepl('libcurl', sysdeps$shlib)))
  if(running_on('linux')){
    expect_true(any(grepl('curl', sysdeps$package)))
  }
})
