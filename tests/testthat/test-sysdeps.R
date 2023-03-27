test_that("sysdeps works", {
  skip_on_os('windows')
  sysdeps <- package_sysdeps('curl')
  if(running_on('linux') || getRversion() >= 4){
    expect_true(any(grepl('libcurl', package_links_to('curl'))))
    expect_true(any(grepl('libcurl', sysdeps$shlib)))
  }
  if(running_on('linux')){
    skip_on_cran() #BDR may build libcurl from source
    expect_true(any(grepl('curl', sysdeps$package)))
  }
})
