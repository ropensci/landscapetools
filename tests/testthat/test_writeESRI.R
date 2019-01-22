context("util_writeESRI")

test_that("fails to create file in non-existent directory", {
    expect_warning(expect_error(util_writeESRI(gradient_landscape, file.path(tempdir(), "/x/y")), "cannot open the connection"), "No such file or directory")
})

test_that("write_excel_csv/csv2 includes a byte order mark", {

    tmp <- tempfile()
    on.exit(unlink(tmp))

    util_writeESRI(gradient_landscape, tmp)

    asciitext <- readLines(tmp)

    # header is there
    expect_equal(asciitext[1], "ncols         150")
    expect_equal(asciitext[2], "nrows         150")
    expect_equal(asciitext[3], "xllcorner     0")
    expect_equal(asciitext[4], "yllcorner     0")
    expect_equal(asciitext[5], "cellsize      1")
    expect_equal(asciitext[6], "NODATA_value  -9999")

})
