context("get_H_rasters")
test_that("get_H_rasters works", {
  lH <- get_H_rasters()
  expect_is(lH, "list")
  expect_is(lH$H_rasters, "list")
  expect_is(lH$H_rasters[[1]], "Raster")
  expect_is(lH$upper_scales, "numeric")
  expect_true(length(lH$H_rasters) == length(lH$upper_scales))
  expect_error(get_H_rasters(extdir = "."))
})

context("join_streamlines_with_H_rasters")
test_that("join_streamlines_with_H_rasters works", {
  lH <- get_H_rasters()
  streamlines <- target_streamlines_SFE[1:32, ]
  data_df_H <- join_streamlines_with_H_rasters(lH$H_rasters, streamlines, lH$upper_scales)
  expect_is(data_df_H, "data.frame")
  expect_equal(nrow(data_df_H), length(streamlines))
  expect_equal(ncol(data_df_H), length(lH$H_rasters))
})