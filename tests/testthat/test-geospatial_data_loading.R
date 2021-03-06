context("get_input_data")
test_that("get_input_data works", {
	fpath <- system.file("extdata/input_data", "SFE_all_input.csv", package = "RiverML")
	expect_is(get_input_data(fpath), "data.frame")
	expect_error(get_input_data(NULL))
	expect_true(all(c("Name", "long", "lat") %in% colnames(get_input_data(fpath))))
})	

context("get_target_streamlines")
test_that("get_target_streamlines works", {
	streamlines <- get_target_streamlines("SFE")
	# expect_true(all.equal(streamlines, target_streamlines_SFE))
	expect_is(streamlines, "SpatialLinesDataFrame")
	expect_true('CONFINEMEN' %in% names(streamlines))
	expect_true('SLOPE' %in% names(streamlines))
	expect_true('AREA' %in% names(streamlines))
	expect_error(get_target_streamlines("UNDEFINED"))
	expect_error(get_target_streamlines("SFE", extdir = getwd()))
})

context("get_target_points")
test_that("get_target_points works", {
	data(target_streamlines_SFE)
	points <- get_target_points(target_streamlines_SFE)
	expect_is(points, "data.frame")
	expect_is(get_target_points(target_streamlines_SFE, as.df = FALSE), "SpatialPointsDataFrame")
	expect_error(get_target_points(as.data.frame(target_streamlines_SFE)))
	expect_true(nrow(points) == length(target_streamlines_SFE))
	expect_equal(colnames(points), c("Name", "long", "lat"))
})

