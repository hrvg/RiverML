#' Derive raster statistics: "median","mean", "min", "max", "sd", "skew"
#' @param i numeric, indice
#' @param .ls a RasterStack of terrain analysis rasters, passed from `get_stats_df()`
#' @param .lines a SpatialLinesDataFrame, passed from `get_stats_df()`
#' @param .stat character, list of statistics to derive, passed from `get_stats_df()`
#' @return a numeric vector of statistics
#' @export
#' @keywords tam-dm
raster_stats <- function(i, .ls, .lines, .stat){
	.s <- .ls[[i]]
	.n <- raster::nlayers(.s)
	var_stats <- lapply(.stat, function(x){raster::cellStats(.s, stat = x)})
	return(as.vector(unlist(var_stats)))
}

#' Derive near-channel statistics: "median","mean", "min", "max", "sd", "skew"
#' @param i numeric, indice
#' @param .ls a RasterStack of terrain analysis rasters, passed from `get_stats_df()`
#' @param .lines a SpatialLinesDataFrame, passed from `get_stats_df()`
#' @param .stat character, list of statistics to derive, passed from `get_stats_df()`
#' @param bf numeric, size of the riparian buffer, default to 100 m
#' @import sp
#' @return a numeric vector of statistics
#' @export
#' @keywords tam-dm
near_channel_stats <- function(i, .ls, .lines, .stat, bf = 100){
	.s <- .ls[[i]]
	spext <- as(raster::extent(.s), "SpatialPolygons")
	proj4string(spext) <- sp::CRS(proj4string(.lines))
	if(!is.null(rgeos::gIntersection(.lines, spext))){
		.line <- raster::crop(.lines[sp::SpatialLinesLengths(.lines) > 1E-6, ], .s)
		var_stats <- lapply(.stat, function(x){
			apply(raster::extract(.s, .line, fun = x, na.rm = TRUE, buffer = bf), MARGIN = 2, FUN = mean)
			})
		return(as.vector(unlist(var_stats)))			
	} else {
		return(rep(NA, length(.stat) * raster::nlayers(.s)))
	}
}

#' Wrapper function to calculate terrain analysis statistics for a RasterStack
#' @param fun either `near_channel_stats` or `raster_stats`
#' @param fun_name a character string specifying the name of the function in the column of the resulting `data.frame`
#' @param ls a RasterStack of terrain analysis rasters
#' @param dem_file character, DEM filename (used for fixing names)
#' @param lines a SpatialLinesDataFrame
#' @param stat functions, list of statistics to derive
#' @param stat_name character, labels corresponding to `stat`
#' @importFrom stats median sd
#' @return a `data.frame` with the values for the requested terrain analysis distribution metrics
#' @export
#' @keywords tam-dm
get_stats_df <- function(fun, fun_name, ls, dem_file, lines = NULL, stat = c(median, mean, min, max, sd, e1071::skewness), stat_name = c("median", "mean", "min", "max", "sd", "skew")){
	if(!(any(identical(fun, raster_stats), identical(fun, near_channel_stats)))) stop("Wrong function!")
	if(!is.null(lines)){
		lines <- sp::spTransform(lines, raster::crs(ls[[1]]))
	}
	lv <- lapply(seq(1, length(ls)), fun, .ls = ls, .lines = lines, .stat = stat)
	data_mat <- do.call(rbind, lv)
	data_df <- data.frame(data_mat)
	col_names <- outer(names(ls[[1]]), stat_name, FUN = "paste", sep = "_")
	dim(col_names) <- NULL
	col_names <- unlist(lapply(col_names, function(name) stringr::str_replace(name, tools::file_path_sans_ext(dem_file), "elevation")))
	colnames(data_df) <- paste(col_names,fun_name, sep=".")
	return(data_df[, order(col_names)])
}

#' Get the terrain analysis metrics
#' @param polys a list of SpatialPolygon
#' @param DEM a Raster
#' @param curvature logical, default to `FALSE`, if `TRUE` allows the calculation of profile and planform curvature
#' @export
#' @keywords tam-dm
get_terrain_metrics <- function(polys, DEM, curvature = FALSE){
	llr <- lapply(seq_along(polys), function(i){
			p <- polys[[i]]
			mDEM <- raster::crop(DEM,p)
			if (curvature){
				terrain_metrics <- terrain_(mDEM, opt = c('slope','aspect','curvplan','curvprof', 'TPI', 'TRI', 'roughness', 'flowdir'), unit = 'tangent', neighbors = 8)
			} else {
				terrain_metrics <- raster::terrain(mDEM, opt = c('slope','aspect', 'TPI', 'TRI', 'roughness', 'flowdir'), unit = 'tangent', neighbors = 8)
			}
			br <- raster::brick(mDEM,terrain_metrics)
			nl <- raster::nlayers(br)
			lr <- lapply(seq(nl), function(j) br[[j]])
			return(lr)
		})
	ls <- lapply(llr, raster::stack)
	return(ls)
}