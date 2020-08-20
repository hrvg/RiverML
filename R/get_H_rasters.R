#' Retrieve rasters of statistical roughness of topography
#' @param extdir default to `NULL`, if not `NULL` this indicates a `file.path` or `character` pointing to a directory external to the package. This function expects to find shapefiles named `scale.grd` in `extdir`.
#' @return a named list with two elements `H_rasters` and `upper_scales`
#' @export
get_H_rasters <- function(extdir = NULL){
	if(is.null(extdir)){
		extdir <- system.file("extdata/statistical_roughness/", package = "RiverML")
	} 
	lf <- list.files(path = file.path(extdir), pattern = ".grd")
	if (length(lf) == 0) stop(paste("Stastitical roughness files not found in:", extdir))
	upper_scales <- unlist(lapply(lf, function(f) tools::file_path_sans_ext(f)))
	upper_scales <- unname(sapply(upper_scales, as.numeric))
	lf <- lf[order(upper_scales)]
	upper_scales <- upper_scales[order(upper_scales)]
	H_rasters <- lapply(lf, function(f) raster::raster(file.path(extdir,f)))
	return(list(H_rasters = H_rasters, upper_scales = upper_scales))
}

#' Extract the values of all H_rasters along streamlines
#' @param H_rasters a list of Raster
#' @param streamlines a SpatialLinesDataFrame
#' @param upper_scales the upper scales corresponding to `H_rasters` (for naming)
#' @return a `data.frame`
#' @export
join_streamlines_with_H_rasters <- function(H_rasters, streamlines, upper_scales){
	v <- lapply(H_rasters, function(x){raster::extract(x, streamlines, fun = mean)})
	v <- matrix(unlist(v), nrow = length(streamlines), ncol = length(H_rasters))
	data_df_H <- data.frame(v)
	colnames(data_df_H) <- paste("H", upper_scales, sep=".")
	return(data_df_H)
}