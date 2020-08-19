#' Load a SpatialLinesDataFrame containing values for valley confinement, GIS slope and RUSLE.
#' The SpatialDataFrame source data is the NHDv2Plus dataset which is segmented into 200-m stream intervals.
#' @param region defines the area of study and the files loaded
#' @param extdir default to `NULL`, if not `NULL` this indicates a `file.path` or `character` pointing to a directory external to the package. This function expects to find shapefiles named `region_200m_VAA_LDD.shp` in `extdir`.
#' @return a SpatialLinesDataFrame
#' @example get_target_streamlines("SFE")
#' @importFrom methods as
#' @import raster
#' @export
get_target_streamlines <- function(region, extdir = NULL){
	if(is.null(extdir)){
		target_streamlines_file <- system.file("extdata/target_streamlines/", paste0(region, "_200m_VAA_LDD.shp"), package = "RiverML")
		if(!file.exists(target_streamlines_file)) stop("Region not provided with package")
	} else {
		target_streamlines_file <- file.path(extdir, paste0(region, "_200m_VAA_LDD.shp"))
		if(!file.exists(target_streamlines_file)) stop("`target_streamlines_file` not found in `extdir`")
	}
	target_streamlines <- sf::read_sf(file.path(target_streamlines_file))
	target_streamlines <- as(target_streamlines, "Spatial")
	attr_names <- names(target_streamlines)
	if (!"CONFINEMEN" %in% attr_names){
		warning("Changing attribute name: Confinemen -> CONFINEMEN")
		attr_names[which(attr_names == "Confinemen")] <- "CONFINEMEN"
		warning("Changing attribute name: Slope -> SLOPE")
		attr_names[which(attr_names == "Slope")] <- "SLOPE"
		warning("Changing attribute name: Area -> AREA")
		attr_names[which(attr_names == "Area")] <- "AREA"
	}
	target_streamlines$ID <- seq(nrow(target_streamlines))
	return(target_streamlines)
}

#' Extracts mid-points from streamlines
#' @param target_streamlines a SpatialLinesDataFrame
#' @return a data.frame with columns `Name`, `long` and `lat`
#' @export
get_target_points_df <- function(target_streamlines){
	if(class(target_streamlines) != "SpatialLinesDataFrame") stop("`target_streamlines` is not a SpatialLinesDataFrame")
	target_points <- maptools::SpatialLinesMidPoints(target_streamlines)
	crdref <- sp::CRS("+proj=longlat +datum=WGS84")
	target_points_proj <- sp::spTransform(target_points, crdref)
	target_points_proj_df <- as.data.frame(target_points_proj)
	target_points_proj_df <- target_points_proj_df[, names(target_points_proj_df) %in% c("ID", "coords.x1", "coords.x2")]
	colnames(target_points_proj_df) <- c("Name", "long", "lat")
	return(target_points_proj_df)
}