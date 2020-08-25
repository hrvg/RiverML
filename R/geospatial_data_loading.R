#' Load a SpatialLinesDataFrame containing values for valley confinement, GIS slope and RUSLE.
#' The SpatialDataFrame source data is the NHDv2Plus dataset which is segmented into 200-m stream intervals.
#' @param region defines the area of study and the files loaded
#' @param extdir default to `NULL`, if not `NULL` this indicates a `file.path` or `character` pointing to a directory external to the package. This function expects to find shapefiles named `region_200m_VAA_LDD.shp` in `extdir`.
#' @return a SpatialLinesDataFrame
#' @importFrom methods as
#' @import raster
#' @export
#' @keywords geospatial-data
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
		names(target_streamlines) <- attr_names
	}
	target_streamlines$ID <- seq(nrow(target_streamlines))
	return(target_streamlines)
}

#' Extracts mid-points from streamlines
#' @param target_streamlines a SpatialLinesDataFrame
#' @param .crs a reference coordinate system to project the points to 
#' @param as.df logical, default to `TRUE`, defines if a `SpatialPoints` or a `data.frame` object is returned
#' @return a `data.frame` with columns `Name`, `long` and `lat`
#' @export
#' @keywords geospatial-data
get_target_points <- function(target_streamlines, .crs = sp::CRS("+proj=longlat +datum=WGS84"), as.df = TRUE){
	if(class(target_streamlines) != "SpatialLinesDataFrame") stop("`target_streamlines` is not a SpatialLinesDataFrame")
	target_points <- maptools::SpatialLinesMidPoints(target_streamlines)
	target_points_proj <- sp::spTransform(target_points, .crs)
	if (as.df){
		target_points_proj_df <- as.data.frame(target_points_proj)
		target_points_proj_df <- target_points_proj_df[, names(target_points_proj_df) %in% c("ID", "coords.x1", "coords.x2")]
		colnames(target_points_proj_df) <- c("Name", "long", "lat")
		return(target_points_proj_df)
	} else {
		return(target_points_proj)
	}
}

#' Get a SpatialPolygons around a point
#' @param i the indice of the requested points
#' @param pts SpatialPoints
#' @param dl a number of pixel added in all direction to the coordinates of the i-th point in order to create a square tile
#' @param .crs a `sp::CRS`, usually the one from the DEM to be tiled
#' @export
#' @keywords geospatial-data
get_pol <- function(i, pts, dl = 1000, .crs = sp::CRS("+proj=longlat +datum=WGS84")){
	x_min <- pts@coords[i,1] - dl
	x_max <- pts@coords[i,1] + dl
	y_min <- pts@coords[i,2] - dl
	y_max <- pts@coords[i,2] + dl
	coords = matrix(c(x_min, y_min,
	               x_min, y_max,
	               x_max, y_max,
	               x_max, y_min,
	               x_min, y_min), 
	             ncol = 2, byrow = TRUE)
	p <-  sp::Polygon(coords)
	sp1 <-  sp::SpatialPolygons(list(sp::Polygons(list(p), ID = "a")), proj4string=.crs)
	return(sp1)
}

#' Loading input data
#' @param fpath a `file.path` to an input `.csv` file
#' @return a `data.frame` with columns `Name`, `long` and `lat`
#' @importFrom utils read.csv
#' @importFrom magrittr %>%
#' @importFrom stats na.omit
#' @export
#' @keywords geospatial-data
get_input_data <- function(fpath){
	if(!file.exists(fpath)) stop("Input file does not exist.")
	input_data <- read.csv(fpath, header=TRUE, sep = ",")
	if(!all(c("Name", "long", "lat") %in% colnames(input_data))) stop("Input data has the wrong format.")
	input_data <- na.omit(input_data) %>% dplyr::arrange(Name)
	return(input_data)
}

#' Get the polygons for a set of locations defined by `input_data` and `n`
#' @param input_data a `data.frame` with columns `lon` and `lat`
#' @param n numeric the indices of the rows of input_data to use
#' @param DEM a `raster` object
#' @param .dl a number of pixel added in all direction to the coordinates of the i-th point in order to create a square tile
#' @param .crs a `sp::CRS`, usually the one from the DEM to be tiled
#' @return a list of SpatialPolygon
#' @export
#' @keywords geospatial-data
get_input_polygons <- function(input_data, n, DEM, .dl = 25, .crs = sp::CRS('+proj=longlat +datum=WGS84')){
	if(!raster::isLonLat(DEM)) warning("Raster is not in LongLat format. Proceeding but raising warning...")
	lonlat <- cbind(input_data$lon,input_data$lat)
	pts <- sp::SpatialPoints(lonlat, proj4string = .crs)
	pts <- sp::spTransform(pts, raster::crs(DEM))
	polys <- lapply(n, function(i) get_pol(i, pts, dl = .dl * raster::res(DEM)[1], .crs = raster::crs(DEM)))
	return(polys)
}

#' Get the SpatialPoint from input data
#' @param input_data a `data.frame` with columns `lon` and `lat`
#' @param .crs a `sp::CRS`, usually `+proj=longlat +datum=WGS84`
#' @return a `SpatialPoints` object
#' @export
#' @keywords geospatial-data
get_points_from_input_data <- function(input_data, .crs = sp::CRS("+proj=longlat +datum=WGS84")){
	if(!all(c("long", "lat") %in% colnames(input_data))) stop("Input data has the wrong format.")
	points <- sp::SpatialPoints(cbind(input_data$long, input_data$lat), proj4string = .crs)
	return(points)
}

#' Snaps points to points
#' @param points_a a `SpatialPoints` object
#' @param points_b a `SpatialPoints` object
#' @return `numeric`, the indices in `points_b` that are the closest to each `points_a`
#' @export
#' @keywords geospatial-data
snap_points_to_points <- function(points_a, points_b){
	spdst <- terra::distance(terra::vect(points_a), terra::vect(points_b))
	snap <- apply(spdst, 1, which.min)
	return(snap)
}
