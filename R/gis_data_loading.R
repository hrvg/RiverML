#' Load a SpatialLinesDataFrame containing values for valley confinement, GIS slope and RUSLE.
#' The SpatialDataFrame source data is the NHDv2Plus dataset which is segmented into 200-m stream intervals.
#' @param region
#' @param stream_order logical, if `TRUE` changing the file loaded for a given region
#' @return a SpatialLinesDataFrame
#' @export
get_conf_gis <- function(region, stream_order = FALSE){
	conf_gis_dir <- "extdata/gis-files/200m_streams/"
	if (region == "SFE"){
		conf_gis_dir <- "/data/california-rivers/gis-files/SFE_200m_1standhigher_order"
		conf_gis_shp <- "SFE_1standhigher_binned200m_streams_VAA_LDD.shp"
		conf_gis <- sf::read_sf(file.path(conf_gis_dir,conf_gis_shp))
		conf_gis <- as(conf_gis, "Spatial")
	} else if (region == "NC"){
		conf_gis_shp <- "North_200m_with_first_order_VAA_LDD.shp"
		conf_gis <- sf::read_sf(file.path(conf_gis_dir,conf_gis_shp))
		conf_gis <- as(conf_gis, "Spatial")
	} else if (region == "NCC"){
		conf_gis_shp <- "NorthCentral_200m_with_first_order_VAA_LDD.shp"
		conf_gis <- sf::read_sf(file.path(conf_gis_dir,conf_gis_shp))
		conf_gis <- as(conf_gis, "Spatial")
	} else if (region == "SCC"){
		conf_gis_shp <- "SouthCentral_200m_with_first_order_VAA_LDD.shp"
		conf_gis <- sf::read_sf(file.path(conf_gis_dir,conf_gis_shp))
		conf_gis <- as(conf_gis, "Spatial")
	} else if (region == "SC"){
		conf_gis_shp <- "South_200m_VAA_LDD.shp"
		conf_gis <- sf::read_sf(file.path(conf_gis_dir,conf_gis_shp))			
		conf_gis <- as(conf_gis, "Spatial")
	} else if (region == "SECA"){
		conf_gis_shp <- "SouthEastCalifornia_200m_VAA_LDD.shp"
		conf_gis <- sf::read_sf(file.path(conf_gis_dir,conf_gis_shp))			
		conf_gis <- as(conf_gis, "Spatial")	
		attr_names <- names(conf_gis)
		attr_names[which(attr_names == "Confinemen")] <- "CONFINEMEN"
		attr_names[which(attr_names == "Slope")] <- "SLOPE"
		attr_names[which(attr_names == "Area")] <- "AREA"
		names(conf_gis) <- attr_names
	} else if (region == "SJT"){
		conf_gis_shp <- "SanJoaquin_200m_VAA_LDD.shp"
		conf_gis <- sf::read_sf(file.path(conf_gis_dir,conf_gis_shp))			
		conf_gis <- as(conf_gis, "Spatial")
		attr_names <- names(conf_gis)
		attr_names[which(attr_names == "Confinemen")] <- "CONFINEMEN"
		attr_names[which(attr_names == "Slope")] <- "SLOPE"
		attr_names[which(attr_names == "Area")] <- "AREA"
		names(conf_gis) <- attr_names
	} else if (region == "K"){
		conf_gis_shp <- "Klamath_200m_VAA_LDD.shp"
		conf_gis <- sf::read_sf(file.path(conf_gis_dir,conf_gis_shp))			
		conf_gis <- as(conf_gis, "Spatial")
		attr_names <- names(conf_gis)
		attr_names[which(attr_names == "Confinemen")] <- "CONFINEMEN"
		attr_names[which(attr_names == "Slope")] <- "SLOPE"
		attr_names[which(attr_names == "Area")] <- "AREA"
		names(conf_gis) <- attr_names	
	} else {
		if (stream_order){
			conf_gis_shp <- "Sacramento_200mbin_variables_VAA_LDD.shp"
		} else {
			conf_gis_shp <- "Sacramento_200mbin_variables.shp"
		}
		conf_gis <- sf::read_sf(file.path(conf_gis_dir,conf_gis_shp))			
		conf_gis <- as(conf_gis, "Spatial")
	}
	conf_gis$ID <- seq(nrow(conf_gis))
	return(conf_gis)
}