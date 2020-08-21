#' Get the StreamCat data
#' @param streamcat_dir `character` or `file.path` to a directory containing the StreamCat data
#' @param hydro_regions `character`, target US HydroRegion
#' @return a `data.frame` with StreamCat data
#' @export
get_streamcat_df <- function(streamcat_dir, hydro_regions = c("Region18", "Region16", "Region15")){
	lf <- list.files(path = file.path(streamcat_dir), full.names = TRUE, recursive = TRUE)
	if(length(lf) == 0) stop(paste("No file found at:", streamcat_dir))
	drops <- c("COMID","CatAreaSqKm","WsAreaSqKm","CatPctFull","WsPctFull",
		"CatAreaSqKmRp100", "WsAreaSqKmRp100", "CatPctFullRp100", "WsPctFullRp100")
	l_streamcat_df <- lapply(hydro_regions, function(reg){
	 	lf_reg <- lf[grep(reg, lf)]
		ldf_reg <- lapply(lf_reg, function(f) utils::read.csv(f, header = TRUE, sep = ","))
		.ldf_reg <- ldf_reg[[1]]
		ldf_reg <- lapply(ldf_reg[2:length(ldf_reg)], function(df) df[ , !(names(df) %in% drops)])
		streamcat_df_reg <- do.call(cbind, ldf_reg)
		streamcat_df_reg <- cbind(.ldf_reg, streamcat_df_reg)
		return(streamcat_df_reg)
	})
	streamcat_df <- do.call(rbind, l_streamcat_df)
	return(streamcat_df)
}

#' Remove some variables from `streamcat_df`
#' @param streamcat_df a `data.frame`
#' @param .type `character`, one of `WsCat`, `Ws` or `Cat`
#' @return a `data.frame` with dropped variables
drops_streamcat_df <- function(streamcat_df, .type ="WsCat"){
	if(.type == "Ws"){
		drops <- c("ICI", "CHYD", "CCHEM","CSED","CCONN","CTEMP","CHABT", "WsPctFull", "WsPctFullRp100")
	} else if(.type == "Cat"){
		drops <- c("IWI", "WHYD", "WCHEM","WSED","WCONN","WTEMP","WHABT", "CatPctFull", "CatPctFullRp100")
	} else if(.type == "WsCat"){
		drops <- c( "WsPctFull", "WsPctFullRp100", "CatPctFull", "CatPctFullRp100")
	}
	streamcat_df <- streamcat_df[ , !(names(streamcat_df) %in% drops)]	
	return(streamcat_df)
}

#' Matches `streamcat_df` and `target_streamlines` using `COMID`
#' @param streamcat_df a `data.frame` with `COMID` column
#' @param target_streamlines a `SpatialLinesDataFrame` with `COMID` attribute
get_target_streamcat_df <- function(streamcat_df, target_streamlines){
	if(!"COMID" %in% colnames(streamcat_df)) stop("COMID not found in streamcat_df")
	if(!"COMID" %in% names(target_streamlines)) stop("COMID not found in target_streamlines")
	indices <- match(target_streamlines$COMID, streamcat_df$COMID)
	target_streamcat_df <- streamcat_df[indices, ]
	target_streamcat_df <- drops_streamcat_df(target_streamcat_df, .type = "WsCat")
	return(target_streamcat_df)
}