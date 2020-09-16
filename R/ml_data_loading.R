#' Load training features
#' @param region `character`, identifier of the region of study
#' @param extdir `character` or `file.path`, a directory in which to fetch the `groups` and `data_df` `csv` files
#' @return a named list with two elements `groups` the classes of each observation, `data_df` the training data `data.frame`
#' @export
#' @keywords ml-data-loading
get_training_data <- function(region, extdir = NULL){
	if(is.null(extdir)){
		groups_file <- system.file("extdata/input_data/", paste0(region, "_groups.csv"), package = "RiverML")
		if(!file.exists(groups_file)) stop("Region not provided with package")
		data_df_file <- system.file("extdata/input_data/", paste0(region, "_data_df.csv"), package = "RiverML")
		if(!file.exists(data_df_file)) stop("Region not provided with package")
	} else {
		groups_file <- file.path(extdir, paste0(region, "_groups.csv"))
		if(!file.exists(groups_file)) stop("`groups_file` not found in `extdir`")
		data_df_file <- file.path(extdir, paste0(region, "_data_df.csv"))
		if(!file.exists(data_df_file)) stop("`data_df_file` not found in `extdir`")
	}
	groups <- read.csv(file = groups_file)
	groups <- as.factor(groups$x)
	data_df <- read.csv(file = data_df_file)
	return(
		list(groups = groups, data_df = data_df)
		)
}

#' Load target features
#' @param region `character`, identifier of the region of study
#' @param extdir `character` or `file.path`, a directory in which to fetch the `groups` and `data_df` `csv` files
#' @return `data_df` the target data `data.frame`
#' @export
#' @keywords ml-data-loading
get_target_data <- function(region, extdir = NULL){
	if(is.null(extdir)){
		data_df_file <- system.file("extdata/input_data/", paste0(region, "_all_data_df.csv"), package = "RiverML")
		if(!file.exists(data_df_file)) stop("Region not provided with package")
	} else {
		data_df_file <- file.path(extdir, paste0(region, "_all_data_df.csv"))
		if(!file.exists(data_df_file)) stop("`all_data_df_file` not found in `extdir`")
	}
	data_df <- read.csv(file = data_df_file)
	return(data_df)
}


#' Format the class labels
#' @param groups the class groups
#' @param region `character`, identifier of the region of study
#' @return a `factor` with formatted groups in the format `regionXX`
#' @export
#' @keywords ml-data-loading
fmt_labels <- function(groups, region){
	groups <- as.numeric(groups)
	groups <- paste0(region, ifelse(groups < 10, "0", ""), groups)
	if(region == "ALLSAC") groups <- gsub("ALLSAC", "SAC", groups)
	groups <- as.factor(groups)
	return(groups)	
}

#' Ensures that non-finite values are flagged as `NA`
#' @param data_df a `data.frame`
#' @return a `data.frame`
#' @export
#' @keywords ml-data-loading
sanitize_data <- function(data_df){
	sanitized_data <- data_df
	sanitized_data <- do.call(data.frame, lapply(sanitized_data, function(x) replace(x, is.infinite(x), NA))) 
	sanitized_data <- as.data.frame(sapply(sanitized_data, as.numeric), colnames = names(sanitized_data))
	return(sanitized_data)
}

#' Retrieve the coordinates of the observations
#' @param region `character`, identifier of the region of study
#' @return the coordinates of the labelled points for a given `region`
#' @export
#' @keywords ml-data-loading
get_coords <- function(region){
	input_dir <- system.file("extdata/input_data", package = "RiverML")
	fname <- paste0(region,"_input.csv")
	input_data <- get_input_data(file.path(input_dir, fname))
	points <- get_points_from_input_data(input_data)
	.coords <- coordinates(points)
	coords <- data.frame(x = .coords[, 1], y = .coords[, 2])
	return(coords)
}

#' Transform training data from `list` to `data.frame`
#' @param smote_data  a named list with two elements `data` and `labels`
#' @return a `data.frame` with an additional column `channel_type`
#' @keywords ml-data-loading
#' @export
make_training_data <- function(smote_data){
	training_data <- cbind(smote_data$data, channel_type = smote_data$labels)
	return(training_data)
}