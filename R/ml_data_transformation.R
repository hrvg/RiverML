#' Get the preprocessing
#' @param sanitized_data a `data.frame`
#' @param seed_preproc `numeric` seed for `set.seed()` to reproduce results
#' @param PREPROC vector of preprocessing identifiers (e.g. `"scale"`)
#' @return a `caret` model object that can be used to transform the data
#' @export
#' @keywords ml-data-transformation
get_ppc <- function(sanitized_data, seed_preproc, PREPROC){
	set.seed(seed_preproc)
	ppc <- caret::preProcess(sanitized_data, method = PREPROC)
	return(ppc)
}

#' Process the data according to a `preProcess` object
#' @param raw_training_data a `data.frame` of sanitized data
#' @param ppc a `preProcess` object from `get_ppc()`
#' @param labels `character` or `factor` the labels
#' @return a named list with two elements: `data` a `data.frame` with the transformed data; `labels` the labels
#' @export
#' @keywords ml-data-transformation
preproc_data <- function(raw_training_data, ppc, labels){
	training_data <- caret::predict(ppc, raw_training_data)
	return(list(data = training_data, labels = labels))
}

#' Resolve the class imbalance using the SMOTE algorithm
#' 
#' This function is wrapped around `resolve_class_imbalance()`.
#' 
#' @param data a named list with two elements `data` and `labels`
#' @param seed_preproc `numeric` seed for `set.seed()` to reproduce results
#' @return a list with two elements `data` and `labels`
#' @export
#' @keywords ml-data-transformation
get_smote_data <- function(training_data, seed_preproc){
	set.seed(seed_preproc)
	suppressMessages(smote_data <- resolve_class_imbalance(data = training_data$data, labels = training_data$labels, type = "Smote"))
	return(smote_data)
}

#' Produces noisy coordinates for SMOTE data
#' @param coords the coordinates of observations
#' @param labels `character` or `factor` the labels
#' @param smote_data a named list with two elements `data` and `labels`
#' @param seed_preproc `numeric` seed for `set.seed()` to reproduce results
#' @return a list with two elements `data` and `labels`
#' @export
#' @keywords ml-data-transformation
#' 
#' This function is wrapped around `resolve_class_imbalance()`.
#' 
get_smote_coords <- function(coords, labels, smote_data, seed_preproc){
	set.seed(seed_preproc)
	suppressMessages(smote_coords <- resolve_class_imbalance(data = coords, labels = labels, type = "GaussianNoise"))
	smote_coords <- smote_coords$data
	smote_coords <- smote_coords[match(row.names(smote_coords), row.names(smote_data$data)), ]
	return(smote_coords)
}

#' Resolve class imbalances using `UBL` package
#' 
#' This function is a wrapper around `caret::upSample()`, `UBL::SmoteClassif()`, `UBL::GaussNoiseClassif()` and `UBL::RandUnderClassif()`.
#' 
#' @param data a `data.frame`
#' @param labels `character` or `factor`, the labels
#' @param type `character`, a switch for the type of imbalance, one of `legacy_noise`, `Smote`, `GaussianNoise`, `Under`, `GaussianExtreme`
#' @param .pert `numeric`, perturbation in the Gaussian noise
#' @export
#' @keywords ml-data-transformation
resolve_class_imbalance <- function(data = training.data, labels = training.labels, type = model_df[i, ]$Imba_type, .pert = .1){
	# data = training.data
	#  labels = training.labels
	#  type = model_df[i, ]$Imba_type
	if (type == "legacy_noise"){
		warning("legacy option, I think it might not work with non centered data as it might lead to negative values")
		newData <- caret::upSample(data, labels, list = TRUE)
		upsampled.data <- newData$x
		upsampled.labels <- newData$y
		duplicate_index <- duplicated(upsampled.data)
		error_level <- .05
		jittered.data <- lapply(seq(nrow(upsampled.data)), function(i){
			row <- upsampled.data[i, ]
			if (duplicate_index[i] == TRUE){
				row <- lapply(row, function(element){
					return(element * stats::rnorm(1, mean = 1, sd = error_level))
				})
			}
			return(row)
		})
		upsampled.data <- do.call(rbind, jittered.data)
	} else if (type == "Smote"){
		unloadNamespace("raster")
		library(gstat)
		library(UBL)
		smote.data <- cbind(data.frame(labels = labels), data)
		t <- as.data.frame(max(table(labels)) / table(labels))
		perc_list <- split(t[, 2], t[, 1])
		smoteTrain <- UBL::SmoteClassif(labels ~ ., smote.data, 
			C.perc = perc_list,
			k = min(c(ifelse(min(table(labels)) > 1, min(table(labels))-1, 1), 5)),
			repl = FALSE,
			dist = "Euclidean")
		unloadNamespace("UBL")
		unloadNamespace("automap")
		unloadNamespace("gstat")
		unloadNamespace("spacetime")
		library(raster)
		upsampled.labels <- smoteTrain$labels
		upsampled.data <- smoteTrain[, (2:ncol(smoteTrain))]
	} else if (type == "GaussianNoise") {
		unloadNamespace("raster")
		library(gstat)
		library(UBL)
		smote.data <- cbind(data.frame(labels = labels), data)
		t <- as.data.frame(max(table(labels)) / table(labels))
		perc_list <- split(t[, 2], t[, 1])
		noiseTrain <- UBL::GaussNoiseClassif(labels ~ ., smote.data, 
			C.perc = perc_list,
			pert = .pert,
			repl = FALSE)
		upsampled.labels <- noiseTrain$labels
		upsampled.data <- noiseTrain[, (2:ncol(noiseTrain))]
		unloadNamespace("UBL")
		unloadNamespace("automap")
		unloadNamespace("gstat")
		unloadNamespace("spacetime")
		library(raster)
	} else if (type == "Under") {
		unloadNamespace("raster")
		library(gstat)
		library(UBL)
		smote.data <- cbind(data.frame(labels = labels), data)
		# t <- as.data.frame(max(table(labels)) / table(labels))
		# perc_list <- split(t[, 2], t[, 1])
		noiseTrain <- UBL::RandUnderClassif(labels ~ ., smote.data, 
			C.perc = "balance",
			repl = FALSE)
		upsampled.labels <- noiseTrain$labels
		upsampled.data <- noiseTrain[, (2:ncol(noiseTrain))]
		unloadNamespace("UBL")
		unloadNamespace("automap")
		unloadNamespace("gstat")
		unloadNamespace("spacetime")
		library(raster)
	} else if (type == "GaussianExtreme") {
		unloadNamespace("raster")
		library(gstat)
		library(UBL)
		smote.data <- cbind(data.frame(labels = labels), data)
		noiseTrain <- UBL::GaussNoiseClassif(labels ~ ., smote.data, 
			C.perc = "extreme",
			pert = 0.1,
			repl = FALSE)
		upsampled.labels <- noiseTrain$labels
		upsampled.data <- noiseTrain[, (2:ncol(noiseTrain))]
		unloadNamespace("UBL")
		unloadNamespace("automap")
		unloadNamespace("gstat")
		unloadNamespace("spacetime")
		library(raster)
	}
	return(list(data = upsampled.data, labels = upsampled.labels))
}