#' Get predictions
#' 
#' This function is a convenience wrapper around the `get_final_models()` and `compute_predictions()` functions.
#' 
#' @param REGIONS `character`, the `task.id` identifiers and the list of area of study
#' @param LRN_IDS `character`, the `learner.id` identifiers and the list of learners
#' @param mod.dir `character`, the path to the directory of the model, usually set to `PATH`
#' @param TARGET `logical`, default to `TRUE`, toggle the target or training datasets
#' @param TUNE_FS `logical`, toggle the behaviour for the feature selection
#' @return a named list of predictions
#' @keywords ml-predictions
#' @export
get_predictions <- function(REGIONS, LRN_IDS, mod.dir, TARGET = TRUE, TUNE_FS){
	mods <- get_final_models(mod.dir, REGIONS, TUNE_FS)
	preds <- compute_predictions(REGIONS, LRN_IDS, mods, TARGET, TUNE_FS)
	return(preds)
}

#' Retrieve the final (optimal) models
#' @param mod.dir `character`, the path to the directory of the model, usually set to `PATH`
#' @param REGIONS `character`, the `task.id` identifiers and the list of area of study
#' @param TUNE_FS `logical`, toggle the behaviour for the feature selection
#' @return a named list of models
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @keywords ml-predictions
#' @export
get_final_models <- function(mod.dir, REGIONS, TUNE_FS){
	# utils::globalVariables(c("PATH", "bestFeatureSets"))
	if (!TUNE_FS){
		mods <- lapply(REGIONS, function(reg) readRDS(file.path(mod.dir, paste0(reg, "_final.Rds"))))
	} else {
		dirList <- list.dirs(PATH, recursive = FALSE)
		pattern <- "_FS_"
		dirList <- dirList[grepl(pattern, dirList)]
		fileList <- lapply(dirList, function(dir) list.files(dir, pattern = "_final.Rds"))
		dirList <- dirList[which(sapply(fileList, length) != 0)]
		FS_NUM_LIST <- sapply(dirList, function(dir) unlist(strsplit(dir, pattern))[2])
		FS_NUM_LIST <- as.numeric(FS_NUM_LIST)
		# bestFeatureSets <- readRDS("./data/FS/bestFeatureSets_MLR_Predict.Rds")
		mods <- list()
		for (i in seq_along(REGIONS)){
			reg <- REGIONS[i]
			lrn_ids <- (bestFeatureSets %>% dplyr::filter(.data$task.id == reg))$learner.id
			mods[[i]] <- lapply(lrn_ids, function(lrn){
				dir <- dirList[FS_NUM_LIST == (bestFeatureSets %>% dplyr::filter(.data$task.id == reg, .data$learner.id == lrn))$min] 
				mod <- readRDS(file.path(dir, paste0(reg, "_final.Rds")))
				mod$mods[names(mod$mods) != lrn] <- NULL
				return(mod)
			})
			names(mods[[i]]) <- lrn_ids
		}
		names(mods) <- REGIONS
	}
	return(mods)
}

#' Retrieve the final (optimal) models
#' @param REGIONS `character`, the `task.id` identifiers and the list of area of study
#' @param LRN_IDS `character`, the `learner.id` identifiers and the list of learners
#' @param mods a named list of models returned by `get_final_models()`
#' @param TARGET `logical`, default to `TRUE`, toggle the target or training datasets
#' @param TUNE_FS `logical`, toggle the behaviour for the feature selection
#' @return a named list with predictions
#' @importFrom magrittr %>%
#' @import progress
#' @keywords ml-predictions
#' @export
compute_predictions <- function(REGIONS, LRN_IDS, mods, TARGET, TUNE_FS){
	pb <- progress_bar$new(format = "[:bar] :current/:total - :percent in :elapsed/:eta \n", total = length(REGIONS), show_after = 0)
	invisible(pb$tick(0))
	preds <- list()
	for (i in seq_along(REGIONS)){
		region <- REGIONS[i]
		if (!TUNE_FS){
			if(!TARGET){
				training_data <- mods[[region]]$training_data
				target_data <- training_data$data
				labels <- training_data$labels
			} else {
				labels <- NULL
				target_data <- get_target_data(region)
				raw_target_data <- sanitize_data(target_data)
				target_data <- preproc_data(raw_target_data, mods[[region]]$ppc, labels)
			}

			l.pred <- lapply(LRN_IDS, function(lrn){
				stats::predict(mods[[region]]$mods[[lrn]], newdata = target_data)
			})
			names(l.pred) <- LRN_IDS
			preds[[i]] <- list(pred = l.pred, labels = labels)
			pb$tick()
		} else {
			l.pred <- lapply(LRN_IDS, function(lrn){
				if(!TARGET){
					training_data <- mods[[region]][[lrn]]$training_data
					target_data <- training_data$data
					labels <- training_data$labels
				} else {
					labels <- NULL
					target_data <- get_target_data(region)
					raw_target_data <- sanitize_data(target_data)
					selectedFeatures <- colnames(mods[[region]][[lrn]]$training_data$data)
					raw_target_data <- raw_target_data[, colnames(raw_target_data) %in% selectedFeatures]
					target_data <- preproc_data(raw_target_data, mods[[region]][[lrn]]$ppc, labels)
					target_data <- target_data$data
				}
				stats::predict(mods[[region]][[lrn]]$mods[[lrn]], newdata = target_data)
			})
			names(l.pred) <- LRN_IDS
			if(!TARGET){
				labels <- mods[[region]][[1]]$training_data$labels
			} else {
				labels <- NULL
			}	
			preds[[i]] <- list(pred = l.pred, labels = labels)
			pb$tick()
		}
	}
	names(preds) <- REGIONS
	return(preds)
}

#' Performs a posterior multinomial calibration of machine learning predictions
#' @param REGIONS `character`, the `task.id` identifiers and the list of area of study
#' @param LRN_IDS `character`, the `learner.id` identifiers and the list of learners
#' @param preds `list`, output from `get_predictions()`
#' @param plot `logical`, if `TRUE` displays a calibration plot
#' @return the calibration models
#' @import glmnet
#' @importFrom magrittr %>%
#' @keywords ml-predictions
#' @export
get_calibrations <- function(REGIONS, LRN_IDS, preds, plot = FALSE){
	calibrations <- lapply(REGIONS, function(hydro_class){
		labels <- preds[[hydro_class]]$labels
		if (min(table(labels)) < 2){
			minority_label <- which(labels == names(which.min(table(labels))))
			labels[length(labels) + 1] <- labels[minority_label]
			labels[length(labels) + 1] <- labels[minority_label]
			labels[length(labels) + 1] <- labels[minority_label]
		}
		cals <- lapply(LRN_IDS, function(lrn){
			predictions <- preds[[hydro_class]]$pred[[lrn]]$data %>% dplyr::select(- "response")
			if (min(table(preds[[hydro_class]]$labels)) < 2){
				predictions <- rbind(predictions, predictions[minority_label, ])
				predictions <- rbind(predictions, predictions[minority_label, ])
				predictions <- rbind(predictions, predictions[minority_label, ])
			}
			predictions <- predictions[, order(colnames(predictions))]
			colnames(predictions) <- gsub("prob.", "", colnames(predictions))
			sigmoidalCal <- cv.glmnet(stats::model.matrix(~., data =  predictions), labels,  family = "multinomial")
			if (plot){
				calibrated_predictions <- stats::predict(sigmoidalCal, newx = stats::model.matrix(~., data =  predictions), type = "response")
				dim(calibrated_predictions) <- dim(predictions)
				colnames(calibrated_predictions) <- colnames(predictions)
				lvls <- colnames(calibrated_predictions)
				class.predictions <- apply(predictions, MARGIN = 1, FUN = function(row){
					lvls[which.max(row)]
				})

				graphics::par(mfrow = c(3, 1))
				if (hydro_class == "ALLSAC") hydro_class <- "SAC"
				uncalibrated_prob <- predictions[, order(as.numeric(gsub(hydro_class, "", colnames(calibrated_predictions))))]
				uncalibrated_prob <- uncalibrated_prob[order(as.numeric(gsub(hydro_class, "", class.predictions))), ]
				oce::imagep(t(uncalibrated_prob), col = oce::oce.colorsViridis(), decimate = FALSE, xlab = "Predicted posterior probabilities for each classes", ylab = "Training data ordered \n by observed classes", main = "Uncalibrated posterior probabilities", cex = 1)

				calibrated_prob <- calibrated_predictions[, order(as.numeric(gsub(hydro_class, "", colnames(calibrated_predictions))))]
				calibrated_prob <- calibrated_prob[order(as.numeric(gsub(hydro_class, "", class.predictions))), ]
				oce::imagep(t(calibrated_prob), col = oce::oce.colorsViridis(), decimate = FALSE, xlab = "Predicted posterior probabilities for each classes", ylab = "Training data ordered \n by observed classes", main = "Calibrated posterior probabilities", cex = 1)

				oce::imagep(t(calibrated_prob) - t(uncalibrated_prob), col = oce::oce.colors9B(), decimate = FALSE, xlab = "Predicted posterior probabilities for each classes", ylab = "Training data ordered \n by observed classes", main = "Absolute difference between uncalibrated and calibrated posterior probabilities", cex = 1)
			}
			return(sigmoidalCal)
		})
		names(cals) <- LRN_IDS
		return(cals)
	})
	names(calibrations) <- REGIONS
	return(calibrations)
}

#' Calibrate machine learning predictions
#' @param REGIONS `character`, the `task.id` identifiers and the list of area of study
#' @param LRN_IDS `character`, the `learner.id` identifiers and the list of learners
#' @param preds `list`, output from `get_predictions()`
#' @param calibrations `list`, output from `get_calibrations()`
#' @return a list of calibrated predictions
#' @import glmnet
#' @importFrom magrittr %>%
#' @keywords ml-predictions
#' @export
calibrated_predictions <- function(REGIONS, LRN_IDS, preds, calibrations){
	calibrated_predictions <- lapply(REGIONS, function(hydro_class){
		cal_preds <- lapply(LRN_IDS, function(lrn){
			predictions <- preds[[hydro_class]]$pred[[lrn]]$data %>% dplyr::select(- "response")
			predictions <- predictions[, order(colnames(predictions))]
			colnames(predictions) <- gsub("prob.", "", colnames(predictions))
			sigmoidalCal <- calibrations[[hydro_class]][[lrn]]
			calibrated_predictions <- stats::predict(sigmoidalCal, newx = stats::model.matrix(~., data =  predictions), type = "response")
			dim(calibrated_predictions) <- dim(predictions)
			colnames(calibrated_predictions) <- colnames(predictions)
			return(calibrated_predictions)
		})
		names(cal_preds) <- LRN_IDS
		return(cal_preds)
	})
	names(calibrated_predictions) <- REGIONS
	return(calibrated_predictions)
}

#' Calculates Shannon-Weiner entropy
#' @param l a list of values numeric or factor
#' @param .prob_bool if TRUE, l is understood as a list of probabilities, if FALSE l is understood as a factor
#' @param units units of the logarithm in the calculation of entropy
#' @return Shannon-Weiner entropy
#' @keywords ml-predictions
#' @export
shannon_weiner <- function(l, .prob_bool = FALSE, units = exp(1)){
	if (.prob_bool){
		relative_prob <- l
	} else {
		lvls <- levels(l)
		relative_prob <- sapply(lvls, function(lvl) length(which(l == lvl))/length(l))		
	}
	shannon_weiner_value <- sapply(relative_prob, function(prob) prob*log(prob, base = units))
	shannon_weiner_value <- - sum(na.omit(shannon_weiner_value))
	return(shannon_weiner_value)
}

#' Calculate the richness (number of different species)
#' @param l a list of values numeric or factor
#' @return the richness
#' @keywords ml-predictions
#' @export
richness <- function(l){
	rich <- length(unique(l))
	return(rich)
}

#' Calculate Simpson's evenness
#' @param l a list of values numeric or factor
#' @param .prob_bool if TRUE, l is understood as a list of probabilities, if FALSE l is understood as a factor
#' @return Simpson's evenness
#' @keywords ml-predictions
#' @export
simpson_evenness <- function(l, .prob_bool = FALSE){
	if (.prob_bool){
		relative_prob <- l
	} else {
		lvls <- levels(l)
		relative_prob <- sapply(lvls, function(lvl) length(which(l == lvl))/length(l))		
	}
	D <- relative_prob^2
	D <- 1 / sum(na.omit(D))
	D_max <- richness(l)
	evenness <- D / D_max
	return(evenness)
}

#' Formats entropy rate results
#' @param entropy_rate a list of list
#' @importFrom magrittr %>%
#' @export
#' @return a formatted `data.frame`
#' @keywords ml-predictions
get_entropy_df <- function(entropy_rate){
	entropy_rate_df <- reshape2::melt(t(as.data.frame(entropy_rate))) %>% 
		dplyr::rename(region = .data$Var1, learner.id = .data$Var2, entropy_rate = .data$value) %>%
		dplyr::mutate(region = gsub("ALLSAC", "SAC", .data$region)) %>%
		dplyr::arrange(.data$region)
	return(entropy_rate_df)
}