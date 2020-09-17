#' Get all the benchmark result in a directory of directories
#' 
#' This function walks the tree of folder to retrieve all the benchmark results usually organized by learners then by regions.
#' The function leverages `get_BMR()`, `mlr::getBMRPerformances()` and `mlr::getBMRTuneResults()`.
#' 
#' @param PATH initial path
#' @param pattern `character`, default to `FS_`, the pattern for selecting the sub-directories
#' @return a named list with two elements `perf` and `tune` the benchmark performance results and the tuning results
#' @keywords ml-postprocess
#' @import progress
#' @export
getAllBMRS <- function(PATH, pattern = "FS_"){
	dirList <- list.dirs(PATH, recursive = FALSE)
	dirList <- dirList[grepl(pattern, dirList)]
	FS_NUM_LIST <- sapply(dirList, function(dir) unlist(strsplit(dir, pattern))[2])
	FS_NUM_LIST <- as.numeric(FS_NUM_LIST)
	dirList <- dirList[order(FS_NUM_LIST)]
	FS_NUM_LIST <- sort(FS_NUM_LIST)
	BMR_perf <- list()
	BMR_tune <- list()
	pb <- progress_bar$new(format = "[:bar] :current/:total - :percent in :elapsed/:eta \n", total = length(dirList), show_after = 0)
	invisible(pb$tick(0))
	for (i in seq_along(dirList)){ 
		dir <- dirList[i]
		BMR <- get_BMR(dir)
		perf <- mlr::getBMRPerformances(BMR, as.df = TRUE)
		perf$FS_NUM <- FS_NUM_LIST[i]
		BMR_perf[[i]] <- perf
		if (pattern != "baseline_"){
			tune <- mlr::getBMRTuneResults(BMR, as.df = TRUE)
			tune$FS_NUM <- FS_NUM_LIST[i]
			BMR_tune[[i]] <- tune
		}
		pb$tick()
	}
	BMR_perf <- do.call(rbind, BMR_perf)
	if (pattern != "baseline_")	BMR_tune <- do.call(rbind, BMR_tune)
	return(list(perf = BMR_perf, tune = BMR_tune))
}

#' Retrieve benchmark results
#' 
#' This function is a convenience wrapper around `mlr::mergeBenchmarkResults()`.
#' 
#' @param BMR_dir `character`, base directory to look for each regional benchmark
#' @return a `mlr` benchmark results object 
#' @keywords ml-postprocess
#' @export
get_BMR <- function(BMR_dir){
	PARAMETERS_BMR <- readRDS(file.path(BMR_dir, "PARAMETERS_BMR.Rds"))
	l.BMR <- lapply(PARAMETERS_BMR$REGIONS, function(reg) readRDS(file.path(BMR_dir, paste0(reg,"_benchmark.Rds"))))
	BMR <- mlr::mergeBenchmarkResults(l.BMR)
	names(BMR$results) <- gsub(" SMOTE no StreamCAT", "", names(BMR$results))
	names(BMR$results) <- gsub("ALLSAC", "SAC", names(BMR$results))
	return(BMR)
}

#' Selecting elements from `BMR_res` and cosmetic changes
#' 
#' The cosmetic changes are simple name-fixing in the column and in the `task.id`.
#' 
#' @param BMR_res a `BMR_res` obtained from `get_BMR()` (or `getAllBMRS()`)
#' @param type `character`, `perf` or `tune`
#' @keywords ml-postprocess
#' @return a `data.frame` with the selected results
#' @export
getBMR_perf_tune <- function(BMR_res, type = "perf"){
	if(!type %in% c("perf", "tune")) stop("type should be 'perf' or 'tune'")
	BMR_perf <- BMR_res[[type]]
	BMR_perf$task.id <- gsub(" SMOTE", "", BMR_perf$task.id)
	if (type == "tune") colnames(BMR_perf) <- gsub(".test.mean", "", colnames(BMR_perf))
	return(BMR_perf)
}

#' Make average AUC plot
#' @param BMR_perf a `data.frame` of benchmark performance results
#' @return a `ggplot` object
#' @keywords ml-postprocess
#' @importFrom magrittr %>%
#' @export
makeAverageAUCPlot <- function(BMR_perf){
	BMR_avg <- BMR_perf %>% dplyr::group_by(task.id, learner.id, FS_NUM) %>% dplyr::summarize(avg = mean(multiclass.au1u))
	p_avg <- ggplot2::ggplot(BMR_avg %>% dplyr::filter(learner.id != "featureless"), ggplot2::aes(x = FS_NUM, y = avg, group = learner.id, color = learner.id)) + 
		ggplot2::geom_line() +
		ggplot2::facet_wrap(~ task.id, nrow = 2) +
		ggplot2::labs(x = "Number of Predictors", 
			y = "Multiclass 1v1 AUC") +
		ggpubr::theme_pubr()
	return(p_avg)
}

#' Make average accuracy plot
#' @param BMR_perf a `data.frame` of benchmark performance results
#' @return a `ggplot` object
#' @keywords ml-postprocess
#' @importFrom magrittr %>%
#' @export
makeAverageAccPlot <- function(BMR_perf){
	BMR_acc <- BMR_perf %>% dplyr::group_by(task.id, learner.id, FS_NUM) %>% dplyr::summarize(avg = mean(acc))
	p_acc <- ggplot2::ggplot(BMR_acc, ggplot2::aes(x = FS_NUM, y = avg, group = learner.id, color = learner.id)) + 
		ggplot2::geom_line() +
		ggplot2::facet_grid(~ task.id) +
		ggplot2::labs(x = "Number of Predictors", 
			y = "Accuracy") +
		ggpubr::theme_pubr()
	return(p_acc)
}

#' Make training time plot
#' @param BMR_perf a `data.frame` of benchmark performance results
#' @return a `ggplot` object
#' @keywords ml-postprocess
#' @importFrom magrittr %>%
#' @export
makeTotalTimetrainPlot <- function(BMR_perf){
	BMR_time <- BMR_perf %>% dplyr::group_by(task.id, learner.id, FS_NUM) %>% dplyr::summarize(time = sum(timetrain) / 3600)
	p_timetrain <- ggplot2::ggplot(BMR_time, ggplot2::aes(x = FS_NUM, y = time, group = learner.id, color = learner.id)) + 
		ggplot2::geom_smooth(fill = NA, lwd = 1) +
		ggplot2::geom_line(alpha = 0.5) +
		ggplot2::facet_grid(~ task.id) +
		ggplot2::labs(x = "Number of Predictors", 
			y = "Time for 100 iterations of model training (hours)") +
		ggpubr::theme_pubr() +
		ggplot2::scale_y_log10(
				breaks = scales::trans_breaks(n = 5, 'log10', function(x) 10^x),
	            labels = scales::trans_format('log10', scales::math_format(10^.x))
	            	)
	return(p_timetrain)
}


#' Make an example plot of model selection
#' @param BMR_perf a `data.frame` from `getBMR_perf_tune()`
#' @param lrn `character`, mlr `learner.id`
#' @param confidence_level `numeric`, confidence level
#' @param window_size `numeric`, width of the search window
#' @return a named list with names from `task.id`
#' @keywords ml-postprocess
#' @importFrom magrittr %>%
#' @export
makeExampleModelSelectionPlot <- function(BMR_perf, lrn = c("svm", "randomForest"), confidence_level = 0.05, window_size = 7){
	plim <-  confidence_level / window_size
	p_examples <- lapply(unique(BMR_perf$task.id), function(reg){
		BMR_df <- BMR_perf %>% dplyr::filter(task.id %in% reg, learner.id %in% lrn)

		stat_test <- BMR_df %>% dplyr::filter(learner.id %in% lrn) %>%
			  dplyr::mutate(FS_NUM = as.factor(FS_NUM)) %>%
			  dplyr::group_by(learner.id, task.id) %>% 
			  rstatix::dunn_test(multiclass.au1u ~ FS_NUM, p.adjust.method = "none")

		ypos = 1
		step_increase = 0.05

		stat_test <- stat_test %>% as.data.frame() %>%
					dplyr::filter(as.numeric(group2) <= (as.numeric(group1) + (window_size - 1))) %>%
			  		dplyr::mutate(y.position = ypos) %>%
			  		dplyr::filter(p.adj <= plim)

		p_example <- ggpubr::ggboxplot(BMR_df %>% 
			dplyr::filter(learner.id %in% lrn) %>% 
			dplyr::mutate(FS_NUM = as.numeric(as.character(FS_NUM))), 
			x = "FS_NUM", 
			y = "multiclass.au1u", 
			fill = "learner.id", 
			facet.by = c("learner.id", "task.id")) +
			ggpubr::stat_pvalue_manual(
				stat_test,  
				label = "p.adj.signif",
				step.increase = step_increase, 
				step.group.by = c("learner.id", "task.id"), 
				label.size = -Inf,
				hide.ns = TRUE) +
			ggplot2::labs(x = "Number of Predictors", 
				y = "Multiclass 1v1 AUC") + 
			ggpubr::theme_pubr()
		return(p_example)
	})
	names(p_examples) <- unique(BMR_perf$task.id)
	return(p_examples)
}

#' Visualize the influence of window size on model selection
#' @param BMR_perf a `data.frame` from `getBMR_perf_tune()`
#' @param selectedPATH `character`, path to the selected features
#' @param lrn `character`, mlr `learner.id`
#' @return a `ggplot` object
#' @keywords ml-postprocess
#' @importFrom magrittr %>%
#' @export
makeWindowInfluencePlot <- function(BMR_perf, selectedPATH, lrn = "randomForest"){
	Y2_data <- regional_characteristics %>% dplyr::mutate(region = gsub("ALLSAC", "SAC", region)) %>% dplyr::arrange(region)
	l_bestFeatureSets <- lapply(seq(2,49), function(x){
		df <- get_bestFeatureSets(BMR_perf, selectedPATH, window_size = x) %>% 
			dplyr::filter(learner.id == lrn) %>%
			dplyr::mutate(window_size = x, min = as.numeric(min), min_per_class = min / Y2_data$n.classes)
		return(df)
	})
	bestFeatureSets_df <- do.call(rbind, l_bestFeatureSets)  %>% dplyr::select(-features, -learner.id)

	p_window <- ggplot2::ggplot(bestFeatureSets_df, ggplot2::aes(x = window_size, y = min_per_class, group = task.id, color = task.id)) +
		ggplot2::geom_line() + 
		ggplot2::geom_point() + 
		ggplot2::labs(x = "Window size", 
			y = "Number of Predictors per Number of Classes") +
		ggpubr::theme_pubr()
	return(p_window)
}

#' Get the optinmal feature sets
#' @param BMR_perf a `data.frame` from `getBMR_perf_tune()`
#' @param selectedPATH `character`, path to the selected features
#' @param lrn `character`, mlr `learner.id`
#' @param window_size `numeric`, width of the search window
#' @param confidence_level `numeric`, confidence level
#' @return a `data.frame` with the optimal model number of features and selected features per `task.id` and `learner.id`
#' @keywords ml-postprocess
#' @importFrom magrittr %>%
#' @export
get_bestFeatureSets <- function(BMR_perf, selectedPATH, lrn = c("svm", "randomForest", "nnTrain"), window_size = 5, confidence_level = 0.05){
	plim <- confidence_level / window_size
	stat_test <- BMR_perf %>% dplyr::filter(learner.id %in% lrn) %>%
		  dplyr::group_by(learner.id, task.id) %>%
		  rstatix::dunn_test(multiclass.au1u ~ FS_NUM, p.adjust.method = "none")	

	foo <- function(.x, window_size){
		if (nrow(.x %>% dplyr::filter(as.numeric(p.adj) <= plim)) == 0){
			return(dplyr::summarize(.x, min = min(group1)))
		} 
		.x <- .x %>% dplyr::filter(as.numeric(p.adj) <= plim)
		ind <- which(.x$group2 <= (.x$group1 + (window_size - 1)))
		if (length(ind) > 0){
			.x <- .x %>% 
			dplyr::filter(group2 <= (group1 + (window_size - 1))) %>%
			dplyr::filter(group1 == max(group1)) %>%
			dplyr::summarize(min = min(group2))
		} else {
			ind <- which.min(.x$group1)
			.x <- .x[ind, ] %>% 
			dplyr::summarize(min = min(group1))
		}
		return(.x)
	}

	bestFeatureSets <- stat_test %>% 
		dplyr::group_by(task.id, learner.id) %>%
		dplyr::mutate(group1 = as.numeric(group1), group2 = as.numeric(group2)) %>%
		dplyr::group_modify(~ foo(.x, window_size)) %>% 
		dplyr::ungroup() %>%
		dplyr::mutate(task.id = gsub("SAC", "ALLSAC", task.id), min = as.character(min))
	bestFeatureSets <- bestFeatureSets %>% dplyr::mutate(features = apply(bestFeatureSets, MARGIN = 1, function(row){
		feats <- readRDS(file.path(selectedPATH, row[3], paste0(row[1], "_selectedFeatures.Rds")))
		do.call(paste, as.list(feats))
		}
	))
	return(bestFeatureSets)
}


#' Create feature importance across all regions of study
#' 
#' This function is a wrapper around `makeFeatureImportancePlot()` and additionally highlights which features are selected in the optimal feature sets.
#' 
#' @param BMR_perf a `data.frame` of benchmark performance results
#' @param selectedPATH `character`, path to the selected features
#' @param bestFeatureSets the optimal selected features for each regions
#' @return a named list of feature importance plot with names pulled from `task.id`
#' @keywords ml-postprocess
#' @importFrom magrittr %>%
#' @export
makeAllFeatureImportancePlotFS <- function(BMR_perf, selectedPATH, bestFeatureSets){
	feature_df <- data.frame(
		task.id = rep(unique(BMR_perf$task.id), length(unique(BMR_perf$FS))),	
		FS = rep(unique(BMR_perf$FS), length(unique(BMR_perf$task.id)))
		) %>% dplyr::arrange(FS) %>%
		dplyr::mutate(
			task.id = gsub("SAC", "ALLSAC", task.id),
			FS = as.character(FS))
	feature_df <- dplyr::mutate(feature_df, features = apply(feature_df, MARGIN = 1, function(row){
		feats <- readRDS(file.path(selectedPATH, row[2], paste0(row[1], "_selectedFeatures.Rds")))
		do.call(paste, as.list(feats))
		}
	)) %>% dplyr::mutate(task.id = gsub("ALLSAC", "SAC", task.id))

	FeatureImportancePlots <- lapply(unique(feature_df$task.id), function(reg){
		bestFeatureSet <- bestFeatureSets %>% dplyr::filter(task.id == reg) %>% 
			dplyr::mutate(min = as.numeric(min)) %>%
			dplyr::filter(learner.id == "randomForest") %>%
			dplyr::filter(min == min(min)) %>%
			dplyr::select("features") %>%
			lapply(strsplit, " ") %>% unlist()

		allSelectedFeatures <- feature_df %>% dplyr::filter(task.id == reg) %>% dplyr::select("features") %>% 
			lapply(strsplit, " ") %>% unlist() %>% table() %>% as.data.frame() 
		colnames(allSelectedFeatures) <- c("var", "Overall") 
		allSelectedFeatures <- dplyr::arrange(allSelectedFeatures, -Overall)
		allSelectedFeatures$best <- allSelectedFeatures$var %in% bestFeatureSet
		allSelectedFeatures$best <- ifelse(allSelectedFeatures$best, "optimal RF feature set", "other sets")

		p <- makeFeatureImportancePlot(allSelectedFeatures, first = 30, best = TRUE)
		p + ggplot2::labs(title = gsub("ALLSAC", "SAC", reg))
	})
	names(FeatureImportancePlots) <- unique(feature_df$task.id)
	return(FeatureImportancePlots)
}


#' Makes a dot chart of feature importance
#' @param FeatureImportance a `data.frame` with column `Overall` and `var`
#' @param first `numeric`, number of feature to display
#' @param best `logical`, default to `FALSE`, to highlight which features are selected in an optimal (best) set
#' @return a `ggplot` object
#' @keywords ml-postprocess
#' @export
makeFeatureImportancePlot <- function(FeatureImportance, first = 20, best = FALSE){
	FeatureImportance <- fixVarNames(FeatureImportance, var = "var")
	FeatureImportance$type <- factor(FeatureImportance$type, 
		levels = c("TAM", "GIS", "Statistical roughness", "Topology", "Contextual"),
		labels = c("TAM", "GIS", "Statistical roughness", "Topology", "Contextual"))
	FeatureImportance <- head(FeatureImportance, first)
	if (best){
		p <-    ggplot2::ggplot(FeatureImportance, ggplot2::aes(color = type)) +
		        ggplot2::geom_point(ggplot2::aes(x=Overall, y=forcats::fct_reorder(var,Overall))) +
				ggplot2::geom_segment(ggplot2::aes(linetype = best, x = 0, xend = Overall, y = forcats::fct_reorder(var,Overall), yend = forcats::fct_reorder(var,Overall))) +
		        ggplot2::labs(title = "RF model importance (top 20 variables)", y = "variable", x = "variable importance") + 
				ggplot2::theme_minimal() +
		        ggplot2::scale_color_discrete(drop = FALSE)
		p	
	} else {
		p <-    ggplot2::ggplot(FeatureImportance, ggplot2::aes(color = type)) +
		        ggplot2::geom_point(ggplot2::aes(x=Overall, y=forcats::fct_reorder(var,Overall))) +
				ggplot2::geom_segment(ggplot2::aes(x = 0, xend = Overall, y = forcats::fct_reorder(var,Overall), yend = forcats::fct_reorder(var,Overall))) +
		        ggplot2::labs(title = "RF model importance (top 20 variables)", y = "variable", x = "variable importance") + 
				ggplot2::theme_minimal() +
		        ggplot2::scale_color_discrete(drop = FALSE)
		p		
	}
}

#' Fixes variable names for data visualization purpose
#' @param df a `data.frame`
#' @param var `character` the name of the `var` column in `df
#' @return a `data.frame` with fixed variable names
#' @keywords ml-postprocess
#' @export
fixVarNames <- function(df, var = "var"){
	df[[var]] <- gsub("H.", "Hurst Coefficient ", df[[var]], fixed = TRUE)
	df[[var]] <- gsub("SLOPE", "Slope", df[[var]])
	df[[var]] <- gsub("SO", "Stream order", df[[var]])
	df[[var]] <- gsub("CONFINEMEN", "Valley confinement", df[[var]])
	df[[var]] <- gsub("WsAreaSqKm", "Watershed Drainage area", df[[var]])
	df[[var]] <- gsub("CatAreaSqKm", "Local Drainage area", df[[var]])
	df[[var]] <- gsub("LDD", "Local Drainage Density", df[[var]])
	df[[var]] <- gsub("_skew", " skewness ", df[[var]])
	df[[var]] <- gsub("_sd", " standard deviation ", df[[var]])
	df[[var]] <- gsub("_min", " min ", df[[var]])
	df[[var]] <- gsub("_max", " max ", df[[var]])
	df[[var]] <- gsub("_mean", " mean ", df[[var]])
	df[[var]] <- gsub("_median", " median ", df[[var]])
	df[[var]] <- gsub("flowdir", "Flow direction", df[[var]])
	df[[var]] <- gsub("curvplan", "Curvature (planform)", df[[var]])
	df[[var]] <- gsub("curvprof", "Curvature (profile)", df[[var]])
	df[[var]] <- gsub("aspect", "Aspect", df[[var]])
	df[[var]] <- gsub(".rstr", "(raster)", df[[var]])
	df[[var]] <- gsub(".nrch", "(near)", df[[var]])
	df[[var]] <- gsub("tpi", "TPI", df[[var]])
	df[[var]] <- gsub("tri", "TRI", df[[var]])
	df[[var]] <- gsub("roughness", "Roughness", df[[var]])
	df[[var]] <- gsub("elevation", "Elevation", df[[var]])
	df[[var]] <- gsub("layer", "Elevation", df[[var]])
	df[[var]] <- gsub("north_california_NED_13", "Elevation", df[[var]])
	df[[var]] <- gsub("slope", "Slope", df[[var]])
	df$type <- sapply(df[[var]], function(var){
		if (grepl("Hurst", var)){
			return("Statistical roughness")
		} else if (grepl("\\(", var)) {
			return("TAM")
		} else if (var %in% c("Valley confinement", "Slope")){
			return("GIS")
		} else if (var %in% c("Local Drainage area", "Watershed Drainage area", "Local Drainage Density", "Stream order")){
			return("Topology")
		} else {	
			return("Contextual")
		}
	})
	return(df)
}

#' Get the frequency of selection of a given feature across all regions
#' @param bestFeatureSets the optimal selected features for each regions
#' @return a `data.frame`
#' @keywords ml-postprocess
#' @importFrom magrittr %>%
#' @export
getFreqBestFeatureSets <- function(bestFeatureSets){
	bestFeatureSets <- bestFeatureSets %>% dplyr::mutate(task.id = gsub("ALLSAC", "SAC", task.id), min = as.numeric(min))
	freq_bestFeatureSets <- dplyr::group_by(bestFeatureSets, task.id) %>% 
		dplyr::filter(learner.id == "randomForest") %>%
		dplyr::filter(min == min(min)) %>% dplyr::ungroup()
	freq_bestFeatureSets <- freq_bestFeatureSets[!duplicated(freq_bestFeatureSets$task.id), ]
	freq_bestFeatureSets <- rstatix::freq_table(unlist(lapply(freq_bestFeatureSets$features, function(fl) strsplit(fl, " ")))) %>% dplyr::mutate(prop = n / nrow(freq_bestFeatureSets))
	return(freq_bestFeatureSets)	
}

#' Get the tuning results of the optimal models
#' @param BMR_tune a `data.frame` containing the results of the tuning
#' @param bestFeatureSets the optimal selected features for each regions
#' @return a `data.frame` subsetted to correspond to the `bestFeatureSets`
#' @keywords ml-postprocess
#' @importFrom magrittr %>%
#' @export
getBestBMRTune <- function(BMR_tune, bestFeatureSets){
	bestBMR_tune <- dplyr::left_join(
		bestFeatureSets %>% dplyr::mutate(task.id = gsub("ALLSAC", "SAC", task.id), min = as.numeric(min)),
		BMR_tune, 
		by = c("task.id" = "task.id", "learner.id" = "learner.id", "min" = "FS_NUM")
		) %>%
	dplyr::rename(FS_NUM = min) 
	return(bestBMR_tune)
}

#' Make a violin plot comparing the results from the optimal models
#' 
#' The significant difference between the models are shown with a t-test with unequal variance
#' 
#' @param bestBMR_tune a `data.frame` containing the results of the optimal models
#' @return a `ggplot` object
#' @keywords ml-postprocess
#' @importFrom magrittr %>%
#' @export
makeBestTuneAUCPlot <- function(bestBMR_tune){
	stat_test <- bestBMR_tune %>% dplyr::group_by(task.id) %>% rstatix::t_test(multiclass.au1u ~ learner.id, var.equal = FALSE)
	p_bestTuneAUC <- ggpubr::ggviolin(bestBMR_tune, 
		x = "learner.id", 
		y = "multiclass.au1u", 
		facet.by = c("task.id")) +
		ggpubr::stat_pvalue_manual(
			stat_test %>% as.data.frame() %>% dplyr::mutate(y.position = 1.05),  
			label = "p.adj.signif",
			step.increase = 0.1, 
			step.group.by = c("task.id"), 
			hide.ns = TRUE) +
		ggplot2::labs(x = "ML Model", 
			y = "Multiclass 1v1 AUC") + 
		ggpubr::theme_pubr() +
		ggplot2::scale_x_discrete(labels=c(
			# "xgboost" = "XGB",
			"nnTrain" = "ANN",
			"randomForest" = "RF",
			"svm" = "SVM"))
	return(p_bestTuneAUC)
}


#' Get hyper-parameters names
#' @param lrn a `mlr` `learner.id`
#' @return `character` list of the hyper-parameter(s) name(s)
#' @keywords ml-postprocess
#' @export
getHyperparNames <- function(lrn){
	vars <- switch(as.character(lrn), 
		"randomForest" = c("mtry"),
		"svm" = c("cost"),
		"nnTrain" = c("max.number.of.layers", "hidden", "learningrate", "batchsize", "momentum", "visible_dropout", "hidden_dropout")
	)
	return(vars)
}

#' Calculates entropy
#' @param var numeric, vector of values
#' @return entropy in log2 units
#' @keywords ml-postprocess
#' @export
normH <- function(var){
	prop <- rstatix::freq_table(var)$prop
	philentropy::H(prop / sum(prop), unit = "log2")
}

#' Compute the tuning entropy
#' @param BMR_tune a `data.frame` containing the results of the tuning
#' @param TUNELENGTH `numeric`, tuning length
#' @param maxH_nnTrain `numeric` the maximum value for entropy for the `nnTrain` learner
#' @return a `data.frame`
#' @keywords ml-postprocess
#' @importFrom magrittr %>%
#' @export
getBMRTuningEntropy <- function(BMR_tune, TUNELENGTH, maxH_nnTrain = sum(sapply(c(5, 7,5,3,5,3,3), log2))){
	BMR_lrnH <- BMR_tune %>% dplyr::group_by(task.id, FS_NUM, learner.id) %>% 
		dplyr::group_modify(~ 
			dplyr::summarize_at(.x, getHyperparNames(unique(.y$learner.id)), normH) %>%
			dplyr::transmute(total_tuning_entropy = rowSums(.))
		) %>%	
		reshape2::melt(id.vars = c("task.id", "learner.id", "FS_NUM"))
	BMR_lrnH$value[BMR_lrnH$learner.id == "svm"] <- BMR_lrnH$value[BMR_lrnH$learner.id == "svm"] / log2(TUNELENGTH)	
	BMR_lrnH$grid_size <- BMR_lrnH$FS_NUM - 1
	BMR_lrnH$grid_size[BMR_lrnH$grid_size > TUNELENGTH] <- TUNELENGTH
	BMR_lrnH$value[BMR_lrnH$learner.id == "randomForest"] <- BMR_lrnH$value[BMR_lrnH$learner.id == "randomForest"] / log2(BMR_lrnH$grid_size[BMR_lrnH$learner.id == "randomForest"])
	BMR_lrnH$value[BMR_lrnH$learner.id == "nnTrain"] <- BMR_lrnH$value[BMR_lrnH$learner.id == "nnTrain"] / maxH_nnTrain
	BMR_lrnH$value[is.na(BMR_lrnH$value)] <- 0
	return(BMR_lrnH)
}

#' Get the tuning entropy corresponding to the optimal models
#' @param BMR_lrnH a `data.frame` generated with `getBMRTuningEntropy()`
#' @param bestFeatureSets the optimal selected features for each regions
#' @param bestBMR_tune a `data.frame` containing the results of the tuning for optimal models
#' @importFrom magrittr %>%
#' @keywords ml-postprocess
#' @export
getBestBMRTuningEntropy <- function(BMR_lrnH, bestFeatureSets, bestBMR_tune){
	bestBMR_lrnH <- dplyr::left_join(
		bestFeatureSets %>% dplyr::mutate(task.id = gsub("ALLSAC", "SAC", task.id), min = as.numeric(min)), 
		BMR_lrnH, 
		by = c("task.id" = "task.id", "learner.id" = "learner.id", "min" = "FS_NUM")
		) %>%
		dplyr::rename(FS_NUM = min, tuning_entropy = value) %>%
		dplyr::select(- "variable")
	bestBMR_lrnH <- dplyr::left_join(
		bestBMR_lrnH, 
		bestBMR_tune %>%
		reshape2::melt(measure.vars = c("cost", "mtry", "max.number.of.layers")) %>%
		dplyr::group_by(task.id, learner.id, variable, FS_NUM) %>%
		dplyr::summarize(
			min_hyperpar = min(value), 
			max_hyperpar = max(value),
			len_hyperpar = length(unique(value)),
			mfv_hyperpar = min(modeest::mfv(value))) %>%
		na.omit(),
		by = c("task.id", "learner.id", "FS_NUM")
		)
	bestBMR_lrnH <- dplyr::left_join(
		bestBMR_lrnH, 
		bestBMR_tune %>%
		dplyr::group_by(task.id, learner.id, FS_NUM) %>%
		dplyr::summarize(
			mean_auc = mean(multiclass.au1u),
			mean_timetrain = mean(timetrain),
			mean_acc = mean(acc)) %>%
		na.omit(),
		by = c("task.id", "learner.id", "FS_NUM")
	)
	return(bestBMR_lrnH)
}

#' Create a plot of the evolution of tuning entropy with the number of selected features
#' @param BMR_lrnH a `data.frame` generated with `getBMRTuningEntropy()`
#' @param bestBMR_lrnH a `data.frame` with the tuning entropy corresponding to the optimal models
#' @importFrom magrittr %>%
#' @keywords ml-postprocess
#' @export
makeTuningEntropyPlot <- function(BMR_lrnH, bestBMR_lrnH){
	p_Hnorms <- lapply(unique(BMR_lrnH$task.id), function(task){
		p_Hnorm <- ggplot2::ggplot(BMR_lrnH %>% dplyr::filter(task.id == task), ggplot2::aes(x = FS_NUM, y = value, group = learner.id, color = learner.id)) + 
			ggplot2::geom_line() +
			ggplot2::stat_smooth(fill=NA, alpha=1, lwd = 1.5) +
			ggplot2::geom_point(data = bestBMR_lrnH %>% dplyr::filter(task.id == task), ggplot2::aes(x = FS_NUM, y = tuning_entropy), size = 4) +
			ggplot2::facet_grid(~ task.id) +
			ggplot2::labs(x = "Number of Predictors", 
				y = "Normalized Tuning Entropy") +
			ggpubr::theme_pubr()
		return(p_Hnorm)	
		})
	names(p_Hnorms) <- unique(BMR_lrnH$task.id)
	return(p_Hnorms)
}


#' Plot the distribution of hyper-parameters resulting from the nested resampling
#' @param BMR a `data.frame` with tuning results
#' @param lrn `character` a `mlr` `learner.id`
#' @return a `ggplot` object
#' @importFrom magrittr %>%
#' @keywords ml-postprocess
#' @export
getTunePlot <- function(BMR, lrn){
	tuneResults <- BMR %>% dplyr::filter(learner.id == lrn) %>% dplyr::select(dplyr::all_of(c("task.id", getHyperparNames(lrn))))
	TunePlot <- ggplot2::ggplot(reshape2::melt(tuneResults, id.vars = "task.id") %>% dplyr::mutate(value = as.factor(value)), ggplot2::aes(x = value, fill = task.id, color = task.id)) +
		ggplot2::geom_bar(position=ggplot2::position_dodge2()) +
		ggpubr::theme_pubr() +
		ggplot2::labs(x = "") +
		ggplot2::facet_wrap(~ variable, scales = "free_x")
	return(TunePlot)	
}