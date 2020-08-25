#' Get the inner folds for the nested resampling
#' @param FINAL `logical`, selects the type of runs, if `FALSE` trains, else predict 
#' @param NU `numeric`, number of folds for the \eqn{\nu}-fold cross-validation 
#' @param REPS `numeric`, number of repetitions for the repeated \eqn{\nu}-fold cross-validation
#' @param SPCV `logical`, if `TRUE` activates the spatial cross-validation. This also deactivates the stratification of the folds.
#' @return a `ResampleDesc` describing the type of resampling
#' @export
#' @keywords ml-benchmark
get_inner <- function(FINAL, NU, REPS, SPCV){
	if(FINAL){
		if (SPCV){
			INNER <- mlr::makeResampleDesc("SpRepCV", rep = REPS, folds = NU, stratify = FALSE)
		} else {
			INNER <- mlr::makeResampleDesc("RepCV", rep = REPS, folds = NU, stratify = TRUE)
		}
	} else {
		INNER <- mlr::makeResampleDesc("CV", iters = NU, stratify = TRUE)
	}
	return(INNER)
}

#' Retrieve the value of the benchmark tuning results
#' @param fpath a `character` or a `file.path` pointing to a directory containing the result files
#' @param fname `character`, one of "bestBMR_tune.Rds" or "bestBMR_lrnH.Rds"
#' @importFrom magrittr %>%
#' @return the benchmark tuning results as a `data.frame`
#' @export
#' @keywords ml-benchmark
get_bestBMR_tuning_results <- function(fpath, fname){
	if(!fname %in% c("bestBMR_tune.Rds", "bestBMR_lrnH.Rds")) stop(paste("Invalid", fname))
	if (!file.exists(file.path(fpath, fname))) stop(paste(fname, "not found in", fpath))
	bestBMR_tuning_results <- readRDS(file.path(fpath, fname)) %>%
		dplyr::mutate(task.id = gsub("SAC", "ALLSAC", task.id),
			learner.id = paste0("classif.", learner.id)) %>%
	dplyr::filter(learner.id %in% LRN_IDS) %>% 
	dplyr::filter(task.id %in% REGIONS)
	return(bestBMR_tuning_results )	
}

#' Saves the benchmark parameters
#' @param PATH path to the output directory
#' @param FINAL `logical`, selects the type of runs, if `FALSE` trains, else predict 
#' @export
#' @keywords ml-benchmark
savePARAMETERS <- function(PATH, FINAL){
	saveRDS(
		list(
			REGIONS = REGIONS, 
			LRN_IDS = LRN_IDS, 
			PREPROC = PREPROC, 
			TUNELENGTH = TUNELENGTH, 
			ITERS = ITERS, 
			PROB = PROB, 
			FINAL = FINAL, 
			PATH = PATH, 
			NU = NU, 
			REPS = REPS, 
			INNER = INNER,
			MES = MES,
			FS = FS,
			TUNE_FS = TUNE_FS,
			FS_NUM = FS_NUM
		), 
		file.path(PATH, paste0(ifelse(FINAL, "PARAMETERS.Rds", "PARAMETERS_BMR.Rds")))
	)
}