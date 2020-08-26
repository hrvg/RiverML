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


#' Get the outer folds for the nested resampling
#' @param nu `numeric`, number of folds for the \eqn{\nu}-fold cross-validation 
#' @param reps `numeric`, number of repetitions for the repeated \eqn{\nu}-fold cross-validation
#' @param labels `character` or `factor` the labels
#' @param tasks list of `mlr` tasks
#' @return `mlr` `resampleDesc`
#' @export
#' @keywords ml-benchmark
get_outers <- function(nu = 5, reps =10, labels, tasks){
	# min_rep <- max(2, nu %% min(min(table(labels)), 5))
	# min_fold <- min(5, min(table(labels)))
	if (reps >= 2){
		outer <- makeResampleDesc("RepCV", rep = reps, folds = nu, stratify = TRUE)
		outer_spcv <- makeResampleDesc("SpRepCV", rep = reps, folds = nu,)
		outer_smote <- makeResampleDesc("RepCV", rep = reps, folds = nu, stratify = TRUE)
		outer_smote_spcv <- makeResampleDesc("SpRepCV", rep = reps, folds = nu)
	} else {
		outer <- makeResampleDesc("CV", iters = nu, stratify = TRUE)
		outer_spcv <- makeResampleDesc("SpCV", iters = nu,)
		outer_smote <- makeResampleDesc("CV", iters = nu, stratify = TRUE)
		outer_smote_spcv <- makeResampleDesc("SpCV", iters = nu)
	}
	outers <- lapply(tasks, function(task){
		if(grepl("SMOTE", task$task.desc$id)){
			if(grepl("spcv", task$task.desc$id)) {
				return(outer_smote_spcv) 
			}
			else{
				return(outer_smote)
			}
		} else {
			if(grepl("spcv", task$task.desc$id)) {
				return(outer_spcv) 
			}
			else{
				return(outer)
			}
		}
	})
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
#' 
#' This function grabs the following variables from the Global Environment:
#' - REGIONS
#' - LRN_IDS
#' - PREPROC
#' - TUNELENGTH
#' - ITERS
#' - PROB
#' - FINAL
#' - PATH
#' - NU
#' - REPS
#' - INNER
#' - MES
#' - FS
#' - TUNE_FS
#' - FS_NUM
#' 
#' 
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

#' Computes the benchmark in parallel mode
#' @param learners a list of `mlr` learners
#' @param tasks a list of `mlr` tasks
#' @param outers `mlr` `resampleDesc`, outer resampling
#' @param prob `logical`, controls the type of output, if `TRUE` probabilities, if `FALSE` response
#' @param mes `mlr` list of measure to compute while tuning, the learner are tuning against the first element
#' @return a list of `mlr` benchmark results
#' @import progress
#' @import parallel
#' @import parallelMap
#' @export
#' @keywords ml-benchmark
compute_benchmark <- function(learners, tasks, outers, prob, mes){
	bmrs <- list()
	pb <- progress_bar$new(format = "[:bar] :current/:total - :percent in :elapsed/:eta \n", total = length(learners), show_after = 0)
	invisible(pb$tick(0))
	parallelStop()
	for (i in seq_along(learners)){
		set.seed(1789, "L'Ecuyer-CMRG")
		if (learners[[i]]$id %in% c("h2o.deeplearning", "h2o.gbm")){
			localH2o <- h2o::h2o.init(nthreads = 8, min_mem_size='10G', max_mem_size = "20G")
			h2o::h2o.removeAll() ## clean slate - just in case the cluster was already running
			h2o::h2o.no_progress()
			bmr <- benchmark(learners[[i]], tasks, outers, measures = mes, models = TRUE, keep.extract = TRUE)
			h2o::h2o.shutdown(prompt = FALSE)
		} else {
			parallelStartSocket(8, level = "mlr.resample", load.balancing = TRUE)
			# parallelStartSocket(8, level = "mlr.tuneParams", load.balancing = TRUE)
			clusterSetRNGStream(iseed = 1789)
			bmr <- mlr::benchmark(learners[[i]], tasks, outers, measures = mes, models = TRUE, keep.extract = TRUE)
			parallelStop()
		}
		bmrs[[i]] <- bmr
		pb$tick()
	}
	bmr <- mlr::mergeBenchmarkResults(bmrs)
	return(bmr)
}


#' Compute the final models in sequential mode
#' @param learners a list of `mlr` learners
#' @param tasks a list of `mlr` tasks
#' @param prob `logical`, controls the type of output, if `TRUE` probabilities, if `FALSE` response
#' @param mes `mlr` list of measure to compute while tuning, the learner are tuning against the first element
#' @return a list of model to predict with
#' @import progress
#' @export
#' @keywords ml-benchmark
compute_final_model <- function(learners, tasks, prob, mes){
	mods <- list()
	pb <- progress_bar$new(format = "[:bar] :current/:total - :percent in :elapsed/:eta \n", total = length(learners), show_after = 0)
	invisible(pb$tick(0))
	# parallelStop()
	for (i in seq_along(learners)){
		# set.seed(1789, "L'Ecuyer-CMRG")
		# parallelStartSocket(8, level = "mlr.resample", load.balancing = TRUE)
		# parallelStartSocket(8, level = "mlr.tuneParams", load.balancing = TRUE)
		# clusterSetRNGStream(iseed = 1789)
		mod <- mlr::train(learners[[i]], tasks[[1]])
		# parallelStop()
		mods[[i]] <- mod
		pb$tick()
	}
	names(mods) <- LRN_IDS
	return(mods)
}

#' Wrapper function to compute the benchmark
#' 
#' `regional_benchmark()` is a wrapper function calling a number of functions. See details.
 #' 
#' Here is a some pseudo-code that explains what is happening behind the scenes.
#' 1. _Skip_. Because `regional_benchmark()` is called inside the `for`-loop `for (FS_NUM in FS_NUM_LIST)` (see above), if `FINAL` is #' `TRUE`, `regional_benchmark()` skips the `region` it does not need to calculate the final models.
#' 1. _Data loading_. This handled by `get_training_data()`
#' 1. _Data formatting_. This is handled by `fmt_labels()`, `sanitize_data()` and `get_coords()`.
#' 1. _Feature selection_. If `FINAL` is `TRUE` the selected features are retrieved from  ` get_bestBMR_tuning_results()`. If `FINAL` is `FALSE` the selected features are derived from transformed training data using `get_ppc()` and `preproc_data()`. The resulting transformed data are filtered for correlation higher than 0.95 with `caret::findCorrelation()`. Then, 500 subsampled `mlr` Tasks are created with `mlr::makeResampleDesc()`, `mlr::makeClassifTask()`, `mlr::makeResampleInstance()` and `mlr::filterFeatures()`. The #' `FS_NUM` most commonly select features across the 500 realizations are selected.
#' 1. _Pre-processing_. The target and training data are transformed using `get_ppc()` on the target data and `preproc_data()` on the #' training data. SMOTE is applied using `get_smote_data()` and `get_smote_coords()` which both call `resolve_class_imbalance()`.
#' 1. _Tasks_. Tasks are obtained using `mlr::makeClassifTask()`.
#' 1. _Learners_. Learners are constructed using `get_learners()` or `get_final_learners()`.
#' 1. _Compute benchmark_. The benchmark is run with `compute_final_model()` or `compute_benchmark()` (which needs to retrieve the outer #' folds of the nested resampling with `get_outers()`).
#' 
#' @param REGIONS `character`, vector of `region` identifiers (e.g. `"SFE"`)                               
#' @param LRN_IDS `character`, vector of learner identifiers (e.g. `"classif.randomForest"`)               
#' @param TUNELENGTH `numeric`, controls the number of hyper-parameters for discrete tuning                 
#' @param INNER `ResampleDesc`,  the inner folds for the nested resampling                           
#' @param ITERS `numeric`, controls the number of tries for the random tuning                          
#' @param PROB `logical`, selects the type of output, if `TRUE` probabilities, if `FALSE` response    
#' @param NU `numeric`, number of folds for the $\nu$-fold cross-validation                         
#' @param REPS `numeric`, number of repetitions for the repeated $\nu$-fold cross-validation          
#' @param PREPROC `character`, vector of preprocessing identifiers (e.g. `"scale"`)                        
#' @param FINAL `logical`, selects the type of runs, if `FALSE` trains, else predict
#' @param REDUCED `logical`, legacy option                 
#' @param MES `list`, list of measures from the `mlr` package, the first one is optimized against 
#' @param INFO `logical`, controls the information printed by the training process                    
#' @param FS `logical`, if `TRUE` activates the feature selection                                   
#' @param FS_NUM `numeric`, number of features to select if `FS` is `TRUE`                              
#' @export
#' @return a list of `mlr` benchmark results
regional_benchmark <- function(regions = c("ALLSAC", "SFE", "K", "NC", "NCC", "SCC", "SC", "SJT"), LRN_IDS, TUNELENGTH, INNER, ITERS, PROB, NU, REPS, PREPROC, FINAL, PATH, REDUCED, MES, INFO, FS, FS_NUM){
	bmrs <- lapply(regions, function(hydro_class){
		### SKIP ###
		if (FINAL){
			.FS_NUM <- FS_NUM # avoiding name conflict
			.REGIONS <- bestBMR_lrnH %>% filter(FS_NUM == .FS_NUM) %>% pull(task.id)
			# LRN_IDS <- bestBMR_lrnH %>% filter(FS_NUM == .FS_NUM) %>% pull(learner.id)
			if (!hydro_class %in% .REGIONS){
				return(NA)
			}
		}

		### DATA LOADING ### 
		results <- load_features(hydro_class)
		data_df <- results$data_df
		groups <- results$groups	

		### DATA FORMATING ###
		labels <- fmt_labels(groups, hydro_class)
		raw_training_data <- get_raw_training_data(data_df)
		coords <- get_coords(hydro_class)

		target_data <- get_target_data(hydro_class)
		raw_target_data <- get_raw_target_data(target_data)
		colnames(raw_training_data) <- colnames(raw_target_data)

		### FEATURE SELECTION ###
		if (FS){
			### PREPROC ###
			if (!FINAL){
				seed_preproc <- 720
				ppc <- get_ppc(raw_training_data, seed_preproc, c("nzv", PREPROC)) # adding nzv to avoid zero cov
				training_data <- preproc_training_data(raw_training_data, ppc, labels) #  list of 2
				
				namesICI <- c("CHYD", "CCHEM", "CSED", "CCONN", "CTEMP", "CHABT", "ICI", "WHYD", "WCHEM", "WSED", "WCONN", "WTEMP", "WHABT", "IWI")
				ind <- which(!colnames(training_data$data) %in% namesICI)
				training_data$data <- training_data$data[, ind]
				df2 <- cor(training_data$data)
				hc <- caret::findCorrelation(df2, cutoff = 0.95) # putt any value as a "cutoff" 
				hc <- sort(hc)
				droppedFeatures <- colnames(training_data$data)[hc]
				training_data$data <- training_data$data[,-c(hc)]
				print("Removing highly correlated features:")
				print(droppedFeatures)
				
				tictoc::tic()
				seed_preproc <- 720
				resampleDesc <- makeResampleDesc("Subsample", stratify = TRUE, iters = 500, split = 0.8)
				task <- makeClassifTask(data = make_training_data(training_data), target = "channel_type", coord = coords, id = paste(hydro_class, "FS"))
				rs <- makeResampleInstance(resampleDesc, task)$train.inds
				selectedFeatures <- lapply(rs, function(ind){
					resampled_task <- makeClassifTask(data = make_training_data(training_data)[ind, ], target = "channel_type", coord = coords[ind, ], id = paste(hydro_class, "FS"))
					FS_task <- filterFeatures(resampled_task, method = "FSelectorRcpp_information.gain", abs = FS_NUM)
					head(colnames(FS_task$env$data), -1)
				})
				selectedFeatures <- names(sort(table(unlist(selectedFeatures)), decreasing = TRUE))
				selectedFeatures <- head(selectedFeatures, FS_NUM)
				tictoc::toc()
				saveRDS(selectedFeatures, file.path(PATH, paste0(hydro_class, "_selectedFeatures.Rds")))
			} else {
				selectedFeatures <- bestBMR_lrnH %>% 
				filter(task.id == hydro_class, FS_NUM == .FS_NUM) %>% 
				pull(features) %>% 
				strsplit(" ") %>% 
				unlist()
			}
			print("Selected Features:")
			print(selectedFeatures)
			raw_training_data <- raw_training_data[, colnames(raw_training_data) %in% selectedFeatures]
			raw_target_data <- raw_target_data[, colnames(raw_target_data) %in% selectedFeatures]
		}	

		### PREPROC ###
		seed_preproc <- 720
		ppc <- get_ppc(raw_target_data, seed_preproc, PREPROC)
		colnames(raw_training_data) <- colnames(raw_target_data)
		training_data <- preproc_training_data(raw_training_data, ppc, labels)

		### SMOTE ###
		smote_data <- get_smote_data(training_data, seed_preproc)
		smote_coords <- get_smote_coords(coords, labels, smote_data, seed_preproc)

		### TASKS ###
		if (FS){
			tasks <- list(
				task_smote_no_StreamCat =  makeClassifTask(data = make_training_data_legacy(smote_data, data_df, StreamCat = TRUE), 		target = "channel_type", coord = smote_coords, id = paste(hydro_class, "SMOTE"))
			)
		} else {
			tasks <- list(
				task_smote_no_StreamCat =  makeClassifTask(data = make_training_data_legacy(smote_data, data_df, StreamCat = FALSE), 		target = "channel_type", coord = smote_coords, id = paste(hydro_class, "SMOTE", "no StreamCAT"))
			)
		}
		print(tasks)

		### SANITY CHECK ###
		stopifnot(any(sapply(tasks, function(task) !task$task.desc$has.missings)))

		### LEARNERS ###
		learners <- get_learners(LRN_IDS, tuneLength = TUNELENGTH, inner = INNER, iters = ITERS, prob = PROB, make_training_data(smote_data), MES, INFO)

		if (FINAL){
			### COMPUTE FINAL MODEL###
			# cwd_bak <- getwd()
			# setwd("F:/hguillon/research")
			learners <- get_final_learners(learners, PATH, hydro_class)
			print(learners)
			mods <- compute_final_model(learners, tasks, prob = PROB, mes = MES)
			l <- list(mods = mods, ppc = ppc, training_data = training_data)
			saveRDS(l, file.path(PATH, paste0(hydro_class, "_final.Rds")))
			# setwd(cwd_bak)
			return(l)
		} else {
			### BENCHMARK ###
			outers <- get_outers(nu = NU, reps = REPS, labels, tasks)

			### COMPUTE BENCHMARK ###
			# cwd_bak <- getwd()
			# setwd("F:/hguillon/research")
			bmr <- compute_benchmark(learners, tasks, outers, prob = PROB, mes = MES)
			saveRDS(bmr, file.path(PATH, paste0(hydro_class, "_benchmark.Rds")))
			# setwd(cwd_bak)
			return(bmr)
		}
	})
}
