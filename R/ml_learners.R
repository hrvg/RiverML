#' Get the learners
#' 
#' This function is a wrapper around `get_learners_internal()`. 
#' 
#' @param lrn_ids `character`, list of `mlr` learner idenfitiers
#' @param tuneLength `numeric`, defines the granularity of the discrete tuning grid
#' @param inner `resampleDesc` from `mlr`, the inner folds of the nested resampling
#' @param iters `numeric`, the number of iteration for the random discrete tuning
#' @param prob `logical`, controls the type of output, if `TRUE` probabilities, if `FALSE` response
#' @param smote_data a named list with two elements `data` and `labels`, if `smote_data` is a `data.frame` it is cast into the expected list format
#' @param mes `mlr` list of measure to compute while tuning, the learner are tuning against the first element
#' @param info `logical`, controls the amount of information printed when tuning
#' @export
#' @keywords ml-learners
get_learners <- function(lrn_ids, tuneLength = 20, inner = mlr::makeResampleDesc("Holdout", stratify = TRUE), iters = 5, prob = FALSE, smote_data, mes, info){
	if (is.data.frame(smote_data)){
		smote_data <- list(data = smote_data[, -ncol(smote_data)], labels = smote_data[, ncol(smote_data)])
	}
	learners <- c(
		get_learners_internal(lrn_ids, smote_data, inner, tuneLength, .info = info, pca = FALSE, corr = FALSE, prob = TRUE, randomit = iters, mes)
		# get_learners_internal(lrn_ids, pca = TRUE, corr = FALSE, inner_resampling = inner, grid_resolution = tuneLength,  prob = prob, randomit = iters),
		# get_learners_internal(lrn_ids, pca = FALSE, corr = TRUE, inner_resampling = inner, grid_resolution = tuneLength,  prob = prob, randomit = iters),
		# get_learners_internal(lrn_ids, pca = TRUE, corr = TRUE, inner_resampling = inner, grid_resolution = tuneLength, prob = prob, randomit = iters)
		)
	return(learners)
}

#' Internal function to get the learners
#' 
#' The following learners `"classif.h2o.glm", "classif.lda", "classif.mda", "classif.naiveBayes", "classif.IBk", "classif.kknn", "classif.featureless"` do not get transformed into a `TuneWrapper`.
#' The following learners `c("classif.h2o.gbm", "classif.h2o.deeplearning", "classif.nnTrain", "classif.xgboost")` are tuned with `mlr::makeTuneControlRandom()`.
#' All other learners are tuned with `mlr::makeTuneControlGrid()`.
#' Setting the possible hyper-parameters values is handled by `get_ps()`
#' 
#' @param lrn_ids `character`, list of `mlr` learner idenfitiers
#' @param data a named list with two elements `data` and `labels`
#' @param inner_resampling `resampleDesc` from `mlr`, the inner folds of the nested resampling
#' @param grid_resolution `numeric`, defines the granularity of the discrete tuning grid
#' @param .info `logical`, controls the amount of information printed when tuning
#' @param pca `logical`, is a PCA performed
#' @param corr `logical`, are highly correlated predictors removed
#' @param prob `logical`, controls the type of output, if `TRUE` probabilities, if `FALSE` response
#' @param randomit `numeric`, the number of iteration for the random discrete tuning
#' @param mes `mlr` list of measure to compute while tuning, the learner are tuning against the first element
#' @return a list of `mlr` learners
#' @export 
#' @keywords ml-learners
get_learners_internal <- function(lrn_ids, data, inner_resampling, grid_resolution, .info = FALSE, pca = FALSE, corr = FALSE, prob = TRUE, randomit = 100, mes){
	learners <- lapply(lrn_ids, function(lrn.id){
		lrn <- mlr::makeLearner(lrn.id, predict.type = ifelse(prob, "prob", "response"))
		lrn <- mlr::makePreprocWrapperCaret(lrn, ppc.pca = pca, ppc.corr = corr)
		# if (lrn.id == "classif.h2o.deeplearning"){
		# 	lrn <- setHyperPars(lrn, seed = 120, epochs = 100)
		# }
		if (!lrn.id %in% c("classif.h2o.glm", "classif.lda", "classif.mda", "classif.naiveBayes", "classif.IBk", "classif.kknn", "classif.featureless")){
			ps <- get_ps(lrn.id, data, grid_resolution)
			if (lrn.id %in% c("classif.h2o.gbm", "classif.h2o.deeplearning", "classif.nnTrain", "classif.xgboost")){
				ctrl <- mlr::makeTuneControlRandom(maxit = randomit)
			} else {
				ctrl <- mlr::makeTuneControlGrid(resolution = grid_resolution)
			}
			lrn <- mlr::makeTuneWrapper(lrn, inner_resampling, measures = mes, ps, ctrl, show.info = .info)
		}	
		lrn <- mlr::setLearnerId(lrn, gsub("classif.", "", lrn.id))
		if (pca) lrn$id <- paste0(lrn$id,".pca")
		if (corr) lrn$id <- paste0(lrn$id,".corr")
		return(lrn)
	})
	return(learners)
}

#' Get the values for the hyper-parameter(s) set
#' 
#' Here is the list of learner supported by this function:
#' - "classif.svm"
#' - "classif.randomForest"
#' - "classif.plsdaCaret"
#' - "classif.ksvm"
#' - "classif.earth"
#' - "classif.IBk"
#' - "classif.fnn"
#' - "classif.rpart"
#' - "classif.lda"
#' - "classif.mda"
#' - "classif.h2o.glm"
#' - "classif.h2o.gbm"
#' - "classif.nnTrain"
#' - "classif.h2o.deeplearning"
#' - "classif.rda"
#' - "classif.naiveBayes"
#' - "classif.xgboost"
#' 
#' @param lrn.id `character`, a `mlr` learner identifier
#' @param data a named list with two elements `data` and `labels`
#' @param grid_resolution `numeric`, defines the granularity of the discrete tuning grid
#' @return a `mlr` `paramSet`
#' @export
#' @keywords ml-learners
get_ps <- function(lrn.id, data, grid_resolution){
	if (lrn.id == "classif.svm"){
		par_range <- caret::getModelInfo("svmLinear")[[1]]$grid(x = data$data, y = data$labels, len = grid_resolution)
		ps <- mlr::makeParamSet(
		  	mlr::makeDiscreteParam("cost", values = par_range$tau),
		  	mlr::makeDiscreteParam("kernel", values = c("linear"))
		)
	} else if (lrn.id == "classif.randomForest"){
		par_range <- caret::getModelInfo("rf")[[1]]$grid(x = data$data, y = data$labels, len = grid_resolution)
		ps <- mlr::makeParamSet(
		  	mlr::makeDiscreteParam("mtry", values = par_range$mtry)		
		)
	} else if (lrn.id == "classif.plsdaCaret"){
		par_range <- caret::getModelInfo("kernelpls")[[1]]$grid(x = data$data, y = data$labels, len = grid_resolution)
		ps <- mlr::makeParamSet(
		  	mlr::makeDiscreteParam("ncomp", values = par_range$ncomp)		
		)
	} else if (lrn.id == "classif.ksvm"){
		par_range <- caret::getModelInfo("svmRadial")[[1]]$grid(x = data$data, y = data$labels, len = grid_resolution)
		ps <- mlr::makeParamSet(
		  	mlr::makeDiscreteParam("C", values = unique(par_range$tau)),
		  	mlr::makeDiscreteParam("sigma", values = unique(par_range$sigma))			 
		)
	} else if (lrn.id == "classif.earth"){
		par_range <- caret::getModelInfo("earth")[[1]]$grid(x = data$data, y = data$labels, len = grid_resolution)
		ps <- mlr::makeParamSet(
		  	mlr::makeDiscreteParam("nprune", values = unique(par_range$nprune)),
		  	mlr::makeDiscreteParam("degree", values = unique(par_range$degree))		
		)
	} else if (lrn.id == "classif.IBk"){
		par_range <- caret::getModelInfo("knn")[[1]]$grid(x = data$data, y = data$labels, len = grid_resolution)
		ps <- mlr::makeParamSet(
		  	mlr::makeDiscreteParam("K", values = par_range$kmax)		
		)
	} else if (lrn.id == "classif.fnn"){
		par_range <- caret::getModelInfo("knn")[[1]]$grid(x = data$data, y = data$labels, len = grid_resolution)
		ps <- mlr::makeParamSet(
		  	mlr::makeDiscreteParam("k", values = par_range$kmax)		
		)	
	} else if (lrn.id == "classif.rpart"){
		par_range <- caret::getModelInfo("rpart")[[1]]$grid(x = data$data, y = data$labels, len = grid_resolution)
		ps <- mlr::makeParamSet(
		  	mlr::makeDiscreteParam("cp", values = par_range$cp)		
		)
	} else if (lrn.id == "classif.lda"){
		par_range <- caret::getModelInfo("lda")[[1]]$grid(x = data$data, y = data$labels, len = grid_resolution)
		ps <- mlr::makeParamSet(
		)
	} else if (lrn.id == "classif.mda"){
		par_range <- caret::getModelInfo("mda")[[1]]$grid(x = data$data, y = data$labels, len = grid_resolution)
		ps <- mlr::makeParamSet(
		)
	} else if (lrn.id == "classif.h2o.glm"){
		par_range <- caret::getModelInfo("glm")[[1]]$grid(x = data$data, y = data$labels, len = grid_resolution)
		ps <- mlr::makeParamSet(
		)	
	} else if (lrn.id == "classif.h2o.gbm"){
		par_range <- caret::getModelInfo("gbm")[[1]]$grid(x = data$data, y = data$labels, len = grid_resolution)
		ps <- mlr::makeParamSet(
			mlr::makeDiscreteParam("max_depth", values = unique(par_range$max_depth)),
		  	mlr::makeDiscreteParam("ntrees", values = unique(par_range$ntrees)),
		  	mlr::makeDiscreteParam("learn_rate", values = unique(par_range$learn_rate)),
		  	mlr::makeDiscreteParam("min_rows", values = unique(par_range$min_rows))		
		)
	} else if (lrn.id == "classif.nnTrain"){
		ps <- mlr::makeParamSet(
	     	mlr::makeDiscreteParam("max.number.of.layers", values = seq(2,5)),
	     	mlr::makeDiscreteParam("hidden", values = list(
				a = rep(5, 5),
				b = rep(10, 5),
				c = rep(20, 5),
				d = rep(30, 5),
				e = rep(50, 5),
				f = rep(100, 5),
				g = rep(200, 5)
				)
			),
			mlr::makeDiscreteParam("activationfun", values = c("tanh")),
			mlr::makeDiscreteParam("output", values = c("softmax")),
	      	mlr::makeDiscreteParam("numepochs", values = c(20)), # changing number of epochs
	      	# mlr::makeDiscreteParam("learningrate", values = c(0.05,0.01,0.005,0.001)),
	      	mlr::makeDiscreteParam("learningrate", values = c(0.5, 0.1, 0.05, 0.01, 0.005)),
	      	mlr::makeDiscreteParam("batchsize", values = c(16, 32, 64)),
	      	# mlr::makeDiscreteParam("batchsize", values = c(nrow(data$data))),
	      	mlr::makeDiscreteParam("momentum", values = seq(0.5, 0.9, by = 0.1)),
	      	mlr::makeDiscreteParam("hidden_dropout", values = c(0, 0.1, 0.2)),
	      	mlr::makeDiscreteParam("visible_dropout", values = c(0, 0.1, 0.2))
	    )
	} else if (lrn.id == "classif.h2o.deeplearning"){
		ps <- mlr::makeParamSet(
			mlr::makeDiscreteParam("activation", values = c(
				"Rectifier",    
				"RectifierWithDropout", 
				"Maxout", 
				"MaxoutWithDropout",
				"Tanh", 
				"TanhWithDropout")
			),
			mlr::makeDiscreteParam("hidden", values = list(
				a = c(20, 20), 
				b = c(50, 50), 
				c = c(5, 5, 5, 5, 5), 
				d = c(10, 10, 10, 10), 
				e = c(20, 20, 20, 20),
				h = c(5, 5, 5),
				f = c(50, 50, 50), 
				g = c(100, 100, 100)
				)
			),
			mlr::makeDiscreteParam("epochs", values = c(100)),
			mlr::makeDiscreteParam("seed", values = c(120)),
			mlr::makeDiscreteParam("l1", values = seq(0, 1e-4, length.out = 20)),
			mlr::makeDiscreteParam("l2", values = seq(0, 1e-4, length.out = 20)),
			mlr::makeDiscreteParam("adaptive_rate", values = list(a = TRUE)),
			# mlr::makeDiscreteParam("rate", values = c(0.005, 0.001)),
			# mlr::makeDiscreteParam("rate_annealing", values = c(1e-8, 1e-7, 1e-6)),
			mlr::makeDiscreteParam("rho", values = c(0.9, 0.95, 0.99, 0.999)),
			mlr::makeDiscreteParam("epsilon", values = c(1e-10, 1e-8, 1e-6, 1e-4)),
			# mlr::makeDiscreteParam("momentum_start", values = c(0.5)),
			# mlr::makeDiscreteParam("momentum_stable", values = c(0.99)),
			mlr::makeDiscreteParam("input_dropout_ratio", values = c(0, 0.1, 0.2)),
			mlr::makeDiscreteParam("max_w2", values = c(10, 100, 1000))
		)
	} else if (lrn.id == "classif.rda"){
		par_range <- caret::getModelInfo("rda")[[1]]$grid(x = data$data, y = data$labels, len = grid_resolution)
		ps <- mlr::makeParamSet(
		  	mlr::makeDiscreteParam("gamma", values = unique(par_range$gamma)),
		  	mlr::makeDiscreteParam("lambda", values = unique(par_range$lambda))		
		)
	} else if (lrn.id == "classif.naiveBayes"){
		par_range <- caret::getModelInfo("nb")[[1]]$grid(x = data$data, y = data$labels, len = grid_resolution)
		ps <- mlr::makeParamSet(
		  	mlr::makeDiscreteParam("laplace", values = par_range$smooth)		
		)
	} else if (lrn.id == "classif.xgboost"){
		par_range <- caret::getModelInfo("xgbTree")[[1]]$grid(x = data$data, y = data$labels, len = grid_resolution)
		ps <- mlr::makeParamSet(
			mlr::makeDiscreteParam("max_depth", values = unique(par_range$max_depth)),
			# mlr::makeDiscreteParam("nrounds", values = unique(par_range$nrounds))
			# mlr::makeDiscreteParam("nrounds", values = seq(10, 1e-4, length.out = 20)) # matching RF	
			mlr::makeDiscreteParam("nrounds", values = c(seq(10, 100, by = 10)))
			# makeDiscreteParam("eta", values = unique(par_range$eta)),	
			# makeDiscreteParam("gamma", values = unique(par_range$gamma)),	
			# makeDiscreteParam("colsample_bytree", values = unique(par_range$colsample_bytree)),
			# makeDiscreteParam("min_child_weight", values = unique(par_range$min_child_weight))
			# makeDiscreteParam("subsample", values = par_range$subsample)	
		)
	} else {
		stop("Unknown learner id.")
	}
	return(ps)
}

#' Get the final learners from the benchmark tuning results
#' @param wrapped_learners `TuneWrapper` obtained with `get_learners()`
#' @param PATH `file.path` to the benchmark results
#' @param region `character` identifying the region of study and the `mlr` task
#' @param bestBMR_tune `data.frame`, results from the benchmark
#' @return a `mlr` learner with most frequently tuned hyper-parameters
#' @export
#' @importFrom magrittr %>%
#' @importFrom stats na.omit
#' @importFrom rlang .data
#' @keywords ml-learners
get_final_learners <- function(wrapped_learners, PATH, region, bestBMR_tune){
	par_names <- unlist(lapply(wrapped_learners, function(wrapped_learner) names(wrapped_learner$opt.pars$pars)))
	tune <- bestBMR_tune %>% 
		dplyr::filter(.data$task.id == region) %>%
		reshape2::melt(measure.vars = par_names) %>%
		dplyr::group_by(.data$task.id, .data$learner.id, .data$variable) %>%
		dplyr::summarize(mfv = min(modeest::mfv(.data$value))) %>%
		na.omit() %>%
		dplyr::ungroup()
	learners <- lapply(wrapped_learners, function(wrapped_learner){
		lrn.id <- paste(wrapped_learner$type, wrapped_learner$id, sep = ".")	
		lrn <- mlr::makeLearner(lrn.id, predict.type = wrapped_learner$predict.type)
		ps <- as.list(tune %>% dplyr::filter(.data$learner.id == lrn.id) %>% dplyr::pull(.data$mfv))
		names(ps) <- tune %>% dplyr::filter(.data$learner.id == lrn.id) %>% dplyr::pull(.data$variable)
		PS <- mlr::getParamSet(lrn)$pars
		for (i in seq_along(ps)){
			psType <- PS[[names(ps)[i]]]$type
			if(psType == "discrete") psType <- "character"
			ps[[i]] <- as(ps[[i]], psType) 
		} 
		lrn <- mlr::setHyperPars(lrn, par.vals = ps)
		return(lrn)
	})
	return(learners)
}