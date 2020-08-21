# checking for args
args = commandArgs(trailingOnly=TRUE) # checking for args

if (length(args)!=2) { # test if there is at least one argument: if not, return an error
  stop("Output dir or input file missing.\n", call.=FALSE)
}

outdir <- args[1]
run_type <- args[2]
region <- unlist(strsplit(run_type, "_"))[1]

slurm_arrayid <- Sys.getenv('SLURM_ARRAY_TASK_ID') # grab the array id value from the environment variable passed from sbatch
slurm_arrayid <- as.numeric(slurm_arrayid) # coerce the value to an integer

fname_tam <- paste0(outdir, slurm_arrayid,'_tam.csv')
fname_H <- paste0(outdir, slurm_arrayid,'_H.csv')
save_dir <- "exploitation/out/run68/"

library("RiverML")

stream_order_bool <- TRUE
strcat_dir <- "data/california-rivers/gis-files/StreamCAT"
streamcat_df <- get_streamcat_df(file.path(root_dir, strcat_dir))

target_streamlines <- get_target_streamlines(region)
target_streamcat_df <- get_target_streamcat_df(streamcat_df, target_streamlines)

target_streamcat_df <- cbind(target_streamcat_df,
	SLOPE = target_streamlines$SLOPE,
	CONFINEMEN = target_streamlines$CONFINEMEN, 
	RUSLE = target_streamlines$RUSLE,
	SO = target_streamlines$StreamOrde,
	LDD = target_streamlines$LDD)
target_streamcat_df$COMID <- NULL

lf <- list.files(file.path(outdir), pattern = "tam.csv")
ids <- unlist(lapply(lf, function(x) unlist(strsplit(x,"_"))[1]))
ids <- sapply(ids, as.numeric)
lf <- lf[order(ids)]
l_target_data_df_tam <- lapply(lf, function(f) read.csv(file.path(outdir, f)))
target_data_df_tam <- do.call(rbind, l_target_data_df_tam)

lf <- list.files(file.path(outdir), pattern = "H.csv")
ids <- unlist(lapply(lf, function(x) unlist(strsplit(x,"_"))[1]))
ids <- sapply(ids, as.numeric)
lf <- lf[order(ids)]
l_target_data_df_H <- lapply(lf, function(f) read.csv(file.path(outdir, f)))
target_data_df_H <- do.call(rbind, l_target_data_df_H)

target_data_df <- cbind(target_streamcat_df, target_data_df_tam, target_data_df_H)

write.csv(target_data_df, 
	file = file.path(root_dir, save_dir, paste0(region, '_all_data_df.csv')), 
	row.names = FALSE)

target_ID_df <- data.frame(ID = target_streamlines$ID, COMID = target_streamlines$COMID)
write.csv(target_ID_df, 
	file = file.path(root_dir, save.dir, paste0(region, '_all_ID_df.csv')), 
	row.names = FALSE)