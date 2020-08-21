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

library("RiverML")

source("envpath.R")
root.dir <- get_rootdir()

# user defined functions
source('/home/hguillon/farm_scripts/terrain_unix.R')


###################################
########### FIELD SITES ###########
###################################

input_dir <- "exploitation/input/california-rivers"
fname <- paste0(run_type,"_input.csv")
input_data <- get_input_data(file.path(input_dir, fname))

df_optim <- HPC_optim(nrow(input_data), io_time = 20e3, computing_time = 4e3, pl = FALSE)
n_start <- (slurm_arrayid - 1) * df_optim$task_per_array + 1
n_end <- n_start + (df_optim$task_per_array - 1)
if (n_end > nrow(input_data)) n_end <- nrow(input_data)
n <- seq(n_start, n_end)

if(!file.exists(fname_tam)){

	dem_dir <- 'data/california-rivers/gis-files/NED' 
	dem_file <- 'CA_DEM.grd'
	DEM <- raster(file.path(root.dir,dem_dir,dem_file))

	polys <- get_input_polygons(input_data, n, DEM, .dl = 25)
	ls <- get_terrain_metrics(polys, DEM, curvature = FALSE)
	raster_stats_df <- get_stats_df(raster_stats, "rstr", ls, dem_file)

	target_streamlines <- get_target_streamlines(region)
	target_streamlines <- target_streamlines[n, ]
	near_channel_stats_df <- get_stats_df(near_channel_stats, "nrch", ls, lines = target_streamlines, dem_file)


	target_data_df_tam <- cbind(raster_stats_df, near_channel_stats_df)

	write.csv(target_data_df_tam, file.path(fname_tam), row.names = FALSE)
	rm(DEM)
	gc()
}

if(!file.exists(fname_H)){
	lH <- get_H_rasters()
	H_rasters <- lH$H_rasters
	upper_scales <- lH$upper_scales
	target_data_df_H <- join_streamlines_with_H_rasters(H_rasters, target_streamlines, upper_scales)
	write.csv(target_data_df_H, file.path(fname_H), row.names = FALSE)
}