# checking for args
args = commandArgs(trailingOnly=TRUE)

# test if there is at least one argument: if not, return an error
if (length(args)!=2) {
  stop("Output dir or input file missing.\n", call.=FALSE)
}
#  else if (length(args)==1) {
#   # default output file
#   args[2] = "out.txt"
# }

outdir <- args[1]
run_type <- args[2]

hydro_class <- unlist(strsplit(run_type, "_"))[1]

library("rgdal")
library("raster")
library("sp")
library("data.table")
library("sf")
library("psych")

source("envpath.R")
source("HPC_optim.R")
root.dir <- get_rootdir()

# user defined functions
source("/home/hguillon/farm_scripts/fdim_functions.R")
# source('/home/hguillon/farm_scripts/get_points.R')
source('/home/hguillon/farm_scripts/terrain_unix.R')

import::here(.from = "../classification/R/utils/data_loading.R", 
	get_H_raster)

import::here(.from = "../classification/R/utils/lib_classification.R", 
	get_points,
	get_conf.gis,
	sp.join_pts.llH,
	merging_data_dfs,
	get_target_pts,
	get_target_cls,
	get_target_streamcat_df,
	raster_stats,
	near_channel_stats, 
	get_stats_df
	)

getpol <- function(i,dl=1000,.crs=crs(DEM)){
	x_min <- pts@coords[i,1] - dl
	x_max <- pts@coords[i,1] + dl
	y_min <- pts@coords[i,2] - dl
	y_max <- pts@coords[i,2] + dl
	coords = matrix(c(x_min, y_min,
	               x_min, y_max,
	               x_max, y_max,
	               x_max, y_min,
	               x_min, y_min), 
	             ncol = 2, byrow = TRUE)
	p <-  Polygon(coords)
	sp1 <-  SpatialPolygons(list(Polygons(list(p), ID = "a")), proj4string=.crs)
	return(sp1)
}

# grab the array id value from the environment variable passed from sbatch
slurm_arrayid <- Sys.getenv('SLURM_ARRAY_TASK_ID')

# coerce the value to an integer
slurm_arrayid <- as.numeric(slurm_arrayid)

###################################
########### FIELD SITES ###########
###################################

input_dir <- "exploitation/input/california-rivers"
fname_sites <- paste0(run_type,"_input.csv")

raw_mat <- read.csv(file.path(root.dir, input_dir,fname_sites), header=TRUE, sep=",")
raw_mat <- na.omit(raw_mat)

if (fname_sites=='LSR_input.csv'){
		raw_mat$CLASS[raw_mat$CLASS==0] <- NA
		raw_mat <- na.omit(raw_mat)
	} 

names <- raw_mat$Name
lon <- raw_mat$long
lat <- raw_mat$lat

df_optim <- HPC_optim(length(names), io_time = 20e3, computing_time = 4e3, pl = FALSE)
n_start <- (slurm_arrayid - 1) * df_optim$array_per_task + 1
n_end <- n_start + (df_optim$array_per_task - 1)
if (n_end > length(names)) n_end <- length(names)
n <- seq(n_start, n_end)

site_ids <- names[n] # implicit requirement for get_stats_df
fname_tam <- paste0(outdir,slurm_arrayid,'_tam.csv')
fname_H <- paste0(outdir,slurm_arrayid,'_H.csv')
if(!file.exists(fname_tam)){

	#####################################
	########## READING RASTER ###########
	#####################################
	dem.dir <- 'data/california-rivers/gis-files/NED' 
	dem.file <- 'CA_DEM.grd'
	DEM <- raster(file.path(root.dir,dem.dir,dem.file))

	###########################
	### TRANSFORMING POINTS ###
	###########################

	lonlat <- cbind(lon,lat)
	crdref <- CRS('+proj=longlat +datum=WGS84')
	pts <- SpatialPoints(lonlat, proj4string = crdref)
	pts <- spTransform(pts, crs(DEM))
	ilist <- seq(1,nrow(pts@coords),1)

	r <- res(DEM)[1]
	.dl <- 25*r 
	polys <- lapply(ilist[n],getpol, dl=.dl)
	
	######################
	### GET TARGET CLS ###
	######################
	
	conf.gis <- get_conf.gis(hydro_class, stream_order = TRUE)
	target_cls <- get_target_cls(hydro_class)
	lines <- target_cls[n, ]

	###############
	### GET TAM ###
	###############
	library("psych")
	llr <- lapply(seq_along(polys), function(i){
		p <- polys[[i]]
		mDEM <- crop(DEM,p)
		terrain_metrics <- terrain_(mDEM, opt=c('slope','aspect','curvplan','curvprof', 'TPI', 'TRI', 'roughness', 'flowdir'), unit='tangent', neighbors=8)
		br <- brick(mDEM,terrain_metrics)
		nl <- nlayers(br)
		lr <- lapply(seq(nl), function(j) br[[j]])
		return(lr)
	})
	ls <- lapply(llr, stack)
	data_df.dist <- get_stats_df(raster_stats, "rstr")
	lines <- spTransform(lines, crs(ls[[1]]))	
	data_df.near_channel <- get_stats_df(near_channel_stats, "nrch")
	target.data_df_tam <- cbind(data_df.dist, data_df.near_channel)
	write.csv(target.data_df_tam, file.path(fname_tam), row.names = FALSE)
	rm(DEM)
	gc()

	###############
	### GET LLH ###
	###############
	l <- get_H_raster()
	llH <- l$llH
	hb.scales <- l$hb.scales
	target.data_df_H <- sp.join_pts.llH(llH, target_cls[n, ])
	write.csv(target.data_df_H, file.path(fname_H), row.names = FALSE)
}