#' Computes an optimized repartition of the HPC computing
#' 
#' This function uses the limits of maximum concurrent jobs, the input/output time and computng time.
#' The time are derived using `profvis` package.
#' 
#' @param len the number of time the computation needs to be iterated
#' @param hpc_lim the maximum number of subjobs per user
#' @param io_time the input/output time in ms, default to `20e3`
#' @param computing_time the computing time in ms, default to `1830`
#' @param AssocGrpCpuLimit the maximum number of CPU per user, default to `258`
#' @param pl logical, if `TRUE` a plot is produced, default to `TRUE`
#' @param core_per_task number of CPU per task, default to 1.
#' @return a `data.frame` with columns `task_per_array`, `exec_time_min` and `n_array`
#' @keywords misc HPC
#' @export
HPC_optim <- function(len, hpc_lim = 1e4, io_time = 20e3, computing_time = 1830, AssocGrpCpuLimit = 258, pl = TRUE, core_per_task = 1){
  avail <- floor(AssocGrpCpuLimit / core_per_task)
  xx <- seq(len + len %% 2)
  rev.x <- len %/% xx + ifelse(len %% xx == 0, 0, 1)
  yy <- (io_time + computing_time * xx) * (1 + rev.x %/% hpc_lim) * (1 + rev.x %/% avail)
  yy <- yy / (1e3 * 60) # ms -> s -> min
  if (pl){
    graphics::plot(xx, yy, ylab = "Total exec time [min]", xlab = "Task per array [-]", log = "xy", type = "n")
    graphics::lines(xx, yy)
  }
  df <- data.frame(task_per_array = xx, exec_time_min = yy, n_array = rev.x)
  return(df[which.min(df$exec_time_min), ])
}