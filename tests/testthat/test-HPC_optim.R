test_that("HPC_optim works", {
  expect_is(HPC_optim(1e3, pl = FALSE), "data.frame")
  expect_equal(colnames(HPC_optim(1e3, pl = FALSE)), c("task_per_array", "exec_time_min", "n_array"))
})
