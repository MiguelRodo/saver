test_that("saver works", {

  # random data
  dataset_list <- list(cd4_th1_il17_il22_il6 = cytofacs::cd4_th1_il17_il22_il6,
                       cd8_th1_il17_il22_il6 = cytofacs::cd8_th1_il17_il22_il6,
                       cd4_th1_il17 = cytofacs::cd4_th1_il17,
                       cd8_th1 = cytofacs::cd8_th1,
                       nk_th1_il17_il22_il6 = cytofacs::nk_th1_il17_il22_il6,
                       bcell_th1_il17_il22_il6 = cytofacs::bcell_th1_il17_il22_il6,
                       tcrgd_th1_il17_il22_il6  = cytofacs::tcrgd_th1_il17_il22_il6,
                       cd4_eff_th1_il17_il22_il6 = cytofacs::cd4_eff_th1_il17_il22_il6,
                       cd4_mem_th1_il17_il22_il6 = cytofacs::cd4_mem_th1_il17_il22_il6)
  ds_list_2 <- purrr::map(1:40, function(x) dataset_list)

  rndom_vec_glbl <- rnorm(1e5)

  #
  library(ggplot2)
  library(magrittr)
  # can add references to other packages
  get_p_cowplot <- function(tbl){
    ggplot2::ggplot(data = tbl, aes(x = x, y = y)) +
      geom_point() +
      cowplot::theme_cowplot()
  }

  get_p <- function(tbl){
    ggplot2::ggplot(data = tbl, aes(x = x, y = y)) + geom_point()
  }

  get_p_simple <- function(){
    ggplot2::ggplot()
  }

  fn <- get_p_simple
  fn <- get_p
  fn <- get_p_cowplot

  obj_list <- list('tbl' = data.frame(x = 1, y = 1))

  # so, these objects are taken from the calling environment of a function
  # so, the parent to the calling environment should be the global environment (perhaps?)

  pkg <- 'ggplot2'


  # function that should work best
  # --------------------

  # get parent environment
  # we could maybe just set it to a new environment,
  # one above the global environment.
  search_vec <- search()
  env_vec <- c(
    'tools:rstudio', 'package:stats', 'package:graphics',
    'package:grDevices', 'package:utils', 'package:datasets',
    'package:methods', 'Autoloads', 'package:base'
  ) %>%
    c(
      paste0("package:", pkg)
    )
  opt_vec <- which(search_vec %in% env_vec)
  glob_ind <- which(search_vec == ".GlobalEnv")
  opt_vec <- opt_vec[opt_vec > glob_ind]
  opt <- min(opt_vec)
  pos <- opt - 1
  new_env <- new.env(parent = as.environment(search_vec[pos]))

  assign('fn', fn, envir = new_env)

  for(i in seq_along(obj_list)){
    obj <- obj_list[[i]]
    parse_text <- paste0(
      "assign(x = '",
      names(obj_list)[i], "', ",
      'value = obj,',
      'pos = new_env)'
    )
    eval(parse(text = parse_text))
  }

  dir_test <- file.path("C:/Users/migue/Work", "Miscellaneous", "Tests", "saver")
  if(!dir.exists(dir_test)) dir.create(dir_test)
  p_expr <- rlang::expr(p <- fn(tbl = tbl))
 # p_env <- rlang::expr(p <- ggplot2::ggplot())
  #p_expr <- rlang::expr(f <- y ~ x)
  eval(p_expr, envir = new_env)
  saveRDS(new_env$p, file.path(dir_test, 'p_prop.rds'))
  p_prop_rl <- readRDS(file.path(dir_test, 'p_prop.rds'))

  ## standard save
  tbl <- obj_list$tbl
  p_std <- ggplot2::ggplot(data = tbl, aes(x = x, y = y)) +
    geom_point() +
    cowplot::theme_cowplot()
  saveRDS(p_std, file.path(dir_test, 'p_std.rds'))
  p_std_rl <- readRDS(file.path(dir_test, "p_std.rds"))

  ## one directory down
  large_env <- new.env(parent = .GlobalEnv)
  eval(p_expr, envir = large_env)
  saveRDS(large_env$p, file.path(dir_test, 'p_one_down.rds'))
  p_one_down_rl <- readRDS(file.path(dir_test, "p_one_down.rds"))


  # same env as new_env, but with dataset_list added
  large_env_2 <- new.env(parent = new_env)
  large_env_2$ds_list_2 <- ds_list_2
  large_env_2$tbl <- obj_list$tbl
  p_expr <- rlang::expr(p <- fn(tbl = tbl))
  eval(p_expr, envir = large_env_2)
  saveRDS(large_env_2$p, file.path(dir_test, 'p_prop_l.rds'))
  p_prop_l_rl <- readRDS(file.path(dir_test, "p_prop_l.rds"))
  env_f_2 <- attr(large_env_2$f, ".Environment")
  pryr::object_size(large_env_2$ds_list_2)
  pryr::object_size(ds_list_2)
  pryr::object_size(large_env$ds_list_2)

  # add to environment using by creating something inside the environment
  large_env_3 <- new.env(parent = new_env)
  expr_random <- rlang::expr(rndom_vec <- rnorm(1e6))
  eval(expr_random, envir = large_env_3)
  large_env_3$tbl <- obj_list$tbl
  p_expr <- rlang::expr(p <- ggplot(data = tbl, aes(x)))
  eval(p_expr, envir = large_env_3)
  saveRDS(large_env_3$p, file.path(dir_test, 'p_prop_denovo.rds'))
  p_prop_l_rl <- readRDS(file.path(dir_test, "p_prop_denovo.rds"))
  env_f_3 <- attr(large_env_3$f, ".Environment")
  saveRDS(env_f_3, 'env_f3.rds')
  saveRDS(large_env_3$f, 'f3.rds')
  pryr::object_size(large_env_2$ds_list_2)
  pryr::object_size(ds_list_2)
  pryr::object_size(large_env$ds_list_2)



  formula1 <- y ~ x
  save(formula1, file = "formula1.Rdata")

  f <- function() {
    z <- rnorm(1000000)
    formula2 <- y ~ x
    save(formula2, file = "formula2.Rdata")
  }
  f()

  p <- ggplot(data = tbl, aes(x))
  save(formula1, file = "p1.Rdata")

  p <- function() {
    z <- rnorm(1000000)
    p <- ggplot(data = tbl, aes(x))
    save(p, file = "p2.Rdata")
  }
  p()

}
