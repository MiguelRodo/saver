test_that("save_rds_eval works", {

  # save in temp directory to avoid cruft
  dir_save <- file.path(
    tempdir(),
    "test",
    "save_rds_eval"
  )
  if (!dir.exists(dir_save)) dir.create(
    dir_save,
    recursive = TRUE
  )

  # function
  # -----------------------

  # evaluation, with a function
  save_rds_eval(
    fn_or_call = mean,
    filename = "mean",
    dir_save = dir_save,
    return_obj = TRUE,
    eval = TRUE,
    x = 1:5
  )

  expect_true(file.exists(file.path(dir_save, "mean.rds")))
  expect_true(is.numeric(readRDS(file.path(dir_save, "mean.rds"))))
  expect_identical(
    readRDS(file.path(dir_save, "mean.rds")),
    3
  )
  y <- load_rds_eval(
    filename = "mean.rds",
    dir_save = dir_save
  )
  expect_identical(
    readRDS(file.path(dir_save, "mean.rds")),
    y
  )
  file.remove(file.path(dir_save, "mean.rds"))

  # no evaluation, with a function
  save_rds_eval(
    fn_or_call = mean,
    filename = "mean",
    dir_save = dir_save,
    return_obj = FALSE,
    eval = FALSE,
    x = 1:5
  )

  expect_true(file.exists(file.path(dir_save, "mean.rds")))
  rds_saved <- readRDS(file.path(dir_save, "mean.rds"))
  expect_identical(class(rds_saved), "saver_uneval")
  expect_identical(
    names(rds_saved),
    c(
      "fn_or_call", ".data",
      ".data_nm", "env_eval"
    )
  )

  # check that object is then evaluated
  # when loading correctly
  obj_out <- load_rds_eval(
    filename = "mean.rds",
    dir_save = dir_save
  )

  expect_true(is.numeric(obj_out))
  expect_identical(
    obj_out,
    3
  )

  file.remove(file.path(dir_save, "mean.rds"))

  # call
  # -----------------------

  # evaluation, with a call
  save_rds_eval(
    fn_or_call = quote(mean(x = x)),
    filename = "mean",
    dir_save = dir_save,
    return_obj = TRUE,
    eval = TRUE,
    x = 1:5
  )
  expect_true(file.exists(file.path(dir_save, "mean.rds")))
  expect_true(is.numeric(readRDS(file.path(dir_save, "mean.rds"))))
  expect_identical(
    readRDS(file.path(dir_save, "mean.rds")),
    3
  )
  y <- load_rds_eval(
    filename = "mean.rds",
    dir_save = dir_save
  )
  expect_identical(
    readRDS(file.path(dir_save, "mean.rds")),
    y
  )
  file.remove(file.path(dir_save, "mean.rds"))


  # no evaluation, with a call
  save_rds_eval(
    fn_or_call = quote(mean(x = x)),
    filename = "mean",
    dir_save = dir_save,
    return_obj = FALSE,
    eval = FALSE,
    x = 1:5
  )


  expect_true(file.exists(file.path(dir_save, "mean.rds")))
  rds_saved <- readRDS(file.path(dir_save, "mean.rds"))
  expect_identical(class(rds_saved), "saver_uneval")
  expect_identical(
    names(rds_saved),
    c("fn_or_call", ".data",  ".data_nm", "env_eval")
  )

  # check that object is then evaluated
  # when loading correctly
  obj_out <- load_rds_eval(
    filename = "mean.rds",
    dir_save = dir_save
  )

  expect_true(is.numeric(obj_out))
  expect_identical(
    obj_out,
    3
  )

  # check that we do really avoid
  # large size objects
  # -------------------------------

  test_fn <- function() {
    rndm_vec <- rnorm(1e5)
    f <- function() {
      ~x
    }
    saveRDS(f(), file.path(dir_save, "large.rds"))
    save_rds_eval(
      fn_or_call = f,
      filename = "small",
      dir_save = dir_save,
      return_obj = FALSE,
      eval = TRUE
    )
  }
  test_fn()
  # od(dir_save)
  large_obj <- readRDS(file.path(dir_save, "large.rds"))
  small_obj <- readRDS(file.path(dir_save, "small.rds"))

  expect_true(
    file.size(file.path(dir_save, "large.rds")) >
      20 * file.size(file.path(dir_save, "small.rds"))
  )
  expect_identical(
    # names(parent.env(large_obj)),
    names(parent.env(attr(large_obj, ".Environment"))),
    c("f", "rndm_vec")
  )
  expect_identical(
    # names(parent.env(small_obj)),
    names(attr(small_obj, ".Environment")),
    ""[-1]
  )

  unlink(dir_save, recursive = TRUE)

  # check that checks pass and
  # fail when they should
  # -------------------------------

  # no evaluation, with a call
  save_rds_eval(
    fn_or_call = quote(mean(x = x)),
    filename = "mean",
    dir_save = dir_save,
    return_obj = FALSE,
    eval = FALSE,
    test = TRUE,
    message_size = 1,
    x = 1:5
  )

  try(
    suppressWarnings(rm("x", envir = .GlobalEnv)),
    silent = TRUE
  )

  expect_error(save_rds_eval(
    fn_or_call = quote(mean(x = x)),
    filename = "mean",
    dir_save = dir_save,
    return_obj = FALSE,
    eval = FALSE,
    test = TRUE,
    message_size = 1
  ))

  # check that messages are delivered
  # when they should be
  # -------------------------------


  expect_message(save_rds_eval(
    fn_or_call = function() cars,
    filename = "mean",
    dir_save = dir_save,
    return_obj = FALSE,
    eval = TRUE,
    test = TRUE,
    message_size = 0
  ))

  expect_silent(save_rds_eval(
    fn_or_call = function() cars,
    filename = "mean",
    dir_save = dir_save,
    return_obj = FALSE,
    eval = FALSE,
    test = TRUE,
    message_size = 1
  ))

  # no eval

  expect_message(save_rds_eval(
    fn_or_call = function() cars,
    filename = "mean",
    dir_save = dir_save,
    return_obj = FALSE,
    eval = FALSE,
    test = TRUE,
    message_size = 0
  ))

  expect_silent(save_rds_eval(
    fn_or_call = function() cars,
    filename = "mean",
    dir_save = dir_save,
    return_obj = FALSE,
    eval = FALSE,
    test = TRUE,
    message_size = 1
  ))

  # test with a function that is loaded in a package
  expect_type(
    save_rds_eval(
      fn_or_call = test_fn_2391,
      filename = "test_fn_testing",
      dir_save = dir_save,
      return_obj = TRUE,
      eval = TRUE,
      test = FALSE
    ),
    "closure"
  )

  # .data usage as a data frame - character
  # ----------------------

  file.remove(file.path(dir_save, "mean.rds"))

  saveRDS(data.frame(x = 1:5), file.path(tempdir(), "test_df"))
  path_chr <- file.path(tempdir(), "test_df")
  path_call <- quote(file.path(tempdir(), "test_df"))

  # evaluation, with a call
  save_rds_eval(
    fn_or_call = quote(mean(x = .data$x)),
    .data = path_chr,
    filename = "mean",
    dir_save = dir_save,
    return_obj = FALSE,
    eval = TRUE,
    test = TRUE,
    message_size = 1
  )

  expect_true(file.exists(file.path(dir_save, "mean.rds")))
  expect_true(is.numeric(readRDS(file.path(dir_save, "mean.rds"))))
  expect_identical(
      readRDS(file.path(dir_save, "mean.rds")),
      3
    )
  y <- load_rds_eval(
    filename = "mean.rds",
    dir_save = dir_save
  )
  expect_identical(
    readRDS(file.path(dir_save, "mean.rds")),
    y
  )
  file.remove(file.path(dir_save, "mean.rds"))

  # no evaluation, with a call
  save_rds_eval(
    fn_or_call = quote(mean(x = .data$x)),
    .data = path_chr,
    filename = "mean",
    dir_save = dir_save,
    return_obj = FALSE,
    eval = FALSE,
    test = TRUE,
    message_size = 1
  )

  expect_true(file.exists(file.path(dir_save, "mean.rds")))
  rds_saved <- readRDS(file.path(dir_save, "mean.rds"))
  expect_identical(class(rds_saved), "saver_uneval")
  expect_identical(
    names(rds_saved),
    c("fn_or_call", ".data", ".data_nm", "env_eval")
  )

  expect_error(
    save_rds_eval(
      fn_or_call = quote(mean(x = .data$x)),
      .data = data.frame(x = 1:5),
      filename = "mean",
      dir_save = dir_save,
      return_obj = FALSE,
      eval = FALSE,
      test = TRUE,
      message_size = 1,
      .data = "7"
    )
  )


  file.remove(file.path(dir_save, "mean.rds"))

  # .data usage as a character
  # ----------------

  # evaluation, with a call
  save_rds_eval(
    fn_or_call = quote(mean(x = .data$x)),
    .data = data.frame(x = 1:5),
    filename = "mean",
    dir_save = dir_save,
    return_obj = FALSE,
    eval = TRUE,
    test = TRUE,
    message_size = 1
  )

  expect_true(file.exists(file.path(dir_save, "mean.rds")))
  expect_true(is.numeric(readRDS(file.path(dir_save, "mean.rds"))))
  expect_identical(
    readRDS(file.path(dir_save, "mean.rds")),
    3
  )
  y <- load_rds_eval(
    filename = "mean.rds",
    dir_save = dir_save
  )
  expect_identical(
    readRDS(file.path(dir_save, "mean.rds")),
    y
  )
  file.remove(file.path(dir_save, "mean.rds"))

  # no evaluation, with a call
  save_rds_eval(
    fn_or_call = quote(mean(x = .data$x)),
    .data = data.frame(x = 1:5),
    filename = "mean",
    dir_save = dir_save,
    return_obj = FALSE,
    eval = FALSE,
    test = TRUE,
    message_size = 1
  )

  expect_true(file.exists(file.path(dir_save, "mean.rds")))
  rds_saved <- readRDS(file.path(dir_save, "mean.rds"))
  expect_identical(class(rds_saved), "saver_uneval")
  expect_identical(
    names(rds_saved),
    c("fn_or_call", ".data", ".data_nm", "env_eval")
  )

  expect_error(
    save_rds_eval(
      fn_or_call = quote(mean(x = .data$x)),
      .data = data.frame(x = 1:5),
      filename = "mean",
      dir_save = dir_save,
      return_obj = FALSE,
      eval = FALSE,
      test = TRUE,
      message_size = 1,
      .data = "7"
    )
  )

   # .data usage as a call
   # ----------------

   # evaluation, with a call
   save_rds_eval(
     fn_or_call = quote(mean(x = .data$x)),
     .data = path_call,
     filename = "mean",
     dir_save = dir_save,
     return_obj = FALSE,
     eval = TRUE,
     test = TRUE,
     message_size = 1
   )

   expect_true(file.exists(file.path(dir_save, "mean.rds")))
   expect_true(is.numeric(readRDS(file.path(dir_save, "mean.rds"))))
   expect_identical(
     readRDS(file.path(dir_save, "mean.rds")),
     3
   )
   y <- load_rds_eval(
     filename = "mean.rds",
     dir_save = dir_save
   )
   expect_identical(
     readRDS(file.path(dir_save, "mean.rds")),
     y
   )
   file.remove(file.path(dir_save, "mean.rds"))

   # no evaluation, with a call
   save_rds_eval(
     fn_or_call = quote(mean(x = .data$x)),
     .data = path_call,
     filename = "mean",
     dir_save = dir_save,
     return_obj = FALSE,
     eval = FALSE,
     test = TRUE,
     message_size = 1
   )

   expect_true(file.exists(file.path(dir_save, "mean.rds")))
   rds_saved <- readRDS(file.path(dir_save, "mean.rds"))
   expect_identical(class(rds_saved), "saver_uneval")
   expect_identical(
     names(rds_saved),
     c("fn_or_call", ".data", ".data_nm", "env_eval")
   )
   expect_error(
     save_rds_eval(
       fn_or_call = quote(mean(x = .data$x)),
       .data = data.frame(x = 1:5),
       filename = "mean",
       dir_save = dir_save,
       return_obj = FALSE,
       eval = FALSE,
       test = TRUE,
       message_size = 1,
       .data = "7"
     )
   )

})

test_that(".get_nearest_non_saver_env works", {
  expect_identical(
    .get_nearest_non_saver_env(.GlobalEnv),
    .GlobalEnv
  )
  expect_identical(
    .get_nearest_non_saver_env(environment(saver::eval_rds)),
    .GlobalEnv
  )
})
