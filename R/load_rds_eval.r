#' @title Load an RDS object saved with save_rds_eval
#'
#' @description If object was not evaluated, it will be
#' before being returned.
#'
#' @export
load_rds_eval <- function(filename,
                          dir_save = NULL) {

  filename <- ifelse(
    grepl(".rds$", filename),
    filename,
    paste0(filename, ".rds")
  )

  if (!is.null(dir_save)) {
    filename <- file.path(dir_save, filename)
  }

  filename <- normalizePath(filename)

  obj_out <- readRDS(filename)

  if (class(obj_out) != "saver_uneval") {
    return(obj_out)
  }

  if (!is.null(obj_out$.data)) {
    .data <- switch(class(obj_out$.data)[1],
      "character" = readRDS(obj_out$.data),
      # here has to be an environment
      # that isn't the saver environment
      "call" = readRDS(
        eval(
          obj_out$.data,
          envir = obj_out$env_eval,
        )
      ),
      obj_out$.data
    )
    assign(obj_out$.data_nm, value = .data, envir = obj_out$env_eval)
  }

  eval_rds(
    fn_or_call = obj_out$fn_or_call,
    env_eval = obj_out$env_eval
  )
}

.get_nearest_non_saver_env <- function(env) {
  exc_env <- "saver|imports:saver|base"
  if (!grepl(exc_env, environmentName(env))) {
    return(env)
  }
  env_parents_list <- rlang::env_parents(env)
  for (env in env_parents_list) {
    if (!grepl(exc_env, environmentName(env))) {
      return(env)
    }
  }

  stop("no non-saver pkg special env detected")
}
