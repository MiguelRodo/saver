#' @title Save objects as rds without enclosing extra objects
#'
#' @description
#' Many objects, such as \code{ggplot2::gplot} objects and formulae,
#' enclose the \href{https://adv-r.hadley.nz/environments.html#execution-environments}{calling environment}.
#' If such objects are saved using \code{saveRDS},
#' then the enclosed environments are also saved.
#' Therefore this may unintentionally save large objects.
#' This function avoids this by evaluating the function in
#' the global environment (or a direct child), and objects in
#' global environments are not saved when enclosed.
#' The outputted object can then be saved without extra unwanted items.
#'
#' @param fn_or_call function or call.
#' Function whose output is the `ggplot` object.
#' @param .data character, call or any other object
#' (esp. a data frame).
#' If the data are large and you wish to
#' save merely a pointer to it, then you can
#' either provide a \code{character} or a call
#' that evaluates to a character.
#' Both will ultimately represent character paths,
#' that will then be supplied to \code{readRDS},
#' with the resultant object then provided to the
#' function/call with the name \code{.data}.
#' Use a call rather than a character if
#' you want the path to be correct on
#' different computers, e.g. by using \code{here::here()}
#' If \code{.data} is not a character or \code{NULL}, then it will
#' be attached as is to the environment. If \code{data}
#' is  \code{NULL},
#' then will not be attached. Default is \code{NULL}.
#' @param filename character.
#' File name of output object.
#' Note that if it does not end in \code{".rds"}, \code{".rds"} is appended.
#' @param dir_save character.
#' Directory to save to.
#' Default is \code{NULL}, in which case the save directory is
#' \code{here::here()}.
#' @param return_obj logical.
#' If \code{TRUE}, then the output of \code{fn_or_call} is returned.
#' Default is \code{FALSE}.
#' @param eval logical.
#' If \code{FALSE}, then \code{fn_or_call}
#' is not evaluated even if it is not \code{NULL}.
#' Default is \code{TRUE}.
#' @param test logical.
#' If \code{FALSE} and \code{eval == FALSE}, then
#' \code{load_rds_eval} is run to see if the
#' saved object can be used in evaluation without
#' returning an error. In particular, this is used
#' to check that everything that should be saved
#' via \code{...} is saved. Default is \code{FALSE}.
#' @param  message_size numeric. If \code{test == TRUE},
#' then a message is printed if the size exceeds \code{warning} in MB.
#' Default is 1.
#' Set to 0 to always message the size if \code{test == TRUE}.
#' @param ... name-value pairs (\code{alist}).
#' These are passed to code{fn_or_call} as parameter-argument pairs.
#'
#' @return If \code{return_obj == FALSE}, then \code{invisible(TRUE)}.
#' If \code{return_obj == TRUE}, then output type depends on \code{eval}.
#' If \code{eval == TRUE}, then the output from
#' evaluating \code{fn_or_call}.
#' If \code{eval == FALSE}, then
#' \code{list(fn_or_call = fn_or_call, p_dots = p_dots)}.
#'
#' @details
#' For example, \code{ggplot2::gplot} encloses the
#' calling environment in two places: the \code{mapping}
#' and \code{plot_env} list elements.
#'
#'
#' @examples
#' plot_fn <- function() {
#'   ggplot(data.frame(x = 1, y = 1), aes(x, y)) +
#'     geom_point()
#' }
#' save_rds(plot_fn,
#'   "example.rds",
#'   return_obj = TRUE
#' )
#' p <- readRDS("example.rds")
#' p
#' pryr::object_size(p)
#' @export
save_rds_eval <- function(fn_or_call = NULL,
                          .data = NULL,
                          .data_nm = ".data",
                          filename,
                          dir_save = NULL,
                          return_obj = FALSE,
                          eval = TRUE,
                          test = FALSE,
                          message_size = 1,
                          ...) {

  # prep
  # --------------

  if (missing(filename)) {
    stop("filename must be supplied to save_rds")
  }

  # set up file name to save to
  filename <- ifelse(
    grepl(".rds$", filename),
    filename,
    paste0(filename, ".rds")
  )

  if (!is.null(dir_save)) {
    if (!dir.exists(dir_save)) {
      dir.create(dir_save, recursive = TRUE)
    }
    filename <- file.path(dir_save, filename)
  }

  filename <- normalizePath(
    filename,
    mustWork = FALSE
  )

  # capture dots
  p_dots <- list(...)

  # create environment to
  # evaluate in (and fn env, if applicable)
  env_eval <- .get_env_eval(
    fn_or_call = fn_or_call,
    p_dots = p_dots
  )

  if (is.function(fn_or_call)) {
    environment(fn_or_call) <- env_eval
  }

  if (!eval) {
    obj_out <- list(
      fn_or_call = fn_or_call,
      .data = .data,
      .data_nm = .data_nm,
      env_eval = env_eval
    )

    class(obj_out) <- "saver_uneval"
  } else {
    assign_data_nm(
      .data = .data,
      .data_nm = .data_nm,
      env = env_eval
    )

    obj_out <- eval_rds(
      fn_or_call = fn_or_call,
      env_eval = env_eval
    )
  }

  saveRDS(
    object = obj_out,
    file = filename
  )

  if (test) {
    .test_rds_eval(
      test = test,
      filename = filename,
      message_size = message_size
    )
  }

  if (!return_obj) {
    return(filename)
  }

  obj_out
}



#' @title Test whether object can be loaded and its size
.test_rds_eval <- function(test,
                           filename,
                           message_size) {

  if (!test) {
    return(invisible(TRUE))
  }

  obj_test <- try(
    load_rds_eval(
      filename = filename
    ),
    silent = TRUE
  )
  if (class(obj_test) == "try-error") {
    stop(
      paste0(
        "could not evaluate saver_uneval object '",
        basename(filename), "' (msg: ",
        attr(obj_test, "condition")$message, ")"
      ),
      call. = FALSE
    )
  } else {
    object_size <- pryr::object_size(obj_test)
    if (object_size / 1e6 >= message_size) {
      message(
        paste0(
          basename(filename),
          " is ",
          round(pryr::object_size(obj_test) / 1e6, 1),
          "MB"
        )
      )
    }
  }

  invisible(TRUE)
}

.get_nearest_special_env <- function(env) {
  if (isNamespace(env)) {
    return(env)
  }
  env_parents_list <- rlang::env_parents(env)
  ind_nearest_special_env <- min(which(
    purrr::map_lgl(
      rlang::env_parents(env),
      function(x) {
        env_nm <- attr(x, "name")
        imports_in_nm <- grepl("imports", env_nm)
        is_global <- identical(x, .GlobalEnv)
        is_ns <- isNamespace(x)
        is_ns || imports_in_nm || is_global
      }
    )
  ))
  env_parents_list[[ind_nearest_special_env]]
}

.check_if_in_pkg <- function(fn_or_call) {
  is_ns_vec <- purrr::map_lgl(
    .check_if_in_pkg_env(fn_or_call = fn_or_call),
    isNamespace
  )
  any(is_ns_vec)
}

.check_if_in_pkg_env <- function(fn_or_call) {
  switch(class(fn_or_call)[1],
    "function" = rlang::env_parents(environment(fn_or_call)),
    # "call" = .get_nearest_special_env(
    #  rlang::caller_env(n = 3)
    #)
    "call" = rlang::env_parents(
      rlang::caller_env(n = 3)
    )
  )
}

.get_parent_env_to_list <- function(fn_or_call,
                                    in_pkg) {
  switch(as.character(in_pkg),
    "FALSE" = .GlobalEnv,
    "TRUE" = switch(class(fn_or_call)[1],
      "function" = .get_nearest_special_env(
        environment(fn_or_call)
      ),
      "call" = .get_nearest_special_env(
        rlang::caller_env(n = 3)
      )
    )
  )
}

.get_env_eval <- function(fn_or_call, p_dots) {
  in_pkg <- .check_if_in_pkg(fn_or_call = fn_or_call)
  parent_env_to_list <-
    .get_parent_env_to_list(
      fn_or_call = fn_or_call,
      in_pkg = in_pkg
    )
  new.env(
    parent = list2env(
      x = p_dots,
      parent = parent_env_to_list
    )
  )
}

assign_data_nm <- function(.data,
                           .data_nm,
                           env) {

  .data <- switch(class(.data)[1],
    "character" = readRDS(.data),
    "call" = readRDS(
      eval(.data, envir = env)
    ),
    .data
  )

  if (!is.null(.data)) {
    assign(.data_nm, value = .data, envir = env)
  }

  invisible(TRUE)

}