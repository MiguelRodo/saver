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
#' @param eval_fun logical.
#' If \code{FALSE}, then \code{fn_or_call}
#' is not evaluated even if it is not \code{NULL}.
#' Default is \code{TRUE}.
#' @param ... name-value pairs (\code{alist}).
#' These are passed to code{fn_or_call} as parameter-argument pairs.
#'
#' @return If \code{return_obj == FALSE}, then \code{invisible(TRUE)}.
#' If \code{return_obj == TRUE}, then output type depends on \code{eval_fun}.
#' If \code{eval_fun == TRUE}, then the output from
#' evaluating \code{fn_or_call}.
#' If \code{eval_fun == FALSE}, then
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
#'  ggplot(data.frame(x = 1, y = 1), aes(x,y)) + geom_point()
#' }
#' save_rds(plot_fn,
#'          "example.rds",
#'          return_obj = TRUE)
#' p <- readRDS("example.rds")
#' p
#' pryr::object_size(p)
#'
#' @export
save_rds_eval <- function(fn_or_call = NULL,
                          filename,
                          dir_save = NULL,
                          return_obj = FALSE,
                          eval_fun = TRUE,
                          ...) {

  # prep
  # --------------

  if (missing(filename)) stop("filename must be supplied to save_rds")

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

  # capture dots
  p_dots <- list(...)

  # do exit work immediately if nothing given
  # -------------------
  if (!eval_fun) {
    return(
      .save_rds_eval_non(
        fn_or_call = fn_or_call,
        p_dots = p_dots,
        filename = filename,
        return_obj = return_obj
      )
    )
  }

  # stop if evaluation required but fn_or_call
  # not a call or a function (or a name)
  # -------------------
  if (eval_fun && !class(fn_or_call)[1] %in% c("function", "call", "name")) {
    stop("fn_or_call must be a function or
    a call (or a name) when eval_fun == TRUE")
  }

  .save_rds_eval(
    fn_or_call = fn_or_call,
    p_dots = p_dots,
    filename = filename,
    return_obj = return_obj
    )
}

# use a generic to deal with
# function, call and name classes
.save_rds_eval <- function(fn_or_call,
                           p_dots,
                           filename,
                           return_obj) {
  UseMethod(".save_rds_eval")
}

# method to save output when
# function is the input
.save_rds_eval.function <- function(fn_or_call,
                                    p_dots,
                                    filename,
                                    return_obj) {

  # set function environment to new env one below global
  # so that execution environment
  # cannot enclose anything (permanent)
  environment(fn_or_call) <- new.env(parent = .GlobalEnv)

  # create call text to be parsed
  parse_text <- "fn_or_call("

  for (i in seq_along(p_dots)) {
    nm <- names(p_dots)[i]
    parse_text <- paste0(
      parse_text,
      nm, " = p_dots[['", nm, "']], "
    )
  }

  if (length(p_dots) > 0) {
    parse_text <- stringr::str_sub(parse_text, end = -3)
  }

  parse_text <- paste0(parse_text, ")")

  # evaluate in current environment
  obj_out <- eval(
    parse(text = parse_text)
  )

  saveRDS(
    object = obj_out,
    file = filename
  )

  if (!return_obj) {
    return(invisible(TRUE))
  }
  obj_out
}

# method to save output
# when input is a call
.save_rds_eval.call <- function(fn_or_call,
                                p_dots,
                                filename,
                                return_obj) {

  env_eval <- list2env(p_dots, parent = .GlobalEnv)
  obj_out <- eval(fn_or_call, envir = env_eval)

  saveRDS(
    object = obj_out,
    file = filename
  )

  if (!return_obj) {
    return(invisible(TRUE))
  }
  obj_out
}

# method to save output
# when input is a call
.save_rds_eval.name <- function(fn_or_call,
                                p_dots,
                                filename,
                                return_obj) {
  .save_rds_eval.call(
    fn_or_call = fn_or_call,
    p_dots = p_dots,
    filename = filename,
    return_obj = return_obj
  )
}

.save_rds_eval_non <- function(fn_or_call,
                                p_dots,
                                filename,
                                return_obj) {

  obj_out <- list(
    fn_or_call = fn_or_call,
    p_dots = p_dots
  )

  class(obj_out) <- "saver_uneval"

  saveRDS(
    object = obj_out,
    file = filename
  )

  if (!return_obj) {
    return(invisible(TRUE))
  }

  obj_out

}
