#' @title Evaluate object of class "saver_uneval"
#'
#' @return Output from evaluating object
#'
#' @export
eval_rds <- function(fn_or_call,
                     env_eval
                     ) {

  if (is.function(fn_or_call)) {

    param_vec_nm <- c(
      names(parent.env(env_eval)),
      names(env_eval)
    )

    # construct call
    assign("fn", fn_or_call, envir = env_eval)
    # create call text to be parsed
    parse_text <- "fn("

    for (i in seq_along(param_vec_nm)) {
      nm <- param_vec_nm[i]
      parse_text <- paste0(
        parse_text,
        nm, " = ", nm, ", "
      )
    }

    if (length(param_vec_nm) > 0) {
      parse_text <- stringr::str_sub(parse_text, end = -3)
    }

    parse_text <- paste0(parse_text, ")")
    fn_or_call <- rlang::parse_expr(parse_text)
  } else {
    # don't need to construct
    # call if already given
    NULL
  }

  eval(
    expr = fn_or_call,
    envir = env_eval
  )

}