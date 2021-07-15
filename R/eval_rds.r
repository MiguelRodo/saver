#' @title Evaluate object of class "saver_uneval"
#'
#' @return Output from evaluating object
#'
#' @export
eval_rds <- function(fn_or_call,
                     p_dots,
                     env_eval
                     ) {

  if (is.function(fn_or_call)) {
    assign("fn", fn_or_call, envir = env_eval)
    # create call text to be parsed
    parse_text <- "fn("

    for (i in seq_along(p_dots)) {
      nm <- names(p_dots)[i]
      parse_text <- paste0(
        parse_text,
        # nm, " = p_dots[['", nm, "']], "
        nm, " = ", nm, ", "
      )
    }

    if (length(p_dots) > 0) {
      parse_text <- stringr::str_sub(parse_text, end = -3)
    }

    parse_text <- paste0(parse_text, ")")
    fn_or_call <- rlang::parse_expr(parse_text)
  }

  eval(
    expr = fn_or_call,
    envir = env_eval
  )

}