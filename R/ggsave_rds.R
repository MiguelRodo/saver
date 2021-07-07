#' @title Save `ggplot` objects as rds without enclosing environment
#'
#' @description \code{ggplot2::gplot} encloses the calling environment in
#' two places: the \code{mapping} and \code{plot_env} list elements.
#' When the plot is created in created within a descendent environment of the global environment,
#' it encloses a non-global environment and so all the variables in that environment are saved when
#' saving the object using \code{saveRDS}. This potentially leads to massive objects.
#' This function gets away from that by evaluating the plot-creation code in a new environment
#' containing only the necessary variables and whose direct parent is the global environment.
#'
#' @param fun function. Function whose output is the `ggplot` object.
#' @param filename character. File name of output object. Note that, if it does not
#' end in \code{".rds"}, \code{".rds"} is appended.
#' @param dir_save character. Directory to save to.
#' Default is \code{NULL}, in which case the save directory is \code{here::here()}.
#' @param return_plot logical.
#' If \code{TRUE}, then the output of \code{fun} is returned.
#' Default is \code{FALSE}.
#' @param ... name-value pairs (\code{alist}).
#' These are passed to code{fun} as parameter-argument pairs.
#'
#' @return
#' If \code{return_plot == TRUE}, then the output from \code{fun}.
#' Otherwise \code{invisible(TRUE)}
#'
#' @examples
#' plot_fn <- function() ggplot(data.frame(x = 1, y = 1), aes(x,y)) + geom_point()
#' ggsave_rds(plot_fn,
#'            'example.rds',
#'            return_plot = TRUE)
#' p <- readRDS('example.rds')
#' p
#' pryr::object_size(p)
#'
#' @export
ggsave_rds <- function(fun,
                       filename,
                       dir_save = NULL,
                       return_plot = FALSE,
                       ...){

  p_dots <- rlang::list2(...)
  # should check that all arguments are named

  new_env <- new.env(parent = .GlobalEnv)
  assign('.plotting_fn_384', fun, pos = new_env)
  for(i in seq_along(p_dots)){
    nm <- names(p_dots)[i]
    val <- p_dots[[i]]
    parse_text <- paste0(
      "assign(",
      "x = '", nm, "', ",
      "value = p_dots[[i]], ", #, val,
      "pos  = new_env)"
    )
    eval(parse(text = parse_text))
  }

  parse_text <- paste0(
    '.plot_out_2910 <- .plotting_fn_384('
  )
  for(i in seq_along(p_dots)){
    nm <- names(p_dots)[i]
    val <- p_dots[[i]]
    parse_text <- paste0(
      parse_text, " ",
      nm, " = ", nm, ", "
    )
  }
  if(length(p_dots) > 0){
    parse_text <- stringr::str_sub(parse_text, end = -3)
  }

  parse_text <- paste0(parse_text, ")")

  eval(
    parse(text = parse_text),
    envir = new_env
  )

  eval(parse(
    text = "new_env$.plot_out_2910$plot_env <- new_env"
  ))
  filename <- ifelse(
    grepl(".rds$", filename),
    filename,
    paste0(filename, ".rds")
  )

  if(!is.null(dir_save)){
    if(!dir.exists(dir_save)){
      dir.create(dir_save, recursive = TRUE)
    }
    filename <- file.path(dir_save, filename)
  }

  saveRDS(new_env$.plot_out_2910,
          filename)
  if(!return_plot) return(invisible(TRUE))
  new_env$.plot_out_2910
}
