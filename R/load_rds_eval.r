load_rds_eval <- function(filename,
                          dir_save = NULL,
                          eval = TRUE) {

  filename <- ifelse(
    grepl(".rds$", filename),
    filename,
    paste0(filename, ".rds")
  )

  if (!is.null(dir_save)) {
    filename <- file.path(dir_save, filename)
  }

  obj_out <- readRDS(filename)

  if (class(obj_out) != "saver_uneval" || !eval) {
    return(obj_out)
  }
  .data <- switch(as.character(is.character(obj_out$.data)),
    "TRUE" = readRDS(obj_out$.data),
    "FALSE" = obj_out$.data
  )

  if (!is.null(.data)) {
    assign(".data", value = .data, envir = obj_out$env_eval)
  }

  eval_rds(
    fn_or_call = obj_out$fn_or_call,
    p_dots = obj_out$p_dots,
    env_eval = obj_out$env_eval
  )
}